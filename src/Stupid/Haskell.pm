package Stupid::Haskell;

# TODO a note about use of STRefs for all variables, and how maybe we could
# do static analysis to see if they're single write and then encode them as
# haskell variables rather than as STRefs

# Types in haskell must begin with an uppercase, which is reflected here

use strict;

sub Stupid::LanguageWrapper::emitCode {
    my $self = shift;

# TODO these imports could be re-exported by StupidStuff
    print "import Data.STRef\n";
    print "import Control.Applicative\n";
    print "import Control.Monad.ST\n";
    print "import StupidStuff\n";
    print "main = do { putStrLn \"Test\"; print \$ runST prog }\n";
    print "prog = ";
    $self->{tree}->emitCode();
}

sub Stupid::Declare::emitCode {
    my $self = shift;

    $self->{var}->emitDeclaration($self->{init});
    print "\n";
}

sub Stupid::StatementList::emitCode {
    my $self = shift;
    print "do { ";
    for my $s (@{$self->{statements}}) {
	$s->emitCode();
    }
    print " ; return () }";
# TODO for now we always return nothing at all - the only example I've seen
# of stupid code gives any particular return method so I arbitrarily assume
# none at all
}

sub Stupid::Statement::emitCode {
    my $self = shift;

    $self->{expr}->emitCode();
    print ";";
}

sub Stupid::If::emitCode {
    my $self = shift;

    print "do {\n";
    print "xxxifcond <- ";
    $self->{cond}->emitCode();
    print "; \n if xxxifcond";
    print ' then do { ';
    $self->{then}->emitCode();
    print " } else do { ";
    $self->{else}->emitCode();
    print "}\n";
    print "};\n"; 
}

sub Stupid::While::emitCode {
    my $self = shift;

# TODO stupidwhile will need defining.
# its condition is a monadic action returning a value, not a pure
# expression (despite what it superficially looks like) because
# it depends on the value of variables...
    print 'stupidwhile (';
    $self->{cond}->emitCode();
    print ") (do {\n";
    $self->{body}->emitCode();
    print "});\n";
}

sub Stupid::Comment::emitCode {
    my $self = shift;

# TODO comment needs escaping...
    print "{- $self->{comment} -}\n";
}

sub Stupid::Comment::emitDeclarations {
    my $self = shift;

# TODO comment needs escaping
    print "{- (DECL) $self->{comment} -}\n";
}


# TODO this would definitely look nicer with an applicative functor style
sub Stupid::Set::emitCode {
    my $self = shift;

# should be able to pull the InST out into its own operator using
# applicative functor syntax, but I don't have my head around that at
# the moment...
    print '(writeSTRefInST (';
    $self->{left}->emitLValue();
    print ' )) $< ( ';
    $self->{right}->emitCode();
    print ")\n";



#    print "do { writeInternalVar<-";
#    $self->{right}->emitCode();
#    print "; writeSTRef ( " ;
#    $self->{left}->emitLValue();
#    print ") writeInternalVar } \n";
}

# should specify the type here even though it might
# often be inferred - we have the information in the
# source language, and its probably a good idea to
# keep it for type checking

# variables need to be IORefs to STRefs or something like that, rather
# than plain haskell variables. are there variable default-value rules
# or must such always be declared?

# but what about arrays? Going to use an MArray here? That looks most like
# one-dimensional arrays. but if there are multidimensional arrays, what
# should happen? (various options)

sub Stupid::Variable::emitDeclaration {
    my $self = shift;
    my $init = shift;

    # skip the type declaration...
    # $self->{type}->emitDeclaration($self->{name});

    # now create a STRef for this variable and assign the initial value
    print $self->{name};
    print ' <- (newSTRef $< ( (';
    $init->emitCode();
    print ') :: ST s  ';
    $self->{type}->emitHaskellType();
    print "));\n"
# I don't know how to emit types for arrays as they are represented here
# (because of the free variable in STRefs :(

    # (do { assignV <- ';  # TODO assignV might clash with the namespace of stupid variables (and in general, stupid variable names shouldn't be permitted to clash with haskell symbols that may be defined elsewhere, so probably should mangle them...)
    #$init->emitCode();
    #print ' ; newSTRef ( assignV ';
    ### print ' :: ';
    ### $self->{type}->emitHaskellType();
    ### This works for single values, but seems to get upset with STRefs as
    ### I'm leaving a type variable free (STRef s Uint32, for example)
    ### so for now cannot assert types on arrays :(
    #print " ) }) ; \n";
}

sub Stupid::Variable::emitCode {
    my $self = shift;
    print "(readSTRef (";
    print $self->{name};
    print "))"
}

sub Stupid::Variable::emitLValue {
    my $self = shift;
    print '(return ';
    print $self->{name};
    print ') ';
}

sub Stupid::Type::UInt32::emitHaskellType {
    my $self = shift;
    my $name = shift;

    print "Uint32";
}

# TODO is this the stupid typename or the target language type name?
sub Stupid::Type::UInt32::typeName {
    my $self = shift;

    return 'Uint32';
}

sub Stupid::Type::UInt8::typeName {
    my $self = shift;

    return 'Uint8';
}

# TODO what should arrays look like? We know the size.
# Is the size static in the source code or is it allowed to be computed
# at run time? do we care here? (probably not)
# Is an array an stref containing a list of STRefs, or is it just a
# list of STRefs? If we want to be able to assign to the array as a whole
# (eg treat is as an lvalue) then we need to. Its more overhead, though...


sub Stupid::Type::Array::emitHaskellType {
    my $self = shift;
    my $name = shift;

    print ' [STRef s ',$self->{type}->typeName(), '] ';

    # TODO populate members ... '[', $self->{size}->value(), ']';
}

sub Stupid::ArrayRef::emitLValue {
    my $self = shift;
    print '( (';
    $self->{array}->emitCode();
    print ') !!! (';
    $self->{offset}->emitCode();
    print ')) ';
}

sub Stupid::ArrayRef::emitCode {
    my $self = shift;
    print '( readSTRef $< (';
    $self->emitLValue();
    print '))';
}

sub Stupid::HexValue::emitCode {
    my $self = shift;
    print '( return ';
    print $self->{value}->as_hex();
    print ')';
# shouldn't need a U on the end of this, but maybe can put in type spec to force type to be unsigned (either here or higher up)
}

sub Stupid::DecimalValue::emitCode {
    my $self = shift;
    print '( return ';
    print $self->{value};
    print ')';
# shouldn't need a U on the end of this, but maybe can put in type spec to force type to be unsigned (either here or higher up)
}

sub Stupid::ArrayValue::emitCode {
    my $self = shift;

    #TODO this should be STRefs. (actually ideally would be MArrays but I
    # can't get those sorted in my head, typewise)
    print '(do { r <- mapM (\a -> do { nxv <- a ; newSTRef nxv }) [ ';
    my $first = 1;
    foreach my $v (@{$self->value()}) {
	print ', ' if !$first;
	$v->emitCode();
	$first = 0;
    }
    print ' ]  ; return r } )';
}

sub Stupid::And32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `and32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::And8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `and8` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::BOr::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `bor` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Eq32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `eq32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::LShift32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `lshift32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::LShift8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `lshift8` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Mask32To8::emitCode {
    my $self = shift;

    print '( horribleCastTo8M $< (';
    $self->{operand}->emitCode();
    print ' `and32` (return 0xff)))';
}

sub Stupid::Minus32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `minus32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Mod32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `mod32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Ne32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `ne32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Not32::emitCode {
    my $self = shift;

    print '(not32 ';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::Not8::emitCode {
    my $self = shift;

    print '(not8 ';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::Or8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `or8` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Plus32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `plus32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::RRotate32::emitCode {
    my $self = shift;

# We stipulate that expressions are side-effect free, so we can do this!
    print '((';
    $self->{left}->emitCode();
    print ' `rshift32` ';
    $self->{right}->emitCode();
    print ') `or32` (';
    $self->{left}->emitCode();
    print ' `lshift32` ((return 32) `minus32` ';
    $self->{right}->emitCode();
    print ')))';
}

sub Stupid::RShift32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `rshift32` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Widen8To32::emitCode {
    my $self = shift;

    print '(horribleCastTo32M $< ';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::XOr32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `xor32` ';
    $self->{right}->emitCode();
    print ')';
}

1;
