package Stupid::Haskell;

# TODO a note about use of STRefs for all variables, and how maybe we could
# do static analysis to see if they're single write and then encode them as
# haskell variables rather than as STRefs

# Types in haskell must begin with an uppercase, which is reflected here

use strict;
use warnings;

sub Stupid::LanguageWrapper::emitCode {
    my $self = shift;

# TODO these imports could be re-exported by StupidStuff
    print "module StupidGenerated where\n";
    print "import Data.IORef\n";
    print "import Control.Monad.ST\n";
    print "import System.IO\n";
    print "import StupidStuff\n";
#    print "prog = ";
    $self->{tree}->emitCode();
}

## FUNCTODO
sub Stupid::TopLevelList::emitCode {
    my $self = shift;

    for my $f (@{$self->{topLevels}}) {
        $f->emitCode();
    }
}

sub Stupid::FunctionCall::emitCode {
    my $self = shift;

    $self->{function}->emitCall();
    print ' ';
    # $self->{function}->maybeAddSelf(); # do we ever need this? perhaps not... where is it used? for object-style invocations only, I think, and in those, the self is passed through a different syntax
    $self->{args}->emitParameters();
    # whats happening for return parameters here?
    print " ;\n";
}

## FUNCTODO
sub Stupid::Function::emitCode {
    my $self = shift;

    # first the type signature
    # the types used here are a bit unintuitive for haskell and so perhaps
    # should not be what is used in the long term; but it seems to
    # correspond fairly closely with the way that the C bindings are
    # implemented so may be easier for now.
    print $self->{name}, ' :: ';
    my $first;
    $first = $self->{returns}->emitReturnTypes();
    $first = $self->{args}->emitArgTypes($first);
    print " -> " if !$first;
    print " IO ()\n";

    # second the body
    print $self->{name}, ' ' ;
    # this should emit just the names, not the types (the types should
    # have been emitted above)
    $first = $self->{returns}->emitReturnDecls();
    $self->{args}->emitArgs($first);
    print " = ";
    $self->{body}->emitCode();
    print "\n";
}

sub Stupid::ArgList::emitArgTypes {
    my $self = shift;
    my $first = shift;

    for my $arg (@{$self->{args}}) {
        print ' -> ' if !$first;
	print "IORef ";
        $arg->emitHaskellType();
        $first = 0;
    }
    return $first;
}


sub Stupid::ArgList::emitReturnTypes {
    my $self = shift;

    my $first = 1;
    for my $arg (@{$self->{args}}) {
        print '->' if !$first;
        print "IORef ";
        $arg->emitHaskellType();
        $first = 0;
    }
    return $first;
}

sub Stupid::Declare::emitHaskellType {
    my $self = shift;

    $self->{var}->emitHaskellType();
}

sub Stupid::Variable::emitHaskellType {
    my $self = shift;

    $self->{type}->emitHaskellType();
}

# may need to make member references less ambigious if Haskell
# doesn't like structures with the same name
#
# at the moment (supporting ostreams but not structs) this is being
# used to get an output function, not a user-defined structure entry.
#
# however eventually it will be. so should that syntax be carried over
# into the haskell code or should this translater do something fancy?
#
sub Stupid::MemberRef::emitCode {
    my $self = shift;

    print '(';
    print 'get';
    print $self->{member}->{name};
    print ' (';
    $self->{owner}->emitCode();
    print '))';
}

sub Stupid::MemberRef::emitCall {
    my $self = shift;

    $self->emitCode();
}

sub Stupid::MemberRef::maybeAddSelf {
    my $self = shift;

    $self->{owner}->maybeAddSelf();
}

sub Stupid::Type::Struct::emitCode {
    my $self = shift;

    print "data Struct$self->{name} = MkStruct$self->{name} { \n";
    $self->{decls}->emitCode(); # TODO this decls probably do not look quite like variable declarations
    print "}\n";
}

sub Stupid::Type::StructInstance::emitHaskellType {
    my $self = shift;
    my $name = shift;

    print " $self->{name} ";
}


sub Stupid::AbstractDeclList::emitCode {
    my $self = shift;

    my $first = 1;
    foreach my $decl (@{$self->{decls}}) {
        if($first) {
            print " {- nocomma Stupid::AbstractDeclList::emitCode -}\n";
            $first=0;
        } else {
            print ", {- Stupid::AbstractDeclList::emitCode -}\n";
        }
        $decl->emitCode();
    }
}

sub Stupid::AbstractDeclare::emitCode {
    my $self = shift;

    print '  ';
    $self->{type}->emitDeclaration($self->{name});
}


# FUNCTODO
sub Stupid::ArgList::emitReturnDecls {
    my $self = shift;

    my $first = 1;
    for my $arg (@{$self->{args}}) {
        print ' ' if !$first;
        $arg->emitReturnDecl();
        $first = 0;
    }
    return $first;
}

# FUNCTODO
sub Stupid::ArgList::emitArgs {
    my $self = shift;
    my $first = shift;

    for my $arg (@{$self->{args}}) {
        print ' ' if !$first;
        $arg->emitArg();
        $first = 0;
    }
}

# FUNCTODO
sub Stupid::Declare::emitArg {
    my $self = shift;

    $self->{var}->emitArg();
}

sub Stupid::Variable::maybeAddSelf {
    my $self = shift;

    print "$self->{name}, " if $self->{type}->needsSelf();
}

#sub Stupid::Variable::emitMemberRef {
#    my $self = shift;
#    my $member = shift;
#    print ' XXX {- Variable::emitMemberRef -}';
#    print $member->{name};
#}

# FUNCTODO
sub Stupid::Variable::emitArg {
    my $self = shift;
    print $self->{name};
    # $self->{type}->emitArg($self->{name});
}

sub Stupid::Type::OStream::needsSelf {
    return 1;
}

sub Stupid::Type::OStream::emitArg {
    my $self = shift;
    my $name = shift;

    print "OutputStream $name";
}

sub Stupid::Type::OStream::accessor {
    return '';
}

sub Stupid::Type::OStream::emitHaskellType {
    print "Handle";
}

sub Stupid::MemberCall::emitCode {
    my $self = shift;

    my $m = $self->{member};


    my %methods = (
        'put' => 'writeToOutputStream'
    );

    $m = $methods{$m};

    print "($m)";
    print '(';
    $self->{owner}->emitCode();
    print ') (';
    $self->{args}->emitCode();
    print ");\n";
}


# FUNCTODO
sub Stupid::Type::UInt32::emitArg {
    my $self = shift;
    my $name = shift;

    $self->emitHaskellType($name);
}

#FUNCTODO
sub Stupid::Type::UInt8::emitArg {
    my $self = shift;
    my $name = shift;

    $self->emitHaskellType($name);
}




# FUNCTODO
sub Stupid::Declare::emitReturnDecl {
    my $self = shift;
    $self->{var}->emitReturnDecl();
}

# FUNCTODO
sub Stupid::Variable::emitReturnDecl {
    my $self = shift;
    print ' ', $self->{name}, ' ';
    # $self->{type}->emitReturnDecl($self->{name});
}

# FUNCTODO
 sub Stupid::Type::UInt32::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "uint32 *$name";
}

sub Stupid::Type::Array::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    $self->emitHaskellType($name);
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

# if this is only used for parameters to functions, maybe needs
# $< as separator?

# its not - its used in structs, and in function parameters too...
# in the haskell backend, those will have different separators -
# funciton parameters are space-separated, but struct definitions
# (haskell records) use a , between fields

sub Stupid::ExprList::emitCode {
    my $self = shift;
print '{- ExprList emitCode -}';
    my $first = 1;
    for my $expr (@{$self->{expressions}}) {
        print " (";
        $expr->emitCode();
        print ") ";
    }
}

sub Stupid::ExprList::emitParameters {
    my $self = shift;

print '{- ExprList emitParameters -}';

 #   my $first = 1;
    for my $expr (@{$self->{expressions}}) {
  #      print ', ' if !$first;
   #     $first = 0;
        $expr->emitParameter();
    }
}

sub Stupid::If::emitCode {
    my $self = shift;

    print "do {\n";
    print "xxxifcond <- ";
    $self->{cond}->emitCode();
    print "; \n if xxxifcond";
    print ' then ';
    $self->{then}->emitCode();
    print "  else ";
    $self->{else}->emitCode();
    print "\n";
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
    print ") (\n";
    $self->{body}->emitCode();
    print ");\n";
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
    print '(writeIORefInIO (';
    $self->{left}->emitLValue();
    print ' )) $< ( ';
    $self->{right}->emitCode();
    print ")\n";



#    print "do { writeInternalVar<-";
#    $self->{right}->emitCode();
#    print "; writeIORef ( " ;
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
    print ' <- (newIORef $< ( (';
    $init->emitCode();
    print ') :: IO  ';
    $self->{type}->emitHaskellType();
    print "));\n"
# I don't know how to emit types for arrays as they are represented here
# (because of the free variable in STRefs :(

    # (do { assignV <- ';  # TODO assignV might clash with the namespace of stupid variables (and in general, stupid variable names shouldn't be permitted to clash with haskell symbols that may be defined elsewhere, so probably should mangle them...)
    #$init->emitCode();
    #print ' ; newIORef ( assignV ';
    ### print ' :: ';
    ### $self->{type}->emitHaskellType();
    ### This works for single values, but seems to get upset with STRefs as
    ### I'm leaving a type variable free (STRef s Uint32, for example)
    ### so for now cannot assert types on arrays :(
    #print " ) }) ; \n";
}

sub Stupid::Variable::emitCode {
    my $self = shift;
    print "(readIORef (";
    print $self->{name};
    print "))"
}

sub Stupid::Variable::emitLValue {
    my $self = shift;
    print '(return ';
    print $self->{name};
    print ') ';
}

# is this for the left or the right? what we output varies in Haskell...
sub Stupid::Variable::emitParameter {
   my $self = shift;
   $self->emitCode();
}

sub Stupid::Type::UInt32::emitHaskellType {
    my $self = shift;
    my $name = shift;

    print "Uint32";
}

sub Stupid::Type::UInt32::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "$name :: Uint32";
}


# TODO is this the stupid typename or the target language type name?
sub Stupid::Type::UInt32::typeName {
    my $self = shift;

    return 'Uint32';
}

sub Stupid::Type::UInt8::emitHaskellType {
    my $self = shift;
    my $name = shift;

    print "Uint8";
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

    print ' [IORef ',$self->{type}->typeName(), '] ';

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

sub Stupid::ArrayRef::emitParameter {
    my $self = shift;

    $self->emitLValue();
}


sub Stupid::ArrayRef::emitCode {
    my $self = shift;
    print '( readIORef $< (';
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

sub Stupid::DecimalValue::emitParameter {
    my $self = shift;

    $self->emitCode();
}


sub Stupid::ArrayValue::emitCode {
    my $self = shift;

    #TODO this should be STRefs. (actually ideally would be MArrays but I
    # can't get those sorted in my head, typewise)
    print '(do { r <- mapM (\a -> do { nxv <- a ; newIORef nxv }) [ ';
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

sub Stupid::BAnd::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `band` ';
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

sub Stupid::Ge8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `ge8` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Le8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `le8` ';
    $self->{right}->emitCode();
    print ')';
}



sub Stupid::Ge32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `ge32` ';
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

sub Stupid::Mod8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `mod8` ';
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

sub Stupid::Minus8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `minus8` ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Plus8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' `plus8` ';
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
