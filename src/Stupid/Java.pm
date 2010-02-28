package Stupid::Java;

use strict;

# a note on mapping between java types and stupid types:
# for now, I'm using a "bigger" signed Java type (more bits)
# to hold unsigned types. This won't give the correct
# rollover behaviour with some normal operations, so I'll have to
# implement those operations myself... however, as a first approximation
# it will do.

# uint32 is stored in a long (64 bit signed)
# uint8 is stored in a short (16 bit signed)


sub Stupid::LanguageWrapper::emitCode {
    my $self = shift;

    # this should eventually be public. to do that the name of the
    # class needs to correspond to the name of the generated .java file
    print "import Stupid.Mutable;\n";
    print "class StupidGenerated {\n";
    $self->{tree}->emitCode();
    print "}\n";
}

sub Stupid::FunctionList::emitCode {
    my $self = shift;

    for my $f (@{$self->{functions}}) {
	$f->emitCode();
    }
}

sub Stupid::Function::emitCode {
    my $self = shift;

    print 'public void ', $self->{name}, '(';
    my $first = $self->{returns}->emitReturnDecls();
    $self->{args}->emitArgs($first);
    print ") {\n";
    $self->{body}->emitCode();
    print "}\n";
}

sub Stupid::ArgList::emitReturnDecls {
    my $self = shift;

    my $first = 1;
    for my $arg (@{$self->{args}}) {
	print ',' if !$first;
	$arg->emitReturnDecl();
	$first = 0;
    }
    return $first;
}

sub Stupid::ArgList::emitArgs {
    my $self = shift;
    my $first = shift;

    for my $arg (@{$self->{args}}) {
	print ',' if !$first;
	$arg->emitArg();
	$first = 0;
    }
}

sub Stupid::Declare::emitReturnDecl {
    my $self = shift;

    $self->{var}->emitReturnDecl();
}

sub Stupid::Declare::emitArg {
    my $self = shift;

    $self->{var}->emitArg();
}

sub Stupid::Declare::emitCode {
    my $self = shift;

    $self->{var}->emitDeclaration($self->{init});
    print ";\n";
}

sub Stupid::StatementList::emitCode {
    my $self = shift;

    for my $s (@{$self->{statements}}) {
	$s->emitCode();
    }
}

sub Stupid::Statement::emitCode {
    my $self = shift;

    $self->{expr}->emitCode();
    print ";\n";
}

sub Stupid::MemberCall::emitCode {
    my $self = shift;

    $self->{owner}->emitCode();
    print "->$self->{member}(";
    # something of a hack at this stage
    $self->{owner}->emitCode();
    print '->info, ';
    # end of hack
    $self->{args}->emitCode();
    print ");\n";
}

sub Stupid::ExprList::emitCode {
    my $self = shift;

    my $first = 1;
    for my $expr (@{$self->{expressions}}) {
	print ', ' if !$first;
	$first = 0;
	$expr->emitCode();
    }
}

sub Stupid::If::emitCode {
    my $self = shift;

    print 'if (';
    $self->{cond}->emitCode();
    print ") {\n";
    $self->{then}->emitCode();
    print "} else {\n";
    $self->{else}->emitCode();
    print "}\n";
}

sub Stupid::While::emitCode {
    my $self = shift;

    print 'while (';
    $self->{cond}->emitCode();
    print ") {\n";
    $self->{body}->emitCode();
    print "}\n";
}

sub Stupid::Comment::emitCode {
    my $self = shift;

    print "/* $self->{comment} */\n";
}

sub Stupid::Comment::emitDeclarations {
    my $self = shift;

    print "/* (DECL) $self->{comment} */\n";
}

sub Stupid::Set::emitCode {
    my $self = shift;

    $self->{left}->emitLValue();
    print ' =  (';
    print $self->{left}->{type}->typeName();
    # when this is an array access, then we don't get a type object here...

    print ')(';
    $self->{right}->{expectedType} = $self->{left}->{type};
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Variable::emitReturnDecl {
    my $self = shift;

    $self->{type}->emitReturnDecl($self->{name});
}

sub Stupid::Variable::emitArg {
    my $self = shift;

    $self->{type}->emitArg($self->{name});
}

sub Stupid::Variable::emitDeclaration {
    my $self = shift;
    my $init = shift;

    $self->{type}->emitDeclaration($self->{name});
    print " = ";
    $self->{type}->emitConstructor();
    print ";\n";
    $init->{expectedType} = $self->{type};
    $self->emitLValue();
    print ' = (';
    print $self->{type}->typeName();
    print ')(';
    $init->emitCode();
    print ')';
}

sub Stupid::Variable::emitCode {
    my $self = shift;

    print $self->{name};
    print $self->{type}->accessor();
}

sub Stupid::Variable::emitLValue {
    my $self = shift;

    $self->{type}->dereference() if $self->{isReturn};
    print $self->{name};
    print $self->{type}->accessor();
}

sub Stupid::Type::UInt32::dereference {
    my $self = shift;

    print '';
}

sub Stupid::Type::UInt32::accessor {
    return '.value';
}

sub Stupid::Type::UInt32::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "Mutable<Long> $name";
}

sub Stupid::Type::UInt32::emitArg {
    my $self = shift;
    my $name = shift;

    $self->emitDeclaration($name);
}

sub Stupid::Type::UInt32::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "Mutable<Long> $name";
}

sub Stupid::Type::UInt32::emitConstructor {
    my $self = shift;
    print " new Mutable<Long>()";
}


sub Stupid::Type::UInt32::typeName {
    my $self = shift;

    return 'long';
}

sub Stupid::Type::UInt8::typeName {
    my $self = shift;

    return 'short';
}

sub Stupid::Type::UInt8::emitArg {
    my $self = shift;
    my $name = shift;

    $self->emitDeclaration($name);
}

sub Stupid::Type::UInt8::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "Mutable<Short> $name";
}

sub Stupid::Type::UInt8::emitConstructor {
    my $self = shift;
    print " new Mutable<Short>()";
}

sub Stupid::Type::UInt8::dereference {
    my $self = shift;

    print '';
}

sub Stupid::Type::UInt8::accessor {
    return '.value';
}

sub Stupid::Type::UInt8::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "Mutable<Short> $name";
}

sub Stupid::Type::OStream::emitArg {
    my $self = shift;
    my $name = shift;

    print "stupid_ostream *$name";
}

sub Stupid::Type::Array::accessor {
    return ""; # arrays don't get wrapped inside anything so no need to unwrap them
}

sub Stupid::Type::Array::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    $self->emitDeclaration($name);
}

sub Stupid::Type::Array::emitArg {
    my $self = shift;
    my $name = shift;

    $self->emitDeclaration($name);
}

sub Stupid::Type::Array::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print $self->{type}->typeName(), ' ', $name, '[]';
}

# TODO should initialise to default values? (all 0s for numerics)
# Using null here means that arrays will not work unless they're
# explicitly initialised

sub Stupid::Type::Array::emitConstructor {
    my $self = shift;
    print " null ";
}


sub Stupid::Type::Array::typeName {
    my $self = shift;
    my $tn = $self->{type}->typeName();
    return "${tn}[]";
}

sub Stupid::Type::Array::memberType {
    my $self = shift;
    return $self->{type};
}

sub Stupid::ArrayRef::emitLValue {
    my $self = shift;

    # we'll set our own type information here
    # it might be better at some type propagation stage,
    # but for now, whatever...
    $self->{type} = $self->{array}->{type}->memberType();
    $self->{array}->emitCode();
    print '[(int)(0+('; # now that makes unboxing happen, which wouldn't otherwise
    $self->{offset}->emitCode();
    print '))]';
}

sub Stupid::ArrayRef::emitCode {
    my $self = shift;

    $self->emitLValue();
}

sub Stupid::HexValue::emitCode {
    my $self = shift;

    print $self->{value}->as_hex();
}

sub Stupid::DecimalValue::emitCode {
    my $self = shift;

    print $self->{value};
}

sub Stupid::ArrayValue::emitCode {
    my $self = shift;
    my $tn = $self->{expectedType}->typeName();
    print "new $tn { ";
    my $first = 1;
    foreach my $v (@{$self->value()}) {
	print ', ' if !$first;
	$v->emitCode();
	$first = 0;
    }
    print ' }';
}

sub Stupid::And32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' & ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::And8::emitCode {
    my $self = shift;

    print '(short)(';
    $self->{left}->emitCode();
    print ' & ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::BAnd::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' && ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::BOr::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' || ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Eq32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' == ';
    $self->{right}->emitCode();
    print ')';
}


sub Stupid::Ge8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' >= ';
    $self->{right}->emitCode();
    print ')';
}


sub Stupid::Le8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' <= ';
    $self->{right}->emitCode();
    print ')';
}



sub Stupid::LShift32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' << ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::LShift8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' << ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Mask32To8::emitCode {
    my $self = shift;

    print '((short)(';
    $self->{operand}->emitCode();
    print '&0xff))';
}

sub Stupid::Minus8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' - ';
    $self->{right}->emitCode();
    print ')';
}


sub Stupid::Minus32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' - ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Mod8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' % ';
    $self->{right}->emitCode();
    print ')';
}


sub Stupid::Mod32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' % ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Ne32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' != ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Not32::emitCode {
    my $self = shift;

    print '(~';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::Not8::emitCode {
    my $self = shift;

    print '(~';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::Or8::emitCode {
    my $self = shift;

    print '(short)(';
    $self->{left}->emitCode();
    print ' | ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Plus8::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' + ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Plus32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' + ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::RRotate32::emitCode {
    my $self = shift;

# We stipulate that expressions are side-effect free, so we can do this!
    print '((';
    $self->{left}->emitCode();
    print ' >> ';
    $self->{right}->emitCode();
    print ') | (';
    $self->{left}->emitCode();
    print ' << (32 - ';
    $self->{right}->emitCode();
    print ')))';
}

sub Stupid::RShift32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' >> ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Widen8To32::emitCode {
    my $self = shift;

    print '((long)';
    $self->{operand}->emitCode();
    print ')';
}

sub Stupid::XOr32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' ^ ';
    $self->{right}->emitCode();
    print ')';
}

1;
