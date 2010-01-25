package Stupid::C;

use strict;

sub Stupid::LanguageWrapper::emitCode {
    my $self = shift;

    $self->{tree}->emitCode();
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
    print ' = ';
    $self->{right}->emitCode();
}

sub Stupid::Variable::emitDeclaration {
    my $self = shift;
    my $init = shift;

    $self->{type}->emitDeclaration($self->{name});
    print ' = ';
    $init->emitCode();
}

sub Stupid::Variable::emitCode {
    my $self = shift;

    print $self->{name};
}

sub Stupid::Variable::emitLValue {
    my $self = shift;

    print $self->{name};
}

sub Stupid::Type::UInt32::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "uint32 $name";
}

sub Stupid::Type::UInt32::typeName {
    my $self = shift;

    return 'uint32';
}

sub Stupid::Type::UInt8::typeName {
    my $self = shift;

    return 'unsigned char';
}

sub Stupid::Type::Array::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print $self->{type}->typeName(), ' ', $name,
      '[', $self->{size}->value(), ']';
}

sub Stupid::ArrayRef::emitLValue {
    my $self = shift;

    $self->{array}->emitCode();
    print '[';
    $self->{offset}->emitCode();
    print ']';
}

sub Stupid::ArrayRef::emitCode {
    my $self = shift;

    $self->emitLValue();
}

sub Stupid::HexValue::emitCode {
    my $self = shift;

    print $self->{value}->as_hex(), 'U';
}

sub Stupid::DecimalValue::emitCode {
    my $self = shift;

    print $self->{value}, 'U';
}

sub Stupid::ArrayValue::emitCode {
    my $self = shift;

    print '{ ';
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

    print '(';
    $self->{left}->emitCode();
    print ' & ';
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

    print '((uint8)(';
    $self->{operand}->emitCode();
    print '&0xff))';
}

sub Stupid::Minus32::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' - ';
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

    print '(';
    $self->{left}->emitCode();
    print ' | ';
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
    print ' << (32U - ';
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

    print '((uint32)';
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