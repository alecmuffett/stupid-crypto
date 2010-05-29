package Stupid2::C;

use strict;
use warnings;

sub Stupid2::Type::Int::typeName {
    my $self = shift;

    my $base = 'uint';
    $base = 'int' if $self->{width}->isSigned();
    return $base . $self->{width}->bits();
}

sub Stupid2::Type::Int::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print $self->typeName(), " $name";
}

sub Stupid2::Type::Int::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print $self->typeName(), " *$name";
}

sub Stupid2::Type::Int::emitArg {
    my $self = shift;
    my $name = shift;

    print 'const ';
    $self->emitDeclaration($name);
}

sub Stupid2::Type::Int::dereference {
    my $self = shift;

    print '*';
}

sub Stupid2::Type::Int::emitPointer {
    my $self = shift;

    print '&';
}

# FIXME: bad name for this function
sub Stupid2::Type::Int::emitParameter {
    my $self = shift;
}

sub Stupid2::Equals::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' == ';
    $self->{right}->emitCode();
    print ')';
}


1;
