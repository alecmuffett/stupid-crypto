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

1;
