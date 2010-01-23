package Stupid::C;

use strict;

sub Stupid::Null::emitCode {
    my $self = shift;

    $self->{left}->emitCode();
    print ";\n";
    $self->{right}->emitCode();
}

sub Stupid::Comment::emitCode {
    my $self = shift;

    print "/* $self->{comment} */\n";
}

sub Stupid::Set::emitCode {
    my $self = shift;

    $self->{left}->emitLValue();
    print ' = ';
    $self->{right}->emitCode();
}

sub Stupid::UInt32Variable::emitLValue {
    my $self = shift;

    print $self->{name};
}

sub Stupid::HexValue::emitCode {
    my $self = shift;

    print "$self->{value}U";
}

1;
