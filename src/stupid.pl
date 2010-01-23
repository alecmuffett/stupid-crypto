#!/usr/bin/perl -w

use strict;

use grammar;
use File::Slurp;
use Carp;

my $sourceFile = shift;

my $code = read_file($sourceFile);

$::Context = new Stupid::Context();

my $parser = new grammar();
$parser->YYData->{code} = $code;
my $ptree = $parser->YYParse(yylex => \&lexer,
			     yyerror => \&yyerror,
			     yydebug => 6);
$ptree->value();
$::Context->dumpSymbols();

sub lexer {
    my $parser = shift;

    my $code = $parser->YYData->{code};

    use Data::Dumper; print Data::Dumper->Dump([\$code]);
    # skip newlines
    while(substr($code, 0, 1) eq "\n") {
	print "skip\n";
	$code = substr($code, 1);
    }

    # remove leading whitespace
    $code =~ s/^\s*//s;

    # EOF
    return ('',undef) if $code eq '';

    my ($type, $value);
    # Comment
    if($code =~ /^\#(.*?)$(.*)/sm) {
	print "comment = $1\n";
	$type = 'COMMENT';
	$value = $1;
	$code = $2;
    # Keyword
    } elsif($code =~ /^(uint32|=)(.*)$/s) {
	$type = $1;
	$value = undef;
	$code = $2;
    # VALUE
    } elsif($code =~ /^0x([0-9a-f]+)(.*)$/s) {
	$type = 'VALUE';
	$value = new Stupid::HexValue($1);
	$code = $2;
    # WORD
    } elsif($code =~ /^(\S+)(.*)$/s) {
	$type = 'WORD';
	$value = $1;
	$code = $2;
    # FAIL!!!
    } else {
	error($parser, "Can't parse");
    }

    $parser->YYData->{code} = $code;
    return ($type, $value);
}

sub yyerror {
    my $parser = shift;

    $parser->YYData->{code} =~ /(.*)$/m;
    print STDERR "Failed at $1\n";
}

sub error {
    my $parser = shift;
    my $error = shift;

    $parser->YYData->{code} =~ /(.*)$/m;
    print STDERR "$error: $1\n";
}

package Stupid::Context;

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    return $self;
}

sub addSymbol {
    my $self = shift;
    my $symbol = shift;

    $self->{symbols}->{$symbol->{name}} = $symbol;
}

sub dumpSymbols {
    my $self = shift;

    print "Symbol Dump\n";
    print "===========\n";
    foreach my $name (keys %{$self->{symbols}}) {
	print "$name = ", $self->{symbols}->{$name}->value()->as_hex(), "\n";
    }
}

package Stupid::Comment;

use strict;

sub new {
    my $class = shift;
    my $comment = shift;

    my $self = {};
    bless $self, $class;

    $self->{comment} = $comment;

    return $self;
}

sub value {
    my $self = shift;

    print "# $self->{comment}\n";
    return undef;
}

package Stupid::Null;

use strict;

# A followed by B (as in two statements), value is B.

sub new {
    my $class = shift;
    my $l = shift;
    my $r = shift;

    my $self = {};
    bless $self, $class;

    $self->{left} = $l;
    $self->{right} = $r;

    return $self;
}

sub value {
    my $self = shift;

    $self->{left}->value();
    return $self->{right}->value();
}

package Stupid::Set;

use strict;

# A = B

sub new {
    my $class = shift;
    my $l = shift;
    my $r = shift;

    my $self = {};
    bless $self, $class;

    $self->{left} = $l;
    $self->{right} = $r;

    return $self;
}

sub value {
    my $self = shift;

    return $self->{left}->setValue($self->{right}->value());
}

package Stupid::UInt32Variable;

use strict;
use Carp;
use Math::BigInt;

# 32 bits, addition/subtraction wrap.

sub new {
    my $class = shift;
    my $context = shift;
    my $name = shift;

    my $self = {};
    bless $self, $class;

    $self->{name} = $name;
    $context->addSymbol($self);

    return $self;
}

sub name {
    my $self = shift;

    return $self->{name};
}

sub setValue {
    my $self = shift;
    my $value = shift;

    # Check value is in range
    croak "Bad uint32 value: $value" if $value->sign() ne '+'
	|| $value->bcmp(new Math::BigInt('0x100000000')) >= 0;
    $self->{value} = $value;
    print "$self->{name} = ", $self->{value}->as_hex(), "\n";

    return $self->{value};
}

sub value {
    my $self = shift;

    return $self->{value};
}

package Stupid::HexValue;

use strict;
use Math::BigInt;

# Unsigned hex value, any length

sub new {
    my $class = shift;
    my $value = shift;

    my $self = {};
    bless $self, $class;

    $self->{value} = new Math::BigInt("0x$value");

    return $self;
}

sub value {
    my $self = shift;

    return $self->{value};
}
