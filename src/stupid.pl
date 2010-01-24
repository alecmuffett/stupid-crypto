#!/usr/bin/perl -w

use strict;

use grammar;
use File::Slurp;
use Carp;

$| = 1;

my $sourceFile = shift;

my $code = read_file($sourceFile);

$::Context = new Stupid::Context();

my $parser = new grammar();
$parser->YYData->{code} = $code;
my $ptree = $parser->YYParse(yylex => \&lexer,
			     yyerror => \&yyerror,
			     yydebug => 6);

use Data::Dumper; print Data::Dumper->Dump([\$ptree]);

#$ptree->value();
#$::Context->dumpSymbols();

my $wrapped = new Stupid::LanguageWrapper($ptree);

use Stupid::C;

$wrapped->emitCode();

sub lexer {
    my $parser = shift;

    my $code = $parser->YYData->{code};

    # skip newlines
    while(substr($code, 0, 1) eq "\n") {
	print "skip\n";
	$code = substr($code, 1);
    }

    # remove leading whitespace
    $code =~ s/^\s*//s;

#    use Data::Dumper; print Data::Dumper->Dump([\$code]);
    # EOF
    return ('',undef) if $code eq '';

    my ($type, $value);
    # Keyword
    if($code =~ /^(array|uint8|uint32|if|else|while|and8|and32|bor|eq32|lshift32|lshift8|mask32to8|minus32|mod32|ne32|not32|not8|or8|plus32|rrotate32|rshift32|widen8to32|xor32|\(|\)|\[|\]|{|}|,|;|=)(.*)$/s) {
	$type = $1;
	$value = undef;
	$code = $2;
    # STRING
    } elsif($code =~ /^"([^"]+)"(.*)$/s) {
	$type = 'STRING';
	$value = $1;
	$code = $2;
    # hex VALUE
    } elsif($code =~ /^0x([0-9a-f]+)(.*)$/s) {
	$type = 'VALUE';
	$value = new Stupid::HexValue($1);
	$code = $2;
    # decimal VALUE
    } elsif($code =~ /^([0-9]+)(.*)$/s) {
	$type = 'VALUE';
	$value = new Stupid::DecimalValue($1);
	$code = $2;
    # WORD
    } elsif($code =~ /^([A-Za-z][A-Za-z0-9_]*)(.*)$/s) {
	$type = 'WORD';
	$value = $1;
	$code = $2;
    # FAIL!!!
    } else {
	error($parser, "Can't parse");
    }

    print "type = $type";
    print " value = $value" if defined $value;
    print "\n";

    $parser->YYData->{code} = $code;
    return ($type, $value);
}

sub yyerror {
    my $parser = shift;

    $parser->YYData->{code} =~ /(.*)$/m;
    print STDERR "Failed at $1\n";
    print STDERR '(', $parser->YYData->{code}, ')';
}

sub error {
    my $parser = shift;
    my $error = shift;

    $parser->YYData->{code} =~ /(.*)$/m;
    croak "$error: $1\n";
}

package Stupid::LanguageWrapper;

use strict;

sub new {
    my $class = shift;
    my $tree = shift;

    my $self = {};
    bless $self, $class;

    $self->{tree} = $tree;

    return $self;
}

package Stupid::Context;

use strict;
use Carp;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    return $self;
}

sub addSymbol {
    my $self = shift;
    my $symbol = shift;

    $self->{symbols}->{$symbol->name()} = $symbol;
}

sub findSymbol {
    my $self = shift;
    my $name = shift;

    my $symbol = $self->{symbols}->{$name};
    croak "Can't find symbol $name" if !$symbol;
    return $symbol
}

sub dumpSymbols {
    my $self = shift;

    print "Symbol Dump\n";
    print "===========\n";
    foreach my $name (keys %{$self->{symbols}}) {
	print "$name = ", $self->asString($self->{symbols}->{$name}->value()), "\n";
    }
}

sub asString {
    my $self = shift;
    my $thing = shift;

#use Data::Dumper; print Data::Dumper->Dump([\$thing], ['thing']);
    my $str = '';
    if(ref $thing eq 'ARRAY') {
	$str = join(', ', map { $self->asString($_) } @$thing);
    } elsif(ref $thing eq 'Math::BigInt') {
	$str = 'BigInt: '.$thing->as_hex();
    } else {
	$str = ref($thing).': '.$thing->value()->as_hex();
    }
	
    return $str;
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

package Stupid::ArrayRef;

use strict;

# A followed by B (as in two statements). No value.

sub new {
    my $class = shift;
    my $array = shift;
    my $offset = shift;

    my $self = {};
    bless $self, $class;

    $self->{array} = $array;
    $self->{offset} = $offset;

    return $self;
}

sub value {
    my $self = shift;

    # FIXME range checking
    return $self->{array}->value()->[$self->{offset}->value()];
}

sub setValue {
    my $self = shift;
    my $value = shift;

    # FIXME range checking
    return $self->{array}->value()->[$self->{offset}->value()] = $value;
}

package Stupid::StatementList;

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{statements} = [];

    return $self;
}

sub appendStatement {
    my $self = shift;
    my $statement = shift;

    push @{$self->{statements}}, $statement;
}

sub value {
    my $self = shift;

    for my $s (@{$self->{statements}}) {
	$s->value();
    }

    return undef;
}

package Stupid::Statement;

use strict;

sub new {
    my $class = shift;
    my $expr = shift;

    my $self = {};
    bless $self, $class;

    $self->{expr} = $expr;

    return $self;
}

package Stupid::If;

use strict;

sub new {
    my $class = shift;
    my $cond = shift;
    my $then = shift;
    my $else = shift;

    my $self = {};
    bless $self, $class;

    $self->{cond} = $cond;
    $self->{then} = $then;
    $self->{else} = $else;

    return $self;
}

package Stupid::While;

use strict;

sub new {
    my $class = shift;
    my $cond = shift;
    my $body = shift;

    my $self = {};
    bless $self, $class;

    $self->{cond} = $cond;
    $self->{body} = $body;

    return $self;
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

package Stupid::And32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::And8;

use strict;

# Unsigned decimal value, any length

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

package Stupid::BOr;

use strict;

# Unsigned decimal value, any length

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

package Stupid::Eq32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::LShift32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::LShift8;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type checking
    return $self->{left}->value()->blsft($self->{right}->value());
}

package Stupid::Minus32;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type and underflow checking
    return $self->{left}->value()->bsub($self->{right}->value());
}

package Stupid::Mod32;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type checking
    return $self->{left}->value()->bmod($self->{right}->value());
}

package Stupid::Ne32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::Mask32To8;

use strict;

# Unsigned decimal value, any length

sub new {
    my $class = shift;
    my $operand = shift;

    my $self = {};
    bless $self, $class;

    $self->{operand} = $operand;

    return $self;
}

package Stupid::Not32;

use strict;

# Unsigned decimal value, any length

sub new {
    my $class = shift;
    my $operand = shift;

    my $self = {};
    bless $self, $class;

    $self->{operand} = $operand;

    return $self;
}

package Stupid::Not8;

use strict;

# Unsigned decimal value, any length

sub new {
    my $class = shift;
    my $operand = shift;

    my $self = {};
    bless $self, $class;

    $self->{operand} = $operand;

    return $self;
}

package Stupid::Widen8To32;

use strict;

# Unsigned decimal value, any length

sub new {
    my $class = shift;
    my $operand = shift;

    my $self = {};
    bless $self, $class;

    $self->{operand} = $operand;

    return $self;
}

package Stupid::Or8;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type checking
    return $self->{left}->value()->bior($self->{right}->value());
}

package Stupid::Plus32;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type and overflow checking
    $self->{left}->value()->badd($self->{right}->value());
}

package Stupid::RRotate32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::RShift32;

use strict;

# Unsigned decimal value, any length

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

    # FIXME type checking
    $self->{left}->value()->brsft($self->{right}->value());
}

package Stupid::XOr32;

use strict;

# Unsigned decimal value, any length

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

package Stupid::Declare;

use strict;

# declaration of a variable

sub new {
    my $class = shift;
    my $context = shift;
    my $var = shift;
    my $init = shift;

    my $self = {};
    bless $self, $class;

    $self->{var} = $var;
    $self->{init} = $init;
    $context->addSymbol($var);

    return $self;
}

sub value {
    my $self = shift;

    $self->{var}->setValue($self->{init}->value());
}

package Stupid::Type::UInt32;

use strict;
use Carp;
use Math::BigInt;

# 32 bits, addition/subtraction wrap.

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    return $self;
}

sub checkValue {
    my $self = shift;
    my $value = shift;

    # Check value is in range
    croak "Bad uint32 value: $value" if $value->sign() ne '+'
	|| $value->bcmp(new Math::BigInt('0x100000000')) >= 0;
    $self->{value} = $value;
#    print "$self->{name} = ", $self->{value}->as_hex(), "\n";

    return $self->{value};
}

package Stupid::Type::UInt8;

use strict;
use Carp;
use Math::BigInt;

# 32 bits, addition/subtraction wrap.

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    return $self;
}

sub checkValue {
    my $self = shift;
    my $value = shift;

    # Check value is in range
    croak "Bad uint8 value: $value" if $value->sign() ne '+'
	|| $value->bcmp(new Math::BigInt('0x100')) >= 0;
    $self->{value} = $value;
#    print "$self->{name} = ", $self->{value}->as_hex(), "\n";

    return $self->{value};
}

package Stupid::Type::Array;

use strict;
use Carp;

sub new {
    my $class = shift;
    my $type = shift;
    my $size = shift;

    my $self = {};
    bless $self, $class;

    $self->{type} = $type;
    $self->{size} = $size;

    return $self;
}

sub checkValue {
    my $self = shift;
    my $value = shift;

    croak 'not an array' if ref($value) ne 'ARRAY';
    # FIXME check size and type of each value
}

package Stupid::Variable;

use strict;
use Carp;

# A variable of some type

sub new {
    my $class = shift;
    my $type = shift;
    my $name = shift;

    my $self = {};
    bless $self, $class;

    $self->{type} = $type;
    $self->{name} = $name;

    return $self;
}

sub setValue {
    my $self = shift;
    my $value = shift;

    confess "value is null" if !defined $value;
    $self->{type}->checkValue($value);
    $self->{value} = $value;
}

sub name {
    my $self = shift;

    return $self->{name};
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

# FIXME: rather than implement these I should do something more uniform, like
# wrap all BigInts (arrays don't wrap them). But in the interests of time..
sub bior {
    my $self = shift;
    my $right = shift;

    return $self->{value}->bior($right);
}

package Stupid::DecimalValue;

use strict;
use Math::BigInt;

# Unsigned decimal value, any length

sub new {
    my $class = shift;
    my $value = shift;

    my $self = {};
    bless $self, $class;

    $self->{value} = new Math::BigInt($value);

    return $self;
}

sub value {
    my $self = shift;

    return $self->{value};
}

package Stupid::ArrayValue;

use strict;
use Math::BigInt;

# Unsigned decimal value, any length

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{values} = [];

    return $self;
}

sub append {
    my $self = shift;
    my $value = shift;

    push @{$self->{values}}, $value;
}

sub value {
    my $self = shift;

    return $self->{values};
}
