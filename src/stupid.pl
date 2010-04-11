#!/bin/sh
eval 'exec perl -x -w $0 ${1+"$@"}'

#!perl actually starts here.

use strict;

use grammar;
use File::Slurp;
use Carp;
use Getopt::Long;

$| = 1;

my $language;
our $debug;

croak if !GetOptions("language=s" => \$language,
    "debug" => \$debug);
croak "Must specify an output language" if !$language;

my $sourceFile = shift;

my $code = read_file($sourceFile);

our @keywords;
initLexer();
$::Context = new Stupid::Context();

my $parser = new grammar();
$parser->YYData->{code} = $code;
my $ptree = $parser->YYParse(yylex => \&lexer,
			     yyerror => \&yyerror,
			     yydebug => $debug ? 6 : 0);

use Data::Dumper; $Data::Dumper::Indent=1; print STDERR Data::Dumper->Dump([\$ptree]) if $debug;

#$ptree->value();
#$::Context->dumpSymbols();

my $wrapped = new Stupid::LanguageWrapper($ptree);
$wrapped->{sourceFile} = $sourceFile;

eval "use Stupid::$language";
croak $@ if $@;

$wrapped->emitCode();

exit 0;

sub initLexer {
    @keywords = qw(array uint8 uint32 ostream
 function struct if else while
 and8 and32 band bor eq32 ge8 le8 lshift32 lshift8 mask32to8 minus8 minus32
 mod8 mod32 ne32 ne8 not32 not8 or8 plus8 plus32 rrotate32 rshift32 widen8to32
 wrapplus32 xor32);
}

sub lexer {
    my $parser = shift;

    my $code = $parser->YYData->{code};

    # skip newlines
    while(substr($code, 0, 1) eq "\n") {
	$code = substr($code, 1);
    }

    # remove leading whitespace
    $code =~ s/^\s*//s;

#    use Data::Dumper; print Data::Dumper->Dump([\$code]);
    # EOF
    return ('',undef) if $code eq '';

    my ($type, $value);
    # Punctuation
    if($code =~ /^(\(|\)|\[|\]|{|}|,|;|=|\.)(.*)$/s) {
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
    } elsif($code =~ /^'(.)'(.*)$/s) {
	$type = 'CHAR';
	$value = new Stupid::DecimalValue(ord($1));
	$code = $2;
    # WORD
    } elsif($code =~ /^([A-Za-z][A-Za-z0-9_]*)(.*)$/s) {
	$value = $1;
	# Keyword
	if(grep { $_ eq $value } @keywords) {
	    $type = $value;
	    $value = undef;
	} else {
	    $type = 'WORD';
	}
	$code = $2;
    # FAIL!!!
    } else {
	error($parser, "Can't parse");
    }

    if($debug) {
	print STDERR "type = $type";
	print STDERR " value = $value" if defined $value;
	print STDERR "\n";
    }

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

package Stupid;

use strict;

sub ArrayFromString {
    my $str = shift;

    my $t = new Stupid::ArrayValue();
    foreach my $c (split //, $str) {
	$t->append(new Stupid::DecimalValue(ord($c)));
    }

    return $t;
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

sub addStruct {
    my $self = shift;
    my $struct = shift;

    $self->{structs}->{$struct->{name}} = $struct;
}

sub findStruct {
    my $self = shift;
    my $name = shift;

    my $struct = $self->{structs}->{$name};
    croak "Can't find struct $name" if !$struct;
    return $struct;
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

package Stupid::Function;

use strict;

sub new {
    my $class = shift;
    my $context = shift;
    my $name = shift;
    my $returns = shift;
    my $args = shift;
    my $body = shift;

    my $self = {};
    bless $self, $class;

    $self->{name} = $name;
    $self->{returns} = $returns;
    $self->{args} = $args;
    $self->{body} = $body;

    $context->addSymbol($self);

    return $self;
}

sub name {
    my $self = shift;

    return $self->{name};
}

package Stupid::TopLevelList;

# FIXME: topl-level classes should inherit from a TopLevel class.

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{topLevels} = [];

    return $self;
}

sub appendTopLevel {
    my $self = shift;
    my $tl = shift;

    push @{$self->{topLevels}}, $tl;
}

package Stupid::Type::Struct;

use strict;

sub new {
    my $class = shift;
    my $context = shift;
    my $name = shift;
    my $decls = shift;

    my $self = {};
    bless $self, $class;

    $self->{name} = $name;
    $self->{decls} = $decls;

    $context->addStruct($self);

    return $self;
}

sub findMember {
    my $self = shift;
    my $name = shift;

    return $self->{decls}->findDeclaration($name);
}

package Stupid::Type::StructInstance;

use strict;

sub new {
    my $class = shift;
    my $context = shift;
    my $name = shift;

    my $self = {};
    bless $self, $class;

    $self->{struct} = $context->findStruct($name);

    return $self;
}

sub findMember {
    my $self = shift;
    my $name = shift;

    return $self->{struct}->findMember($name);
}

package Stupid::AbstractDeclare;

use strict;

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

package Stupid::AbstractDeclList;

use strict;

use Carp;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{decls} = [];

    return $self;
}

sub appendAbstractDecl {
    my $self = shift;
    my $decl = shift;

    push @{$self->{decls}}, $decl;
}

sub findDeclaration {
    my $self = shift;
    my $name = shift;

    foreach my $decl (@{$self->{decls}}) {
	return $decl if $decl->{name} eq $name;
    }
    croak "Can't find declaration of $name";
}

package Stupid::ExprList;

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{expressions} = [];

    return $self;
}

sub appendExpr {
    my $self = shift;
    my $expr = shift;

    push @{$self->{expressions}}, $expr;
}

sub isEmpty {
    my $self = shift;

    return $#{$self->{expressions}} == -1;
}

package Stupid::FunctionCall;

use strict;

sub new {
    my $class = shift;
    my $function = shift;
    my $args = shift;

    my $self = {};
    bless $self, $class;

    $self->{function} = $function;
    $self->{args} = $args;

    return $self;
}

package Stupid::MemberRef;

use strict;

sub new {
    my $class = shift;
    my $owner = shift;
    my $member = shift;

    my $self = {};
    bless $self, $class;

    $self->{owner} = $owner;
    $self->{member} = $self->{owner}->findMember($member);

    return $self;
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

package Stupid::ArgList;

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{args} = [];

    return $self;
}

sub appendArg {
    my $self = shift;
    my $arg = shift;

    push @{$self->{args}}, $arg;
}

sub markAsReturn {
    my $self = shift;

    for my $a (@{$self->{args}}) {
	$a->markAsReturn();
    }
}

sub markAsArgument {
    my $self = shift;

    for my $a (@{$self->{args}}) {
	$a->markAsArgument();
    }
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

package Stupid::BAnd;

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

package Stupid::Le8;

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


package Stupid::Ge8;

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

package Stupid::Minus8;

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


package Stupid::Mod8;

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

package Stupid::Ne8;

use strict;

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

package Stupid::Plus8;

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


package Stupid::Plus32;

use strict;

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

package Stupid::WrapPlus32;

use strict;

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

sub markAsReturn {
    my $self = shift;

    $self->{var}->markAsReturn();
}

sub markAsArgument {
    my $self = shift;

    $self->{var}->markAsArgument();
}

sub value {
    my $self = shift;

    $self->{var}->setValue($self->{init}->value());
}

sub findMember {
    my $self = shift;
    my $member = shift;

    return $self->{var}->findMember($member);
}

package Stupid::Type::OStream::Put;

use strict;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{name} = 'put';

    return $self;
}

package Stupid::Type::OStream;

use strict;

use Carp;

sub new {
    my $class = shift;

    my $self = {};
    bless $self, $class;

    $self->{put} = new Stupid::Type::OStream::Put();

    return $self;
}

sub findMember {
    my $self = shift;
    my $name = shift;

    croak "ostreams do not have the member $name" if $name ne 'put';
    return $self->{put};
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
    $self->{isReturn} = 0;

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

sub markAsReturn {
    my $self = shift;

    $self->{isReturn} = 1;
}

sub markAsArgument {
    my $self = shift;

    $self->{isArgument} = 1;
}

sub findMember {
    my $self = shift;
    my $member = shift;

    return $self->{type}->findMember($member);
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
