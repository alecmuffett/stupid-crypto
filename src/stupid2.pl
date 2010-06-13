#!/bin/sh
eval 'exec perl -x -w $0 ${1+"$@"}'

#!perl actually starts here. Note this throws all lines numbers out by 3.

use strict;

use grammar2;
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

my $parser = new grammar2();
$parser->YYData->{code} = $code;
my $ptree = $parser->YYParse(yylex => \&lexer,
			     yyerror => \&yyerror,
			     yydebug => $debug ? 6 : 0);

use Data::Dumper; $Data::Dumper::Indent=1; print STDERR Data::Dumper->Dump([\$ptree]) if $debug;

#$ptree->value();
#$::Context->dumpSymbols();

$ptree->deduceWidth();

use Data::Dumper; $Data::Dumper::Indent=1; print STDERR Data::Dumper->Dump([\$ptree]) if $debug;

my $wrapped = new Stupid::LanguageWrapper($ptree);
$wrapped->{sourceFile} = $sourceFile;

eval "use Stupid::$language";
croak $@ if $@;

eval "use Stupid2::$language";
croak $@ if $@;

$wrapped->emitCode();

exit 0;

sub initLexer {
    @keywords = qw(int array ostream
 function struct if else while);
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
    if($code =~ /^(\(|\)|\[|\]|{|}|,|;|\.|_|==|!=|\+|=)(.*)$/s) {
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
	$value = new Stupid2::HexValue($1);
	$code = $2;
    # decimal UVALUE
    } elsif($code =~ /^([0-9]+)u(.*)$/s) {
	$type = 'UVALUE';
	$value = new Stupid2::DecimalValue($1);
	$code = $2;
    # decimal VALUE
    } elsif($code =~ /^([0-9]+)(.*)$/s) {
	$type = 'VALUE';
	$value = new Stupid2::DecimalValue($1);
	$code = $2;
    } elsif($code =~ /^'(.)'(.*)$/s) {
	$type = 'CHAR';
	$value = new Stupid2::DecimalValue(ord($1));
	$code = $2;
    # WORD
    } elsif($code =~ /^([A-Za-z][A-Za-z0-9]*)(.*)$/s) {
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

package Stupid2::Bitwidth;

use strict;
use Carp;

sub new {
    my $class = shift;
    my $width = shift;
    my $signed = shift;

    $width = new Math::BigInt($width) if ref($width) eq '';

    my $self = {};
    bless $self, $class;

    confess ref($width) if ref($width) ne 'Math::BigInt';

    $self->{width} = $width;
    $self->{signed} = $signed;

    confess 'Weird width'
	if (!defined($self->{width}) && defined($self->{signed}))
	|| (defined($self->{width}) && !defined($self->{signed}));
			    

    return $self;
}

sub signed {
    my $self = shift;

    return $self->{signed};
}

sub bits {
    my $self = shift;

    return $self->{width};
}

sub asString {
    my $self = shift;

    if(!defined $self->{width}) {
	croak if defined $self->{signed};
	return '[undefined]';
    }

    my $str = $self->{width}->value();
    $str .= 'u' if !$self->{signed};
}

sub equals {
    my $self = shift;
    my $other = shift;

    return $self->{width} == $other->{width}
      && $self->{signed} == $other->{signed};
}

sub merge {
    my $self = shift;
    my $other = shift;

    croak ref($other) if ref($other) ne 'Stupid2::Bitwidth';

    $self->{width} = $other->{width} if !defined $self->{width};
    $self->{signed} = $other->{signed} if !defined $self->{signed};

    return if !defined $other->{width};

    confess 'Can\'t merge non-identical Bitwidths: '.$self->asString()
	.'/'.$other->asString() if !$self->equals($other);
}

package Stupid2::ArrayWidth;

use strict;
use Carp;

sub new {
    my $class = shift;
    my $count = shift;
    my $width = shift;

    croak ref($width) if ref($width) ne 'Stupid2::Bitwidth';

    my $self = {};
    bless $self, $class;

    $self->{count} = $count;
    $self->{width} = $width;

    return $self;
}

sub getArrayElementWidth {
    my $self = shift;

    return $self->{width};
}

sub merge {
    my $self = shift;
    my $other = shift;

    croak ref($other) if ref($other) ne 'Stupid2::ArrayWidth';

    if(ref($other) ne ref($self)) {
	croak 'Can\'t merge width of type '.ref($other)
	    .' with width of type '.ref($self);
    }
    croak 'Array widths differ' if !$self->{width}->equals($other->{width});
    $self->{count} = $other->{count} if !defined $self->{count};
    return if !defined $other->{count};
    croak 'Array counts differ' if $self->{count} != $other->{count};
}

package Stupid2::HasWidth;

# A base class for things with a width, intended for multiple
# inheritance Anything that inherits from this should be able to
# figure out its width from its children. If any of its children do
# not have a width set, then it should be set after
# deduction. Children should also be checked for consistency.

use strict;
use Carp;

sub width {
    my $self = shift;

    confess 'No width set' if !defined $self->{width};
    return $self->{width};
}

sub maybeWidth {
    my $self = shift;

    return $self->{width};
}

sub bits {
    my $self = shift;

    return $self->width()->bits();
}

sub signed {
    my $self = shift;

    return $self->width()->signed();
}

sub setWidth {
    my $self = shift;
    my $width = shift;

    if (!defined $width) {
	confess "Trying to unset width!" if defined $self->{width};
	return;
    }

    confess ref($width) if ref($width) ne 'Stupid2::Bitwidth'
	&& ref($width) ne 'Stupid2::ArrayWidth';

    if(!defined $self->{width}) {
	$self->{width} = $width;
    } else {
	# Note that the width might be compatible but have more or
	# less info, e.g. an ArrayWidth with no count.  If the width
	# is not compatible, then the callee should croak.
	$self->{width}->merge($width);
    }
    $self->setChildrensWidth();
}

sub maybeSetWidth {
    my $self = shift;
    my $width = shift;

    return if !defined $width;
    $self->setWidth($width);
}

package Stupid2::HasWidthWithoutDeduction;

# A base class for things with a width that cannot deduce their width
# from their children. Width should be set from on high.

use strict;
use base qw(Stupid2::HasWidth);

sub deduceWidth {
    # can't!
}  

sub setChildrensWidth {
    # if we can't work out our width from our children, then we can't
    # work out their width from our width. I think.
}

package Stupid2::Type::Int;

use strict;
use base qw(Stupid2::HasWidthWithoutDeduction);
use Carp;

sub new {
    my $class = shift;
    my $width = shift;

    my $self = {};
    bless $self, $class;

    $self->setWidth($width);

    return $self;
}

package Stupid;

use strict;

sub ArrayFromString {
    my $str = shift;

    my $t = new Stupid2::ArrayValue();
    foreach my $c (split //, $str) {
	$t->append(new Stupid2::DecimalValue(ord($c),
		    new Stupid2::Bitwidth(8, 0)));
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

package Stupid2::Function;

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

sub deduceWidth {
    my $self = shift;

    $self->{body}->deduceWidth();
}

package Stupid2::TopLevelList;

# FIXME: top-level classes should inherit from a TopLevel class.

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

sub deduceWidth {
    my $self = shift;

    foreach my $tl (@{$self->{topLevels}}) {
	$tl->deduceWidth();
    }
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

package Stupid2::ExprList;

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

sub deduceWidth {
    my $self = shift;

    foreach my $expr (@{$self->{expressions}}) {
	$expr->deduceWidth();
    }
}

package Stupid2::FunctionCall;

use strict;
use Carp;

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

sub deduceWidth {
    my $self = shift;

# Don't need to do the function, since that should be done where it is
# declared.
    $self->{args}->deduceWidth();
}

sub maybeWidth {
    my $self = shift;

    return undef;
}

sub maybeSetWidth {
    my $self = shift;
    my $width = shift;

# FIXME: is this actually wrong?
#    confess "Setting function call width" if defined $width;
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

package Stupid2::Comment;

use strict;

sub new {
    my $class = shift;
    my $comment = shift;

    my $self = {};
    bless $self, $class;

    $self->{comment} = $comment;

    return $self;
}

sub deduceWidth {
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

package Stupid2::DecimalValue;

use strict;
use base qw(Stupid2::HasWidthWithoutDeduction);
use Math::BigInt;
use Carp;

sub new {
    my $class = shift;
    my $value = shift;
    my $width = shift;

    my $self = {};
    bless $self, $class;

    $self->{value} = new Math::BigInt($value);
    $self->setWidth($width);

    return $self;
}

sub value {
    my $self = shift;

    return $self->{value};
}

sub setAppropriateWidth {
    my $self = shift;

    if ($self->{value} >= 0) {
	if ($self->{value} < 256) {
	    $self->setWidth(new Stupid2::Bitwidth(8, 0));
	} else {
	    $self->setWidth(new Stupid2::Bitwidth(32, 0));
	}
    } else {
	croak;
    }
}
	

package Stupid2::ArrayRef;

use strict;
use base qw(Stupid2::HasWidth);

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

sub deduceWidth {
    my $self = shift;

    $self->{offset}->deduceWidth();
    if (!defined $self->{offset}->maybeWidth()) {
	$self->{offset}->setAppropriateWidth();
    }
    $self->maybeSetWidth($self->{array}->memberWidth());
}

sub setChildrensWidth {
    my $self = shift;

    # We don't know what the array count is, but we do know what its
    # width should be.
    my $array_width = new Stupid2::ArrayWidth(undef, $self->{width});
    $self->{array}->setWidth($array_width);
}

package Stupid2::StatementList;

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

sub deduceWidth {
    my $self = shift;

    for my $s (@{$self->{statements}}) {
	$s->deduceWidth();
    }

    return undef;
}

package Stupid2::Statement;

use strict;

sub new {
    my $class = shift;
    my $expr = shift;

    my $self = {};
    bless $self, $class;

    $self->{expr} = $expr;

    return $self;
}

sub deduceWidth {
    my $self = shift;

    $self->{expr}->deduceWidth();
}

package Stupid2::If;

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

sub deduceWidth {
    my $self = shift;

    $self->{cond}->deduceWidth();
    $self->{then}->deduceWidth();
    $self->{else}->deduceWidth();
}

package Stupid2::While;

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

sub deduceWidth {
    my $self = shift;

    $self->{cond}->deduceWidth();
    $self->{body}->deduceWidth();
}

package Stupid2::Binary;

use strict;
use base qw(Stupid2::HasWidth);
use Carp;

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

sub deduceWidth {
    my $self = shift;

    $self->{left}->deduceWidth();
    $self->{right}->deduceWidth();

    $self->{left}->maybeSetWidth($self->{right}->maybeWidth());
    $self->{right}->maybeSetWidth($self->{left}->maybeWidth());
}

sub setChildrensWidth {
    my $self = shift;

    $self->{left}->setWidth($self->{width});
    $self->{right}->setWidth($self->{width});
}

package Stupid2::Set;

# A = B

use strict;
use base qw(Stupid2::Binary);

package Stupid::And32;

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

package Stupid::And8;

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

package Stupid::BAnd;

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


package Stupid::BOr;

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

package Stupid2::Eq;

use strict;
use base qw(Stupid2::Binary);

package Stupid::Le8;

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


package Stupid::Ge8;

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


package Stupid::LShift32;

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

package Stupid::LShift8;

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

    # FIXME type checking
    return $self->{left}->value()->blsft($self->{right}->value());
}

package Stupid::Minus8;

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

    # FIXME type and underflow checking
    return $self->{left}->value()->bsub($self->{right}->value());
}

package Stupid::Minus32;

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

    # FIXME type and underflow checking
    return $self->{left}->value()->bsub($self->{right}->value());
}


package Stupid::Mod8;

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

    # FIXME type checking
    return $self->{left}->value()->bmod($self->{right}->value());
}


package Stupid::Mod32;

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

    # FIXME type checking
    return $self->{left}->value()->bmod($self->{right}->value());
}

package Stupid2::Ne;

use strict;
use base qw(Stupid2::Binary);

package Stupid::Mask32To8;

use strict;

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

package Stupid2::Plus;

use strict;
use base qw(Stupid2::Binary);

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

package Stupid2::Declare;

use strict;
use base qw(Stupid2::HasWidth);

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

sub deduceWidth {
    my $self = shift;

    $self->{var}->deduceWidth();
    $self->{var}->maybeSetWidth($self->{init}->maybeWidth());
    $self->{init}->maybeSetWidth($self->{var}->maybeWidth());
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

package Stupid2::Type::Array;

use strict;
use base qw(Stupid2::HasWidthWithoutDeduction);
use Carp;

sub new {
    my $class = shift;
    my $type = shift;
    # FIXME: $size should be $count...
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

sub memberWidth {
    my $self = shift;

    return $self->{type}->width();
}

sub width {
    my $self = shift;

    return new Stupid2::ArrayWidth($self->{size}, $self->{type}->width());
}

package Stupid2::Variable;

use strict;
use base qw(Stupid2::HasWidth);
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

sub name {
    my $self = shift;

    return $self->{name};
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

sub deduceWidth {
    my $self = shift;

    $self->setWidth($self->{type}->width());
    croak if !defined $self->{width};
}

sub memberWidth {
    my $self = shift;

    return $self->{type}->memberWidth();
}

sub setChildrensWidth {
    my $self = shift;

    $self->{type}->setWidth($self->{width});
}

package Stupid2::HexValue;

use strict;
use base qw(Stupid2::HasWidthWithoutDeduction);
use Math::BigInt;

# Unsigned hex value, any length

sub new {
    my $class = shift;
    my $value = shift;
    my $width = shift;

    my $self = {};
    bless $self, $class;

    $self->{value} = new Math::BigInt("0x$value");
    $self->setWidth($width);

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

package Stupid2::ArrayValue;

use strict;
use base qw(Stupid2::HasWidth);
use Math::BigInt;

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

sub values {
    my $self = shift;

    return $self->{values};
}

sub setChildrensWidth {
    my $self = shift;

    my $child_width = $self->{width}->getArrayElementWidth();
    foreach my $value (@{$self->{values}}) {
	$value->setWidth($child_width);
    }
}
