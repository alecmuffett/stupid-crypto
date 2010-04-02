package Stupid::C;

use strict;

use Carp;

sub Stupid::LanguageWrapper::emitCode {
    my $self = shift;

    print <<EOC;
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#ifdef __APPLE__
typedef u_int32_t uint32;
typedef u_int8_t uint8;
#else
typedef uint32_t uint32;
typedef uint8_t uint8;
#endif

typedef struct {
  void (*put)(void *info, uint8 ch);
  void *info;
} stupid_ostream;

void stupid_fatal(const char * const fmt, ...) {
  va_list vl;

  va_start(vl, fmt);
  vfprintf(stderr, fmt, vl);
  fputc('\\n', stderr);
  exit(1);
}

uint8 plus8(const uint8 a, const uint8 b) {
  uint8 t = a + b;
  if(t < a)
    stupid_fatal("%d plus8 %d overflows", a, b);
  return t;
}

uint32 plus32(const uint32 a, const uint32 b) {
  uint32 t = a + b;
  if(t < a)
    stupid_fatal("%u plus32 %u overflows", a, b);
  return t;
}
EOC

    $self->{tree}->emitCode();
}

sub Stupid::TopLevelList::emitCode {
    my $self = shift;

    for my $f (@{$self->{topLevels}}) {
	$f->emitCode();
    }
}

sub Stupid::Function::emitCode {
    my $self = shift;

    print 'void ', $self->{name}, '(';
    my $first = $self->{returns}->emitReturnDecls();
    $self->{args}->emitArgs($first);
    print ") {\n";
    $self->{body}->emitCode();
    print "}\n";
}

sub Stupid::Function::maybeAddSelf {
}

sub Stupid::Function::emitCall {
    my $self = shift;

    print $self->{name};
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
#	use Data::Dumper; $Data::Dumper::Indent=1; print "/*\n", Data::Dumper->Dump([$s]), " */\n";
	$s->emitCode();
    }
}

sub Stupid::Statement::emitCode {
    my $self = shift;

    $self->{expr}->emitCode();
    print ";\n";
}

sub Stupid::FunctionCall::emitCode {
    my $self = shift;

    $self->{function}->emitCall();
    print '(';
    $self->{function}->maybeAddSelf();
    $self->{args}->emitParameters();
    print ");\n";
}

sub Stupid::FunctionCall::emitCallWithLValue {
    my $self = shift;
    my $lvalue = shift;

    $self->{function}->emitCall();
    print '(';
    $lvalue->emitPointer();
    print ', ' if !$self->{args}->isEmpty();
    $self->{args}->emitParameters();
    print ");\n";
}

sub Stupid::MemberRef::emitCode {
    my $self = shift;

    $self->{owner}->emitCode();
    $self->{owner}->emitMemberRef($self->{member});
}

sub Stupid::MemberRef::emitCall {
    my $self = shift;

    $self->emitCode();
}

sub Stupid::MemberRef::maybeAddSelf {
    my $self = shift;

    $self->{owner}->maybeAddSelf();
}

sub Stupid::MemberRef::emitLValue {
    my $self = shift;

    $self->{owner}->emitCode();
    $self->{owner}->emitMemberRef($self->{member});
}

sub Stupid::MemberRef::emitParameter {
    my $self = shift;

    $self->{member}->{type}->emitParameter();
    print '(';
    $self->emitLValue();
    print ')';
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

sub Stupid::ExprList::emitParameters {
    my $self = shift;

    my $first = 1;
    for my $expr (@{$self->{expressions}}) {
	print ', ' if !$first;
	$first = 0;
	$expr->emitParameter();
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

    # special case ... clearly we could do this in full generality,
    # e.g f()[8] or f().foo or (a, b) = (c, d) or (a, b) = (b, a)
    # [hmmm]
    if(ref($self->{right}) eq 'Stupid::FunctionCall') {
	$self->{right}->emitCallWithLValue($self->{left});
	return;
    }

    $self->{left}->emitLValue();
    print ' = ';
    $self->{right}->emitCode();
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

    # special case ... clearly we could do this in full generality,
    # e.g f()[8] or f().foo or (a, b) = (c, d) or (a, b) = (b, a)
    # [hmmm]
    if(ref($init) eq 'Stupid::FunctionCall') {
	print ";\n";
	$init->emitCallWithLValue($self);
	return;
    }

    print ' = ';
    $init->emitCode();
}

sub Stupid::Variable::emitCode {
    my $self = shift;

    print '(';
    $self->{type}->dereference() if $self->{isReturn};
    $self->{type}->derefArgument() if $self->{isArgument};
    print $self->{name};
    print ')';
}

sub Stupid::Variable::emitParameter {
    my $self = shift;

    $self->{type}->emitParameter()
      if !$self->{isReturn} && !$self->{isArgument};;
    print $self->{name};
}

sub Stupid::Variable::emitMemberRef {
    my $self = shift;
    my $member = shift;

    print '.';
    print $member->{name};
}

sub Stupid::Variable::emitLValue {
    my $self = shift;

    $self->{type}->dereference() if $self->{isReturn};
    print $self->{name};
}

sub Stupid::Variable::emitPointer {
    my $self = shift;

    $self->{type}->emitPointer() if !$self->{isReturn};
    print $self->{name};
}

sub Stupid::Variable::maybeAddSelf {
    my $self = shift;

    print "$self->{name}, " if $self->{type}->needsSelf();
}

sub Stupid::Type::Struct::emitCode {
    my $self = shift;

    print "struct $self->{name} {\n";
    $self->{decls}->emitCode();
    print "};\n";
}

sub Stupid::Type::StructInstance::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "struct $self->{struct}->{name} $name";
}

sub Stupid::Type::StructInstance::emitArg {
    my $self = shift;
    my $name = shift;

    print "const struct $self->{struct}->{name} * const $name";
}

sub Stupid::Type::StructInstance::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "struct $self->{struct}->{name} *$name";
}

sub Stupid::Type::StructInstance::dereference {
    my $self = shift;

    print '*';
}

sub Stupid::Type::StructInstance::derefArgument {
    my $self = shift;

    print '*';
}

sub Stupid::Type::StructInstance::emitPointer {
    my $self = shift;

    print '&';
}

sub Stupid::Type::StructInstance::emitParameter {
    my $self = shift;

    print '&';
}

sub Stupid::AbstractDeclList::emitCode {
    my $self = shift;

    foreach my $decl (@{$self->{decls}}) {
	$decl->emitCode();
    }
}

sub Stupid::AbstractDeclare::emitCode {
    my $self = shift;

    print '  ';
    $self->{type}->emitDeclaration($self->{name});
    print ";\n";
}

sub Stupid::Type::UInt32::dereference {
    my $self = shift;

    print '*';
}

sub Stupid::Type::UInt32::derefArgument {
# ints are passed by value
}

sub Stupid::Type::UInt32::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "uint32 *$name";
}

sub Stupid::Type::UInt32::emitArg {
    my $self = shift;
    my $name = shift;

    print 'const ';
    $self->emitDeclaration($name);
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

    return 'uint8';
}

sub Stupid::Type::UInt8::emitArg {
    my $self = shift;
    my $name = shift;

    print 'const ';
    $self->emitDeclaration($name);
}

sub Stupid::Type::UInt8::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "uint8 $name";
}

sub Stupid::Type::UInt8::emitPointer {
    my $self = shift;

    print '&';
}

sub Stupid::Type::UInt8::emitParameter {
    my $self = shift;
}

sub Stupid::Type::UInt8::dereference {
    my $self = shift;

    print '*';
}

sub Stupid::Type::UInt8::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "uint8 *$name";
}

sub Stupid::Type::OStream::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "stupid_ostream *$name";
}

sub Stupid::Type::OStream::emitArg {
    croak "ostreams must be outputs";
}

sub Stupid::Type::OStream::needsSelf {
    return 1;
}

sub Stupid::Type::OStream::dereference {
    print '*';
}

sub Stupid::Type::Array::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    $self->emitDeclaration($name);
}

sub Stupid::Type::Array::emitArg {
    my $self = shift;
    my $name = shift;

    print 'const ';
    $self->emitDeclaration($name);
}

sub Stupid::Type::Array::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print $self->{type}->typeName(), ' ', $name,
      '[', $self->{size}->value(), ']';
}

sub Stupid::Type::Array::emitPointer {
    # because of C bizarroness this is not printing '&'
}

sub Stupid::Type::Array::emitParameter {
    # because of C bizarroness this is not printing '&'
}

sub Stupid::Type::Array::dereference {
    # once more, no need for a *
}

sub Stupid::Type::Array::derefArgument {
    # once more, no need for a *
}

sub Stupid::ArrayRef::emitLValue {
    my $self = shift;

    $self->{array}->emitCode();
    print '[';
    $self->{offset}->emitCode();
    print ']';
}

sub Stupid::ArrayRef::emitParameter {
    my $self = shift;

    $self->emitLValue();
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

sub Stupid::DecimalValue::emitParameter {
    my $self = shift;

    $self->emitCode();
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

# FIXME: we really want this in some common ancestor
sub Stupid::LShift32::emitParameter {
    my $self = shift;

    $self->emitCode();
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

sub Stupid::Ne8::emitCode {
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

sub Stupid::Plus8::emitCode {
    my $self = shift;

    print 'plus8(';
    $self->{left}->emitCode();
    print ', ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::Plus32::emitCode {
    my $self = shift;

    print 'plus32(';
    $self->{left}->emitCode();
    print ', ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid::WrapPlus32::emitCode {
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
