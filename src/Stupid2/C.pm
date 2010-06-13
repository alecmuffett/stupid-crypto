package Stupid2::C;

use strict;
use warnings;

sub Stupid2::TopLevelList::emitCode {
    my $self = shift;

    for my $f (@{$self->{topLevels}}) {
	$f->emitCode();
    }
}

sub Stupid2::Comment::emitCode {
    my $self = shift;

    print "/* $self->{comment} */\n";
}

sub Stupid2::Function::emitCode {
    my $self = shift;

    print 'void ', $self->{name}, '(';
    my $first = $self->{returns}->emitReturnDecls();
    $self->{args}->emitArgs($first);
    print ") {\n";
    $self->{body}->emitCode();
    print "}\n";
}

sub Stupid2::Function::emitCall {
    my $self = shift;

    print $self->{name};
}

sub Stupid2::Function::maybeAddSelf {
}

sub Stupid2::StatementList::emitCode {
    my $self = shift;

    for my $s (@{$self->{statements}}) {
#	use Data::Dumper; $Data::Dumper::Indent=1; print "/*\n", Data::Dumper->Dump([$s]), " */\n";
	$s->emitCode();
    }
}

sub Stupid2::Statement::emitCode {
    my $self = shift;

    $self->{expr}->emitCode();
    print ";\n";
}

sub Stupid2::ExprList::emitParameters {
    my $self = shift;

    my $first = 1;
    for my $expr (@{$self->{expressions}}) {
	print ', ' if !$first;
	$first = 0;
	$expr->emitParameter();
    }
}

sub Stupid2::Declare::emitReturnDecl {
    my $self = shift;

    $self->{var}->emitReturnDecl();
}

sub Stupid2::Declare::emitCode {
    my $self = shift;

    $self->{var}->emitDeclaration($self->{init});
    print ";\n";
}

sub Stupid2::Declare::emitArg {
    my $self = shift;

    $self->{var}->emitArg();
}

sub Stupid2::Set::emitCode {
    my $self = shift;

    # special case ... clearly we could do this in full generality,
    # e.g f()[8] or f().foo or (a, b) = (c, d) or (a, b) = (b, a)
    # [hmmm]
    if(ref($self->{right}) eq 'Stupid2::FunctionCall') {
	$self->{right}->emitCallWithLValue($self->{left});
	return;
    }

    $self->{left}->emitLValue();
    print ' = ';
    $self->{right}->emitCode();
}

sub Stupid2::FunctionCall::emitCode {
    my $self = shift;

    $self->{function}->emitCall();
    print '(';
    $self->{function}->maybeAddSelf();
    $self->{args}->emitParameters();
    print ");\n";
}

sub Stupid2::FunctionCall::emitCallWithLValue {
    my $self = shift;
    my $lvalue = shift;

    $self->{function}->emitCall();
    print '(';
    $lvalue->emitPointer();
    print ', ' if !$self->{args}->isEmpty();
    $self->{args}->emitParameters();
    print ");\n";
}

sub Stupid2::If::emitCode {
    my $self = shift;

    print 'if (';
    $self->{cond}->emitCode();
    print ") {\n";
    $self->{then}->emitCode();
    print "} else {\n";
    $self->{else}->emitCode();
    print "}\n";
}

sub Stupid2::While::emitCode {
    my $self = shift;

    print 'while (';
    $self->{cond}->emitCode();
    print ") {\n";
    $self->{body}->emitCode();
    print "}\n";
}

sub Stupid2::Type::Array::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print $self->{type}->typeName(), ' ', $name,
      '[', $self->{size}->value(), ']';
}

sub Stupid2::Type::Struct::emitCode {
    my $self = shift;

    print "struct $self->{name} {\n";
    $self->{decls}->emitCode();
    print "};\n";
}

sub Stupid2::Type::StructInstance::emitReturnDecl {
    my $self = shift;
    my $name = shift;

    print "struct $self->{struct}->{name} *$name";
}

sub Stupid2::Type::StructInstance::dereference {
    my $self = shift;

    print '*';
}

sub Stupid2::Type::StructInstance::emitDeclaration {
    my $self = shift;
    my $name = shift;

    print "struct $self->{struct}->{name} $name";
}

sub Stupid2::Type::StructInstance::emitPointer {
    my $self = shift;

    print '&';
}

sub Stupid2::AbstractDeclList::emitCode {
    my $self = shift;

    foreach my $decl (@{$self->{decls}}) {
	$decl->emitCode();
    }
}

sub Stupid2::AbstractDeclare::emitCode {
    my $self = shift;

    print '  ';
    $self->{type}->emitDeclaration($self->{name});
    print ";\n";
}

sub Stupid2::Type::Int::typeName {
    my $self = shift;

    my $base = 'uint';
    $base = 'int' if $self->signed();
    return $base . $self->bits();
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

sub Stupid2::Variable::emitDeclaration {
    my $self = shift;
    my $init = shift;

    $self->{type}->emitDeclaration($self->{name});

    # special case ... clearly we could do this in full generality,
    # e.g f()[8] or f().foo or (a, b) = (c, d) or (a, b) = (b, a)
    # [hmmm]
    if(ref($init) eq 'Stupid2::FunctionCall') {
	print ";\n";
	$init->emitCallWithLValue($self);
	return;
    }

    print ' = ';
    $init->emitCode();
}

sub Stupid2::Variable::emitReturnDecl {
    my $self = shift;

    $self->{type}->emitReturnDecl($self->{name});
}

sub Stupid2::Variable::emitCode {
    my $self = shift;

    print '(';
    $self->{type}->dereference() if $self->{isReturn};
    $self->{type}->derefArgument() if $self->{isArgument};
    print $self->{name};
    print ')';
}

sub Stupid2::Variable::emitMemberRef {
    my $self = shift;
    my $member = shift;

    print '.';
    print $member->{name};
}

sub Stupid2::Variable::maybeAddSelf {
    my $self = shift;

    print "$self->{name}, " if $self->{type}->needsSelf();
}

sub Stupid2::Variable::emitLValue {
    my $self = shift;

    $self->{type}->dereference() if $self->{isReturn};
    print $self->{name};
}

sub Stupid2::Variable::emitParameter {
    my $self = shift;

    $self->{type}->emitParameter()
      if !$self->{isReturn} && !$self->{isArgument};;
    print $self->{name};
}

sub Stupid2::Variable::emitArg {
    my $self = shift;

    $self->{type}->emitArg($self->{name});
}

sub Stupid2::Variable::emitPointer {
    my $self = shift;

    $self->{type}->emitPointer() if !$self->{isReturn};
    print $self->{name};
}

sub Stupid2::MemberRef::emitCode {
    my $self = shift;

    $self->{owner}->emitCode();
    $self->{owner}->emitMemberRef($self->{member});
}

sub Stupid2::MemberRef::emitCall {
    my $self = shift;

    $self->emitCode();
}

sub Stupid2::MemberRef::maybeAddSelf {
    my $self = shift;

    $self->{owner}->maybeAddSelf();
}

sub Stupid2::MemberRef::emitLValue {
    my $self = shift;

    $self->{owner}->emitCode();
    $self->{owner}->emitMemberRef($self->{member});
}

sub Stupid2::ArrayRef::emitParameter {
    my $self = shift;

    $self->emitLValue();
}

sub Stupid2::ArrayRef::emitLValue {
    my $self = shift;

    $self->{array}->emitCode();
    print '[';
    $self->{offset}->emitCode();
    print ']';
}

sub Stupid2::ArrayValue::emitCode {
    my $self = shift;

    print '{ ';
    my $first = 1;
    foreach my $v (@{$self->values()}) {
	print ', ' if !$first;
	$v->emitCode();
	$first = 0;
    }
    print ' }';
}

sub Stupid2::DecimalValue::emitCode {
    my $self = shift;

    print $self->{value};
    print 'U' if !$self->signed();
}

sub Stupid2::DecimalValue::emitParameter {
    my $self = shift;

    $self->emitCode();
}

sub Stupid2::HexValue::emitCode {
    my $self = shift;

    print $self->{value}->as_hex(), 'U';
}

sub Stupid2::Eq::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' == ';
    $self->{right}->emitCode();
    print ')';
}

sub Stupid2::Ne::emitCode {
    my $self = shift;

    print '(';
    $self->{left}->emitCode();
    print ' != ';
    $self->{right}->emitCode();
    print ')';
}

# FIXME: deal with sign.
sub Stupid2::Plus::emitCode {
    my $self = shift;

    print 'plus', $self->bits(), '(';
    $self->{left}->emitCode();
    print ', ';
    $self->{right}->emitCode();
    print ')';
}

1;
