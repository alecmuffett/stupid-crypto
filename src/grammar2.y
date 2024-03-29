%right '='

%%

prog	: toplevel_list
	    { $_[1]; }
	;

toplevel_list : toplevel_list toplevel
	    { $_[1]->appendTopLevel($_[2]); $_[1]; }
	| toplevel
	    { my $t = new Stupid2::TopLevelList();
	      $t->appendTopLevel($_[1]);
	      $t; }
	;

toplevel : comment ';'
	| function
	| struct_decl
	;

struct_decl :	'struct' WORD '(' abstract_decl_list ')' ';'
	    { new Stupid2::Type::Struct($::Context, $_[2], $_[4]); }
	;

abstract_decl_list : abstract_decl_list ',' abstract_decl
	    { $_[1]->appendAbstractDecl($_[3]); $_[1]; }
	| abstract_decl
	    { my $t = new Stupid2::AbstractDeclList();
	      $t->appendAbstractDecl($_[1]);
	      $t; }
	;

abstract_decl :	type vardecl
	    { new Stupid2::AbstractDeclare($_[1], $_[2]); }
	;

function :	'function' '(' arglist ')' WORD '(' arglist ')'
		  '{' statements '}'
	    { $_[3]->markAsReturn();
	      $_[7]->markAsArgument();
	      new Stupid2::Function($::Context, $_[5], $_[3], $_[7], $_[10]); }
	;

arglist :	arglist ',' arg
	    { $_[1]->appendArg($_[3]); $_[1]; }
	|	arg
	    { my $t1 = new Stupid::ArgList();
	      $t1->appendArg($_[1]);
	      $t1; }
	|
	    { new Stupid::ArgList(); }
	;

arg	:	type vardecl
	    { new Stupid2::Declare($::Context, new Stupid2::Variable($_[1],
								     $_[2])); }
	;

statements :	statements statement
	    { $_[1]->appendStatement($_[2]); $_[1]; }
	|	statement
	    { my $t1 = new Stupid2::StatementList();
	      $t1->appendStatement($_[1]);
	      $t1; }
	|
	    { new Stupid2::StatementList(); }
	;

statement :	decl ';'
	    { $_[1]; }
	|	comment ';'
	    { $_[1]; }
	|	var '=' expr ';'
	    { new Stupid2::Statement(new Stupid2::Set($_[1], $_[3])); }
	| 	'if' '(' expr ')' '{' statements '}' 'else' '{' statements '}'
	    { new Stupid2::If($_[3], $_[6], $_[10]); }
	| 	'while' '(' expr ')' '{' statements '}'
	    { new Stupid2::While($_[3], $_[6]); }
	|	call ';'
	    { $_[1]; }
	;

expr	:	expr 'and32' expr
	    { new Stupid::And32($_[1], $_[3]); }
	|	expr 'and8' expr
	    { new Stupid::And8($_[1], $_[3]); }
	|	expr 'band' expr
	    { new Stupid::BAnd($_[1], $_[3]); }
	|	expr 'bor' expr
	    { new Stupid::BOr($_[1], $_[3]); }
	|	expr '==' expr
	    { new Stupid2::Eq($_[1], $_[3]); }
	|	expr 'ge8' expr
	    { new Stupid::Ge8($_[1], $_[3]); }
	|	expr 'le8' expr
	    { new Stupid::Le8($_[1], $_[3]); }
	|	expr 'lshift32' expr
	    { new Stupid::LShift32($_[1], $_[3]); }
	|	expr 'lshift8' expr
	    { new Stupid::LShift8($_[1], $_[3]); }
	|	expr 'minus8' expr
	    { new Stupid::Minus8($_[1], $_[3]); }
	|	expr 'minus32' expr
	    { new Stupid::Minus32($_[1], $_[3]); }
	|	expr 'mod8' expr
	    { new Stupid::Mod8($_[1], $_[3]); }
	|	expr 'mod32' expr
	    { new Stupid::Mod32($_[1], $_[3]); }
	|	expr '!=' expr
	    { new Stupid2::Ne($_[1], $_[3]); }
	|	expr 'or8' expr
	    { new Stupid::Or8($_[1], $_[3]); }
	|	expr '+' expr
	    { new Stupid2::Plus($_[1], $_[3]); }
	|	expr 'wrapplus32' expr
	    { new Stupid::WrapPlus32($_[1], $_[3]); }
	|	expr 'rrotate32' expr
	    { new Stupid::RRotate32($_[1], $_[3]); }
	|	expr 'rshift32' expr
	    { new Stupid::RShift32($_[1], $_[3]); }
	|	expr 'xor32' expr
	    { new Stupid::XOr32($_[1], $_[3]); }
	|	'mask32to8' expr
	    { new Stupid::Mask32To8($_[2]); }
	|	'not32' expr
	    { new Stupid::Not32($_[2]); }
	|	'not8' expr
	    { new Stupid::Not8($_[2]); }
	|	'widen8to32' expr
	    { new Stupid::Widen8To32($_[2]); }
	|	'(' expr ')'
	    { $_[2]; }
	|	var
	    { $_[1]; }
	|	value
	;

exprlist:	exprlist ',' expr
	    { $_[1]->appendExpr($_[3]); $_[1]; }
	|	expr
	    { my $t = new Stupid2::ExprList(); $t->appendExpr($_[1]); $t; }
	|
	    { new Stupid2::ExprList(); }
	;

var	:	WORD
	    { $::Context->findSymbol($_[1]); }
	|	var '[' expr ']'
	    { new Stupid2::ArrayRef($_[1], $_[3]); }
	|	expr '.' WORD
	    { new Stupid2::MemberRef($_[1], $_[3]); }
	|	call
	;

call:	|	expr '(' exprlist ')'
	    { new Stupid2::FunctionCall($_[1], $_[3]); }
	;

decl	:	type vardecl '=' expr
	    { new Stupid2::Declare($::Context,
				   new Stupid2::Variable($_[1], $_[2]),
				   $_[4]); }
	;

type	:	'int' '_' bitwidth
	    { new Stupid2::Type::Int($_[3]); }
	|	'ostream'
	    { new Stupid::Type::OStream(); }
	|	'array' '(' type ',' VALUE ')'
	    { new Stupid2::Type::Array($_[3], $_[5]); }
	|	'struct' WORD
	    { new Stupid2::Type::StructInstance($::Context, $_[2]); }
	;

bitwidth :      VALUE
	    { new Stupid2::Bitwidth($_[1]->value(), 1); }
	|	UVALUE
	    { new Stupid2::Bitwidth($_[1]->value(), 0); }
	;

arrayval :	'[' val_list ']'
	    { $_[2]; }
	;

val_list :	val_list ',' expr
	    { $_[1]->append($_[3]); $_[1]; }
	|	expr
	    { my $t = new Stupid2::ArrayValue(); $t->append($_[1]); $t; }
	|	STRING
	    { Stupid::ArrayFromString($_[1]); }
	;

value	:       arrayval
	|	VALUE
	|	VALUE '_' bitwidth
	    { $_[1]->setWidth($_[3]); $_[1]; }
	|	CHAR
	;

vardecl	:	WORD
	;

comment :	STRING
	    { new Stupid2::Comment($_[1]); }
	;

%%
