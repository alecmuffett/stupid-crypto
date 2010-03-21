%right '='

%%

prog	: toplevel_list
	    { $_[1]; }
	;

toplevel_list : toplevel_list toplevel
	    { $_[1]->appendTopLevel($_[2]); $_[1]; }
	| toplevel
	    { my $t = new Stupid::TopLevelList();
	      $t->appendTopLevel($_[1]);
	      $t; }
	;

toplevel : comment ';'
	| function
	| struct_decl
	;

struct_decl :	'struct' WORD '(' abstract_decl_list ')' ';'
	    { new Stupid::Type::Struct($_[2], $_[4]); }
	;

abstract_decl_list : abstract_decl_list ',' abstract_decl
	    { $_[1]->appendAbstractDecl($_[3]); $_[1]; }
	| abstract_decl
	    { my $t = new Stupid::AbstractDeclList();
	      $t->appendAbstractDecl($_[1]);
	      $t; }
	;

abstract_decl :	type vardecl
	    { new Stupid::AbstractDeclare($_[1], $_[2]); }
	;

function :	'function' '(' arglist ')' WORD '(' arglist ')'
		  '{' statements '}'
	    { $_[3]->markAsReturn();
	      new Stupid::Function($::Context, $_[5], $_[3], $_[7], $_[10]); }
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
	    { new Stupid::Declare($::Context, new Stupid::Variable($_[1],
								   $_[2])); }
	;

statements :	statements statement
	    { $_[1]->appendStatement($_[2]); $_[1]; }
	|	statement
	    { my $t1 = new Stupid::StatementList();
	      $t1->appendStatement($_[1]);
	      $t1; }
	;

statement :	decl ';'
	    { $_[1]; }
	|	comment ';'
	    { $_[1]; }
	|	var '=' expr ';'
	    { new Stupid::Statement(new Stupid::Set($_[1], $_[3])); }
	| 	'if' '(' expr ')' '{' statements '}' 'else' '{' statements '}'
	    { new Stupid::If($_[3], $_[6], $_[10]); }
	| 	'while' '(' expr ')' '{' statements '}'
	    { new Stupid::While($_[3], $_[6]); }
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
	|	expr 'eq32' expr
	    { new Stupid::Eq32($_[1], $_[3]); }
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
	|	expr 'ne32' expr
	    { new Stupid::Ne32($_[1], $_[3]); }
	|	expr 'ne8' expr
	    { new Stupid::Ne8($_[1], $_[3]); }
	|	expr 'or8' expr
	    { new Stupid::Or8($_[1], $_[3]); }
	|	expr 'plus8' expr
	    { new Stupid::Plus8($_[1], $_[3]); }
	|	expr 'plus32' expr
	    { new Stupid::Plus32($_[1], $_[3]); }
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
	    { my $t = new Stupid::ExprList(); $t->appendExpr($_[1]); $t; }
	|
	    { new Stupid::ExprList(); }
	;

var	:	WORD
	    { $::Context->findSymbol($_[1]); }
	|	var '[' expr ']'
	    { new Stupid::ArrayRef($_[1], $_[3]); }
	|	expr '.' WORD
	    { new Stupid::MemberRef($_[1], $_[3]); }
	|	call
	;

call:	|	expr '(' exprlist ')'
	    { new Stupid::FunctionCall($_[1], $_[3]); }
	;

decl	:	type vardecl '=' expr
	    { new Stupid::Declare($::Context,
				  new Stupid::Variable($_[1], $_[2]), $_[4]); }
	;

type	:	'uint32'
	    { new Stupid::Type::UInt32(); }
	|	'uint8'
	    { new Stupid::Type::UInt8(); }
	|	'ostream'
	    { new Stupid::Type::OStream(); }
	|	'array' '(' type ',' VALUE ')'
	    { new Stupid::Type::Array($_[3], $_[5]); }
	|	'struct' WORD
	    { new Stupid::Type::StructInstance($_[2]); }
	;

arrayval :	'[' val_list ']'
	    { $_[2]; }
	;

val_list :	val_list ',' expr
	    { $_[1]->append($_[3]); $_[1]; }
	|	expr
	    { my $t = new Stupid::ArrayValue(); $t->append($_[1]); $t; }
	|	STRING
	    { Stupid::ArrayFromString($_[1]); }
	;

value	:       arrayval
	|	VALUE
	|	CHAR
	;

vardecl	:	WORD
	;

comment :	STRING
	    { new Stupid::Comment($_[1]); }
	;

%%
