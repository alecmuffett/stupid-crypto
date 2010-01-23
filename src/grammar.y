%right '='

%%

prog	:	statements
	;

statements :	statements statement
	    { new Stupid::Null($_[1], $_[2]) }
	| statement
	;

statement :	decl '=' VALUE
	    { new Stupid::Set($_[1], $_[3]); }
	| COMMENT
	    { new Stupid::Comment($_[1]); }
	;

decl	:	'byte' vardecl
	    { new Stupid::ByteVariable($_[2]); }
	|	'uint32' vardecl
	    { new Stupid::UInt32Variable($::Context, $_[2]); }
	;

vardecl	:	WORD
	;

%%
