grammar scl;

r: block*;

whileStatement
	: 'WHILE' whileCondition 'DO' whileBlockStatements 'END_WHILE'
	;
forStatement
	:  'FOR' forInitialCondition ('TO'|'to') forEndCondition ('BY' forStepCondition)? 'DO' forBlockStatements ('END_FOR'|'End_FOR')
	;
ifStatement
	: 'IF' ifCondition THEN ifBlockStatments  ('ELSIF' ifCondition THEN ifBlockStatments)* (ELSE  ifBlockStatments)? 'END_IF'
	;
whileCondition
	: expr
	;
whileBlockStatements
	:(stat)*
	;
repeatStatement
	: 'REPEAT' (stat)*  'UNTIL' expr 'END_REPEAT'
	;
switchStatement
	:	'CASE'  expr  'OF'  switchBlock+  (ELSE ':'? defualtswitchBlockStatements)? 'END_CASE'
	;

block
	: dataBlock
	| typeBlock 
	| fbBlock
	| fcBlock
	;

dataBlock: 'DATA_BLOCK' blockName dbInstanceName?  (structType (';')?)? nonRetainBlock? 'BEGIN' dbAssignments 'END_DATA_BLOCK';
typeBlock: 'TYPE' blockName  structType ';'? 'END_TYPE'; 
fbBlock: 'FUNCTION_BLOCK' blockName blockVarDeclarations* ('BEGIN')? blockFunctionality  'END_FUNCTION_BLOCK';
fcBlock: 'FUNCTION' blockName ':' function_type blockVarDeclarations* ('BEGIN')? blockFunctionality  'END_FUNCTION';

//data_block
blockName: expressionName;
dbInstanceName: expressionName;

structType
	: 'STRUCT' variableDefinitions 'END_STRUCT'
	;
	
variableDefinitions
	: (variableDefinition? ';')*
	;
	
variableDefinition
	: expressionName (',' expressionName)* ('AT' weirdThing)? ':' variableType (':=' constant (',' constant)*)?
	| expressionName (',' expressionName)* ('AT' weirdThing)? ':' variableType (':=' '['constant? '('constant')' ']')?
	;
weirdThing: expressionName;

variableType
	: elementaryType
	| arrayType
	| structType
	| udtType
	;
	
udtType
	: expressionName
	;
	
elementaryType
	: 'BOOL'
	| 'BYTE'
	| 'CHAR'
	| 'STRING' ('[' constant']')? 
	| 'WORD'
	| 'DWORD'
	| 'INT'
	| 'DINT'
	| 'REAL'
	| 'S5TIME'
	| 'TIME'
	| 'Date'
	| 'TIME_OF_DAY'
	| 'DATE_AND_TIME'
	;
	
arrayType
	: 'ARRAY' arrayRange 'OF' variableType
	;

arrayRange
	: '[' ArraySubRange (',' ArraySubRange)* ']'
	;
	
ArraySubRange
	: [0-9]+ (' ')* DOTDOT (' ')* [0-9]+
	;
DOTDOT: '..';


dbAssignments
	: (dbAssignment? ';')*
	;
	
dbAssignment
	: expressionName ':=' ('+'|'-')? constant (',' ('+'|'-')? constant)?
	;

nonRetainBlock
	: 'non_RETAIN' expressionName
	;

blockVarDeclarations 
	: blockTempVars
	| blockInOutDeclarations 
	| blockInputDeclarations 
	| blockOutputDeclarations 
	| blockConstDeclarations 
	| blockStaticDeclarations
	| blockLabelDeclarations
	;
blockTempVars
	: 'VAR_TEMP' variableDefinitions END_VAR
	;
blockInOutDeclarations
	: 'VAR_IN_OUT' variableDefinitions END_VAR
	;
blockInputDeclarations
	: 'VAR_INPUT' variableDefinitions END_VAR
	;
blockOutputDeclarations
	: 'VAR_OUTPUT' variableDefinitions END_VAR
	;
blockConstDeclarations
	: 'CONST'  (assignmentStatement? ';')* 'END_CONST'
	;
blockStaticDeclarations
	: 'VAR' variableDefinitions END_VAR
	;
blockLabelDeclarations
	: 'LABEL'  (expressionName (',' expressionName)*)? ';'* 'END_LABEL'
	;
blockFunctionality: (stat)*
				  ;
function_type
	: 'VOID'
	| elementaryType
	| udtType
	;


END_VAR:  ('E'|'e') ('N'|'n') ('D'|'d') ('_') ('V'|'v') ('A'|'a') ('R'|'r');


stat
	:ifStatement ';' 
	| switchStatement ';'
	| forStatement ';'
	| whileStatement ';'
	| repeatStatement ';'
	| 'CONTINUE'  ';'
	| 'EXIT'  ';'
	| 'RETURN' ';'
	| 'GOTO' expressionName ';'
	| labelStatment 
	| assignmentStatement ';'
	| functionStatement ';'
	| ';'
	;
functionStatement
	: expressionName '(' functionParameterExpression (',' functionParameterExpression)* ')'
	| expressionName '(' expr? ')'
	;
labelStatment
	: expressionName ':'
	;

forInitialCondition
	: assignmentStatement
	| expressionName
	;
forEndCondition
	: expr
	;
forStepCondition
	: expr
	;
	
forBlockStatements
	:(stat)*
	;
	

ifCondition
	: expr
	;
ifBlockStatments
	: (stat)*
	;


	


switchBlock
	:	switchLabel switchBlockStatements
	;
switchLabel
	: switchLabelConstant (',' switchLabelConstant)* ':'
	;
switchLabelConstant
	: expressionName
	| constant
	| ArraySubRange
	;
switchBlockStatements
	: (stat)* 
	;
defualtswitchBlockStatements
	: switchBlockStatements
	;


assignmentStatement 
	:leftHandAssignment ':=' rightHandAssignment 
	;
leftHandAssignment 
	: expr
	;
rightHandAssignment 
	: expr
	;
expr
    : expressionName '(' functionParameterExpression (',' functionParameterExpression)* ')'
	| expressionName '(' expr? ')' ('.' expr)*
	| expr '**' expr
	| expr '*' expr
	| expr '/' expr
	| expr '+' expr
	| expr '-' expr
	| expr 'MOD' expr
	| expr 'DIV' expr
	| expr '<' expr
	| expr '>' expr
	| expr '<=' expr
	| expr '>=' expr
	| expr '=' expr
	| expr '<>' expr
	| 'NOT' expr
	| expr ('AND'|'&') expr
	| expr 'XOR' expr
	| expr 'OR' expr
	| expressionName
	| constant
	| '+' expr
	| '-' expr
	| '(' expr ')'
	;		  



functionParameterExpression
	: expr ':=' expr
	;

constant
	:REALLiteral
	|INTLiteral
	|BOOLLiteral
	|DINTLiteral
	|CHARLiteral
	|STRINGLiteral
	|BYTELiteral
	|WORDLiteral
	|DWORDLiteral
	|DATELiteral
	|TIMELiteral
	|TIME_OF_DAYLiteral
	|DATE_AND_TIMELiteral
	|POINTERLiteral
	|GLOBALBOOLLiteral
	;


ELSE
	: ('E'|'e') ('L'|'l') ('S'|'s') ('E'|'e')
	;
THEN
	: ('T'|'t') ('H'|'h') ('E'|'e') ('N'|'n')
	;	
fragment
Letter
:	[a-zA-Z$_]
|	~[\u0000-\u00FF\uD800-\uDBFF]
|	[\uD800-\uDBFF] [\uDC00-\uDFFF]
;

fragment
LetterOrDigit
:	[a-zA-Z0-9$_] 
|	~[\u0000-\u00FF\uD800-\uDBFF]
|	[\uD800-\uDBFF] [\uDC00-\uDFFF]
;

	
BOOLLiteral
	:	('F'|'f') ('A'|'a') ('L'|'l') ('S'|'s') ('E'|'e') 
	|	('T'|'t') ('R'|'r') ('U'|'u') ('E'|'e') 
	|	'BOOL#0'
	|	'BOOL#1'
	;
	
BYTELiteral
	: BYTELiteralID '#16#' HexDigits
	| BYTELiteralID '#' [0-9]+
	| BYTELiteralID '#2#' BinaryDigits
	| BYTELiteralID '#' CHARLiteral
	;
CHARLiteral
	:	'\'' SingleCharacter '\''
	|	'\'' EscapeSequence '\''
	|	'CHAR#' [0-9]+
	;
STRINGLiteral
	:	'\'' StringCharacters? '\''
	;
	
WORDLiteral
	: WORDLiteralID '#16#' HexDigits
	| WORDLiteralID	'#8#' OctalDigits
	| '8#' OctalDigits
	| WORDLiteralID '#2#' BinaryDigits
	| WORDLiteralID '#' [0-9]+
	;
DWORDLiteral
	: DWORDLiteralID '#16#' HexDigits
	| DWORDLiteralID	'#8#' OctalDigits
	| '8#' OctalDigits
	| DWORDLiteralID '#2#' BinaryDigits
	| DWORDLiteralID '#' [0-9]+
	| 'L#' Sign? HexDigits
	;
POINTERLiteral
	:'16#' HexDigits
	|'16#' Digits
	;
INTLiteral
	: [0-9]+
	| INTLiteralID '#16#' HexDigits
	| INTLiteralID '#'[0-9]+
	| INTLiteralID '#2#' BinaryDigits
	| INTLiteralID '#8#' OctalDigits
	;	
DINTLiteral
	: DINTLiteralID '#16#' HexDigits
	| DINTLiteralID '#'[0-9]+
	| DINTLiteralID '#2#' BinaryDigits
	| DINTLiteralID '#8#' OctalDigits
	;
REALLiteral
	:  DecimalFloatingPointLiteral
	|  DecimalFloatingPointLiteral (('e'|'E') Sign? DecimalFloatingPointLiteral)? 
	| REALLiteralID '#' Sign? DecimalFloatingPointLiteral
	| REALLiteralID '#' Sign? DecimalFloatingPointLiteral (('e'|'E') Sign? DecimalFloatingPointLiteral)
	;	

TIMELiteral
	: TIMELiteralID '#' (DecimalFloatingPointLiteral 'd')? (DecimalFloatingPointLiteral 'h')? (DecimalFloatingPointLiteral 'm')? (DecimalFloatingPointLiteral 's')? (DecimalFloatingPointLiteral 'ms')?
	;
	
DATELiteral
	: DATELiteralID '#' Digits '-' Digits '-' Digits
	;

TIME_OF_DAYLiteral
	: TIME_OF_DAYLiteralID '#' (DecimalFloatingPointLiteral ':')  (DecimalFloatingPointLiteral ':')  (DecimalFloatingPointLiteral)
	;

DATE_AND_TIMELiteral
	: DATE_AND_TIMELiteralID '#' Digits '-' Digits '-' Digits '-' Digits ':' Digits ':' DecimalFloatingPointLiteral 
	;
	
GLOBALBOOLLiteral
	: 'M' Digits '.' Digit
	;
	
fragment
DATELiteralID
	: ('D'|'d')
	| ('D'|'d') ('A'|'a') ('T'|'t') ('E'|'e') 
	;
	
fragment
BYTELiteralID
	: ('B'|'b')
	| ('B'|'b') ('Y'|'y') ('T'|'t') ('E'|'e') 
	;
	
fragment
INTLiteralID
	: ('I'|'i') ('N'|'n') ('T'|'t')
	;
	
fragment
DINTLiteralID
	: ('D'|'d') ('I'|'i') ('N'|'n') ('T'|'t')
	;
fragment
WORDLiteralID
	: ('W'|'w') ('O'|'o') ('R'|'r') ('D'|'d')
	| ('W'|'w')
	;	
fragment
DWORDLiteralID
	: ('D'|'d') ('W'|'w') ('O'|'o') ('R'|'r') ('D'|'d')
	| ('D'|'d') ('W'|'w')
	;
	
fragment
REALLiteralID
	: ('R'|'r') ('E'|'e') ('A'|'a') ('L'|'l') 
	;
	
fragment
TIME_OF_DAYLiteralID
	: ('TOD') 
	| ('TIME_OF_DAY')
	;

fragment
TIMELiteralID
	: ('T'|'t') ('I'|'i') ('M'|'m') ('E'|'e') 
	| ('T'|'t')
	;
	
fragment
DATE_AND_TIMELiteralID
	: 'DT'
	| 'DATE_AND_TIME'
	;
	
fragment
DecimalFloatingPointLiteral
	:	Digits '.' Digits? 
	| Digits
	| [0-9]+ (('e'|'E') [0-9]+)? 
	;	

fragment
Digits
	:	Digit (DigitsAndUnderscores? Digit)?
	;

fragment
Digit
	:	'0'
	|	NonZeroDigit
	;

fragment
SignedInteger
	:	Sign? Digits
	;

fragment
Sign
	:	[+-]
	;
	
fragment
NonZeroDigit
	:	[1-9]
	;

fragment
DigitsAndUnderscores
	:	DigitOrUnderscore+
	;

fragment
DigitOrUnderscore
	:	Digit
	|	'_'
	;
	
fragment
HexDigits
	:	HexDigit (HexDigitsAndUnderscores? HexDigit)?
	;

fragment
HexDigit
	:	[0-9a-fA-F]
	;

fragment
HexDigitsAndUnderscores
	:	HexDigitOrUnderscore+
	;

fragment
HexDigitOrUnderscore
	:	HexDigit
	|	'_'
	;

fragment
OctalDigits
	:	OctalDigit (OctalDigitsAndUnderscores? OctalDigit)?
	;

fragment
OctalDigit
	:	[0-7]
	;

fragment
OctalDigitsAndUnderscores
	:	OctalDigitOrUnderscore+
	;

fragment
OctalDigitOrUnderscore
	:	OctalDigit
	|	'_'
	;
	
BinaryDigits
	:	BinaryDigit (BinaryDigitsAndUnderscores? BinaryDigit)?
	;

fragment
BinaryDigit
	:	[01]
	;

fragment
BinaryDigitsAndUnderscores
	:	BinaryDigitOrUnderscore+
	;

fragment
BinaryDigitOrUnderscore
	:	BinaryDigit
	|	'_'
	;	
	

	
fragment
StringCharacters
	:	StringCharacter+
	;
	
fragment
StringCharacter
	:	~['\\]
	|	EscapeSequence
	;
fragment
SingleCharacter
	:	~['\\]
	;	
fragment
EscapeSequence
	:	'\\' [btnfr"'\\]
	;
expressionName
:	Identifier identifierIndex?
|	ambiguousName '.' Identifier identifierIndex?
;
ambiguousName
:	Identifier identifierIndex?
|	ambiguousName '.' Identifier identifierIndex?
;

identifierIndex
:	'[' identifierIndexValue (',' identifierIndexValue)* ']'
;

identifierIndexValue
: INTLiteral
| expressionName
| expr
;

Identifier
:	Letter LetterOrDigit*
|  '"'Letter LetterOrDigit* '"'
;


//
// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '(*' (COMMENT | .)*?  '*)' ->skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;
COMMENT_STARTERS: 'TITLE'
				| 'VERSION'
				| 'FAMILY'
				| 'AUTHOR'
				| 'NAME'
				| 'NON_RETAIN'
				| 'UNLINKED'
				| '{'
				;
				
OTHER: COMMENT_STARTERS .*? '\n' ->skip;