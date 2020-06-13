Definitions.

DQSTR = "(\\.|[^\"\\]*)"
SQSTR = '(\\.|[^\'\\]*)'
BQSTR = \`(\\.|[^\`\\]*)\`
NUMBER = [\+\-\*\/]?[0-9]*(\.[0-9]+((e|E)[0-9]+)?)?

HASHCOMMENT = #[^\n]*\n
DASHCOMMENT = \-\-([\s\t]+[^\n]*)?\n
OCOMM = \/\*
CCOMM = \*\/
WHITESPACE   = [\s\t\n\r]

COMMA     = ,
SEMICOLON = ;
EQ   = \=
NEQ1 = \!\=
NEQ2 = \<\>
GT   = \>
GEQ  = \>\=
LT   = \<
LEQ  = \<\=
PLS  = \+
MNS  = \-
DIV  = \/
MUL  = \*
EXC  = \!
OPAR = \(
CPAR = \)
WORD = [^\s\t\n\r,;\!\-\=\<\>\+\-\/\*\(\)]+

Rules.

{DQSTR} : {token, TokenChars}.
{SQSTR} : {token, TokenChars}.
{BQSTR} : {token, TokenChars}.
{NUMBER} : {token, TokenChars}.

{HASHCOMMENT} : skip_token.
{DASHCOMMENT} : skip_token.
{OCOMM} : {token, begin_comment}.
{CCOMM} : {token, end_comment}.
{WHITESPACE}+ : skip_token.

{COMMA}     : {token, TokenChars}.
{SEMICOLON} : {token, TokenChars}.
{EQ}   : {token, TokenChars}.
{NEQ1} : {token, TokenChars}.
{NEQ2} : {token, TokenChars}.
{GT}   : {token, TokenChars}.
{GEQ}  : {token, TokenChars}.
{LT}   : {token, TokenChars}.
{LEQ}  : {token, TokenChars}.
{PLS}  : {token, TokenChars}.
{MNS}  : {token, TokenChars}.
{DIV}  : {token, TokenChars}.
{MUL}  : {token, TokenChars}.
{EXC}+ : {token, TokenChars}.
{OPAR} : {token, TokenChars}.
{CPAR} : {token, TokenChars}.
{WORD} : {token, TokenChars}.

Erlang code.
