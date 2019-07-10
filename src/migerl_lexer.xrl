Definitions.

DQSTR = "(\\.|[^\"\\]*)"
SQSTR = '(\\.|[^\'\\]*)'
BQSTR = \`(\\.|[^\`\\]*)\`

COMMENTBLOCK = \-\-[^\n]*\n
WHITESPACE   = [\s\t\n\r]+

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
OPAR = \(
CPAR = \)
OCOM = \/\*
CCOM = \*\/
WORD = [^\s\t\n\r,;\!\-\=\<\>\+\-\/\*\(\)]+

Rules.

{DQSTR} : {token, TokenChars}.
{SQSTR} : {token, TokenChars}.
{BQSTR} : {token, TokenChars}.

{COMMENTBLOCK} : skip_token.
{WHITESPACE}   : skip_token.

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
{OPAR} : {token, TokenChars}.
{CPAR} : {token, TokenChars}.
{OCOM} : {token, TokenChars}.
{CCOM} : {token, TokenChars}.
{WORD} : {token, TokenChars}.

Erlang code.
