type pos = int
type svalue = Tokens.svalue
type lexresult = (svalue, pos) Tokens.token
type ('a, 'b) token = ('a, 'b) Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun intToString s = let val SOME x = Int.fromString s in x end

%% 
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%%

<INITIAL>if    	  => (Tokens.IF(yypos, yypos+2));
<INITIAL>while 	  => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>for   	  => (Tokens.FOR(yypos, yypos+3));
<INITIAL>to    	  => (Tokens.TO(yypos, yypos+2));
<INITIAL>break 	  => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>let   	  => (Tokens.LET(yypos, yypos+3));
<INITIAL>in    	  => (Tokens.IN(yypos, yypos+2));
<INITIAL>end   	  => (Tokens.END(yypos, yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>var      => (Tokens.VAR(yypos, yypos+3));
<INITIAL>type     => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>array    => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL>then     => (Tokens.THEN(yypos, yypos+4));
<INITIAL>else     => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>do  	  => (Tokens.DO(yypos, yypos+2));
<INITIAL>of       => (Tokens.OF(yypos, yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos, yypos+3));
<INITIAL>var  	  => (Tokens.VAR(yypos,yypos+3));

","			   => (Tokens.COMMA(yypos,yypos+1));
":"			   => (Tokens.COLON(yypos, yypos+1));
";" 		   => (Tokens.SEMICOLON(yypos, yypos+1));
"("			   => (Tokens.LPAREN(yypos, yypos+1));
")"			   => (Tokens.RPAREN(yypos, yypos+1));
"["			   => (Tokens.LBRACK(yypos, yypos+1));
"]"			   => (Tokens.RBRACK(yypos, yypos+1));
"{"			   => (Tokens.LBRACE(yypos, yypos+1));
"}"			   => (Tokens.RBRACE(yypos, yypos+1));
"."			   => (Tokens.DOT(yypos, yypos+1));
"+"			   => (Tokens.PLUS(yypos, yypos+1));
"-"			   => (Tokens.MINUS(yypos, yypos+1));
"*"			   => (Tokens.TIMES(yypos, yypos+1));
"/"			   => (Tokens.DIVIDE(yypos, yypos+1));
"="			   => (Tokens.EQ(yypos, yypos+1));
"<>"		   => (Tokens.NEQ(yypos, yypos+2));
"<"			   => (Tokens.LT(yypos, yypos+1));
"<="		   => (Tokens.LE(yypos, yypos+2));
">"			   => (Tokens.GT(yypos, yypos+1));
">="		   => (Tokens.GE(yypos, yypos+2));
"&"			   => (Tokens.AND(yypos, yypos+1));
"|"			   => (Tokens.OR(yypos, yypos+1));
":="           => (Tokens.ASSIGN(yypos, yypos+2));

\"[^\"]*\"     		 => (Tokens.STRING(yytext, yypos, yypos + size yytext));
[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos + size yytext));
[0-9]+	       		 => (Tokens.INT(intToString yytext, yypos, yypos + size yytext));
\/\*.*\*\/     		 => (continue());
[\ \t\b\f\r]+  		 => (continue());

.       	   => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
\n			   => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

