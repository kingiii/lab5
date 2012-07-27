type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
  
val eof = (fn () => let val pos = hd(!linePos)
                    in  Tokens.EOF(pos, pos)
                    end)

%%

%header (functor JavaLexFun(structure Tokens: Java_TOKENS));
%s COMMENT LCOMMENT;
letter=[_a-zA-Z];
al=[_0-9a-zA-Z];
digit=[0-9];
id={letter}{al}*;
ws=[\ \t];

%%

<INITIAL>{ws}+ 	        => (continue());
<INITIAL>"."            => (Tokens.DOT (yypos, yypos + size yytext));
<INITIAL>","            => (Tokens.COMMA (yypos, yypos + size yytext));
<INITIAL>";"			=> (Tokens.SEMICOLON (yypos, yypos + size yytext));
<INITIAL>"("			=> (Tokens.LPAREN (yypos, yypos + size yytext));
<INITIAL>")"            => (Tokens.RPAREN (yypos, yypos + size yytext));
<INITIAL>"{"            => (Tokens.LBRACE (yypos,yypos + size yytext));
<INITIAL>"}"            => (Tokens.RBRACE (yypos,yypos + size yytext));
<INITIAL>"["            => (Tokens.LBRACK (yypos,yypos + size yytext));
<INITIAL>"]"            => (Tokens.RBRACK (yypos,yypos + size yytext));

<INITIAL>"+"            => (Tokens.PLUS (yypos, yypos + size yytext));
<INITIAL>"-"            => (Tokens.MINUS (yypos, yypos + size yytext));
<INITIAL>"*"            => (Tokens.TIMES (yypos, yypos + size yytext));
<INITIAL>"/"            => (Tokens.DIV (yypos, yypos + size yytext));
<INITIAL>"%"            => (Tokens.MOD (yypos, yypos + size yytext));

<INITIAL>"<"            => (Tokens.LT (yypos,yypos + size yytext));
<INITIAL>"<="            => (Tokens.LE (yypos,yypos + size yytext));
<INITIAL>">"            => (Tokens.GT (yypos,yypos + size yytext));
<INITIAL>">="            => (Tokens.GE (yypos,yypos + size yytext));
<INITIAL>"=="            => (Tokens.EQ (yypos,yypos + size yytext));
<INITIAL>"!="            => (Tokens.NEQ (yypos,yypos + size yytext));

<INITIAL>"="            => (Tokens.ASSIGN (yypos, yypos + size yytext));
<INITIAL>"!"            => (Tokens.NOT (yypos,yypos + size yytext));
<INITIAL>"&&"           => (Tokens.AND (yypos,yypos + size yytext));
<INITIAL>"||"           => (Tokens.OR (yypos,yypos + size yytext));

<INITIAL>"boolean"      => (Tokens.BOOLEAN (yypos,yypos + size yytext));
<INITIAL>"class"        => (Tokens.CLASS (yypos,yypos + size yytext));
<INITIAL>"else"         => (Tokens.ELSE (yypos,yypos + size yytext));
<INITIAL>"extends"      => (Tokens.EXTENDS (yypos,yypos + size yytext));
<INITIAL>"false"        => (Tokens.FALSE (yypos,yypos + size yytext));
<INITIAL>"if"           => (Tokens.IF (yypos,yypos + size yytext));
<INITIAL>"int"          => (Tokens.INT (yypos,yypos + size yytext));
<INITIAL>"length"       => (Tokens.LENGTH (yypos,yypos + size yytext));
<INITIAL>"main"         => (Tokens.MAIN (yypos,yypos + size yytext));
<INITIAL>"new"          => (Tokens.NEW (yypos,yypos + size yytext));
<INITIAL>"System.out.println"      => (Tokens.SYSTEM_OUT_PRINTLN (yypos,yypos + size yytext));
<INITIAL>"public"       => (Tokens.PUBLIC (yypos,yypos + size yytext));
<INITIAL>"return"       => (Tokens.RETURN (yypos,yypos + size yytext));
<INITIAL>"static"       => (Tokens.STATIC (yypos,yypos + size yytext));
<INITIAL>"String"       => (Tokens.STRING (yypos,yypos + size yytext));
<INITIAL>"this"         => (Tokens.THIS (yypos,yypos + size yytext));
<INITIAL>"true"         => (Tokens.TRUE (yypos,yypos + size yytext));
<INITIAL>"false"        => (Tokens.FALSE (yypos,yypos + size yytext));
<INITIAL>"void"         => (Tokens.VOID (yypos,yypos + size yytext));
<INITIAL>"while"        => (Tokens.WHILE (yypos,yypos + size yytext));

<INITIAL>"/*"           => (YYBEGIN COMMENT; continue());

<COMMENT>"/*"           => (continue());
<COMMENT>"*/"           => (YYBEGIN INITIAL; continue());	

<INITIAL>"//"		    => (YYBEGIN LCOMMENT; continue());
<LCOMMENT>\n		    => (YYBEGIN INITIAL;
                            lineNum := !lineNum+1;
                            linePos := yypos :: !linePos;
                            continue());

<COMMENT,LCOMMENT>.	=> (continue());
<INITIAL>{id}           => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>{digit}+	=> (Tokens.INTEGER_LITERAL(Option.valOf(Int.fromString(yytext)), yypos, yypos + size yytext));
<INITIAL,COMMENT>\n	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL,COMMENT>\r     => (continue());
<INITIAL>.              => (ErrorMsg.error (yypos, String.concat["Invalid character: [", yytext, "]"]); continue());
