import java_cup.runtime.*;

%%

%cup
%line
%column
%unicode
%class LLexer

%{

class Tok {
    public String name;
    public String value;
    Tok(String name, String value) {
        this.name = name;
        this.value = value;
    }
}

Symbol newSym(int tokenId, String name, String value) {
    return new Symbol(tokenId, yycolumn, yycolumn + yylength() - 1, new Tok(name, value));
}

Symbol newSym(int tokenId, String name) {
    return newSym(tokenId, name, null);
}

Symbol newKW(int tokenId, String name) {
    return newSym(tokenId, "KW_" + name, null);
}

Symbol newOp(int tokenId, String value) {
    return newSym(tokenId, "OP", value);
}

Symbol newVar(int tokenId, String value) {
    return newSym(tokenId, "Var", "\"" + value + "\"");
}

Symbol newNum(int tokenId, String value) {
    return newSym(tokenId, "Num", value);
}

static class LlangException extends RuntimeException {
    String text;
    int line;
    int col;
    public LlangException(String text, int line, int col) {
        this.text = text;
        this.line = line;
        this.col = col;
    }
}

%}

ident           = [A-Za-z_]([A-Za-z0-9_])*
int             = [0-9]+
whitespace      = [ \n\t]

%%

skip            { return newKW(sym.SKIP, "skip"); }
write           { return newKW(sym.WRITE, "write"); }
read            { return newKW(sym.READ, "read"); }
while           { return newKW(sym.WHILE, "while"); }
do              { return newKW(sym.DO, "do"); }
if              { return newKW(sym.IF, "if"); }
then            { return newKW(sym.THEN, "then"); }
else            { return newKW(sym.ELSE, "else"); }

"+"             { return newOp(sym.PLUS, "plus"); }
"-"             { return newOp(sym.MINUS, "minus"); }
"*"             { return newOp(sym.TIMES, "times"); }
"/"             { return newOp(sym.DIVIDE, "divide"); }
"%"             { return newOp(sym.MODULO, "modulo"); }
"=="            { return newOp(sym.EQ, "eq"); }
"!="            { return newOp(sym.NOT_EQ, "not_eq"); }
">"             { return newOp(sym.GTR, "greater"); }
">="            { return newOp(sym.GTR_EQ, "greater_eq"); }
"<"             { return newOp(sym.LESS, "less"); }
"<="            { return newOp(sym.LESS_EQ, "less_eq"); }
"&&"            { return newOp(sym.AND, "and"); }
"||"            { return newOp(sym.OR, "or"); }

";"             { return newSym(sym.SEMICOLON, "colon"); }
":="            { return newSym(sym.ASSIGN, "assign"); }

{ident}         { return newVar(sym.IDENT, yytext()); }
{int}           { return newNum(sym.INT, yytext()); }
op              ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | ">" | ">=" | "<" | "<=" | "&&" | "||" { }
expr            ::= {ident} |
                    {int} |
                    expr op expr
                    { }
S               ::= "skip" |
                    {ident} ":=" expr |
                    S ";" S |
                    "write" expr |
                    "read" expr |
                    "while" expr "do" S |
                    "if" expr "then" S "else" S
                    { }
<YYINITIAL> {
    P           ::= S { }
    {whitespace} { }
}
.               { throw new LlangException(yytext(), yyline, yycolumn); }
