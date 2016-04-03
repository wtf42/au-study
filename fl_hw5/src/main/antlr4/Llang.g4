grammar Llang;

KW_SKIP         : 'skip';
KW_WRITE        : 'write';
KW_READ         : 'read';
KW_WHILE        : 'while';
KW_DO           : 'do';
KW_IF           : 'if';
KW_THEN         : 'then';
KW_ELSE         : 'else';

PLUS            : '+';
MINUS           : '-';
TIMES           : '*';
DIVIDE          : '/';
MODULO          : '%';
EQ              : '==';
NOT_EQ          : '!=';
GREATER         : '>';
GREATER_EQ      : '>=';
LESS            : '<';
LESS_EQ         : '<=';
AND             : '&&';
OR              : '||';

COLON           : ';';
ASSIGN          : ':=';

IDENT           : [A-Za-z_][A-Za-z0-9_]*;
INT             : [0-9]+;
WS              : [ \t\r\n]+ -> skip;

op              : PLUS
                | MINUS
                | TIMES
                | DIVIDE
                | MODULO
                | EQ
                | NOT_EQ
                | GREATER
                | GREATER_EQ
                | LESS
                | LESS_EQ
                | AND
                | OR;
expr            : IDENT
                | INT
                | expr op expr;
s               : KW_SKIP
                | IDENT ASSIGN expr
                | s COLON s
                | KW_WRITE expr
                | KW_READ expr
                | KW_WHILE expr KW_DO s
                | KW_IF expr KW_THEN s KW_ELSE s;
p               : s EOF;
