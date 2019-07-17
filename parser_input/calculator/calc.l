%{
#include"calc_yacc.h"
#include<math.h>
#define yylval zzlval
%}

%option prefix="zz"

%%
[-+*/=(),]										return *yytext;
[0-9]+ 											{yylval.xint=atoi(yytext);return INT;}
[0-9]*\.?[0-9]*(([eE][-+]?)?[0-9]+)?			{yylval.xdouble=atof(yytext);return DOUBLE;}
[ \t\n] 										;
pi												{yylval.xdouble=M_PI;return DOUBLE;}
PI												{yylval.xdouble=M_PI;return DOUBLE;}
M_PI											{yylval.xdouble=M_PI;return DOUBLE;}
pmass											{yylval.xdouble=938.272081e6;return DOUBLE;}
emass											{yylval.xdouble=0.5109989461e6;return DOUBLE;}
re												{yylval.xdouble=2.8179403227e-15;return DOUBLE;}
clight											{yylval.xdouble=299792458.0;return DOUBLE;}
exp												{yylval.xint=5;return UOP;}
sin												{yylval.xint=11;return UOP;}
cos												{yylval.xint=12;return UOP;}
tan												{yylval.xint=13;return UOP;}
cot												{yylval.xint=14;return UOP;}
[_a-zA-Z][_a-zA-Z0-9]*							{yylval.xstr=strdup(yytext);return ID;}
%%
