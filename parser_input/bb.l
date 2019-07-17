%{
#include"bb_yacc.h"
%}

%%
\/\/.*										{/*comment*/}
[/][*](.|[\n])*[*][/]						{/*comment*/}
[-+]?[0-9]+ 								{yylval.xint=atoi(yytext);return INT;}
[-+]?[0-9]*\.?[0-9]*(([eE][-+]?)?[0-9]+)?	{yylval.xdouble=atof(yytext);return DOUBLE;}
true										{yylval.xint=1;return BOOL;}
false										{yylval.xint=0;return BOOL;}
[ \t\n] 									;
[=\[\]]										return *yytext;
[,;]										return SEPARATOR;
[_a-zA-Z][_a-zA-Z0-9]*						{yylval.xstr=strdup(yytext);return ID;}
&end										{yylval.xstr=NULL;return END;}
&[_a-zA-Z][_a-zA-Z0-9]*						{yylval.xstr=strdup(yytext+1);return BEG;}
d<.*>										{yytext[strlen(yytext)-1]='\0';yylval.xstr=strdup(yytext+2);return DCALC;}
i<.*>										{yytext[strlen(yytext)-1]='\0';yylval.xstr=strdup(yytext+2);return ICALC;}
v<[_a-zA-Z][_a-zA-Z0-9]*>					{yytext[strlen(yytext)-1]='\0';yylval.xstr=strdup(yytext+2);return VAR;}
\".*\"										{yytext[strlen(yytext)-1]='\0';yylval.xstr=strdup(yytext+1);return STR;}
%%
