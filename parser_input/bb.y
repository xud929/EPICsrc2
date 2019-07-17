%{
#include<iostream>
#include<map>
#include<string>

extern "C" int yylex(void);
extern "C" void yyerror(const char *);
extern "C" int yywrap(void);
std::map<std::string, int> bbints;
std::map<std::string, double> bbdbls;
std::map<std::string, bool> bbools;
std::map<std::string, std::string> bbstrs;
std::map<std::string, std::string> bbudef;
static std::string current_scope;
static int count=0;
namespace xcalc{
  double xparse(char*,double);
  int xparse(char*,int);
  void clear();
}
%}

%define parse.error verbose

%union{
	int xint;
	double xdouble;
	char *xstr;
};

%token <xint> INT BOOL
%token <xdouble> DOUBLE
%token <xstr> ID BEG END DCALC ICALC VAR STR
%token <xint> STOP STAT_SEPA PARA_SEPA
%type <xstr> beg end statement paragraph exp_str exp_var
%type <xint> exp_int exp_bool
%type <xdouble> exp_dbl
%left SEPARATOR

%%
paragraph: beg statement end {free($2);}
		 | paragraph SEPARATOR paragraph 
		 ;
statement: ID '=' exp_int {count=0;bbints[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | ID '=' exp_dbl {count=0;bbdbls[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | ID '=' exp_bool {count=0;bbools[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | ID '=' exp_var  {count=0;bbudef[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;free($3);}
		 | ID '=' exp_str  {count=0;bbstrs[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;free($3);}
		 | ID '[' INT ']' '=' exp_int {count=$3;bbints[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$6;$$=$1;}
		 | ID '[' INT ']' '=' exp_dbl {count=$3;bbdbls[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$6;$$=$1;}
		 | ID '[' INT ']' '=' exp_bool {count=$3;bbools[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$6;$$=$1;}
		 | ID '[' INT ']' '=' exp_var  {count=$3;bbudef[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$6;$$=$1;free($6);}
		 | ID '[' INT ']' '=' exp_str  {count=$3;bbstrs[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$6;$$=$1;free($6);}
		 | statement SEPARATOR exp_int {++count;bbints[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | statement SEPARATOR exp_dbl {++count;bbdbls[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | statement SEPARATOR exp_bool {++count;bbools[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;}
		 | statement SEPARATOR exp_var  {++count;bbudef[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;free($3);}
		 | statement SEPARATOR exp_str  {++count;bbudef[current_scope+" "+std::string($1)+" "+std::to_string(count)]=$3;$$=$1;free($3);}
		 | statement SEPARATOR statement {free($1);$$=$3;}
		 ;
exp_int	 : INT {$$=$1;}
		 | ICALC {$$=xcalc::xparse($1,0);xcalc::clear();free($1);}
		 ;
exp_bool : BOOL {$$=$1;}
		 ;
exp_dbl	 : DOUBLE {$$=$1;}
		 | DCALC {$$=xcalc::xparse($1,0.0);xcalc::clear();free($1);}
		 ;
exp_str	 : STR {$$=$1;}
		 ;
exp_var	 : VAR {$$=$1;}
		 ;
beg		 : BEG {current_scope=std::string($1);free($1);}
	  	 ;
end		 : END {current_scope.clear();}
	  	 ;
%%

void yyerror(const char *str){
	std::cerr<<str<<std::endl;
}
int yywrap(){
	return 1;
}

