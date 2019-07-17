%define api.prefix {zz}
%{
#include<iostream>
#include<string>
#include<map>
#include<vector>
#include<cmath>

extern "C" int yylex(void);
extern "C" void yyerror(const char *);
extern "C" int zzwrap(void);

typedef struct zz_buffer_state * ZZ_BUFFER_STATE;
extern "C" ZZ_BUFFER_STATE zz_scan_string(char * str);
extern "C" void zz_delete_buffer(ZZ_BUFFER_STATE buffer);

namespace xcalc{
int reti;
double retd;
int ret_type=-1;
std::map<std::string,int> macro_int;
std::map<std::string,double> macro_dbl;
std::map<std::string,int> macro_type;
inline int check(const char*);
inline char *set_tmp();
inline void releasetmp(const char*);
template<typename T>
inline char* calc_process(char *, T, int);
template<>
inline char* calc_process(char *, int, int);
template<>
inline char* calc_process(char *, double, int);
template<>
inline char* calc_process(char *, char *, int);
inline char* calc_process(char *, int);
template<typename R, typename T>
inline R calc_process(R, T, int);
}

%}

/*
	+,-,*,/ ---> 1,2,3,4 
	exp ---> 5
	sin,cos,tan,cot ---> 11,12,13,14
*/

%define parse.error verbose

%union xdata{
	int xint;
	double xdouble;
	char *xstr;
};


%token <xint> INT
%token <xdouble> DOUBLE
%token <xstr> ID
%token <xint> UOP
%type <xint> exp exps exp_int
%type <xdouble> exp_dbl
%type <xstr> exp_var

%left ','
%right '='
%left '+' '-'
%left '*' '/'
%right UOP
%nonassoc UMINUS

%%
exps		: exp  								{$$=$1;xcalc::ret_type=$$;}
	  		| exps ',' exp						{$$=$3;xcalc::ret_type=$$;}
	  		;
exp			:									{$$=-1;} 
	  		| exp_int							{xcalc::reti=$1;$$=0;}
	  		| exp_dbl							{xcalc::retd=$1;$$=1;}
			| exp_var							
			{
				$$=xcalc::macro_type[$1];
				if($$==1)
					xcalc::retd=xcalc::macro_dbl[$1];
				else if($$==0)
					xcalc::reti=xcalc::macro_int[$1];
				if($1[0]=='-')
					xcalc::releasetmp($1);
				free($1);
			}
			;
exp_int		: INT								{$$=$1;}
		 	| exp_int '+' exp_int				{$$=$1+$3;}
		 	| exp_int '-' exp_int  				{$$=$1-$3;}
		 	| exp_int '*' exp_int   			{$$=$1*$3;}
		 	| exp_int '/' exp_int   			{$$=$1/$3;}
			| '-' exp_int %prec UMINUS 			{$$=-$2;}
			| '(' exp_int ')'					{$$=$2;}
			;
exp_dbl		: DOUBLE							{$$=$1;}
		 	| exp_dbl '+' exp_dbl				{$$=$1+$3;}
		 	| exp_dbl '-' exp_dbl  				{$$=$1-$3;}
		 	| exp_dbl '*' exp_dbl   			{$$=$1*$3;}
		 	| exp_dbl '/' exp_dbl   			{$$=$1/$3;}
			| '-' exp_dbl %prec UMINUS 			{$$=-$2;}
			| '(' exp_dbl ')'					{$$=$2;}
			| exp_dbl '+' exp_int				{$$=$1+$3;}
			| exp_dbl '-' exp_int				{$$=$1-$3;}
			| exp_dbl '*' exp_int				{$$=$1*$3;}
			| exp_dbl '/' exp_int				{$$=$1/$3;}
			| exp_int '+' exp_dbl				{$$=$1+$3;}
			| exp_int '-' exp_dbl				{$$=$1-$3;}
			| exp_int '*' exp_dbl				{$$=$1*$3;}
			| exp_int '/' exp_dbl				{$$=$1/$3;}
			| UOP exp_dbl						{$$=xcalc::calc_process(0.0,$2,$1);}
			| UOP exp_int						{$$=xcalc::calc_process(0.0,$2,$1);}
			;
exp_var		: ID								
		 	{
				if(xcalc::macro_type.find($1)==xcalc::macro_type.end())
				  xcalc::macro_type[$1]=-1;
				$$=$1;
			}
		 	| exp_var '=' exp_int				
			{
				if($1[0]=='-'){
					xcalc::releasetmp($1);
					yyerror("Invalid assignment of temporary variables!\n");
					free($1);
					return 1;
				}
				$$=$1;
				xcalc::macro_type[$$]=0;
				xcalc::macro_int[$1]=$3;
				xcalc::macro_dbl.erase($1);
			}			
		 	| exp_var '=' exp_dbl				
			{
				if($1[0]=='-'){
					xcalc::releasetmp($1);
					yyerror("Invalid assignment of temporary variables!\n");
					free($1);
					return 1;
				}
				$$=$1;
				xcalc::macro_type[$$]=1;
				xcalc::macro_dbl[$1]=$3;
				xcalc::macro_int.erase($1);
			}
			| exp_var '=' exp_var				
			{
				if($1[0]=='-'){
					xcalc::releasetmp($1);
					yyerror("Invalid assignment of temporary variables!\n");
					if($3[0]=='-')
						xcalc::releasetmp($3);
					free($1);
					free($3);
					return 1;
				}
				int type=xcalc::check($3);
				if(type==-1){
					free($1);
					free($3);
					return 1;
				}
				if(type){
					xcalc::macro_dbl[$1]=xcalc::macro_dbl[$3];
					xcalc::macro_int.erase($1);
				}else{
					xcalc::macro_int[$1]=xcalc::macro_int[$3];
					xcalc::macro_dbl.erase($1);
				}
				xcalc::macro_type[$1]=type;
				if($3[0]=='-')
					xcalc::releasetmp($3);
				free($3);
			}
			| exp_var '+' exp_int 			{$$=xcalc::calc_process($1,$3,1);if($$==NULL)return 1;}
			| exp_int '+' exp_var			{$$=xcalc::calc_process($3,$1,-1);if($$==NULL)return 1;}
			| exp_var '+' exp_dbl 			{$$=xcalc::calc_process($1,$3,1);if($$==NULL)return 1;}
			| exp_dbl '+' exp_var			{$$=xcalc::calc_process($3,$1,-1);if($$==NULL)return 1;}
			| exp_var '+' exp_var			{$$=xcalc::calc_process($1,$3,1);if($$==NULL)return 1;}
			| exp_var '-' exp_int 			{$$=xcalc::calc_process($1,$3,2);if($$==NULL)return 1;}
			| exp_int '-' exp_var			{$$=xcalc::calc_process($3,$1,-2);if($$==NULL)return 1;}
			| exp_var '-' exp_dbl 			{$$=xcalc::calc_process($1,$3,2);if($$==NULL)return 1;}
			| exp_dbl '-' exp_var			{$$=xcalc::calc_process($3,$1,-2);if($$==NULL)return 1;}
			| exp_var '-' exp_var			{$$=xcalc::calc_process($1,$3,2);if($$==NULL)return 1;}
			| exp_var '*' exp_int 			{$$=xcalc::calc_process($1,$3,3);if($$==NULL)return 1;}
			| exp_int '*' exp_var			{$$=xcalc::calc_process($3,$1,-3);if($$==NULL)return 1;}
			| exp_var '*' exp_dbl 			{$$=xcalc::calc_process($1,$3,3);if($$==NULL)return 1;}
			| exp_dbl '*' exp_var			{$$=xcalc::calc_process($3,$1,-3);if($$==NULL)return 1;}
			| exp_var '*' exp_var			{$$=xcalc::calc_process($1,$3,3);if($$==NULL)return 1;}
			| exp_var '/' exp_int 			{$$=xcalc::calc_process($1,$3,4);if($$==NULL)return 1;}
			| exp_int '/' exp_var			{$$=xcalc::calc_process($3,$1,-4);if($$==NULL)return 1;}
			| exp_var '/' exp_dbl 			{$$=xcalc::calc_process($1,$3,4);if($$==NULL)return 1;}
			| exp_dbl '/' exp_var			{$$=xcalc::calc_process($3,$1,-4);if($$==NULL)return 1;}
			| exp_var '/' exp_var			{$$=xcalc::calc_process($1,$3,4);if($$==NULL)return 1;}
			| '(' exp_var ')'				{$$=$2;}
			| '-' exp_var %prec UMINUS 		{$$=xcalc::calc_process($2,-1,3);if($$==NULL)return 1;}
			| UOP exp_var					{$$=xcalc::calc_process($2,$1);if($$==NULL)return 1;}
			;
%%

namespace xcalc{
inline char *set_tmp(){
	static int az=0;
	int width=1;
	int current=az/26;
	char *v=new char[52];
	v[0]='a'+az%26;
	while(current!=0){
		v[width]='a'+current%26;
		current/=26;
		++width;
	}
	char *tmp=new char[width+2];
	tmp[0]='-';tmp[width+1]='\0';
	for(unsigned i=1;i<=width;++i)
		tmp[i]=v[width-i];
	delete [] v;
	++az;
	return tmp;
}
inline int check(const char* str){
	int type=macro_type[str];
	if(type==-1){
		yyerror(("variable "+std::string(str)+" undefined!").c_str());
	}
	return type;
}
inline void releasetmp(const char* str){
	macro_dbl.erase(str);
	macro_int.erase(str);
	macro_type.erase(str);
}
template<>
inline char* calc_process(char *var, int value, int bop)
{
	char *ttt;
	int type=check(var);
	if(type==-1){
		free(var);
		return NULL;
	}
	if(var[0]=='-')
		ttt=var;
	else
		ttt=set_tmp();
	macro_type[ttt]=type;
	if(type==1){
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_dbl[var]+value;
				break;
			case 2:
				macro_dbl[ttt]=macro_dbl[var]-value;
				break;
			case -2:
				macro_dbl[ttt]=value-macro_dbl[var];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_dbl[var]*value;
				break;
			case 4:
				macro_dbl[ttt]=macro_dbl[var]/value;
				break;
			case -4:
				macro_dbl[ttt]=value/macro_dbl[var];
				break;
			default:
				break;
		}
	}else{
		switch(bop){
			case 1: case -1:
				macro_int[ttt]=macro_int[var]+value;
				break;
			case 2:
				macro_int[ttt]=macro_int[var]-value;
				break;
			case -2:
				macro_int[ttt]=value-macro_int[var];
				break;
			case 3: case -3:
				macro_int[ttt]=macro_int[var]*value;
				break;
			case 4:
				macro_int[ttt]=macro_int[var]/value;
				break;
			case -4:
				macro_int[ttt]=value/macro_int[var];
				break;
			default:
				break;
		}
	}
	if(ttt!=var){
		if(var[0]=='-')
		  releasetmp(var);
		free(var);
	}
	return ttt;
}
template<>
inline char* calc_process(char *var, double value, int bop)
{
	char *ttt;
	int type=check(var);
	if(type==-1){
		free(var);
		return NULL;
	}
	if(var[0]=='-')
		ttt=var;
	else
		ttt=set_tmp();
	macro_type[ttt]=1;
	if(type==1){
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_dbl[var]+value;
				break;
			case 2:
				macro_dbl[ttt]=macro_dbl[var]-value;
				break;
			case -2:
				macro_dbl[ttt]=value-macro_dbl[var];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_dbl[var]*value;
				break;
			case 4:
				macro_dbl[ttt]=macro_dbl[var]/value;
				break;
			case -4:
				macro_dbl[ttt]=value/macro_dbl[var];
				break;
			default:
				break;
		}
	}else{
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_int[var]+value;
				break;
			case 2:
				macro_dbl[ttt]=macro_int[var]-value;
				break;
			case -2:
				macro_dbl[ttt]=value-macro_int[var];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_int[var]*value;
				break;
			case 4:
				macro_dbl[ttt]=macro_int[var]/value;
				break;
			case -4:
				macro_dbl[ttt]=value/macro_int[var];
				break;
			default:
				break;
		}
	}
	if(ttt!=var){
		if(var[0]=='-')
		  releasetmp(var);
		free(var);
	}
	return ttt;
}
template<>
inline char* calc_process(char* var1, char* var2, int bop)
{
	char *ttt;
	int left=check(var1), right=check(var2);
	if(left==-1 || right==-1){
		free(var1);free(var2);
		return NULL;
	}
	if(var1[0]=='-')
		ttt=var1;
	else if(var2[0]=='-')
		ttt=var2;
	else
		ttt=set_tmp();
	if(left==0 && right==0){
		switch(bop){
			case 1: case -1:
				macro_int[ttt]=macro_int[var1]+macro_int[var2];
				break;
			case 2:
				macro_int[ttt]=macro_int[var1]-macro_int[var2];
				break;
			case -2:
				macro_int[ttt]=macro_int[var2]-macro_int[var1];
				break;
			case 3: case -3:
				macro_int[ttt]=macro_int[var1]*macro_int[var2];
				break;
			case 4:
				macro_int[ttt]=macro_int[var1]/macro_int[var2];
				break;
			case -4:
				macro_int[ttt]=macro_int[var2]/macro_int[var1];
				break;
			default:
				break;
		}
	}else if(left==1 && right==0){
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_dbl[var1]+macro_int[var2];
				break;
			case 2:
				macro_dbl[ttt]=macro_dbl[var1]-macro_int[var2];
				break;
			case -2:
				macro_dbl[ttt]=macro_int[var2]-macro_dbl[var1];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_dbl[var1]*macro_int[var2];
				break;
			case 4:
				macro_dbl[ttt]=macro_dbl[var1]/macro_int[var2];
				break;
			case -4:
				macro_dbl[ttt]=macro_int[var2]/macro_dbl[var1];
				break;
			default:
				break;
		}
	}else if(left==0 && right==1){
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_int[var1]+macro_dbl[var2];
				break;
			case 2:
				macro_dbl[ttt]=macro_int[var1]-macro_dbl[var2];
				break;
			case -2:
				macro_dbl[ttt]=macro_dbl[var2]-macro_int[var1];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_int[var1]*macro_dbl[var2];
				break;
			case 4:
				macro_dbl[ttt]=macro_int[var1]/macro_dbl[var2];
				break;
			case -4:
				macro_dbl[ttt]=macro_dbl[var2]/macro_int[var1];
				break;
			default:
				break;
		}
	}else{
		switch(bop){
			case 1: case -1:
				macro_dbl[ttt]=macro_dbl[var1]+macro_dbl[var2];
				break;
			case 2:
				macro_dbl[ttt]=macro_dbl[var1]-macro_dbl[var2];
				break;
			case -2:
				macro_dbl[ttt]=macro_dbl[var2]-macro_dbl[var1];
				break;
			case 3: case -3:
				macro_dbl[ttt]=macro_dbl[var1]*macro_dbl[var2];
				break;
			case 4:
				macro_dbl[ttt]=macro_dbl[var1]/macro_dbl[var2];
				break;
			case -4:
				macro_dbl[ttt]=macro_dbl[var2]/macro_dbl[var1];
				break;
			default:
				break;
		}
	}
	macro_type[ttt]=left || right;
	if(ttt==var1){
		if(var2[0]=='-')
			releasetmp(var2);
		free(var2);
	}else if(ttt==var2){
		if(var1[0]=='-')
			releasetmp(var1);
		free(var1);
	}else{
		if(var1[0]=='-')
			releasetmp(var1);
		if(var2[0]=='-')
			releasetmp(var2);
		free(var1);
		free(var2);
	}
	return ttt;
}
template<typename R,typename T>
inline R calc_process(R, T value, int uop){
	R r;
	switch(uop){
		case 5: r=exp(value);break;
		case 11:r=sin(value);break;
		case 12:r=cos(value);break;
		case 13:r=tan(value);break;
		case 14:r=1.0/tan(value);break;
		default:break;
	}
	return r;
}
inline char* calc_process(char *var, int uop){
	char *ttt;
	int type=check(var);
	if(type==-1){
		free(var);
		return NULL;
	}
	if(var[0]=='-')
		ttt=var;
	else
		ttt=set_tmp();
	
	if(type){
		switch(uop){
			case 5:
				macro_type[ttt]=1;
				macro_dbl[ttt]=exp(macro_dbl[var]);
				break;
			case 11:
				macro_type[ttt]=1;
				macro_dbl[ttt]=sin(macro_dbl[var]);
				break;
			case 12:
				macro_type[ttt]=1;
				macro_dbl[ttt]=cos(macro_dbl[var]);
				break;
			case 13:
				macro_type[ttt]=1;
				macro_dbl[ttt]=tan(macro_dbl[var]);
				break;
			case 14:
				macro_type[ttt]=1;
				macro_dbl[ttt]=1.0/tan(macro_dbl[var]);
				break;
			default:
				break;
		}
	}else{
		switch(uop){
			case 5:
				macro_type[ttt]=1;
				macro_dbl[ttt]=exp(macro_int[var]);
				break;
			case 11:
				macro_type[ttt]=1;
				macro_dbl[ttt]=sin(macro_int[var]);
				break;
			case 12:
				macro_type[ttt]=1;
				macro_dbl[ttt]=cos(macro_int[var]);
				break;
			case 13:
				macro_type[ttt]=1;
				macro_dbl[ttt]=tan(macro_int[var]);
				break;
			case 14:
				macro_type[ttt]=1;
				macro_dbl[ttt]=1.0/tan(macro_int[var]);
				break;
			default:
				break;
		}
	}
	if(ttt!=var){
		releasetmp(var);
		free(var);
	}
	return ttt;
}

}
void yyerror(const char *str){
	xcalc::ret_type=-1;
	std::cerr<<str<<std::endl;
}
int zzwrap(){
	return 1;
}

namespace xcalc{
double xparse(char* str,double){
	ZZ_BUFFER_STATE buffer=zz_scan_string(str);
	zzparse();
	zz_delete_buffer(buffer);
	if(xcalc::ret_type==1)
		return xcalc::retd;
	else if(xcalc::ret_type==0)
		return xcalc::reti;
	else{
		std::cerr<<"Warning: no valid value, default 0 is returned."<<std::endl;
		return 0.0;
	}
}
int xparse(char* str,int){
	ZZ_BUFFER_STATE buffer=zz_scan_string(str);
	zzparse();
	zz_delete_buffer(buffer);
	if(xcalc::ret_type==1)
		return xcalc::retd;
	else if(xcalc::ret_type==0)
		return xcalc::reti;
	else{
		std::cerr<<"Warning: no valid value, default 0 is returned."<<std::endl;
		return 0;
	}
}
void xparse(char* str){
	ZZ_BUFFER_STATE buffer=zz_scan_string(str);
	zzparse();
	zz_delete_buffer(buffer);
}
void clear(){
	ret_type=-1;
	macro_int.clear();
	macro_dbl.clear();
	macro_type.clear();
}
}
