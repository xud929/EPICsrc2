#include<iostream>
#include<fstream>
#include<map>
#include<string>
#include<cmath>
#include<set>
#include<regex>
#include<sstream>
#include"bb_yacc.h"

extern std::map<std::string,int> bbints;
extern std::map<std::string,double> bbdbls;
extern std::map<std::string,bool> bbools;
extern std::map<std::string,std::string> bbstrs;
extern std::map<std::string,std::string> bbudef;
extern FILE *yyin;

using std::ofstream;
using std::string;
using std::regex;using std::regex_match;using std::regex_search;using std::regex_replace;using std::smatch;using std::sregex_iterator;
using std::ostringstream;

namespace xcalc{
  void xparse(char*);
  void clear();
  extern std::map<string,int> macro_int;
  extern std::map<string,double> macro_dbl;
}

string check_input(char *file, char *cmd){
  string check_info;
  yyin=fopen(file,"r");
  if(yyin==NULL){
	check_info+="Error: Read output file failed!\n";
	return check_info;
  }
  yyparse();

  std::set<string> defined_int,defined_dbl;
  //process undefined variable
  if(!bbudef.empty() && cmd!=NULL){
	xcalc::xparse(cmd);
	for(const auto i : bbudef){
	  auto it=xcalc::macro_dbl.find(i.second);
	  if(it==xcalc::macro_dbl.end()){
		auto it=xcalc::macro_int.find(i.second);
		if(it==xcalc::macro_int.end())
		  check_info+="Error: variable ("+i.second+") is undefined.\n";
		else{
		  bbints[i.first]=it->second;
		  defined_int.insert(it->first);
		}
	  }else{
		bbdbls[i.first]=it->second;
		defined_dbl.insert(it->first);
	  }
	}
  }

  //process strings
  if(!bbstrs.empty()){
	string fmt=file;
	auto pos=fmt.find_last_of(".");
	if(pos!=string::npos)
	  fmt=fmt.substr(0,pos);
	for(auto & str : bbstrs){
	  regex r("%[sS]");
	  str.second=regex_replace(str.second,r,fmt);
	  for(const auto & i : defined_int){
		r="([\\s\\S]*)%([0-9]+)?d\\{"+i+"\\}([\\s\\S]*)";
		string tmp;
		for(sregex_iterator itr(str.second.begin(),str.second.end(),r),end_itr;itr!=end_itr;++itr){
		  auto m=*itr;
		  std::size_t width=0;
		  if(m[2].matched)
			width=std::stoi(m[2].str());
		  auto s=std::to_string(xcalc::macro_int.at(i));
		  if(s.size()<width)
			s=string(width-s.size(),'0')+s;
		  tmp+=m[1].str()+s+m[3].str();
		}
		if(!tmp.empty())
		  str.second=tmp;
	  }
	  for(const auto & i :defined_dbl){
		r="([\\s\\S]*)%([0-9]+)?\\.?([0-9]+)?([eEf])\\{"+i+"\\}([\\s\\S]*)";
		string tmp;
		for(sregex_iterator itr(str.second.begin(),str.second.end(),r),end_itr;itr!=end_itr;++itr){
		  auto m=*itr;
		  string style=m[4].str();
		  std::size_t width=0,precision=0;
		  if(m[2].matched)
			width=std::stoi(m[2].str());
		  if(m[3].matched)
			precision=std::stoi(m[3].str());
		  ostringstream out;
		  if(style=="e" || style=="E")
			out.flags(std::ios::scientific);
		  else
			out.flags(std::ios::fixed);
		  if(width)
			out.width(width);
		  if(precision)
			out.precision(precision);
		  if(xcalc::macro_dbl.at(i)>0)
			out.fill('+');
		  else if(xcalc::macro_dbl.at(i)<0)
			out.fill('-');
		  else
			out.fill('0');
		  out<<xcalc::macro_dbl.at(i);
		  tmp+=m[1].str()+out.str()+m[5].str();
		}
		if(!tmp.empty())
		  str.second=tmp;
	  }
	}
  }

  //check process strings
  if(!bbstrs.empty()){
	for(auto & str : bbstrs){
	  regex r("%[0-9]*\\.?[0-9]*[deEf]\\{[\\s\\S]*\\}");
	  smatch res;
	  if(regex_search(str.second,res,r))
		check_info+="Warning: format "+res.str()+" is not replaced.\n";
	}
  }

  xcalc::clear();
  bbudef.clear();

#define CHECKINT(scope,variable,default_value,count,size) \
  if(bbints.find(#scope" "#variable" "#count)==bbints.end()){\
	bbints[#scope" "#variable" "#count]=default_value;\
	check_info+="Warning: " #scope " variable <" #variable "(type int, size " #size ")> is not defined, and the default value <" #count ": " #default_value "> is used.\n";\
  }
#define CHECKDBL(scope,variable,default_value,count,size) \
  if(bbdbls.find(#scope" "#variable" "#count)==bbdbls.end()){\
	bbdbls[#scope" "#variable" "#count]=default_value;\
	check_info+="Warning: " #scope " variable <" #variable "(type double, size " #size ")> is not defined, and the default value <" #count ": " #default_value "> is used.\n";\
  }
#define CHECKBOOL(scope,variable,default_value,count,size) \
  if(bbools.find(#scope" "#variable" "#count)==bbools.end()){\
	bbools[#scope" "#variable" "#count]=default_value;\
	check_info+="Warning: " #scope " variable <" #variable "(type bool, size " #size ")> is not defined, and the default value <" #count ": " #default_value "> is used.\n";\
  }
#define FIELD(scope,variable,count) #scope" "#variable" "#count

  //check global
  CHECKINT(global,seed,0,0,1);
  CHECKINT(global,total_turns,10000,0,1);
  CHECKINT(global,output_turns,100,0,1);
  CHECKDBL(global,half_crossing_angle,0.0,0,1);

  //check beamL
  CHECKDBL(beamL,n_particle,1e11,0,1);
  CHECKINT(beamL,n_macro,100000,0,1);
  CHECKDBL(beamL,energy,10e9,0,1);
  CHECKDBL(beamL,mass,0.511e6,0,1);
  CHECKDBL(beamL,charge,-1.0,0,1);
  CHECKINT(beamL,zslice,10,0,1);
  CHECKDBL(beamL,transverse_size,100e-6,0,2);
  CHECKDBL(beamL,transverse_size,10e-6,1,2);
  CHECKDBL(beamL,longitudinal_size,1e-2,0,2);
  CHECKDBL(beamL,longitudinal_size,1e-4,1,2);
  //check beamR
  CHECKDBL(beamR,n_particle,1e11,0,1);
  CHECKINT(beamR,n_macro,100000,0,1);
  CHECKDBL(beamR,energy,10e9,0,1);
  CHECKDBL(beamR,mass,0.511e6,0,1);
  CHECKDBL(beamR,charge,-1.0,0,1);
  CHECKINT(beamR,zslice,10,0,1);
  CHECKDBL(beamR,transverse_size,100e-6,0,2);
  CHECKDBL(beamR,transverse_size,10e-6,1,2);
  CHECKDBL(beamR,longitudinal_size,1e-2,0,2);
  CHECKDBL(beamR,longitudinal_size,1e-4,1,2);

  //check latticeL
  CHECKDBL(latticeL,beta_IP,0.1,0,2);
  CHECKDBL(latticeL,beta_IP,0.01,1,2);
  CHECKDBL(latticeL,alpha_IP,0.0,0,2);
  CHECKDBL(latticeL,alpha_IP,0.0,1,2);
  CHECKDBL(latticeL,eta_IP,0.0,0,2);
  CHECKDBL(latticeL,eta_IP,0.0,1,2);
  CHECKDBL(latticeL,etap_IP,0.0,0,2);
  CHECKDBL(latticeL,etap_IP,0.0,1,2);
  CHECKDBL(latticeL,tune,0.01,0,3);
  CHECKDBL(latticeL,tune,0.01,1,3);
  CHECKDBL(latticeL,tune,0.001,2,3);
  CHECKDBL(latticeL,chromaticity,0.0,0,2);
  CHECKDBL(latticeL,chromaticity,0.0,1,2);
  CHECKBOOL(latticeL,including_damping_excitation,false,0,2);
  CHECKBOOL(latticeL,including_damping_excitation,true,1,2);
  if(bbools.at(FIELD(latticeL,including_damping_excitation,0))){
	CHECKDBL(latticeL,damping_turns,4000.0,0,3);
	CHECKDBL(latticeL,damping_turns,4000.0,1,3);
	CHECKDBL(latticeL,damping_turns,2000.0,2,3);
  }
  if(bbools.at(FIELD(latticeL,including_damping_excitation,1))){
	CHECKDBL(latticeL,equibrium_emittance,0.0,0,2);
	CHECKDBL(latticeL,equibrium_emittance,0.0,1,2);
	CHECKDBL(latticeL,longitudinal_size,0.0,0,2);
	CHECKDBL(latticeL,longitudinal_size,0.0,1,2);
  }
  //check latticeR
  CHECKDBL(latticeR,beta_IP,0.1,0,2);
  CHECKDBL(latticeR,beta_IP,0.01,1,2);
  CHECKDBL(latticeR,alpha_IP,0.0,0,2);
  CHECKDBL(latticeR,alpha_IP,0.0,1,2);
  CHECKDBL(latticeR,eta_IP,0.0,0,2);
  CHECKDBL(latticeR,eta_IP,0.0,1,2);
  CHECKDBL(latticeR,etap_IP,0.0,0,2);
  CHECKDBL(latticeR,etap_IP,0.0,1,2);
  CHECKDBL(latticeR,tune,0.01,0,3);
  CHECKDBL(latticeR,tune,0.01,1,3);
  CHECKDBL(latticeR,tune,0.001,2,3);
  CHECKDBL(latticeR,chromaticity,0.0,0,2);
  CHECKDBL(latticeR,chromaticity,0.0,1,2);
  CHECKBOOL(latticeR,including_damping_excitation,true,0,2);
  CHECKBOOL(latticeR,including_damping_excitation,true,1,2);
  if(bbools.at(FIELD(latticeR,including_damping_excitation,0))){
	CHECKDBL(latticeR,damping_turns,4000.0,0,3);
	CHECKDBL(latticeR,damping_turns,4000.0,1,3);
	CHECKDBL(latticeR,damping_turns,2000.0,2,3);
  }
  if(bbools.at(FIELD(latticeR,including_damping_excitation,1))){
	CHECKDBL(latticeR,equibrium_emittance,0.0,0,2);
	CHECKDBL(latticeR,equibrium_emittance,0.0,1,2);
	CHECKDBL(latticeR,longitudinal_size,0.0,0,2);
	CHECKDBL(latticeR,longitudinal_size,0.0,1,2);
  }

  //check ccbL
  CHECKDBL(ccbL,fundamental_frequency,100.0e6,0,1);
  CHECKDBL(ccbL,deviation,1.0,0,1);
  CHECKINT(ccbL,higher_harmonic,-1,0,1);
  CHECKDBL(ccbL,betax,800.0,0,1);
  CHECKDBL(ccbL,alphax,0.0,0,1);
  CHECKDBL(ccbL,etax,0.0,0,1);
  CHECKDBL(ccbL,etapx,0.0,0,1);
  CHECKDBL(ccbL,phasex,-M_PI/2.0,0,1);
  //check ccaL
  CHECKDBL(ccaL,fundamental_frequency,100.0e6,0,1);
  CHECKDBL(ccaL,deviation,1.0,0,1);
  CHECKINT(ccaL,higher_harmonic,-1,0,1);
  CHECKDBL(ccaL,betax,800.0,0,1);
  CHECKDBL(ccaL,alphax,0.0,0,1);
  CHECKDBL(ccaL,etax,0.0,0,1);
  CHECKDBL(ccaL,etapx,0.0,0,1);
  CHECKDBL(ccaL,phasex,M_PI/2.0,0,1);
  //check ccbR
  CHECKDBL(ccbR,fundamental_frequency,100.0e6,0,1);
  CHECKDBL(ccbR,deviation,1.0,0,1);
  CHECKINT(ccbR,higher_harmonic,-1,0,1);
  CHECKDBL(ccbR,betax,800.0,0,1);
  CHECKDBL(ccbR,alphax,0.0,0,1);
  CHECKDBL(ccbR,etax,0.0,0,1);
  CHECKDBL(ccbR,etapx,0.0,0,1);
  CHECKDBL(ccbR,phasex,-M_PI/2.0,0,1);
  //check ccaR
  CHECKDBL(ccaR,fundamental_frequency,100.0e6,0,1);
  CHECKDBL(ccaR,deviation,1.0,0,1);
  CHECKINT(ccaR,higher_harmonic,-1,0,1);
  CHECKDBL(ccaR,betax,800.0,0,1);
  CHECKDBL(ccaR,alphax,0.0,0,1);
  CHECKDBL(ccaR,etax,0.0,0,1);
  CHECKDBL(ccaR,etapx,0.0,0,1);
  CHECKDBL(ccaR,phasex,M_PI/2.0,0,1);

#undef CHECKINT
#undef CHECKDBL
#undef CHECKBOOL
#undef FIELD

  return check_info;
}
