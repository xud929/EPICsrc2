#include<iostream>
#include<map>
#include<string>

using std::cin;using std::cout;using std::cerr;using std::endl;
using std::map;using std::string;

namespace xcalc{
  extern map<string, int> macro_type;
  extern map<string, int> macro_int;
  extern map<string, double> macro_dbl;
  double xparse(char*,double);
  int xparse(char*,int);
  void xparse(char*);
}


int main(int argc, char *argv[]){
  xcalc::xparse(argv[1]);
  for(const auto & i : xcalc::macro_int)
	cout<<i.first<<"\t"<<i.second<<endl;
  cout<<"\n"<<endl;
  for(const auto & i : xcalc::macro_dbl)
	cout<<i.first<<"\t"<<i.second<<endl;
  cout<<"%s"<<endl;
  return 0;
}
