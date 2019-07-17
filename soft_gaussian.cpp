#include <iostream>
#include"beam.h"
#include<random>
#include<mpi.h>
#include<cmath>
#include<fstream>
#include<cassert>
#include<string>
#include<map>
#include<xdr/xprint.h>

using namespace std;
using std::ofstream;
using std::string;
using std::map;

extern std::map<std::string,int> bbints;
extern std::map<std::string,double> bbdbls;
extern std::map<std::string,bool> bbools;
extern std::map<std::string,std::string> bbstrs;
string check_input(char *, char *);

const char* xdr::flags::delimiter=", ";


int main(int argc, char *argv[]) {
  MPI_Init(&argc, &argv);

  int rank,size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  assert(size>=3);

  string check_info;
  if(argc==1){
	if(rank==0)
	  cout<<"Hello world!"<<endl;
	return 0;
  }else if(argc==2)
	check_info=check_input(argv[1],nullptr);
  else
	check_info=check_input(argv[1],argv[2]);

  if(!check_info.empty()){
	if(rank==0)
	  cerr<<check_info<<endl;
	return 1;
  }

#define FIELD(scope,variable,count) #scope" "#variable" "#count
#define INT0(scope,variable) bbints.at(FIELD(scope,variable,0))
#define INT1(scope,variable) bbints.at(FIELD(scope,variable,1))
#define INT2(scope,variable) bbints.at(FIELD(scope,variable,2))
#define DBL0(scope,variable) bbdbls.at(FIELD(scope,variable,0))
#define DBL1(scope,variable) bbdbls.at(FIELD(scope,variable,1))
#define DBL2(scope,variable) bbdbls.at(FIELD(scope,variable,2))
#define BOL0(scope,variable) bbools.at(FIELD(scope,variable,0))
#define BOL1(scope,variable) bbools.at(FIELD(scope,variable,1))
#define BOL2(scope,variable) bbools.at(FIELD(scope,variable,2))
#define STR0(scope,variable) bbstrs.at(FIELD(scope,variable,0))
#define STR1(scope,variable) bbstrs.at(FIELD(scope,variable,1))
#define STR2(scope,variable) bbstrs.at(FIELD(scope,variable,2))

  /*************************************************************************************************************************************/
  //global setup
  std::mt19937 mt;
  int seed=INT0(global,seed);
  if(seed==0){
  	std::random_device rand_seed;
	mt.seed(rand_seed());
  }else
	mt.seed(seed+rank);
  double half_crossing_angle=DBL0(global,half_crossing_angle);
  int total_turns=INT0(global,total_turns);
  int output_turns=INT0(global,output_turns);
  string output_luminosity=STR0(global,output_luminosity);


  /*************************************************************************************************************************************/
  //beam setup
  beam beamL, beamR;

  beamL.SetN(DBL0(beamL,n_particle),INT0(beamL,n_macro));
  beamL.SetSize(DBL0(beamL,transverse_size),DBL1(beamL,transverse_size),DBL0(beamL,longitudinal_size),DBL1(beamL,longitudinal_size));
  beamL.SetEnergy(DBL0(beamL,energy),DBL0(beamL,mass),DBL0(beamL,charge));
  beamL.SetSlice(INT0(beamL,zslice));
  string output_momentsL=STR0(beamL,output_moments);

  beamR.SetN(DBL0(beamR,n_particle),INT0(beamR,n_macro));
  beamR.SetSize(DBL0(beamR,transverse_size),DBL1(beamR,transverse_size),DBL0(beamR,longitudinal_size),DBL1(beamR,longitudinal_size));
  beamR.SetEnergy(DBL0(beamR,energy),DBL0(beamR,mass),DBL0(beamR,charge));
  beamR.SetSlice(INT0(beamR,zslice));
  string output_momentsR=STR0(beamR,output_moments);

  /*************************************************************************************************************************************/
  //lattice setup
  COneTurnMap mapxL(DBL0(latticeL,beta_IP),DBL0(latticeL,alpha_IP),DBL0(latticeL,tune),DBL0(latticeL,chromaticity));
  COneTurnMap mapyL(DBL1(latticeL,beta_IP),DBL1(latticeL,alpha_IP),DBL1(latticeL,tune),DBL1(latticeL,chromaticity));
  Crf rfL(DBL2(latticeL,tune));

  COneTurnMap mapxR(DBL0(latticeR,beta_IP),DBL0(latticeR,alpha_IP),DBL0(latticeR,tune),DBL0(latticeR,chromaticity));
  COneTurnMap mapyR(DBL1(latticeR,beta_IP),DBL1(latticeR,alpha_IP),DBL1(latticeR,tune),DBL1(latticeR,chromaticity));
  Crf rfR(DBL2(latticeR,tune));

  lattice_radiation_property radL,radR;

  bool isDampingL=BOL0(latticeL,including_damping_excitation), isExcitationL=BOL1(latticeL,including_damping_excitation);
  if(isDampingL)
	radL.SetDamping(DBL0(latticeL,damping_turns),DBL1(latticeL,damping_turns),DBL2(latticeL,damping_turns));
  if(isExcitationL){
	radL.SetLongitudinal(DBL0(latticeL,longitudinal_size),DBL1(latticeL,longitudinal_size));
	radL.SetEmit(DBL0(latticeL,equibrium_emittance),DBL1(latticeL,equibrium_emittance));
	radL.SetTransverse(mapxL,mapyL);
  }
  radL.SetFlags(isDampingL,isExcitationL);

  bool isDampingR=BOL0(latticeR,including_damping_excitation), isExcitationR=BOL1(latticeR,including_damping_excitation);
  if(isDampingR)
	radR.SetDamping(DBL0(latticeR,damping_turns),DBL1(latticeR,damping_turns),DBL2(latticeR,damping_turns));
  if(isExcitationR){
	radR.SetLongitudinal(DBL0(latticeR,longitudinal_size),DBL1(latticeR,longitudinal_size));
	radR.SetEmit(DBL0(latticeR,equibrium_emittance),DBL1(latticeR,equibrium_emittance));
	radR.SetTransverse(mapxR,mapyR);
  }
  radR.SetFlags(isDampingR,isExcitationR);

  /*************************************************************************************************************************************/
  //crab cavity setup
  CCrabCavity ccbL(1,DBL0(ccbL,fundamental_frequency),-half_crossing_angle,DBL0(ccbL,deviation),INT0(ccbL,higher_harmonic));
  ccbL.set_optics(DBL0(ccbL,betax),DBL0(ccbL,alphax),DBL0(ccbL,etax),DBL0(ccbL,etapx),DBL0(ccbL,phasex),
	  DBL0(latticeL,beta_IP),DBL0(latticeL,alpha_IP),DBL0(latticeL,eta_IP),DBL0(latticeL,etap_IP));

  CCrabCavity ccaL(-1,DBL0(ccaL,fundamental_frequency),-half_crossing_angle,DBL0(ccaL,deviation),INT0(ccaL,higher_harmonic));
  ccaL.set_optics(DBL0(ccaL,betax),DBL0(ccaL,alphax),DBL0(ccaL,etax),DBL0(ccaL,etapx),DBL0(ccaL,phasex),
	  DBL0(latticeL,beta_IP),DBL0(latticeL,alpha_IP),DBL0(latticeL,eta_IP),DBL0(latticeL,etap_IP));

  CCrabCavity ccbR(1,DBL0(ccbR,fundamental_frequency),-half_crossing_angle,DBL0(ccbR,deviation),INT0(ccbR,higher_harmonic));
  ccbR.set_optics(DBL0(ccbR,betax),DBL0(ccbR,alphax),DBL0(ccbR,etax),DBL0(ccbR,etapx),DBL0(ccbR,phasex),
	  DBL0(latticeR,beta_IP),DBL0(latticeR,alpha_IP),DBL0(latticeR,eta_IP),DBL0(latticeR,etap_IP));

  CCrabCavity ccaR(-1,DBL0(ccaR,fundamental_frequency),-half_crossing_angle,DBL0(ccaR,deviation),INT0(ccaR,higher_harmonic));
  ccaR.set_optics(DBL0(ccaR,betax),DBL0(ccaR,alphax),DBL0(ccaR,etax),DBL0(ccaR,etapx),DBL0(ccaR,phasex),
	  DBL0(latticeR,beta_IP),DBL0(latticeR,alpha_IP),DBL0(latticeR,eta_IP),DBL0(latticeR,etap_IP));
  /*************************************************************************************************************************************/
  // generate initial distribution 
  beamL.Ini6D(mapxL, mapyL, rfL, mt);
  beamR.Ini6D(mapxR, mapyR, rfR, mt);

  /*************************************************************************************************************************************/
  // track 
  int index=0,current=0;
  vector<double> luminosity(total_turns);
  vector<vector<double>> momentsL(total_turns), momentsR(total_turns);
  ofstream out;
  switch(rank){
	case 0:
	  out.open(output_luminosity);
	  break;
	case 1:
	  out.open(output_momentsL);
	  break;
	case 2:
	  out.open(output_momentsR);
	  break;
	default:
	  break;
  }

  //double t0=MPI_Wtime();
  while(index!=total_turns){
	if(half_crossing_angle){
	  beamL.crab_kick(ccbL);
	  beamL.LorentzTransform(half_crossing_angle, 1);
	  beamR.crab_kick(ccbR);
	  beamR.LorentzTransform(half_crossing_angle, 1);
	}

	beamL.findzBoundary();
	beamR.findzBoundary();

	beamL.set_longitudinal_slices2();
	beamR.set_longitudinal_slices2();

	luminosity[index]=beambeam_pass2(beamL,beamR);

	if(half_crossing_angle){
	  beamL.LorentzTransform(half_crossing_angle, -1);
	  beamL.crab_kick(ccaL);
	  beamR.LorentzTransform(half_crossing_angle, -1);
	  beamR.crab_kick(ccaR);
	}

	beamL.OneTurn2(mapxL, mapyL, rfL, radL, mt);
	beamR.OneTurn2(mapxR, mapyR, rfR, radR, mt);

	momentsL[index]=beamL.CalMoments();
	momentsR[index]=beamR.CalMoments();

	if((index+1)%output_turns==0){
	  for(int line=0;line<output_turns;++line){
		auto lineno=current+line;
		switch(rank){
		  case 0:
			out<<lineno+1<<"\t"<<luminosity[lineno]<<"\n";
			break;
		  case 1:
			out<<lineno+1<<"\t";
			for(const auto & i : momentsL[lineno])
			  out<<i<<"\t";
			out<<"\n";
			break;
		  case 2:
			out<<lineno+1<<"\t";
			for(const auto & i : momentsR[lineno])
			  out<<i<<"\t";
			out<<"\n";
			break;
		  default:
			break;
		}
	  }
	  out<<std::flush;
	  current+=output_turns;
	}

	++index;
	MPI_Barrier(MPI_COMM_WORLD);
  }

  if(rank<3)
	out.close();
 
  //beamL.print_coord("sg.pro");
  //beamR.print_coord("sg.ele");

  MPI_Finalize();
  return 0;
}
