/** $Id$ **/
#ifndef _INC_FLCoreClasses
#define _INC_FLCoreClasses

#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define const_nan 0.0

#ifdef WIN32
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

#define  __min(x,y) (x<y?x:y)
#define  __max(x,y) (x<y?y:x)
extern int             outofbounds_int;
extern bool            outofbounds_bool;
extern double          outofbounds_double;

int NElemList(SEXP);

#define const_nan 0.0

typedef enum tagFLRConstSRR 
	{
   FLRConst_Mean            = 1,
   FLRConst_SRR_geomean     = 1,
   FLRConst_BevHolt         = 2,
   FLRConst_SRR_bevholt     = 2,
   FLRConst_Ricker          = 3,
   FLRConst_SRR_ricker      = 3,
   FLRConst_SegReg          = 4,
   FLRConst_SRR_segreg      = 4,      
   FLRConst_SRR_shepherd    = 5,
   FLRConst_SRR_bevholt_d   = 21,
   FLRConst_SRR_bevholt_c_a = 22,
   FLRConst_SRR_bevholt_c_b = 23,
   FLRConst_SRR_bevholt_sv  = 24,
   FLRConst_SRR_bevholt_ndc = 25,
   FLRConst_SRR_bevholt_ar1 = 26,
   FLRConst_SRR_ricker_d    = 31,
   FLRConst_SRR_ricker_c_a  = 32,
   FLRConst_SRR_ricker_c_b  = 33,
   FLRConst_SRR_ricker_sv   = 34,
   FLRConst_SRR_ricker_ndc  = 35,
   FLRConst_SRR_ricker_ar1  = 36
   } FLRConstSRR;

typedef enum tagFLRConst_Target 
	{
   FLRConst_SSB        = 1,
   FLRConst_Biomass    = 2,
   FLRConst_Catch      = 3,
   FLRConst_Landings   = 4,
   FLRConst_Discards   = 5,
   FLRConst_F          = 6,
   FLRConst_Z          = 7,
   FLRConst_FLandings  = 8,
   FLRConst_FDiscards  = 9,
   FLRConst_Effort     = 10,
   FLRConst_Costs      = 11,
   FLRConst_Revenue    = 12,
   FLRConst_Profit     = 13,
   FLRConst_MnSz       = 14,
   FLRConst_None       = 0
	} FLRConst_Target;

bool isFLInteger(SEXP);
bool isFLLogical(SEXP);
bool isFLDouble(SEXP);
bool isFLQuant(SEXP);
bool isFLQuants(SEXP);
bool isFLBiol(SEXP);
bool isFLFleet(SEXP);
bool isFLCatch(SEXP);
bool isFLStock(SEXP);
bool isFLBiols(SEXP);
bool isFLFleets(SEXP);
bool isFLStocks(SEXP);
bool isFLCatches(SEXP);
bool isFLMetiers(SEXP);
bool isFLPar(SEXP);

SEXP CreateRange(int,int,int,int,int);
SEXP CreateRange(int,int,int,int,int,int,int);

SEXP ReturnDouble(double); 

const char *strlwr(const char *);

FLRConstSRR getSRType(SEXP);

class FLVector
{
public:        

   FLVector(void);      
   FLVector(SEXP);
  ~FLVector(void);      

   void Init(SEXP);
   void Init(int, int, double val=0.0);      
 
   SEXP Return(void);      
   double& operator () (int _i);
   int mindim, maxdim;

protected: 
   bool InitFlag;

   double *data;   

   void alloc(void);      
   void unalloc(void);         
   };                  


class FLBool
   {
public:        
   FLBool(void);      
   FLBool(SEXP);
  ~FLBool(void);      

   void Init(SEXP);      
   SEXP Return(void);      
   bool& operator () (int _i);
   int mindim, maxdim;

protected: 
   bool InitFlag;
   bool *data;   
   void alloc(void);      
   void unalloc(void);      
   };                  

class FL2D
{
public:        
   FL2D(void);      
   FL2D(SEXP);
  ~FL2D(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   int mindim(int);
   int maxdim(int);

   double& operator () (int _i, int _j);

protected: 
   bool InitFlag;

   int min1, max1, min2, max2;

   double **data;   

   void alloc(void);      
   void unalloc(void);      
   };                  

class FL2DRagged
{
public:        
   FL2DRagged(void);      
  ~FL2DRagged(void);      

   void Init(int);
   void Init(int, SEXP);

   SEXP Return(int);      

   int mindim(int);
   int maxdim(int);

   double& operator () (int _i, int _j);

protected: 
   bool *InitFlag;

   int  n;
   int *_mindim, *_maxdim;

   double **data;   

   void alloc(int);         
   void unalloc(int);         
   void unalloc(void);         
   };                  

class FLQuant
{
public:        
   FLQuant(void);      
   FLQuant(SEXP);
   FLQuant(int, int, int, int, int, int, int, int, double);
  ~FLQuant(void);      

   void alloc(void);      
   void Init(SEXP);      
   void Init(int, int, int, int, int, int, int, int, double val=0.0);     
   SEXP Return(void);      

   double& operator () (int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   bool &InitFlag();

   int &minquant(), &maxquant(), &plusgrp(),
       &minyr(),    &maxyr(),
       &nunits(),
       &nseasons(),
       &nareas(),
       &niters();

protected: 
   bool flq_InitFlag;

   int flq_minquant, flq_maxquant, flq_plusgrp,
       flq_minyr,    flq_maxyr,
       flq_nunits,
       flq_nseasons,
       flq_nareas,
       flq_niters;

   double ******data;   

   void unalloc(void);         
   };                  

class FLQuant2
{
public:        
   FLQuant2(void);      
   FLQuant2(SEXP);
   FLQuant2(int);
   FLQuant2(int, int, int, int, int, int, int, int, int, double);
  ~FLQuant2(void);      

   void alloc_n7(int);      
   void alloc(int);      

   void Init(int, SEXP);      
   void Init(int, int, int, int, int, int, int, int, int, double val=0.0);     

   SEXP Return(int);      
   SEXP Return(void);      

   double& operator () (int n7, int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   bool &InitFlag(int);

   int &minquant(int), &maxquant(int),
       &minyr(int),    &maxyr(int),
       &nunits(int),
       &nseasons(int),
       &nareas(int),
       &niters(int),
       &n(void);

protected: 
   bool *flq_InitFlag;

   int flq_n,
       *flq_minquant, *flq_maxquant,
       *flq_minyr,    *flq_maxyr,
       *flq_nunits,
       *flq_nseasons,
       *flq_nareas,
       *flq_niters;

   double *******data;   

   void unalloc_n7(void);         
   void unalloc(int);         
   void unalloc(void);         
   }; 

class FLQuant3
{
public:        
   FLQuant3(void);      
   FLQuant3(int, int);
   FLQuant3(int, int, int, int, int, int, int, int, int, int, double);

  ~FLQuant3(void);      

   void alloc_n8(int);      
   void alloc_n7(int, int);      
   void alloc(int, int);      

   void Init(int, int);      
   void Init(int, int, SEXP);      
   void Init(int, int, int, int, int, int, int, int, int, int, double val=0.0);     

   SEXP Return(int, int);      
   SEXP Return(void);      

   double& operator () (int n7, int n8, int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   bool &InitFlag(int, int);

   int &minquant(int,int), &maxquant(int,int),
       &minyr(int,int),    &maxyr(int,int),
       &nunits(int,int),
       &nseasons(int,int),
       &nareas(int,int),
       &niters(int,int),
       &n7(int),
       &n8();

protected: 
   bool **flq_InitFlag;

   int flq_n8, *flq_n7,
       **flq_minquant, **flq_maxquant,
       **flq_minyr,    **flq_maxyr,
       **flq_nunits,
       **flq_nseasons,
       **flq_nareas,
       **flq_niters;

   double ********data;   

   void unalloc_n8(void);         
   void unalloc_n7(int);
   void unalloc(int,int);         
   void unalloc(void);         
   }; 

class FLQuant4
{
public:        
   FLQuant4(void);
   FLQuant4(int, int, int);
   FLQuant4(int, int, int, int, int, int, int, int, int, int, int, double);

  ~FLQuant4(void);      

   void alloc_n9(int);      
   void alloc_n8(int, int);      
   void alloc_n7(int, int, int);      
   void alloc(int, int, int);      

   void Init(int, int, int, SEXP);      
   void Init(int, int, int, int, int, int, int, int, int, int, int, double val=0.0);     

   SEXP Return(int, int, int);      
   SEXP Return(void);      

   double& operator () (int n9,int n8, int n7, int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   bool &InitFlag(int, int, int);

   int &minquant(int,int,int), &maxquant(int,int,int),
       &minyr(int,int,int),    &maxyr(int,int,int),
       &nunits(int,int,int),
       &nseasons(int,int,int),
       &nareas(int,int,int),
       &niters(int,int,int);

protected: 
   bool ***flq_InitFlag;
   int flq_n9, *flq_n8, **flq_n7,
       ***flq_minquant, ***flq_maxquant,
       ***flq_minyr,    ***flq_maxyr,
       ***flq_nunits,
       ***flq_nseasons,
       ***flq_nareas,
       ***flq_niters;
   double *********data;   
   void unalloc_n9(void);         
   void unalloc_n8(int);         
   void unalloc_n7(int, int);         
   void unalloc(int,int,int);         
   void unalloc(void);         
   }; 

class _FLQuant
{
public:        
   _FLQuant(void);
   _FLQuant(int, int, int);
   _FLQuant(int, int, int, int, int, int, int, int, double, int n7=1,int n8=1, int n9=1);
  ~_FLQuant(void);      

   void alloc_n9(int n9=1);      
   void alloc_n8(int n8=1, int n9=1);      
   void alloc_n7(int n7=1,int n8=1, int n9=1);      
   void alloc(int, int, int);      
   void Init(int, int, int, SEXP);      
   void Init(int, int, int, int, int, int, int, int, double val=0.0, int n7=1,int n8=1, int n9=1);     
   SEXP Return(int, int, int);      
   SEXP Return(void);      
   double& operator () (int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1, int n7=1,int n8=1, int n9=1);

   bool &InitFlag(int, int, int);

   int &minquant(int i7=1, int i8=1, int i9=1), &maxquant(int i7=1, int i8=1, int i9=1),
       &minyr(   int i7=1, int i8=1, int i9=1), &maxyr(int i7=1, int i8=1, int i9=1),
       &nunits(  int i7=1, int i8=1, int i9=1),
       &nseasons(int i7=1, int i8=1, int i9=1),
       &nareas(  int i7=1, int i8=1, int i9=1),
       &niters(  int i7=1, int i8=1, int i9=1);

protected: 
   bool ***flq_InitFlag;

   int flq_n9, *flq_n8, **flq_n7,
       ***flq_minquant, ***flq_maxquant,
       ***flq_minyr,    ***flq_maxyr,
       ***flq_nunits,
       ***flq_nseasons,
       ***flq_nareas,
       ***flq_niters;

   double *********data;   
   void unalloc_n9(void);         
   void unalloc_n8(int i9=1);         
   void unalloc_n7(int i8=1, int i9=1);         
   void unalloc(   int i7=1, int i8=1, int i9=1);         
   }; 

class FLStock 
{
public:        

   FLStock(void);      
   FLStock(SEXP);      
  ~FLStock(void);      

   void Init(SEXP);      
   SEXP Return(void);
   SEXP ReturnRange(void);      

  FLQuant catch_, 
          catch_n, 
          catch_wt, 
          discards, 
          discards_n, 
          discards_wt, 
          landings, 
          landings_n, 
          landings_wt, 
          stock, 
          stock_n, 
          stock_wt, 
          m, 
          mat, 
          harvest, 
          harvest_spwn, 
          m_spwn; 

   int minfbar,  maxfbar,
       minquant, maxquant, plusgrp,
       minyr,    maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

   double Fbar(           int, int, int, int, int);
   double Zbar(           int, int, int, int, int);
   double FbarLandings(   int, int, int, int, int);
   double FbarDiscards(   int, int, int, int, int);
   double SSB(            int, int, int, int, int);
   double computeCatch(   int, int, int, int, int);
   double computeLandings(int, int, int, int, int);
   double computeDiscards(int, int, int, int, int);
   double computeStock(   int, int, int, int, int);
   double computeMnSz(    int, int, int, int, int);

protected: 
   bool InitFlag;
   void unalloc(void);      
   };                  

class FLIndex 
{
public:        
   FLIndex(void);      
   FLIndex(SEXP);      
  ~FLIndex(void);      

   void Init(SEXP);
   SEXP Return(void);
   SEXP ReturnRange(void);

   FLQuant index_var,
           index,
           catch_n,
           catch_wt,
           effort,
           sel_pattern,
           index_q;   

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

   double startf, endf;
   void alloc(void);      
   };                  

class FLBiol 
{
public:        

   FLBiol(void);      
   FLBiol(SEXP);      
  ~FLBiol(void);      

   FLQuant n,
           m,
           wt,
           fec,
           spwn;   

   void Init(SEXP);      
   SEXP Return(void);      

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;
   void alloc(void);      
   };

class FLBiols 
{
public:        
   FLBiols(void);      
   FLBiols(SEXP);      
  ~FLBiols(void);      

   FLQuant2 n,
            m,
            wt,
            fec,
            spwn;   

   void alloc(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   int minquant(int), maxquant(int), plusgrp(int),
       minyr(int),    maxyr(int),
       nunits(int),
       nseasons(int),
       nareas(int),
       niters(int);

protected: 
   bool *InitFlag;

   int nspp,
       *_minquant, *_maxquant, *_plusgrp,
       *_minyr,    *_maxyr,
       *_nunits,
       *_nseasons,
       *_nareas,
       *_niters;
   void unalloc(void);      
   };

class FLCatch 
{
public:        
   FLCatch(void);      
   FLCatch(SEXP);      
  ~FLCatch(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   FLQuant catch_, 
           catch_n, 
           catch_wt, 
           catch_q, 
           catch_sel, 
           landings, 
           landings_n, 
           landings_wt, 
           landings_sel, 
           discards, 
           discards_n, 
           discards_wt, 
           discards_sel, 
           price;

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas;
   };     

class FLMetier 
{
public:        
   FLMetier(void);      
   FLMetier(SEXP);      
  ~FLMetier(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   FLQuant effshare,
           vcost;

   FLQuant2 catch_, 
            catch_n, 
            catch_wt, 
            catch_q, 
            catch_sel, 
            landings, 
            landings_n, 
            landings_wt, 
            landings_sel, 
            discards, 
            discards_n, 
            discards_wt, 
            discards_sel, 
            price;

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       nspp;
   };     

class FLFleet 
{
public:        
   FLFleet(void);      
   FLFleet(SEXP);      
  ~FLFleet(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   FLQuant effort,
           fcost,
           capacity,
           crewshare;

   FLQuant2 effshare,
            vcost;

   FLQuant3 catch_, 
            catch_n, 
            catch_wt, 
            catch_q, 
            catch_sel, 
            landings, 
            landings_n, 
            landings_wt, 
            landings_sel, 
            discards, 
            discards_n, 
            discards_wt, 
            discards_sel, 
            price;

protected: 
   bool InitFlag;
   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters,
       nspp,
       nmetier;
   void alloc(void);      
   void unalloc(void);      
};     

class FLFleets 
{
public:        
   FLFleets(void);      
   FLFleets(SEXP);      
  ~FLFleets(void);      

   bool Init(SEXP);      

   SEXP Return(void);      

   FLQuant2 fcost,
            capacity,
            crewshare;

   FLQuant3 effshare,
            vcost;

   FLQuant4 catch_, 
            catch_n, 
            catch_wt, 
            catch_q, 
            catch_sel, 
            landings, 
            landings_n, 
            landings_wt, 
            landings_sel, 
            discards, 
            discards_n, 
            discards_wt, 
            discards_sel, 
            price;

  int  &minquant(void), &maxquant(void), &plusgrp(void),
       &minyr(void),    &maxyr(void),
       &nunits(void),
       &nseasons(void),
       &nareas(void),
       &niters(void),
       &minquant(int,int,int), &maxquant(int,int,int), &plusgrp(int,int,int),
       &minyr(int,int,int),    &maxyr(int,int,int),
       &nunits(int,int,int),
       &nseasons(int,int,int),
       &nareas(int,int,int),
       &niters(int,int,int);

protected: 
   bool InitFlag;

   int fls_minquant, fls_maxquant, fls_plusgrp,
       fls_minyr,    fls_maxyr,
       fls_nunits,
       fls_nseasons,
       fls_nareas,
       fls_niters,
       ***fl_minquant, ***fl_maxquant, ***fl_plusgrp,
       ***fl_minyr,  ***fl_maxyr,
       ***fl_nunits,
       ***fl_nseasons,
       ***fl_nareas,
       ***fl_niters,
       nspp,
       nmetier,
       nfleet;
   void alloc(void);      
   void unalloc(void);      
};     

class FLStocks 
{
public:        
   FLStocks(void);      
   FLStocks(SEXP);      
  ~FLStocks(void);      

   void Init(SEXP);      
   SEXP Return(void);      

  FLQuant2 catch_, 
           catch_n, 
           catch_wt, 
           discards, 
           discards_n, 
           discards_wt, 
           landings, 
           landings_n, 
           landings_wt, 
           stock, 
           stock_n, 
           stock_wt, 
           m, 
           mat, 
           harvest, 
           harvest_spwn, 
           m_spwn; 

   int minquant(int), maxquant(int), plusgrp(int),
       minfbar(int),  maxfbar(int),
       minyr(int),    maxyr(int),
       nunits(int),
       nseasons(int),
       nareas(int),
       niters(int),
       nspp(void);

   double Fbar(           int, int, int, int, int, int);
   double FbarLandings(   int, int, int, int, int, int);
   double FbarDiscards(   int, int, int, int, int, int);
   double SSB(            int, int, int, int, int, int);
   double computeCatch(   int, int, int, int, int, int);
   double computeLandings(int, int, int, int, int, int);
   double computeDiscards(int, int, int, int, int, int);
   double computeStock(   int, int, int, int, int, int);
   double computeMnSz(    int, int, int, int, int, int);

protected: 
   bool *InitFlag;
   int _nspp,
       *_minquant, *_maxquant, *_plusgrp,
       *_minfbar,  *_maxfbar,
       *_minyr,    *_maxyr,
       *_nunits,
       *_nseasons,
       *_nareas,
       *_niters;

   void unalloc(void);      
   };                  

class FLIndices 
{
public:        
   FLIndices(void);      
   FLIndices(SEXP);      
  ~FLIndices(void);      

   void Init(SEXP);
   SEXP ReturnRange(int);
   SEXP Return(void);      

   FLQuant2 catch_n,     
            catch_wt,       
            effort,  
            sel_pattern,      
            index_q,            
            index,    
            index_var;   

   int minquant(int), maxquant(int), plusgrp(int),
       minyr(int),    maxyr(int),
       nunits(int),
       nseasons(int),
       nareas(int),
       niters(int),
       nindices();

   double startf(int), endf(int);

protected: 
   bool *InitFlag;
   int nidx,
       *_minquant, *_maxquant, *_plusgrp,
       *_minyr,    *_maxyr,
       *_nunits,
       *_nseasons,
       *_nareas,
       *_niters;
   double *_startf, *_endf;
   void unalloc(void);      
   };                  

#endif /* _INC_FLCoreClasses */
