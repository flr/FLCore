#include "FLCoreClasses.hpp"

int             outofbounds_int;
bool            outofbounds_bool;
double          outofbounds_double;

const char *strlwr(const char *s)
  {
   if (s != NULL)
      {
      char *p;

      for (p = const_cast<char*> (s); *p; ++p)
         *p = tolower(*p);
      }

  return s;
  }

FLRConstSRR getSRType(SEXP v)      
   {
   if (!isVector(v) || !isString(v))
      return FLRConst_Mean;;

    SEXP dims     = GET_DIM(v),
         dimnames = GET_DIMNAMES(v);

    short n = length(dims);

    if (n != 1)
       return FLRConst_Mean;;

    short max = INTEGER(dims)[0];

    const char *t = "";

    for (short i = 0; i<max; i++)
      {
      t = strlwr((char *)CHAR(STRING_ELT(v, i)));
   
      if       (strcmp(t, "m")==1)
         return FLRConst_Mean;
      else  if (strcmp(t, "b")==1)
         return FLRConst_BevHolt;
      else  if (strcmp(t, "r")==1)
         return FLRConst_Ricker;
      else  if (strcmp(t, "s")==1)
         return FLRConst_SegReg;
      }
 
    return FLRConst_Mean;   
    }
  
void InputAgeRange(SEXP obj, int *MinAge, int *MaxAge)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a  = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<n; i++)
      {
      const char *s = CHAR(STRING_ELT(names, i));

      if (      strcmp(s, "min")==0)
         *MinAge     = (int)((a)[i]);
      else  if (strcmp(s, "max")==0)
         *MaxAge     = (int)((a)[i]);
      }

  UNPROTECT(1);
  }

int NElemList(SEXP x)
   {
   //Check that it is a list
   if (!IS_LIST(x) || TYPEOF(x) != VECSXP) 
      return 0;
   else
      return length(x);
  }

bool isBool(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "logical")==0;
   }

bool isFLQuant(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLQuant")==0;
   }

bool isFLQuants(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLQuants")==0;
   }

bool isFLBiol(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLBiol")==0;
   }

bool isFLBiols(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLBiols")==0;
   }

bool isFLFleet(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLFleet")==0;
   }

bool isFLStock(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLStock")==0;
   }

bool isFLIndex(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndex")==0;
   }

bool isFLIndices(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndices")==0;
   }

bool isFLFleets(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLFleets")==0;
   }

bool isFLCatch(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLCatch")==0;
   }

bool isFLCatches(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLCatches")==0;
   }

bool isFLMetiers(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLMetiers")==0;
   }

bool isFLStocks(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLStocks")==0;
   }

bool isFLXSA(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLXSA")==0;
   }

bool isFLBRP(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLBRP")==0;
   }

bool isFLPar(SEXP t)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLPar")==0;
   }

FLQuant::FLQuant(void)      
    {
    InitFlag() = false;
    }

double& FLQuant::operator()(int _age,int _yr,int _unit, int _season, int _area, int _iter) 
   { 
   if (!InitFlag()      || 
       _age <minquant() || _age >maxquant()   || 
       _yr  <minyr()    || _yr  >maxyr()      || 
       _unit<1          || _unit>nunits()     || 
       _season<1        || _season>nseasons() || 
       _area<1          || _area>nareas()) 
      return outofbounds_double;
   else
      {
      _iter = __min(_iter,niters());
      return (data)[_age][_yr][_unit][_season][_area][_iter];
      }
   } 

FLQuant::FLQuant(SEXP x)  
    {
    InitFlag() = false;

    if (isFLQuant(x))
       Init(x);
    }

FLQuant::FLQuant(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)
   {
   InitFlag() = false;

   Init(_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val);   
   }

void FLQuant::Init(SEXP x)      
    {
    if (InitFlag()) unalloc();

    SEXP Quant    = GET_SLOT(x, install(".Data")),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

    int dim[6], n = length(dims);

    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
    if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
        ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
      return;

    minquant() = 0;
    minyr()    = 0;
    maxquant() = (int)dim[0] -1;
    maxyr()    = (int)dim[1] -1;
    nunits()   = (int)dim[2];
    nseasons() = (int)dim[3];
    nareas()   = (int)dim[4]; 
    niters()   = (int)dim[5];
	   
      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int  t = 0;
         const char *c;
         
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
            {
            c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 

            minquant() += t;
            maxquant() += t;
  	         }
		   
         if (n >= 2 && INTEGER(dims)[1] >= 1) 
            {
            t = 0;
            c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 
            
            minyr()   += t;
            maxyr()   += t;
 	      	}
		   }


   alloc();

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(); iIter++)
	   for (iArea = 1; iArea <= nareas(); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(); iUnit++)
		         for (iYear = minyr(); iYear <= maxyr(); iYear++)
			   		   for (iAge = minquant(); iAge <= maxquant(); iAge++)
			       			   data[iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
    }

void FLQuant::Init(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)      
    {
    if (InitFlag()) unalloc();

  	 int dim[6];

    dim[0] = _maxquant-_minquant+1;
    dim[1] = _maxyr-_minyr+1;
    dim[2] = _nunits;
    dim[3] = _nseasons;
    dim[4] = _nareas;
    dim[5] = _niters;

    if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
        ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
      return;

   minquant() = _minquant;
   maxquant() = _maxquant;
   minyr()    = _minyr;
   maxyr()    = _maxyr;
   nunits()   = _nunits;
   nareas()   = _nareas;
   nseasons() = _nseasons;
   niters()   = _niters;

   alloc();

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(); iIter++)
	   for (iArea = 1; iArea <= nareas(); iArea++)
	    	   for (iSeason = 1; iSeason <= nseasons(); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(); iUnit++)
		      		   for (iYear = minyr(); iYear <= maxyr(); iYear++)
			   		   for (iAge = minquant(); iAge <= maxquant(); iAge++)
			       			   data[iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

//Only return 5 dim array 
SEXP FLQuant::Return(void)      
    {
    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant()-minquant() +1;
    INTEGER(dim)[1] = maxyr()   -minyr()    +1;
    INTEGER(dim)[2] = nunits(); 
    INTEGER(dim)[3] = nseasons(); 
    INTEGER(dim)[4] = nareas();
    INTEGER(dim)[5] = niters();

   //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant()-minquant() +1));
    for (iAge=minquant(),i=0; iAge<=maxquant(); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr()-minyr()+1));
    for (iYear=minyr(), i=0; iYear<=maxyr(); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits()==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits()));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits()));
       for (iUnit=1, i=0; iUnit<=nunits(); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons()==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons()));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons()));
       for (iSeason=1, i=0; iSeason<=nseasons(); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas()==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas()));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas()));
       for (iArea=1, i=0; iArea<=nareas(); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters()));
    for (iIter=1, i=0; iIter<=niters(); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 

    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(); iIter++)
	    for (iArea = 1; iArea <= nareas(); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(); iUnit++)
	    		    for (iYear = minyr(); iYear <= maxyr(); iYear++)
			 		    for (iAge = minquant(); iAge <= maxquant(); iAge++)
			      			    REAL(v)[i++] = data[iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

void FLQuant::alloc(void)      
   {
   if (InitFlag()) unalloc();

   int i = 0,j = 0, k = 0, l = 0, m = 0;
   data = new double*****[maxquant()-minquant()+1] - minquant();
   for(i=minquant();i<=maxquant();i++) 
      {
      (data)[i]  = new double****[maxyr()-minyr()+1] - minyr();
      for (j=minyr(); j<=maxyr(); j++)
         {
         (data)[i][j]  = new double***[nunits()] - 1;
         for (k = 1; k<=nunits(); k++)
            {
            (data)[i][j][k]  = new double**[nseasons()] - 1;
            for (l=1; l<=nseasons(); l++)
	             {
                (data)[i][j][k][l]  = new double*[nareas()] - 1;
	             for (m=1; m<=nareas(); m++)
      		       (data)[i][j][k][l][m] = new double[niters()] - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag()=true;
   }

void FLQuant::unalloc(void)      
   {
   if (!InitFlag()) return;

   for (int iAge  = minquant(); iAge <= maxquant(); iAge++)
      {
      for (int iYear  = minyr(); iYear  <= maxyr();   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits();  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(); iSeason++) 
         		{
		         for (int iArea = 1; iArea <= nareas(); iArea++) 
                   delete [] (data[iAge][iYear][iUnit][iSeason][iArea]+1);
     
            	delete [] (data[iAge][iYear][iUnit][iSeason]+1);
            	}
	         delete [] (data[iAge][iYear][iUnit]+1);
	         }
         delete [] (data[iAge][iYear]+1);
         }
      delete [] (data[iAge]+minyr());
      }
   delete [] (data+minquant());
   
   InitFlag()=false;
   }                               


FLQuant::~FLQuant(void)      
   {
   unalloc();
   }                               

int& FLQuant::minquant()
   {
   return flq_minquant;
   }

int& FLQuant::maxquant()
   {
   return flq_maxquant;
   }

int& FLQuant::plusgrp()
   {
   return flq_plusgrp;
   }

int& FLQuant::minyr()
   {
   return flq_minyr;
   }

int& FLQuant::maxyr()
   {
   return flq_maxyr;
   }

int& FLQuant::nunits()
   {
   return flq_nunits;
   }

int& FLQuant::nseasons()
   {
   return flq_nseasons;
   }

int& FLQuant::nareas()
   {
   return flq_nareas;
   }
 
int& FLQuant::niters()
   {
   return flq_niters;
   }

bool& FLQuant::InitFlag()
   {
   return flq_InitFlag;
   }

FLStock::FLStock(void)      
    {
    InitFlag = false;
    }

FLStock::FLStock(SEXP x)      
    {
    InitFlag = false;

    if (isFLStock(x) && !InitFlag)
       Init(x);
    }
   
void FLStock::Init(SEXP x)
   {
   minquant = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp  = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr    = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr    = (int)REAL(GET_SLOT(x, install("range")))[4];
 
   minfbar  = minquant; 
   maxfbar  = maxquant; 
   
   if (LENGTH(GET_SLOT(x, install("range"))) >= 6)
      minfbar  = (int)REAL(GET_SLOT(x, install("range")))[5];
   if (LENGTH(GET_SLOT(x, install("range"))) >= 7)
      maxfbar  = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   catch_.Init(      GET_SLOT(x, install("catch"))); 
   catch_n.Init(     GET_SLOT(x, install("catch.n"))); 
   catch_wt.Init(    GET_SLOT(x, install("catch.wt"))); 
   discards.Init(    GET_SLOT(x, install("discards"))); 
   discards_n.Init(  GET_SLOT(x, install("discards.n"))); 
   discards_wt.Init( GET_SLOT(x, install("discards.wt"))); 
   landings.Init(    GET_SLOT(x, install("landings"))); 
   landings_n.Init(  GET_SLOT(x, install("landings.n"))); 
   landings_wt.Init( GET_SLOT(x, install("landings.wt"))); 
   stock.Init(       GET_SLOT(x, install("stock"))); 
   stock_n.Init(     GET_SLOT(x, install("stock.n"))); 
   stock_wt.Init(    GET_SLOT(x, install("stock.wt"))); 
   m.Init(           GET_SLOT(x, install("m"))); 
   mat.Init(         GET_SLOT(x, install("mat"))); 
   harvest.Init(     GET_SLOT(x, install("harvest"))); 
   harvest_spwn.Init(GET_SLOT(x, install("harvest.spwn"))); 
   m_spwn.Init(      GET_SLOT(x, install("m.spwn"))); 

   niters= __max(niters,catch_.niters());         
   niters= __max(niters,catch_n.niters());       
   niters= __max(niters,catch_wt.niters());      
   niters= __max(niters,discards.niters());      
   niters= __max(niters,discards_n.niters());    
   niters= __max(niters,discards_wt.niters());   
   niters= __max(niters,landings.niters());      
   niters= __max(niters,landings_n.niters());    
   niters= __max(niters,landings_wt.niters());   
   niters= __max(niters,stock.niters());         
   niters= __max(niters,stock_n.niters());       
   niters= __max(niters,stock_wt.niters());      
   niters= __max(niters,m.niters());             
   niters= __max(niters,mat.niters());           
   niters= __max(niters,harvest.niters());       
   niters= __max(niters,harvest_spwn.niters()); 
   niters= __max(niters,m_spwn.niters());       

   nunits   = m.nunits();
   nseasons = m.nseasons();
   nareas   = m.nareas();

   //need to check seasons, areas & units
   }

double FLStock::computeStock(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      val += stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter);

   return val;
   }                               

double FLStock::computeMnSz(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0,
          num = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      val += stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter);
      num += stock_n( iage, iyr, iunit, iseason, iarea, iter);
      }
   
   return val/num;
   }                               

double FLStock::SSB(int iyr, int iunit, int iseason, int iarea, int iter) 
   {
   double val = 0.0, ssb = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      double deaded = exp(-m(      iage, iyr, iunit, iseason, iarea, iter)*m_spwn(      iage, iyr, iunit, iseason, iarea, iter)
                          -harvest(iage, iyr, iunit, iseason, iarea, iter)*harvest_spwn(iage, iyr, iunit, iseason, iarea, iter));
                    
      val  = stock_n( iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(iage, iyr, iunit, iseason, iarea, iter)*
             mat(     iage, iyr, iunit, iseason, iarea, iter)*deaded;

      if (!R_IsNA(val) && val>0.0) ssb +=val;
      }

   return ssb;
   }  
                             
double FLStock::computeCatch(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      val += catch_n( iage, iyr, iunit, iseason, iarea, iter)*
             catch_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock::computeLandings(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
//      double z = m(iage, iyr, iunit, iseason, iarea, iter) + harvest(iage, iyr, iunit, iseason, iarea, iter);

//      val += landings_n( iage, iyr, iunit, iseason, iarea, iter)/
//             catch_n(    iage, iyr, iunit, iseason, iarea, iter)*
//             stock_n(    iage, iyr, iunit, iseason, iarea, iter)*
//             harvest(    iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
//             landings_wt(iage, iyr, iunit, iseason, iarea, iter);

      val +=  catch_n(   iage, iyr, iunit, iseason, iarea, iter)*
              landings_n(iage, iyr, iunit, iseason, iarea, iter)/
             (landings_n(iage, iyr, iunit, iseason, iarea, iter)+
              discards_n(iage, iyr, iunit, iseason, iarea, iter))*
             landings_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock::computeDiscards(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant; iage<=maxquant; iage++)
      {
      double z = m(iage, iyr, iunit, iseason, iarea, iter) + harvest(iage, iyr, iunit, iseason, iarea, iter);

      val +=  catch_n(   iage, iyr, iunit, iseason, iarea, iter)*
              discards_n(iage, iyr, iunit, iseason, iarea, iter)/
             (landings_n(iage, iyr, iunit, iseason, iarea, iter)+
              discards_n(iage, iyr, iunit, iseason, iarea, iter))*
              discards_wt(iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStock::Fbar(int iyr, int iUnit, int iSeason, int iArea, int iIter)
   {
	double val=0.0;

	for (int iAge = minfbar; iAge<= maxfbar;iAge++)
		val += harvest(iAge, iyr, iUnit,iSeason,iArea,iIter);

	return (val / (maxfbar-minfbar+1));
   }

double FLStock::Zbar(int iyr, int iUnit, int iSeason, int iArea, int iIter)
   {
	double val=0.0;

	for (int iAge = minfbar; iAge<= maxfbar;iAge++)
		val += harvest(iAge, iyr, iUnit,iSeason,iArea,iIter) +
             m(      iAge, iyr, iUnit,iSeason,iArea,iIter);

	return (val / (maxfbar-minfbar+1));
   }

double FLStock::FbarLandings(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minfbar; iage<=maxfbar; iage++)
      val += harvest(   iage, iyr, iunit, iseason, iarea, iter)*
             landings_n(iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar-minfbar+1);
   }                               


double FLStock::FbarDiscards(int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minfbar; iage<=maxfbar; iage++)
      val += harvest(   iage, iyr, iunit, iseason, iarea, iter)*
             discards_n(iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar-minfbar+1);
   }   


FLStock::~FLStock(void)      
   {
   ;//unalloc();
   }                               

SEXP FLStock::ReturnRange(void)
   {
   SEXP Range;

   Range          = PROTECT(NEW_NUMERIC(7)); 
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = minfbar;
   REAL(Range)[6] = maxfbar;

   SEXP names;
    
   PROTECT(names = allocVector(STRSXP, 7));
   SET_STRING_ELT(names, 0, mkChar("min"));
   SET_STRING_ELT(names, 1, mkChar("max"));
   SET_STRING_ELT(names, 2, mkChar("plusgroup"));
   SET_STRING_ELT(names, 3, mkChar("minyear"));
   SET_STRING_ELT(names, 4, mkChar("maxyear"));
   SET_STRING_ELT(names, 5, mkChar("minfbar"));
   SET_STRING_ELT(names, 6, mkChar("maxfbar"));
   setAttrib(Range, R_NamesSymbol, names);
   
   return Range;
   }
         
SEXP FLStock::Return(void)
   {
   SEXP Stock, Range;

   PROTECT(Stock  = NEW_OBJECT(MAKE_CLASS("FLStock")));
   Range          = ReturnRange(); 

   SET_SLOT(Stock, install("catch"),       catch_.Return());
   SET_SLOT(Stock, install("catch.n"),     catch_n.Return());
   SET_SLOT(Stock, install("catch.wt"),    catch_wt.Return());
   SET_SLOT(Stock, install("discards"),    discards.Return());
   SET_SLOT(Stock, install("discards.n"),  discards_n.Return());
   SET_SLOT(Stock, install("discards.wt"), discards_wt.Return());
   SET_SLOT(Stock, install("landings"),    landings.Return());
   SET_SLOT(Stock, install("landings.n"),  landings_n.Return());
   SET_SLOT(Stock, install("landings.wt"), landings_wt.Return());
   SET_SLOT(Stock, install("stock"),       stock.Return());
   SET_SLOT(Stock, install("stock.n"),     stock_n.Return());
   SET_SLOT(Stock, install("stock.wt"),    stock_wt.Return());
   SET_SLOT(Stock, install("mat"),         mat.Return());
   SET_SLOT(Stock, install("harvest"),     harvest.Return()); 
   SET_SLOT(Stock, install("harvest.spwn"),harvest_spwn.Return());
   SET_SLOT(Stock, install("m"),           m.Return()); 
   SET_SLOT(Stock, install("m.spwn"),      m_spwn.Return());
   SET_SLOT(Stock, install("range"),       Range);
      
   UNPROTECT(3);

   return Stock;
   }
   
FLIndex::FLIndex(void)      
    {
    InitFlag = false;
    }

FLIndex::FLIndex(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndex(x) && !InitFlag)
       Init(x);
    }
   
void FLIndex::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   startf      = (int)REAL(GET_SLOT(x, install("range")))[5];
   endf        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   index.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index_var.Init(0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index.Init(    GET_SLOT(x, install("index"))); 
   index_var.Init(GET_SLOT(x, install("index.var"))); 
   catch_n.Init(    GET_SLOT(x, install("catch.n"))); 
   catch_wt.Init(    GET_SLOT(x, install("catch.wt"))); 
   effort.Init(    GET_SLOT(x, install("effort"))); 
   sel_pattern.Init(    GET_SLOT(x, install("sel.pattern"))); 
   index_q.Init(    GET_SLOT(x, install("index.q"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndex::~FLIndex(void)      
   {
   ; //unalloc();
   }                               

SEXP FLIndex::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndex")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = startf;
   REAL(Range)[6] = endf;
   
   SET_SLOT(Index, install("range"),      Range);
   SET_SLOT(Index, install("index"),      index.Return());
   SET_SLOT(Index, install("index.var"),  index_var.Return());
   SET_SLOT(Index, install("catch.n"),      catch_n.Return());
   SET_SLOT(Index, install("catch.wt"),      catch_wt.Return());
   SET_SLOT(Index, install("effort"),      catch_n.Return());
   SET_SLOT(Index, install("sel.pattern"),      sel_pattern.Return());
   SET_SLOT(Index, install("index.q"),      index_q.Return());
      
   UNPROTECT(2);

   return Index;
   }

FLBiol::FLBiol(void)      
    {
    InitFlag = false;
    }

FLBiol::FLBiol(SEXP x)      
    {
    InitFlag = false;

    if (isFLBiol(x))
       Init(x);
    }
   
void FLBiol::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   n.Init(   GET_SLOT(x, install("n"))); 
   m.Init(   GET_SLOT(x, install("m"))); 
   wt.Init(  GET_SLOT(x, install("wt"))); 
   fec.Init( GET_SLOT(x, install("fec"))); 
   spwn.Init(GET_SLOT(x, install("spwn"))); 
   
   //need to check seasons, areas & units
   }

FLBiol::~FLBiol(void)      
   {
   ; //unalloc();
   }                               

SEXP FLBiol::Return(void)
   {
   SEXP Biol, Range;

   PROTECT(Biol  = NEW_OBJECT(MAKE_CLASS("FLBiol")));
   Range          = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
       
   SET_SLOT(Biol, install("n"),       n.Return());
   SET_SLOT(Biol, install("m"),       m.Return());
   SET_SLOT(Biol, install("wt"),      wt.Return());
   SET_SLOT(Biol, install("fec"),     fec.Return());
   SET_SLOT(Biol, install("spwn"),    spwn.Return());

   SET_SLOT(Biol, install("range"),   Range);
      
   UNPROTECT(2);

   return Biol;
   }
   
FLVector::FLVector(void)      
    {
    InitFlag = false;
    }

double& FLVector::operator()(int _i) 
   { 

   if (_i < mindim || _i > maxdim) 
      return outofbounds_double =0.0;
   
   return (data)[_i]; 
   } 

FLVector::FLVector(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FLVector::Init(SEXP x)      
   {
   SEXP v;
   int n;

   if (!isVector(x) || !isNumeric(x)) 
      return;

   PROTECT(v = AS_NUMERIC(x));
   n = LENGTH(v);

   double *d = NUMERIC_POINTER(v); 

   SEXP names = GET_NAMES(v);
   if (LENGTH(names) == n) //index by name
      {
      //get indices
      mindim = atoi(CHAR(STRING_ELT(names, 0)));
      maxdim = mindim + n - 1;

      //check indices
      for (int i=1; i<n; i++)
         if ((mindim+i) != atoi(CHAR(STRING_ELT(names, i))))
            return;
      }
   else
     {
     mindim = 1;
     maxdim = n;
     }
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = (d)[i - mindim];       
      
   UNPROTECT(1);

   return;
   }

void FLVector::Init(int _min, int _max, double val)      
   {
   if (_min > _max) return;

   mindim = _min;
   maxdim = _max;
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = val;       
      
   return;
   }

SEXP FLVector::Return(void)      
    {
    int i, j;
    
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    v = PROTECT(NEW_NUMERIC(maxdim-mindim+1)); 
  
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = maxdim-mindim+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(INTSXP, maxdim-mindim+1));
    for (j=mindim, i=0; j<=maxdim; j++, i++)
        INTEGER(d1)[i] = j; 
    SET_VECTOR_ELT(dimnames, 1, d1);
     
    //Set data
    for (j=mindim, i=0; j<=maxdim; j++, i++)
       REAL(v)[i] =data [j];
           
    
    UNPROTECT(5);
    
    return v;
    }

void FLVector::unalloc(void)      
   {
   if (!InitFlag) return;

   delete [] (data+mindim);
   
   InitFlag=false;
   }                               

void FLVector::alloc(void)      
   {
   if (InitFlag) unalloc();

   data = new double[maxdim-mindim+1] - mindim;
   }

FLVector::~FLVector(void)      
   {
   unalloc();
   }                               

FLBool::FLBool(void)      
    {
    InitFlag = false;
    }

bool& FLBool::operator()(int _i) 
   { 

   if (_i < mindim || _i > maxdim) 
      outofbounds_bool=false;

   return (data)[_i]; 
   } 

FLBool::FLBool(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FLBool::Init(SEXP x)      
   {
   SEXP v;
   int n;

   if (!isLogical(x)) 
      return;

   PROTECT(v = AS_LOGICAL(x));
   n = LENGTH(v);

   int *d = LOGICAL_POINTER(v); 

   SEXP names = GET_NAMES(v);
   if (LENGTH(names) == n) //index by name
      {
      //get indices
      mindim = atoi(CHAR(STRING_ELT(names, 0)));
      maxdim = mindim + n - 1;

      //check indices
      for (int i=1; i<n; i++)
         if ((mindim+i) != atoi(CHAR(STRING_ELT(names, i))))
            return;
      }
   else
     {
     mindim = 1;
     maxdim = n;
     }
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = (d)[i - mindim] <= 0 ? false : true;       
      
   UNPROTECT(1);

   return;
   }

SEXP FLBool::Return(void)      
    {
    int i, j;
    
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    v = PROTECT(NEW_NUMERIC(maxdim-mindim+1)); 
  
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = maxdim-mindim+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(INTSXP, maxdim-mindim+1));
    for (j=mindim, i=0; j<=maxdim; j++, i++)
        INTEGER(d1)[i] = j; 
    SET_VECTOR_ELT(dimnames, 1, d1);
     
    //Set data
    for (j=mindim, i=0; j<=maxdim; j++, i++)
       REAL(v)[i] =data [j];
           
    
    UNPROTECT(5);
    
    return v;
    }

void FLBool::unalloc(void)      
   {
   if (!InitFlag) return;

   delete [] (data+mindim);
   
   InitFlag=false;
   }                               

void FLBool::alloc(void)      
   {
   if (InitFlag) unalloc();

   data= new bool[maxdim-mindim+1] - mindim;
   }

FLBool::~FLBool(void)      
   {
   unalloc();
   }                               


FL2D::FL2D(void)      
    {
    InitFlag = false;
    }

double& FL2D::operator()(int _i, int _j) 
   { 
   if (_i < min1 || _i > max1 || _j < min2 || _j > max2) 
      return outofbounds_double=0.0;

   return (data)[_i][_j]; 
   } 

int FL2D::mindim(int i) 
   { 
   switch (i) {
      case 1:
         return min1;
      case 2:
         return min2;
      }

   return outofbounds_int=0;
   }

int FL2D::maxdim(int i) 
   { 
   switch (i) {
      case 1:
         return max1;
      case 2:
         return max2;
      }

   return outofbounds_int=0;
   }

FL2D::FL2D(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FL2D::Init(SEXP x)      
    {
    if (isMatrix(x)  && isNumeric(x))
       {
       SEXP dims     = GET_DIM(x),
            dimnames = GET_DIMNAMES(x);

       int dim[2], n = length(dims);

       if (n !=2) return;

       double *a = NUMERIC_POINTER(x);

       dim[0] = INTEGER(dims)[0];
       dim[1] = INTEGER(dims)[1];
    
       min1  = 0;
       min2  = 0;
       max1  = (int)dim[0] -1;
       max2  = (int)dim[1] -1;

       if (dimnames != R_NilValue) 
         if (TYPEOF(dimnames) == VECSXP) 
            {
            int  t = 0;
            const char *c;
             	   
            if (n >= 1 && INTEGER(dims)[0] >= 1) 
               {
               c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));

               //check that name is not a text string
               for (int i=0; i<=(signed)strlen(c); i++)
                  if (isalpha(c[i])) t=1;

               if (t !=1)
	               t = atoi(c); 

               min1 += t;
               max1 += t;
  	            }
		      if (n >= 2 && INTEGER(dims)[1] >= 1) 
               {
	            c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));

               //check that name is not a text string
               for (int i=0; i<=(signed)strlen(c); i++)
                  if (isalpha(c[i])) t=1;

               if (t !=1)
	               t = atoi(c); 
            
               min2 += t;
               max2 += t;
    	         }
		      }
		 
       alloc();

       int i1, i2,
             i,  j;


       for (i1 = min1, i = 0; i1 <= max1; i1++, i++)
          for (i2 = min2, j = 0; i2 <= max2; i2++, j++)
             data[i1][i2] = (a)[i + j*(max1-min1+1)];       
       }
    else if (isVector(x)  && isNumeric(x))
       {
       SEXP v;

       PROTECT(v = AS_NUMERIC(x));
       int n = LENGTH(v);

       double *d = NUMERIC_POINTER(v); 

       SEXP names = GET_NAMES(v);
       if (LENGTH(names) == n) //index by name
          {
          int  t = 0;
          const char *c;
             	   
          c = CHAR(STRING_ELT(names, 0));

          //check that name is not a text string
          for (int i=0; i<=(signed)strlen(c); i++)
             if (isalpha(c[i])) t=1;

          if (t !=1)
             t = atoi(c); 

          min1 = t; 
 
          if (t==0)      
            min1 = atoi(CHAR(STRING_ELT(names, 0)));
          else 
            min1 = t;

          max1 = min1 + n - 1;
          min2 = max2 = 1;
 
          //check indices
          //for (int i=1; i<n; i++)
          //   if ((min1+i) != atoi(CHAR(VECTOR_ELT(names, i))))
          //      return;
          }
       else
          {
          min1 = 1;
          max1 = n;
          min2 = max2 = 1;
          }
    
       alloc();
 
       int i1, i;
       for (i1 = min1, i = 0; i1 <= max1; i1++, i++)
          data[i1][1] = (d)[i];       

       UNPROTECT(1);
       }
    else
       return;
        
    return;
    }

SEXP FL2D::Return(void)      
    {
    int i, j, i_, j_;
    
    SEXP v, 
         d1, d2,  
         dim,   dimnames;    

    //Create new S4 object    
    PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 2));       
    INTEGER(dim)[0] = max1 -min1 +1;
    INTEGER(dim)[1] = max2-min2+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 2));
    
    PROTECT(d1 = allocVector(INTSXP, max1-min1 +1));
    for (i_=min1, i=0; i_<=max1; i_++, i++)
        INTEGER(d1)[i] = i_; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, max2-min2+1));
    for (j_=min2, i=0; j_<=max2; j_++, i++)
        INTEGER(d2)[i] = j_; 
    SET_VECTOR_ELT(dimnames, 1, d2);
    
    setAttrib(v, install("dimnames"), dimnames);
     
    //Set data
    for (i_=min1, i=0; i_<=max1; i_++, i++)
       for (j_=min2, j=0; j_<=max2; j_++, j++)
          REAL(v)[i + j*(max1-min1+1)] =data[i_][j_];
           
    UNPROTECT(6);
    
    return v;
    }

void FL2D::unalloc(void)      
   {
   if (!InitFlag) return;

   for (int i=min1; i<=max1; i++)
      delete [] (data[i] + min2);

   delete [] (data+min1);
  
   InitFlag=false;
   }                               

void FL2D::alloc(void)      
   {
   if (InitFlag) unalloc();

   int i = 0;
   data = new double*[max1-min1+1] - min1;
   for(i=min1;i<=max1;i++) 
      (data)[i]  = new double[max2-min2+1] - min2;
      
   }                                                           
   
FL2D::~FL2D(void)      
   {
   unalloc();
   }                               

FLCatch::FLCatch(void)      
    {
    InitFlag = false;
    }

FLCatch::FLCatch(SEXP x)      
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }
   
FLCatch::~FLCatch(void)      
   {
   ;   
   }                               

void FLCatch::Init(SEXP x)
   {

   minquant  = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant  = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp   = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr     = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr     = (int)REAL(GET_SLOT(x, install("range")))[4];

   catch_q.Init(     GET_SLOT(x, install("catch.q")));
   catch_.Init(      GET_SLOT(x, install("catch")));
   catch_n.Init(     GET_SLOT(x, install("catch.n")));
   catch_wt.Init(    GET_SLOT(x, install("catch.wt")));
   catch_sel.Init(   GET_SLOT(x, install("catch.sel")));
   discards.Init(    GET_SLOT(x, install("discards")));
   discards_n.Init(  GET_SLOT(x, install("discards.n")));
   discards_wt.Init( GET_SLOT(x, install("discards.wt")));
   discards_sel.Init(GET_SLOT(x, install("discards.sel")));
   landings.Init(    GET_SLOT(x, install("landings")));
   landings_n.Init(  GET_SLOT(x, install("landings.n")));
   landings_wt.Init( GET_SLOT(x, install("landings.wt")));
   landings_sel.Init(GET_SLOT(x, install("landings.sel")));
   price.Init(       GET_SLOT(x, install("price")));

   InitFlag = true;
   }

SEXP FLCatch::Return(void)
   {
   SEXP Catch, Range;


   PROTECT(Catch  = NEW_OBJECT(MAKE_CLASS("FLCatch")));
   Range          = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   
   SET_SLOT(Catch, install("catch"),       catch_.Return());
   SET_SLOT(Catch, install("catch.q"),     catch_q.Return());            
	SET_SLOT(Catch, install("catch.n"),     catch_n.Return());      
	SET_SLOT(Catch, install("catch.wt"),    catch_wt.Return());     
   SET_SLOT(Catch, install("catch.sel"),   catch_sel.Return());    
	SET_SLOT(Catch, install("discards"),    discards.Return());     
	SET_SLOT(Catch, install("discards.n"),  discards_n.Return());   
	SET_SLOT(Catch, install("discards.wt"), discards_wt.Return());  
	SET_SLOT(Catch, install("discards.sel"),discards_sel.Return()); 
	SET_SLOT(Catch, install("landings"),    landings.Return());     
	SET_SLOT(Catch, install("landings.n"),  landings_n.Return());   
	SET_SLOT(Catch, install("landings.wt"), landings_wt.Return());  
	SET_SLOT(Catch, install("landings.sel"),landings_sel.Return());  
   SET_SLOT(Catch, install("price"),       price.Return());         

      
   UNPROTECT(2);

   return Catch;
   }
 
FLFleet::FLFleet(void)
   {
   InitFlag = false;
      
   nunits   = 
   nseasons = 
   nareas   = 
   nspp     = 0;
   }
      
FLFleet::FLFleet(SEXP x)
   {
   InitFlag = false;

   nunits   = 
   nseasons = 
   nareas   = 
   nspp     = 0;

   Init(x);
   }

FLFleet::~FLFleet(void)     
   {
   ;
   }

void  FLFleet::Init(SEXP x)
   {
   if (nunits >= 1 || nseasons >= 1 || nareas >= 1 || nspp >= 1) unalloc();

   minquant = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp  = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr    = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr    = (int)REAL(GET_SLOT(x, install("range")))[4];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   
   effort.Init(   GET_SLOT(x, install("effort"))); 
   capacity.Init( GET_SLOT(x, install("capacity")));   
   crewshare.Init(GET_SLOT(x, install("crewshare")));
   fcost.Init(    GET_SLOT(x, install("fcost"))); 

   SEXP metiers = PROTECT(GET_SLOT(x, install("metiers")));
  
   nmetier = NElemList(metiers);

   if (nmetier < 1) return;
   
   effshare.alloc_n7(nmetier);
   vcost.alloc_n7(   nmetier);

   SEXP catches = PROTECT(GET_SLOT(VECTOR_ELT(metiers, 0), install("catches")));
  
   nspp = NElemList(catches);

   if (nspp < 1) return;

   catch_.Init(      nmetier, nspp);
   catch_n.Init(     nmetier, nspp);
   catch_wt.Init(    nmetier, nspp);
   catch_sel.Init(   nmetier, nspp);
   catch_q.Init(     nmetier, nspp);
   landings.Init(    nmetier, nspp);
   landings_n.Init(  nmetier, nspp);
   landings_wt.Init( nmetier, nspp);
   landings_sel.Init(nmetier, nspp);
   discards.Init(    nmetier, nspp);
   discards_n.Init(  nmetier, nspp);
   discards_wt.Init( nmetier, nspp);
   discards_sel.Init(nmetier, nspp);
   price.Init(       nmetier, nspp);

   for (int i=1; i<=nmetier; i++)
      {
      SEXP metier  = PROTECT(VECTOR_ELT(metiers, i-1));
      SEXP catches = PROTECT(GET_SLOT(metier, install("catches")));

      effshare.Init(i, GET_SLOT(metier, install("effshare"))); 
      vcost.Init(   i, GET_SLOT(metier, install("vcost"))); 
  
      nspp = NElemList(catches);

      if (nspp < 1) return;
      
      for (int j=1; j<=nspp; j++)
         {
         SEXP t = PROTECT(VECTOR_ELT(catches, j-1));

         catch_.Init(      i, j, GET_SLOT(t, install("catch"))); 
         catch_n.Init(     i, j, GET_SLOT(t, install("catch.n"))); 
         catch_wt.Init(    i, j, GET_SLOT(t, install("catch.wt"))); 
         catch_sel.Init(   i, j, GET_SLOT(t, install("catch.sel"))); 
         catch_q.Init(     i, j, GET_SLOT(t, install("catch.q"))); 
         landings.Init(    i, j, GET_SLOT(t, install("landings"))); 
         landings_n.Init(  i, j, GET_SLOT(t, install("landings.n"))); 
         landings_wt.Init( i, j, GET_SLOT(t, install("landings.wt"))); 
         landings_sel.Init(i, j, GET_SLOT(t, install("landings.sel"))); 
         discards.Init(    i, j, GET_SLOT(t, install("discards"))); 
         discards_n.Init(  i, j, GET_SLOT(t, install("discards.n"))); 
         discards_wt.Init( i, j, GET_SLOT(t, install("discards.wt"))); 
         discards_sel.Init(i, j, GET_SLOT(t, install("discards.sel"))); 
         price.Init(       i, j, GET_SLOT(t, install("price"))); 
         }
       }

   UNPROTECT(2);
   }

void  FLFleet::alloc(void)      
      {
      ;
      }

void  FLFleet::unalloc(void)      
      {
      ;
      }

SEXP  FLFleet::Return(void)     
   {
   SEXP fleet, Range;

   PROTECT(fleet  = NEW_OBJECT(MAKE_CLASS("FLFleet")));
   PROTECT(Range  = PROTECT(NEW_NUMERIC(5))); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   
   SET_SLOT(fleet, install("range"),      Range);         
   SET_SLOT(fleet, install("effort"),     effort.Return());         
   SET_SLOT(fleet, install("capacity"),   capacity.Return());         
   SET_SLOT(fleet, install("crewshare"),  crewshare.Return());         
   SET_SLOT(fleet, install("fcost"),      fcost.Return());         
   SET_SLOT(fleet, install("vcost"),      vcost.Return());         
   
   SEXP metiers      = NEW_OBJECT(MAKE_CLASS("FLMetiers"));
   SEXP metiers_data = allocVector(VECSXP, nmetier);        
   
   for (int i=1; i<=nmetier; i++)
      {
      SEXP metier = NEW_OBJECT(MAKE_CLASS("FLMetier"));

      SET_SLOT(metier, install("effshare"), effshare.Return(i));         
      SET_SLOT(metier, install("vcost"),    vcost.Return(i));         
 
      SEXP catches    = NEW_OBJECT(MAKE_CLASS("FLCatches"));
      SEXP catch_data = allocVector(VECSXP, nspp);        
   
      for (int j=1; j<=nspp; j++)
         {
         SEXP _catch = NEW_OBJECT(MAKE_CLASS("FLCatch"));
      
         SET_SLOT(_catch, install("catch"),        catch_.Return(i,j));
         SET_SLOT(_catch, install("catch.n"),      catch_n.Return(i,j));
         SET_SLOT(_catch, install("catch.wt"),     catch_wt.Return(i,j));
         SET_SLOT(_catch, install("catch.sel"),    catch_sel.Return(i,j));
         SET_SLOT(_catch, install("catch.q"),      catch_q.Return(i,j));
         SET_SLOT(_catch, install("landings"),     landings.Return(i,j));
         SET_SLOT(_catch, install("landings.n"),   landings_n.Return(i,j));
         SET_SLOT(_catch, install("landings.wt"),  landings_wt.Return(i,j));
         SET_SLOT(_catch, install("landings.sel"), landings_sel.Return(i,j));
         SET_SLOT(_catch, install("discards"),     discards.Return(i,j));
         SET_SLOT(_catch, install("discards.n"),   discards_n.Return(i,j));
         SET_SLOT(_catch, install("discards.wt"),  discards_wt.Return(i,j));
         SET_SLOT(_catch, install("discards.sel"), discards_sel.Return(i,j));
         SET_SLOT(_catch, install("price"),        price.Return(i,j));

         SET_VECTOR_ELT(catch_data, j-1, _catch); 
         }
      catches = R_do_slot_assign(catches, install(".Data"), catch_data);
    
      SET_SLOT(metier, install("catches"), catches);
      
      SET_VECTOR_ELT(metiers_data, i-1, metier); 
      }
   metiers = R_do_slot_assign(metiers, install(".Data"), metiers_data);
      
   SET_SLOT(fleet, install("metiers"), metiers);
   
   return fleet;
   }

SEXP ReturnDouble(double x) 
    {
    SEXP v = PROTECT(NEW_NUMERIC(1)); 
  
    REAL(v)[0] = x;
           
    UNPROTECT(1);
    
    return v;    
    }

FLQuant2::FLQuant2(void)      
    {
    flq_n = 0;
    }

FLQuant2::FLQuant2(SEXP x)  
    {
    flq_n = 0;

    if (!isFLQuants(x)) 
       return;
    
    alloc_n7(NElemList(x));

    for (int i=1; i<=flq_n; i++)
       {
       Init(i,PROTECT(VECTOR_ELT(x, i-1)));

       UNPROTECT(1); 
       }
    }

FLQuant2::FLQuant2(int n7)  
    {
	 if (n7 < 1)
       return;

    alloc_n7(n7);
    }

FLQuant2::FLQuant2(int n7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)
   {
	if (n7 >= 1) 
	   return;

   Init(n7,_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val);
   }

void FLQuant2::alloc_n7(int n7)
   {
   if (n7 <1) 
      return;
   
   //if (flq_n>0)
   //   {
   //   for (int i=1; i<=flq_n; i++)
   //      if (InitFlag(i))
   //         unalloc(i);
   //
   //   unalloc_n7();
   //   }

   flq_n = n7;
 
   flq_minquant = new int[flq_n] - 1; 
   flq_maxquant = new int[flq_n] - 1; 
   flq_minyr    = new int[flq_n] - 1;     
   flq_maxyr    = new int[flq_n] - 1; 
   flq_nunits   = new int[flq_n] - 1; 
   flq_nseasons = new int[flq_n] - 1; 
   flq_nareas   = new int[flq_n] - 1; 
   flq_niters   = new int[flq_n] - 1; 
   flq_InitFlag = new bool[flq_n] - 1; 

   for (int i=1; i<=flq_n; i++)
      flq_InitFlag[i]=FALSE;

   data = new double******[flq_n] - 1; ;   
   }

void FLQuant2::unalloc_n7(void)
   {
   if (flq_n <1) 
       return;
   
   delete[]  (flq_minquant+1);
   delete[]  (flq_maxquant+1);
   delete[]  (flq_minyr   +1);
   delete[]  (flq_maxyr   +1);
   delete[]  (flq_nunits  +1); 
   delete[]  (flq_nseasons+1); 
   delete[]  (flq_nareas  +1); 
   delete[]  (flq_niters  +1); 
   delete[]  (flq_InitFlag+1); 
   
   delete[]  (data+1);

   flq_n = 0;
   }

void FLQuant2::Init(int i7, SEXP x)
   {
   if (i7<0 || i7>flq_n) 
      return;
   
   if (!isFLQuant(x)) 
      return;

   if (InitFlag(i7)) 
      unalloc(i7);

   SEXP Quant    = GET_SLOT(x, install(".Data")),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

   double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

   int dim[6], n = length(dims);

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];
   dim[3] = INTEGER(dims)[3];
   dim[4] = INTEGER(dims)[4];
   dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
   if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
       ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
     return;

   minquant(i7) = 0;
   minyr(i7)    = 0;
   maxquant(i7) = (int)dim[0] -1;
   maxyr(i7)    = (int)dim[1] -1;
   nunits(i7)   = (int)dim[2];
   nseasons(i7) = (int)dim[3];
   nareas(i7)   = (int)dim[4]; 
   niters(i7)   = (int)dim[5];
	   
      
   if (dimnames != R_NilValue) 
     if (TYPEOF(dimnames) == VECSXP) 
        {
        int  t = 0;
        const char *c;
        
        if (n >= 1 && INTEGER(dims)[0] >= 1) 
           {
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));
            //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;
            if (t !=1)
            t = atoi(c); 
            minquant(i7) += t;
            maxquant(i7) += t;
 	         }
	   
        if (n >= 2 && INTEGER(dims)[1] >= 1) 
           {
           t = 0;
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));
           //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;

            if (t !=1)
            t = atoi(c); 
            
            minyr(i7)   += t;
            maxyr(i7)   += t;
 	      	}
	    }


   alloc(i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i7); iUnit++)
		         for (iYear = minyr(i7); iYear <= maxyr(i7); iYear++)
			   		   for (iAge = minquant(i7); iAge <= maxquant(i7); iAge++)
			       			   data[i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                   
    }
         
void FLQuant2::Init(int i7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)      
   {
   if (i7<0 || i7>flq_n)
      return;

   if (InitFlag(i7)) 
      unalloc(i7);

   minquant(i7) = _minquant;
   maxquant(i7) = _maxquant;
   minyr(i7)    = _minyr;
   maxyr(i7)    = _maxyr;
   nunits(i7)   = _nunits;
   nareas(i7)   = _nareas;
   nseasons(i7) = _nseasons;
   niters(i7)   = _niters;

   alloc(i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i7); iUnit++)
		         for (iYear = minyr(i7); iYear <= maxyr(i7); iYear++)
			   	   for (iAge = minquant(i7); iAge <= maxquant(i7); iAge++)
			      	   data[i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

double& FLQuant2::operator()(int i7, int _age,int _yr,int _unit, int _season, int _area, int _iter) { 

  //if (i7<1)                 return outofbounds_double;
  //if (i7 > flq_n)           return outofbounds_double; 
  //if (!InitFlag(i7))        return outofbounds_double;
  //if (_age   <minquant(i7)) return outofbounds_double;
  //if (_age   >maxquant(i7)) return outofbounds_double;
  //if (_yr    <minyr(i7))    return outofbounds_double;
  //if (_yr    >maxyr(i7))    return outofbounds_double;
  //if (_unit  <1)            return outofbounds_double;
  //if (_unit  >nunits(i7))   return outofbounds_double;
  //if (_season<1)            return outofbounds_double;
  //if (_season>nseasons(i7)) return outofbounds_double;
  //if (_area  <1)            return outofbounds_double;
  //if (_area  >nareas(i7))   return outofbounds_double;
      
  if (i7<1 || i7 > flq_n   || 
       !InitFlag(i7)        || 
       _age   <minquant(i7) || _age   >maxquant(i7) || 
       _yr    <minyr(i7)    || _yr    >maxyr(i7)    || 
       _unit  <1            || _unit  >nunits(i7)   ||
       _season<1            || _season>nseasons(i7) || 
       _area  <1            || _area  >nareas(i7)) 
      return outofbounds_double;
   else
      return (data)[i7][_age][_yr][_unit][_season][_area][__min(_iter,niters(i7))];  
   } 


//Only return 5 dim array 
SEXP FLQuant2::Return(int i7)      
    {
    if (i7<0 || i7>flq_n) 
       return R_NilValue;

    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant(i7)-minquant(i7) +1;
    INTEGER(dim)[1] = maxyr(i7)   -minyr(i7)    +1;
    INTEGER(dim)[2] = nunits(i7); 
    INTEGER(dim)[3] = nseasons(i7); 
    INTEGER(dim)[4] = nareas(i7); 
    INTEGER(dim)[5] = niters(i7);

    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant(i7)-minquant(i7) +1));
    for (iAge=minquant(i7), i=0; iAge<=maxquant(i7); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr(i7)-minyr(i7)+1));
    for (iYear=minyr(i7), i=0; iYear<=maxyr(i7); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits(i7)==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits(i7)));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits(i7)));
       for (iUnit=1, i=0; iUnit<=nunits(i7); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons(i7)==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons(i7)));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons(i7)));
       for (iSeason=1, i=0; iSeason<=nseasons(i7); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas(i7)==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas(i7)));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas(i7)));
       for (iArea=1, i=0; iArea<=nareas(i7); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters(i7)));
    for (iIter=1, i=0; iIter<=niters(i7); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 
 
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(i7); iIter++)
	    for (iArea = 1; iArea <= nareas(i7); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(i7); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(i7); iUnit++)
	    		    for (iYear = minyr(i7); iYear <= maxyr(i7); iYear++)
			 		    for (iAge = minquant(i7); iAge <= maxquant(i7); iAge++)
			      			    REAL(v)[i++] = data[i7][iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

SEXP FLQuant2::Return(void)
   {
   SEXP ReturnObject;

   PROTECT(ReturnObject = allocVector(VECSXP,flq_n));
   for (int i=0; i<flq_n; i++)
      SET_VECTOR_ELT(ReturnObject, i,  Return(i+1));
   
   UNPROTECT(1);
   return ReturnObject;
   }

void FLQuant2::alloc(int i7)      
   {
   if (i7<0 || i7>flq_n)
      return;
 
   (data)[i7] = new double*****[maxquant(i7)-minquant(i7)+1] - minquant(i7);
   for(int i=minquant(i7);i<=maxquant(i7);i++) 
      {
      (data)[i7][i] = new double****[maxyr(i7)-minyr(i7)+1] - minyr(i7);
      for (int j=minyr(i7); j<=maxyr(i7); j++)
         {
         (data)[i7][i][j] = new double***[nunits(i7)] - 1;
         for (int k = 1; k<=nunits(i7); k++)
            {
            (data)[i7][i][j][k] = new double**[nseasons(i7)] - 1;
            for (int l=1; l<=nseasons(i7); l++)
	             {
                (data)[i7][i][j][k][l] = new double*[nareas(i7)] - 1;
	             for (int m=1; m<=nareas(i7); m++)
      		       (data)[i7][i][j][k][l][m] = new double[niters(i7)] - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag(i7)=true;
   }

void FLQuant2::unalloc(int i7)      
   {
   if (i7<1 || i7>flq_n || !InitFlag(i7)) 
      return;
   
   for (int iAge  = minquant(i7); iAge <= maxquant(i7); iAge++)
      {
      for (int iYear  = minyr(i7); iYear  <= maxyr(i7);   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits(i7);  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(i7); iSeason++) 
       		   {
	             for (int iArea = 1; iArea <= nareas(i7); iArea++) 
                   delete [] (data[i7][iAge][iYear][iUnit][iSeason][iArea]+1);
  
         	   delete [] (data[i7][iAge][iYear][iUnit][iSeason]+1);
         	   }
            delete [] (data[i7][iAge][iYear][iUnit]+1);
            }
         delete [] (data[i7][iAge][iYear]+1);
         }
      delete [] (data[i7][iAge]+minyr(i7));
      }
   delete [] (data[i7]+minquant(i7));
   }                    

void FLQuant2::unalloc(void)      
   {
   for (int i=1; i<=flq_n; i++)
      if (InitFlag(i)) unalloc(i);

   unalloc_n7(); 
   }                               

FLQuant2::~FLQuant2(void)      
   {
   unalloc(); 
   }                               

int& FLQuant2::minquant(int i7)
   {
   return flq_minquant[i7];
   }

int& FLQuant2::maxquant(int i7)
   {
   return flq_maxquant[i7];
   }

int& FLQuant2::minyr(int i7)
   {
   return flq_minyr[i7];
   }

int& FLQuant2::maxyr(int i7)
   {
   return flq_maxyr[i7];
   }

int& FLQuant2::nunits(int i7)
   {
   return flq_nunits[i7];
   }

int& FLQuant2::nseasons(int i7)
   {
   return flq_nseasons[i7];
   }

int& FLQuant2::nareas(int i7)
   {
   return flq_nareas[i7];
   }
 
int& FLQuant2::niters(int i7)
   {
   return flq_niters[i7];
   }

int& FLQuant2::n(void)
   {
   return flq_n;
   }

bool& FLQuant2::InitFlag(int i7)
   {
   return flq_InitFlag[i7];
   }

FLQuant3::FLQuant3(void)      
    {
    flq_n8 = 0;
    }

FLQuant3::FLQuant3(int n8, int n7)  
    {
	 if (n8<1 || n7<1)
       return;

    alloc(n8, n7);
    }

FLQuant3::FLQuant3(int n8, int n7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)
   {
   if (n8<1 || n7<1)
       return;

   Init(n8, n7,_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val);
   }

void FLQuant3::alloc_n8(int n8)
   {
   if (n8 <1) 
      return;
   
   //if (flq_n8>0)
   //   {
   //   for (int i=1; i<=flq_n8; i++)
   //      {
	//	   for (int j=1; j<=flq_n7[i]; j++)
  	//	      if  (InitFlag(i,j))
	//			   unalloc(i,j);
   //
   //      unalloc_n7(i);
	//	   }
   //
   //   unalloc_n8();
   //   }

   flq_n8 = n8;
 
   flq_n7       = new int[flq_n8]  - 1; 
   flq_minquant = new int*[flq_n8] - 1; 
   flq_maxquant = new int*[flq_n8] - 1; 
   flq_minyr    = new int*[flq_n8] - 1;     
   flq_maxyr    = new int*[flq_n8] - 1; 
   flq_nunits   = new int*[flq_n8] - 1; 
   flq_nseasons = new int*[flq_n8] - 1; 
   flq_nareas   = new int*[flq_n8] - 1; 
   flq_niters   = new int*[flq_n8] - 1; 
   flq_InitFlag = new bool*[flq_n8] - 1; 

   data = new double*******[flq_n8] - 1; ;   
   }

void FLQuant3::alloc_n7(int i8, int n7)
   {
   if (i8 <1 || i8>flq_n8) 
      return;
   
   //if (flq_n7[i8]>0)
   //   {
   //	for (int i=1; i<=flq_n7[i8]; i++)
  	//	   if (InitFlag(i8,i))
	//	      unalloc(i8,i);
   //   unalloc_n7(i8);
	//   }

   flq_n7[i8] = n7;
 
   flq_minquant[i8] = new int[flq_n7[i8]] - 1; 
   flq_maxquant[i8] = new int[flq_n7[i8]] - 1; 
   flq_minyr[i8]    = new int[flq_n7[i8]] - 1;     
   flq_maxyr[i8]    = new int[flq_n7[i8]] - 1; 
   flq_nunits[i8]   = new int[flq_n7[i8]] - 1; 
   flq_nseasons[i8] = new int[flq_n7[i8]] - 1; 
   flq_nareas[i8]   = new int[flq_n7[i8]] - 1; 
   flq_niters[i8]   = new int[flq_n7[i8]] - 1; 
   flq_InitFlag[i8] = new bool[flq_n7[i8]] - 1; 

   data[i8] = new double******[flq_n7[i8]] - 1; 

   for (int j=1; j<=flq_n7[i8]; j++)
      flq_InitFlag[i8][j]=FALSE;
   }

void FLQuant3::unalloc_n8(void)
   {
   if (flq_n8 <1) 
       return;
   
   delete[]  (flq_n7+1);
   delete[]  (flq_minquant+1);
   delete[]  (flq_maxquant+1);
   delete[]  (flq_minyr   +1);
   delete[]  (flq_maxyr   +1);
   delete[]  (flq_nunits  +1); 
   delete[]  (flq_nseasons+1); 
   delete[]  (flq_nareas  +1); 
   delete[]  (flq_niters  +1); 
   delete[]  (flq_InitFlag+1); 
   
   delete[]  (data+1);

   flq_n8 = 0;
   }

void FLQuant3::unalloc_n7(int i8)
   {
   if (i8<1 || i8>flq_n8) 
       return;
   
   delete[]  (flq_minquant[i8]+1);
   delete[]  (flq_maxquant[i8]+1);
   delete[]  (flq_minyr[i8]   +1);
   delete[]  (flq_maxyr[i8]   +1);
   delete[]  (flq_nunits[i8]  +1); 
   delete[]  (flq_nseasons[i8]+1); 
   delete[]  (flq_nareas[i8]  +1); 
   delete[]  (flq_niters[i8]  +1); 
   delete[]  (flq_InitFlag[i8]+1); 
   
   delete[]  (data[i8]+1);

   flq_n7[i8] = 0;
   }

void FLQuant3::Init(int n8, int n7)  
    {
	 if (n8<1 || n7<1)
       return;

    alloc_n8(n8);

    for (int i=1; i<=n8; i++)  
       alloc_n7(i, n7);
    }

void FLQuant3::Init(int i8, int i7, SEXP x)
   {
   if (i8<1 || i8>flq_n8) 
      return;
	  
   if (i7<1 || i7>flq_n7[i8]) 
      return;

   if (!isFLQuant(x)) 
      return;

   if (InitFlag(i8, i7)) 
      unalloc(i8, i7);

   SEXP Quant    = GET_SLOT(x, install(".Data")),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

   double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

   int dim[6], n = length(dims);

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];
   dim[3] = INTEGER(dims)[3];
   dim[4] = INTEGER(dims)[4];
   dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
   if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
       ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
     return;
     
   minquant(i8,i7) = 0;
   minyr(i8,i7)    = 0;
   maxquant(i8,i7) = (int)dim[0] -1;
   maxyr(i8,i7)    = (int)dim[1] -1;
   nunits(i8,i7)   = (int)dim[2];
   nseasons(i8,i7) = (int)dim[3];
   nareas(i8,i7)   = (int)dim[4]; 
   niters(i8,i7)   = (int)dim[5];
	   
      
   if (dimnames != R_NilValue) 
     if (TYPEOF(dimnames) == VECSXP) 
        {
        int  t = 0;
        const char *c;
        
        if (n >= 1 && INTEGER(dims)[0] >= 1) 
           {
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));
            //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;
            if (t !=1)
            t = atoi(c); 
            minquant(i8,i7) += t;
            maxquant(i8,i7) += t;
 	         }
	   
        if (n >= 2 && INTEGER(dims)[1] >= 1) 
           {
           t = 0;
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));
           //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;

            if (t !=1)
            t = atoi(c); 
            
            minyr(i8,i7)   += t;
            maxyr(i8,i7)   += t;
 	      	}
	    }


   alloc(i8, i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i8,i7); iUnit++)
		         for (iYear = minyr(i8,i7); iYear <= maxyr(i8,i7); iYear++)
			   		   for (iAge = minquant(i8,i7); iAge <= maxquant(i8,i7); iAge++)
			       			   data[i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                   
    
	}
         
void FLQuant3::Init(int i8, int i7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)      
   {
   if (i8<1 || i8>flq_n8) 
      return;

   if (i7<1 || i7>flq_n7[i8]) 
      return;

   if (InitFlag(i8, i7)) 
      unalloc(i8, i7);

   minquant(i8,i7) = _minquant;
   maxquant(i8,i7) = _maxquant;
   minyr(i8,i7)    = _minyr;
   maxyr(i8,i7)    = _maxyr;
   nunits(i8,i7)   = _nunits;
   nareas(i8,i7)   = _nareas;
   nseasons(i8,i7) = _nseasons;
   niters(i8,i7)   = _niters;

   alloc(i8,i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i8,i7); iUnit++)
		         for (iYear = minyr(i8,i7); iYear <= maxyr(i8,i7); iYear++)
			   	   for (iAge = minquant(i8,i7); iAge <= maxquant(i8,i7); iAge++)
			      	   data[i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

double& FLQuant3::operator()(int i8, int i7, int _age,int _yr,int _unit, int _season, int _area, int _iter) { 
   if (i8<1 || i8 > flq_n8     ||
       i7<1 || i7 > flq_n7[i8] ||
       !InitFlag(i8,i7)        || 
       _age   <minquant(i8,i7) || _age   >maxquant(i8,i7) || 
       _yr    <minyr(i8,i7)    || _yr    >maxyr(i8,i7)    || 
       _unit  <1               || _unit  >nunits(i8,i7)   ||
       _season<1               || _season>nseasons(i8,i7) || 
       _area  <1               || _area  >nareas(i8,i7)) 
      return outofbounds_double;
   else
      return (data)[i8][i7][_age][_yr][_unit][_season][_area][__min(_iter,niters(i8,i7))]; 
   } 


SEXP FLQuant3::Return(int i8, int i7)      
    {
    if (i8<0 || i8>flq_n8) 
       return R_NilValue;

    if (i7<0 || i7>flq_n7[i8]) 
       return R_NilValue;

    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant(i8,i7)-minquant(i8,i7) +1;
    INTEGER(dim)[1] = maxyr(i8,i7)   -minyr(i8,i7)    +1;
    INTEGER(dim)[2] = nunits(i8,i7); 
    INTEGER(dim)[3] = nseasons(i8,i7); 
    INTEGER(dim)[4] = nareas(i8,i7); 
    INTEGER(dim)[5] = niters(i8,i7);
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant(i8,i7)-minquant(i8,i7) +1));
    for (iAge=minquant(i8,i7), i=0; iAge<=maxquant(i8,i7); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr(i8,i7)-minyr(i8,i7)+1));
    for (iYear=minyr(i8,i7), i=0; iYear<=maxyr(i8,i7); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits(i8,i7)==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits(i8,i7)));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits(i8,i7)));
       for (iUnit=1, i=0; iUnit<=nunits(i8,i7); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons(i8,i7)==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons(i8,i7)));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons(i8,i7)));
       for (iSeason=1, i=0; iSeason<=nseasons(i8,i7); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas(i8,i7)==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas(i8,i7)));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas(i8,i7)));
       for (iArea=1, i=0; iArea<=nareas(i8,i7); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters(i8,i7)));
    for (iIter=1, i=0; iIter<=niters(i8,i7); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 
 
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(i8,i7); iIter++)
	    for (iArea = 1; iArea <= nareas(i8,i7); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(i8,i7); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(i8,i7); iUnit++)
	    		    for (iYear = minyr(i8,i7); iYear <= maxyr(i8,i7); iYear++)
			 		    for (iAge = minquant(i8,i7); iAge <= maxquant(i8,i7); iAge++)
			      			    REAL(v)[i++] = data[i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

SEXP FLQuant3::Return(void)
   {
   SEXP ReturnObject, ReturnObject2;

   PROTECT(ReturnObject = allocVector(VECSXP,flq_n8));
   for (int i=1; i<=flq_n8; i++)
       {
       PROTECT(ReturnObject2 = allocVector(VECSXP,flq_n7[i]));

       for (int j=1; j<=flq_n7[i]; j++)
         SET_VECTOR_ELT(ReturnObject2, j-1, Return(i, j));

       SET_VECTOR_ELT(ReturnObject, i-1, ReturnObject2);
       
	   UNPROTECT(1);
	   }

   UNPROTECT(1);

   return ReturnObject;
   }

void FLQuant3::alloc(int i8, int i7)      
   {
    if (i8<0 || i8>flq_n8) 
       return;

    if (i7<0 || i7>flq_n7[i8]) 
       return;
 
   (data)[i8][i7] = new double*****[maxquant(i8,i7)-minquant(i8,i7)+1] - minquant(i8,i7);
   for(int i=minquant(i8,i7);i<=maxquant(i8,i7);i++) 
      {
      (data)[i8][i7][i] = new double****[maxyr(i8,i7)-minyr(i8,i7)+1] - minyr(i8,i7);
      for (int j=minyr(i8,i7); j<=maxyr(i8,i7); j++)
         {
         (data)[i8][i7][i][j] = new double***[nunits(i8,i7)] - 1;
         for (int k = 1; k<=nunits(i8,i7); k++)
            {
            (data)[i8][i7][i][j][k] = new double**[nseasons(i8,i7)] - 1;
            for (int l=1; l<=nseasons(i8,i7); l++)
	             {
                (data)[i8][i7][i][j][k][l] = new double*[nareas(i8,i7)] - 1;
	             for (int m=1; m<=nareas(i8,i7); m++)
      		       (data)[i8][i7][i][j][k][l][m] = new double[niters(i8,i7)] - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag(i8,i7)=true;
   }

void FLQuant3::unalloc(int i8, int i7)      
   {
   if (i8<1 || i8>flq_n8 || i7<1 || i7>flq_n7[i8] || !InitFlag(i8,i7)) 
      return;
   
   for (int iAge  = minquant(i8,i7); iAge <= maxquant(i8,i7); iAge++)
      {
      for (int iYear  = minyr(i8,i7); iYear  <= maxyr(i8,i7);   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits(i8,i7);  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(i8,i7); iSeason++) 
       		   {
	             for (int iArea = 1; iArea <= nareas(i8,i7); iArea++) 
                   delete [] (data[i8][i7][iAge][iYear][iUnit][iSeason][iArea]+1);
  
         	   delete [] (data[i8][i7][iAge][iYear][iUnit][iSeason]+1);
         	   }
            delete [] (data[i8][i7][iAge][iYear][iUnit]+1);
            }
         delete [] (data[i8][i7][iAge][iYear]+1);
         }
      delete [] (data[i8][i7][iAge]+minyr(i8,i7));
      }
   delete [] (data[i8][i7]+minquant(i8,i7));
   }                    

void FLQuant3::unalloc(void)      
   {
   for (int i=1; i<=flq_n8; i++)
	  {
	  for (int j=1; j<=flq_n7[i]; j++)
	     if (InitFlag(i,j)) 
           unalloc(i,j);

	  unalloc_n7(i); 
	  }

   unalloc_n8(); 	  
   }                               

FLQuant3::~FLQuant3(void)      
   {
   unalloc(); 
   }                               

int& FLQuant3::minquant(int i8, int i7)
   {
   return flq_minquant[i8][i7];
   }

int& FLQuant3::maxquant(int i8, int i7)
   {
   return flq_maxquant[i8][i7];
   }

int& FLQuant3::minyr(int i8, int i7)
   {
   return flq_minyr[i8][i7];
   }

int& FLQuant3::maxyr(int i8, int i7)
   {
   return flq_maxyr[i8][i7];
   }

int& FLQuant3::nunits(int i8, int i7)
   {
   return flq_nunits[i8][i7];
   }

int& FLQuant3::nseasons(int i8, int i7)
   {
   return flq_nseasons[i8][i7];
   }

int& FLQuant3::nareas(int i8, int i7)
   {
   return flq_nareas[i8][i7];
   }
 
int& FLQuant3::niters(int i8, int i7)
   {
   return flq_niters[i8][i7];
   }

int& FLQuant3::n7(int i8)
{
   return flq_n7[i8];
}

int& FLQuant3::n8()
{
   return flq_n8;
}

bool& FLQuant3::InitFlag(int i8, int i7)
   {
   return flq_InitFlag[i8][i7];
   }

FLQuant4::FLQuant4(void)      
    {
    flq_n9 = 0;
    }

FLQuant4::FLQuant4(int n9, int n8, int n7)  
    {
	 if (n8<1 || n7<1)
       return;

    alloc(n9, n8, n7);
    }

FLQuant4::FLQuant4(int n9, int n8, int n7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)
   {
   if (n9<1 || n8<1 || n7<1)
       return;

   Init(n9, n8, n7,_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val);
   }

void FLQuant4::alloc_n9(int n9)
   {
   if (n9 <1) 
      return;
   
   //if (flq_n9>0)
   //   {
   //   for (int i=1; i<=flq_n9; i++)
   //      {
	//	 for (int j=1; j<=flq_n8[i]; j++)
  	//	    {
	//		for (int k=1; k<=flq_n7[i][j]; k++)
  	//			if  (InitFlag(i,j,k))
	//			   unalloc(i,j,k);
   //
   //         unalloc_n7(i,j);
	//		}
   //      unalloc_n8(i);
	//	 }
   //   unalloc_n9();
   //   }

   flq_n9 = n9;
 
   flq_n8       = new int[flq_n9]   - 1; 
   flq_n7       = new int*[flq_n9]   - 1; 
   flq_minquant = new int**[flq_n9]  - 1; 
   flq_maxquant = new int**[flq_n9]  - 1; 
   flq_minyr    = new int**[flq_n9]  - 1;     
   flq_maxyr    = new int**[flq_n9]  - 1; 
   flq_nunits   = new int**[flq_n9]  - 1; 
   flq_nseasons = new int**[flq_n9]  - 1; 
   flq_nareas   = new int**[flq_n9]  - 1; 
   flq_niters   = new int**[flq_n9]  - 1; 
   flq_InitFlag = new bool**[flq_n9] - 1; 

   data = new double********[flq_n9] - 1; ;   
   }

void FLQuant4::alloc_n8(int i9, int n8)
   {
   if (i9 <1 || i9>flq_n9) 
      return;

   if (n8 <1) 
      return;
   
   //if (flq_n8[i9]>0)
   //   {
   //   for (int i=1; i<=flq_n8[i9]; i++)
   //      {
	//	   for (int j=1; j<=flq_n7[i9][i]; j++)
  	//	    if  (InitFlag(i9, i,j))
	//			unalloc(i9, i,j);
   //
   //      unalloc_n7(i9,i);
	//	   }
   //
   //   unalloc_n8(i9);
   //   }

   flq_n8[i9] = n8;
 
   flq_n7[i9]       = new int[flq_n8[i9]]   - 1; 
   flq_minquant[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_maxquant[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_minyr[i9]    = new int*[flq_n8[i9]]  - 1;     
   flq_maxyr[i9]    = new int*[flq_n8[i9]]  - 1; 
   flq_nunits[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_nseasons[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_nareas[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_niters[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_InitFlag[i9] = new bool*[flq_n8[i9]] - 1; 

   data[i9] = new double*******[flq_n8[i9]] - 1; ;   
   }

void FLQuant4::alloc_n7(int i9, int i8, int n7)
   {
   if (i9 <1 || i9>flq_n9) 
      return;

   if (i8 <1 || i8>flq_n8[i9]) 
      return;
   
   //if (flq_n7[i9][i8]>0)
   //   {
   //	  for (int i=1; i<=flq_n7[i9][i8]; i++)
  	//    if (InitFlag(i9,i8,i))
	//       unalloc(i9,i8,i);
   //
   //    unalloc_n7(i9,i8);
	//   }

   flq_n7[i9][i8] = n7;
 
   flq_minquant[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_maxquant[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_minyr[i9][i8]    = new int[flq_n7[i9][i8]] - 1;     
   flq_maxyr[i9][i8]    = new int[flq_n7[i9][i8]] - 1; 
   flq_nunits[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_nseasons[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_nareas[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_niters[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_InitFlag[i9][i8] = new bool[flq_n7[i9][i8]] - 1; 

   data[i9][i8] = new double******[flq_n7[i9][i8]] - 1; 

   for (int j=1; j<=flq_n7[i9][i8]; j++)
      flq_InitFlag[i9][i8][j]=FALSE;
   }

void FLQuant4::unalloc_n9(void)
   {
   if (flq_n9 <1) 
       return;
   
   delete[]  (flq_n8+1);
   delete[]  (flq_n7+1);
   delete[]  (flq_minquant+1);
   delete[]  (flq_maxquant+1);
   delete[]  (flq_minyr   +1);
   delete[]  (flq_maxyr   +1);
   delete[]  (flq_nunits  +1); 
   delete[]  (flq_nseasons+1); 
   delete[]  (flq_nareas  +1); 
   delete[]  (flq_niters  +1); 
   delete[]  (flq_InitFlag+1); 
   
   delete[]  (data+1);

   flq_n9 = 0;
   }

void FLQuant4::unalloc_n8(int i9)
   {
   if (i9 <1) 
       return;

   if (flq_n8[i9] <1) 
       return;
   
   delete[]  (flq_n7[i9]      +1);
   delete[]  (flq_minquant[i9]+1);
   delete[]  (flq_maxquant[i9]+1);
   delete[]  (flq_minyr[i9]   +1);
   delete[]  (flq_maxyr[i9]   +1);
   delete[]  (flq_nunits[i9]  +1); 
   delete[]  (flq_nseasons[i9]+1); 
   delete[]  (flq_nareas[i9]  +1); 
   delete[]  (flq_niters[i9]  +1); 
   delete[]  (flq_InitFlag[i9]+1); 
   
   delete[]  (data[i9]+1);

   flq_n8[i9] = 0;
   }

void FLQuant4::unalloc_n7(int i9, int i8)
   {
   if (i9<1 || i9>flq_n9) 
       return;

   if (i8<1 || i8>flq_n8[i9]) 
       return;
   
   delete[]  (flq_minquant[i9][i8]+1);
   delete[]  (flq_maxquant[i9][i8]+1);
   delete[]  (flq_minyr[i9][i8]   +1);
   delete[]  (flq_maxyr[i9][i8]   +1);
   delete[]  (flq_nunits[i9][i8]  +1); 
   delete[]  (flq_nseasons[i9][i8]+1); 
   delete[]  (flq_nareas[i9][i8]  +1); 
   delete[]  (flq_niters[i9][i8]  +1); 
   delete[]  (flq_InitFlag[i9][i8]+1); 
   
   delete[]  (data[i9][i8]+1);

   flq_n7[i9][i8] = 0;
   }

void FLQuant4::Init(int i9, int i8, int i7, SEXP x)
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (!isFLQuant(x)) 
      return;

   if (InitFlag(i9, i8, i7)) 
      unalloc(i9, i8, i7);

   //PROTECT(duplicate(

   SEXP Quant    = GET_SLOT(x, install(".Data")),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

   double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

   int dim[6], n = length(dims);

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];
   dim[3] = INTEGER(dims)[3];
   dim[4] = INTEGER(dims)[4];
   dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
   if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
       ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
     return;
   
   minquant(i9,i8,i7) = 0;
   minyr(i9,i8,i7)    = 0;
   maxquant(i9,i8,i7) = (int)dim[0] -1;
   maxyr(i9,i8,i7)    = (int)dim[1] -1;
   nunits(i9,i8,i7)   = (int)dim[2];
   nseasons(i9,i8,i7) = (int)dim[3];
   nareas(i9,i8,i7)   = (int)dim[4]; 
   niters(i9,i8,i7)   = (int)dim[5];
	   
   if (dimnames != R_NilValue) 
     if (TYPEOF(dimnames) == VECSXP) 
        {
        int  t = 0;
        const char *c;
        
        if (n >= 1 && INTEGER(dims)[0] >= 1) 
           {
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));
            //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;
            if (t !=1)
            t = atoi(c); 
            minquant(i9,i8,i7) += t;
            maxquant(i9,i8,i7) += t;
 	         }
	   
        if (n >= 2 && INTEGER(dims)[1] >= 1) 
           {
           t = 0;
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));
           //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;

            if (t !=1)
            t = atoi(c); 
            
            minyr(i9,i8,i7)   += t;
            maxyr(i9,i8,i7)   += t;
 	      	}
	    }


   alloc(i9, i8, i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
		         for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			   		   for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			       			   data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                   
    }
         
void FLQuant4::Init(int i9, int i8, int i7, int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)      
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (InitFlag(i9, i8, i7)) 
      unalloc(i9, i8, i7);

   minquant(i9,i8,i7) = _minquant;
   maxquant(i9,i8,i7) = _maxquant;
   minyr(i9,i8,i7)    = _minyr;
   maxyr(i9,i8,i7)    = _maxyr;
   nunits(i9,i8,i7)   = _nunits;
   nareas(i9,i8,i7)   = _nareas;
   nseasons(i9,i8,i7) = _nseasons;
   niters(i9,i8,i7)   = _niters;

   alloc(i9,i8,i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
		         for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			   	   for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			      	   data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

double& FLQuant4::operator()(int i9, int i8, int i7, int _age,int _yr,int _unit, int _season, int _area, int _iter) { 
   if (i9<1 || i9 > flq_n9         ||
       i8<1 || i8 > flq_n8[i9]     ||
       i7<1 || i7 > flq_n7[i9][i8] ||
       !InitFlag(i9,i8,i7)         || 
       _age   <minquant(i9,i8,i7)  || _age   >maxquant(i9,i8,i7) || 
       _yr    <minyr(i9,i8,i7)     || _yr    >maxyr(i9,i8,i7)    || 
       _unit  <1                   || _unit  >nunits(i9,i8,i7)   ||
       _season<1                   || _season>nseasons(i9,i8,i7) || 
       _area  <1                   || _area  >nareas(i9,i8,i7)) 
      return outofbounds_double;
   else
      return (data)[i9][i8][i7][_age][_yr][_unit][_season][_area][__min(_iter,niters(i9,i8,i7))]; 
   } 

SEXP FLQuant4::Return(int i9, int i8, int i7)      
    {
    if (i9<0 || i9>flq_n9) 
       return R_NilValue;

    if (i8<0 || i8>flq_n8[i9]) 
       return R_NilValue;

    if (i7<0 || i7>flq_n7[i9][i8]) 
       return R_NilValue;

    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant(i9,i8,i7)-minquant(i9,i8,i7) +1;
    INTEGER(dim)[1] = maxyr(i9,i8,i7)   -minyr(i9,i8,i7)    +1;
    INTEGER(dim)[2] = nunits(i9,i8,i7); 
    INTEGER(dim)[3] = nseasons(i9,i8,i7); 
    INTEGER(dim)[4] = nareas(i9,i8,i7); 
    INTEGER(dim)[5] = niters(i9,i8,i7);
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant(i9,i8,i7)-minquant(i9,i8,i7) +1));
    for (iAge=minquant(i9,i8,i7), i=0; iAge<=maxquant(i9,i8,i7); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr(i9,i8,i7)-minyr(i9,i8,i7)+1));
    for (iYear=minyr(i9,i8,i7), i=0; iYear<=maxyr(i9,i8,i7); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits(i9,i8,i7)==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits(i9,i8,i7)));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits(i9,i8,i7)));
       for (iUnit=1, i=0; iUnit<=nunits(i9,i8,i7); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons(i9,i8,i7)==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons(i9,i8,i7)));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons(i9,i8,i7)));
       for (iSeason=1, i=0; iSeason<=nseasons(i9,i8,i7); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas(i9,i8,i7)==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas(i9,i8,i7)));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas(i9,i8,i7)));
       for (iArea=1, i=0; iArea<=nareas(i9,i8,i7); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters(i9,i8,i7)));
    for (iIter=1, i=0; iIter<=niters(i9,i8,i7); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 
 
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	    for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
	    		    for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			 		    for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			      			    REAL(v)[i++] = data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

SEXP FLQuant4::Return(void)
   {
   SEXP ReturnObject, ReturnObject2, ReturnObject3;

   PROTECT(ReturnObject = allocVector(VECSXP,flq_n9));
   for (int i=1; i<=flq_n9; i++)
       {
       PROTECT(ReturnObject2 = allocVector(VECSXP,flq_n8[i]));

       for (int j=1; j<=flq_n8[i]; j++)
         {
	     PROTECT(ReturnObject3 = allocVector(VECSXP,flq_n7[i][j]));

         for (int k=1; k<=flq_n7[i][j]; k++)
         	 SET_VECTOR_ELT(ReturnObject3, k-1, Return(i, j, k));

       	 SET_VECTOR_ELT(ReturnObject2, j-1, ReturnObject3);
         
		 UNPROTECT(1);
		 }
       
       SET_VECTOR_ELT(ReturnObject, i-1, ReturnObject2);
	   UNPROTECT(1);
	   }
   UNPROTECT(1);

   return ReturnObject;
   }

void FLQuant4::alloc(int i9, int i8, int i7)      
   {
   if (i9<1 || i9>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   (data)[i9][i8][i7] = new double*****[maxquant(i9,i8,i7)-minquant(i9,i8,i7)+1] - minquant(i9,i8,i7);
   for(int i=minquant(i9,i8,i7);i<=maxquant(i9,i8,i7);i++) 
      {
      (data)[i9][i8][i7][i] = new double****[maxyr(i9,i8,i7)-minyr(i9,i8,i7)+1] - minyr(i9,i8,i7);
      for (int j=minyr(i9,i8,i7); j<=maxyr(i9,i8,i7); j++)
         {
         (data)[i9][i8][i7][i][j] = new double***[nunits(i9,i8,i7)] - 1;
         for (int k = 1; k<=nunits(i9,i8,i7); k++)
            {
            (data)[i9][i8][i7][i][j][k] = new double**[nseasons(i9,i8,i7)] - 1;
            for (int l=1; l<=nseasons(i9,i8,i7); l++)
	             {
                (data)[i9][i8][i7][i][j][k][l] = new double*[nareas(i9,i8,i7)] - 1;
	             for (int m=1; m<=nareas(i9,i8,i7); m++)
      		       (data)[i9][i8][i7][i][j][k][l][m] = new double[niters(i9,i8,i7)] - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag(i9,i8,i7)=true;
   }

void FLQuant4::unalloc(int i9, int i8, int i7)      
   {
   if (i9<1 || i9>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (!InitFlag(i9,i8,i7)) 
      return;
   
   for (int iAge  = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
      {
      for (int iYear  = minyr(i9,i8,i7); iYear  <= maxyr(i9,i8,i7);   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits(i9,i8,i7);  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(i9,i8,i7); iSeason++) 
       		   {
	             for (int iArea = 1; iArea <= nareas(i9,i8,i7); iArea++) 
                   delete [] (data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea]+1);
  
         	   delete [] (data[i9][i8][i7][iAge][iYear][iUnit][iSeason]+1);
         	   }
            delete [] (data[i9][i8][i7][iAge][iYear][iUnit]+1);
            }
         delete [] (data[i9][i8][i7][iAge][iYear]+1);
         }
      delete [] (data[i9][i8][i7][iAge]+minyr(i9,i8,i7));
      }
   delete [] (data[i9][i8][i7]+minquant(i9,i8,i7));
   }                    

void FLQuant4::unalloc(void)      
   {
   for (int i=1; i<=flq_n9; i++)
	  {
	  for (int j=1; j<=flq_n8[i]; j++)
		 {
		 for (int k=1; k<=flq_n7[i][j]; k++)
	        if (InitFlag(i,j,k)) 
               unalloc(i,j,k);

	     unalloc_n7(i,j); 
	     }
      unalloc_n8(i); 	  
      }
   unalloc_n9(); 	  
   }                               

FLQuant4::~FLQuant4(void)      
   {
   unalloc(); 
   }                               

int& FLQuant4::minquant(int i9, int i8, int i7)
   {
   return flq_minquant[i9][i8][i7];
   }

int& FLQuant4::maxquant(int i9, int i8, int i7)
   {
   return flq_maxquant[i9][i8][i7];
   }

int& FLQuant4::minyr(int i9, int i8, int i7)
   {
   return flq_minyr[i9][i8][i7];
   }

int& FLQuant4::maxyr(int i9, int i8, int i7)
   {
   return flq_maxyr[i9][i8][i7];
   }

int& FLQuant4::nunits(int i9, int i8, int i7)
   {
   return flq_nunits[i9][i8][i7];
   }

int& FLQuant4::nseasons(int i9, int i8, int i7)
   {
   return flq_nseasons[i9][i8][i7];
   }

int& FLQuant4::nareas(int i9, int i8, int i7)
   {
   return flq_nareas[i9][i8][i7];
   }
 
int& FLQuant4::niters(int i9, int i8, int i7)
   {
   return flq_niters[i9][i8][i7];
   }

bool& FLQuant4::InitFlag(int i9, int i8, int i7)
   {
   return flq_InitFlag[i9][i8][i7];
   }

_FLQuant::_FLQuant(void)      
    {
    flq_n9 = 0;
    }

_FLQuant::_FLQuant(int n7, int n8, int n9)  
    {
	 if (n8<1 || n7<1)
       return;

    alloc(n9, n8, n7);
    }

_FLQuant::_FLQuant(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val,int n7, int n8, int n9)
   {
   if (n9<1 || n8<1 || n7<1)
       return;

   Init(_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val, n7, n8, n9);
   }

void _FLQuant::alloc_n9(int n9)
   {
   if (n9 <1) 
      return;
   
   //if (flq_n9>0)
   //   {
   //   for (int i=1; i<=flq_n9; i++)
   //      {
	//	 for (int j=1; j<=flq_n8[i]; j++)
  	//	    {
	//		for (int k=1; k<=flq_n7[i][j]; k++)
  	//			if  (InitFlag(i,j,k))
 	//			   unalloc(i,j,k);
   //
   //         unalloc_n7(i,j);
	//		}
   //      unalloc_n8(i);
	//	 }
   //   unalloc_n9();
   //   }

   flq_n9 = n9;
 
   flq_n8       = new int[flq_n9]   - 1; 
   flq_n7       = new int*[flq_n9]   - 1; 
   flq_minquant = new int**[flq_n9]  - 1; 
   flq_maxquant = new int**[flq_n9]  - 1; 
   flq_minyr    = new int**[flq_n9]  - 1;     
   flq_maxyr    = new int**[flq_n9]  - 1; 
   flq_nunits   = new int**[flq_n9]  - 1; 
   flq_nseasons = new int**[flq_n9]  - 1; 
   flq_nareas   = new int**[flq_n9]  - 1; 
   flq_niters   = new int**[flq_n9]  - 1; 
   flq_InitFlag = new bool**[flq_n9] - 1; 

   data = new double********[flq_n9] - 1; ;   
   }

void _FLQuant::alloc_n8(int i9, int n8)
   {
   if (i9 <1 || i9>flq_n9) 
      return;

   if (n8 <1) 
      return;
   
   //if (flq_n8[i9]>0)
   //   {
   //   for (int i=1; i<=flq_n8[i9]; i++)
   //      {
	//	 for (int j=1; j<=flq_n7[i9][i]; j++)
  	//	    if  (InitFlag(i9, i,j))
	//			unalloc(i9, i,j);
   //
   //      unalloc_n7(i9,i);
	//	 }
   //
   //   unalloc_n8(i9);
   //   }

   flq_n8[i9] = n8;
 
   flq_n7[i9]       = new int[flq_n8[i9]]   - 1; 
   flq_minquant[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_maxquant[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_minyr[i9]    = new int*[flq_n8[i9]]  - 1;     
   flq_maxyr[i9]    = new int*[flq_n8[i9]]  - 1; 
   flq_nunits[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_nseasons[i9] = new int*[flq_n8[i9]]  - 1; 
   flq_nareas[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_niters[i9]   = new int*[flq_n8[i9]]  - 1; 
   flq_InitFlag[i9] = new bool*[flq_n8[i9]] - 1; 

   data[i9] = new double*******[flq_n8[i9]] - 1; ;   
   }

void _FLQuant::alloc_n7(int i9, int i8, int n7)
   {
   if (i9 <1 || i9>flq_n9) 
      return;

   if (i8 <1 || i8>flq_n8[i9]) 
      return;
   
   //if (flq_n7[i9][i8]>0)
   //   {
   //	  for (int i=1; i<=flq_n7[i9][i8]; i++)
  	//    if (InitFlag(i9,i8,i))
	//       unalloc(i9,i8,i);
   //
   //    unalloc_n7(i9,i8);
	//   }

   flq_n7[i9][i8] = n7;
 
   flq_minquant[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_maxquant[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_minyr[i9][i8]    = new int[flq_n7[i9][i8]] - 1;     
   flq_maxyr[i9][i8]    = new int[flq_n7[i9][i8]] - 1; 
   flq_nunits[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_nseasons[i9][i8] = new int[flq_n7[i9][i8]] - 1; 
   flq_nareas[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_niters[i9][i8]   = new int[flq_n7[i9][i8]] - 1; 
   flq_InitFlag[i9][i8] = new bool[flq_n7[i9][i8]] - 1; 

   data[i9][i8] = new double******[flq_n7[i9][i8]] - 1; 

   for (int j=1; j<=flq_n7[i9][i8]; j++)
      flq_InitFlag[i9][i8][j]=FALSE;
   }

void _FLQuant::unalloc_n9(void)
   {
   if (flq_n9 <1) 
       return;
   
   delete[]  (flq_n8+1);
   delete[]  (flq_n7+1);
   delete[]  (flq_minquant+1);
   delete[]  (flq_maxquant+1);
   delete[]  (flq_minyr   +1);
   delete[]  (flq_maxyr   +1);
   delete[]  (flq_nunits  +1); 
   delete[]  (flq_nseasons+1); 
   delete[]  (flq_nareas  +1); 
   delete[]  (flq_niters  +1); 
   delete[]  (flq_InitFlag+1); 
   
   delete[]  (data+1);

   flq_n9 = 0;
   }

void _FLQuant::unalloc_n8(int i9)
   {
   if (i9 <1) 
       return;

   if (flq_n8[i9] <1) 
       return;
   
   delete[]  (flq_n7[i9]      +1);
   delete[]  (flq_minquant[i9]+1);
   delete[]  (flq_maxquant[i9]+1);
   delete[]  (flq_minyr[i9]   +1);
   delete[]  (flq_maxyr[i9]   +1);
   delete[]  (flq_nunits[i9]  +1); 
   delete[]  (flq_nseasons[i9]+1); 
   delete[]  (flq_nareas[i9]  +1); 
   delete[]  (flq_niters[i9]  +1); 
   delete[]  (flq_InitFlag[i9]+1); 
   
   delete[]  (data[i9]+1);

   flq_n8[i9] = 0;
   }

void _FLQuant::unalloc_n7(int i9, int i8)
   {
   if (i9<1 || i9>flq_n9) 
       return;

   if (i8<1 || i8>flq_n8[i9]) 
       return;
   
   delete[]  (flq_minquant[i9][i8]+1);
   delete[]  (flq_maxquant[i9][i8]+1);
   delete[]  (flq_minyr[i9][i8]   +1);
   delete[]  (flq_maxyr[i9][i8]   +1);
   delete[]  (flq_nunits[i9][i8]  +1); 
   delete[]  (flq_nseasons[i9][i8]+1); 
   delete[]  (flq_nareas[i9][i8]  +1); 
   delete[]  (flq_niters[i9][i8]  +1); 
   delete[]  (flq_InitFlag[i9][i8]+1); 
   
   delete[]  (data[i9][i8]+1);

   flq_n7[i9][i8] = 0;
   }

void _FLQuant::Init(int i9, int i8, int i7, SEXP x)
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (!isFLQuant(x)) 
      return;

   if (InitFlag(i9, i8, i7)) 
      unalloc(i9, i8, i7);

   SEXP Quant    = GET_SLOT(x, install(".Data")),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

   double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

   int dim[6], n = length(dims);

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];
   dim[3] = INTEGER(dims)[3];
   dim[4] = INTEGER(dims)[4];
   dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
   if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
       ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
     return;

   minquant(i9,i8,i7) = 0;
   minyr(i9,i8,i7)    = 0;
   maxquant(i9,i8,i7) = (int)dim[0] -1;
   maxyr(i9,i8,i7)    = (int)dim[1] -1;
   nunits(i9,i8,i7)   = (int)dim[2];
   nseasons(i9,i8,i7) = (int)dim[3];
   nareas(i9,i8,i7)   = (int)dim[4]; 
   niters(i9,i8,i7)   = (int)dim[5];
	   
      
   if (dimnames != R_NilValue) 
     if (TYPEOF(dimnames) == VECSXP) 
        {
        int  t = 0;
        const char *c;
        
        if (n >= 1 && INTEGER(dims)[0] >= 1) 
           {
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), 0));
            //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;
            if (t !=1)
            t = atoi(c); 
            minquant(i9,i8,i7) += t;
            maxquant(i9,i8,i7) += t;
 	         }
	   
        if (n >= 2 && INTEGER(dims)[1] >= 1) 
           {
           t = 0;
           c = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 1), 0));
           //check that name is not a text string
           for (int i=0; i<=(signed)strlen(c); i++)
              if (isalpha(c[i])) t=1;

            if (t !=1)
            t = atoi(c); 
            
            minyr(i9,i8,i7)   += t;
            maxyr(i9,i8,i7)   += t;
 	      	}
	    }


   alloc(i9, i8, i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
		         for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			   		   for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			       			   data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                   
    }
         
void _FLQuant::Init(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val, int i7, int i8, int i9)      
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (InitFlag(i9, i8, i7)) 
      unalloc(i9, i8, i7);

   minquant(i9,i8,i7) = _minquant;
   maxquant(i9,i8,i7) = _maxquant;
   minyr(i9,i8,i7)    = _minyr;
   maxyr(i9,i8,i7)    = _maxyr;
   nunits(i9,i8,i7)   = _nunits;
   nareas(i9,i8,i7)   = _nareas;
   nseasons(i9,i8,i7) = _nseasons;
   niters(i9,i8,i7)   = _niters;

   alloc(i9,i8,i7);

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	   for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
		         for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			   	   for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			      	   data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

double& _FLQuant::operator()(int _age,int _yr,int _unit, int _season, int _area, int _iter, int i7, int i8, int i9) { 
   if (i9<1 || i9 > flq_n9         ||
       i8<1 || i8 > flq_n8[i9]     ||
       i7<1 || i7 > flq_n7[i9][i8] ||
       !InitFlag(i9,i8,i7)         || 
       _age   <minquant(i9,i8,i7)  || _age   >maxquant(i9,i8,i7) || 
       _yr    <minyr(i9,i8,i7)     || _yr    >maxyr(i9,i8,i7)    || 
       _unit  <1                   || _unit  >nunits(i9,i8,i7)   ||
       _season<1                   || _season>nseasons(i9,i8,i7) || 
       _area  <1                   || _area  >nareas(i9,i8,i7)) 
      return outofbounds_double;
   else
      return (data)[i9][i8][i7][_age][_yr][_unit][_season][_area][_iter]; 
   } 


SEXP _FLQuant::Return(int i7, int i8, int i9)      
    {
    if (i9<0 || i9>flq_n9) 
       return R_NilValue;

    if (i8<0 || i8>flq_n8[i9]) 
       return R_NilValue;

    if (i7<0 || i7>flq_n7[i9][i8]) 
       return R_NilValue;

    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 6));       
    INTEGER(dim)[0] = maxquant(i9,i8,i7)-minquant(i9,i8,i7) +1;
    INTEGER(dim)[1] = maxyr(i9,i8,i7)   -minyr(i9,i8,i7)    +1;
    INTEGER(dim)[2] = nunits(i9,i8,i7); 
    INTEGER(dim)[3] = nseasons(i9,i8,i7); 
    INTEGER(dim)[4] = nareas(i9,i8,i7); 
    INTEGER(dim)[5] = niters(i9,i8,i7);
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 6));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant(i9,i8,i7)-minquant(i9,i8,i7) +1));
    for (iAge=minquant(i9,i8,i7), i=0; iAge<=maxquant(i9,i8,i7); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr(i9,i8,i7)-minyr(i9,i8,i7)+1));
    for (iYear=minyr(i9,i8,i7), i=0; iYear<=maxyr(i9,i8,i7); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits(i9,i8,i7)==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits(i9,i8,i7)));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits(i9,i8,i7)));
       for (iUnit=1, i=0; iUnit<=nunits(i9,i8,i7); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons(i9,i8,i7)==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons(i9,i8,i7)));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons(i9,i8,i7)));
       for (iSeason=1, i=0; iSeason<=nseasons(i9,i8,i7); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas(i9,i8,i7)==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas(i9,i8,i7)));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas(i9,i8,i7)));
       for (iArea=1, i=0; iArea<=nareas(i9,i8,i7); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

    PROTECT(d6 = allocVector(INTSXP, niters(i9,i8,i7)));
    for (iIter=1, i=0; iIter<=niters(i9,i8,i7); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
    SET_STRING_ELT(names, 5, mkChar("iter")); 
 
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(i9,i8,i7); iIter++)
	    for (iArea = 1; iArea <= nareas(i9,i8,i7); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(i9,i8,i7); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(i9,i8,i7); iUnit++)
	    		    for (iYear = minyr(i9,i8,i7); iYear <= maxyr(i9,i8,i7); iYear++)
			 		    for (iAge = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
			      			    REAL(v)[i++] = data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

SEXP _FLQuant::Return(void)
   {
   SEXP ReturnObject, ReturnObject2, ReturnObject3;

   PROTECT(ReturnObject = allocVector(VECSXP,flq_n9));
   for (int i=1; i<=flq_n9; i++)
       {
       PROTECT(ReturnObject2 = allocVector(VECSXP,flq_n8[i]));

       for (int j=1; j<=flq_n8[i]; j++)
         {
	     PROTECT(ReturnObject3 = allocVector(VECSXP,flq_n7[i][j]));

         for (int k=1; k<=flq_n7[i][j]; k++)
         	 SET_VECTOR_ELT(ReturnObject3, k-1, Return(i, j, k));

       	 SET_VECTOR_ELT(ReturnObject2, j-1, ReturnObject3);
         
		 UNPROTECT(1);
		 }
       
       SET_VECTOR_ELT(ReturnObject, i-1, ReturnObject2);
	   UNPROTECT(1);
	   }
   UNPROTECT(1);

   return ReturnObject;
   }


void _FLQuant::alloc(int i7, int i8, int i9)      
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   (data)[i9][i8][i7] = new double*****[maxquant(i9,i8,i7)-minquant(i9,i8,i7)+1] - minquant(i9,i8,i7);
   for(int i=minquant(i9,i8,i7);i<=maxquant(i9,i8,i7);i++) 
      {
      (data)[i9][i8][i7][i] = new double****[maxyr(i9,i8,i7)-minyr(i9,i8,i7)+1] - minyr(i9,i8,i7);
      for (int j=minyr(i9,i8,i7); j<=maxyr(i9,i8,i7); j++)
         {
         (data)[i9][i8][i7][i][j] = new double***[nunits(i9,i8,i7)] - 1;
         for (int k = 1; k<=nunits(i9,i8,i7); k++)
            {
            (data)[i9][i8][i7][i][j][k] = new double**[nseasons(i9,i8,i7)] - 1;
            for (int l=1; l<=nseasons(i9,i8,i7); l++)
	             {
                (data)[i9][i8][i7][i][j][k][l] = new double*[nareas(i9,i8,i7)] - 1;
	             for (int m=1; m<=nareas(i9,i8,i7); m++)
      		       (data)[i9][i8][i7][i][j][k][l][m] = new double[niters(i9,i8,i7)] - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag(i9,i8,i7)=true;
   }

void _FLQuant::unalloc(int i7, int i8, int i9)      
   {
   if (i9<1 || i8>flq_n9) 
      return;

   if (i8<1 || i8>flq_n8[i9]) 
      return;
   
   if (i7<1 || i7>flq_n7[i9][i8]) 
      return;

   if (!InitFlag(i9,i8,i7)) 
      return;
   
   for (int iAge  = minquant(i9,i8,i7); iAge <= maxquant(i9,i8,i7); iAge++)
      {
      for (int iYear  = minyr(i9,i8,i7); iYear  <= maxyr(i9,i8,i7);   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits(i9,i8,i7);  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(i9,i8,i7); iSeason++) 
       		   {
	             for (int iArea = 1; iArea <= nareas(i9,i8,i7); iArea++) 
                   delete [] (data[i9][i8][i7][iAge][iYear][iUnit][iSeason][iArea]+1);
  
         	   delete [] (data[i9][i8][i7][iAge][iYear][iUnit][iSeason]+1);
         	   }
            delete [] (data[i9][i8][i7][iAge][iYear][iUnit]+1);
            }
         delete [] (data[i9][i8][i7][iAge][iYear]+1);
         }
      delete [] (data[i9][i8][i7][iAge]+minyr(i9,i8,i7));
      }
   delete [] (data[i9][i8][i7]+minquant(i9,i8,i7));
   }                    

_FLQuant::~_FLQuant(void)      
   {
   for (int i=1; i<=flq_n9; i++)
	  {
	  for (int j=1; j<=flq_n8[i]; j++)
		 {
		 for (int k=1; k<=flq_n7[i][j]; k++)
	        if (InitFlag(i,j,k)) 
               unalloc(i,j,k);

	     unalloc_n7(i,j); 
	     }
      unalloc_n8(i); 	  
      }
   unalloc_n9(); 	  
   }                               

int& _FLQuant::minquant(int i7, int i8, int i9)
   {
   return flq_minquant[i9][i8][i7];
   }

int& _FLQuant::maxquant(int i7, int i8, int i9)
   {
   return flq_maxquant[i9][i8][i7];
   }

int& _FLQuant::minyr(int i7, int i8, int i9)
   {
   return flq_minyr[i9][i8][i7];
   }

int& _FLQuant::maxyr(int i7, int i8, int i9)
   {
   return flq_maxyr[i9][i8][i7];
   }

int& _FLQuant::nunits(int i7, int i8, int i9)
   {
   return flq_nunits[i9][i8][i7];
   }

int& _FLQuant::nseasons(int i7, int i8, int i9)
   {
   return flq_nseasons[i9][i8][i7];
   }

int& _FLQuant::nareas(int i7, int i8, int i9)
   {
   return flq_nareas[i9][i8][i7];
   }
 
int& _FLQuant::niters(int i7, int i8, int i9)
   {
   return flq_niters[i9][i8][i7];
   }

bool& _FLQuant::InitFlag(int i7, int i8, int i9)
   {
   return flq_InitFlag[i9][i8][i7];
   }

FLFleets::FLFleets(SEXP x)     
   {
   FLFleets::Init(x);
   }

bool FLFleets::Init(SEXP x)
   {
   if (!isFLFleets(x)) 
      return FALSE;

   nfleet = NElemList(x);

   fl_minquant = new int**[nfleet] - 1;
   fl_maxquant = new int**[nfleet] - 1;
   fl_plusgrp  = new int**[nfleet] - 1;
   fl_minyr    = new int**[nfleet] - 1;
   fl_maxyr    = new int**[nfleet] - 1;
   fl_nunits   = new int**[nfleet] - 1;
   fl_nseasons = new int**[nfleet] - 1;
   fl_nareas   = new int**[nfleet] - 1;
   fl_niters   = new int**[nfleet] - 1;

   fcost.alloc_n7(    nfleet);
   capacity.alloc_n7( nfleet);
   crewshare.alloc_n7(nfleet);

   effshare.alloc_n8( nfleet);
   vcost.alloc_n8(    nfleet);

   catch_.alloc_n9(      nfleet); 
   catch_n.alloc_n9(     nfleet); 
   catch_wt.alloc_n9(    nfleet); 
   catch_q.alloc_n9(     nfleet); 
   catch_sel.alloc_n9(   nfleet); 
   landings.alloc_n9(    nfleet); 
   landings_n.alloc_n9(  nfleet); 
   landings_wt.alloc_n9( nfleet); 
   landings_sel.alloc_n9(nfleet); 
   discards.alloc_n9(    nfleet); 
   discards_n.alloc_n9(  nfleet); 
   discards_wt.alloc_n9( nfleet); 
   discards_sel.alloc_n9(nfleet); 
   price.alloc_n9(       nfleet);

  for (int i=1; i<=nfleet; i++)
      {
      SEXP fleet   = PROTECT(VECTOR_ELT(x, i-1));
      SEXP metiers = PROTECT(GET_SLOT(fleet, install("metiers")));
      nmetier      = NElemList(metiers);
 
      fl_minquant[nfleet] = new int*[nmetier] - 1;
      fl_maxquant[nfleet] = new int*[nmetier] - 1;
      fl_plusgrp[nfleet]  = new int*[nmetier] - 1;
      fl_minyr[nfleet]    = new int*[nmetier] - 1;
      fl_maxyr[nfleet]    = new int*[nmetier] - 1;
      fl_nunits[nfleet]   = new int*[nmetier] - 1;
      fl_nseasons[nfleet] = new int*[nmetier] - 1;
      fl_nareas[nfleet]   = new int*[nmetier] - 1;
      fl_niters[nfleet]   = new int*[nmetier] - 1;

      effshare.alloc_n7(    i, nmetier);
      vcost.alloc_n7(       i, nmetier);

      catch_.alloc_n8(      i, nmetier); 
      catch_n.alloc_n8(     i, nmetier); 
      catch_wt.alloc_n8(    i, nmetier); 
      catch_q.alloc_n8(     i, nmetier); 
      catch_sel.alloc_n8(   i, nmetier); 
      landings.alloc_n8(    i, nmetier); 
      landings_n.alloc_n8(  i, nmetier); 
      landings_wt.alloc_n8( i, nmetier); 
      landings_sel.alloc_n8(i, nmetier); 
      discards.alloc_n8(    i, nmetier); 
      discards_n.alloc_n8(  i, nmetier); 
      discards_wt.alloc_n8( i, nmetier); 
      discards_sel.alloc_n8(i, nmetier); 
      price.alloc_n8(       i, nmetier);

      fcost.Init(    i,PROTECT(GET_SLOT(fleet,install("fcost"))));
      capacity.Init( i,PROTECT(GET_SLOT(fleet,install("capacity"))));
      crewshare.Init(i,PROTECT(GET_SLOT(fleet,install("crewshare"))));

      effshare.alloc_n7(i, nfleet);
      vcost.alloc_n7(   i, nfleet);

      for (int j=1; j<=nmetier; j++)
         {
         SEXP metier  = PROTECT(VECTOR_ELT(metiers, j-1));
         SEXP catches = PROTECT(GET_SLOT(metier, install("catches")));
         nspp         = NElemList(catches);
              
         fl_minquant[i][j] = new int[nspp] - 1;
         fl_maxquant[i][j] = new int[nspp] - 1;
         fl_plusgrp[i][j]  = new int[nspp] - 1;
         fl_minyr[i][j]    = new int[nspp] - 1;
         fl_maxyr[i][j]    = new int[nspp] - 1;
         fl_nunits[i][j]   = new int[nspp] - 1;
         fl_nseasons[i][j] = new int[nspp] - 1;
         fl_nareas[i][j]   = new int[nspp] - 1;
         fl_niters[i][j]   = new int[nspp] - 1;

         catch_.alloc_n7(      i, j, nspp); 
         catch_n.alloc_n7(     i, j, nspp); 
         catch_wt.alloc_n7(    i, j, nspp); 
         catch_q.alloc_n7(     i, j, nspp); 
         catch_sel.alloc_n7(   i, j, nspp); 
         landings.alloc_n7(    i, j, nspp); 
         landings_n.alloc_n7(  i, j, nspp); 
         landings_wt.alloc_n7( i, j, nspp); 
         landings_sel.alloc_n7(i, j, nspp); 
         discards.alloc_n7(    i, j, nspp); 
         discards_n.alloc_n7(  i, j, nspp); 
         discards_wt.alloc_n7( i, j, nspp); 
         discards_sel.alloc_n7(i, j, nspp); 
         price.alloc_n7(       i, j, nspp);

         effshare.Init(i,j,PROTECT(GET_SLOT(metier,install("effshare"))));
         vcost.Init(   i,j,PROTECT(GET_SLOT(metier,install("vcost"))));
         
         for (int k=1; k<=nspp; k++)
            {
            SEXP flc  = PROTECT(VECTOR_ELT(catches, k-1));
         
            catch_.Init(      i,j,k,PROTECT(GET_SLOT(flc,install("catch"))));
            catch_n.Init(     i,j,k,PROTECT(GET_SLOT(flc,install("catch.n"))));
            catch_wt.Init(    i,j,k,PROTECT(GET_SLOT(flc,install("catch.wt"))));
            catch_q.Init(     i,j,k,PROTECT(GET_SLOT(flc,install("catch.q"))));
            catch_sel.Init(   i,j,k,PROTECT(GET_SLOT(flc,install("catch.sel"))));
            landings.Init(    i,j,k,PROTECT(GET_SLOT(flc,install("landings"))));
            landings_n.Init(  i,j,k,PROTECT(GET_SLOT(flc,install("landings.n"))));
            landings_wt.Init( i,j,k,PROTECT(GET_SLOT(flc,install("landings.wt"))));
            landings_sel.Init(i,j,k,PROTECT(GET_SLOT(flc,install("landings.sel"))));
            discards.Init(    i,j,k,PROTECT(GET_SLOT(flc,install("discards"))));
            discards_n.Init(  i,j,k,PROTECT(GET_SLOT(flc,install("discards.n"))));
            discards_wt.Init( i,j,k,PROTECT(GET_SLOT(flc,install("discards.wt"))));
            discards_sel.Init(i,j,k,PROTECT(GET_SLOT(flc,install("discards.sel"))));
            price.Init(       i,j,k,PROTECT(GET_SLOT(flc,install("price"))));
            }
            UNPROTECT(nspp*2); 
         }
         UNPROTECT(nmetier*2); 
      }
      UNPROTECT(nfleet*2);  

   return true;
   }

SEXP FLFleets::Return(void)
   {
   SEXP ReturnObject;
   SEXP flfs   = PROTECT(NEW_OBJECT(MAKE_CLASS("FLFleets")));
   SEXP fleets = PROTECT(NEW_OBJECT(MAKE_CLASS("FLFleets")));
   
   int i, j, k;
   for (i=1; i<=nfleet; i++)
      {
      SEXP flf = allocVector(VECSXP,nmetier);
      SEXP flms = PROTECT(NEW_OBJECT(MAKE_CLASS("FLMetiers")));
      
      for (j=1; j<=nmetier; j++)
         {
         SEXP flm = allocVector(VECSXP,nspp);
         SEXP flcs   = PROTECT(NEW_OBJECT(MAKE_CLASS("FLCatches")));
            
         for (k=1; k<=nspp; k++)
            {
            SEXP Range  = PROTECT(NEW_NUMERIC(5));   
            SEXP flc = PROTECT(NEW_OBJECT(MAKE_CLASS("FLCatch")));
            
            REAL(Range)[0] = minquant(i,j,k);
            REAL(Range)[1] = maxquant(i,j,k);
            REAL(Range)[2] = plusgrp( i,j,k);
            REAL(Range)[3] = minyr(   i,j,k);
            REAL(Range)[4] = maxyr(   i,j,k);
       
            SET_SLOT(flc, install("range"), Range);

            SET_SLOT(flc, install("catch"),       catch_.Return(      i,j,k));
            SET_SLOT(flc, install("catch.n"),     catch_n.Return(     i,j,k));
            SET_SLOT(flc, install("catch.wt"),    catch_wt.Return(    i,j,k));
            SET_SLOT(flc, install("catch.q"),     catch_q.Return(     i,j,k));
            SET_SLOT(flc, install("catch.sel"),   catch_sel.Return(   i,j,k));
            SET_SLOT(flc, install("landings"),    landings.Return(    i,j,k));
            SET_SLOT(flc, install("landings.n"),  landings_n.Return(  i,j,k));
            SET_SLOT(flc, install("landings.wt"), landings_wt.Return( i,j,k));
            SET_SLOT(flc, install("landings.sel"),landings_sel.Return(i,j,k)); 
            SET_SLOT(flc, install("discards"),    discards.Return(    i,j,k));
            SET_SLOT(flc, install("discards.n"),  discards_n.Return(  i,j,k));
            SET_SLOT(flc, install("discards.wt"), discards_wt.Return( i,j,k));
            SET_SLOT(flc, install("discards.sel"),discards_sel.Return(i,j,k)); 
            SET_SLOT(flc, install("price"),       price.Return(       i,j,k));

return(flc);

            SET_VECTOR_ELT(flcs, k-1, flc);
   
            UNPROTECT(2);
            }
         SET_SLOT(flm, install("effshare"), effshare.Return(i,j));
         SET_SLOT(flm, install("vcos"),     vcost.Return(    i,j));
         
         SET_VECTOR_ELT(flms, j-1, flm);
         }
      SET_SLOT(flf, install("fcost"),     fcost.Return(     i));
      SET_SLOT(flf, install("capacity"),  capacity.Return(  i));
      SET_SLOT(flf, install("crewshare"), crewshare.Return(i));

      SET_VECTOR_ELT(flfs, i-1, flf);
      }

   ReturnObject = R_do_slot_assign(fleets, install(".Data"), flfs);
          
   UNPROTECT(1);
/*
   minquant()=minquant(1,1,1);
   maxquant()=maxquant(1,1,1);
   minyr()   =minyr(   1,1,1);   
   maxyr()   =maxyr(   1,1,1);   
   niters()  =niters(  1,1,1);  
   nareas()  =nareas(  1,1,1);  
   nseasons()=nseasons(1,1,1);
   nunits()  =nunits(  1,1,1);  

   for (i=1; i<=nfleet; i++)
      for (j=1; j<=nmetier; j++)
         for (k=1; k<=nspp; k++)
            {
            minquant()=__min( minquant(), minquant(i,j,k));
            maxquant()=__max( maxquant(), maxquant(i,j,k));
            minyr()   =__min( minyr()   , minyr(   i,j,k));   
            maxyr()   =__max( maxyr()   , maxyr(   i,j,k));   
            niters()  =__max( niters()  , niters(  i,j,k));  
            nareas()  =__max( nareas()  , nareas(  i,j,k));  
            nseasons()=__max( nseasons(), nseasons(i,j,k));
            nunits()  =__max( nunits()  , nunits(  i,j,k));  
            }
*/
   return  ReturnObject;
   }

int& FLFleets::minquant(int i9, int i8, int i7)
   {
   return fl_minquant[i9][i8][i7];
   }

int& FLFleets::maxquant(int i9, int i8, int i7)
   {
   return fl_maxquant[i9][i8][i7];
   }

int& FLFleets::plusgrp(int i9, int i8, int i7)
   {
   return fl_plusgrp[i9][i8][i7];
   }

int& FLFleets::minyr(int i9, int i8, int i7)
   {
   return fl_minyr[i9][i8][i7];
   }

int& FLFleets::maxyr(int i9, int i8, int i7)
   {
   return fl_maxyr[i9][i8][i7];
   }

int& FLFleets::nunits(int i9, int i8, int i7)
   {
   return fl_nunits[i9][i8][i7];
   }

int& FLFleets::nseasons(int i9, int i8, int i7)
   {
   return fl_nseasons[i9][i8][i7];
   }

int& FLFleets::nareas(int i9, int i8, int i7)
   {
   return fl_nareas[i9][i8][i7];
   }
 
int& FLFleets::niters(int i9, int i8, int i7)
   {
   return fl_niters[i9][i8][i7];
   }

int& FLFleets::minquant(void)
   {
   return fls_minquant;
   }

int& FLFleets::maxquant(void)
   {
   return fls_maxquant;
   }

int& FLFleets::plusgrp(void)
   {
   return fls_plusgrp;
   }

int& FLFleets::minyr(void)
   {
   return fls_minyr;
   }

int& FLFleets::maxyr(void)
   {
   return fls_maxyr;
   }

int& FLFleets::nunits(void)
   {
   return fls_nunits;
   }

int& FLFleets::nseasons(void)
   {
   return fls_nseasons;
   }

int& FLFleets::nareas(void)
   {
   return fls_nareas;
   }
 
int& FLFleets::niters(void)
   {
   return fls_niters;
   }

FLFleets::~FLFleets(void)     
   {
   ;
   }

FLBiols::FLBiols(void)      
    {
    nspp=0;
    }

FLBiols::FLBiols(SEXP x)      
    {
    nspp=0;

    if (isFLBiols(x))
       Init(x);
    }
   
void FLBiols::Init(SEXP x)
   {
   if (nspp>0) 
      unalloc();
   
   if (isFLBiols(x))
      nspp = NElemList(x);
   else
      return;

   if (nspp<1) 
      return;
      
   _minquant = new int[nspp] - 1;
   _minquant = new int[nspp] - 1;
   _maxquant = new int[nspp] - 1;
   _plusgrp  = new int[nspp] - 1;
   _minyr    = new int[nspp] - 1;
   _maxyr    = new int[nspp] - 1;
   _nunits   = new int[nspp] - 1;
   _nseasons = new int[nspp] - 1;
   _nareas   = new int[nspp] - 1;
   _niters   = new int[nspp] - 1;

   n.alloc_n7(   nspp);
   m.alloc_n7(   nspp);
   wt.alloc_n7(  nspp);
   fec.alloc_n7( nspp);
   spwn.alloc_n7(nspp);

   for (int i=1; i<=nspp; i++)
      {
      SEXP biol = PROTECT(VECTOR_ELT(x, i-1));

      _minquant[i] = (int)REAL(GET_SLOT(biol, install("range")))[0];
      _maxquant[i] = (int)REAL(GET_SLOT(biol, install("range")))[1];
      _plusgrp[i]  = (int)REAL(GET_SLOT(biol, install("range")))[2];
      _minyr[i]    = (int)REAL(GET_SLOT(biol, install("range")))[3];
      _maxyr[i]    = (int)REAL(GET_SLOT(biol, install("range")))[4];

      n.Init(   i, GET_SLOT(biol, install("n"))); 
      m.Init(   i, GET_SLOT(biol, install("m"))); 
      wt.Init(  i, GET_SLOT(biol, install("wt"))); 
      fec.Init( i, GET_SLOT(biol, install("fec"))); 
      spwn.Init(i, GET_SLOT(biol, install("spwn"))); 
      }

  UNPROTECT(nspp); 
  }

void FLBiols::unalloc(void)      
   {
   delete[] (_minquant+1);
   delete[] (_maxquant+1);
   delete[] (_plusgrp +1);
   delete[] (_minyr   +1);
   delete[] (_maxyr   +1);
   delete[] (_nunits  +1);
   delete[] (_nseasons+1);
   delete[] (_nareas  +1);
   delete[] (_niters  +1);
   }                               

FLBiols::~FLBiols(void)      
   {
   unalloc();
   }                               

SEXP FLBiols::Return(void)
   {
   SEXP ReturnObject = PROTECT(NEW_OBJECT(MAKE_CLASS("FLBiols")));
   SEXP biols        = PROTECT(allocVector(VECSXP,nspp));
   
   for (int i=1; i<=nspp; i++)
      {
      SEXP biol         = PROTECT(NEW_OBJECT(MAKE_CLASS("FLBiol")));
      SEXP Range        = PROTECT(NEW_NUMERIC(5));   
      
      REAL(Range)[0] = minquant(i);
      REAL(Range)[1] = maxquant(i);
      REAL(Range)[2] = plusgrp(i);
      REAL(Range)[3] = minyr(i);
      REAL(Range)[4] = maxyr(i);
       
      SET_SLOT(biol, install("range"), Range);

      SET_SLOT(biol, install("n"),     n.Return(i));
      SET_SLOT(biol, install("m"),     m.Return(i));
      SET_SLOT(biol, install("wt"),    wt.Return(i));
      SET_SLOT(biol, install("fec"),   fec.Return(i));
      SET_SLOT(biol, install("spwn"),  spwn.Return(i));

      SET_VECTOR_ELT(biols, i-1, biol);
      
      UNPROTECT(2);
      }

   ReturnObject = R_do_slot_assign(ReturnObject, install(".Data"), biols);
          
   UNPROTECT(2);

   return  ReturnObject;
   }

int FLBiols::minquant(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _minquant[ispp];
   }

int FLBiols::maxquant(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _maxquant[ispp];
   }

int FLBiols::plusgrp(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _plusgrp[ispp];
   }

int FLBiols::minyr(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _minyr[ispp];
   }

int FLBiols::maxyr(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _minyr[ispp];
   }

int FLBiols::nunits(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _nunits[ispp];
   }

int FLBiols::nseasons(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _nseasons[ispp];
   }

int FLBiols::nareas(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _nareas[ispp];
   }

int FLBiols::niters(int ispp)
   {
   if (ispp<1 || ispp>nspp) ispp=1;
   
   return _niters[ispp];
   }

FLStocks::FLStocks(void)      
    {
    _nspp=0;
    }

FLStocks::FLStocks(SEXP x)      
    {
    _nspp=0;

    if (isFLStocks(x))
       Init(x);
    }
   
void FLStocks::Init(SEXP x)
   {
   if (_nspp>0) 
      unalloc();
   
   if (isFLStocks(x))
      _nspp = NElemList(x);
   else
      return;

   if (_nspp<1) 
      return;
      
   _minquant = new int[_nspp] - 1;
   _maxquant = new int[_nspp] - 1;
   _plusgrp  = new int[_nspp] - 1;
   _minfbar  = new int[_nspp] - 1;
   _maxfbar  = new int[_nspp] - 1;
   _minyr    = new int[_nspp] - 1;
   _maxyr    = new int[_nspp] - 1;
   _nunits   = new int[_nspp] - 1;
   _nseasons = new int[_nspp] - 1;
   _nareas   = new int[_nspp] - 1;
   _niters   = new int[_nspp] - 1;

   stock.alloc_n7(       _nspp);
   stock_n.alloc_n7(     _nspp);
   stock_wt.alloc_n7(    _nspp);
   catch_.alloc_n7(      _nspp);
   catch_n.alloc_n7(     _nspp);
   catch_wt.alloc_n7(    _nspp);
   landings.alloc_n7(    _nspp);
   landings_n.alloc_n7(  _nspp);
   landings_wt.alloc_n7( _nspp);
   discards.alloc_n7(    _nspp);
   discards_n.alloc_n7(  _nspp);
   discards_wt.alloc_n7( _nspp);
   m.alloc_n7(           _nspp);
   mat.alloc_n7(         _nspp);
   harvest.alloc_n7(     _nspp);
   harvest_spwn.alloc_n7(_nspp);
   m_spwn.alloc_n7(      _nspp);
      
   for (int i=1; i<=_nspp; i++)
      {
      SEXP xstock = PROTECT(VECTOR_ELT(x, i-1));

      _minquant[i] = (int)REAL(GET_SLOT(xstock, install("range")))[0];
      _maxquant[i] = (int)REAL(GET_SLOT(xstock, install("range")))[1];
      _plusgrp[i]  = (int)REAL(GET_SLOT(xstock, install("range")))[2];
      _minyr[i]    = (int)REAL(GET_SLOT(xstock, install("range")))[3];
      _maxyr[i]    = (int)REAL(GET_SLOT(xstock, install("range")))[4];
      _minfbar[i]  = (int)REAL(GET_SLOT(xstock, install("range")))[5];
      _maxfbar[i]  = (int)REAL(GET_SLOT(xstock, install("range")))[6];
        
      _niters[i]   = 1;

      stock.Init(       i, GET_SLOT(xstock, install("stock"))); 
      stock_n.Init(     i, GET_SLOT(xstock, install("stock.n"))); 
      stock_wt.Init(    i, GET_SLOT(xstock, install("stock.wt"))); 
      catch_.Init(      i, GET_SLOT(xstock, install("catch"))); 
      catch_n.Init(     i, GET_SLOT(xstock, install("catch.n"))); 
      catch_wt.Init(    i, GET_SLOT(xstock, install("catch.wt"))); 
      landings.Init(    i, GET_SLOT(xstock, install("catch"))); 
      landings_n.Init(  i, GET_SLOT(xstock, install("catch.n"))); 
      landings_wt.Init( i, GET_SLOT(xstock, install("catch.wt"))); 
      discards.Init(    i, GET_SLOT(xstock, install("discards"))); 
      discards_n.Init(  i, GET_SLOT(xstock, install("discards.n"))); 
      discards_wt.Init( i, GET_SLOT(xstock, install("discards.wt"))); 
      m.Init(           i, GET_SLOT(xstock, install("m"))); 
      mat.Init(         i, GET_SLOT(xstock, install("mat"))); 
      harvest.Init(     i, GET_SLOT(xstock, install("harvest")));
      harvest_spwn.Init(i, GET_SLOT(xstock, install("harvest.spwn")));
      m_spwn.Init(      i, GET_SLOT(xstock, install("m.spwn")));

      _niters[i] =__max(_niters[i], stock.niters(i)); 
      _niters[i] =__max(_niters[i], stock_n.niters(i));    
      _niters[i] =__max(_niters[i], stock_wt.niters(i));   
      _niters[i] =__max(_niters[i], catch_.niters(i));     
      _niters[i] =__max(_niters[i], catch_n.niters(i));    
      _niters[i] =__max(_niters[i], catch_wt.niters(i));   
      _niters[i] =__max(_niters[i], landings.niters(i));   
      _niters[i] =__max(_niters[i], landings_n.niters(i)); 
      _niters[i] =__max(_niters[i], landings_wt.niters(i));
      _niters[i] =__max(_niters[i], discards.niters(i));   
      _niters[i] =__max(_niters[i], discards_n.niters(i)); 
      _niters[i] =__max(_niters[i], discards_wt.niters(i));
      _niters[i] =__max(_niters[i], m.niters(i));          
      _niters[i] =__max(_niters[i], mat.niters(i));        
      _niters[i] =__max(_niters[i], harvest.niters(i));    
      _niters[i] =__max(_niters[i], harvest_spwn.niters(i));
      _niters[i] =__max(_niters[i], m_spwn.niters(i));     
      }

  UNPROTECT(_nspp); 
  }

void FLStocks::unalloc(void)      
   {
   delete[] (_minquant+1);
   delete[] (_maxquant+1);
   delete[] (_plusgrp +1);
   delete[] (_minfbar +1);
   delete[] (_maxfbar +1);
   delete[] (_minyr   +1);
   delete[] (_maxyr   +1);
   delete[] (_nunits  +1);
   delete[] (_nseasons+1);
   delete[] (_nareas  +1);
   delete[] (_niters  +1);
   }                               

double FLStocks::computeStock(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      val += stock_n( ispp, iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);

   return val;
   }                               

double FLStocks::computeMnSz(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   double val = 0.0, 
          num = 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      {
      val += stock_n( ispp, iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);
      num += stock_n( ispp, iage, iyr, iunit, iseason, iarea, iter);
      }

   return val/num;
   }                               

double FLStocks::SSB(int ispp, int iyr, int iunit, int iseason, int iarea, int iter) 
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0, ssb= 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      {
      double deaded = exp(-m(      ispp, iage, iyr, iunit, iseason, iarea, iter)*m_spwn(      ispp, iage, iyr, iunit, iseason, iarea, iter)
                          -harvest(ispp, iage, iyr, iunit, iseason, iarea, iter)*harvest_spwn(ispp, iage, iyr, iunit, iseason, iarea, iter));
                    
      val  = stock_n( ispp, iage, iyr, iunit, iseason, iarea, iter)*
             stock_wt(ispp, iage, iyr, iunit, iseason, iarea, iter)*
             mat(     ispp, iage, iyr, iunit, iseason, iarea, iter)*deaded;

      if (!R_IsNA(val) && val>0.0) ssb +=val;
      }

   return val;
   }  
                             
double FLStocks::computeCatch(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      {
//      double z = m(ispp, iage, iyr, iunit, iseason, iarea, iter) + harvest(ispp, iage, iyr, iunit, iseason, iarea, iter);

//      val += stock_n( ispp, iage, iyr, iunit, iseason, iarea, iter)*
//             harvest( ispp, iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
//             catch_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);

      val += catch_n( ispp, iage, iyr, iunit, iseason, iarea, iter)*
             catch_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStocks::computeLandings(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      {
//      double z = m(ispp, iage, iyr, iunit, iseason, iarea, iter) + harvest(ispp, iage, iyr, iunit, iseason, iarea, iter);

//      val += landings_n( ispp, iage, iyr, iunit, iseason, iarea, iter)/
//             (discards_n(ispp, iage, iyr, iunit, iseason, iarea, iter)+
//              landings_n( ispp, iage, iyr, iunit, iseason, iarea,iter))*
//             stock_n(    ispp, iage, iyr, iunit, iseason, iarea, iter)*
//             harvest(    ispp, iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
//             landings_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);

      val +=  catch_n(   ispp, iage, iyr, iunit, iseason, iarea, iter)*
              landings_n(ispp, iage, iyr, iunit, iseason, iarea, iter)/
             (landings_n(ispp, iage, iyr, iunit, iseason, iarea, iter)+
              discards_n(ispp, iage, iyr, iunit, iseason, iarea, iter))*
             landings_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStocks::computeDiscards(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0;

   for (int iage=minquant(ispp); iage<=maxquant(ispp); iage++)
      {
      double z = m(ispp, iage, iyr, iunit, iseason, iarea, iter) + harvest(ispp, iage, iyr, iunit, iseason, iarea, iter);

      val += discards_n( ispp, iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(    ispp, iage, iyr, iunit, iseason, iarea, iter)*
             stock_n(    ispp, iage, iyr, iunit, iseason, iarea, iter)*
             harvest(    ispp, iage, iyr, iunit, iseason, iarea, iter)/z*(1-exp(-z))*
             discards_wt(ispp, iage, iyr, iunit, iseason, iarea, iter);
      }

   return val;
   } 

double FLStocks::Fbar(int ispp, int iyr, int iUnit, int iSeason, int iArea, int iIter)
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

	double val=0.0;

	for (int iAge = minfbar(ispp); iAge<= maxfbar(ispp);iAge++)
		val += harvest(ispp, iAge, iyr, iUnit,iSeason,iArea,iIter);

	return (val / (maxfbar(ispp)-minfbar(ispp)+1));
   }

double FLStocks::FbarLandings(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0;

   for (int iage=minfbar(ispp); iage<=maxfbar(ispp); iage++)
      val += harvest(   ispp, iage, iyr, iunit, iseason, iarea, iter)*
             landings_n(ispp, iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   ispp, iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar(ispp)-minfbar(ispp)+1);
   }                               


double FLStocks::FbarDiscards(int ispp, int iyr, int iunit, int iseason, int iarea, int iter)      
   {
   if (ispp<0 || ispp>_nspp) return 0.0;

   double val = 0.0;

   for (int iage=minfbar(ispp); iage<=maxfbar(ispp); iage++)
      val += harvest(   ispp, iage, iyr, iunit, iseason, iarea, iter)*
             discards_n(ispp, iage, iyr, iunit, iseason, iarea, iter)/
             catch_n(   ispp, iage, iyr, iunit, iseason, iarea, iter);

   return val/(maxfbar(ispp)-minfbar(ispp)+1);
   }   


FLStocks::~FLStocks(void)      
   {
   unalloc();
   }                               

SEXP FLStocks::Return(void)
   {
   SEXP ReturnObject = PROTECT(NEW_OBJECT(MAKE_CLASS("FLStocks")));
   SEXP stocks       = PROTECT(allocVector(VECSXP,_nspp));
   
   for (int i=1; i<=_nspp; i++)
      {
      SEXP _stock       = PROTECT(NEW_OBJECT(MAKE_CLASS("FLStock")));
      SEXP Range        = PROTECT(NEW_NUMERIC(5));   
      
      REAL(Range)[0] = minquant(i);
      REAL(Range)[1] = maxquant(i);
      REAL(Range)[2] = plusgrp(i);
      REAL(Range)[3] = minyr(i);
      REAL(Range)[4] = maxyr(i);
       
      SET_SLOT(_stock, install("range"), Range);

      SET_SLOT(_stock, install("stock"),        stock.Return(       i));
      SET_SLOT(_stock, install("stock.n"),      stock_n.Return(     i));
      SET_SLOT(_stock, install("stock.wt"),     stock_wt.Return(    i));
      SET_SLOT(_stock, install("catch"),        catch_.Return(      i));
      SET_SLOT(_stock, install("catch.n"),      catch_n.Return(     i));
      SET_SLOT(_stock, install("catch.wt"),     catch_wt.Return(    i));
      SET_SLOT(_stock, install("landings"),     landings.Return(    i));
      SET_SLOT(_stock, install("landings.n"),   landings_n.Return(  i));
      SET_SLOT(_stock, install("landings.wt"),  landings_wt.Return( i));
      SET_SLOT(_stock, install("discards"),     discards.Return(    i));
      SET_SLOT(_stock, install("discards.n"),   discards_n.Return(  i));
      SET_SLOT(_stock, install("discards.wt"),  discards_wt.Return( i));
      SET_SLOT(_stock, install("m"),            m.Return(           i));
      SET_SLOT(_stock, install("mat"),          mat.Return(         i));
      SET_SLOT(_stock, install("harvest"),      harvest.Return(     i));
      SET_SLOT(_stock, install("harvest.spwn"), harvest_spwn.Return(i));
      SET_SLOT(_stock, install("m.spwn"),       m_spwn.Return(      i));

      SET_VECTOR_ELT(stocks, i-1, _stock);
      }

   ReturnObject = R_do_slot_assign(ReturnObject, install(".Data"), stocks);
          
   UNPROTECT(4);

   return  ReturnObject;
   }

int FLStocks::minquant(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _minquant[ispp];
   }

int FLStocks::maxquant(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _maxquant[ispp];
   }

int FLStocks::plusgrp(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _plusgrp[ispp];
   }

int FLStocks::minfbar(int i7)
   {
   return _minfbar[i7];
   }

int FLStocks::maxfbar(int i7)
   {
   return _maxfbar[i7];
   }

int FLStocks::minyr(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _minyr[ispp];
   }

int FLStocks::maxyr(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _maxyr[ispp];
   }

int FLStocks::nunits(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _nunits[ispp];
   }

int FLStocks::nseasons(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _nseasons[ispp];
   }

int FLStocks::nareas(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _nareas[ispp];
   }

int FLStocks::niters(int ispp)
   {
   if (ispp<1 || ispp>_nspp) ispp=1;
   
   return _niters[ispp];
   }

int FLStocks::nspp(void)
   {
   return _nspp;
   }


FLIndices::FLIndices(void)      
    {
    nidx=0;
    }

FLIndices::FLIndices(SEXP x)      
    {
    nidx=0;

    if (isFLIndices(x))
       Init(x);
    }
   
void FLIndices::Init(SEXP x)
   {
   if (nidx>0) 
      unalloc();
   
   if (isFLIndices(x))
      nidx = NElemList(x);
   else
      return;

   if (nidx<1) 
      return;
      
   _minquant = new int[nidx] - 1;
   _minquant = new int[nidx] - 1;
   _maxquant = new int[nidx] - 1;
   _plusgrp  = new int[nidx] - 1;
   _minyr    = new int[nidx] - 1;
   _maxyr    = new int[nidx] - 1;
   _nunits   = new int[nidx] - 1;
   _nseasons = new int[nidx] - 1;
   _nareas   = new int[nidx] - 1;
   _niters   = new int[nidx] - 1;

   catch_n.alloc_n7(     nidx);
   catch_wt.alloc_n7(   nidx);
   effort.alloc_n7(     nidx);
   sel_pattern.alloc_n7(nidx);
   index_q.alloc_n7(    nidx);
   index.alloc_n7(      nidx);
   index_var.alloc_n7(  nidx);
      
   for (int i=1; i<=nidx; i++)
      {
      SEXP xidx = PROTECT(VECTOR_ELT(x, i-1));

      _minquant[i] = (int)REAL(GET_SLOT(xidx, install("range")))[0];
      _maxquant[i] = (int)REAL(GET_SLOT(xidx, install("range")))[1];
      _plusgrp[i]  = (int)REAL(GET_SLOT(xidx, install("range")))[2];
      _minyr[i]    = (int)REAL(GET_SLOT(xidx, install("range")))[3];
      _maxyr[i]    = (int)REAL(GET_SLOT(xidx, install("range")))[4];

      catch_n.Init(    i, GET_SLOT(xidx, install("catch.n"))); 
      catch_wt.Init(   i, GET_SLOT(xidx, install("catch.wt"))); 
      effort.Init(     i, GET_SLOT(xidx, install("effort"))); 
      sel_pattern.Init(i, GET_SLOT(xidx, install("sel.pattern"))); 
      index_q.Init(    i, GET_SLOT(xidx, install("index.q"))); 
      index.Init(      i, GET_SLOT(xidx, install("index"))); 
      index_var.Init(  i, GET_SLOT(xidx, install("index.var"))); 
      }

  UNPROTECT(nidx); 
  }

void FLIndices::unalloc(void)      
   {
   delete[] (_minquant+1);
   delete[] (_maxquant+1);
   delete[] (_plusgrp +1);
   delete[] (_minyr   +1);
   delete[] (_maxyr   +1);
   delete[] (_nunits  +1);
   delete[] (_nseasons+1);
   delete[] (_nareas  +1);
   delete[] (_niters  +1);
   }                               

FLIndices::~FLIndices(void)      
   {
   unalloc();
   }                               

SEXP FLIndices::Return(void)
   {
   SEXP ReturnObject = PROTECT(NEW_OBJECT(MAKE_CLASS("FLIndices")));
   SEXP idxs       = PROTECT(allocVector(VECSXP,nidx));
   
   for (int i=1; i<=nidx; i++)
      {
      SEXP Range        = PROTECT(NEW_NUMERIC(5));   
      SEXP _idx       = PROTECT(NEW_OBJECT(MAKE_CLASS("FLStock")));
      
      REAL(Range)[0] = minquant(i);
      REAL(Range)[1] = maxquant(i);
      REAL(Range)[2] = plusgrp(i);
      REAL(Range)[3] = minyr(i);
      REAL(Range)[4] = maxyr(i);
       
      SET_SLOT(_idx, install("range"), Range);

      SET_SLOT(_idx, install("catch.n"),     catch_n.Return(    i));
      SET_SLOT(_idx, install("catch.wt"),    catch_wt.Return(   i));
      SET_SLOT(_idx, install("effort"),      effort.Return(     i));
      SET_SLOT(_idx, install("sel.pattern"), sel_pattern.Return(i));
      SET_SLOT(_idx, install("index.q"),     index_q.Return(    i));
      SET_SLOT(_idx, install("index"),       index.Return(      i));
      SET_SLOT(_idx, install("index.var"),   index_var.Return(  i));

      SET_VECTOR_ELT(idxs, i-1, _idx);
      
      UNPROTECT(2);
      }

   ReturnObject = R_do_slot_assign(ReturnObject, install(".Data"), idxs);
          
   UNPROTECT(2);

   return  ReturnObject;
   }

int FLIndices::nindices()
	{
	return nidx;
	}

int FLIndices::minquant(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _minquant[iidx];
   }

int FLIndices::maxquant(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _maxquant[iidx];
   }

int FLIndices::plusgrp(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _plusgrp[iidx];
   }

int FLIndices::minyr(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _minyr[iidx];
   }

int FLIndices::maxyr(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _minyr[iidx];
   }

int FLIndices::nunits(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _nunits[iidx];
   }

int FLIndices::nseasons(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _nseasons[iidx];
   }

int FLIndices::nareas(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _nareas[iidx];
   }

int FLIndices::niters(int iidx)
   {
   if (iidx<1 || iidx>nidx) iidx=1;
   
   return _niters[iidx];
   }
