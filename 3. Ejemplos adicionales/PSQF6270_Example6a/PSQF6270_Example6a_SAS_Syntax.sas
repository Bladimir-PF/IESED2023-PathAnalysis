* Generic session options - page and line size, no page breaks;
OPTIONS nonumber nodate nocenter pagesize=MAX linesize=MAX formdlim=' ' spool;                                          
* Kill default titles on output, ODS tables in log, turn on graphics; 
TITLE; ODS TRACE OFF; ODS GRAPHICS ON;

* Location for original files for these models – change this path;
%LET filesave = C:\Dropbox\23_PSQF6270\PSQF6270_Example6a;
LIBNAME filesave "&filesave.";

/* Bringing data into work library;
PROC IMPORT DATAFILE="&filesave.\Mindfulness Example.sav" 
     DBMS=SPSS OUT=work.Mindful REPLACE; RUN;

DATA work.Mindful; SET work.Mindful; FORMAT _ALL_; 
* Renaming/creating other variables;
     * ID; ID = pin1;
	* Gender; SexMW = Sex_MFonly; * 0=Men, 1=Women;
     * X;  MindC = Mindful_Total1 - 2; 
     * M1; Intern = MCS_Internal_Total;
     * M2; Extern = MCS_External_Total;
     * M3; Hostile = HS_Total;
     * M4; Benev = BS_Total;
     * Y;  Warmth = (Feminists + Womens_Movement) /2;
* Subset to variables needed;
     KEEP ID--Warmth;
RUN;
* Save to permanent SAS file;
DATA filesave.Mindful; SET work.Mindful; RUN;
* Export to STATA;
PROC EXPORT DATA=work.Mindful OUTFILE="&filesave.\Mindful.dta" DBMS=STATA REPLACE; RUN;
* Replace missing values with -999 and export to Mplus;
DATA work.ForMplus; SET work.Mindful;
     ARRAY vars(8) ID--Warmth; 
	DO i=1 TO 8; IF vars(i)=. THEN vars(i)=-999; END; DROP i;
RUN;
PROC EXPORT DATA=work.ForMplus OUTFILE="&filesave.\Mindful.csv" DBMS=CSV REPLACE; 
     PUTNAMES=NO; RUN;
*/
* Split data by gender for multiple group model;
DATA work.Mindful; SET filesave.Mindful; RUN;
DATA work.Men;   SET work.Mindful; WHERE SexMW=0; RUN;
DATA work.Women; SET work.Mindful; WHERE SexMW=1; RUN;

/* Testing what happens to missing predictors across programs;
DATA work.MindTest; SET work.Mindful;
	IF _N_<11 THEN MindC=.; RUN;
DATA filesave.MindTest; SET work.MindTest; RUN;
* Export to STATA;
PROC EXPORT DATA=work.MindTest OUTFILE="&filesave.\MindTest.dta" DBMS=STATA REPLACE; RUN;
* Replace missing values with -999 and export to Mplus;
DATA work.ForMplus; SET work.MindTest;
     ARRAY vars(8) ID--Warmth; 
	DO i=1 TO 8; IF vars(i)=. THEN vars(i)=-999; END; DROP i;
RUN;
PROC EXPORT DATA=work.ForMplus OUTFILE="&filesave.\MindTest.csv" DBMS=CSV REPLACE; 
     PUTNAMES=NO; RUN;
*/

* Open external file to save results to;
ODS RTF FILE="&filesave.\PSQF6270_Example6a_SAS_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

TITLE1 "SAS Single-Group Path Model with Indirect Effects using Regular FIML and Standard Errors";
PROC CALIS DATA=work.Mindful MEANSTR TOTEFF METHOD=FIML; *PLOTS=PATHDIAGRAM;
* VAR = List of variables in model;
  VAR  MindC Intern Extern Hostile Benev Warmth;
* MEAN = Labeling means/intercepts per variable;
  MEAN MindC=Xint, Intern=M1int, Extern=M2int, Hostile=M3int, Benev=M4int, Warmth=Yint;
* PVAR = Labeling variances/residual variances per variable;
  PVAR MindC=Xvar, Intern=M1var, Extern=M2var, Hostile=M3var, Benev=M4var, Warmth=Yvar;
* PCOV = Requesting and labeling residual covariances;
  PCOV Intern Extern=CovM12, Intern Hostile=CovM13, Intern Benev=CovM14, 
       Extern Hostile=CovM23, Extern Benev=CovM24, Hostile Benev=Cov34;
* PATH = Model specification and labels;
  PATH MindC ---> Warmth = XtoY,
       MindC ---> Intern Extern Hostile Benev =  XtoM1 XtoM2 XtoM3 XtoM4,
       Intern Extern Hostile Benev ---> Warmth = M1toY M2toY M3toY M4toY;
* TESTFUNC = Requesting indirect, total indirect+direct, and total indirect effects;
* First list newly created parameters to be defined below;
  TESTFUNC XtoM1toY XtoM2toY XtoM3toY XtoM4toY totXtoY totInd; 
* Then define indirect effects;
           XtoM1toY = XtoM1*M1toY; XtoM2toY = XtoM2*M2toY; 
           XtoM3toY = XtoM3*M3toY; XtoM4toY = XtoM4*M4toY; 
           totXtoY  = XtoM1toY + XtoM2toY + XtoM3toY + XtoM4toY + XtoY; * As given by TOTEFF;
           totInd   = XtoM1toY + XtoM2toY + XtoM3toY + XtoM4toY;        * As given by TOTEFF;
/* To make path diagram in SAS 9.4;
      PATHDIAGRAM 
          TITLE="Mindfulness Mediation Example" SOLUTION=STANDARD
          ARRANGE=FLOW NOFITTABLE
          LABEL=[MindC="Mindfulness" Hostile="Hostile Sexism" Benev="Benevolent Sexism"
                  Intern="Internal Motivation" Extern="External Motivation" 
                  Warmth="Warmth Towards Non-Traditional Women"]; */
RUN; TITLE1;


TITLE1 "SAS Multiple-Group Path Model with Indirect Effects using Regular FIML and Standard Errors";
PROC CALIS DATA=work.Mindful MEANSTR TOTEFF METHOD=FIML; 
* VAR = List of variables in model;
  VAR  MindC Intern Extern Hostile Benev Warmth;
* GROUP creates separate models (referred to by number);
  GROUP 1 / LABEL="Men"   DATA=work.Men; 
  GROUP 2 / LABEL="Women" DATA=work.Women; 
* Model for Men -- all parameters are labeled separately for separate estimation by group;
  MODEL 1 / GROUP=1; 
  MEAN MindC=mXint, Intern=mM1int, Extern=mM2int, Hostile=mM3int, Benev=mM4int, Warmth=mYint;
  PVAR MindC=mXvar, Intern=mM1var, Extern=mM2var, Hostile=mM3var, Benev=mM4var, Warmth=mYvar;
  PCOV Intern Extern=mCovM12, Intern Hostile=mCovM13, Intern Benev=mCovM14, 
       Extern Hostile=mCovM23, Extern Benev=mCovM24, Hostile Benev=mCov34;
  PATH MindC ---> Warmth = mXtoY,
       MindC ---> Intern Extern Hostile Benev   = mXtoM1 mXtoM2 mXtoM3 mXtoM4,
       Intern Extern Hostile Benev ---> Warmth = mM1toY mM2toY mM3toY mM4toY;
* Model for Women  -- all parameters are labeled separately for separate estimation by group;
  MODEL 2 / GROUP=2; 
  MEAN MindC=wXint, Intern=wM1int, Extern=wM2int, Hostile=wM3int, Benev=wM4int, Warmth=wYint;
  PVAR MindC=wXvar, Intern=wM1var, Extern=wM2var, Hostile=wM3var, Benev=wM4var, Warmth=wYvar;
  PCOV Intern Extern=wCovM12, Intern Hostile=wCovM13, Intern Benev=wCovM14, 
       Extern Hostile=wCovM23, Extern Benev=wCovM24, Hostile Benev=wCov34;
  PATH MindC ---> Warmth = wXtoY,
       MindC ---> Intern Extern Hostile Benev =  wXtoM1 wXtoM2 wXtoM3 wXtoM4,
       Intern Extern Hostile Benev ---> Warmth = wM1toY wM2toY wM3toY wM4toY;
* First list newly created parameters to be defined below;
  TESTFUNC mXtoM1Y mXtoM2Y mXtoM3Y mXtoM4Y wXtoM1Y wXtoM2Y wXtoM3Y wXtoM4Y
           mtotXtoY mtotInd wtotXtoY wtotInd 
           dXtoM1 dXtoM2 dXtoM3 dXtoM4 dM1toY dM2toY dM3toY dM4toY
           dXtoM1Y dXtoM2Y dXtoM3Y dXtoM4Y dtotXtoY dtotInd;  
* Indirect effects for both groups;
           mXtoM1Y = mXtoM1*mM1toY; wXtoM1Y = wXtoM1*wM1toY;
           mXtoM2Y = mXtoM2*mM2toY; wXtoM2Y = wXtoM2*wM2toY;
           mXtoM3Y = mXtoM3*mM3toY; wXtoM3Y = wXtoM3*wM3toY;
           mXtoM4Y = mXtoM4*mM4toY; wXtoM4Y = wXtoM4*wM4toY;
* Total indirect+direct and total indirect effects for both groups (as given by TOEFF);
           mtotXtoY = mXtoM1Y + mXtoM2Y + mXtoM3Y + mXtoM4Y + mXtoY;
           mtotInd  = mXtoM1Y + mXtoM2Y + mXtoM3Y + mXtoM4Y;
           wtotXtoY = wXtoM1Y + wXtoM2Y + wXtoM3Y + wXtoM4Y + wXtoY;
           wtotInd  = wXtoM1Y + wXtoM2Y + wXtoM3Y + wXtoM4Y; 
* Differences in direct effects across groups;
           dXtoM1 = mXtoM1-wXtoM1; dXtoM2 = mXtoM2-wXtoM2;
           dXtoM3 = mXtoM3-wXtoM3; dXtoM4 = mXtoM4-wXtoM4;
           dM1toY = mM1toY-wM1toY; dM2toY = mM2toY-wM2toY;
           dM3toY = mM3toY-wM3toY; dM4toY = mM4toY-wM4toY;
* Differences in indirect effects across groups;
           dXtoM1Y = mXtoM1Y-wXtoM1Y; dXtoM2Y = mXtoM2Y-wXtoM2Y;
           dXtoM3Y = mXtoM3Y-wXtoM3Y; dXtoM4Y = mXtoM4Y-wXtoM4Y;
* Differences in total indirect+direct and total indirect effects across groups;
           dtotXtoY = mtotXtoY-wtotXtoY; dtotInd = mtotInd-wtotInd; 
RUN; TITLE1;


TITLE1 "SAS Multiple-Group Path Model with Indirect Effects using Regular FIML and Standard Errors";
TITLE2 "Testing Equality of Direct effect XtoM1 by Holding it Equal by Sex";
TITLE3 "Model chi-square provides significance test of 1 new constraint";
PROC CALIS DATA=work.Mindful MEANSTR TOTEFF METHOD=FIML; 
* VAR = List of variables in model;
  VAR  MindC Intern Extern Hostile Benev Warmth;
* GROUP creates separate models (referred to by number);
  GROUP 1 / LABEL="Men"   DATA=work.Men; 
  GROUP 2 / LABEL="Women" DATA=work.Women; 
* Model for Men -- XtoM1 path shares label to constrain across groups;
  MODEL 1 / GROUP=1; 
  MEAN MindC=mXint, Intern=mM1int, Extern=mM2int, Hostile=mM3int, Benev=mM4int, Warmth=mYint;
  PVAR MindC=mXvar, Intern=mM1var, Extern=mM2var, Hostile=mM3var, Benev=mM4var, Warmth=mYvar;
  PCOV Intern Extern=mCovM12, Intern Hostile=mCovM13, Intern Benev=mCovM14, 
       Extern Hostile=mCovM23, Extern Benev=mCovM24, Hostile Benev=mCov34;
  PATH MindC ---> Warmth = mXtoY,
       MindC ---> Intern Extern Hostile Benev =   XtoM1 mXtoM2 mXtoM3 mXtoM4,
       Intern Extern Hostile Benev ---> Warmth = mM1toY mM2toY mM3toY mM4toY;
* Model for Women  -- XtoM1 path shares label to constrain across groups;
  MODEL 2 / GROUP=2; 
  MEAN MindC=wXint, Intern=wM1int, Extern=wM2int, Hostile=wM3int, Benev=wM4int, Warmth=wYint;
  PVAR MindC=wXvar, Intern=wM1var, Extern=wM2var, Hostile=wM3var, Benev=wM4var, Warmth=wYvar;
  PCOV Intern Extern=wCovM12, Intern Hostile=wCovM13, Intern Benev=wCovM14, 
       Extern Hostile=wCovM23, Extern Benev=wCovM24, Hostile Benev=wCov34;
  PATH MindC ---> Warmth = wXtoY,
       MindC ---> Intern Extern Hostile Benev =    XtoM1 wXtoM2 wXtoM3 wXtoM4,
       Intern Extern Hostile Benev ---> Warmth = wM1toY wM2toY wM3toY wM4toY;
RUN; TITLE1; TITLE2; TITLE3;

* Close external file of saved results;
ODS RTF CLOSE;
