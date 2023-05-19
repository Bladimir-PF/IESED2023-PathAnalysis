########################### PSQF 6270 Example 6a using R #################################

# Set width of output and number of significant digits printed,
# number of digits before using scientific notation, shut off significance stars
options(width=120, digits=8, scipen=9, show.signif.stars=FALSE)

#####  Check to see if packages are downloaded, install if not, then load  #####

if (!require("haven")) install.packages("haven")
library(haven) # To import SAS data with labels as table

if (!require("expss")) install.packages("expss")
library(expss) # To add variable and value labels, sorting

if (!require("psych")) install.packages("psych")
library(psych) # To add descriptive summary functions

if (!require("lavaan")) install.packages("lavaan")
library(lavaan) # To fit path models 

if (!require("TeachingDemos")) install.packages("TeachingDemos")
library(TeachingDemos) # To create text output files

###################################################################################
# Define variables for working directory and data name
filesave = "C:\\Dropbox/23_PSQF6270/PSQF6270_Example6a/"
filename = "Mindful.sas7bdat"
setwd(dir=filesave)

# ImportSAS data
Mindful = read_sas(data_file=paste0(filesave,filename)) 
# Convert to data frame without labels to use for analysis
Mindful = as.data.frame(Mindful)

# Open external file to save results to
txtStart(file=paste0(filesave,"PSQF6270_Example6a_R_Output.txt"))

#print("Show global options for lavaan")
#lavOptions(x=NULL, default=NULL, mimic="mplus")

print("R Single-Group Path Model with Indirect Effects using Robust FIML and Standard Errors")
# Create model syntax as separate text object
SyntaxSingle = "
# Means/Intercepts and Variances/Residual Variances (labels)
  MindC   ~ (Xint)*1;  MindC   ~~ (Xvar)*MindC;
  Intern  ~ (M1int)*1; Intern  ~~ (M1var)*Intern; 
  Extern  ~ (M2int)*1; Extern  ~~ (M2var)*Extern;
  Hostile ~ (M3int)*1; Hostile ~~ (M3var)*Hostile;
  Benev   ~ (M4int)*1; Benev   ~~ (M4var)*Benev;
  Warmth  ~ (Yint)*1;  Warmth  ~~ (Yvar)*Warmth;
# Direct MindC --> Warmth
  Warmth ~ (XtoY)*MindC
# Left side of model
  Intern  ~ (XtoM1)*MindC
  Extern  ~ (XtoM2)*MindC
  Hostile ~ (XtoM3)*MindC
  Benev   ~ (XtoM4)*MindC
# Right side of model
  Warmth ~ (M1toY)*Intern + (M2toY)*Extern + (M3toY)*Hostile + (M4toY)*Benev
# Residual Covariances
  Intern  ~~ (Cov1)*Extern  + (Cov2)*Hostile + (Cov3)*Benev
  Extern  ~~ (Cov4)*Hostile + (Cov5)*Benev
  Hostile ~~ (Cov6)*Benev
# Indirect effects, total indirect+direct, and total indirect effects
  XtoM1toY := XtoM1*M1toY; XtoM2toY := XtoM2*M2toY
  XtoM3toY := XtoM3*M3toY; XtoM4toY := XtoM4*M4toY
  totXtoY  := XtoM1*M1toY + XtoM2*M2toY + XtoM3*M3toY + XtoM4*M4toY + XtoY
  totInd   := XtoM1*M1toY + XtoM2*M2toY + XtoM3*M3toY + XtoM4*M4toY
" # Now estimate model and get output
ModelSingle = lavaan(data=Mindful, model=SyntaxSingle, estimator="MLR", mimic="mplus")
summary(ModelSingle, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

txtComment(" ") # insert blank space in output 


print("R Multiple-Group Path Model with Indirect Effects using Robust FIML and Standard Errors")
# Create model syntax as separate text object
SyntaxMultiple = "
# Means/Intercepts and Variances/Residual Variances (labels)
  MindC   ~ c(mXint,  wXint)*1;  MindC   ~~ c(mXvar,  wXvar)*MindC;
  Intern  ~ c(mM1int, wM1int)*1; Intern  ~~ c(mM1var, wM1var)*Intern; 
  Extern  ~ c(mM2int, wM2int)*1; Extern  ~~ c(mM2var, wM2var)*Extern;
  Hostile ~ c(mM3int, wM3int)*1; Hostile ~~ c(mM3var, wM3var)*Hostile;
  Benev   ~ c(mM4int, wM4int)*1; Benev   ~~ c(mM4var, wM4var)*Benev;
  Warmth  ~ c(mYint,  wYint)*1;  Warmth  ~~ c(mYvar,  wYvar)*Warmth;
# Direct MindC --> Warmth
  Warmth ~ c(mXtoY, wXtoY)*MindC
# Left side of model
  Intern  ~ c(mXtoM1, wXtoM1)*MindC
  Extern  ~ c(mXtoM2, wXtoM2)*MindC
  Hostile ~ c(mXtoM3, wXtoM3)*MindC
  Benev   ~ c(mXtoM4, wXtoM4)*MindC
# Right side of model
  Warmth ~ c(mM1toY, wM1toY)*Intern  + c(mM2toY, wM2toY)*Extern 
         + c(mM3toY, wM3toY)*Hostile + c(mM4toY, wM4toY)*Benev
# Residual Covariances
  Intern  ~~ c(mCov1, wCov1)*Extern  + c(mCov2, wCov2)*Hostile + c(mCov3, wCov3)*Benev
  Extern  ~~ c(mCov4, wCov4)*Hostile + c(mCov5, wCov5)*Benev
  Hostile ~~ c(mCov6, wCov6)*Benev
# Indirect effects for both groups
  mXtoM1Y := mXtoM1*mM1toY; wXtoM1Y := wXtoM1*wM1toY
  mXtoM2Y := mXtoM2*mM2toY; wXtoM2Y := wXtoM2*wM2toY
  mXtoM3Y := mXtoM3*mM3toY; wXtoM3Y := wXtoM3*wM3toY
  mXtoM4Y := mXtoM4*mM4toY; wXtoM4Y := wXtoM4*wM4toY
# Total indirect+direct and total indirect effects for both groups
  mtotXtoY  := mXtoM1*mM1toY + mXtoM2*mM2toY + mXtoM3*mM3toY + mXtoM4*mM4toY + mXtoY
  mtotInd   := mXtoM1*mM1toY + mXtoM2*mM2toY + mXtoM3*mM3toY + mXtoM4*mM4toY
  wtotXtoY  := wXtoM1*wM1toY + wXtoM2*wM2toY + wXtoM3*wM3toY + wXtoM4*wM4toY + wXtoY
  wtotInd   := wXtoM1*wM1toY + wXtoM2*wM2toY + wXtoM3*wM3toY + wXtoM4*wM4toY
# Differences in direct effects across groups
  dXtoM1 := mXtoM1-wXtoM1; dXtoM2 := mXtoM2-wXtoM2;
  dXtoM3 := mXtoM3-wXtoM3; dXtoM4 := mXtoM4-wXtoM4;
  dM1toY := mM1toY-wM1toY; dM2toY := mM2toY-wM2toY;
  dM3toY := mM3toY-wM3toY; dM4toY := mM4toY-wM4toY;
# Differences in indirect effects across groups
  dXtoM1Y := mXtoM1Y-wXtoM1Y; dXtoM2Y := mXtoM2Y-wXtoM2Y
  dXtoM3Y := mXtoM3Y-wXtoM3Y; dXtoM4Y := mXtoM4Y-wXtoM4Y
# Differences in total indirect+direct and total indirect effects across groups
  dtotXtoY := mtotXtoY-wtotXtoY; dtotInd := mtotInd-wtotInd 
" # Now estimate model and get output
ModelMultiple = lavaan(data=Mindful, model=SyntaxMultiple, estimator="MLR", mimic="mplus", group="SexMW")
summary(ModelMultiple, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

txtComment(" ") # insert blank space in output 


print("R Multiple-Group Path Model with Indirect Effects using Robust FIML and Standard Errors")
print("Testing Equality of Direct effect XtoM1 by Holding it Equal by Sex")
print("Model chi-square provides significance test of 1 new constraint")
# Create model syntax as separate text object
SyntaxMultipleXtoM1 = "
# Means/Intercepts and Variances/Residual Variances (labels)
  MindC   ~ c(mXint,  wXint)*1;  MindC   ~~ c(mXvar,  wXvar)*MindC;
  Intern  ~ c(mM1int, wM1int)*1; Intern  ~~ c(mM1var, wM1var)*Intern; 
  Extern  ~ c(mM2int, wM2int)*1; Extern  ~~ c(mM2var, wM2var)*Extern;
  Hostile ~ c(mM3int, wM3int)*1; Hostile ~~ c(mM3var, wM3var)*Hostile;
  Benev   ~ c(mM4int, wM4int)*1; Benev   ~~ c(mM4var, wM4var)*Benev;
  Warmth  ~ c(mYint,  wYint)*1;  Warmth  ~~ c(mYvar,  wYvar)*Warmth;
# Direct MindC --> Warmth
  Warmth ~ c(mXtoY, wXtoY)*MindC
# Left side of model
  Intern  ~ (XtoM1)*MindC
  Extern  ~ c(mXtoM2, wXtoM2)*MindC
  Hostile ~ c(mXtoM3, wXtoM3)*MindC
  Benev   ~ c(mXtoM4, wXtoM4)*MindC
# Right side of model
  Warmth ~ c(mM1toY, wM1toY)*Intern  + c(mM2toY, wM2toY)*Extern 
         + c(mM3toY, wM3toY)*Hostile + c(mM4toY, wM4toY)*Benev
# Residual Covariances
  Intern  ~~ c(mCov1, wCov1)*Extern  + c(mCov2, wCov2)*Hostile + c(mCov3, wCov3)*Benev
  Extern  ~~ c(mCov4, wCov4)*Hostile + c(mCov5, wCov5)*Benev
  Hostile ~~ c(mCov6, wCov6)*Benev
" # Now estimate model and get output
ModelMultipleXtoM1 = lavaan(data=Mindful, model=SyntaxMultipleXtoM1, estimator="MLR", mimic="mplus", group="SexMW")
summary(ModelMultipleXtoM1, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

txtComment(" ") # insert blank space in output 


# Close external output file
txtStop()

