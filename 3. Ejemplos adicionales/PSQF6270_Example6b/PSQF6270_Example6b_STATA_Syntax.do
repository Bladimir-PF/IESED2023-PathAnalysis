// Prevent "more" messages from appearing
   set more off 
// Control line length
   set linesize 150

// Define global variable for file location to be replaced in code below
   global filesave "C:\Dropbox\22_PSQF6270\PSQF6270_Example6b"
 
// Import example data file 
   use "$filesave\driver.dta", clear
   
// Drop cases not part of follow-up study
   drop if follow==0

// Open external file to save results to
   log using $filesave\PSQF6270_Example6b_STATA_Output.log, replace name(Example6b)
   
* /// means continue the command + comment
*  // means comment only

display "STATA Path Model for Example 6b"
display "Results do not match Mplus because of missing data"
gsem                                                        ///
(speed2@1 acc2@1 <-RandInt)                                 /// Random intercept factor for binary outcome covariance
(simfac limit4 speed2 acc2 RandInt@0 <- _cons)              /// All outcome intercepts estimated by default
(simfac <- sex age75 visfac zufov1 zufov2 zufov3 dscan)	                       /// X1-X7        to normal M1
(limit4 <- sex age75 visfac zufov1 zufov2 zufov3 dscan simfac)	               /// X1-X7, M1    to normal M2
(acc2   <- sex age75 visfac zufov1 zufov2 zufov3 dscan simfac limit4, logit)   /// X1-X7, M1-M2 to binary Y1
(speed2 <- sex age75 visfac zufov1 zufov2 zufov3 dscan simfac limit4, logit),  /// X1-X7, M1-M2 to binary Y2
var(e.simfac e.limit4 e.RandInt)           /// All residual variances estimated (by default)  
method(ml) vce(robust) 					    // Equation-wise ML, robust SEs
gsem, coeflegend                            // Print parameter labels, too (to use in lincom) 
nlcom _b[simfac:dscan]*_b[acc2:simfac]      // Indirect effect: dscan --> sim --> acc
estat eform speed2 acc2                     // Get odds ratios for binary outcomes

// Close external file of saved results	
log close Example6b

