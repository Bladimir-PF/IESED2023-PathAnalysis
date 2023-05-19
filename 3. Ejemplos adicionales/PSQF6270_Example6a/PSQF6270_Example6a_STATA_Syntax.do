// Prevent "more" messages from appearing
   set more off
// Control line length
   set linesize 150

// Defining global variable for file location to be replaced in code below
   global filesave "C:\Dropbox\23_PSQF6270\PSQF6270_Example6a"
 
// Import example data file in STATA format (all names converted to lower-case)
   use "$filesave\Mindful.dta", clear

// Open external file to save results to
   log using $filesave\PSQF6270_Example6a_STATA_Output.log, replace name(Example6a)

* /// means continue the command + comment
*  // means comment only
* sem, standardized // print standardized effects this time

display "STATA Single-Group Path Model with Indirect Effects using Regular FIML and Robust Standard Errors"
sem                                                  ///
    (intern extern hostile benev warmth <- _cons)    /// All intercepts estimated (by default) 
    (warmth <- mindc)                                /// Regression X to Y
    (intern extern hostile benev <- mindc)           /// Regressions X to M1,M2,M3,M4
    (warmth <- intern extern hostile benev),         /// Regressions M1,M2,M3,M4 to Y
     means(mindc) var(mindc)                         /// Print X mean and variance (not default)
     var(e.intern e.extern e.hostile e.benev e.warmth)  /// All residual variances (by default)  
     covstruct(e.intern e.extern e.hostile e.benev, unstructured) /// All possible residual covariances
     method(mlmv) vce(robust)                   // Full-information ML with robust SEs (fit is same)
     estat teffects                             // Direct, indirect, and total effects (combined)
     nlcom _b[intern:mindc] *_b[warmth:intern]  // Indirect effect XtoM1toY
     nlcom _b[extern:mindc] *_b[warmth:extern]  // Indirect effect XtoM2toY
     nlcom _b[hostile:mindc]*_b[warmth:hostile] // Indirect effect XtoM3toY
     nlcom _b[benev:mindc]  *_b[warmth:benev]   // Indirect effect XtoM4toY
     nlcom _b[intern:mindc] *_b[warmth:intern]  + _b[extern:mindc]*_b[warmth:extern] + ///
           _b[hostile:mindc]*_b[warmth:hostile] + _b[benev:mindc]* _b[warmth:benev]  + ///
           _b[warmth:mindc]                     // Total indirect+direct effects
     nlcom _b[intern:mindc]* _b[warmth:intern]  + _b[extern:mindc]*_b[warmth:extern] + ///
           _b[hostile:mindc]*_b[warmth:hostile] + _b[benev:mindc]*_b[warmth:benev]     ///
		                                         // Total indirect effects
     sem, coeflegend                             // Print parameter labels (to use in lincom)
     sem, standardized                           // Print standardized solution 
     estat gof, stats(all)                       // Print model fit statistics
	  display "LL for H1 Saturated Model= " e(critvalue_s)
     display "# of Estimated parameters= " e(df_m)
     estat eqgof  	                             // Print R2 per variable
	 


display "STATA Multiple-Group Path Model with Indirect Effects using Regular FIML and Robust Standard Errors"
sem                                                  ///
    (intern extern hostile benev warmth <- _cons)    /// All intercepts estimated (by default) 
    (warmth <- mindc)                                /// Regression X to Y
    (intern extern hostile benev <- mindc)           /// Regressions X to M1,M2,M3,M4
    (warmth <- intern extern hostile benev),         /// Regressions M1,M2,M3,M4 to Y
     means(mindc) var(mindc)                         /// Print X mean and variance (not default)
     var(e.intern e.extern e.hostile e.benev e.warmth)  /// All possible residual variances (by default)  
     covstruct(e.intern e.extern e.hostile e.benev, unstructured) /// All residual covariances
     method(mlmv) vce(robust)          /// Full-information ML with robust SEs (fit is same)
	  group(sexmw) ginvariant(none)      // none= full non-invariance
	  estat teffects                     // Direct, indirect (not correct), and total effects
     // Men and women indirect effect XtoM1toY and difference
     nlcom _b[intern:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.intern]  
     nlcom _b[intern:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.intern]
     nlcom _b[intern:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.intern] - ///
           _b[intern:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.intern]
     // Men and women indirect effect XtoM2toY and difference
     nlcom _b[extern:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.extern]
     nlcom _b[extern:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.extern]
     nlcom _b[extern:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.extern] - ///
           _b[extern:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.extern]
     // Men and women indirect effect XtoM3toY and difference
     nlcom _b[hostile:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.hostile]
     nlcom _b[hostile:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.hostile]
     nlcom _b[hostile:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.hostile] - ///
           _b[hostile:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.hostile]
     // Men and women indirect effect XtoM4toY and difference
     nlcom _b[benev:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.benev] 
     nlcom _b[benev:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.benev]
     nlcom _b[benev:0bn.sexmw#c.mindc]*_b[warmth:0bn.sexmw#c.benev] - ///
           _b[benev:1.sexmw#c.mindc]  *_b[warmth:1.sexmw#c.benev]
     // Total and total indirect per group would be computed as for single-group model
     sem, coeflegend                             // Print parameter labels (to use in lincom)
     sem, standardized                           // Print standardized solution
     estat gof, stats(all)                       // Print model fit statistics
     display "LL for H1 Saturated Model= " e(critvalue_s)
     display "# of Estimated parameters= " e(df_m)
     estat eqgof 	                             // Print R2 per variable
     estat ginvariant      // Wald or Score test for each parm's invariance
                           // Wald  = test of constraining equal if unequal
                           // Score = test of allowing unequal if equal

display "STATA Multiple-Group Path Model with Indirect Effects using Regular FIML and Robust Standard Errors"
display "Testing Equality of Direct effect XtoM1 by Holding it Equal by Sex with @a"
display "Model chi-square gives significance test of 1 new constraint"
sem                                                  ///
    (intern extern hostile benev warmth <- _cons)    /// All intercepts estimated (by default) 
    (warmth <- mindc)                                /// X to Y for both groups
    (0: intern@a extern hostile benev <- mindc)      /// X to M1,M2,M3,M4 for group 0
	 (1: intern@a extern hostile benev <- mindc)      /// X to M1,M2,M3,M4 for group 1
    (warmth <- intern extern hostile benev),         /// M1,M2,M3,M4 to Y for both groups 	
     means(mindc) var(mindc)                          /// Print X mean and variance (not default)
     var(e.intern e.extern e.hostile e.benev e.warmth)  /// All possible residual variances (by default)  
     covstruct(e.intern e.extern e.hostile e.benev, unstructured) /// All residual covariances
     method(mlmv) vce(robust)               /// Full-information ML with robust SEs (fit is same)
	  group(sexmw) ginvariant(none)           // none= full non-invariance
     sem, coeflegend                         // Print parameter labels (to use in lincom)
     sem, standardized                       // Print standardized solution
     estat gof, stats(all)                   // Print model fit statistics
     display "LL for H1 Saturated Model= " e(critvalue_s)
     display "# of Estimated parameters= " e(df_m)
     estat eqgof    	                     // Print R2 per variable
     estat ginvariant      // Wald or Score test for each parm's invariance
                           // Wald  = test of constraining equal if unequal
                           // Score = test of allowing unequal if equal
	 
// Close external file of saved results
   log close Example6a

