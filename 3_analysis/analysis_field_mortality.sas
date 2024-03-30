* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Charcoal rot severity				 *
* Mortality							 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\charcoal-mortality;
	%LET results_path_img=&base_path.\4_results\charcoal-mortality\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/charcoal-mortality;
*	%LET results_path_img=&base_path./4_results/charcoal-mortality/html_images;

	** both;
	%LET name_base=charcoal-mortality_; 

*** load macros for controlling output;
	** local;
	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
*	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ********** *
* A. Import  *
* ********** *;

*** save log to file;
proc printto new log="&results_path/&name_base.A_sas-log.log"; run; 

** import data;
	* local;
	proc import 
			datafile="&base_path.\2_data\severity_final_charcoal-mortality_gone-removed_SAS.csv"
			dbms=dlm replace out=field_mort;
		delimiter=",";
		getnames=YES;
	run;
			
	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/severity_final_charcoal-mortality_gone-removed_SAS.csv"
*			dbms=dlm replace out=field_mort;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_mort;
	by season irrigation cultivar inoculum block date severity_mort severity_present;
run;

** check dataset;
proc print data=field_mort;
	title 'mac irr field mortality full review';
run;

**set date as character to remove first date;
data field_mort;
	set field_mort;
	datenew=put(date, yymmddn8.);
run;

*** adjust values;
	** previous rounds of analyses failed with raw data, 1 added to all values to remove 0s;
data field_mort_adj;
	set field_mort;
	severity_mort_adj = severity_mort + 1;
run;


** filter out first rating date from each season;
data field_mort_adj_rm1;
	set field_mort_adj;
	if datenew = ("20190508") then delete;
	if datenew = ("20200428") then delete;
	if datenew = ("20210417") then delete;
run;

** check dataset;	
*proc print data=field_mort_rm1;
*	title 'Mortality Ratings without First Rating Date';
*run;

*** direct log back to SAS Log window;
proc printto; run; 

** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results


* ***************************************** *
* B. Test Overdispersion and Random Effects *
* ***************************************** *;
	* original (unadjusted) data;
	
	* see Appendix B for code;

	* NOTE: code and results are for previous version of data before manual quality control
	
	* RESULTS: few errors but most result in a missing SE estimate
	* CONCLUSION: add 1 to all observations


* ********************************************************* *
* C. Test Overdispersion and Random Effects - Adjusted Data *
* ********************************************************* *;

*** Notes
	** over-dispersion is when variance > mean;
		* method=laplace used to get dispersion assessment
		* assessed by Pearson chi-square / DF;
			* over-dispersed if > 1, requires remedial action if > 2 per Stroup 2015 (10.2134/agronj2013.0342), https://site.caes.uga.edu/expstatgrif/files/2018/07/CountsGLMMfin1.pdf;
	** start from simple model and add complexity;

	** random effect statements
		* none
		* basic: random intercept / subject=Block;
		* Stroup: random intercept / subject=Block(Irrigation);
		* simplified Madden: random Irrigation*Cultivar*Inoculum / subject=Block;
		* Madden: random intercept Irrigation Irrigation*Cultivar*Inoculum / subject=Block;

*** OBJ: test for overdispersion and select random effect statements

*** STEP 1 ***;
	* NOTE: commented out because later section provides the same results

	** OBJ: test simple model for overdispersion;
*	%output_html_files_on(name_step=C_step-1, title_step=C step 1); * redirect html, log output to files;
*
*	proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*		class irrigation cultivar inoculum block date;
*		by season;
*		model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
*		title 'field mortality 1st date removed, adjusted C test dispersion and random effects - step 1 no random effect';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULT: no errors
		* plots look ok, Pearson/DF: 2018 = 0.22, 2019 = 0.75, 2020 = 1.74

	** CONCLUSION: try adding random effects
	

*** Step 2 ***;
	** OBJ: add simple random effects;
	** see Appendix C for code
	** RESULTS:
		* Step 2-1 basic random effect term: 2018, estimated G matrix not positive definite | Pearson/DF: 2019 = 0.63, 2020 = 1.64;
		* Step 2-2 simplified Madden random effect term: 2018, estimated G matrix not positive definite | Pearson/DF: 2019 = 0.39, 2020 = 0.48
	** CONCLUSION: try advanced random effects


*** Step 3 ***;
	** OBJ: add advanced random effects;

*	** Step 3-1 **
*		* OBJ: Stroup random effect term;
*		%output_html_files_on(name_step=C_step-3-1, title_step=C step 3-1); * redirect html, log output to files;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			title 'field mortality 1st date removed, adjusted C test dispersion and random effects - step 3-1 Stroup random effect term';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS:
			* 2018, estimated G matrix not positive definite
			* Pearson/DF: 2019 = 0.48, 2020 = 1.42;

*	** Step 3-2 **
		* OBJ: Madden random effect term;
		* see Appendix C for code
		* RESULTS: 2018, estimated G matrix not positive definite | 2019 Pearson/DF = 0.46 | 2020, estimated G matrix not positive definite;
	
	** CONCLUSION: try repeated measures
		* 2018: use no random effect 
		* 2019, 2020: Stroup term most in line with split block for 2019, 2020


* ****************************************** *
* D. Test Repeated Measures - Adjusted Data *
* ****************************************** *;

*** Step 1 ***;
	** OBJ: test different structures for repeated measures;
	** see Appendix D for code
	** RESULTS:
		* cs ran normally for 2019 only
		* un, csh, ar(1), ante(1) failed for all seasons	
	** CONCLUSION: try simpler random effects for type=cs
	

*** Step 2 ***;
	** OBJ: test simpler random effects
	** see Appendix D for code
	** RESULTS: 
		* Step 2-1 no random effect: 2018 failed, 2019/2020 appeared to run normally
		* Step 2-2 basic random effect: 2018/2020 failed, 2019 appeared to run normally

	** CONCLUSION: test all rating dates
	

*** Step 3 ***;
	** OBJ: test all rating dates

	** Step 3-1 **
		* OBJ: no random effect;
			* NOTE: commented out because later section provides the same results
*		%output_html_files_on(name_step=D_step-3-1, title_step=D step 3-1); * redirect html, log output to files;
*
*		proc glimmix data=field_mort_adj method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 3-1 no random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: 2018 failed, 2019/2020 appeared to run normally

	** Step 3-2 **
		* OBJ: basic random effect;
		* see Appendix D for code
		* RESULTS: 2018/2019 failed, 2020 appeared to run normally
	
	** CONCLUSION: 
		* 2018: original analysis (1st date removed, no random effects, no repeated measures)
		* 2019, 2020: all dates, no random effects, repeated measures


* *************************************** *
* E. Analyze Main Effects - Adjusted Data *
* *************************************** *;

* OBJ: analyze main effects by removing method=laplace;

*** create datasets;
	data field_mort_adj_rm1_18;
		set field_mort_adj_rm1;
		if season in ('2019-2020','2020-2021') then delete;
	run;

	data field_mort_adj_1920;
		set field_mort_adj;
		if season in ('2018-2019') then delete;
	run;


*** Step 1 ***;
	** Step 1-1 **
		* OBJ: analyze main effects for 2018;
		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
		
		proc glimmix data=field_mort_adj_rm1_18 plot=residualpanel;
			class irrigation cultivar inoculum block date;
			by season;
			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
			title 'field mortality adjusted, 1st date removed - E analyze main effects - step 1-1 2018';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: inoculum is only significant effect

	** Step 1-2 ** 
		* OBJ: analyze main effects for 2019, 2020;
			* NOTE: commented out because later section provides the same results
*		%output_html_files_on(name_step=E_step-1-2, title_step=E step 1-2); * redirect html, log output to files;
*
*		proc glimmix data=field_mort_adj_1920 plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted - E analyze main effects - step 1-2 2019, 2020';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: 
			* 2019: irrigation*inoculum, cultivar*inoculum
			* 2020: irrigation*inoculum, weak cultivar*inoculum

	** CONCLUSION: investigate interactions


* ****************************************** *
* F. Investigate Interaction - Adjusted Data *
* ****************************************** *;

** OBJ: investigate interaction from E above;
	* NOTE: commented out because Section F provides the same results

*** Step 1 ***;
	** OBJ: investigate interactions;
		* NOTE: commented out because later section provides the same results
*	%output_html_files_on(name_step=F_step-1, title_step=F step 1); * redirect html, log output to files;
*
*	proc glimmix data=field_mort_adj_1920 plot=residualpanel;
*		class irrigation cultivar inoculum block date;
*		by season;
*		model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*
*		random date / type=cs subject=irrigation*cultivar*inoculum*block;
*		
*		slice irrigation*inoculum / sliceBy=inoculum;
*		slice cultivar*inoculum / sliceBy=inoculum;
*		
*		title 'field mortality adjusted - F investigate interactions - step 1';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* 2019: irrigation*inoculum, both slices | cultivar*inoculum, inoculated only
		* 2020: irrigation*inoculum, inoculated, weak control | weak cultivar*inoculum, control only

	** CONCLUSION: separate means


* ********************************* *
* G. Separate Means - Adjusted Data *
* ********************************* *;

** OBJ: separate means from slices in section F above;

*** Step 1 ***;
	** OBJ: separate means using multiple comparisons;
		* NOTE: no means separation for cultivar*inoculum because only 2 levels;
	%output_html_files_on(name_step=G_step-1, title_step=G step 1); * redirect html, log output to files;

	proc glimmix data=field_mort_adj_1920 plot=residualpanel;
		class irrigation cultivar inoculum block date;
		by season;
		model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;

		random date / type=cs subject=irrigation*cultivar*inoculum*block;
		
		slice irrigation*inoculum / sliceBy=inoculum means ilink linestable adjust=tukey;
		slice cultivar*inoculum / sliceBy=inoculum;
		
		title 'field mortality adjusted - G separate means - step 1';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: differences among treatments for all significant slices

	** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;



* NOTE: code to redirect output to HTML removed for clarity

* **************************** *
* Appendix B - Check for overdispersion  *
* **************************** *;

*** Step 1 ***
	
*	** STEP 1-1 **;
*		*OBJ: Stroup random effect term;
*
*		proc glimmix data=field_mortality_rm1 method=laplace plot=residualpanel;
*			class Irrigation Cultivar Inoculum Block Rating_Date;
*			by Season;
*			model final_mort / total_ratings = Irrigation|Cultivar|Inoculum|Rating_Date  / dist=binomial link=logit;
*			random intercept / subject=Block(Irrigation);
*			title 'field mortality B step 1-1b - diagnose dispersion Stroup date-adj';
*		run;
*
*		* RESULT: no errors in Log. Pearson/DF 2018= 0.31 no SE, 2019= 0.50 no SE, 2020= 1.41
*		with date adjustment: 2018= 0.36 no SE, 2019= 0.61 no SE, 2020= 1.46
*
*	** STEP 1-2 **;
*		*OBJ: Madden random effect term;
*
*		proc glimmix data=field_mortality_rm1 method=laplace plot=residualpanel;
*			class Irrigation Cultivar Inoculum Block Rating_Date;
*			by Season;
*			model final_mort / total_ratings = Irrigation|Cultivar|Inoculum|Rating_Date  / dist=binomial link=logit;
*			random intercept Irrigation Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field mortality B step 1-2b - diagnose dispersion Madden date-adj';
*		run;
*
*		*RESULT: no error in Log. Pearson/DF 2018=  Estimated G matrix is not positive definite., 2019= 0.47 no SE, 2020= QUANEW needs more than 200 iterations or 2000 function calls. 
*		with date adjustment: 2018=Estimated G matrix is not positive definite., 2019= 0.57, 2020=QUANEW needs more than 200 iterations or 2000 function calls. 
*
*	** STEP 1-3 **;
*		*OBJ: general random effect term;
*
*		proc glimmix data=field_mortality_rm1 method=laplace plot=residualpanel;
*			class Irrigation Cultivar Inoculum Block Rating_Date;
*			by Season;
*			model final_mort / total_ratings = Irrigation|Cultivar|Inoculum|Rating_Date  / dist=binomial link=logit;
*			random intercept / subject=Block;
*			title 'field mortality B step 1-3b - diagnose dispersion general date-adj';
*		run;
*
*		*RESULT: no error in Log. Pearson/DF 2018=  Estimated G matrix is not positive definite., 2019= 0.62, 2020= 1.58
*		with date adjustment: 2018=Estimated G matrix is not positive definite., 2019= 0.74, 2020=1.71
*
*	** STEP 1-4 **;
*		* OBJ:  simplified Madden random effect term;
*
*		proc glimmix data=field_mortality_rm1 method=laplace plot=residualpanel;
*			class Irrigation Cultivar Inoculum Block Rating_Date;
*			by Season;
*			model final_mort / total_ratings = Irrigation|Cultivar|Inoculum|Rating_Date  / dist=binomial link=logit;
*	   		random Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field mortality B step 1-4b - diagnose dispersion simplified Madden date-adj';
*		run;
*	
*		*RESULT: no error in Log. Pearson/DF 2018= 0.26 Infinity listed in test of fixed effects, 2019= 0.42 no SE, 2020= 0.61
*		with date adjustment: 2018=0.27, 2019=0.51 no SE, 2020=0.54
*
*	** STEP 1-5 **;
*		*OBJ: try removing random effect term;
*
*		proc glimmix data=field_mortality_rm1 method=laplace plot=residualpanel;
*			class Irrigation Cultivar Inoculum Block Rating_Date;
*			by Season;
*			model final_mort / total_ratings = Irrigation|Cultivar|Inoculum|Rating_Date  / dist=binomial link=logit;
*			title 'field mortality B step 1-5b - diagnose dispersion removed random date-adj';
*		run;
*	
*		*RESULT: no error in Log. Pearson/DF 2018= 0.34 but F-value 0 for all in tests of fixed effects, 2019= 0.73, 2020= 1.63
*		with date adjustment: 2018=0.38, 2019=0.89, 2020=1.76
*
*	*OVERALL RESULT OF STEP 1: no random statement works well for all 3 seasons. Simplified Madden results in lowest Pearson/DF both with and without the first rating date removed, but in 2019 there was no SE.
*	The next lowest comes from Stroup but no SE in 2018 or 2019. Next lowest is General but not for 2018, then remove random.
*
*		Stroup: no errors in Log. Pearson/DF 2018= 0.31 no SE, 2019= 0.50 no SE, 2020= 1.41
*		Madden: no error in Log. Pearson/DF 2018=  Estimated G matrix is not positive definite., 2019= 0.47 no SE, 2020= QUANEW needs more than 200 iterations or 2000 function calls. 
*		General: no error in Log. Pearson/DF 2018=  Estimated G matrix is not positive definite., 2019= 0.62, 2020= 1.58
*		Simplified Madden: no error in Log. Pearson/DF 2018= 0.26 Infinity listed in test of fixed effects, 2019= 0.42 no SE, 2020= 0.61
*		Remove random: no error in Log. Pearson/DF 2018= 0.34 but F-value 0 for all in tests of fixed effects, 2019= 0.73, 2020= 1.63
*
*		with first rating date removed from each season:
*			Stroup: no errors in Log. Pearson/DF 2018= 0.36 no SE, 2019= 0.61 no SE, 2020= 1.46
*			Madden: no error in Log. Pearson/DF = 2018=Estimated G matrix is not positive definite., 2019= 0.57, 2020=QUANEW needs more than 200 iterations or 2000 function calls. 
*			General: no error in Log. Pearson/DF 2018=Estimated G matrix is not positive definite., 2019= 0.74, 2020=1.71
*			Simplified Madden: no error in Log. Pearson/DF 2018=0.27, 2019=0.51 no SE, 2020=0.54
*			Remove random: no error in Log. Pearson/DF 2018=0.38, 2019=0.89, 2020=1.76


* ******************************************************************* *
* Appendix C - Test Overdispersion and Random Effects - Adjusted Data *
* ******************************************************************* *;


**** Step 2 ***;
*	** OBJ: add simple random effects;
*
*	** Step 2-1 **
*		* OBJ: basic random effect term;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
*			random intercept / subject=block;
*			title 'field mortality 1st date removed, adjusted C test dispersion and random effects - step 2-1 basic random effect term';
*		run;
*
*	** Step 2-2 **
*		* OBJ: simplified Madden random effect term;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field mortality 1st date removed, adjusted C test dispersion and random effects - step 2-2 simplified Madden';
*		run;
*
**** Step 3 ***;
*	** OBJ: add advanced random effects;
*
*	** Step 3-2 **
*		* OBJ: Madden random effect term;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum|date  / dist=binomial link=logit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field mortality 1st date removed, adjusted C test dispersion and random effects - step 3-2 Madden';
*		run;


* *************************************************** *
* Appendix D - Test Repeated Measures - Adjusted Data *
* *************************************************** *;

**** Step 1 ***;
*	** OBJ: test different structures for repeated measures;
*
*	** Step 1-1 **;
*		* OBJ: unstructured;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			random date / type=un subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 1-1 un';
*		run;
*
*	** Step 1-2 **;
*		* OBJ: compound symmetry;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 1-2 cs';
*		run;
*
*	** Step 1-3 **;
*		* OBJ: compound symmetry heterogeneous;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			random date / type=csh subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 1-3 csh';
*		run;
*	
*	** Step 1-4 **;
*		* OBJ: auto regressive;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			random date / type=ar(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 1-4 ar(1)';
*		run;
*
*	** Step 1-5 **
*		* OBJ: ante dependence;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			random date / type=ante(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 1-5 ante(1)';
*		run;
*	
*	** CONCLUSION: try simpler random effects for type=cs
	

**** Step 2 ***;
*	** OBJ: test simpler random effects
*
*	** Step 2-1 **
*		* OBJ: no random effect;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 2-1 no random effect';
*		run;
*
*	** Step 2-2 **
*		* OBJ: basic random effect;
*
*		proc glimmix data=field_mort_adj_rm1 method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block;
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 2-2 basic random effect';
*		run;
	

**** Step 3 ***;
*	** OBJ: test all rating dates
*
*	** Step 3-2 **
*		* OBJ: basic random effect;
*
*		proc glimmix data=field_mort_adj method=laplace plot=residualpanel;
*			class irrigation cultivar inoculum block date;
*			by season;
*			model severity_mort_adj / severity_present = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept / subject=block;
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field mortality adjusted, 1st date removed - D test repeated measures - step 3-2 basic random effect';
*		run;
