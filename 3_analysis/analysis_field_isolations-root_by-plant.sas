* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Plant Isolation Data - Roots 	     *
* Per-Plant							 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\isolation-roots;
	%LET results_path_img=&base_path.\4_results\isolation-roots\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/isolation-roots;
*	%LET results_path_img=&base_path./4_results/isolation-roots/html_images;

	** both;
	%LET name_base=isolation-roots_; 

*** load macros for controlling output;
	** local;
	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
*	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ********* *
* A. Import *
* ********* *;

** import data;
	* local;
	proc import 
			datafile="&base_path.\2_data\isolation_final_dead-removed_root_SAS.csv"
			dbms=dlm replace out=field_iso_root;
		delimiter=",";
		getnames=YES;
	run;
	
	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/isolation_final_dead-removed_root_SAS.csv"
*			dbms=dlm replace out=field_iso_root;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_iso_root;
	by season irrigation cultivar inoculum block date plant;
run;

** check dataset;
proc print data=field_iso_root;
	title 'mac irr field isolations full review';
run;
	
** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results

* ***************************************** *
* B. Test Overdispersion and Random Effects *
* ***************************************** *;	
	* original (unadjusted data);

	* see Appendix B for code
	
	* NOTE: code and results for previous version of data with random dead plants included and before changes to data structure (long format, column names)

	** RESULTS: estimated G matrix not positive definite, models did not converge, or too much overdispersion
	** CONCLUSION: add 1 to all observations


* ********************************************************* *
* C. Test Overdispersion and Random Effects - Adjusted Data *
* ********************************************************* *;	

	* NOTE: for current version of data (random dead plants removed) analysis went straight into adjusted data

** create dataset;
data field_iso_root_adj;
	set field_iso_root;
	pieces_mp_adj = pieces_mp + 1;
run;

*** Step 1 ***;
	** OBJ: test model (with SamplingDate) for overdispersion;

	** Step 1-1 **;
		* OBJ: Stroup random effect term;
		* see Appendix C for code
		* RESULTS: 
			* estimated G matrix is not positive definite (both 2018 and 2019)
			* residual plots look better, pearson/DF: 2018 = 0.27, 2019 = 0.79, 2020 = 1.19
		
	** Step 1-2 **;
		* OBJ: Madden random effect term;
		* see Appendix C for code
		* RESULTS: 
			* estimated G matrix is not positive definite (all three years)
			* residual plots look better, pearson/DF: 2018 = 0.27, 2019 = 0.79, 2020 = 1.13

	** CONCLUSION: simplify random effects


*** Step 2 ***;
	** OBJ: test model for overdispersion, simplifying random effects;

	** Step 2-1 **;
		* OBJ: Stroup random effect term;		
		* see Appendix C for code
		* RESULTS: estimated G matrix is not positive definite (both 2018 and 2019), pearson/DF for 2020=1.20
		
	** Step 2-2 **;
		* OBJ: Madden random effect term;
		* see Appendix C for code
		* RESULTS: 
			* 2018, 2019 - estimated G matrix is not positive definite
			* 2020 - no errors, Pearson/DF = 1.11

	** CONCLUSION:
		* 2018: split off, try simplifying model
		* 2019: use Step 2-2 model 
		* 2020: use Step 2-2 model 


*** Step 3 ***;

	** OBJ: test simpler model by removing random effect;
	%output_html_files_on(name_step=C_step-3, title_step=C step 3); * redirect html, log output to files;
	proc glimmix data=field_iso_root_adj method=laplace plot=residualpanel;
		class season irrigation cultivar inoculum block date;
		by season;
		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
		title 'field iso root C step 3 - diagnose dispersion';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;
	
	** RESULTS: no errors, residual plot looks ok, not overdispersed, pearson/DF: 2018 = 0.27, 2019 = 0.79, 2020 = 1.29
		
	** CONCLUSION: use this model for all years


* *************************************** *
* D. Analyze Main Effects - Adjusted Data *
* *************************************** *;		

* OBJ: analyze main effects by removing method=laplace;
	* NOTE: commented out because D Step 1, E provides the same results

*** Step 1 ***;
	** OBJ: analyze main effects;
*	%output_html_files_on(name_step=D_step-1, title_step=D step 1); * redirect html, log output to files;
*	proc glimmix data=field_iso_root_adj plot=residualpanel;
*		class season irrigation cultivar inoculum block date;
*		by season;
*		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*		title 'field iso root D step 1 - analyze main effects';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;
	
	** RESULTS: 
		* 2018: inoculum, date;
		* 2019: cultivar, inoculum, cultivar*inoculum, date;
		* 2020: inoculum, date;
		
	** CONCLUSION: investigate cultivar*inoculum 2019 interaction

*** Step 2 ***;
	** OBJ: repeat D Step 1 with 2019 removed for output;
	
	* set data;
	data field_iso_root_adj_1820;
		set field_iso_root_adj;
		if season in ('2019-2020') then delete;
	run;
	
	%output_html_files_on(name_step=D_step-2, title_step=D step 2); * redirect html, log output to files;
	proc glimmix data=field_iso_root_adj_1820 plot=residualpanel;
		class season irrigation cultivar inoculum block date;
		by season;
		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
		title 'field iso root D step 2 - analyze main effects, 2018-19, 2020-21';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;
	

* ****************************************** *
* E. Investigate Interaction - Adjusted Data *
* ****************************************** *;
	
*** Step 1 ***;
	** OBJ: investigate cultivar*inoculum interaction for 2019;
	
	* set data;
	data field_iso_root_adj_19;
		set field_iso_root_adj;
		if season in ('2018-2019','2020-2021') then delete;
	run;
	
	%output_html_files_on(name_step=E_step-1, title_step=E step 1); * redirect html, log output to files;
	
	proc glimmix data=field_iso_root_adj_19 plots=none;
		class season irrigation cultivar inoculum block date;
		by season;
		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
		
		slice cultivar*inoculum / sliceBy=inoculum;
*		slice cultivar*inoculum / sliceBy=cultivar;

		title 'field iso root E step 1 - investigate interaction 2019';
	run;
	
	%output_html_files_off(); * turn off redirecting html, log output to files;
	
	** RESULTS
		* slicing by cultivar -> effect of inoculum significant for both cultivars, therefore should be sliced by inoculum 
		* effect of cultivar for inoculum only
		
	** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;


* *************************************************** *
* Appendix B - Test Overdispersion and Random Effects *
* *************************************************** *;
	
	* NOTE: code and results for previous version of data with random dead plants included and before changes to data structure, column names
	* NOTE: code to redirect output to HTML removed for clarity
	
*** sources blended
	** split plot design: Stroup 2014 Agronomy J 811-supplement7.sas
	** binomial data: Stroup 2014 Agronomy J 811-supplement6.sas
	** second random effect option Madden, Kriss GLMM workshop APS 2016 CaseStudy2.sas

*** Step 1 ***;
	** OBJ: test model (with SamplingDate) for overdispersion;
		* over-dispersion is when variance > mean;
		* method=laplace used to get dispersion assessment
		* assessed by Pearson chi-square / DF;
			* over-dispersed if > 1, requires remedial action if > 2 per Stroup 2015 (10.2134/agronj2013.0342), https://site.caes.uga.edu/expstatgrif/files/2018/07/CountsGLMMfin1.pdf

	** Step 1-1 **;
		* OBJ: Stroup random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model RootsMp / TotalRoots = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*			random intercept / subject=Block(Irrigation);
*			title 'field iso root B step 1-1 - diagnose dispersion';
*		run;

		* RESULTS: no errors, 
			* 2018 - fixed effect results clustered at extremes and residuals do not look good, pearson/DF = 0.87
			* 2019 - residuals do not look good, pearson/DF too high (1.86)
			* 2020 - pearson/DF = 3.28, too high
		
	** Step 1-2 **;
		* OBJ: Madden random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model RootsMp / TotalRoots = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*			random intercept Irrigation Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field iso root B step 1-2 - diagnose dispersion';
*		run;

		* RESULTS: estimated G matrix not positive definite (all 3 seasons)

	** CONCLUSION: simplify random effects
	
*** Step 2 ***;
	** OBJ: simplify random effects;

	** Step 2-1 **;
		* OBJ: general random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model RootsMp / TotalRoots = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*			random intercept / subject=Block;
*			title 'field iso root B step 2-1 - diagnose dispersion';
*		run;

		* RESULTS: 2018) estimated G matrix is not positive definite, 2019) pearson/DF = 1.88, 2020) pearson/DF = 3.05
		
	** Step 2-2 **;
		* OBJ: simplified Madden random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model RootsMp / TotalRoots = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*			random Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field iso root B step 2-2 - diagnose dispersion';
*		run;	

		* RESULTS: no errors, residual plots don't look great, 2018 pearson/DF = 0.72, 2019 pearson/DF still a bit high (1.62), 2020 pearson/DF too high (2.53)
		
	** CONCLUSION: remove random effects

*** Step 3 ***;
	** OBJ: remove random effects;

*	proc glimmix data=field_iso method=laplace plot=residualpanel;
*		class Season Irrigation Cultivar Inoculum Block SamplingDate;
*		by Season;
*		model RootsMp / TotalRoots = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*		title 'field iso root B step 3 - diagnose dispersion';
*	run;

	** RESULTS: no errors, but residual plots, 2019 and 2020 overdispersion unchanged
	
	** CONCLUSION: add 1 to all observations


* ******************************************************************* *
* Appendix C - Test Overdispersion and Random Effects - Adjusted Data *
* ******************************************************************* *;	

**** Step 1 ***;
*	** OBJ: test model (with SamplingDate) for overdispersion;
*
*	** Step 1-1 **;
*		* OBJ: Stroup random effect term;
*
*		proc glimmix data=field_iso_root_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*			title 'field iso root C step 1-1 - diagnose dispersion';
*		run;
*		
*	** Step 1-2 **;
*		* OBJ: Madden random effect term;
*
*		proc glimmix data=field_iso_root_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field iso root C step 1-2 - diagnose dispersion';
*		run;
*
**** Step 2 ***;
*	** OBJ: test model for overdispersion, simplifying random effects;
*
*	** Step 2-1 **;
*		* OBJ: Stroup random effect term;		
*
*		proc glimmix data=field_iso_root_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random intercept / subject=block;
*			title 'field iso root C step 2-1 - diagnose dispersion';
*		run;
*
*	** Step 2-2 **;
*		* OBJ: Madden random effect term;
*
*		proc glimmix data=field_iso_root_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field iso root C step 2-2 - diagnose dispersion';
*		run;