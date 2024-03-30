* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Plant Isolation Data - Crowns	     *
* Per-Plant							 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\isolation-crown;
	%LET results_path_img=&base_path.\4_results\isolation-crown\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/isolation-crown;
*	%LET results_path_img=&base_path./4_results/isolation-crown/html_images;

	** both;
	%LET name_base=isolation-crown_; 

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
			datafile="&base_path.\2_data\isolation_final_dead-removed_crown_SAS.csv"
			dbms=dlm replace out=field_iso_crown;
		delimiter=",";
		getnames=YES;
	run;
	
	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/isolation_final_dead-removed_crown_SAS.csv"
*			dbms=dlm replace out=field_iso_crown;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_iso_crown;
	by season irrigation cultivar inoculum block date plant;
run;

** check dataset;
proc print data=field_iso_crown;
	title 'mac irr field isolations full review';
run;


** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results


* ***************************************** *
* B. Test Overdispersion and Random Effects *
* ***************************************** *;	
	* original (unadjusted data);

	* see Appendix B for code
	
	* NOTE: code and results are for previous version of data with random dead plants included and before changes to data structure (long format, column names)

	** RESULTS: estimated G matrix not positive definite, models did not converge, or too much overdispersion
	** CONCLUSION: add 1 to all observations


* ********************************************************* *
* C. Test Overdispersion and Random Effects - Adjusted Data *
* ********************************************************* *;	

	* NOTE: for current version of data (random dead plants removed) analysis went straight into adjusted data

*** OBJ: 
	** test model for overdispersion;
		* over-dispersion is when variance > mean;
		* method=laplace used to get dispersion assessment
		* assessed by Pearson chi-square / DF;
			* over-dispersed if > 1, requires remedial action if > 2 per Stroup 2015 (10.2134/agronj2013.0342), https://site.caes.uga.edu/expstatgrif/files/2018/07/CountsGLMMfin1.pdf;
	** start from simple model and add complexity;
	
*** create dataset;
	data field_iso_crown_adj;
		set field_iso_crown;
		pieces_mp_adj = pieces_mp + 1;
	run;

*** Step 1 ***;
	** OBJ: test simple model for overdispersion;
	%output_html_files_on(name_step=C_step-1, title_step=C step 1); * redirect html, log output to files;
	proc glimmix data=field_iso_crown_adj method=laplace plot=residualpanel;
		class season irrigation cultivar inoculum block date;
		by season;
		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
		title 'field iso crown C test dispersion and random effects - step 1 no random effect';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: ran without errors and estimates appear sensical, Pearson/DF = 0.07, 0.61, 0.49 for 2018, 2019, 2020
				
	** CONCLUSION: add simple random effects


*** Step 2 ***;
	** OBJ: add random effects;

	** Step 2-1 **;
		* OBJ: basic random effect term;
		* see Appendix C for code
		* RESULTS: estimated G matrix is not positive definite for all three years;
		
	** Step 2-2 **;
		* OBJ: simplified Madden random effect term;
		* see Appendix C for code
		* RESULTS: estimated G matrix is not positive definite for all three years

	** Step 2-3 **;
		** OBJ: Stroup random effect term;
		* see Appendix C for code
		* RESULTS: Estimated G matrix is not positive definite for all three years
	
	** CONCLUSION: will not work, analyze main effects without any random effect


* *************************************** *
* D. Analyze Main Effects - Adjusted Data *
* *************************************** *;	

* OBJ: analyze main effects by removing method=laplace;
	* NOTE: commented out because Section E provides the same results

*** Step 1 ***;
	** OBJ: analyze main effects;
*	%output_html_files_on(name_step=D_step-1, title_step=D step 1); * redirect html, log output to files;
*	proc glimmix data=field_iso_crown_adj plot=residualpanel;
*		class season irrigation cultivar inoculum block date;
*		by season;
*		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*		title 'field iso crown D analyze main effects - step 1';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* 2018: no significant effects
		* 2019: inoculum, date, weak irrigation*inoculum
		* 2020: date, weak irrigation*inoculum
				
	** CONCLUSION: investigate irrigation*inoculum interaction


* ****************************************** *
* E. Investigate Interaction - Adjusted Data *
* ****************************************** *;

** OBJ: investigate interaction from D above;
	* NOTE: commented out because Section F provides the same results

*** Step 1 ***;
	** OBJ: investigate interaction
*	%output_html_files_on(name_step=E_step-1, title_step=E step 1); * redirect html, log output to files;
*	proc glimmix data=field_iso_crown_adj plot=residualpanel;
*		class season irrigation cultivar inoculum block date;
*		by season;
*		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*
*		slice irrigation*inoculum / sliceBy=inoculum;
*
*		title 'field iso crown E investigate interaction - step 1';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* 2019: effect of irrigation for inoculated not control
		* 2020: no significant effect for either inoculated or control

	** CONCLUSION: separate means;


* ********************************* *
* F. Separate Means - Adjusted Data *
* ********************************* *;

** OBJ: separate means from slices in section D above;

*** Step 1 ***;
	** OBJ: separate means using multiple comparisons;
	** see Appendix F for code
	** RESULTS: no significance

	** CONCLUSION: try contrast statements;

*** Step 2 ***;
	** OBJ: separate means using contrast statements;
	%output_html_files_on(name_step=F_step-2, title_step=E step 2); * redirect html, log output to files;
	
	proc glimmix data=field_iso_crown_adj plot=residualpanel;
		class season irrigation cultivar inoculum block date;
		by season;
		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;

		slice irrigation*inoculum / sliceBy=inoculum;

		estimate 'Inoculated: opti vs high'
			irrigation 		  1 0 -1
			irrigation*inoculum 0 1 0 0 0 -1 / e;

		estimate 'Inoculated: opti vs low'
			irrigation 		      0 1 -1
			irrigation*inoculum	0 0 0 1 0 -1 / e;

		estimate 'Control: opti vs high'
			irrigation          1 0 -1	  
			irrigation*inoculum 1 0 0 0 -1 0 / e;

		estimate 'Control: opti vs low'
			irrigation 		     0 1 -1   
			irrigation*inoculum	 0 0 1 0 -1 0 / e;

		title 'field iso crown F separate means - step 2 - contrast statements';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* 2019: inoculated opti vs low, all others not significant
		* 2020: weak evidence for control opti vs high and opti vs low

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
	** OBJ: test model with SamplingDate for overdispersion;
		* over-dispersion is when variance > mean;
		* method=laplace used to get dispersion assessment
		* assessed by Pearson chi-square / DF;
			* over-dispersed if > 1, requires remedial action if > 2 per Stroup 2015 (10.2134/agronj2013.0342), https://site.caes.uga.edu/expstatgrif/files/2018/07/CountsGLMMfin1.pdf

	** Step 1-1 **;
		* OBJ: Stroup random effect term;
		
*		proc glimmix data=field_iso method=laplace plot=residualpanel nobound;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum SamplingDate / dist=binomial link=logit;
*			random intercept / subject=Block(Irrigation);
*			title 'field iso crown B step 1-1 - diagnose dispersion';
*		run;
				
		* RESULTS: 
			2018) estimated G matrix not positive definite
			2019) no errors but covariance SE is "." and residual plots do not look good
			2020) Pearson/DF = 1.98
		
	** Step 1-2 **;
		* OBJ: Madden random effect term;
			
**		proc glimmix data=field_iso method=laplace plot=residualpanel nobound;
*			class Season Irrigation Cultivar Inoculum Block SamplingDate;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum SamplingDate/ dist=binomial link=logit;
*			random intercept Irrigation Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field iso crown B step 1-2 - diagnose dispersion';
*		run;
				
		* RESULTS: 
			2018) ERROR QUANEW optimization cannot be completed, optimization routine cannot improve function value
			2019) estimated G matrix is not positive definite
			2020) estimated G matrix is not positive definite
		
	** CONCLUSION: try removing sampling date;


*** Step 2 ***;
	** OBJ: test model for overdispersion;
		* over-dispersion is when variance > mean;
		* method=laplace used to get dispersion assessment
		* assessed by Pearson chi-square / DF;
			* over-dispersed if > 1, requires remedial action if > 2 per Stroup 2015 (10.2134/agronj2013.0342), https://site.caes.uga.edu/expstatgrif/files/2018/07/CountsGLMMfin1.pdf

	** Step 2-1 **;
		* OBJ: Stroup random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel nobound;
*			class Season Irrigation Cultivar Inoculum Block;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum / dist=binomial link=logit;
*			random intercept / subject=Block(Irrigation);
*			title 'field iso crown B step 2-1 - diagnose dispersion';
*		run;

		* RESULTS: 
			2018) no errors but F values are nonsensical (infinity)
			2019) no errors but some F values are nonsensical (zero)
			2020) no errors but Pearson/DF = 3.51
		
	** Step 2-2 **;
		* OBJ: Madden random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel nobound;
*			class Season Irrigation Cultivar Inoculum Block;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum/ dist=binomial link=logit;
*			random intercept Irrigation Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field iso crown B step 2-2 - diagnose dispersion';
*		run;

		* RESULTS: 
			2018) ERROR QUANEW optimization cannot be completed, optimization routine cannot improve function value
			2019) estimated G matrix is not positive definite
			2020) Estimated G matrix is not positive definite. 
		
	** CONCLUSION: simplify random effect term;


*** Step 3 ***;
	** OBJ: simplify random effects to improve fit;

	** Step 3-1 **;
		* OBJ: basic random effect term;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum / dist=binomial link=logit;
*			random intercept / subject=Block;
*			title 'field iso crown B step 3-1 - diagnose dispersion';
*		run;

		* RESULTS: no errors, 
			2018) infinity F value estimates
			2019) fixed effects estimates don't make sense (either .999 or <.0001)
			2020) estimated G matrix is not positive definite 
				
	** Step 3-2 **;
		* OBJ: simplified Madden random effect term;
			* intercept, irrigation random terms removed because showed 0 estimates in Step 1-2;

*		proc glimmix data=field_iso method=laplace plot=residualpanel;
*			class Season Irrigation Cultivar Inoculum Block;
*			by Season;
*			model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum / dist=binomial link=logit;
*			random Irrigation*Cultivar*Inoculum / subject=Block;
*			title 'field iso crown B step 3-2 - diagnose dispersion';
*		run;

		* RESULTS: no errors
			2018) infinity F value estimates
			2019) fixed effects estimates don't make sense (either .999 or <.0001)
			2020) some F values are zeros
		
	** CONCLUSION: remove random effect


*** Step 4 ***;
	** OBJ: remove random effects to improve fit;

*	proc glimmix data=field_iso method=laplace plot=residualpanel;
*		class Season Irrigation Cultivar Inoculum Block;
*		by Season;
*		model CrownMp / TotalCrown = Irrigation|Cultivar|Inoculum / dist=binomial link=logit;
*		title 'field iso crown B step 4-1 - diagnose dispersion';
*	run;

	** RESULTS: nonsensical, F values all 0
					
	** CONCLUSION: add 1 to all observations to eliminate 0s for 2018 and 2019, mostly 0s for 2020


* ******************************************************************* *
* Appendix C - Test Overdispersion and Random Effects - Adjusted Data *
* ******************************************************************* *;	

	* NOTE: code to redirect output to HTML removed for clarity

*** Step 2 ***;
	** OBJ: add random effects;

*	** Step 2-1 **;
*		* OBJ: basic random effect term;
*
*		proc glimmix data=field_iso_crown_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random intercept / subject=block;
*		title 'field iso crown C test dispersion and random effects - step 2-1 add random effects - simple';
*		run;
*		
*	** Step 2-2 **;
*		* OBJ: simplified Madden random effect term;
*
*		proc glimmix data=field_iso_crown_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random irrigation*cultivar*inoculum / subject=block;
*		title 'field iso crown C test dispersion and random effects - step 2-2 add random effects - simplified Madden';
*		run;
*
*	** Step 2-3 **;
*		** OBJ: Stroup random effect term;
*
*		proc glimmix data=field_iso_crown_adj method=laplace plot=residualpanel;
*			class season irrigation cultivar inoculum block date;
*			by season;
*			model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*			random intercept / subject=block(irrigation);
*		title 'field iso crown C test dispersion and random effects - step 2-3 add random effects - Stroup';
*		run;


* ******************************************* *
* Appendix F - Separate Means - Adjusted Data *
* ******************************************* *;

	* NOTE: code to redirect output to HTML removed for clarity;

*** Step 1 ***;
	** OBJ: separate means using multiple comparisons;
*	proc glimmix data=field_iso_crown_adj plot=residualpanel;
*		class season irrigation cultivar inoculum block date;
*		by season;
*		model pieces_mp_adj / pieces_total = irrigation|cultivar|inoculum date / dist=binomial link=logit;
*
*		slice irrigation*inoculum / sliceBy=inoculum means ilink linestable adjust=tukey;
*
*		title 'field iso crown F separate means - step 1 - multiple comparisons';
*	run;
