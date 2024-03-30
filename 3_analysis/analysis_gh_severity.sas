* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Greenhouse Experiment				 *
* Severity Rating Data		 	     *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\gh-severity;
	%LET results_path_img=&base_path.\4_results\gh-severity\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/gh-severity;
*	%LET results_path_img=&base_path./4_results/gh-severity/html_images;

	** both;
	%LET name_base=gh-severity_; 

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
			datafile="&base_path.\2_data\greenhouse_severity_final_SAS.csv"
			dbms=dlm replace out=gh_severity;
		delimiter=",";
		getnames=YES;
	run;
	
	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/greenhouse_severity_final_SAS.csv"
*			dbms=dlm replace out=gh_severity;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=gh_severity;
	by season irrigation cultivar inoculum block plant date severity;
run;


%output_html_files_on(name_step=A_step-1, title_step=A step 1); * redirect html, log output to files;
** check dataset;
proc print data=gh_severity;
	title 'mac irr gh severity ratings full review';
run;
%output_html_files_off(); * turn off redirecting html, log output to files;
	

* ************************* *
* B. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze main effects, basic random effect term;

	** see Appendix B for code

	** RESULTS: 2019 cultivar*inoculum and cultivar*irrigation interaction, 2020 three-way interaction and irrigation*inoculum
	** CONCLUSION: try repeated measures


* ********************************************* *
* C. Analyze - Repeated Measures - Main Effects *
* ********************************************* *;

*** Step 1 ***;
	** OBJ: analyze main effects, basic random effect term;
		* NOTE: commented out because results repeated in step D below;
		
*	%output_html_files_on(name_step=C_step-1, title_step=C step 1); * redirect html, log output to files;
*	proc glimmix data=gh_severity method=laplace;
*		class season irrigation cultivar inoculum block plant date ;
*		by season;
*		model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*		random date / type=cs subject=irrigation*cultivar*inoculum*block;
*		title 'gh severity C step 1 repeated measures - analyze main effects';
*	run;
*	
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS
		* 2019: irrigation, cultivar
		* 2020: irrigation*inoculum, cultivar
	** CONCLUSION: investigate interaction
		

* ************************ *
* D. Analyze - Interaction *
* ************************ *;	

** split dataset;
	data gh_severity_20;
		set gh_severity;
		if season in ('2019-2020') then delete;
	run;

*** Step 1 ***
	** OBJ: analyze interaction from C above;
		* NOTE: commented out because results repeated in step D below;

*	%output_html_files_on(name_step=D_step-1, title_step=D step 1); * redirect html, log output to files;
*	proc glimmix data=gh_severity_20 method=laplace;
*		class season irrigation cultivar inoculum block plant date ;
*		by season;
*		model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*		random date / type=cs subject=irrigation*cultivar*inoculum*block;
*	
*		slice irrigation*inoculum / sliceBy=inoculum;
*
*		title 'gh severity D step 1 - investigate interaction';
*		run;
*
*	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: effect of irrigation for both inoculum treatments
	
	** CONCLUSION: separate means: 2019-2020 irrigation, 2020-2021 irrigation*inoculum
				

* ************************** *
* E. Analyze - Examine Means *
* ************************** *;	

*** Step 1 ***
	** OBJ: examine means for irrigation 2019-2020;

	** split dataset;
	data gh_severity_19;
		set gh_severity;
		if season in ('2020-2021') then delete;
	run;
	
	%output_html_files_on(name_step=E_step-1, title_step=E step 1); * redirect html, log output to files;
	proc glimmix data=gh_severity_19 method=laplace;
		class season irrigation cultivar inoculum block plant date ;
		by season;
		model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
		random date / type=cs subject=irrigation*cultivar*inoculum*block;
	
		estimate 'low vs optimal' irrigation 0 1 -1 / e;
		estimate 'high vs optimal' irrigation 1 0 -1 / e;

		title 'gh severity E step 1 - examine means, irrigation 2019-2020';
		run;

	%output_html_files_off(); * turn off redirecting html, log output to files;


*** Step 2 ***
	** OBJ: separate means from interaction in D above;
		* NOTE: commented out because results repeated in step D below;

	%output_html_files_on(name_step=E_step-2, title_step=E step 2); * redirect html, log output to files;
	proc glimmix data=gh_severity_20 method=laplace;
		class season irrigation cultivar inoculum block plant date ;
		by season;
		model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
		random date / type=cs subject=irrigation*cultivar*inoculum*block;
	
		slice irrigation*inoculum / sliceBy=inoculum;
		
		estimate 'control: low vs optimal'
			irrigation 0 1 -1
			irrigation*inoculum 0 0 1 0 -1 0 / e;
		
		estimate 'control: optimal vs high'
			irrigation 1 0 -1
			irrigation*inoculum 1 0 0 0 -1 0 / e;

		estimate 'inoculated: low vs optimal'
			irrigation 0 1 -1
			irrigation*inoculum 0 0 0 1 0 -1 / e;
			
		estimate 'inoculated: optimal vs high'
			irrigation 1 0 -1
			irrigation*inoculum 0 1 0 0 0 -1 / e;
			
		title 'gh severity E step 2 - examine means, irrigation*inoculum 2020-2021';
		run;

	%output_html_files_off(); * turn off redirecting html, log output to files;


*** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;



* *********************************** *
* Appendix B - Analyze - Main Effects *
* *********************************** *;

*** Step 1 ***;
	** OBJ: analyze main effects, basic random effect term;
*	proc glimmix data=gh_severity method=laplace;
*		class season irrigation cultivar inoculum block plant date ;
*		by season;
*		model severity = irrigation|cultivar|inoculum date / dist=multinomial link=cumlogit;
*		random intercept / subject=block;
*		title 'gh severity B step 1 - analyze main effects';
*	run;

