* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Mite Severity 					 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\mite-severity;
	%LET results_path_img=&base_path.\4_results\mite-severity\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/mite-severity;
*	%LET results_path_img=&base_path./4_results/mite-severity/html_images;

	** both;
	%LET name_base=mite-severity_; 

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
proc printto new log="&results_path./&name_base.A_sas-log.log"; run; 

** import data;
	* local;
	proc import 
			datafile="&base_path.\2_data\severity_final_mite-rating_dead-removed_SAS.csv"
			dbms=dlm replace out=field_mite_severity;
		delimiter=",";
		getnames=YES;
	run;
	
	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/severity_final_mite-rating_dead-removed_SAS.csv"
*			dbms=dlm replace out=field_mite_severity;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_mite_severity;
	by irrigation cultivar inoculum block plant date;
run;

** check dataset;
proc print data=field_mite_severity;
	title "mac irr field mite severity full review" ;
run;

*** direct log back to SAS Log window;
proc printto; run; 
	
** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results


* *********************** *
* B. Test Random Effects  *
* *********************** *;

*** Step 1 ***;
	** OBJ: test simple random effects;
	** see Appendix B for code
	** RESULTS: both ran normally
	** CONCLUSION: test advanced random effects


*** Step 2 ***;
	** OBJ: test advanced random effects;
	
	** Step 2-1, 2-2 **;
		* see Appendix B for code
		* RESULTS: both ran normally
		
	** Step 2-3 **;
		* OBJ: Madden random effect;
			* NOTE: commented out because Section F provides the same results;
*		%output_html_files_on(name_step=B_step-2-3, title_step=B step 2-3); * redirect html, log output to files;
*		proc glimmix data=field_mite_severity method=laplace;
*			class irrigation cultivar inoculum block plant date;
*			model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field mite severity B test random effects - step 2-3 Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: ran normally, had lowest fit statistics of all 5 steps
			* cultivar, weak irrigation*inoculum
	
	** CONCLUSION: investigate interaction use full Madden term
	
	
* ************************** *
* C. Investigate Interaction *
* ************************** *;

*** Step 1 ***;
	** OBJ: analyze interaction;

	%output_html_files_on(name_step=C_step-1, title_step=C step 1); * redirect html, log output to files;
	proc glimmix data=field_mite_severity method=laplace;
		class irrigation cultivar inoculum block plant date;
		model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
		random intercept irrigation irrigation*cultivar*inoculum / subject=block;
		
		slice irrigation*inoculum / sliceBy=inoculum;
		
		title 'field mite severity C investigate interaction - step 1';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: neither slice significant
	
	** CONCLUSION: try comparing irrigation treatments with estimate statements;


* ************************** *
* D. Investigate Interaction *
* ************************** *;

*** Step 1 ***;
	** OBJ: compare irrigation treatments with estimate statements;

	%output_html_files_on(name_step=D_step-1, title_step=D step 1); * redirect html, log output to files;
	proc glimmix data=field_mite_severity method=laplace;
		class irrigation cultivar inoculum block plant date;
		model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
		random intercept irrigation irrigation*cultivar*inoculum / subject=block;
		
		slice irrigation*inoculum / sliceBy=inoculum;

		estimate 'Control: opti vs high'
			Irrigation	1	0	-1
			Irrigation*Inoculum	1	0	0	0	-1	0 / e;
			
		estimate 'Control: opti vs low'
			Irrigation	0	1	-1
			Irrigation*Inoculum	0	0	1	0	-1	0 / e;
			
		estimate 'Inoculated: opti vs high'
			Irrigation	1	0	-1
			Irrigation*Inoculum	0	1	0	0	0	-1 / e;
			
		estimate 'Inoculated: opti vs low'
			Irrigation	0	1	-1
			Irrigation*Inoculum	0	0	0	1	0	-1 / e;
		
		title 'field mite severity D compare treatments - step 1';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: no contrasts significant
	
	** CONCLUSION: analysis complete;



* --------------------APPENDICES-------------------- *;



* *********************** *
* B. Test Random Effects  *
* *********************** *;

**** Step 1 ***;
*	** OBJ: test simple random effects;
*
*	** Step 1-1 **;
*		* OBJ: no random effect;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*
*		proc glimmix data=field_mite_severity method=laplace;
*			class irrigation cultivar inoculum block plant date;
*			model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
*			title 'field mite severity B test random effects - step 1-1 no random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-2 **;
*		* OBJ: basic random effect;
*		%output_html_files_on(name_step=B_step-1-2, title_step=B step 1-2); * redirect html, log output to files;
*
*		proc glimmix data=field_mite_severity method=laplace;
*			class irrigation cultivar inoculum block plant date;
*			model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			title 'field mite severity B test random effects - step 1-2 basic random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: both ran normally
*	
*	** CONCLUSION: test advanced random effects
*
*
**** Step 2 ***;
*	** OBJ: test advanced random effects;
*
*	** Step 2-1 **;
*		* OBJ: Stroup random effect;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*
*		proc glimmix data=field_mite_severity method=laplace;
*			class irrigation cultivar inoculum block plant date;
*			model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			title 'field mite severity B test random effects - step 2-1 Stroup random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 2-2 **;
*		* OBJ: simplified Madden random effect;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=field_mite_severity method=laplace;
*			class irrigation cultivar inoculum block plant date;
*			model severity = irrigation|cultivar|inoculum  / dist=multinomial link=cumlogit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field mite severity B test random effects - step 2-2 simplified Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

