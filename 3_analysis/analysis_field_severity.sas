* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Charcoal rot severity				 *
* Total severity					 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\charcoal-severity;
	%LET results_path_img=&base_path.\4_results\charcoal-severity\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/charcoal-severity;
*	%LET results_path_img=&base_path./4_results/charcoal-severity/html_images;

	** both;
	%LET name_base=charcoal-severity_; 

*** load macros for controlling output;
	** local;
	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
*	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ********* *
* A. Import *
* ********* *;

*** save log to file;
proc printto new log="&results_path/&name_base.A_sas-log.log"; run; 

** import data;
	* local;
	proc import 
			datafile="&base_path.\2_data\severity_final_charcoal-rating_dead-removed_SAS.csv"
			dbms=dlm replace out=field_severity;
		delimiter=",";
		getnames=YES;
	run;

	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/severity_final_charcoal-rating_dead-removed_SAS.csv"
*			dbms=dlm replace out=field_severity;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_severity;
	by season irrigation cultivar inoculum block plant date;
run;

*ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
*proc print data=field_severity;
*	title 'field severity ratings full review';
*run;
*ods html close; * saves html;

*** direct log back to SAS Log window;
proc printto; run; 
	
** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results

	
* ************************************ *
* B. Test Random Effects - 3-way model *
* ************************************ *;

*** sources blended
	** split plot design: Stroup 2014 Agronomy J 811-supplement7.sas
	** second random effect option Madden, Kriss GLMM workshop APS 2016 CaseStudy2.sas

*** Step 1 ***;
	** OBJ: test simple random effects;

	** Step 1-1 **;
		* OBJ: basic random effect term;
		* See Appendix B for code
		* RESULTS: 3-way interaction, date significant all seasons
			* AIC: 7298, 12459, 18535 | cov parm estimates (total): 0.5030, 0.06432, 0.07963;
		
	** Step 1-2 **;
		* OBJ: simplified Madden random effect term;
		* See Appendix B for code
		* RESULTS: no 3-way interaction, all seasons
			* AIC: 7061, 12354, 18228 | cov parm estimates (total): 0.9241, 0.1686, 0.3732;
	
	** CONCLUSION: test advanced random effects
	
*** Step 2 ***;
	** OBJ: test advanced random effects;

	** Step 2-1 **;
		* OBJ: Stroup random effect term;
		* See Appendix B for code
		* RESULTS: 3-way interaction, date significant all seasons
			* AIC: 7245, 12397, 18461 | cov parm estimate (total): 0.6033, 0.1309, 0.1639;
		
	** Step 2-2 **;
		* OBJ: Madden random effect term;
		* See Appendix B for code
		* RESULTS: 2018, irrigation*cultivar, date | 2019, irrigation*inoculum, cultivar*inoculum, date | 2020: irrigation*inoculum, date
			* AIC: 7055, 12383, 18225 | cov parm estimates (total): 0.92, 0.16, 0.37 (approx. same as simplified Madden);
	
	** CONCLUSION: test 4-way model;


* ************************************ *
* C. Test Random Effects - 4-way model *
* ************************************ *;

*** Step 1 ***;
	** OBJ: test simple random effects;

	** Step 1-1 **;
		* OBJ: basic random effect term;
		* See Appendix C for code
		* RESULTS: 2018, 4-way interaction | 2019, 4-way interaction | 2020, irrigation*cultivar*inoculum, irrigation*date, inoculum*date
			* AIC: 7221, 12257, 18515 | cov parm estimates (total): 0.5309, 0.06848, 0.08396
		
	** Step 1-2 **;
		* OBJ: simplified Madden random effect term;
		* See Appendix C for code
		* RESULTS: 2018, 4-way interaction | 2019, 4-way interaction | 2020, irrigation*inoculum, irrigation*date, cultivar*date, inoculum*date
			* AIC: 6932, 12184, 18146 | cov parm estimates (total): 1.0000, 0.1850, 0.3993
	
	** CONCLUSION: test advanced random effects
	
*** Step 2 ***;
	** OBJ: test advanced random effects;

	** Step 2-1 **;
		* OBJ: Stroup random effect term;
		* See Appendix C for code
		* RESULTS: 3-way interaction, date significant all seasons | 2018, 4-way interaction | 2019, 4-way interaction | 2020, irrigation*cultivar*inoculum, irrigation*date, cultivar*date, inoculum*date
			* AIC: 7124, 12190, 18388 | cov parm estimate (total): 0.6523, 0.1417, 0.1767
		
	** Step 2-2 **;
		* OBJ: Madden random effect term;
		* See Appendix C for code
		* RESULTS: 2018, 4-way interaction | 2019, 4-way interaction | 2020, irrigation*inoculum, irrigation*date, cultivar*date, inoculum*date
			* AIC: 6926, 12173, 18142 | cov parm estimates (total): 1.00, 0.1827, 0.3991 (approx. same as simplified Madden)
	
	** CONCLUSION: test repeated measures


* ****************************************** *
* D. Test Random Effects - Repeated Measures *
* ****************************************** *;

*** Step 1 ***;
	** OBJ: test different structures for basic random effect;
		* type=un did not work in initial testing
	** see Appendix D for code
	** RESULTS: 
		* cs ran normally
		* csh, ar(1), ante(1) failed;
	** CONCLUSION: try Stroup random effect;


*** Step 2 ***;
	** OBJ: test different structures for Stroup random effect;
		* type=un did not work in initial testing
		
	** Step 2-1 **;
		* OBJ: compound symmetry;
		* NOTE: commented out because Section F provides the same results;
*		%output_html_files_on(name_step=D_step-2-1, title_step=D step 2-1); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 2-1 cs';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: ran normally
			* 2018: irrigation*cultivar
			* 2019: irrigation*cultivar, irrigation*inoculum, cultivar*inoculum
			* 2020: irrigation*inoculum, weak cultivar*inoculum
		
	** Steps 2-2, 2-3, 2-4 **;
		* see Appendix D for code
		* RESULTS: csh, ar(1), ante(1) all failed
	
	** CONCLUSION: try more advanced random effects for type=cs
	
	
*** Step 3 ***;
	** OBJ: test advanced random effects for type=cs;
	** see Appendix D for code
	** RESULTS: both simplified Madden, Madden random effect terms failed
	** CONCLUSION: use simple random effects

*** CONCLUSION: investigate interactions using random type=cs with Stroup term


* *********************************************** *
* E. Investigate Interactions - Repeated Measures *
* *********************************************** *;	

*** create datasets;
	data field_severity_18;
		set field_severity;
		if season in ('2019-2020','2020-2021') then delete;
	run;

	data field_severity_19;
		set field_severity;
		if season in ('2018-2019','2020-2021') then delete;
	run;

	data field_severity_20;
		set field_severity;
		if season in ('2018-2019','2019-2020') then delete;
	run;

*** Step 1 ***
	** OBJ: analyze interactions from D above
	* NOTE: commented out because Section F provides the same results

*	** Step 1-1 **;
*		* OBJ: 2018;
*		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
*
*		proc glimmix data=field_severity_18 method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			
*			slice irrigation*cultivar / sliceBy=cultivar;
*			
*			title 'field severity E repeated measures investigate interaction - step 1-1 2018';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*
*	** Step 1-2 **;
*		* OBJ: 2019;
*		%output_html_files_on(name_step=E_step-1-2, title_step=E step 1-2); * redirect html, log output to files;
*
*		proc glimmix data=field_severity_19 method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			
*			slice irrigation*cultivar / sliceBy=cultivar;
*			slice irrigation*inoculum / sliceBy=inoculum;
*			slice cultivar*inoculum / sliceBy=inoculum;
*			
*			title 'field severity E repeated measures investigate interaction - step 1-2 2019';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-3 **;
*		* OBJ: 2020;
*		%output_html_files_on(name_step=E_step-1-3, title_step=E step 1-3); * redirect html, log output to files;
*
*		proc glimmix data=field_severity_20 method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			
*			slice irrigation*inoculum / sliceBy=inoculum;
*			slice cultivar*inoculum / sliceBy=inoculum;
*			
*			title 'field severity E repeated measures investigate interaction - step 1-3 2020';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;


* ************************************* *
* F. Separate Means - Repeated Measures *
* ************************************* *;	

*** Step 1 ***
	** OBJ: separate means from E above using estimate statements

	** Step 1-1 **;
		* OBJ: 2018;
		%output_html_files_on(name_step=F_step-1-1, title_step=F step 1-1); * redirect html, log output to files;

		proc glimmix data=field_severity_18 method=laplace;
			class season irrigation cultivar inoculum block plant date ;
			by season;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept / subject=block(irrigation);
			random date / type=cs subject=irrigation*cultivar*inoculum*block;
			
			slice irrigation*cultivar / sliceBy=cultivar;
			
			estimate 'Petaluma: opti vs high'	
				Irrigation	1	0	-1
				Irrigation*Cultivar	1	0	0	0	-1	0 / e;
				
			estimate 'Petaluma: opti vs low'
				Irrigation	0	1	-1
				Irrigation*Cultivar	0	0	1	0	-1	0 / e;
			
			title 'field severity F repeated measures separate means - step 1-1 2018';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;


	** Step 1-2 **;
		* OBJ: 2019;
		%output_html_files_on(name_step=F_step-1-2, title_step=F step 1-2); * redirect html, log output to files;

		proc glimmix data=field_severity_19 method=laplace;
			class season irrigation cultivar inoculum block plant date ;
			by season;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept / subject=block(irrigation);
			random date / type=cs subject=irrigation*cultivar*inoculum*block;
			
			slice irrigation*cultivar / sliceBy=cultivar;
			slice irrigation*inoculum / sliceBy=inoculum;
			slice cultivar*inoculum / sliceBy=inoculum;
			
			estimate 'Fronteras: opti vs high'	
				Irrigation	1	0	-1
				Irrigation*Cultivar	1	0	0	0	-1	0 / e;
				
			estimate 'Fronteras: opti vs low'
				Irrigation	0	1	-1
				Irrigation*Cultivar	0	0	1	0	-1	0 / e;
				
			estimate 'Inoculated: opti vs high'
				Irrigation	1	0	-1
				Irrigation*Inoculum	0	1	0	0	0	-1 / e;
				
			estimate 'Inoculated: opti vs low'
				Irrigation	0	1	-1
				Irrigation*Inoculum	0	0	0	1	0	-1 / e;
			
			title 'field severity F repeated measures separate means - step 1-2 2019';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;


	** Step 1-3 **;
		* OBJ: 2020;
		%output_html_files_on(name_step=F_step-1-3, title_step=F step 1-3); * redirect html, log output to files;

		proc glimmix data=field_severity_20 method=laplace;
			class season irrigation cultivar inoculum block plant date ;
			by season;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept / subject=block(irrigation);
			random date / type=cs subject=irrigation*cultivar*inoculum*block;
			
			slice irrigation*inoculum / sliceBy=inoculum;
			slice cultivar*inoculum / sliceBy=inoculum;
			
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
		
			title 'field severity F repeated measures separate means interaction - step 1-3 2020';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
		
	** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;



* NOTE: code to redirect output to HTML removed for clarity

* ********************************************** *
* Appendix B - Test Random Effects - 3-way model *
* ********************************************** *;

*** Step 1 ***;
*	** OBJ: test simple random effects;
*
*	** Step 1-1 **;
*		* OBJ: basic random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum date / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			title 'field severity B 3-way test random effects - step 1-1 basic';
*		run;
*		
*	** Step 1-2 **;
*		* OBJ: simplified Madden random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum date / dist=multinomial link=cumlogit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field severity B 3-way test random effects - step 1-2 simplified Madden';
*		run;
*		
**** Step 2 ***;
*	** OBJ: test advanced random effects;
*
*	** Step 2-1 **;
*		* OBJ: Stroup random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum date / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			title 'field severity B 3-way test random effects - step 2-1 Stroup';
*		run;
*		
*	** Step 2-2 **;
*		* OBJ: Madden random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum date / dist=multinomial link=cumlogit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field severity B 3-way test random effects - step 2-2 Madden';
*		run;


* ********************************************** *
* Appendix C - Test Random Effects - 4-way model *
* ********************************************** *;

**** Step 1 ***;
*	** OBJ: test simple random effects;
*
*	** Step 1-1 **;
*		* OBJ: basic random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum|date / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			title 'field severity C 4-way test random effects - step 1-1 basic';
*		run;
*		
*	** Step 1-2 **;
*		* OBJ: simplified Madden random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum|date / dist=multinomial link=cumlogit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field severity C 4-way test random effects - step 1-2 simplified Madden random effect';
*		run;
*	
**** Step 2 ***;
*	** OBJ: test advanced random effects;
*
*	** Step 2-1 **;
*		* OBJ: Stroup random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum|date / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			title 'field severity C 4-way test random effects - step 2-1 Stroup random effect';
*		run;

*	** Step 2-2 **;
*		* OBJ: Madden random effect term;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum|date / dist=multinomial link=cumlogit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field severity C 4-way test random effects - step 2-2 Madden random effect';
*		run;


* *************************************************** *
* Appendix D. Test Random Effects - Repeated Measures *
* *************************************************** *;

*** Step 1 ***;
	** OBJ: test different structures for basic random effect;
		* type=un did not work in initial testing
		
*	** Step 1-1 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 1-1 cs';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: appeared to run normally
*		
*	** Step 1-2 **;
*		* OBJ: heterogenous compound symmetry;
*		%output_html_files_on(name_step=D_step-1-2, title_step=D step 1-2); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			random date / type=csh subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 1-2 csh';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: run failed for 2018 and 2019
*
*	** Step 1-3 **;
*		* OBJ: auto regressive;
*		%output_html_files_on(name_step=D_step-1-3, title_step=D step 1-3); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			random date / type=ar(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 1-3 ar(1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for 2019, 2020
*
*	** Step 1-4 **;
*		* OBJ: ante-dependence;
*		%output_html_files_on(name_step=D_step-1-4, title_step=D step 1-4); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block;
*			random date / type=ante(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 1-4 ante(1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for all years
*


**** Step 2 ***;
*	** OBJ: test different structures for Stroup random effect;
*		* type=un did not work in initial testing
*				
*	** Step 2-2 **;
*		* OBJ: heterogenous compound symmetry;
*		%output_html_files_on(name_step=D_step-2-2, title_step=D step 2-2); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=csh subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 2-2 csh';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: run failed for 2019
*
*	** Step 2-3 **;
*		* OBJ: auto regressive;
*		%output_html_files_on(name_step=D_step-2-3, title_step=D step 2-3); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=ar(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 2-3 ar(1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for 2019, 2020
*
*	** Step 2-4 **;
*		* OBJ: ante-dependence;
*		%output_html_files_on(name_step=D_step-2-4, title_step=D step 2-4); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept / subject=block(irrigation);
*			random date / type=ante(1) subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - basic random effect step 2-4 ante(1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for 2019, 2020
	
**** Step 3 ***;
*	** OBJ: test advanced random effects for type=cs;
*		
*	** Step 3-1 **;
*		* OBJ: simplified Madden random effect;
*		%output_html_files_on(name_step=D_step-3-1, title_step=D step 3-1); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random irrigation*cultivar*inoculum / subject=block;
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - cs step 3-1 simplified Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for all years
*
*	** Step 3-2 **;
*		* OBJ: Madden random effect;
*		%output_html_files_on(name_step=D_step-3-2, title_step=D step 3-2); * redirect html, log output to files;
*
*		proc glimmix data=field_severity method=laplace;
*			class season irrigation cultivar inoculum block plant date ;
*			by season;
*			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			random date / type=cs subject=irrigation*cultivar*inoculum*block;
*			title 'field severity D repeated measures test random effects - cs step 3-2 Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: failed for all years
