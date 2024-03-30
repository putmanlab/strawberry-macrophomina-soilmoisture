* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Mite severity index				 *
* For nearest neighbor residuals	 *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\z_not-shown_mite-calc-covar;
	%LET results_path_img=&base_path.\4_results\z_not-shown_mite-calc-covar\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/z_not-shown_mite-calc-covar;
*	%LET results_path_img=&base_path./4_results/z_not-shown_mite-calc-covar/html_images;

	** both;
	%LET name_base=z_not-shown_mite-calc-covar_; 

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
			datafile="&base_path.\2_data\z_not-shown_severity_final_mite-index_dead-removed_SAS.csv"
			dbms=dlm replace out=field_mite_index;
		delimiter=",";
		getnames=YES;
	run;

	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/z_not-shown_severity_final_mite-index_dead-removed_SAS.csv"
*			dbms=dlm replace out=field_mite_index;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_mite_index;
	by season irrigation cultivar inoculum block;
run;

** check dataset;
proc print data=field_mite_index;
	title "mac irr field mite severity full review" ;
run;

*** direct log back to SAS Log window;
proc printto; run; 
	
** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results


* ****************************************** *
* B. Test Overdispersion and Random Effects  *
* ****************************************** *;

*** Step 1 ***;
	** OBJ: test simple random effects;

	** Step 1-1 **;
		* OBJ: no random effect;
		* see Appendix B for code
		* RESULTS: ran normally

	** Step 1-2 **;
		* OBJ: basic random effect;
		* see Appendix B for code
		* RESULTS: ran normally

*** Step 2 ***;
	** OBJ: test advanced random effects;

	** Step 2-1 **;
		* OBJ: Stroup random effect;
		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;

		proc glimmix data=field_mite_index method=laplace;
			class irrigation cultivar inoculum block;
			model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
			random intercept / subject=block(irrigation);
			title 'field mite severity index B test random effects - step 2-1 Stroup random effect';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: ran normally
		
	** Step 2-2 **;
		* OBJ: simplified Madden random effect;
		* see Appendix B for code
		* RESULTS: failed, not positive definite
				
	** Step 2-3 **;
		* OBJ: Madden random effect;
		* see Appendix B for code
		* RESULTS: failed, missing SE estimate

	** RESULTS: Stroup effect term had lowest fit statistics
	
	** CONCLUSION: analyze with method=laplace removed


* ************************ *
* C. Analyze Main Effects  *
* ************************ *;

*** Step 1 ***;
	** OBJ: analyze main effects with method=laplace removed, export;
	%output_html_files_on(name_step=C_step-1, title_step=C step 1); * redirect html, log output to files;

	proc glimmix data=field_mite_index;
		class irrigation cultivar inoculum block;
		model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
		random intercept / subject=block(irrigation);
		
		output out=mite_severity_index_residual residual(ilink)=mite_index_residual pearson(ilink)=mite_index_pearson;
		
		title 'field mite severity index C analyze main effects - step 1';
	run;
	%output_html_files_off(); * turn off redirecting html, log output to files;

*** export residuals;
	proc export data=mite_severity_index_residual
		outfile="&results_path./severity_mite-index_residuals.csv"
		dbms=csv
		replace;
	run;


* --------------------APPENDICES-------------------- *;



* **************************************************** *
* Appendix B - Test Overdispersion and Random Effects  *
* **************************************************** *;

**** Step 1 ***;
*	** OBJ: test simple random effects;
*
*	** Step 1-1 **;
*		* OBJ: no random effect;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*
*		proc glimmix data=field_mite_index method=laplace;
*			class irrigation cultivar inoculum block;
*			model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
*			title 'field mite severity index B test random effects - step 1-1 no random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-2 **;
*		* OBJ: basic random effect;
*		%output_html_files_on(name_step=B_step-1-2, title_step=B step 1-2); * redirect html, log output to files;
*
*		proc glimmix data=field_mite_index method=laplace;
*			class irrigation cultivar inoculum block;
*			model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
*			random intercept / subject=block;
*			title 'field mite severity index B test random effects - step 1-2 basic random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
**** Step 2 ***;
*	** OBJ: test advanced random effects;
*				
*	** Step 2-2 **;
*		* OBJ: simplified Madden random effect;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		
*		proc glimmix data=field_mite_index method=laplace;
*			class irrigation cultivar inoculum block;
*			model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
*			random irrigation*cultivar*inoculum / subject=block;
*			title 'field mite severity index B test random effects - step 2-2 simplified Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*						
*	** Step 2-3 **;
*		* OBJ: Madden random effect;
*		%output_html_files_on(name_step=B_step-2-3, title_step=B step 2-3); * redirect html, log output to files;
*		
*		proc glimmix data=field_mite_index method=laplace;
*			class irrigation cultivar inoculum block;
*			model mite_severity_index = irrigation|cultivar|inoculum  / dist=beta link=logit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field mite severity index B test random effects - step 2-3 Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		

* **************************************** *
* Appendix X - Analysis of Mite Incidence  *
* **************************************** *;


** ********************************** *
** STRAWBERRY Macrophomina irrigation *
** Field Experiment					 *
** Mite incidence 					 *
** For nearest neighbor residuals	 *
** ********************************** *;
*
**** set variables for folder locations and base filename used for saving output
*	** local to lab desktop;
*	*%LET results_path=C:\Users\Lab User\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\4_results\mite_severity;
*	*%LET results_path_img=C:\Users\Lab User\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\4_results\mite_severity\html_images;
*	*%LET name_base=mite-severity_; 
*
*	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/mite-incidence;
*	%LET results_path_img=&base_path./4_results/mite-incidence/html_images;
*	%LET name_base=mite-incidence_; 
*
**** load macros for controlling output;
*	** local;
**	%include 'C:\Users\Lab User\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\3_analysis\output_files_macro.sas';
*	
*	** SAS Studio;
*	%include "&base_path./3_analysis/output_files_macro.sas";
*
**options ls=120 nonumber formdlim=' ' pagesize=52 center;
*
*
** ********** *
** A. Import  *
** ********** *;
*
**** save log to file;
*proc printto new log="&results_path./&name_base.A_sas-log.log"; run; 
*
*** import data;
**proc import 
*		datafile="C:\Users\Lab User\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\2_data\severity_final_mite-incidence_dead-removed_SAS.csv"
*		dbms=dlm replace out=field_mite_incidence;
*proc import 
*		datafile="&base_path./2_data/severity_final_mite-incidence_dead-removed_SAS.csv"
*		dbms=dlm replace out=field_mite_incidence;
*	delimiter=",";
*	getnames=YES;
*run;
*
*** sort dataset;
*proc sort data=field_mite_incidence;
*	by season irrigation cultivar inoculum block;
*run;
*
**** adjust values;
*	** 1 added to all values to remove 0s;
*	** 1 added to total because some plots had 100% incidence;
*data field_mite_incidence_adj;
*	set field_mite_incidence;
*	severity_symp_adj = severity_symp + 1;
*	severity_present_adj = severity_present + 1;
*run;
*
*** check dataset;
*proc print data=field_mite_incidence;
*	title "mac irr field mite severity full review" ;
*run;
*
**** direct log back to SAS Log window;
*proc printto; run; 
*	
*** NOTE: code blocks used for initial testing and optimization were moved to appendices
*	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results
*
*
** ****************************************** *
** B. Test Overdispersion and Random Effects  *
** ****************************************** *;
*
*
**** Step 1 ***;
*	** OBJ: test simple random effects;
*
*	** Step 1-1 **;
*		* OBJ: no random effect;
*	** Step 1-2 **;
*		* OBJ: basic random effect;
*
*	** see Appendix B for code
*	** RESULTS: both ran normally, overdispersion very high
*	
*	** CONCLUSION: test advanced random effects
*
*
**** Step 2 ***;
*	** OBJ: test advanced random effects;
*
*	** Step 2-1 **;
*		* OBJ: Stroup random effect;
*		* see Appendix B for code
*		* RESULTS: ran normally, overdispersion a little high
*
*	** Step 2-2 **;
*		* OBJ: simplified Madden random effect;
*		* see Appendix B for code
*		
*	** Step 2-3 **;
*		* OBJ: Madden random effect;
*		%output_html_files_on(name_step=B_step-2-3, title_step=B step 2-3); * redirect html, log output to files;
*		
*		proc glimmix data=field_mite_incidence_adj method=laplace;
*			class irrigation cultivar inoculum block;
*			model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*			random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*			title 'field mite incidence B test random effects - step 2-3 Madden random effect';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: Madden terms resolved overdispersion
*	
*	** CONCLUSION: analyze effects by removing method=laplace
*
*
** ************************ *
** C. Analyze Main Effects  *
** ************************ *;
*
**** Step 1 ***;
*	** OBJ: analyze main effects using Madden term, removing method=laplace;
*	%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		
*	proc glimmix data=field_mite_incidence_adj;
*		class irrigation cultivar inoculum block;
*		model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
*		random intercept irrigation irrigation*cultivar*inoculum / subject=block;
*		title 'field mite incidence C analyze main effects - step 1';
*	run;
*	%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: no significant effects
*	
*	** CONCLUSION: try disease index
*
*
*
** --------------------APPENDICES-------------------- *;
*
*
*
** *********************** *
** B. Test Random Effects  *
** *********************** *;
*
***** Step 1 ***;
**	** OBJ: test simple random effects;
**
**	** Step 1-1 **;
**		* OBJ: no random effect;
**		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
**
**		proc glimmix data=field_mite_incidence_adj method=laplace;
**			class irrigation cultivar inoculum block;
**			model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
**			title 'field mite incidence B test random effects - step 1-1 no random effect';
**		run;
**		%output_html_files_off(); * turn off redirecting html, log output to files;
**
**	** Step 1-2 **;
**		* OBJ: basic random effect;
**		%output_html_files_on(name_step=B_step-1-2, title_step=B step 1-2); * redirect html, log output to files;
**
**		proc glimmix data=field_mite_incidence_adj method=laplace;
**			class irrigation cultivar inoculum block;
**			model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
**			random intercept / subject=block;
**			title 'field mite incidence B test random effects - step 1-2 basic random effect';
**		run;
**		%output_html_files_off(); * turn off redirecting html, log output to files;
**
**
***** Step 2 ***;
**	** OBJ: test advanced random effects;
**
**	** Step 2-1 **;
**		* OBJ: Stroup random effect;
**		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
**
**		proc glimmix data=field_mite_incidence_adj method=laplace;
**			class irrigation cultivar inoculum block;
**			model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
**			random intercept / subject=block(irrigation);
**			title 'field mite incidence B test random effects - step 2-1 Stroup random effect';
**		run;
**		%output_html_files_off(); * turn off redirecting html, log output to files;
**		
**	** Step 2-2 **;
**		* OBJ: simplified Madden random effect;
**		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
**		
**		proc glimmix data=field_mite_incidence_adj method=laplace;
**			class irrigation cultivar inoculum block;
**			model severity_symp_adj / severity_present_adj = irrigation|cultivar|inoculum  / dist=binomial link=logit;
**			random irrigation*cultivar*inoculum / subject=block;
**			title 'field mite incidence B test random effects - step 2-2 simplified Madden random effect';
**		run;
**		%output_html_files_off(); * turn off redirecting html, log output to files;
**


