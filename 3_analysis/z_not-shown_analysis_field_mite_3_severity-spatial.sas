* ********************************** *
* STRAWBERRY Macrophomina irrigation *
* Field Experiment					 *
* Mite Severity - Spatial covariates *
* ********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\07_SB_SCRI-pathology\SAS_Mac\irrigation;
	%LET results_path=&base_path.\4_results\z_not-shown_mite-severity-spatial;
	%LET results_path_img=&base_path.\4_results\z_not-shown_mite-severity-spatial\html_images;

	** to SAS ODA/SAS Studio;
*	%LET base_path=/home/u63629950/sb-mac-irr;
*	%LET results_path=&base_path./4_results/z_not-shown_mite-severity-spatial;
*	%LET results_path_img=&base_path./4_results/z_not-shown_mite-severity-spatial/html_images;
	
	** both;
	%LET name_base=z_not-shown_mite-severity-spatial_; 

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
			datafile="&base_path.\2_data\z_not-shown_severity_final_mite-severity_dead-removed_spatial-covariates_SAS.csv"
			dbms=dlm replace out=field_mite_severity_spatial;
		delimiter=",";
		getnames=YES;
	run;

	* SAS ODA/SAS Studio;
*	proc import 
*			datafile="&base_path./2_data/z_not-shown_severity_final_mite-severity_dead-removed_spatial-covariates_SAS.csv"
*			dbms=dlm replace out=field_mite_severity_spatial;
*		delimiter=",";
*		getnames=YES;
*	run;

** sort dataset;
proc sort data=field_mite_severity_spatial;
	by irrigation cultivar inoculum cultivar_inoculum block plant date;
run;

** check dataset;
proc print data=field_mite_severity_spatial;
	title "mac irr field mite severity full review" ;
run;

*** direct log back to SAS Log window;
proc printto; run; 
	
** NOTE: code blocks used for initial testing and optimization were moved to appendices
	* whereas code used in the final analysis workflow (e.g., that random effect term was selected) were not moved but commented out because subsequent steps provide same results


* *********************** *
* B. Test Spatial Models  *
* *********************** *;

*** Step 1 ***;
	** OBJ: test spatial models;
		* use Madden random effect term (identified in non-spatial analysis)

	** Step 1-1 **
		* OBJ: original non-spatial analysis;
		%output_html_files_on(name_step=B_step-1-1, title_step=B Step 1-1); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=Block;
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-1 original non-spatial';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 1-2 **
		* OBJ: resid_standard;
		%output_html_files_on(name_step=B_step-1-2, title_step=B Step 1-2); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum resid_standard / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=Block;
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-2 resid_standard';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 1-3 **
		* OBJ: distance_topleft;
		%output_html_files_on(name_step=B_step-1-3, title_step=B Step 1-3); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum distance_topleft / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=Block;
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-3 distance_topleft';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 1-4 **
		* OBJ: sp(exp);
		%output_html_files_on(name_step=B_step-1-4, title_step=B Step 1-4); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=block type=sp(exp) (x y);
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-4 sp(exp)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 1-5 **
		* OBJ: sp(pow);
		%output_html_files_on(name_step=B_step-1-5, title_step=B Step 1-5); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=block type=sp(pow) (x y);
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-5 sp(pow)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 1-6 **
		* OBJ: sp(sph);
		%output_html_files_on(name_step=B_step-1-6, title_step=B Step 1-6); * redirect html, log output to files;

		proc glimmix data=field_mite_severity_spatial method=laplace;
			class irrigation cultivar inoculum block plant date;
			model severity = irrigation|cultivar|inoculum / dist=multinomial link=cumlogit;
			random intercept irrigation irrigation*cultivar*inoculum / subject=block type=sp(sph) (x y);
			covtest glm / wald;
			title 'field mite severity B test spatial models - Madden random effect - Step 1-6 sp(sph)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* Steps 1-2, 1-3: ran normally, results similar to Step 1-1 (non-spatial)
		* spatial autocorr (Steps 1-4, 1-5, 1-6): did not run properly
		
	** CONCLUSION: spatial term did not change results, analysis complete
