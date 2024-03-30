##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Functions				 						 #
##################################################


### severity - normalize number of plants in each severity rating to number of plants in a treatment
	# df.x = dataframe in format of data.dis above
	# summ.cols = character vector of columns to summarize by
	# n.plants = # of plants in each treatment (# plants per plot * # reps)

rating.normalize = function(df.x, summ.cols, n.plants) {
	## get sums
	df.summ = df.x %>% 
		group_by(season, across({{summ.cols}}) ) %>%
		summarize(
			rat_0=sum(severity == "0", na.rm=TRUE),
			rat_1=sum(severity == "1", na.rm=TRUE),
			rat_2=sum(severity == "2", na.rm=TRUE),
			rat_3=sum(severity == "3", na.rm=TRUE),
			rat_4=sum(severity == "4", na.rm=TRUE),
			rat_5=sum(severity == "5", na.rm=TRUE),
			miss=sum(is.na(severity) ),
			n_plants_treatment=n() ) %>%
		ungroup()
	## gather
	df.summ = df.summ %>% gather(key="severity", value="n_plants_rating", -season, -all_of({{summ.cols}}), -n_plants_treatment)
	## remove "rat_"
	df.summ = df.summ %>% mutate(severity=str_replace(severity, "rat_", "") )
	## reorder columns
	df.summ = df.summ %>% select(season, all_of({{summ.cols}}), severity, n_plants_rating, n_plants_treatment)
	## arrange
	df.summ = df.summ %>% arrange(season, across({{summ.cols}}), severity)
	## normalize to number of plants
	df.summ = df.summ %>% mutate(n_plants_norm=round( (n_plants_rating / n_plants_treatment) * n.plants, digits=1) )
	
	return(df.summ)
}


### mortality - summarize counts, calculate averages 
	# df.x = dataframe in the format of data.dis above
	# rating.str = rating type (charcoal rot or mite)
	# summ.cols = treatment columns to group by for the summary (irrigation, cultivar, inoculum, and/or block)
	# summ.date = group by date for summary ("date" or NULL)
	
summarize.mort = function(df.x, rating.str, summ.cols, summ.date) {
	## get sums
	df.summ = df.x %>% 
		filter(rating == rating.str) %>%
		group_by(season, across({{summ.cols}}), period, across({{summ.date}})) %>%
		summarize(
			severity_mort=sum(severity == "5", na.rm=TRUE),	severity_present=sum(severity_status == "present", na.rm=TRUE),
			observation_mort=sum(observation == "dead", na.rm=TRUE), observation_present=sum(observation_status == "present", na.rm=TRUE),
			n_row=n() ) %>%
		ungroup()
	## remove values for severity_ during observational period
	df.summ = df.summ %>% mutate(
		severity_mort=replace(severity_mort, period == "observation", NA),
		severity_present=replace(severity_present, period == "observation", NA) )
	## calculate 
	df.summ = df.summ %>% mutate(
		severity_mort_perc=round( (severity_mort/severity_present) * 100, digits=0), 
		observation_mort_perc=round( (observation_mort/observation_present) * 100, digits=0) )
	
	return(df.summ)
}


### symptom incidence, mites - summarize counts, calculate averages 
	# df.x = dataframe in the format of data.dis above
	# rating.str = rating type (charcoal rot or mite)
	# summ.cols = treatment columns to group by for the summary (irrigation, cultivar, inoculum, and/or block)
	# summ.date = group by date for summary ("date" or NULL)
	
summarize.symp.mite = function(df.x, rating.str, summ.cols, summ.date) {
	## get sums
	df.summ = df.x %>% 
		filter(rating == rating.str) %>%
		group_by(season, across({{summ.cols}}), period, across({{summ.date}})) %>%
		summarize(
			severity_symp=sum(severity %in% c("2","3","4"), na.rm=TRUE),	
			severity_present=sum(severity_status == "present", na.rm=TRUE),
			n_row=n() ) %>%
		ungroup()
	## remove values for severity_ during observational period
	df.summ = df.summ %>% mutate(
		severity_symp=replace(severity_symp, period == "observation", NA),
		severity_present=replace(severity_present, period == "observation", NA) )
	## calculate 
	df.summ = df.summ %>% mutate(
		severity_symp_perc=round( (severity_symp/severity_present) * 100, digits=0) )
	
	return(df.summ)
}
