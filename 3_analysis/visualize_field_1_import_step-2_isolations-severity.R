##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Import and organize - Step 2					 #
# Pathogen isolations, disease severity			 #
##################################################

## built on Docker putmanlab/exploratory-analysis:420.0

if (!require(conflicted)) {
  install.packages("conflicted")
  library(conflicted)
}

library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")


#####################################################
##### SEVERITY - MANUAL QUALITY CONTROL - INPUT #####
#####################################################

### upload (save .xlsx in 2_data_curated_worksheets as .csv, append "_out" to filename, and move to 2_data_curated)
	in.manual.1 = read_csv(file="./2_data_curated/severity-isolation_manual-qual-control_step-2_2019_out.csv", col_names=T, na="")
	in.manual.2 = read_csv(file="./2_data_curated/severity-isolation_manual-qual-control_step-2_2020_out.csv", col_names=T, na="")
	in.manual.3 = read_csv(file="./2_data_curated/severity-isolation_manual-qual-control_step-2_2021_out.csv", col_names=T, na="")

### organize	
	## gather
	in.manual.1 = in.manual.1 %>% gather(key="date", value="value", -season, -irrigation, -cultivar, -inoculum, -block, -plant, -type, -sort_order, -orphan)
	in.manual.2 = in.manual.2 %>% gather(key="date", value="value", -season, -irrigation, -cultivar, -inoculum, -block, -plant, -type, -sort_order)
	in.manual.3 = in.manual.3 %>% gather(key="date", value="value", -season, -irrigation, -cultivar, -inoculum, -block, -plant, -type, -sort_order)

	## extract rating from date column and fix date column
		# add rating column
		in.manual.3 = in.manual.3 %>% mutate(rating=case_when(
			( str_detect(date, "_cr") == TRUE) ~ "charcoal rot",
			( str_detect(date, "_mite") == TRUE) ~ "mite") )
			
		# replace string
		in.manual.3 = in.manual.3 %>% mutate(date_new=case_when(
			( str_detect(date, "_cr") == TRUE) ~ str_replace(date, "_cr", ""),
			( str_detect(date, "_mite") == TRUE) ~ str_replace(date, "_mite", "") ) )
			
		# remove and rename date column
		in.manual.3 = in.manual.3 %>% select(-date) %>% rename(date=date_new)

	## bind
		# remove unneeded column
		in.manual.1 = in.manual.1 %>% select(-orphan)
	
		# add rating column
		in.manual.1 = in.manual.1 %>% mutate(rating="charcoal rot")
		in.manual.2 = in.manual.2 %>% mutate(rating="charcoal rot")
	
		# convert date column
		in.manual.1 = in.manual.1 %>% mutate(date=mdy(date))
		in.manual.2 = in.manual.2 %>% mutate(date=mdy(date))
		in.manual.3 = in.manual.3 %>% mutate(date=ymd(date))
	
		# bind
		in.manual = bind_rows(in.manual.1, in.manual.2, in.manual.3)
	
	## change column type
	in.manual = in.manual %>% mutate(block=as.character(block), plant=as.integer(plant) )
	
	## remove unneeded column, order columns
	in.comb = in.manual %>% select(season, irrigation, cultivar, inoculum, block, plant, type, rating, date, value)

	## spread		
	in.comb = in.comb %>% spread(key=type, value=value)

	## check
	in.comb %>% group_by(sample_type, severity_orig) %>% summarize(ct=n()) %>% print(n=Inf)
	in.comb %>% group_by(rating, severity_orig) %>% summarize(ct=n()) %>% print(n=Inf)


######################################################
##### SEVERITY - MANUAL QUALITY CONTROL - VERIFY #####
######################################################

### prepare
	## split off severity_orig
	temp.dis = in.comb %>% select(-sample_type)
	
	## group severity_orig
		# missing_present = plant is known to be present based on other dates but only rating data point missing, therefore is included in rating groups
		# sampled_dead_missing = marked as sampled in rating data but missing from isolations data; received rating of 5 on first date, therefore included with _rat5
	temp.dis = temp.dis %>% mutate(severity_orig_grp=case_when(
		(rating == "charcoal rot" & severity_orig %in% c("0","1","2","3","4") ) ~ "cr_04",
		(rating == "charcoal rot" & severity_orig == "5" ) ~ "cr_5",
		(rating == "charcoal rot" & severity_orig == "missing_present" ) ~ "cr_04",
		(rating == "mite" & severity_orig %in% c("1","2","3") ) ~ "m_13",
		(rating == "mite" & severity_orig == "4" ) ~ "m_4",
		(rating == "mite" & severity_orig == "missing_present" ) ~ "m_13",
		(severity_orig == "dead") ~ "dead",
		(severity_orig %in% c("gone","sampled","sampled_diagnosis","sampled_rand_dead") ) ~ "gone",
#		(severity_orig == "gone") ~ "gone",
#		(severity_orig %in% c("sampled","sampled_diagnosis","sampled_rand_dead") ) ~ "sampled",
		(severity_orig %in% c("sampled_diag_rat5","sampled_rand_rat5","sampled_dead_missing") ) ~ "samp_rat5") )
		
		# checks
			# for misses
			temp.dis %>% filter(is.na(severity_orig_grp) & !is.na(severity_orig))
			
			# overall
			temp.dis %>% 
				group_by(rating, severity_orig, severity_orig_grp) %>% 
				summarize(ct=n()) %>% 
				arrange(severity_orig_grp, severity_orig, rating) %>% 
				print(n=Inf)
		
### determine min/max dates for each event; event = rating, event observations (e.g., dead, gone, sampled)
	## summarize
	temp.dis = temp.dis %>%
		group_by(season, irrigation, cultivar, inoculum, block, plant, severity_orig_grp) %>%
		summarize(min=min(date), max=max(date) ) %>%
		ungroup()
		
	## filter out NAs
	temp.dis = temp.dis %>% filter(!is.na(severity_orig_grp))
	
	## spread
		# gather date
		temp.dis = temp.dis %>% gather(key="variable", value="date", -season, -irrigation, -cultivar, -inoculum, -block, -plant, -severity_orig_grp)
		
		# unite column
		temp.dis = temp.dis %>% unite("grp_var", c(severity_orig_grp, variable), remove=TRUE) 
		
		# spread
		temp.dis = temp.dis %>% spread(key=grp_var, value=date)
	
### checks
	## if any ratings are preceded by an event
	temp.dis %>% 
		filter(
			cr_04_min > dead_min | cr_5_min > dead_min | m_13_min > dead_min | m_4_min > dead_min |
			cr_04_min > gone_min | cr_5_min > gone_min | m_13_min > gone_min | m_4_min > gone_min |
#			cr_04_min > sampled_min | cr_5_min > sampled_min | m_13_min > sampled_min | m_4_min > sampled_min |
			cr_04_min > samp_rat5_min | cr_5_min > samp_rat5_min | m_13_min > samp_rat5_min | m_4_min > samp_rat5_min) %>%
		arrange(season, irrigation, cultivar, inoculum, block, plant) %>% 
		print(n=Inf)
		
			# 23 plants in 2020-21 that were observed to be dead shortly before first rating date (4/17)
				# 22 plants on 4/5
				# 1 plant on 3/22
			# disease rating of 5 intentionally included in severity ratings, in contrast to other plants that died well before rating period when disease not expected to be active

	## if dead precedes any sample events
	temp.dis %>% 
		filter(dead_min < gone_min | dead_min < samp_rat5_min) %>%
#		filter(dead_min < sampled_min | dead_min < samp_rat5_min) %>%

		arrange(season, irrigation, cultivar, inoculum, block, plant) %>% print(n=Inf)
		
		# 25 plants
		
		# ensure these are accounted for in severity categories		
#		temp.dis %>%
#			filter(dead_min < sampled_min | dead_min < samp_rat5_min) %>%
#			distinct(season, irrigation, cultivar, inoculum, block, plant) %>%
#			mutate(flag_temp=1) %>%
#			right_join(in.comb, by=c(c("season","irrigation","cultivar","inoculum","block","plant")) ) %>%
#			filter(flag_temp == 1) %>%
#			arrange(season, irrigation, cultivar, inoculum, block, plant, date, rating) %>%
#			print(n=500)

	## if gone precedes or follows any sample events
#	temp.dis %>% 
#		filter(gone_min < dead_min | gone_min > dead_max | gone_max > dead_max | gone_min < sampled_min | gone_min > sampled_max | gone_max > sampled_max ) %>%
#		arrange(season, irrigation, cultivar, inoculum, block, plant) %>% print(n=Inf)
		

#################################################
##### SEVERITY - QUALITY CONTROL - FINALIZE #####
#################################################
			
### prepare	
	## assign periods
	data.dis = in.comb %>% mutate(period=case_when(
		(season == "2018-2019" & !(date %in% as_date(c("2019-05-08","2019-05-22","2019-06-05","2019-06-19")) ) ) ~ "observation",
		(season == "2018-2019" &   date %in% as_date(c("2019-05-08","2019-05-22","2019-06-05","2019-06-19"))   ) ~ "rating",
		(season == "2019-2020" & date <  as_date("2020-04-28") ) ~ "observation",
		(season == "2019-2020" & date >= as_date("2020-04-28") ) ~ "rating",
		(season == "2020-2021" & date <  as_date("2021-04-17") ) ~ "observation",
		(season == "2020-2021" & date >= as_date("2021-04-17") ) ~ "rating") )
	
	## check
		# values
		data.dis %>% group_by(rating, severity_orig) %>% summarize(ct=n()) %>% print(n=Inf)
		data.dis %>% group_by(rating, period, severity_orig) %>% summarize(ct=n()) %>% print(n=Inf)		

		# missing period
		data.dis %>% filter(is.na(period))
		
		# for excess rows
#		data.dis %>% 
#			group_by(season, irrigation, cultivar, inoculum, block) %>% 
#			summarize(ct_plant=n_distinct(plant), ct_date=n_distinct(date), ct_plant_date=(n_distinct(plant)*n_distinct(date)), ct_row=n() ) %>% 
#			filter(ct_plant_date != ct_row) %>%
#			print(n=Inf)

### split value into columns
	# two periods of data
		# observations: before severity ratings started, of events only; for exploratory analysis of mortality
		# ratings: disease severity
	# input values (plant removed from total = plant excluded from n_total when calculating incidence)
		# 0-5 = severity ratings to keep
		# dead = observed to be dead before rating period; plant removed from total for severity ratings, but included in observations
		# gone = missing due to unknown cause; plant removed from total for both ratings and observations
		# missing_present = plant is known to be present (due to ratings/sample event on later date) but rating data point is missing; plant retained in both ratings and observations as "present" but severity, observation data columns left as NA
		# sampled = destructively sampled (random) when plant not observed to be dead or have rating == 5; plant removed from total for both ratings and observations
		# sampled_dead_missing = in severity data, marked as dead during observation period and sampled during rating period, but sample not in isolations data; treated same as _rat5 below
		# sampled_diagnosis = destructively sampled for diagnosis (non-random) when plant not recorded to be dead or have rating == 5; plant removed from total for both ratings and observations
		# sampled_rand_dead = destructively sampled (random) when plant previously observed to be dead; plant removed from total for severity ratings, but included in observations
		# sampled_diag_rat5, sampled_rand_rat5 = destructively sampled when rating of 5 recorded before sampling; plant retained in ratings with rating = 5 carried forward
		
	## severity
		# ratings
		data.dis = data.dis %>% mutate(severity=case_when(
			(severity_orig %in% c("0","1","2","3","4","5") ) ~ severity_orig,
			(severity_orig == "missing_present") ~ NA_character_,
			(severity_orig %in% c("dead","gone","sampled","sampled_diagnosis","sampled_rand_dead") ) ~ NA_character_,
			(severity_orig %in% c("sampled_diag_rat5","sampled_rand_rat5","sampled_dead_missing") & period == "rating" & rating == "charcoal rot") ~ "5",
			(severity_orig %in% c("sampled_diag_rat5","sampled_rand_rat5","sampled_dead_missing") & period == "rating" & rating == "mite") ~ "4",
			(is.na(severity_orig) ) ~ NA_character_) )
	
		# status
		data.dis = data.dis %>% mutate(severity_status=case_when(
			(period == "rating" & severity_orig %in% c("0","1","2","3","4","5") ) ~ "present",
			(period == "rating" & severity_orig == "missing_present") ~ "present",
			(period == "rating" & severity_orig %in% c("dead","gone","sampled","sampled_diagnosis","sampled_rand_dead") ) ~ "absent",
			(period == "rating" & severity_orig %in% c("sampled_diag_rat5","sampled_rand_rat5","sampled_dead_missing") ) ~ "present",
			(period == "rating" & !is.na(sample_type) & is.na(severity_orig) ) ~ "present",
			(period == "rating" &  is.na(sample_type) & is.na(severity_orig) ) ~ "absent",
			(period == "observation" & severity_orig == "missing_present") ~ "present") )
		
	## observations
		# events (sampled_diag_rat5 is present in 2018-19 only, included because was filled in for expediency in manual qual control in spreadsheet, these rating dates not analyzed)
			# need to provide values for both periods, because if excluded dead will not be carried forward and a plant sampled in middle of rating period that was dead before rating would show as NA
				# could be used for exploratory analysis of mortality since planting
		data.dis = data.dis %>% mutate(observation=case_when(
			(period == "observation" & severity_orig %in% c("dead","sampled_rand_dead","sampled_diag_rat5") ) ~ "dead",
			(period == "observation" & severity_orig %in% c("gone","sampled","sampled_diagnosis") ) ~ NA_character_,
			(period == "observation" & severity_orig == "missing_present") ~ "none",
			(period == "observation" & is.na(severity_orig) ) ~ "none",
			(period == "rating" & rating == "charcoal rot" & severity_orig %in% c("0","1","2","3","4") ) ~ "none",
			(period == "rating" & rating == "mite" & severity_orig %in% c("1","2","3") ) ~ "none",
			(period == "rating" & rating == "charcoal rot" & severity_orig == "5") ~ "dead",
			(period == "rating" & rating == "mite" & severity_orig == "4") ~ "dead",
			(period == "rating" & severity_orig %in% c("dead","sampled_rand_dead","sampled_dead_missing","sampled_diag_rat5","sampled_rand_rat5") ) ~ "dead",
			(period == "rating" & severity_orig %in% c("gone","sampled","sampled_diagnosis") ) ~ NA_character_,
			(period == "rating" & severity_orig == "missing_present") ~ "none",
			(period == "rating" & is.na(severity_orig) ) ~ "none") )
				
		# status
		data.dis = data.dis %>% mutate(observation_status=case_when(
			(period == "observation" & severity_orig %in% c("dead","sampled_rand_dead","sampled_diag_rat5") ) ~ "present",
			(period == "observation" & severity_orig == "missing_present") ~ "present",
			(period == "observation" & severity_orig %in% c("gone","sampled","sampled_diagnosis") ) ~ "absent",
			(period == "observation" & is.na(severity_orig ) ) ~ "present",
			(period == "rating" & severity_orig %in% c("0","1","2","3","4","5") ) ~ "present",
			(period == "rating" & severity_orig %in% c("dead","sampled_rand_dead","sampled_dead_missing","sampled_diag_rat5","sampled_rand_rat5") ) ~ "present",
			(period == "rating" & severity_orig %in% c("gone","sampled","sampled_diagnosis") ) ~ "absent",
			(period == "rating" & severity_orig == "missing_present") ~ "present") )
	
	## check
	data.dis %>% 
		group_by(rating, period, sample_type, severity_orig, severity, severity_status, observation, observation_status) %>% 
		summarize(ct=n()) %>% 
		arrange(period, severity_status, severity, observation_status, observation, severity_orig, rating) %>% 
		print(n=Inf)

	## remove unneeded column
	data.dis = data.dis %>% select(-severity_orig)
	

##################################################
##### ISOLATIONS - QUALITY CONTROL - PREPARE #####
##################################################
# assemble status information (same day up to two dates before event, to see if many plants were marked with higher severity two dates before)

### prepare - spread ratings by type
	## remove unneeded columns
	data.dis.t = data.dis %>% select(-severity_status, -observation_status)
	
	## spread
	data.dis.t = data.dis.t %>% spread(key=rating, value=severity)
	
	## rename columns
	data.dis.t = data.dis.t %>% rename(severity_charcoal=`charcoal rot`, severity_mite=mite)

### get status - charcoal rot ratings
	## remove unneeded column
	status.iso.cr = data.dis.t %>% select(-severity_mite)

	## get status
	status.iso.cr = status.iso.cr %>% 
		group_by(season, irrigation, cultivar, inoculum, block, plant) %>%
		mutate(
			event_lag_1=lag(sample_type, n=1),
			event_lag_2=lag(sample_type, n=2),
			severity_cr_lag_1=lag(severity_charcoal, n=1),
			severity_cr_lag_2=lag(severity_charcoal, n=2),
			observation_lag_1=lag(observation, n=1),
			observation_lag_2=lag(observation, n=2) ) %>%
		ungroup()
	
### get status - mite ratings
	## filter to remove dates with NAs between sample and last mite rating
	status.iso.mite = data.dis.t %>% filter(!is.na(sample_type) | !is.na(severity_mite) )
	
	## remove unneeded columns
	status.iso.mite = status.iso.mite %>% select(-period, -observation, -severity_charcoal)
	
	## get status	
	status.iso.mite = status.iso.mite %>% 
		group_by(season, irrigation, cultivar, inoculum, block, plant) %>%
		mutate(
			severity_mite_lag_1=lag(severity_mite, n=1),
			severity_mite_lag_2=lag(severity_mite, n=2) ) %>%
		ungroup()
		
### join mite to cr
	status.iso.t = status.iso.cr %>% left_join(status.iso.mite, by=c(c("season","irrigation","cultivar","inoculum","block","plant","date","sample_type")) )

	## check join
	data.dis.t %>%
		left_join( { status.iso.t %>% mutate(flag_temp=1) }, by=c(c("season","irrigation","cultivar","inoculum","block","plant","date","sample_type")) ) %>%
		filter(is.na(flag_temp) )
		
	status.iso.t %>%
		left_join( { data.dis.t %>% mutate(flag_temp=1) }, by=c(c("season","irrigation","cultivar","inoculum","block","plant","date","sample_type")) ) %>%
		filter(is.na(flag_temp) ) 

### filter
	status.iso.t = status.iso.t %>% filter(!is.na(sample_type) )	

	## check that events are the same as original data
		# upload
		data.iso.in = read_csv(file="./2_data_curated/isolation_import_step-1_for-check.csv", col_names=T, na="")
		
		# change column format
		data.iso.in = data.iso.in %>% mutate(block=as.character(block), plant=as.integer(plant) )
		
		# check 
		data.iso.in %>% 
			distinct(season, irrigation, cultivar, inoculum, block, plant, date, sample_type) %>%
			rename(sample_type=sample_type) %>%
			left_join( { status.iso.t %>% mutate(flag_temp=1) }, by=c(c("season","irrigation","cultivar","inoculum","block","plant")) ) %>%
			filter(is.na(flag_temp) )

### view data (uncomment only one group_by line at a time to check different sets)
#	status.iso %>% 
#		group_by(sample_type, event_lag_1, event_lag_2) %>%
#		group_by(sample_type, severity_charcoal, severity_cr_lag_1, severity_cr_lag_2) %>%
#		group_by(sample_type, severity_mite, severity_mite_lag_1, severity_mite_lag_2) %>%
#		group_by(sample_type, observation, observation_lag_1, observation_lag_2) %>%
#		summarize(ct=n()) %>%
#		print(n=Inf)

	# events: none before sample_type, therefore event_lag not needed
	# charcoal: keep same date and lag 1; lag 2 does not add any information
	# mite: keep lag 1, same date and lag 2 do not add any info (except for 1 row in which lag 1 = 1 and lag 2 = 4)
	# observation: keep same date, lag 1; lag 2 does not add info
		# sample_type == "diagnosis" & observation == "none" & is.na(observation_lag_1) & observation_lag_2 == "none" is ok because all have severity data
		# sample_type == "diagnosis" & is.na(observation) & is.na(observation_lag_1) is ok because all have severity data
		# sample_type == "diagnosis" & is.na(observation) & is.na(observation_lag_1) & is.na(observation_lag_2) is ok because all have severity data
		# sample_type == "random" & is.na(observation) & is.na(observation_lag_1) & observation_lag_2 == "dead" is ok because 1 row has severity data

### remove unneeded columns
	status.iso.t = status.iso.t %>% select(-event_lag_1, -event_lag_2, -severity_cr_lag_2, -severity_mite, -severity_mite_lag_2, -observation_lag_2)


###################################################
##### ISOLATIONS - QUALITY CONTROL - FINALIZE #####
###################################################
# collapse status set info into final column

### observations
	## view summary
#	status.iso.t %>% group_by(observation, observation_lag_1) %>% summarize(ct=n()) %>% print(n=Inf)
		
	## assign 
		# in 2018-19 there were no observation dates before first sampling date; all 12 plants come out from below assignment with NAs for obs status; status forced to "none" because plants were present and alive
	status.iso = status.iso.t %>% mutate(observation_status=case_when(
		(observation == "dead") ~ "dead",
		(observation == "none") ~ "none",
		(is.na(observation) & observation_lag_1 == "none") ~ "none",
		(is.na(observation) & observation_lag_1 == "dead") ~ "dead",
		(is.na(observation) & is.na(observation_lag_1) & date == as_date("2018-11-21") ) ~ "none",
		(is.na(observation) & is.na(observation_lag_1) & date != as_date("2018-11-21") ) ~ NA_character_) )
			
		# check
		status.iso %>% group_by(observation, observation_lag_1, observation_status) %>% summarize(ct=n()) %>% arrange(observation_status) %>% print(n=Inf)

### severity - charcoal rot 
	## view summary
#		status.iso %>% group_by(severity_charcoal, severity_cr_lag_1) %>% summarize(ct=n()) %>% print(n=Inf)
		
	## assign
	status.iso = status.iso %>% mutate(severity_last_combined=case_when(
		(severity_charcoal %in% c("0","1","2","3","4","5") ) ~ severity_charcoal,
		(is.na(severity_charcoal) & severity_cr_lag_1 %in% c("0","1","2","3","4","5") ) ~ severity_cr_lag_1,
		(is.na(severity_charcoal) & is.na(severity_cr_lag_1) ) ~ NA_character_) )
			
		# check
		status.iso %>% group_by(severity_charcoal, severity_cr_lag_1, severity_last_combined) %>% summarize(ct=n()) %>% print(n=Inf)

### severity - mite 
	# view summary; no collapsing
#		status.iso %>% group_by(severity_mite_lag_1) %>% summarize(ct=n()) %>% print(n=Inf)

### severity - combined
	## view summary
#	status.iso %>% group_by(severity_last_combined, severity_mite_lag_1) %>% summarize(ct=n()) %>% print(n=Inf)
		
	## assign
	status.iso = status.iso %>% mutate(severity_comb_status=case_when(
		(severity_last_combined != "5" & severity_mite_lag_1 != "4") ~ "alive",
		(severity_last_combined != "5" & is.na(severity_mite_lag_1) ) ~ "alive",
		(severity_last_combined != "5" & severity_mite_lag_1 == "4") ~ "dead",
		(severity_last_combined == "5") ~ "dead",
		(is.na(severity_last_combined) & severity_mite_lag_1 == "4") ~ "dead",
		(is.na(severity_last_combined) &  is.na(severity_mite_lag_1) ) ~ NA_character_) )
		
		# check
		status.iso %>% group_by(severity_last_combined, severity_mite_lag_1, severity_comb_status) %>% summarize(ct=n()) %>% arrange(severity_comb_status) %>% print(n=Inf)

### assign status
	## view summary
#	status.iso %>% group_by(severity_comb_status, observation_status) %>% summarize(ct=n()) %>% print(n=Inf)

	## assign
	status.iso = status.iso %>% mutate(severity_status=case_when(
		(!is.na(severity_comb_status) ) ~ severity_comb_status,
		( is.na(severity_comb_status) & observation_status == "dead") ~ "dead",
		( is.na(severity_comb_status) & observation_status == "none") ~ "alive",
		( is.na(severity_comb_status) & is.na(observation_status) ) ~ NA_character_) )
	
	## check
	status.iso %>% filter(is.na(severity_status) )
	
	status.iso %>% 
		group_by(severity_comb_status, observation_status, severity_status) %>% 
		summarize(ct=n()) %>% 
		arrange(severity_status, severity_comb_status, observation_status) %>% 
		print(n=Inf)

### remove unneeded columns
	status.iso = status.iso %>% select(-observation, -severity_charcoal, -severity_cr_lag_1, -observation_lag_1, -severity_comb_status)
	
			
###################
###### EXPORT #####
###################

### isolation data
	## join
	data.iso = data.iso.in %>% left_join(status.iso, by=c(c("season","irrigation","cultivar","inoculum","block","plant","date","sample_type")) )

	## export
	write_csv(data.iso, file="./2_data_curated/data_isolation_final.csv", col_names=T, na="NA", append=F)	

### disease data
	## export
	write_csv(data.dis, file="./2_data_curated/data_severity_final.csv", col_names=T, na="NA", append=F)	
	
