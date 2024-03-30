##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Organize and visualize						 #
# Flow meter and Weather data					 #
##################################################

## built on Docker putmanlab/exploratory-analysis:420.0

if (!require(conflicted)) {
  install.packages("conflicted")
  library(conflicted)
}

library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(forcats)
library(egg)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")


###########################################
#### FLOW METER - IMPORT AND ORGANIZE #####
###########################################

### import
	flowmeters1 = read_csv(file="./2_data/Flow_Meter_2018-2019 - Readings.csv", col_names=T, na=c("NA",""))
	flowmeters2 = read_csv(file="./2_data/Flow_Meter_2019-2020 - Readings.csv", col_names=T, na=c("NA",""))
	flowmeters3 = read_csv(file="./2_data/Flow_Meter_2020-2021 - Readings.csv", col_names=T, na=c("NA",""))

### organize
	## remove rows with 'before treatment started as date
	flowmeters1 = flowmeters1 %>% filter(date != "Before treatments started")
	
	## Add Season column to each data set
	flowmeters1 = mutate(flowmeters1, season="2018-2019")
	flowmeters2 = mutate(flowmeters2, season="2019-2020")
	flowmeters3 = mutate(flowmeters3, season="2020-2021")

	## Combine flow data
	data.flow = bind_rows(flowmeters1, flowmeters2, flowmeters3)
        
	## Change flow data date from character to date
	data.flow = data.flow %>% mutate(date=mdy(date))

	## filter out dates before tensiometers installed and treatments started
	data.flow = data.flow %>% filter(
		(date >= as_date("2019-01-26") & date <= as_date("2019-06-23") ) |
		(date >= as_date("2019-12-05") & date <= as_date("2020-06-24") ) |
		(date >= as_date("2020-11-23") & date <= as_date("2021-06-27") ) ) 
    
	## convert to long format by treatment
		# add column that counts optimal, high irrigation events
			# make counter column
			data.flow = data.flow %>% 
				group_by(season) %>%
				mutate(event_optimal_id=cumsum(deliver_optimal_gal > 0), event_high_id=cumsum(deliver_high_gal > 0) ) %>% 
				ungroup()
	
			# remove values on non-event days
			data.flow = data.flow %>% mutate(event_optimal_id=replace(event_optimal_id, deliver_optimal_gal == 0 & is.na(recommend_optimal_in), NA) )
			data.flow = data.flow %>% mutate(event_high_id=replace(event_high_id, deliver_high_gal == 0 & is.na(recommend_high_in), NA) )

			# check counter
			data.flow %>% select(date, deliver_low_in, recommend_low_in, event_low_id, deliver_optimal_in, recommend_optimal_in, event_optimal_id, deliver_high_in, recommend_high_in, event_high_id)
									
		# rename columns to facilite gather
		data.flow = data.flow %>% rename(
			flowbefore_low=flowmeter_low_before, flowafter_low=flowmeter_low_after, delivergal_low=deliver_low_gal, deliverin_low=deliver_low_in, recommendin_low=recommend_low_in, eventid_low=event_low_id,
			eventlength_optimal=eventlength_opt, flowbefore_optimal=flowmeter_optimal_before, flowafter_optimal=flowmeter_optimal_after, delivergal_optimal=deliver_optimal_gal, deliverin_optimal=deliver_optimal_in, recommendin_optimal=recommend_optimal_in, eventid_optimal=event_optimal_id,
			flowbefore_high=flowmeter_high_before, flowafter_high=flowmeter_high_after, delivergal_high=deliver_high_gal, deliverin_high=deliver_high_in, recommendin_high=recommend_high_in, eventid_high=event_high_id)
			
		# gather
		data.flow = data.flow %>% gather(key="variable", value="value", -date, -season)
		
		# separate variable
		data.flow = data.flow %>% separate(variable, into=c("variable","treatment"), sep="_")
		
		# spread variable
		data.flow = data.flow %>% spread(key=variable, value=value)
		
		# rename columns
		data.flow = data.flow %>% rename(event_length=eventlength, flowmeter_before=flowbefore, flowmeter_after=flowafter, deliver_gal=delivergal, deliver_in=deliverin, recommend_in=recommendin, event_id=eventid)
        
		# reorder columns
		data.flow = data.flow %>% select(season, treatment, date, event_id, recommend_in, event_length, flowmeter_before, flowmeter_after, deliver_gal, deliver_in)        

		# check for discrepancies
			# deliver gal vs in
#			data.flow %>% filter( (deliver_gal > 0 & deliver_in == 0) | (deliver_gal == 0 & deliver_in > 0) )
			
			# recommend vs deliver
#			data.flow %>% filter(
#				( (!is.na(recommend_in) & recommend_in >  0) & ( is.na(deliver_gal) | deliver_gal == 0) ) |
#				( ( is.na(recommend_in) | recommend_in == 0) & (!is.na(deliver_gal) & deliver_gal >  0) ) )
				
				# result: only four rows

	## Convert flow meter data from inches of water to  millimeters of water by multiplying by 25.4
	data.flow = data.flow %>% mutate(deliver_mm = round( (deliver_in*25.4), digits=1) )

	## change column type
	data.flow = data.flow %>% mutate(event_id=as.integer(event_id) )

	## remove days with 0 (non-events)
	data.flow = data.flow %>% filter(deliver_mm > 0)
	
### calculate summary statistics
	## total number of events
	data.flow %>% 
		mutate(treatment=fct_relevel(treatment, c("low","optimal","high") ) ) %>%
		group_by(season, treatment) %>% 
		summarize(n_events=max(event_id, na.rm=TRUE) )
  
	## total depth delivered, average depth delivered
	data.flow %>% 
		mutate(treatment=fct_relevel(treatment, c("low","optimal","high") ) ) %>%
		group_by(season, treatment) %>% 
		summarize(deliver_mm_total=sum(deliver_mm), deliver_mm_avg=mean(deliver_mm, na.rm = TRUE))

	## average interval between irrigation events
		# get min/max of dates for each event
			summ.flow.event = data.flow %>%
				group_by(season, treatment, event_id) %>%
				summarize(date_start=min(date), date_end=max(date) )
		
			# calculate interval (needs to keep group_by from previous step)
			summ.flow.event = summ.flow.event %>% mutate(days_since_event_end = date_start - lag(date_end, n=1) ) %>% ungroup()
		
			# convert to integer
			summ.flow.event = summ.flow.event %>% mutate(days_since_event_end=as.integer(days_since_event_end) )
			
			# checks
#			summ.flow.event %>% filter(is.na(days_since_event_end) )
#			summ.flow.event %>% filter(event_id == 1)
#			summ.flow.event %>% group_by(season, treatment) %>% summarize(interval_max=max(days_since_event_end, na.rm=TRUE) )

		# calculate avg interval
		summ.flow.event %>% 
			mutate(treatment=fct_relevel(treatment, c("low","optimal","high") ) ) %>%
			group_by(season, treatment) %>%
			summarize(event_interval=mean(days_since_event_end, na.rm=TRUE) ) %>%
			ungroup()

### export event start/end dates
	write_csv(summ.flow.event, file="./2_data_curated/water_flow_event-start-end.csv", col_names=T, na="NA", append=F)
	        
            
###########################################
##### WEATHER - IMPORT AND ORGANIZE #######
###########################################

### upload
	data.wx = read_csv(file="./2_data/DailyAverageCIMIS.csv", col_names=T, na=c("NA","") )
        
### organize
    ## rename columns
    data.wx = data.wx %>% rename(date=Date, precip_mm=`Precip (mm)`, eto_mm=`ETo (mm)`, temp_air_c=`Avg Air Temp (C)`, temp_soil_c=`Avg Soil Temp (C)`)

    ## select needed columns
    data.wx = data.wx %>% select(date, precip_mm, eto_mm, temp_air_c, temp_soil_c)

	## Change date column from character to date
	data.wx = data.wx %>% mutate(date=mdy(date))

	## add season using planting and termination dates 
	data.wx = data.wx %>% mutate(season=case_when(
		(date >= as_date("2018-10-26") & date <= as_date("2019-06-23") ) ~ "2018-2019",
		(date >= as_date("2019-10-23") & date <= as_date("2020-06-24") ) ~ "2019-2020",
		(date >= as_date("2020-10-23") & date <= as_date("2021-06-27") ) ~ "2020-2021") )

	## remove dates outside of season range
	data.wx = data.wx %>% filter(!is.na(season) )
	
	
###############################
##### WATER - SUMMARIZE #######
###############################

### calculate cumulative water
	## fix dates in data.flow
		# join flow to df of all dates; due to gaps in dates, graph shows angled lines instead of flat steps
			# get vectors
			vec.dates = data.wx %>% select(season, date)
			
			# create df with dates x treatment
			df.dates = expand_grid(vec.dates, treatment=c("high","optimal","low") )
			
			# join
			data.flow.2 = df.dates %>% left_join(data.flow, by=c(c("season","treatment","date")) )
		
			# check
			data.flow.2 %>% filter(!is.na(deliver_mm))
						
	## arrange
	data.flow.2 = data.flow.2 %>% arrange(season, treatment, date)
	data.wx = data.wx %>% arrange(season, date)
	
	## fill NAs with 0s because cumsum() cannot handle NAs
	data.flow.2 = data.flow.2 %>% mutate(deliver_mm=replace_na(deliver_mm, 0) )
	
	## calculate
	data.flow.2 = data.flow.2 %>% group_by(season, treatment) %>% mutate(irrigation_cumul_mm=cumsum(deliver_mm) ) %>% ungroup()
	data.wx = data.wx %>% group_by(season) %>% mutate(precip_cumul_mm=cumsum(precip_mm) ) %>% ungroup()

	## replace 0s at beginning with NA
	data.flow.2 = data.flow.2 %>% mutate(irrigation_cumul_mm=replace(irrigation_cumul_mm, season == "2018-2019" & date < as_date("2019-01-26"), NA) )
	data.flow.2 = data.flow.2 %>% mutate(irrigation_cumul_mm=replace(irrigation_cumul_mm, season == "2019-2020" & date < as_date("2019-12-05"), NA) )
	data.flow.2 = data.flow.2 %>% mutate(irrigation_cumul_mm=replace(irrigation_cumul_mm, season == "2020-2021" & date < as_date("2020-11-23"), NA) )

### prepare for graphing
	## add column of dates in same year to facilitate setting a common axis limit range
		# create empty column
		data.flow.2 = data.flow.2 %>% mutate(date_plot=as_date(NA))
		data.wx = data.wx %>% mutate(date_plot=as_date(NA))

		# fill
		data.flow.2 = data.flow.2 %>% mutate(date_plot=replace(date_plot, month(date)  > 7, as_date(paste("2019", month(date[month(date)  > 7]), day(date[month(date)  > 7]), sep="-"))) )
		data.flow.2 = data.flow.2 %>% mutate(date_plot=replace(date_plot, month(date) <= 7, as_date(paste("2020", month(date[month(date) <= 7]), day(date[month(date) <= 7]), sep="-"))) )

		data.wx = data.wx %>% mutate(date_plot=replace(date_plot, month(date)  > 7, as_date(paste("2019", month(date[month(date)  > 7]), day(date[month(date)  > 7]), sep="-"))) )
		data.wx = data.wx %>% mutate(date_plot=replace(date_plot, month(date) <= 7, as_date(paste("2020", month(date[month(date) <= 7]), day(date[month(date) <= 7]), sep="-"))) )
 
 	## order treatments
 	data.flow.2 = data.flow.2 %>% mutate(treatment=fct_relevel(treatment, c("low","optimal","high")) )
 
### summary calculations
	## total precipitation
	data.wx %>% group_by(season) %>% summarize(precip_total=sum(precip_mm))            


#################################
##### VISUALIZE WATER DEPTH #####
#################################

### water depth - cumulative (line)
	## irrigation
	plot.cumul.irr = ggplot(data.flow.2, aes(x=date_plot, y=irrigation_cumul_mm)) +
		geom_line(aes(color=treatment, linetype=treatment)) +
		facet_grid(cols=vars(season) ) +
		scale_x_date(date_breaks="month", minor_breaks="2 weeks", date_labels="%b %d", limits=as_date(c("2019-10-23","2020-06-28"))) +
		scale_color_manual(values=c("red","gold2","forestgreen"), labels=c("Low","Optimal","High") ) +
		scale_linetype_manual(values=c("42","22","solid"), labels=c("Low","Optimal","High") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position=c(.75, .68), legend.text=element_text(size=12)) +
		labs(y="Irrigation (mm)", color="Treatment", linetype="Treatment")
	
	plot.cumul.precip = ggplot(data.wx, aes(x=date_plot, y=precip_cumul_mm)) +
		geom_line() +
		facet_grid(cols=vars(season) ) +
		scale_x_date(date_breaks="month", minor_breaks="2 weeks", date_labels="%b %d", limits=as_date(c("2019-10-23","2020-06-28"))) +
		theme_bw() +
		theme(strip.text.x=element_blank(), strip.background.x=element_blank()) +
		theme(axis.text.x=element_text(size=10, angle=45, vjust=1, hjust=1)) +
		labs(x="Date", y="Precipitation (mm)")

	plot.cumul = ggarrange(plot.cumul.irr, plot.cumul.precip, ncol=1)
	
	ggplot2::ggsave(file="./4_results/supp-figure-S06_water_flow-rain_cumulative.png", device="png", plot=plot.cumul, width=9, height=5, units="in", dpi=600)
    
      
###############################
##### WEATHER - SUMMARIZE #####
###############################

### monthly temperatures
	## soil - 30 days after planting
	data.wx %>%
		filter(
			(date >= as_date("2018-10-26") & date <= as_date("2018-11-25") ) |
			(date >= as_date("2019-10-23") & date <= as_date("2019-11-22") ) |
			(date >= as_date("2020-10-23") & date <= as_date("2020-11-22") ) ) %>%
		group_by(season) %>%
		summarize(date_min=min(date), date_max=max(date), temp_soil_c_avg=mean(temp_soil_c, na.rm=TRUE) )

	## air - March
	data.wx %>%	
		filter(month(date) == 3) %>%
		group_by(season) %>%
		summarize(date_min=min(date), date_max=max(date), temp_air_c_avg=mean(temp_air_c, na.rm=TRUE) )
			
### at symptom development		        
	data.wx %>%
		filter(
			(date >= as_date("2019-04-24") & date <= as_date("2019-05-22") ) |
			(date >= as_date("2020-04-14") & date <= as_date("2020-05-12") ) |
			(date >= as_date("2021-04-03") & date <= as_date("2021-05-01") ) ) %>%
		group_by(season) %>%
		summarize(date_min=min(date), date_max=max(date), across(c(eto_mm, temp_air_c, temp_soil_c), ~ mean(.x, na.rm=TRUE) ) )
