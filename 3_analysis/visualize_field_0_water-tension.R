##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Organize and visualize 						 #
# Soil moisture tension							 #
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

library(RColorBrewer) # for bar fill colors

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")

 
###############################
##### IMPORT AND ORGANIZE #####
###############################

### upload and organize 
	## import
	sensor1 = read_csv(file="./2_data/Hortau_Sensor_Data_2018-2019.csv", col_names=T)
	sensor2 = read_csv(file="./2_data/Hortau_Sensor_Data_2019-2020.csv", col_names=T, na=c("NA"))
	sensor3 = read_csv(file="./2_data/Hortau_Sensor_Data_2020-2021.csv", col_names=T)

	## remove first row - obvious erroneous readings
	sensor1 = sensor1 %>% filter(Date != "1/16/19 1:00")

	## Add column to distinguish season
	sensor1 = mutate(sensor1, season="2018-2019", before=NULL, after=NULL)
	sensor2 = mutate(sensor2, season="2019-2020", before=NULL, after=NULL)
	sensor3 = mutate(sensor3, season="2020-2021", before=NULL, after=NULL)

	## gather sensor data
	sensor1 = sensor1 %>% gather(key="Treatment", value="tension", -Date, -season)
	sensor2 = sensor2 %>% gather(key="Treatment", value="tension", -Date, -season)
	sensor3 = sensor3 %>% gather(key="Treatment", value="tension", -Date, -season)       

	## Extract cultivar and irrigation info from treatment column
	sensor1 = sensor1 %>% separate(Treatment, c("cultivar","irrigation"), sep=" ", remove=T)
	sensor2 = sensor2 %>% separate(Treatment, c("cultivar","irrigation","Rep"), sep=" ", remove=T)
	sensor3 = sensor3 %>% separate(Treatment, c("cultivar","irrigation","Rep"), sep=" ", remove=T)

	## bind
	data.tension = bind_rows(sensor1, sensor2, sensor3)

	## convert to date
		# change column format
		data.tension = data.tension %>% mutate(Date=mdy_hm(Date) ) %>% rename(Datetime=Date)

		# convert timezone to PST; times in downloaded file are actually in PST but the mdy_ function assumes times are in UTC
			# force_tz keeps numbers same but changes tz label; in contrast, with_tz changes both numbers and tz label
		data.tension = data.tension %>% mutate(Datetime=force_tz(Datetime, tz="Etc/GMT+8") )

	## Change order of irrigation treatments for graphing
	data.tension$irrigation = factor(data.tension$irrigation, levels=c("High","Optimal","Low"))

	## Remove outlier in optimal 2020-2021 data
	data.tension = data.tension %>% mutate(tension=replace(tension, tension > 50 & irrigation == "Optimal" & season == "2020-2021", NA) )

	## check
	data.tension %>%
		group_by(season, cultivar, irrigation, Rep) %>%
		summarize(ct=n(), ct_na=sum(is.na(tension)) ) %>%
		filter(ct_na > 0) %>%
		print(n=Inf)

	## aggregate to common interval
	data.tension = data.tension %>%
		group_by(season, cultivar, irrigation, Rep, Datetime=ceiling_date(Datetime, "15 min", change_on_boundary=F) ) %>%
		summarize(tension=mean(tension, na.rm=F) ) %>%
		ungroup()

	## Reverse tension sign
	data.tension = data.tension %>% mutate(tension=tension*-1)

	## Add column to change Fronteras and Petaluma to say "Fronteras/Petaluma" for graphing
	data.tension = data.tension %>% mutate(cultivar_graph=case_when(
		(cultivar %in% c("Petaluma","Fronteras") ) ~ "Fronteras/Petaluma",
		(cultivar == "Monterey") ~ "Monterey" ) )

		# check
		data.tension %>% filter(is.na(cultivar_graph) )
		
   ## fill Rep = NA in 2018-19
    	# when attempting to plot separate lines for each rep using group=interaction(cultivar, Rep), presence of NAs causes error "geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line"
    data.tension = data.tension %>% mutate(Rep=replace(Rep, season == "2018-2019" & is.na(Rep), "1") )

	## remove 2019-20 Monterey low rep 1 during malfunctioning period
	data.tension = data.tension %>% mutate(tension=replace(
		tension, 
		season == "2019-2020" & cultivar == "Monterey" & irrigation == "Low" & Rep == "1" & date(Datetime) >= as_date("2020-02-22") & date(Datetime) <= as_date("2020-05-01"),
		NA) )

### visualize
	# commented out after initial exploratory analysis

#	plot.sensor.1 = ggplot(data.tension, aes(x=Datetime, y=tension, color=cultivar, linetype=cultivar, group=interaction(cultivar, Rep) ) ) +
#		geom_line(size=0.25) +
#		scale_x_datetime(name="Month of season", date_breaks=("month"), date_labels=("%b")) +
#		scale_y_continuous(name="Soil Moisture Tension (kPa)",expand=c(0,0)) +
#		facet_grid(irrigation~season, scales="free_x") +
#		theme_bw()+
#		theme(
#			axis.text.x=element_text(size=10), axis.title.y=element_text(size=13),
#			axis.title.x=element_text(size=13),axis.text.y=element_text(size=10),strip.text=element_text(size=12),
#			legend.position=c(.5,.8), legend.text=element_text(size=12))
#      
#		ggplot2::ggsave(file="./4_results/SuppFigS5.2018-2020-sensors_15min_raw.png", device="png", plot=plot.sensor.1, width=9, height=7, units="in", dpi=600)
#
#		# 2019-20 Monterey low rep 1 showing a lot of spikes, not observed in rep 2
#			# example of consecutive 15 min readings in raw data: 64.2, 89.7, 65.1
#
	## examine outlier spikes
#	plot.tension.outlier.1 = data.tension %>% filter(season == "2019-2020" & irrigation == "Low") %>% {
#	ggplot(., aes(x=Datetime, y=tension, color=cultivar, linetype=cultivar, group=interaction(cultivar, Rep) ) ) +
#		geom_line(size=0.25) +
#		scale_x_datetime(name="Month of season", date_breaks="week", date_minor_breaks="day", date_labels="%b %d", limits=as_datetime(c("2020-02-15 00:01:00","2020-05-15 00:01:00")) ) +
#		scale_y_continuous(name="Soil Moisture Tension (kPa)", limits=c(-61,5), expand=c(0,0)) +
#		facet_grid(rows=vars(cultivar, Rep) ) +
#		theme_bw()
#   }
#	ggplot2::ggsave(file="./4_results/tension_outlier_examine_raw.png", device="png", plot=plot.tension.outlier.1, width=14, height=8, units="in", dpi=600)

	# testing showed only 274 observations were classified as outlier and do not change final numbers much, therefore left unchanged


###############################
##### SUMMARIZE - GENERAL #####
###############################

### summary stats - min, max, and average of daily averages for each irrigation treatment         
	## summarize by day
	data.tension.day = data.tension %>%
		group_by(season, irrigation, Date=as_date(Datetime) ) %>% 
		summarize(tension_day=mean(tension, na.rm = TRUE)) %>%
		ungroup() 

	## calculate summary stats
	data.tension.day %>%
		group_by(season, irrigation) %>%
		summarize(
			tension_day_min=min(tension_day, na.rm = TRUE), 
			tension_day_max=max(tension_day, na.rm = TRUE), 
			tension_day_avg=mean(tension_day, na.rm = TRUE) ) %>%
			arrange(irrigation, season) %>%
		print(n=Inf)
	
		# Check data
		data.tension %>% group_by(cultivar, irrigation) %>% summarize(avg_tension=mean(tension, na.rm=TRUE)) %>% print(n=Inf)


############################################
##### SUMMARIZE - TIME ABOVE THRESHOLD #####
############################################
# time exceeded each tension threshold (-5,-10,-30,-60); each row = 0.25 hr (15 min interval)

### per sensor 
	## calculate
	summ.exceed = data.tension %>% 
		group_by(season, irrigation, cultivar, cultivar_graph, Rep) %>% 
		summarize(
			total_hr = sum(!is.na(tension)) * 0.25, 
			above_5  = sum(tension >= -5, na.rm=T) * 0.25,
			bet_5_10 = sum(tension < -5  & tension >= -10, na.rm=T) * 0.25, 
			bet_10_30= sum(tension < -10 & tension >= -30, na.rm=T) * 0.25, 
			bet_30_60= sum(tension < -30 & tension >= -60, na.rm=T) * 0.25, 
			below_60 = sum(tension < -60, na.rm=T) * 0.25 ) %>%
		ungroup()

	## gather
	summ.exceed = summ.exceed %>% gather(key="threshold", value="hours", -season, -irrigation, -cultivar, -cultivar_graph, -Rep, -total_hr)
	
	## check total hr
	summ.exceed %>% group_by(season, total_hr) %>% summarize(ct=n())
	
	## calculate percent of total hr
	summ.exceed = summ.exceed %>% mutate(perc_hours=round( (hours/total_hr), digits=3) * 100 )
	
	## average between reps
	summ.exceed = summ.exceed %>% 
		group_by(season, irrigation, cultivar, cultivar_graph, threshold) %>%
		summarize(perc_hours=mean(perc_hours, na.rm=FALSE) ) %>%
		ungroup()
		
		# check for NAs
		summ.exceed %>% filter(is.na(perc_hours) )

### across cultivars 
	## calculate
	summ.exceed.irr = data.tension %>% 
		group_by(season, irrigation, Rep) %>% 
		summarize(
			total_hr = sum(!is.na(tension)) * 0.25, 
			above_5  = sum(tension >= -5, na.rm=T) * 0.25,
			bet_5_10 = sum(tension < -5  & tension >= -10, na.rm=T) * 0.25, 
			bet_10_30= sum(tension < -10 & tension >= -30, na.rm=T) * 0.25, 
			bet_30_60= sum(tension < -30 & tension >= -60, na.rm=T) * 0.25, 
			below_60 = sum(tension < -60, na.rm=T) * 0.25 ) %>%
		ungroup()

	## gather
	summ.exceed.irr = summ.exceed.irr %>% gather(key="threshold", value="hours", -season, -irrigation, -Rep, -total_hr)
	
	## check total hr
	summ.exceed.irr %>% group_by(season, total_hr) %>% summarize(ct=n())
	
	## calculate percent of total hr
	summ.exceed.irr = summ.exceed.irr %>% mutate(perc_hours=round( (hours/total_hr), digits=3) * 100 )
	
	## average between reps
	summ.exceed.irr = summ.exceed.irr %>% 
		group_by(season, irrigation, threshold) %>%
		summarize(perc_hours=mean(perc_hours, na.rm=FALSE) ) %>%
		ungroup()
		
		# check for NAs
		summ.exceed.irr %>% filter(is.na(perc_hours) )

### per month
	# calculate
	summ.exceed.month = data.tension %>% 
		group_by(season, irrigation, cultivar, cultivar_graph, Rep, month=month(Datetime) ) %>% 
		summarize(
			total_hr = sum(!is.na(tension)) * 0.25, 
			above_5  = sum(tension >= -5, na.rm=T) * 0.25,
			bet_5_10 = sum(tension < -5  & tension >= -10, na.rm=T) * 0.25, 
			bet_10_30= sum(tension < -10 & tension >= -30, na.rm=T) * 0.25, 
			bet_30_60= sum(tension < -30 & tension >= -60, na.rm=T) * 0.25, 
			below_60 = sum(tension < -60, na.rm=T) * 0.25 ) %>%
		ungroup()
	
	## gather
	summ.exceed.month = summ.exceed.month %>% gather(key="threshold", value="hours", -season, -irrigation, -cultivar, -cultivar_graph, -Rep, -month, -total_hr)
	
	## check total hr
	summ.exceed.month %>% group_by(season, month, total_hr) %>% summarize(ct=n())
	
	## calculate percent of total hr
	summ.exceed.month = summ.exceed.month %>% mutate(perc_hours=round( (hours/total_hr), digits=3) * 100 )
	
	## average between reps
	summ.exceed.month = summ.exceed.month %>% 
		group_by(season, irrigation, cultivar, cultivar_graph, month, threshold) %>%
		summarize(perc_hours=mean(perc_hours, na.rm=TRUE) ) %>%
		ungroup()
		
		# check for NAs
		summ.exceed.month %>% filter(is.na(perc_hours) )
	
	## convert month to character
	summ.exceed.month = summ.exceed.month %>% mutate(month=as.character(month) )
	
### prepare for graphing
	## tension - summarize into 1 hour blocks for graphing
	data.tension.1hr = data.tension %>% 
		group_by(season, cultivar, irrigation, cultivar_graph, Rep, Datetime=ceiling_date(Datetime, "hour")) %>%
		summarize(tension_avg=mean(tension, na.rm=T)) %>%
		ungroup()

	## threshold times
	summ.exceed = summ.exceed %>% mutate(
		cultivar=fct_relevel(cultivar, c("Fronteras","Petaluma","Monterey") ),
		irrigation=fct_relevel(irrigation, c("Low","Optimal","High") ),
		threshold=factor(threshold, levels=c("above_5","bet_5_10","bet_10_30","bet_30_60","below_60"), labels=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60") ) )

	summ.exceed.irr = summ.exceed.irr %>% mutate(
		irrigation=fct_relevel(irrigation, c("Low","Optimal","High") ),
		threshold=factor(threshold, levels=c("above_5","bet_5_10","bet_10_30","bet_30_60","below_60"), labels=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60") ) )

	summ.exceed.month = summ.exceed.month %>% mutate(
		cultivar=fct_relevel(cultivar, c("Fronteras","Petaluma","Monterey") ),
		irrigation=fct_relevel(irrigation, c("Low","Optimal","High") ),
		threshold=factor(threshold, levels=c("above_5","bet_5_10","bet_10_30","bet_30_60","below_60"), labels=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60") ),
		month=factor(month, levels=c("11","12","1","2","3","4","5","6") ) )

### view values
	summ.exceed %>% 
		spread(key=threshold, value=perc_hours) %>% 
		mutate(total_below_10=`-10 to -30` + `-30 to -60` + `< -60`) %>%
		arrange(irrigation, season, cultivar)

	summ.exceed.irr %>%
		spread(key=threshold, value=perc_hours) %>% 
		mutate(total_below_10=`-10 to -30` + `-30 to -60` + `< -60`) %>%
		arrange(irrigation, season)


############################################
##### SUMMARIZE - MINIMUM BEFORE EVENT #####
############################################
# lowest reading prior to irrigation, rain event

### import events 
	## upload
	summ.flow.event = read_csv(file="./2_data_curated/water_flow_event-start-end.csv", col_names=T, na=c("NA"))

	## convert id to integer; date to datetime to join only on last hour of event day
	summ.flow.event = summ.flow.event %>% mutate(
		event_id_end=as.integer(event_id),
		datetime_end=as_datetime(paste(date_end, "23:00:00", sep=""), tz="Etc/GMT+8") )

	## remove unneeded columns; rename column to indicate is end date
	summ.flow.event = summ.flow.event %>% select(season, treatment, event_id_end=event_id, datetime_end)
		
### hourly
	## join datasets
		# add treatment columns for joining
		data.tension.1hr.2 = data.tension.1hr %>% mutate(treatment=case_when(
				(irrigation == "Low") ~ "low",
				(irrigation == "Optimal") ~ "optimal",
				(irrigation == "High") ~ "high") )
		
		# join
		data.tension.1hr.2 = data.tension.1hr.2 %>% left_join(summ.flow.event, by=c("season" = "season", "treatment" = "treatment", "Datetime" = "datetime_end") )

	## sort
	data.tension.1hr.2 = data.tension.1hr.2 %>% arrange(season, irrigation, cultivar, Rep, Datetime)

	## add id to each event interval (ending hour to ending hour); lag so that end hour is not included in next interval; +1 to match id numbers
	data.tension.1hr.2 = data.tension.1hr.2 %>% 
		group_by(season, irrigation, cultivar, Rep) %>%
		mutate(event_interval_id=as.integer( cumsum( !is.na(lag(event_id_end, n=1)) ) + 1 ) ) %>%
		ungroup()

	## check example
#	data.tension.1hr.2 %>% filter(season == "2019-2020" & treatment == "optimal" & date(Datetime) >= as_date("2019-12-05") ) %>% print(n=200)
		
	## summarize
		# get minimum for each event
		summ.tens.1hr = data.tension.1hr.2 %>%
			group_by(season, irrigation, cultivar, Rep, event_interval_id) %>%
			summarize(tension_hr_min=min(tension_avg, na.rm=TRUE) ) %>%
			ungroup()
			
		# remove Inf
		summ.tens.1hr = summ.tens.1hr %>% mutate(tension_hr_min=replace(tension_hr_min, tension_hr_min == Inf, NA) )

		# average - across cultivars, reps
		summ.tens.1hr %>%
			group_by(season, irrigation) %>%
			summarize(tension_hr_min_avg=mean(tension_hr_min, na.rm=TRUE) ) %>%
			arrange(irrigation, season) %>%
			ungroup()

		# average - each cultivar, across reps
#		summ.tens.1hr %>%
#			group_by(season, irrigation, cultivar) %>%
#			summarize(tension_hr_min_avg=mean(tension_hr_min, na.rm=TRUE) ) %>%
#			ungroup()


#####################
##### VISUALIZE #####  
#####################

### tension - 1 hr
	# coord_cartesian to "zoom in" and cut off outlier peaks
	plot.sensor.2 = ggplot(data.tension.1hr, aes(x=Datetime, y=tension_avg, color=cultivar, linetype=cultivar, group=interaction(cultivar, Rep) ) ) +
#		geom_line(linewidth=0.25) +
		geom_line(size=0.25) +
		scale_x_datetime(name="Month of season", date_breaks=("month"), date_labels=("%b")) +
		scale_y_continuous(name="Soil Moisture (kPa)",expand=c(0,0)) +
		facet_grid(irrigation~season, scales="free_x") +
		coord_cartesian(ylim=c(-61, 5) ) +
		theme_bw()+
		theme(
			axis.text.x=element_text(size=10), axis.title.y=element_text(size=13),
			axis.title.x=element_text(size=13), axis.text.y=element_text(size=10), strip.text=element_text(size=12),
			legend.position="bottom", legend.text=element_text(size=10), legend.margin = margin(t=-11, b=-5.5) )
      
	ggplot2::ggsave(file="./4_results/supp-figure-S05_water_tension_1hr.png", device="png", plot=plot.sensor.2, width=9, height=5, units="in", dpi=600)

### exceed tension threshold
	plot.tension.threshold = ggplot(summ.exceed, aes(x=irrigation, y=perc_hours, color=threshold, fill=threshold) ) +
		geom_bar(stat="identity", position="stack") +
        facet_grid(cols=vars(season, cultivar), scales="free_x") +
        scale_x_discrete(labels=c("Low","Opt","High") ) +
		scale_fill_manual(limits=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60"), values=c("#00B0F6","#00BF7D","#A4A500","#F8766D","#E76BF3") ) +
		scale_color_manual(limits=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60"), values=c("#00B0F6","#00BF7D","#A4A500","#F8766D","#E76BF3") ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(x="irrigation Treatment", y="Percent Time in Range (%)")
		
	ggplot2::ggsave(file="./4_results/z_not-shown_water_tension_exceed-threshold.png", device="png", plot=plot.tension.threshold, width=7, height=4, units="in", dpi=600)

	plot.tension.threshold.2 = ggplot(summ.exceed.month, aes(x=month, y=perc_hours, color=threshold, fill=threshold) ) +
		geom_bar(stat="identity", position="stack") +
        facet_grid(rows=vars(irrigation), cols=vars(season, cultivar)) +
		scale_fill_manual(limits=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60"), values=c("#00B0F6","#00BF7D","#A4A500","#F8766D","#E76BF3") ) +
		scale_color_manual(limits=c("> -5","-5 to -10","-10 to -30", "-30 to -60", "< -60"), values=c("#00B0F6","#00BF7D","#A4A500","#F8766D","#E76BF3") ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(x="Month", y="Percent Time in Range (%)", color="Soil Moisture (kPa)", fill="Soil Moisture (kPa)")
		
	ggplot2::ggsave(file="./4_results/supp-figure-S08_water_tension_exceed-threshold_month.png", device="png", plot=plot.tension.threshold.2, width=7, height=5, units="in", dpi=600)
		      