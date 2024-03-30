##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Charcoal rot and mite severity				 #
# Investigate associations						 #
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

#library(cowplot) # for plot_grid
library(patchwork) # for wrap_plots
library(RColorBrewer) # for bar fill colors

# install ggpubr for statistical comparison brackets; after dependencies ggsci and ggsignif
remotes::install_version("ggsci", version="3.0.0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggsignif", version="0.6.4", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggpubr", version="0.4", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggpubr) # for geom_bracket

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")

# load functions
source("./3_analysis/functions_visualize.r")


##################
##### IMPORT #####
##################

### upload
	data.dis = read_csv("./2_data_curated/data_severity_final.csv", col_names=T, na="NA")

### change column format
	data.dis = data.dis %>% mutate(block=as.character(block), plant=as.integer(plant), severity=as.integer(severity) )


#####################################
##### PREPARE FOR VISUALIZATION #####
#####################################

### check if status columns are the same for charcoal rot/mite ratings on 2021-04-17
	data.dis %>%
		filter(date == as_date("2021-04-17") ) %>%
		group_by(irrigation, cultivar, inoculum, block, plant) %>%
		summarize(ct_severity_status=n_distinct(severity_status), ct_observation=n_distinct(observation), ct_observation_status=n_distinct(observation_status) ) %>%
		filter(ct_severity_status > 1 | ct_observation > 1 | ct_observation_status >1 ) %>%
		print(n=Inf)
		
	## both _status columns are identical, observation differs for 20 plants 

### create joined dataset; join mite data to all rating dates
	## split off mite data; second mite rating date removed due to very low severity
		# filter
		data.dis.mite = data.dis %>% filter(rating == "mite" & date == as_date("2021-04-17") )
		
		# rename columns to keep separate after join
		data.dis.mite = data.dis.mite %>% rename(severity_mite=severity, observation_mite=observation)
		
		# remove unneeded columns
		data.dis.mite = data.dis.mite %>% select(-rating, -date, -sample_type, -period, -severity_status, -observation_status)

	## filter charcoal rot data for season and period
	data.dis.1 = data.dis %>% filter(season == "2020-2021" & period == "rating" & rating == "charcoal rot")
	
	## join mite data
	data.dis.1 = data.dis.1 %>% left_join(data.dis.mite, by=c(c("season","irrigation","cultivar","inoculum","block","plant")) )
	
	## clean up columns
		# rename severity
		data.dis.1 = data.dis.1 %>% rename(severity_charcoal=severity)
		
		# remove unneeded columns
		data.dis.1 = data.dis.1 %>% select(-rating, -sample_type, -period)
		
	## checks
		# where observation (charcoal) different from obs_mite on 04-17
		data.dis.1 %>% filter(observation != observation_mite & date == as_date("2021-04-17") )
		
		# where obs_mite is dead but later ratings have none for charcoal
		data.dis.1 %>% 
			group_by(season, irrigation, cultivar, inoculum, block, plant) %>%
			summarize(ct_diff=sum(observation == "none" & observation_mite == "dead") ) %>%
			right_join(data.dis.1, by=c("season","irrigation","cultivar","inoculum","block","plant")) %>%
			filter(ct_diff > 0) %>% 
			print(n=Inf)
			
			# cases idiosyncratic: received charcoal rating = 5 next date, plant recovered

		# view summary to write code below
		data.dis.1 %>% 
			filter(date == as_date("2021-04-17") ) %>%
			group_by(severity_charcoal, severity_status, observation, observation_status, severity_mite, observation_mite) %>% 
			summarize(ct=n()) %>%
			print(n=Inf)
	
### summarize
	## by plot
	summ.plot.1 = data.dis.1 %>%
		group_by(season, irrigation, cultivar, inoculum, block, date, severity_charcoal, severity_mite) %>%
		summarize(plants_count=n_distinct(plant) ) %>%
		ungroup()

### assign spatial positions to plots for visualization
	## irrigation main plots
		# make df
		spatial.main = tibble(
			irrigation=c("Low","Low","Low","Low","Optimal","Optimal","Optimal","Optimal","High","High","High","High"),
			block=c("1","2","3","4","1","2","3","4","1","2","3","4"),
			main_row=as.integer(c(1,3,2,2,3,2,3,1,2,1,1,3) ) )
			
		# join
		summ.plot.1 = summ.plot.1 %>% left_join(spatial.main, by=c(c("irrigation","block")) )


#####################
##### VISUALIZE #####
#####################

### bar plot
	## overall - by rating date
	plot.bar.1 = data.dis.1 %>% filter(!is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal) ) ) +
		geom_bar(aes(fill=as.character(severity_mite) ) ) +
		facet_grid(cols=vars(date) ) +
		theme_bw() +
		theme(legend.position="bottom")
	}
#	ggplot2::ggsave(file="./4_results/severity_charcoal-mite_association_bar-overall.png", device="png", plot=plot.bar.1, width=8, height=4, units="in", dpi=600)

	## overall - by rating date, split by inoculum
	plot.bar.1b = data.dis.1 %>% filter(!is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal) ) ) +
		geom_bar(aes(fill=as.character(severity_mite) ) ) +
		facet_grid(cols=vars(date), rows=vars(inoculum) ) +
		theme_bw() +
		theme(legend.position="bottom")
	}
	ggplot2::ggsave(file="./4_results/z_not-shown_charcoal-mite_severity_association_bar-overall_inoculum.png", device="png", plot=plot.bar.1b, width=8, height=4, units="in", dpi=600)

	## overall - by rating date, faceted by severity_mite
	plot.bar.2 = data.dis.1 %>% filter(!is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal) ) ) +
		geom_bar() +
		facet_grid(cols=vars(date), rows=vars(as.character(severity_mite)) ) +
		theme_bw() +
		theme(legend.position="bottom")
	}
#	ggplot2::ggsave(file="./4_results/severity_charcoal-mite_association_bar-severity.png", device="png", plot=plot.bar.2, width=8, height=4, units="in", dpi=600)

	## overall - by rating date, faceted by severity_mite, split by inoculum
	plot.bar.2b = data.dis.1 %>% filter(!is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal) ) ) +
		geom_bar(aes(fill=inoculum) ) +
		facet_grid(cols=vars(date), rows=vars(as.character(severity_mite)) ) +
		theme_bw() +
		theme(legend.position="bottom")
	}
	ggplot2::ggsave(file="./4_results/z_not-shown_charcoal-mite_severity_association_bar-severity_inoculum.png", device="png", plot=plot.bar.2b, width=8, height=4, units="in", dpi=600)

### contingency dot
	## on first rating date, faceted by main plot position; on first rating date; remove NAs, severity_mite == 1 to focus in on plants with mite damage
	plot.contin.1 = summ.plot.1 %>% filter(date == as_date("2021-04-17") & !is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal), y=as.character(severity_mite) ) ) +
		geom_point(aes(size=plants_count) ) +
		facet_grid(cols=vars(block), rows=vars(main_row) ) +
		theme_bw()
	}
	ggplot2::ggsave(file="./4_results/z_not-shown_charcoal-mite_severity_association_contingency_first-date.png", device="png", plot=plot.contin.1, width=10, height=10, units="in", dpi=600)

	## by rating date, faceted by main plot row; on first rating date; remove NAs to focus in on plants with mite damage
	plot.contin.2 = summ.plot.1 %>% filter(!is.na(severity_charcoal) & !is.na(severity_mite) ) %>% {
	ggplot(., aes(x=as.character(severity_charcoal), y=as.character(severity_mite) ) ) +
		geom_point(aes(size=plants_count) ) +
		facet_grid(cols=vars(date), rows=vars(main_row) ) +
		theme_bw()
	}
	ggplot2::ggsave(file="./4_results/z_not-shown_charcoal-mite_severity_association_contingency_all-dates.png", device="png", plot=plot.contin.2, width=10, height=7.5, units="in", dpi=600)

### visual observations
	## mite damage is spatially heterogenous, concentrated in top left (block 1, row 1) and declining diagonally to bottom right
	## association with charcoal severity declines over time
	## higher mite severity on 04-17 do not seem to lead to higher charcoal severity over time
