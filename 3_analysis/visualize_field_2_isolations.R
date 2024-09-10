#####################################################
# STRAWBERRY Macrophomina            			 	#
# Influence of soil moisture on disease		     	#
# Summarize and visualize 						 	#
# Isolation of pathogen from host tissue - random   #
#####################################################

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

library(cowplot) # for plot_grid

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


##################
##### IMPORT #####
##################

### upload
	data.iso = read_csv("./2_data_curated/data_isolation_final.csv", col_names=T, na="NA")

	# data_isolation_final gives identical results to sb-mac-irr_isolation-counts_final_NEW for averages (results text)

### organize
	## change column types
	data.iso = data.iso %>% mutate(block=as.character(block), across(c(plant, pieces_mp, pieces_none, pieces_total), ~ as.integer(.x) ) )

### make datasets
	## view summary to ensure proper combination of flag columns are chosen
	data.iso %>% 
		group_by(sample_type, observation_status, severity_status) %>%
		summarize(ct=n()) %>%
		print(n=Inf)

	## 1. random - remove plants that were dead at time of sampling
	data.iso.1 = data.iso %>% filter(sample_type == "random" & observation_status == "none" & severity_status == "alive")

### export
	## select tissue
	data.iso.1.sas.root = data.iso.1 %>% filter(tissue == "root")
	data.iso.1.sas.crown = data.iso.1 %>% filter(tissue == "crown")
	
	## remove unneeded columns
	data.iso.1.sas.root = data.iso.1.sas.root %>% select(-sample_type, -tissue, -period, -observation_status, -severity_status)
	data.iso.1.sas.crown = data.iso.1.sas.crown %>% select(-sample_type, -tissue, -period, -observation_status, -severity_status)
	
	## export
	write_csv(data.iso.1.sas.root, file="./2_data_curated/isolation_final_dead-removed_root_SAS.csv", col_names=T, na=".", append=F)
	write_csv(data.iso.1.sas.crown, file="./2_data_curated/isolation_final_dead-removed_crown_SAS.csv", col_names=T, na=".", append=F)


###################################
##### SUMMARIZE - PLANT LEVEL #####
###################################

### each plant
	summ.plant = data.iso.1 %>% 
		group_by(season, irrigation, cultivar, inoculum, block, date, sample_type, plant) %>%
		summarize(pieces_mp_total=sum(pieces_mp, na.rm=TRUE) ) %>%
		ungroup()

### percent plants over each plot, by sample type (note: not used)
	# calculate total
#	summ.plot = data.iso.1 %>% 
#		group_by(season, irrigation, cultivar, inoculum, block, tissue, sample_type) %>%
#		summarize(pieces_mp=sum(pieces_mp, na.rm=TRUE), pieces_total=sum(pieces_total, na.rm=TRUE) ) %>%
#		ungroup()

	# calculate percent
#	summ.plot = summ.plot %>% mutate(percent_mp=as.integer( round( (pieces_mp/pieces_total), digits=2) * 100 ) )

### cumulative plants with positive isolation
	# filter for randomly sampled plants; not needed because already filtered under 'make datasets' above
	summ.plant.cumul = summ.plant %>% filter(sample_type == "random")
	
	# summarize by date
	summ.plant.cumul = summ.plant.cumul %>% 
		group_by(season, irrigation, cultivar, inoculum, date) %>%
		summarize(plants_mp_total=sum(pieces_mp_total > 0) ) %>%
		ungroup()
	
	# join to df of all dates; so cumulative sum produces stepwise graph with flat line for 0s
		# make vector of dates
		dates.all = as_date( c(as_date("2018-10-26"):as_date("2019-06-23"), as_date("2019-10-23"):as_date("2020-06-24"), as_date("2020-10-23"):as_date("2021-06-27") ) )

		# make df
		df.dates = expand_grid(
			'irrigation'=c("High","Optimal","Low"), 
			'cultivar'=c("Monterey","Petaluma","Fronteras"), 
			'inoculum'=c("Control","Inoculated"), 
			'date'=dates.all)
		
		# add season
		df.dates = df.dates %>% mutate(season=case_when(
			(date >= as_date("2018-10-26") & date <= as_date("2019-06-23") ) ~ "2018-2019",
			(date >= as_date("2019-10-23") & date <= as_date("2020-06-24") ) ~ "2019-2020",
			(date >= as_date("2020-10-23") & date <= as_date("2021-06-27") ) ~ "2020-2021") )

		# remove non-existant cultivar combinations
		df.dates = df.dates %>% filter( !(
			(season == "2018-2019" & cultivar == "Fronteras") |
			(season %in% c("2019-2020","2020-2021") & cultivar == "Petaluma") ) )
		
		# join data to df.dates; left join because data does not have all dates
		summ.plant.cumul = df.dates %>% left_join(summ.plant.cumul, by=c(c("season","irrigation","cultivar","inoculum","date")) )
		
		# fill with 0s because cumsum cannot handle NAs
		summ.plant.cumul = summ.plant.cumul %>% mutate(plants_mp_total=replace_na(plants_mp_total, 0) )
		
	# sort
	summ.plant.cumul = summ.plant.cumul %>% arrange(season, irrigation, cultivar, inoculum, date)

	# calculate cumulative sum
	summ.plant.cumul = summ.plant.cumul %>% 
		group_by(season, irrigation, cultivar, inoculum) %>%
		mutate(plants_mp_cumul=cumsum(plants_mp_total) ) %>%
		ungroup()


###############################################
##### SUMMARIZE - PIECE OVERALL/TREATMENT #####
###############################################

### averages - overall for results text
	## pieces - each tissue, by inoculum treatment (first paragraph)
		# find sum of mp/total first before calculate percentage in case of unequal totals; group seasons per paper
		# filtering not needed because already filtered under 'make datasets' above
	data.iso.1 %>% 
		filter(sample_type == "random") %>%
		mutate(season_group=case_when(
			(season %in% c("2019-2020","2020-2021")) ~ "2020_2021",
			(season == "2018-2019") ~ "2018") ) %>%
		group_by(season_group, tissue, inoculum) %>%
		summarize(pieces_mp=sum(pieces_mp, na.rm=T), pieces_total=sum(pieces_total, na.rm=T) ) %>%
		ungroup() %>%
		mutate(percent_pieces_mp=round( (pieces_mp/pieces_total), digits=3) * 100)
	 
	 ## plants - by inoculum treatment; find sum first (first paragraph)
		# filtering not needed because already filtered under 'make datasets' above
	 summ.plant %>%
		filter(sample_type == "random") %>%
		mutate(season_group=case_when(
			(season %in% c("2019-2020","2020-2021")) ~ "2020_2021",
			(season == "2018-2019") ~ "2018") ) %>%
		group_by(season_group, inoculum) %>%
		summarize(plants_mp=sum(pieces_mp_total > 0), plants_total=n() ) %>%
		ungroup() %>%
		mutate(percent_plants_mp=round( (plants_mp/plants_total), digits=2) * 100)

### averages - for figures/tables
	## first: get average counts by subplot over dates (= data points on graph) by summing counts of mp/total then calculating %
		# filtering not needed because already filtered under 'make datasets' above
	summ.piece.plot = data.iso.1 %>% 
		filter(sample_type == "random") %>%
		group_by(season, tissue, inoculum, cultivar, irrigation, block) %>%
		summarize(pieces_mp=sum(pieces_mp, na.rm=T), pieces_total=sum(pieces_total, na.rm=T) ) %>%
		ungroup() %>%
		mutate(percent_pieces_mp=round( (pieces_mp/pieces_total), digits=3) * 100)

	## second: average % over plots
		# pieces - root
			# a - 2019-20 cultivar x inoculum 
			summ.piece.root.a = summ.piece.plot %>% 
				group_by(season, tissue, inoculum, cultivar) %>%
				summarize(avg_percent_pieces_mp=round( mean(percent_pieces_mp, na.rm=TRUE), digits=1) ) %>%
				ungroup()
				
			summ.piece.root.a = summ.piece.root.a %>% filter(tissue == "root" & season == "2019-2020")

			# inoculum 
			summ.piece.root.inoc = summ.piece.plot %>% 
				group_by(season, tissue, inoculum) %>%
				summarize(avg_percent_pieces_mp=round( mean(percent_pieces_mp, na.rm=TRUE), digits=1) ) %>%
				ungroup()
				
				# b - 2020-21 inoculum
				summ.piece.root.b = summ.piece.root.inoc %>% filter(tissue == "root" & season == "2020-2021")

				# supp table - 2018-19 inoculum			
				summ.piece.root.inoc %>% filter(tissue == "root" & season == "2018-2019")			
			
		# pieces - crown
			# irrigation x inoculum (2019-2020, 2020-2021)
			summ.piece.crown = summ.piece.plot %>%
				group_by(season, tissue, inoculum, irrigation) %>%
				summarize(avg_percent_pieces_mp=round( mean(percent_pieces_mp, na.rm=TRUE), digits=1) ) %>%
				ungroup()

			summ.piece.crown = summ.piece.crown %>% filter(tissue == "crown" & season %in% c("2019-2020","2020-2021") )


################################
##### PREPARE FOR GRAPHING #####
################################

### data.iso
	## combined cultivar factor
	data.iso.1 = data.iso.1 %>% mutate(cultivar_graph=case_when(
		(cultivar %in% c("Petaluma","Fronteras") ) ~ "Petaluma/Fronteras",
		(cultivar == "Monterey") ~ "Monterey") )
		
	## factor to set order
	data.iso.1 = data.iso.1 %>% mutate(
		irrigation=fct_relevel(irrigation, c("Low", "Optimal", "High") ),
		cultivar=fct_relevel(cultivar, c("Monterey", "Petaluma", "Fronteras") ) )

### summ.plot
	## factor to set order (not used)
#	summ.plot = summ.plot %>% mutate(irrigation=fct_relevel(irrigation, c("Low", "Optimal", "High") ) )

### summ.plant.cumul	
	## combined cultivar factor
	summ.plant.cumul = summ.plant.cumul %>% mutate(cultivar_graph=case_when(
		(cultivar %in% c("Petaluma","Fronteras") ) ~ "Petaluma/Fronteras",
		(cultivar == "Monterey") ~ "Monterey") )

	## factor to set order
	summ.plant.cumul = summ.plant.cumul %>% mutate(irrigation=fct_relevel(irrigation, c("Low", "Optimal", "High") ) )

	## add column of dates in same year to facilitate setting a common axis limit range
		# create empty column
		summ.plant.cumul = summ.plant.cumul %>% mutate(date_plot=as_date(NA))
		
		# fill
		summ.plant.cumul = summ.plant.cumul %>% mutate(date_plot=replace(date_plot, month(date)  > 7, as_date(paste("2019", month(date[month(date)  > 7]), day(date[month(date)  > 7]), sep="-")) ) )
		summ.plant.cumul = summ.plant.cumul %>% mutate(date_plot=replace(date_plot, month(date) <= 7, as_date(paste("2020", month(date[month(date) <= 7]), day(date[month(date) <= 7]), sep="-")) ) )

### summ.piece.plot
	## factor to set order in plots
	summ.piece.plot = summ.piece.plot %>% mutate(
		irrigation=fct_relevel(irrigation, c("Low", "Optimal", "High") ),
		cultivar=fct_relevel(cultivar, c("Monterey", "Petaluma", "Fronteras") ) )

	## root pieces - replace values < 1 with "<1"
	summ.piece.root.a = summ.piece.root.a %>% mutate(avg_percent_pieces_mp_label=as.character( round(avg_percent_pieces_mp, digits=0) ) )
	summ.piece.root.b = summ.piece.root.b %>% mutate(avg_percent_pieces_mp_label=as.character( round(avg_percent_pieces_mp, digits=0) ) )
	
	summ.piece.root.a = summ.piece.root.a %>% mutate(avg_percent_pieces_mp_label=replace(avg_percent_pieces_mp_label, avg_percent_pieces_mp < 1, "<1") )
	summ.piece.root.b = summ.piece.root.b %>% mutate(avg_percent_pieces_mp_label=replace(avg_percent_pieces_mp_label, avg_percent_pieces_mp < 1, "<1") )

       
#####################
##### VISUALIZE #####       
#####################

### cumulative # plants
	plot.plant.cumul = ggplot(summ.plant.cumul, aes(x=date_plot, y=plants_mp_cumul) ) +
		geom_line(aes(color=irrigation, linetype=irrigation), lwd=1) +
		facet_grid(cols=vars(season), rows=vars(cultivar_graph, inoculum) ) +
		scale_y_continuous(breaks=seq(0,24,by=4), limits=c(0,24.5)) +
		scale_x_date(breaks=as_date(c("2019-11-01","2020-01-01","2020-03-01","2020-05-01","2020-07-01")), date_minor_breaks="month", date_labels="%b %d", limits=as_date(c("2019-10-23","2020-06-29"))) +
		theme_bw() +
		theme(axis.text=element_text(size="12"), strip.text=element_text(size="12"), 
		      legend.text=element_text(size="12"), axis.title=element_text(size="12") ) +
		theme(legend.position="right") +
		labs(x="Date", y="Cumulative Number of Plants Positive for M. phaseolina", color="Irrigation", linetype="Irrigation")
		
	ggplot2::ggsave(file="./4_results/supp-figure-S8_isolations-random_cumul-plants-mp.png", device="png", plot=plot.plant.cumul, width=13.5, height=7, units="in", dpi=600)


### roots - significant effects
	## make tibbles of p-values
	root.pval.a = tibble(inoculum=c("Control","Inoculated"), cultivar=c("Monterey","Fronteras"), p_value=c("0.7001","<0.0001"))
	root.pval.b = tibble(inoculum=c("Control","Inoculated"), p_value=c("<0.0001"))
	
	## plot - cultivar x inoculum 2019-2020
	p.root.a = summ.piece.plot %>% filter(tissue == "root" & season == "2019-2020") %>% {
	ggplot(., aes(x=cultivar) ) +
		geom_dotplot(aes(y=percent_pieces_mp), binaxis="y", binwidth=1, dotsize=1, stackdir="center", stackratio=1.25) +
		geom_crossbar(data=summ.piece.root.a, aes(y=avg_percent_pieces_mp, ymin=avg_percent_pieces_mp, ymax=avg_percent_pieces_mp), width=0.2, fatten=2, color="red") +
		geom_text(data=summ.piece.root.a, aes(y=avg_percent_pieces_mp, label=avg_percent_pieces_mp_label), size=3, color="red", hjust=c(2,2,2.8,2), vjust=c(-0.7,-0.7,0.5,0.5) ) +
		geom_bracket(data=root.pval.a, aes(label=p_value), xmin=c("Monterey","Monterey"), xmax=c("Fronteras","Fronteras"), y.position=c(25,25), label.size=3, vjust=-0.2) +
		facet_grid(rows=vars(inoculum), cols=vars(season) ) +
		scale_y_continuous(limits=c(0,31)) +
		theme_bw() +
		theme(axis.text=element_text(size=10), axis.title=element_text(size=12), strip.text=element_text(size=10) ) +
		labs(y="Root Isolation Incidence (%)", x="Cultivar", tag="A") 
	}

	## plot - inoculum 2020-2021
	p.root.b = summ.piece.plot %>% filter(tissue == "root" & season == "2020-2021") %>% {
	ggplot(., aes(x=inoculum) ) +
		geom_dotplot(aes(x=inoculum, y=percent_pieces_mp), binaxis="y", binwidth=1, dotsize=0.5, stackdir="center", stackratio=1.25) +
		geom_crossbar(data=summ.piece.root.b, aes(y=avg_percent_pieces_mp, ymin=avg_percent_pieces_mp, ymax=avg_percent_pieces_mp), width=0.2, color="red") +
		geom_text(data=summ.piece.root.b, aes(y=avg_percent_pieces_mp, label=avg_percent_pieces_mp_label), size=3, color="red", hjust=c(1.7,2.1), vjust=c(-0.7,0.5) ) +
		geom_bracket(data=root.pval.b, aes(label=p_value), xmin=c("Control","Control"), xmax=c("Inoculated","Inoculated"), y.position=30, label.size=3, vjust=-0.2) +
		facet_grid(cols=vars(season) ) +
		scale_y_continuous(limits=c(0,31)) +
		theme_bw() +
		theme(axis.text=element_text(size=10), axis.title=element_text(size=12), strip.text=element_text(size=10) ) +
		labs(y="Root Isolation Incidence (%)", x="Inoculation", tag="B") 
	}
	              
	p.root.stat = plot_grid(p.root.a, p.root.b, ncol=1, align="h", axis="lr")
       
#	ggplot2::ggsave(file="./4_results/figure-1_isolations-random_root.png", device="png", plot=p.root.stat, width=3.25, height=6, units="in", dpi=600)


### crown - significant effects
	## make annotation tibbles 
		# pvalue bracket
		crown.pval = tibble(
			season	  	=c("2019-2020", "2019-2020"),
			inoculum  	=c("Inoculated","Inoculated"), 
			xmin	  	=c("Low",		  "Optimal"),
			xmax	  	=c("Optimal",	  "High"),
			p_value   	=c("0.0292",	  "0.9160"),
			y_position	=c(28,31),
			y_position_2=c(28,30) )
 
 		# other
		crown.annot = tibble(
			season	  =c("2019-2020",	"2020-2021",   "2020-2021"),
			inoculum  =c("Control",		"Control",     "Inoculated"), 
			irrigation=c("Optimal",		"Optimal",     "Optimal"), 
			text_lab  =c("slice 0.6131","slice 0.1467","slice 0.3480") )
 		
	## plot - irrigation x inoculum 2019-2020, 2020-2021; label=format forces printing .0
	p.crown = summ.piece.plot %>% filter(tissue == "crown" & season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=irrigation) ) +
		geom_dotplot(aes(y=percent_pieces_mp), binaxis="y", binwidth=1, dotsize=1, stackdir="center", stackratio=1.25) +
		geom_crossbar(data=summ.piece.crown, aes(y=avg_percent_pieces_mp, ymin=avg_percent_pieces_mp, ymax=avg_percent_pieces_mp), width=0.2, color="red") +
		geom_text(data=summ.piece.crown, aes(y=avg_percent_pieces_mp, label=format(round(avg_percent_pieces_mp, digits=0))), size=3, color="red", hjust=2.8) +
		geom_bracket(data=crown.pval, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=3, vjust=0) +
		geom_text(data=crown.annot, aes(x=irrigation, label=text_lab), y=31, size=3) +
		facet_grid(cols=vars(season), rows=vars(inoculum) ) +
		scale_y_continuous(limits=c(0,32.5)) +
		theme_bw() +
		theme(axis.text=element_text(size=10), axis.title=element_text(size=12), strip.text=element_text(size=10) ) +
		labs(y="Crown Isolation Incidence (%)", x="Soil Moisture") 
	}

#	ggplot2::ggsave(file="./4_results/figure-2_isolations-random_crown.png", device="png", plot=p.crown, width=7, height=3.5, units="in", dpi=600)

	## plot 2 (for combined) - irrigation x inoculum 2019-2020, 2020-2021; label=format forces printing .0
		# a - 2019-2020
		p.crown.2a = summ.piece.plot %>% filter(tissue == "crown" & season %in% c("2019-2020") ) %>% {
		ggplot(., aes(x=irrigation) ) +
			geom_dotplot(aes(y=percent_pieces_mp), binaxis="y", binwidth=1, dotsize=1, stackdir="center", stackratio=1.25) +
			geom_crossbar(data={summ.piece.crown %>% filter(season == "2019-2020") }, aes(y=avg_percent_pieces_mp, ymin=avg_percent_pieces_mp, ymax=avg_percent_pieces_mp), width=0.2, color="red") +
			geom_text(data={summ.piece.crown %>% filter(season == "2019-2020") }, aes(y=avg_percent_pieces_mp, label=format(round(avg_percent_pieces_mp, digits=0))), size=3, color="red", hjust=2.8) +
			geom_bracket(data={crown.pval %>% filter(season == "2019-2020") }, aes(xmin=xmin, xmax=xmax, y.position=y_position_2, label=p_value), label.size=3, vjust=0) +
			geom_text(data={crown.annot %>% filter(season == "2019-2020") }, aes(x=irrigation, label=text_lab), y=31, size=3) +
			facet_grid(cols=vars(season), rows=vars(inoculum) ) +
			scale_y_continuous(limits=c(0,32.5)) +
			theme_bw() +
			theme(axis.text.y=element_text(size=10), axis.text.x=element_blank(), axis.title.y=element_text(size=12), axis.title.x=element_blank(), strip.text=element_text(size=10) ) +
			labs(y="Crown Isolation Incidence (%)", x="Soil Moisture", tag="C") 
		}
		
		# b - 2020-2021
		p.crown.2b = summ.piece.plot %>% filter(tissue == "crown" & season %in% c("2020-2021") ) %>% {
		ggplot(., aes(x=irrigation) ) +
			geom_dotplot(aes(y=percent_pieces_mp), binaxis="y", binwidth=1, dotsize=1, stackdir="center", stackratio=1.25) +
			geom_crossbar(data={summ.piece.crown %>% filter(season == "2020-2021") }, aes(y=avg_percent_pieces_mp, ymin=avg_percent_pieces_mp, ymax=avg_percent_pieces_mp), width=0.2, color="red") +
			geom_text(data={summ.piece.crown %>% filter(season == "2020-2021") }, aes(y=avg_percent_pieces_mp, label=format(round(avg_percent_pieces_mp, digits=0))), size=3, color="red", hjust=2.8) +
			geom_text(data={crown.annot %>% filter(season == "2020-2021") }, aes(x=irrigation, label=text_lab), y=31, size=3) +
			facet_grid(cols=vars(season), rows=vars(inoculum) ) +
			scale_y_continuous(limits=c(0,32.5)) +
			theme_bw() +
			theme(axis.text=element_text(size=10), axis.title=element_text(size=12), strip.text=element_text(size=10) ) +
			labs(y="Crown Isolation Incidence (%)", x="Soil Moisture", tag="D") 
		}

		p.crown.2 = plot_grid(p.crown.2a, p.crown.2b, ncol=1, align="h", axis="lr")
		
### both tissues - significant effects
	p.both = plot_grid(p.root.stat, p.crown.2, nrow=1)

	ggplot2::ggsave(file="./4_results/figure-1_isolations-random_root-crown.png", device="png", plot=p.both, width=7, height=6, units="in", dpi=600)



