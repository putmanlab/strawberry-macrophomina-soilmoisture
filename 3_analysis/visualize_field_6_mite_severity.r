##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Mite - severity								 #
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


##################
##### EXPORT #####
##################

### original (per plant) datasets
	## view summary to make datasets
	data.dis %>% 
		group_by(rating, period, observation_status, severity_status) %>%
		summarize(ct=n()) %>%
		print(n=Inf)
		
	## 1. mite severity (raw) - rating period - severity present
		# raw
			# filter
			data.dis.1 = data.dis %>% filter(rating == "mite" & period == "rating" & severity_status == "present" & date == as_date("2021-04-17") )
	
			# SAS
				# remove unneeded columns
				data.dis.1.sas = data.dis.1 %>% select(-rating, -sample_type, -period, -severity_status, -observation, -observation_status)
			
				# modify cultivar name so Monterey always goes last in sorting
				data.dis.1.sas = data.dis.1.sas %>% mutate(cultivar=replace(cultivar, cultivar == "Monterey", "zMonterey") )		
			
				# write
				write_csv(data.dis.1.sas, file="./2_data_curated/severity_final_mite-rating_dead-removed_SAS.csv", col_names=T, na=".", append=F)

	## 1. % symptom incidence (derived from raw mite severity), for residual calculation
		# summarize
		summ.1.plot.allt.dat = summarize.symp.mite(df.x=data.dis.1, rating.str="mite", summ.cols=c("irrigation","cultivar","inoculum","block"), summ.date=NULL )
	
		# SAS
			# remove unneeded columns
			summ.1.plot.allt.dat.sas = summ.1.plot.allt.dat %>% select(-period, -n_row, -severity_symp_perc)
			
			# write
			write_csv(summ.1.plot.allt.dat.sas, file="./2_data_curated/severity_final_mite-incidence_dead-removed_SAS.csv", col_names=T, na=".", append=F)
		
	## 1. severity index (derived from raw mite severity), for residual calculation
		# calculate numerator
			# get sums of each rating
			summ.1.plot.index = data.dis.1 %>% 
				filter(!is.na(severity) ) %>%
				group_by(season, irrigation, cultivar, inoculum, block, severity) %>%
				summarize(n_plants_rating=n_distinct(plant) ) %>%
				ungroup()
				
			# calculate rating * n_plants
			summ.1.plot.index = summ.1.plot.index %>% mutate(index_numerator=severity * n_plants_rating)
			
			# summ by plot
			summ.1.plot.index = summ.1.plot.index %>% 
				group_by(season, irrigation, cultivar, inoculum, block) %>% 
				summarize(index_numerator=sum(index_numerator) ) %>%
				ungroup()
				
		# calculate denominator
			# get total plants
			summ.1.plot.index.denom = data.dis.1 %>% 
				filter(!is.na(severity) ) %>%
				group_by(season, irrigation, cultivar, inoculum, block) %>%
				summarize(n_plants_treatment=n_distinct(plant) ) %>%
				ungroup()
				
			# multiply by max rating
			summ.1.plot.index.denom = summ.1.plot.index.denom %>% mutate(index_denom=n_plants_treatment * 4)
		
		# join
		summ.1.plot.index = summ.1.plot.index %>% left_join(summ.1.plot.index.denom, by=c(c("season","irrigation","cultivar","inoculum","block")) )

		# calculate index
		summ.1.plot.index = summ.1.plot.index %>% mutate(mite_severity_index=round( (index_numerator / index_denom), digits=3 ) )
		
		# SAS
			# remove unneeded columns	
			summ.1.plot.index.sas = summ.1.plot.index %>% select(-index_numerator, -n_plants_treatment, -index_denom)
			
			# write
			write_csv(summ.1.plot.index.sas, file="./2_data_curated/z_not-shown_severity_final_mite-index_dead-removed_SAS.csv", col_names=T, na=".", append=F)

### summary - main plot level
	## ratings of 3, 4
	summ.1.main.34 = data.dis.1 %>% 
		group_by(season, irrigation, block) %>%
		summarize(plants_34=sum(severity >= 3, na.rm=TRUE), plants_total=n() ) %>%
		mutate(percent_plants_34=round( (plants_34/plants_total), digits=2) * 100 ) %>%
		ungroup()
		
	## severity index
		# calculate numerator
			# get sums of each rating
			summ.1.main.index = data.dis.1 %>% 
				filter(!is.na(severity) ) %>%
				group_by(season, irrigation, block, severity) %>%
				summarize(n_plants_rating=n_distinct(plant) ) %>%
				ungroup()
				
			# calculate rating * n_plants
			summ.1.main.index = summ.1.main.index %>% mutate(index_numerator=severity * n_plants_rating)
			
			# summ by plot
			summ.1.main.index = summ.1.main.index %>% 
				group_by(season, irrigation, block) %>% 
				summarize(index_numerator=sum(index_numerator) ) %>%
				ungroup()
				
		# calculate denominator
			# get total plants
			summ.1.main.index.denom = data.dis.1 %>% 
				filter(!is.na(severity) ) %>%
				group_by(season, irrigation, block) %>%
				summarize(n_plants_treatment=n() ) %>%
				ungroup()
				
			# multiply by max rating
			summ.1.main.index.denom = summ.1.main.index.denom %>% mutate(index_denom=n_plants_treatment * 4)
		
		# join
		summ.1.main.index = summ.1.main.index %>% left_join(summ.1.main.index.denom, by=c(c("season","irrigation","block")) )

		# calculate index
		summ.1.main.index = summ.1.main.index %>% mutate(mite_severity_index=round( (index_numerator / index_denom), digits=3 ) )
	
	## combine
		# align columns
		summ.1.main.34 = summ.1.main.34 %>% rename(mite_severity=percent_plants_34)
		summ.1.main.index = summ.1.main.index %>% rename(mite_severity=mite_severity_index)
		
		# add grouping variable
		summ.1.main.34 = summ.1.main.34 %>% mutate(summ_mite="percent_plants_34")
		summ.1.main.index = summ.1.main.index %>% mutate(summ_mite="severity_index")
		
		# remove unneeded columns
		summ.1.main.34 = summ.1.main.34 %>% select(-plants_34, -plants_total)
		summ.1.main.index = summ.1.main.index %>% select(-index_numerator, -n_plants_treatment, -index_denom)
		
		# bind
		summ.1.main = bind_rows(summ.1.main.34, summ.1.main.index)
		
	## export
	write_csv(summ.1.main, file="./2_data_curated/z_not-shown_summary_main-plot_mite-severity.csv", col_names=T, na="NA", append=F)

								
#########################################
##### SUMMARIZE - FOR VISUALIZATION #####
#########################################

# normalize total counts to 1 plot * 30 plants/plot * 4 reps * 1 date = 120 plants
	# data is averaged over dates (repeated measures) or at least one factor, therefore total # plants will vary in stacked bars

### view summary to write code below
	data.dis.1 %>% 
		group_by(rating, period, observation_status, severity_status, severity) %>% 
		summarize(ct=n())

### reorder factor levels, set labels
	data.dis.1 = data.dis.1 %>% mutate(
		irrigation=factor(irrigation, levels=c("Low","Optimal","High"), labels=c("Low","Opt","High") ) )
		
### get normalized data; rat.sig: severity ratings for significant factors identified by repeated measures analysis
	summ.1.rat.sig.1 = rating.normalize(df.x=data.dis.1, summ.cols=c("cultivar"), n.plants=120 )
	summ.1.rat.sig.2 = rating.normalize(df.x=data.dis.1, summ.cols=c("irrigation","inoculum"), n.plants=120 )


#####################
##### VISUALIZE #####
#####################

### graph significant factors
	## make tibbles for brackets
		# cultivar
		pval.1 = tibble(
			season=	 c("2020-2021"),
			xmin=	 c("Fronteras"),
			xmax=	 c("Monterey"),
			p_value= c("0.0058"),
			y_position=c(110) )
			
		# irrigation x inoculum (removed: slices not significant)
#		pval.2 = tibble(
#			season=	 c("2020-2021","2020-2021", "2020-2021","2020-2021"),
#			inoculum=c("Control","Control",	   	"Inoculated","Inoculated"),
#			xmin=	 c("Low","Opt",				"Low","Opt"),
#			xmax=	 c("Opt","High",			"Opt","High"),
#			p_value= c("0.6594","0.3629",		"0.8805","0.9708"),
#			y_position=c(90,100,				90,100 ) )
			
	## make tibbles for annotation
		# irrigation x inoculum
		annot.2 = tibble(
			season=c("2020-2021","2020-2021"),
			inoculum=c("Control","Inoculated"),
			irrigation=c("Opt","Opt"),
			text_lab=c("slice 0.3954","slice 0.9805") )
					
	## cultivar
	plot.1.rat.sig.1 = ggplot(summ.1.rat.sig.1, aes(x=cultivar) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.1, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=4 ) +
		facet_grid(cols=vars(season) ) +
		scale_y_continuous(limits=c(0,121), breaks=c(0,30,60,90,120) ) +
		scale_fill_manual(limits=c("miss","1","2","3","4"), values=c("grey95",brewer.pal(4, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.title=element_text(size=11), axis.text=element_text(size=9), strip.text=element_text(size=10) ) +
		theme(strip.text=element_text(margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.position="none", plot.tag.position=c(0.05,0.96) ) +
		labs(x="Cultivar", y="# of plants (normalized)", tag="A")
		
	## irrigation x inoculum
	plot.1.rat.sig.2 = ggplot(summ.1.rat.sig.2, aes(x=irrigation) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
#		geom_bracket(data=pval.2, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=3 ) +
		geom_text(data=annot.2, aes(x=irrigation, label=text_lab), y=116, size=4) +
		facet_grid(cols=vars(season, inoculum) ) +
		scale_y_continuous(limits=c(0,121), breaks=c(0,30,60,90,120) ) +
		scale_fill_manual(limits=c("miss","1","2","3","4"), values=c("grey95",brewer.pal(4, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.title.y=element_blank()) +
		theme(axis.title.x=element_text(size=11), axis.text=element_text(size=9), strip.text=element_text(size=10) ) +
		theme(strip.text=element_text(margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.text=element_text(margin=margin(0,0,0,-3, "pt") ), plot.tag.position=c(0.05,0.96) ) + # reduce left margin of text to move closer to fill box, default is NULL
		guides(fill=guide_legend(direction="horizontal", nrow=1 ) ) +
		labs(x="Soil Moisture", tag="B")
	
	plot.1.rat.sig = wrap_plots(plot.1.rat.sig.1, plot.1.rat.sig.2, nrow=1, widths=c(1,2), guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom") )	
	
	ggplot2::ggsave(file="./4_results/supp-figure-S11_mite_severity.png", device="png", plot=plot.1.rat.sig, width=7, height=4, units="in", dpi=600)
