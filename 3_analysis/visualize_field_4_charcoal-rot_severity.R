##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Charcoal rot - severity						 #
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
		
	## 1. charcoal rot - rating period - severity present
		# filter
		data.dis.1 = data.dis %>% filter(rating == "charcoal rot" & period == "rating" & severity_status == "present")
	
		# SAS
			# remove unneeded columns
			data.dis.1.sas = data.dis.1 %>% select(-rating, -sample_type, -period, -severity_status, -observation, -observation_status)
			
			# modify cultivar name so Monterey always goes last in sorting
			data.dis.1.sas = data.dis.1.sas %>% mutate(cultivar=replace(cultivar, cultivar == "Monterey", "zMonterey") )		
		
			# write
			write_csv(data.dis.1.sas, file="./2_data_curated/severity_final_charcoal-rating_dead-removed_SAS.csv", col_names=T, na=".", append=F)
	

#########################################
##### SUMMARIZE - FOR VISUALIZATION #####
#########################################

# normalize total counts to 1 plot * 30 plants/plot * 4 reps * 1 date = 120 plants
	# data analyzed with repeated measures, therefore is averaged over dates

### view summary to write code below
	data.dis.1 %>% 
		group_by(rating, period, observation_status, severity_status, severity) %>% 
		summarize(ct=n())

### reorder factor levels, set labels
	data.dis.1 = data.dis.1 %>% mutate(
		irrigation=factor(irrigation, levels=c("Low","Optimal","High"), labels=c("Low","Opt","High") ),
		cultivar=factor(cultivar, levels=c("Fronteras","Petaluma","Monterey") ) )

### get normalized data; rat.sig: severity ratings for significant factors identified by repeated measures analysis
	summ.1.rat.sig.1 = rating.normalize(df.x=data.dis.1, summ.cols=c("irrigation","cultivar"), n.plants=120 )
	summ.1.rat.sig.2 = rating.normalize(df.x=data.dis.1, summ.cols=c("irrigation","inoculum"), n.plants=120 )
	summ.1.rat.sig.3 = rating.normalize(df.x=data.dis.1, summ.cols=c("cultivar","inoculum"), n.plants=120 )
	

#####################
##### VISUALIZE #####
#####################

### graph significant factors
	## make tibbles for brackets
		# irrigation x cultivar
		pval.1 = tibble(
			season=	 c("2018-2019","2018-2019", "2019-2020","2019-2020"),
			cultivar=c("Petaluma","Petaluma", 	"Fronteras","Fronteras"),
			xmin=	 c("Low","Opt",				"Low","Opt"),
			xmax=	 c("Opt","High",			"Opt","High"),
			p_value= c("0.0126","0.5172", 		"0.0134","0.6698"),
			y_position=c(90,110,				90,110 ) )
			
		pval.1 = pval.1 %>% mutate(cultivar=factor(cultivar, levels=c("Fronteras","Petaluma","Monterey") ) )

		# irrigation x inoculum
		pval.2 = tibble(
			season=	 c("2019-2020","2019-2020",   "2020-2021","2020-2021", "2020-2021","2020-2021"),
			inoculum=c("Inoculated","Inoculated", "Control","Control",	   "Inoculated","Inoculated"),
			xmin=	 c("Low","Opt",				  "Low","Opt",			   "Low","Opt"),
			xmax=	 c("Opt","High",			  "Opt","High",			   "Opt","High"),
			p_value= c("0.0153","0.6052", 		  "0.0532","0.0076",	   "0.0037","0.9159"),
			y_position=c(90,110,				  90,110,				   90,110 ) )
			
		# cultivar x inoculum
		pval.3 = tibble(
			season=	 c("2019-2020","2019-2020", "2020-2021","2020-2021"),
			inoculum=c("Control","Inoculated", 	"Control","Inoculated"),
			xmin=	 c("Fronteras","Fronteras",	"Fronteras","Fronteras"),
			xmax=	 c("Monterey","Monterey",	"Monterey","Monterey"),
			p_value= c("0.6664","<.0001", 		"0.0617","0.6626"),
			y_position=c(110,110,				110,110) )

	## make tibbles for annotation
		# irrigation x cultivar
		annot.1 = tibble(
			season=c("2018-2019","2019-2020", 			"2020-2021","2020-2021"),
			cultivar=c("Monterey","Monterey", 			"Fronteras","Monterey"),
			irrigation=c("Opt","Opt",		  			"Opt","Opt"),
			text_lab=c("slice 0.3460","slice 0.4086",	"interaction n.s.","interaction n.s.") )

		annot.1 = annot.1 %>% mutate(cultivar=factor(cultivar, levels=c("Fronteras","Petaluma","Monterey") ) )

		# irrigation x inoculum
		annot.2 = tibble(
			season=c("2018-2019","2018-2019",				 "2019-2020"),
			inoculum=c("Control","Inoculated",				 "Control"),
			irrigation=c("Opt","Opt",						 "Opt"),
			text_lab=c("interaction n.s.","interaction n.s.","slice 0.3926") )

		# cultivar x inoculum
		annot.3 = tibble(
			season=c("2018-2019","2018-2019"),
			inoculum=c("Control","Inoculated"),
			cultivar=c(1.5,1.5),
			text_lab=c("interaction n.s.","interaction n.s.") )
						
	## irrigation x cultivar
	plot.1.rat.sig.1 = ggplot(summ.1.rat.sig.1, aes(x=irrigation) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.1, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=2.5 ) +
		geom_text(data=annot.1, aes(x=irrigation, label=text_lab), y=110, size=3) +
		facet_grid(cols=vars(season, cultivar) ) +
		scale_y_continuous(limits=c(0,121), breaks=c(0,30,60,90,120) ) +
		scale_fill_manual(limits=c("miss","0","1","2","3","4","5"), values=c("grey95",brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.title.y=element_blank()) +
		theme(strip.text=element_text(margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.position="none", plot.tag.position=c(0.05,0.96) ) +
		labs(x="Soil Moisture", tag="A")
		
	## irrigation x inoculum
	plot.1.rat.sig.2 = ggplot(summ.1.rat.sig.2, aes(x=irrigation) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.2, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=2.5 ) +
		geom_text(data=annot.2, aes(x=irrigation, label=text_lab), y=110, size=3) +
		facet_grid(cols=vars(season, inoculum) ) +
		scale_y_continuous(limits=c(0,121), breaks=c(0,30,60,90,120) ) +
		scale_fill_manual(limits=c("miss","0","1","2","3","4","5"), values=c("grey95",brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(strip.text=element_text(margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.position="none", plot.tag.position=c(0.05,0.96)) +
		labs(x="Soil Moisture", y="# of plants (normalized)", tag="B")

	## cultivar x inoculum
	plot.1.rat.sig.3 = ggplot(summ.1.rat.sig.3, aes(x=cultivar) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.3, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=2.5 ) +
		geom_text(data=annot.3, aes(x=cultivar, label=text_lab), y=110, size=3) +
		facet_grid(cols=vars(season, inoculum), scales="free_x" ) +
		scale_y_continuous(limits=c(0,121), breaks=c(0,30,60,90,120) ) +
		scale_fill_manual(limits=c("miss","0","1","2","3","4","5"), values=c("grey95",brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.title.y=element_blank(), axis.text.x=element_text(size=6.5) ) +
		theme(strip.text=element_text(margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.text=element_text(margin=margin(0,0,0,-3, "pt") ), plot.tag.position=c(0.05,0.96) ) + # reduce left margin of text to move closer to fill box, default is NULL
		guides(fill=guide_legend(direction="horizontal", nrow=1 ) ) +
		labs(x="Cultivar", fill="Charcoal rot severity", tag="C")
	
	plot.1.rat.sig = wrap_plots(plot.1.rat.sig.1, plot.1.rat.sig.2, plot.1.rat.sig.3, ncol=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom") )	
	
	ggplot2::ggsave(file="./4_results/supp-figure-S11_charcoal-rot_severity.png", device="png", plot=plot.1.rat.sig, width=7, height=6.5, units="in", dpi=600)
