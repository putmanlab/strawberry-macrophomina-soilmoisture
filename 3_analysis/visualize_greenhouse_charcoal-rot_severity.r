##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Greenhouse - Charcoal rot severity			 #
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
	# NOTE: 
		# NA = there was not a rating written for that plant for that date
		# x = the plant was sampled and therefore no longer recieved a rating
#	in.gh.1 = read_csv(file="./2_data/Greenhouse_Severity_Ratings_2018-2019 - Ratings.csv", col_names=T, na=c("NA", "x"))
	in.gh.2 = read_csv(file="./2_data/Greenhouse_Severity_Ratings_2019-2020 - Ratings.csv", col_names=T, na=c("NA",""))
	in.gh.3 = read_csv(file="./2_data/Greenhouse_Severity_Ratings_2020-2021 - Ratings.csv", col_names=T, na=c("NA",""))

### organize
	## assign year
	in.gh.2 = in.gh.2 %>% mutate(season=as.character("2019-2020") )
	in.gh.3 = in.gh.3 %>% mutate(season=as.character("2020-2021") )
	
	## bind
	in.gh = bind_rows(in.gh.2, in.gh.3)
	
	## rename columns
	in.gh = in.gh %>% rename(irrigation=Irrigation, cultivar=Cultivar, inoculum=Inoculum, block=Block, plant=Plant, date=Date, severity=Rating)

	## convert date
	in.gh = in.gh %>% mutate(date=mdy(date) )
	
	## change column type
	in.gh = in.gh %>% mutate(block=as.character(block), plant=as.integer(plant) )
	
	## reorder columns
	in.gh = in.gh %>% select(season, irrigation, cultivar, inoculum, block, plant, date, severity)
	

###########################################
##### MANUAL QUALITY CONTROL - OUTPUT #####
###########################################

### split each season
	data.gh.2 = in.gh %>% filter(season == "2019-2020")
	data.gh.3 = in.gh %>% filter(season == "2020-2021")

### spread 
	data.gh.2 = data.gh.2 %>% spread(key=date, value=severity)
	data.gh.3 = data.gh.3 %>% spread(key=date, value=severity)

### export
	# !!! manual quality control completed, would need to adjust spreadsheets if there are any changes
#	write_csv(data.gh.2, file="./2_data_curated/greenhouse_manual-qual-control_step-1_2020.csv", col_names=T, na="", append=F)	
#	write_csv(data.gh.3, file="./2_data_curated/greenhouse_manual-qual-control_step-1_2021.csv", col_names=T, na="", append=F)	


##########################################
##### MANUAL QUALITY CONTROL - INPUT #####
##########################################

### upload (save .xlsx in 2_data_curated_worksheets as .csv, append "_out" to filename, and move to 2_data_curated)
	in.manual.2 = read_csv(file="./2_data_curated/greenhouse_manual-qual-control_step-2_2020_out.csv", col_names=T, na="NA")
	in.manual.3 = read_csv(file="./2_data_curated/greenhouse_manual-qual-control_step-2_2021_out.csv", col_names=T, na="NA")

### organize
	## gather
	in.manual.2 = in.manual.2 %>% gather(key="date", value="severity", -season, -irrigation, -cultivar, -inoculum, -block, -plant)
	in.manual.3 = in.manual.3 %>% gather(key="date", value="severity", -season, -irrigation, -cultivar, -inoculum, -block, -plant)
	
	## bind
	in.manual = bind_rows(in.manual.2, in.manual.3)
	
	## convert to date
	in.manual = in.manual %>% mutate(date=mdy(date) )
	
	## convert column formats
	in.manual = in.manual %>% mutate(season=as.character(season), block=as.character(block), plant=as.integer(plant) )
	
### quality control - update severity
	## check
	in.manual %>% group_by(severity) %>% summarize(ct=n())
	
	## update; 
		# samp_X = plant was sampled, where X = most recent rating
			# recent rating of 3, 4 -> missing because cannot guess what rating will be
			# recent rating of 5 -> stays a 5 because will always be dead
		# qc_X = rating was manually changed to X
	in.manual = in.manual %>% mutate(severity=replace(severity, severity == "samp_3", NA) )
	in.manual = in.manual %>% mutate(severity=replace(severity, severity == "samp_4", NA) )
	in.manual = in.manual %>% mutate(severity=replace(severity, severity == "samp_5", "5") )
	in.manual = in.manual %>% mutate(severity=replace(severity, severity == "qc_1", "1") )
	in.manual = in.manual %>% mutate(severity=replace(severity, severity == "qc_5", "5") )

	## check
	in.manual %>% group_by(severity) %>% summarize(ct=n())

### final name
	data.gh = in.manual

### export
	write_csv(data.gh, file="./2_data_curated/greenhouse_severity_final_SAS.csv", col_names=T, na=".", append=F)		


#########################################
##### SUMMARIZE - FOR VISUALIZATION #####
#########################################

# normalize total counts to 1 pot * 1 plants/plot * 5 reps * 1 date = 5 plants
	# data analyzed with repeated measures, therefore is averaged over dates
	
### remove missing
	data.gh = data.gh %>% filter(!is.na(severity))

### reorder factor levels, set labels
	data.gh = data.gh %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High"), labels=c("Low","Mod","High") ) )

### get normalized data; rat.sig: severity ratings for significant factors identified by repeated measures analysis
	summ.1.rat.sig.1 = rating.normalize(df.x=data.gh, summ.cols=c("irrigation"), n.plants=20 )
	summ.1.rat.sig.2 = rating.normalize(df.x=data.gh, summ.cols=c("cultivar"), n.plants=30 )
	summ.1.rat.sig.3 = rating.normalize(df.x=data.gh, summ.cols=c("inoculum","irrigation"), n.plants=10 )


#####################
##### VISUALIZE #####
#####################

### make annotations
	## make tibbles for brackets
		# irrigation 
		pval.1 = tibble(
			season=	 c("2019-2020","2019-2020"),
			xmin=	 c("Low","Mod"),
			xmax=	 c("Mod","High"),
			p_value= c("0.2659","0.0002"),
			y_position=c(14,17) )

		# cultivar
		pval.2 = tibble(
			season=	 c("2019-2020","2020-2021"),
			xmin=	 c("Fronteras","Fronteras"),
			xmax=	 c("Monterey","Monterey"),
			p_value= c("0.0152","0.0002"),
			y_position=c(26,26) )
			
		# irrigation x inoculum
		pval.3 = tibble(
			season=	   c("2020-2021","2020-2021","2020-2021","2020-2021"),
			inoculum=  c("Control","Control",	 "Inoculated","Inoculated"),
			xmin=	   c("Low","Mod",			 "Low","Mod"),
			xmax=	   c("Mod","High",			 "Mod","High"),
			p_value=   c("0.0006","<.0001",		 "0.0042","<.0001"),
			y_position=c(7,9,7,9) )

#		pval.3 = pval.3 %>% mutate(irrigation=factor(irrigation, levels=c("Low","Mod","High") ) )

	## make tibbles for annotation
		# irrigation
		annot.1 = tibble(
			season=c("2020-2021"),
			irrigation=c("Mod"),
			text_lab=c("interaction") )

		# cultivar
			# none
			
		# irrigation x inoculum
			# none
			
### graph significant factor
	## variables
	size.ax.text=10
	size.ax.title=11
	size.strip=10.5
	size.lg.title=11
	size.lg.text=10

	## irrigation
	plot.1.rat.sig.1 = summ.1.rat.sig.1 %>% filter(season == "2019-2020") %>% {
	ggplot(., aes(x=irrigation) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.1, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=3.25 ) +
#		geom_text(data=annot.1, aes(x=irrigation, label=text_lab), y=18, size=3) +
		facet_grid(cols=vars(season) ) +
		scale_y_continuous(limits=c(0,21), breaks=c(0,5,10,15,20) ) +
		scale_fill_manual(limits=c("0","1","2","3","4","5"), values=c(brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.text=element_text(size=size.ax.text), axis.title=element_text(size=size.ax.title) ) +
		theme(strip.text=element_text(size=size.strip, margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.position="none", plot.tag.position=c(0.20,0.97) ) +
		labs(x="Soil Moisture", y="# of plants (normalized)", tag="A")
	}
	
	## cultivar
	plot.1.rat.sig.2 = ggplot(summ.1.rat.sig.2, aes(x=cultivar) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.2, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=3.25 ) +
		facet_grid(cols=vars(season) ) +
		scale_y_continuous(limits=c(0,31), breaks=c(0,10,20,30) ) +
		scale_fill_manual(limits=c("0","1","2","3","4","5"), values=c(brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=size.ax.text, angle=30, hjust=1), axis.text.y=element_text(size=size.ax.text) ) +
		theme(axis.title.y=element_blank(), axis.title.x=element_text(size=size.ax.title) ) +
		theme(strip.text=element_text(size=size.strip, margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.position="none", plot.tag.position=c(0.04,0.97)) +
		labs(x="Cultivar", tag="B")

	## irrigation x inoculum
	plot.1.rat.sig.3 = summ.1.rat.sig.3 %>% filter(season == "2020-2021") %>% {
	ggplot(., aes(x=irrigation) ) +
		geom_bar(aes(y=n_plants_norm, fill=severity), stat="identity") +
		geom_bracket(data=pval.3, aes(xmin=xmin, xmax=xmax, y.position=y_position, label=p_value), label.size=3.25 ) +
		facet_grid(cols=vars(season, inoculum), scales="free_x" ) +
		scale_y_continuous(limits=c(0,11), breaks=c(0,5,10) ) +
		scale_fill_manual(limits=c("0","1","2","3","4","5"), values=c(brewer.pal(6, "YlOrRd") ) ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=size.ax.text, angle=30, hjust=1), axis.text.y=element_text(size=size.ax.text) ) +
		theme(axis.title.x=element_text(size=size.ax.title), axis.title.y=element_blank() ) +
		theme(strip.text=element_text(size=size.strip, margin=margin(1.1,4.4,1.1,4.4, "pt") ) ) + # reduce top and bottom margins, default all = 4.4 pt
		theme(legend.text=element_text(size=size.lg.text, margin=margin(0,0,0,-3, "pt") ), plot.tag.position=c(0.03,0.97) ) + # reduce left margin of text to move closer to fill box, default is NULL
		theme(legend.title=element_text(size=size.lg.title) ) +
		guides(fill=guide_legend(direction="horizontal", nrow=1 ) ) +
		labs(x="Soil Moisture", y="# of plants (normalized)", fill="Charcoal rot severity", tag="C")
	}
	
	plot.1.rat.sig.top = wrap_plots(plot.1.rat.sig.1, plot.1.rat.sig.2, nrow=1, widths=c(1,2) )

	plot.1.rat.sig = wrap_plots(plot.1.rat.sig.1, plot.1.rat.sig.2, plot.1.rat.sig.3, nrow=1, widths=c(1,1.5,2.1), guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom") )	
	
	ggplot2::ggsave(file="./4_results/figure-4_greenhouse-severity.png", device="png", plot=plot.1.rat.sig, width=7, height=4, units="in", dpi=600)
