##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Charcoal rot - mortality						 #
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

library(cowplot) # for plot_grid
library(patchwork) # for wrap_plots

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

 	
###############################
##### SUMMARIZE - PREPARE #####
###############################

### view summary of status combination
	data.dis %>% 
		group_by(rating, period, severity, severity_status, observation, observation_status) %>%
		summarize(ct=n()) %>%
		print(n=Inf)

### calculate mortality
	## all treatments
		## plot each date: for export; for figure - line graph
		summ.plot.allt.dat = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("irrigation","cultivar","inoculum","block"), summ.date=c("date") )

		## plot across dates: for figure - line graph
		summ.plot.allt.avg = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("irrigation","cultivar","inoculum","block"), summ.date=NULL )
	
	## irrigation x inoculum
		## treatment each date: for figure - line graph
		summ.trtm.irin.dat = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("irrigation","inoculum"),	summ.date=c("date") )		

		## treatment across dates: for figure - mean of blocks
		summ.trtm.irin.avg = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("irrigation","inoculum"), summ.date=NULL )		

	## cultivar x inoculum
		## treatment each date: for figure - line graph
		summ.trtm.cuin.dat = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("cultivar","inoculum"), summ.date=c("date") )		

		## treatment across dates: for figure - mean of blocks
		summ.trtm.cuin.avg = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("cultivar","inoculum"), summ.date=NULL )		
	
	## inoculum (for final date)
		## treatment each date: for figure - line graph
		summ.trtm.inoc.dat = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("inoculum"),	summ.date=c("date") )		

		## treatment across dates: for figure - mean of blocks
		summ.trtm.inoc.avg = summarize.mort(df.x=data.dis, rating.str="charcoal rot", summ.cols=c("inoculum"), summ.date=NULL )		

	## inoculum (for 2018)
		## filter to exclude first rating date (for analysis) and for rating period (cannot just exclude by date because there are observations in between ratings)
		data.dis.18 = data.dis %>% filter(season == "2018-2019" & period == "rating" & date != as_date("2019-05-08") )

		## treatment across dates: for figure - mean of blocks
		summ.18.trtm.inoc.avg = summarize.mort(df.x=data.dis.18, rating.str="charcoal rot", summ.cols=c("inoculum"), summ.date=NULL )		

### show summaries
	## inoculum: control vs. inoculated average on final rating date (pg 22 paragraph 1)
	summ.trtm.inoc.dat %>% filter(date %in% as_date(c("2019-06-19","2020-06-23","2021-06-28")) )


##################
##### EXPORT #####
##################
		
### summaries
	## charcoal rot mortality
		# filter
		summ.plot.all.rat = summ.plot.allt.dat %>% filter(period == "rating")
		
		# remove unneeded columns
		summ.plot.all.rat.sas = summ.plot.all.rat %>% select(-period, -observation_mort, -observation_present, -n_row, -severity_mort_perc, -observation_mort_perc)
		
		# write
		write_csv(summ.plot.all.rat.sas, file="./2_data_curated/severity_final_charcoal-mortality_gone-removed_SAS.csv", col_names=T, na=".", append=F)


########################################################
##### VISUALIZE - INTERACTIONS 2019-20 AND 2020-21 #####
########################################################

### factor to set order
	summ.plot.allt.dat = summ.plot.allt.dat %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High") ) )
	summ.plot.allt.avg = summ.plot.allt.avg %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High") ) )

	summ.trtm.irin.dat = summ.trtm.irin.dat %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High") ) )
	summ.trtm.irin.avg = summ.trtm.irin.avg %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High") ) )

### specify breaks, limits for facets
	## limits; to make the scale the same for all seasons
	facet_limits = function(x) {
		if (max(x) 			< as_date("2019-08-01")) { as_date(c("2019-04-15","2019-07-01"))
		} else if (max(x) 	< as_date("2020-08-01")) { as_date(c("2020-04-15","2020-07-01"))
		} else if (max(x) 	< as_date("2021-08-01")) { as_date(c("2021-04-15","2021-07-01"))
		} else { NULL }
	}

	## breaks
	facet_breaks = function(x) {
		if (max(x) 			< as_date("2019-08-01")) { as_date(c("2019-05-08","2019-05-22","2019-06-05","2019-06-19"))
		} else if (max(x) 	< as_date("2020-08-01")) { as_date(c("2020-04-28","2020-05-12","2020-05-26","2020-06-09","2020-06-23"))
		} else if (max(x) 	< as_date("2021-08-01")) { as_date(c("2021-04-17","2021-05-03","2021-05-17","2021-05-31","2021-06-14","2021-06-28"))
		} else { NULL }
	}

### make tibbles of annotations (mean separation letters)
	## irrigation x inoculum
	annot.irin = tibble(
		season = c("2019-2020","2019-2020","2019-2020",	"2019-2020","2019-2020","2019-2020",	"2020-2021","2020-2021","2020-2021","2020-2021","2020-2021","2020-2021"),
		inoculum = c("Control","Control","Control",		"Inoculated","Inoculated","Inoculated",	"Control","Control","Control",		"Inoculated","Inoculated","Inoculated"),
		irrigation = c("Low","Optimal","High",			"Low","Optimal","High",					"Low","Optimal","High",				"Low","Optimal","High"),
		text_label = c("a","ab","b",					"a","b","c",							"a","a","a",						"a","b","b"),
		severity_mort_perc = rep(95, 12) )

	annot.irin = annot.irin %>% mutate(irrigation=factor(irrigation, levels=c("Low","Optimal","High") ) )

	## cultivar x inoculum
	annot.cuin = tibble(
		season = c("2019-2020","2019-2020",	 "2020-2021","2020-2021"),
		inoculum = c("Control","Inoculated", "Control","Inoculated"),
		xmin = c("Fronteras","Fronteras",	 "Fronteras","Fronteras"),
		xmax = c("Monterey","Monterey",		 "Monterey","Monterey"),
		p_value = c("0.1930","<.0001",		 "0.0023","0.6805"),
		y_pos = c(90,90,					 90, 90) )
	
### irrigation x inoculum
	## line graph all dates
	plot.irin.line = summ.plot.allt.dat %>% filter(season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=date, y=severity_mort_perc, color=irrigation, linetype=irrigation ) ) +
		geom_line(aes(group=interaction(irrigation, cultivar, inoculum, block) ), size=0.2, alpha=0.6) +
		geom_line(data={ summ.trtm.irin.dat %>% filter(season %in% c("2019-2020","2020-2021") ) }, size=1) +
		facet_grid(cols=vars(season), rows=vars(inoculum), scales="free_x") +
		scale_x_date(limits=facet_limits, breaks=facet_breaks, date_labels="%b %d", date_minor_breaks="2 week") +
		scale_y_continuous(limits=c(0,100) ) +
		scale_linetype_manual(values=c("42","22","solid"), labels=c("Low","Optimal","High") ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9), axis.title.x=element_text(margin=margin(t=5.5)) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank(), strip.text.x=element_text(margin=margin(2.2,4.4,2.2,4.4, "pt") ) ) + # reduce margins, default all = 4.4 pt
		theme(legend.position=c(0.11,0.81), legend.title=element_text(size=8, margin=margin(b=8.25) ), legend.text=element_text(size=7), legend.spacing.y=unit(-0.50, "lines") ) +
		theme(plot.tag.position=c(0.04,0.98) ) +
		guides(color=guide_legend(nrow=3, byrow=TRUE), linetype=guide_legend(nrow=3, byrow=TRUE)) +
		labs(x="Rating Date", y="Mortality Incidence (%)", color="Soil moisture", linetype="Soil moisture", tag="A")
	}
	
	## dotplot plot averages
	plot.irin.dot = summ.plot.allt.avg %>% filter(season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=irrigation, y=severity_mort_perc) ) +
		geom_dotplot(binaxis="y", binwidth=1.5, dotsize=1.4, stackdir="center", stackratio=1.25) +
		geom_crossbar(data={ summ.trtm.irin.avg %>% filter(season %in% c("2019-2020","2020-2021") ) }, aes(ymin=severity_mort_perc, ymax=severity_mort_perc), width=0.3, fatten=2, color="red") +
		geom_text(data={ summ.trtm.irin.avg %>% filter(season %in% c("2019-2020","2020-2021") ) }, aes(label=severity_mort_perc), size=2.5, color="red", hjust=1, nudge_x=-0.25 ) +
		geom_text(data=annot.irin, aes(label=text_label), size=3) +
		facet_grid(cols=vars(season), rows=vars(inoculum)) +
		scale_y_continuous(limits=c(0,100) ) +
		scale_x_discrete(expand=expansion(add=c(0.8,0.4) ) ) + # shift ticks to right by adding buffer to left and removing buffer to right (default = 0.6)
		theme_bw() +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9), axis.title.y=element_blank() ) +
		theme(strip.text.x=element_text(margin=margin(2.2,4.4,2.2,4.4, "pt") ), strip.text.y=element_text(margin=margin(4.4,2.2,4.4,2.2) ) ) + # reduce margins, default all = 4.4 pt
		theme(plot.tag.position=c(0.04,0.98) ) +
		labs(x="Soil moisture", tag="B")
	}

### cultivar x inoculum
	## line graph all dates
	plot.cuin.line = summ.plot.allt.dat %>% filter(season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=date, y=severity_mort_perc, color=cultivar, linetype=cultivar ) ) +
		geom_line(aes(group=interaction(irrigation, cultivar, inoculum, block) ), size=0.2, alpha=0.6) +
		geom_line(data={ summ.trtm.cuin.dat %>% filter(season %in% c("2019-2020","2020-2021") ) }, size=1) +
		facet_grid(cols=vars(season), rows=vars(inoculum), scales="free_x") +
		scale_x_date(limits=facet_limits, breaks=facet_breaks, date_labels="%b %d", date_minor_breaks="2 week") +
		scale_y_continuous(limits=c(0,100) ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank(), strip.text.x=element_text(margin=margin(2.2,4.4,2.2,4.4, "pt") ) ) + # reduce margins, default all = 4.4 pt
		theme(legend.position=c(0.11,0.84), legend.title=element_text(size=8, margin=margin(b=8.25) ), legend.text=element_text(size=7), legend.spacing.y=unit(-0.50, "lines") ) +
		theme(plot.tag.position=c(0.04,0.98) ) +
		guides(color=guide_legend(nrow=2, byrow=TRUE), linetype=guide_legend(nrow=2, byrow=TRUE)) +
		labs(x="Rating Date", y="Mortality Incidence (%)", color="Cultivar", linetype="Cultivar", tag="C")
	}
	
	## dotplot plot averages
	plot.cuin.dot = summ.plot.allt.avg %>% filter(season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=cultivar, y=severity_mort_perc) ) +
		geom_dotplot(binaxis="y", binwidth=1.5, dotsize=1.4, stackdir="center", stackratio=1.25) +
		geom_crossbar(data={ summ.trtm.cuin.avg %>% filter(season %in% c("2019-2020","2020-2021") ) }, aes(ymin=severity_mort_perc, ymax=severity_mort_perc), width=0.3, fatten=2, color="red") +
		geom_text(data={ summ.trtm.cuin.avg %>% filter(season %in% c("2019-2020","2020-2021") ) }, aes(label=severity_mort_perc), size=2.5, color="red", hjust=1, nudge_x=-0.25 ) +
		geom_bracket(data=annot.cuin, aes(xmin=xmin, xmax=xmax, y.position=y_pos, label=p_value), label.size=3, vjust=-0.2) +
		facet_grid(cols=vars(season), rows=vars(inoculum)) +
		scale_y_continuous(limits=c(0,100) ) +
		theme_bw() +
		theme(plot.tag.position=c(0.04,0.98) ) +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9), axis.title.y=element_blank() ) +
		theme(strip.text.x=element_text(margin=margin(2.2,4.4,2.2,4.4, "pt") ), strip.text.y=element_text(margin=margin(4.4,2.2,4.4,2.2) ) ) + # reduce margins, default all = 4.4 pt
		labs(x="Cultivar", tag="D")
	}
	
	plot.irin = wrap_plots(plot.irin.line, plot.irin.dot, nrow=1, widths=c(2.4,1) )
	
	plot.cuin = wrap_plots(plot.cuin.line, plot.cuin.dot, nrow=1, widths=c(2.4,1) )
	
	plot.both = wrap_plots(plot.irin, plot.cuin, ncol=1)
	
	ggplot2::ggsave(file="./4_results/figure-3_charcoal-rot-mortality_19_20.png", device="png", plot=plot.both, width=7, height=6.5, units="in", dpi=600)
      	  
	
############################
##### VISUALIZE - 2018 #####
############################

### inoculum
	## line graph all dates
	plot.inoc.line = summ.plot.allt.dat %>% filter(season == "2018-2019" & period == "rating" & date != as_date("2019-05-08") ) %>% {
	ggplot(., aes(x=date, y=severity_mort_perc, color=inoculum, linetype=inoculum ) ) +
		geom_line(aes(group=interaction(irrigation, cultivar, inoculum, block) ), size=0.2, alpha=0.6) +
		geom_line(data={ summ.trtm.inoc.dat %>% filter(season == "2018-2019" & period == "rating" & date != as_date("2019-05-08") ) }, size=1) +
		facet_grid(cols=vars(season) ) +
		scale_x_date(limits=as_date(c("2019-05-15","2019-06-26")), breaks=as_date(c("2019-05-22","2019-06-05","2019-06-19")), date_labels="%b %d", date_minor_breaks="2 week") +
		scale_y_continuous(limits=c(-1,100) ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9), axis.title.x=element_text(margin=margin(t=5.5)) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
		theme(legend.position=c(0.40,0.85), legend.title=element_text(size=7, margin=margin(b=6) ), legend.text=element_text(size=6), legend.spacing.y=unit(-0.50, "lines"), legend.background=element_blank(), legend.key=element_blank() ) +
		theme(plot.tag.position=c(0.15,0.96) ) +
		guides(color=guide_legend(nrow=3, byrow=TRUE), linetype=guide_legend(nrow=3, byrow=TRUE)) +
		labs(x="Rating Date", y="Mortality Incidence (%)", color="Inoculum", linetype="Inoculum", tag="A")
	}
	
	## dotplot plot averages
	plot.inoc.dot = summ.plot.allt.avg %>% filter(season == "2018-2019" & period == "rating") %>% {
	ggplot(., aes(x=inoculum, y=severity_mort_perc) ) +
		geom_point(shape=1, size=1, position=position_jitter(w=0.15) ) +
		geom_crossbar(data=summ.18.trtm.inoc.avg, aes(ymin=severity_mort_perc, ymax=severity_mort_perc), width=0.3, fatten=2, color="red") +
		geom_text(data=summ.18.trtm.inoc.avg, aes(label=severity_mort_perc), size=2.5, color="red", hjust=1, nudge_x=-0.3 ) +
		geom_bracket(xmin="Control", xmax="Inoculated", y.position=90, label="0.0068", label.size=3) +
		facet_grid(cols=vars(season)) +
		scale_y_continuous(limits=c(-1,100) ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=9, angle=30, hjust=1), axis.text.y=element_text(size=9), axis.title.y=element_blank() ) +
		theme(plot.tag.position=c(0.08,0.98) ) +
		labs(x="Inoculum", tag="B")
	}

	plot.inoc = wrap_plots(plot.inoc.line, plot.inoc.dot, nrow=1, widths=c(1.25,1) )
	
	# dimensions set by trial and error to closely approximate 2019/2020 interaction graph above
	ggplot2::ggsave(file="./4_results/supp-figure-S9_charcoal-rot-mortality_18.png", device="png", plot=plot.inoc, width=3.1, height=3.4, units="in", dpi=600)


####################################
##### VISUALIZE - OBSERVATIONS #####
####################################
	# not shown
	
### specify breaks, limits for facets
	## limits; to make the scale the same for all seasons
	facet_limits_obs = function(x) {
		if (max(x) 			< as_date("2019-08-01")) { as_date(c("2018-11-01","2019-07-01"))
		} else if (max(x) 	< as_date("2020-08-01")) { as_date(c("2019-11-01","2020-07-01"))
		} else if (max(x) 	< as_date("2021-08-01")) { as_date(c("2020-11-01","2021-07-01"))
		} else { NULL }
	}

	## breaks
	facet_breaks_obs = function(x) {
		if (max(x) 			< as_date("2019-08-01")) { as_date(c("2018-12-01","2019-02-01","2019-04-01","2019-06-01") )
		} else if (max(x) 	< as_date("2020-08-01")) { as_date(c("2019-12-01","2020-02-01","2020-04-01","2020-06-01") )
		} else if (max(x) 	< as_date("2021-08-01")) { as_date(c("2020-12-01","2021-02-01","2021-04-01","2021-06-01") )
		} else { NULL }
	}


### all factors
	## line graph all dates
	plot.obs.line = summ.plot.allt.dat %>% filter(season %in% c("2019-2020","2020-2021") ) %>% {
	ggplot(., aes(x=date, y=observation_mort_perc, color=irrigation, linetype=irrigation ) ) +
		geom_line(aes(group=interaction(irrigation, cultivar, inoculum, block) ), size=0.2, alpha=0.6) +
#		geom_line(data={ summ.trtm.irin.dat %>% filter(season %in% c("2019-2020","2020-2021") ) }, size=1) +
		facet_grid(cols=vars(season, cultivar), rows=vars(inoculum), scales="free_x") +
		scale_x_date(limits=facet_limits_obs, breaks=facet_breaks_obs, date_labels="%b %d", date_minor_breaks="1 month") +
		scale_y_continuous(limits=c(0,100) ) +
		scale_linetype_manual(values=c("42","22","solid"), labels=c("Low","Optimal","High") ) +
		theme_bw() +
		theme(axis.text.x=element_text(size=9), axis.text.y=element_text(size=9), axis.title.x=element_text(margin=margin(t=5.5)) ) +
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank(), strip.text.x=element_text(margin=margin(2.2,4.4,2.2,4.4, "pt") ) ) + # reduce margins, default all = 4.4 pt
#		theme(legend.position=c(0.11,0.81), legend.title=element_text(size=8, margin=margin(b=8.25) ), legend.text=element_text(size=7), legend.spacing.y=unit(-0.50, "lines") ) +
#		theme(plot.tag.position=c(0.04,0.98) ) +
#		guides(color=guide_legend(nrow=3, byrow=TRUE), linetype=guide_legend(nrow=3, byrow=TRUE)) +
		labs(x="Rating Date", y="Mortality Incidence (%)", color="Soil moisture", linetype="Soil moisture")
	}
#	ggplot2::ggsave(file="./4_results/z_not-shown_observations_mortality.png", device="png", plot=plot.obs.line, width=10, height=6, units="in", dpi=600)

