##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Mite - severity								 #
# spatial covariate								 #
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
#library(patchwork) # for wrap_plots
#library(RColorBrewer) # for bar fill colors

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")

## not used

##################
##### IMPORT #####
##################

#### severity index residuals
#	in.resid = read_csv("./2_data_curated/severity_mite-index_residuals.csv", col_names=T, na="NA")
#
#### treatment positions
#	in.pos = read_csv("./1_methods/2020_SB_Mac_Field_Layout.csv", col_names=T, na="")
#	
#
################################
###### NEIGHBOR - ORGANIZE #####
################################
#
#### residuals
#	## gather
#	in.resid = in.resid %>% gather(key="residual_type", value="residual", -season, -irrigation, -cultivar, -inoculum, -block, -mite_severity_index)
#
#	## remove string
#	data.resid = in.resid %>% mutate(residual_type=str_replace(residual_type, "mite_index_", "") )
#	
#	## change column type
#	data.resid = data.resid %>% mutate(block=as.integer(block) )
#	
#	## update optimal for joining
#	data.resid = data.resid %>% mutate(irrigation=replace(irrigation, irrigation == "Opti", "Optimal") )
#
#### positions
#	## split
#	in.pos.1 = in.pos %>% select(treat_block_1, irrigation_1, block_1, main_row_1, sub_row_1)
#	in.pos.2 = in.pos %>% select(treat_block_2, irrigation_2, block_2, main_row_2, sub_row_2)
#	in.pos.3 = in.pos %>% select(treat_block_3, irrigation_3, block_3, main_row_3, sub_row_3)
#	in.pos.4 = in.pos %>% select(treat_block_4, irrigation_4, block_4, main_row_4, sub_row_4)
#
#	## rename columns
#	in.pos.1 = in.pos.1 %>% rename(treatment=treat_block_1, irrigation=irrigation_1, block=block_1, main_row=main_row_1, sub_row=sub_row_1)
#	in.pos.2 = in.pos.2 %>% rename(treatment=treat_block_2, irrigation=irrigation_2, block=block_2, main_row=main_row_2, sub_row=sub_row_2)
#	in.pos.3 = in.pos.3 %>% rename(treatment=treat_block_3, irrigation=irrigation_3, block=block_3, main_row=main_row_3, sub_row=sub_row_3)
#	in.pos.4 = in.pos.4 %>% rename(treatment=treat_block_4, irrigation=irrigation_4, block=block_4, main_row=main_row_4, sub_row=sub_row_4)
#	
#	## bind
#	plot.pos = bind_rows(in.pos.1, in.pos.2, in.pos.3, in.pos.4)
#	
#	## add treatment info
#	plot.pos = plot.pos %>% mutate(
#		cultivar=case_when(
#			(treatment %in% c("FI","FC") ) ~ "Fronteras",
#			(treatment %in% c("MI","MC") ) ~ "Monterey"),
#		inoculum=case_when(
#			(treatment %in% c("FC","MC") ) ~ "Control",
#			(treatment %in% c("FI","MI") ) ~ "Inoculated"),
#		season=as.character("2020-2021") )
#			
#	## change column type
#	plot.pos = plot.pos %>% mutate(across(c(block, main_row, sub_row), ~ as.integer(.x) ) )
#	
#	## order columns, remove unneeded columns (including main_row)
#	plot.pos = plot.pos %>% select(season, irrigation, cultivar, inoculum, block, sub_row)
#
#### get neighbor positions
#	## assign positions; L is needed in "1L" to force as integer, otherwise result is changed to numeric
#	plot.pos.near = plot.pos %>% mutate(
#		above_block=if_else(sub_row >= 2,  block, NA_integer_),
#		below_block=if_else(sub_row <= 11, block, NA_integer_),
#		right_block=if_else(block %in% c(1,2,3), block + 1L, NA_integer_),
#		 left_block=if_else(block %in% c(2,3,4), block - 1L, NA_integer_),
#		 
#		above_subrow=if_else(sub_row >= 2,  sub_row - 1L, NA_integer_),
#		below_subrow=if_else(sub_row <= 11, sub_row + 1L, NA_integer_),
#		right_subrow=if_else(block %in% c(1,2,3), sub_row, NA_integer_),
#		 left_subrow=if_else(block %in% c(2,3,4), sub_row, NA_integer_) )
#	
#	## gather
#	plot.pos.near = plot.pos.near %>% gather(key="variable", value="value", -season, -irrigation, -cultivar, -inoculum, -block, -sub_row)
#	
#	## separate column
#	plot.pos.near = plot.pos.near %>% separate(variable, into=c("direction","level"), sep="_")
#	
#	## replace names for spreading
#	plot.pos.near = plot.pos.near %>% mutate(level=replace(level, level == "block", "neighbor_block") )
#	plot.pos.near = plot.pos.near %>% mutate(level=replace(level, level == "subrow", "neighbor_subrow") )
#	
#	## spread level
#	plot.pos.near = plot.pos.near %>% spread(key=level, value=value)
#		
#	## remove NAs
#	plot.pos.near = plot.pos.near %>% filter(!is.na(neighbor_block) )
#
#
#################################
###### NEIGHBOR - CALCULATE #####
#################################
#
#### join
#	## add subplot info to residual df
#	data.resid = data.resid %>% left_join(plot.pos, by=c(c("season","irrigation","cultivar","inoculum","block")) )
#	
#	## join
#	data.resid.near = plot.pos.near %>% left_join( { data.resid %>% select(-season, -irrigation, -cultivar, -inoculum) }, by=c("neighbor_block" = "block", "neighbor_subrow" = "sub_row") )	
#
#### calculate average
#	# calculate
#	summ.resid.near = data.resid.near %>% 
#		group_by(season, irrigation, cultivar, inoculum, block, sub_row, residual_type) %>% 
#		summarize(residual=mean(residual, na.rm=T), ct=n() ) %>% 
#		ungroup()
#
#	# organize
#		# spread
#		summ.resid.near.t = summ.resid.near %>% spread(key=residual_type, value=residual)
#	
#		# rename columns
#		summ.resid.near.t = summ.resid.near.t %>% rename(resid_standard=residual, resid_pearson=pearson)
#		
#		# remove unneeded columns
#		summ.resid.near.t = summ.resid.near.t %>% select(-ct)
#
#### visualize	
#	plot.resid.1 = summ.resid.near %>% filter(residual_type == "residual") %>% 
#	ggplot(., aes(x=block, y=residual)) +
#		geom_bar(stat="identity") +
#		facet_grid(rows=vars(sub_row) ) +
#		theme_bw()
##	ggplot2::ggsave(file="./4_results/z_not-shown_mite-index_residuals-standard.png", device="png", plot=plot.resid.1, width=4, height=8, units="in", dpi=600)
#
#	plot.resid.2 = summ.resid.near %>% filter(residual_type == "pearson") %>% 
#	ggplot(., aes(x=block, y=residual)) +
#		geom_bar(stat="identity") +
#		facet_grid(rows=vars(sub_row) ) +
#		theme_bw()
##	ggplot2::ggsave(file="./4_results/z_not-shown_mite-index_residuals-pearson.png", device="png", plot=plot.resid.2, width=4, height=8, units="in", dpi=600)
#	
#
########################
###### COORDINATES #####
########################
#
#### identify plot center; based on x/y coordinates with 0 at top left corner of plot
#	## calculate x
#		# plots 64 in (=162.56 cm) wide; center of block 1 = half of plot of origin
#	plot.pos.xy = plot.pos %>% mutate(x = 81.28 + ((block - 1) * 162.56) )
#		
#	## calculate y
#		# subplots 3 m (300 cm) long, with 3 m alley between main plots
#		
#		# add column of added distance for main plot alleys (300 cm between each main plot)
#		plot.pos.xy = plot.pos.xy %>% mutate(y_alley=case_when(
#			(sub_row %in% c(1:4) ) ~ 0,
#			(sub_row %in% c(5:8) ) ~ -300,
#			(sub_row %in% c(9:12)) ~ -600) )
#		
#		# calculate distance; subplots 300 cm long (center of first plot is 150 cm from top)
#		plot.pos.xy = plot.pos.xy %>% mutate(y = -150 + ((sub_row -1) * -300) + y_alley)
#		
#	## convert to m
#	plot.pos.xy = plot.pos.xy %>% mutate(across(c(x, y), ~ round( (.x/100), digits=2) ) )
#		
#### straight line distance from origin
#	## calculate
#	plot.pos.xy = plot.pos.xy %>% mutate(distance_topleft = round( ( ( x^2 + y^2 )^0.5 ), digits=2) )
#
#
#################
###### JOIN #####
#################
#
#### join residuals and coordinates
#	plot.pos.f = summ.resid.near.t %>% left_join(plot.pos.xy, by=c(c("season","irrigation","cultivar","inoculum","block","sub_row")) )
#	
#	## remove unneeded column
#	plot.pos.f = plot.pos.f %>% select(-y_alley, -sub_row)
#	
#### join with mite severity
#	## import
#	data.mite.1 = read_csv("./2_data_curated/severity_final_mite-rating_dead-removed_SAS.csv", col_names=T, na=".")
#	
#	## change column type
#	data.mite.1 = data.mite.1 %>% mutate(block=as.integer(block) )
#	
#	## fix cultivar name
#	data.mite.1 = data.mite.1 %>% mutate(cultivar=replace(cultivar, cultivar == "zMonterey", "Monterey") )
#
#	## join
#	data.mite.1.resid = data.mite.1 %>% left_join(plot.pos.f, by=c(c("season","irrigation","cultivar","inoculum","block")) )
#	
#### create concatenated sub-plot treatment column
#	data.mite.1.resid = data.mite.1.resid %>% unite("cultivar_inoculum", c("cultivar","inoculum"), remove=FALSE)
#
#### export
#	write_csv(data.mite.1.resid, file="./2_data_curated/severity_final_mite-severity_dead-removed_spatial-covariates_SAS.csv", col_names=T, na=".", append=F)
#
#		
#		
#		