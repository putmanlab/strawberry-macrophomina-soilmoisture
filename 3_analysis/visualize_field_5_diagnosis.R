#########################################################
# STRAWBERRY Macrophomina            			 		#
# Influence of soil moisture on disease		     		#
# Summarize and visualize 						 		#
# Isolation of pathogen from host tissue - diagnostic   #
#########################################################

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
#remotes::install_version("ggsci", version="3.0.0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("ggsignif", version="0.6.4", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("ggpubr", version="0.4", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#library(ggpubr) # for geom_bracket

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/sb-mac-irr")


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
		
	data.iso %>% 
		filter(season == "2020-2021" & date >= as_date("2021-05-03") & tissue == "crown") %>%
		group_by(date, sample_type, observation_status, severity_status) %>%
		summarize(ct=n()) %>%
		print(n=Inf)		

	## 2. diagnostic - everything except random plants that were alive at time of sampling
	data.iso.2 = data.iso %>% filter(
		sample_type == "diagnosis" |
		(sample_type == "random" & observation_status == "dead") |
		(sample_type == "random" & observation_status == "none" & severity_status == "dead") )

		# check
#		data.iso.2 %>% 
#			group_by(sample_type, observation_status, severity_status) %>%
#			summarize(ct=n()) %>%
#			print(n=Inf)
			
		# check if there is enough data is each treatment for statistical analysis
			# irrigation x inoculum interaction
#			data.iso.2 %>%
#				group_by(season, tissue, irrigation, inoculum) %>%
#				summarize(ct_block=n_distinct(block), ct=n(), ct_plant=n_distinct(plant), sum_pieces=sum(pieces_mp, na.rm=TRUE), ct_na=sum(is.na(pieces_total)) ) %>%
#				print(n=Inf)

			# cultivar x inoculum interaction
#			data.iso.2 %>%
#				group_by(season, tissue, cultivar, inoculum) %>%
#				summarize(ct_block=n_distinct(block), ct=n(), ct_plant=n_distinct(plant), sum_pieces=sum(pieces_mp, na.rm=TRUE), ct_na=sum(is.na(pieces_total)) ) %>%
#				print(n=Inf)

	## 3. diagnostic - first sample date after mite outbreak in 2020-2021
	data.iso.3 = data.iso %>% filter(date == as_date("2021-05-03") )

	## 4. diagnostic - period after mite outbreak in 2020-2021
	data.iso.4 = data.iso %>% filter(season == "2020-2021" & date >= as_date("2021-05-03") )


#####################
##### SUMMARIZE #####
#####################

### plot level - percent plants positive (>= 1 piece positive) over each plot
	## calculate total
	summ.plot = data.iso.2 %>% 
		group_by(season, irrigation, cultivar, inoculum, block, tissue) %>%
		summarize(plants_mp=sum(pieces_mp > 0, na.rm=TRUE), plants_total=n() ) %>%
		ungroup()

### main plot level
	## percent plants positive (>= 1 piece positive)
		# 4
		summ.main.plant.4.a = data.iso.4 %>% 
			group_by(season, irrigation, block, tissue) %>%
			summarize(plants_mp=sum(pieces_mp > 0, na.rm=TRUE), plants_total=n() ) %>%
			mutate(percent_plants_mp=round( (plants_mp/plants_total), digits=2) * 100 ) %>%
			ungroup()

		summ.main.plant.4.b = data.iso.4 %>% 
			filter(sample_type == "diagnosis") %>% 
			group_by(season, irrigation, block, tissue) %>%
			summarize(plants_mp=sum(pieces_mp > 0, na.rm=TRUE), plants_total=n() ) %>%
			mutate(percent_plants_mp=round( (plants_mp/plants_total), digits=2) * 100 ) %>%
			ungroup()

	## percent pieces positive; find sum first
		# 3
		summ.main.piece.3 = data.iso.3 %>% 
			group_by(season, irrigation, block, tissue) %>%
			summarize(pieces_mp=sum(pieces_mp, na.rm=TRUE), pieces_total=sum(pieces_total, na.rm=TRUE) ) %>%
			mutate(percent_pieces_mp=round( (pieces_mp/pieces_total), digits=2) * 100 ) %>%		
			ungroup()

		# 4
		summ.main.piece.4.a = data.iso.4 %>% 
			group_by(season, irrigation, block, tissue) %>%
			summarize(pieces_mp=sum(pieces_mp, na.rm=TRUE), pieces_total=sum(pieces_total, na.rm=TRUE) ) %>%
			mutate(percent_pieces_mp=round( (pieces_mp/pieces_total), digits=2) * 100 ) %>%		
			ungroup()

		summ.main.piece.4.b = data.iso.4 %>% 
			filter(sample_type == "diagnosis") %>% 
			group_by(season, irrigation, block, tissue) %>%
			summarize(pieces_mp=sum(pieces_mp, na.rm=TRUE), pieces_total=sum(pieces_total, na.rm=TRUE) ) %>%
			mutate(percent_pieces_mp=round( (pieces_mp/pieces_total), digits=2) * 100 ) %>%		
			ungroup()

### averages - overall for results text
	 ## by inoculum treatment; find sum first
	 summ.plot %>%
		group_by(season, tissue, inoculum) %>%
		summarize(plants_mp_total=sum(plants_mp, na.rm=TRUE), plants_total_total=sum(plants_total, na.rm=TRUE) ) %>%
		ungroup() %>%
		mutate(percent_plants_mp=round( (plants_mp_total/plants_total_total), digits=2) * 100 ) %>%
		arrange(tissue, season, inoculum)

### main plot - export prep
	## rename columns
	summ.main.plant.4.a = summ.main.plant.4.a %>% rename(mp=plants_mp, total=plants_total, percent_mp=percent_plants_mp)
	summ.main.plant.4.b = summ.main.plant.4.b %>% rename(mp=plants_mp, total=plants_total, percent_mp=percent_plants_mp)
	summ.main.piece.3 = summ.main.piece.3 %>% rename(mp=pieces_mp, total=pieces_total, percent_mp=percent_pieces_mp)
	summ.main.piece.4.a = summ.main.piece.4.a %>% rename(mp=pieces_mp, total=pieces_total, percent_mp=percent_pieces_mp)
	summ.main.piece.4.b = summ.main.piece.4.b %>% rename(mp=pieces_mp, total=pieces_total, percent_mp=percent_pieces_mp)
	
	## add group column
	summ.main.plant.4.a = summ.main.plant.4.a %>% mutate(summ_diag="plant_period_all")
	summ.main.plant.4.b = summ.main.plant.4.b %>% mutate(summ_diag="plant_period_diag")
	summ.main.piece.3 = summ.main.piece.3 %>% mutate(summ_diag="piece_first")
	summ.main.piece.4.a = summ.main.piece.4.a %>% mutate(summ_diag="piece_period_all")
	summ.main.piece.4.b = summ.main.piece.4.b %>% mutate(summ_diag="piece_period_diag")
	
	## bind
	summ.main = bind_rows(summ.main.plant.4.a, summ.main.plant.4.b, summ.main.piece.3, summ.main.piece.4.a, summ.main.piece.4.b)
	
	## remove roots
		# filter
		summ.main = summ.main %>% filter(tissue == "crown")
		
		# remove column
		summ.main = summ.main %>% select(-tissue)	

### export
	# crowns only due to insufficient data for roots (only some plants in this dataset are random-sampled plants that were removed from isolation dataset)

	## main plot
	write_csv(summ.main, file="./2_data_curated/z_not-shown_summary_main-plot_isolation_diagnosis-random.csv", col_names=T, na=".", append=F)		

	## select tissue
#	summ.plot.sas.crown = summ.plot %>% filter(tissue == "crown")
	
	## remove unneeded columns
#	summ.plot.sas.crown = summ.plot.sas.crown %>% select(-tissue)
	
	## export
#	write_csv(summ.plot.sas.crown, file="./2_data_curated/diagnosis_final_summ-plot_crown_SAS.csv", col_names=T, na=".", append=F)