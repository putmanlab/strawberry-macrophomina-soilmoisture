##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Import and organize - Step 1					 #
# Pathogen isolations, disease severity			 #
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

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")


###############################
##### IMPORT - ISOLATIONS #####
###############################

### upload
	isolations2018 = read_csv("./2_data/Plant_Isolations_2018-2019 - All Isolation Data.csv", col_names=T, na="NA")
	isolations2019 = read_csv("./2_data/Plant_Isolations_2019-2020 - All Isolation Data.csv", col_names=T, na="NA")
	isolations2020 = read_csv("./2_data/Plant_Isolations_2020-2021 - All Isolation Data.csv", col_names=T, na="NA")    

### bind
	## get rid of unused columns and incorrect percent columns       
	isolations2018 = isolations2018 %>% select(-Stored, -Petiole_Mp, -Petiole_none, -Total_Petiole)
	isolations2019 = isolations2019 %>% select(-Percent_Roots_Mp, -Percent_Crown_Mp, -Stored, -New_Name, -Notes, -Symptomatic, -Mp)
	isolations2020 = isolations2020 %>% select(-Percent_Roots_Mp, -Percent_Crown_Mp, -Stored, -New_Name, -notes)

	## rename column
	isolations2020 = isolations2020 %>% rename(Crown_none=Crown_None)
	
	## add season
	isolations2018 = isolations2018 %>% mutate(Season="2018-2019")
	isolations2019 = isolations2019 %>% mutate(Season="2019-2020")
	isolations2020 = isolations2020 %>% mutate(Season=as.character("2020-2021") )
	
	## bind
	data.iso.in = bind_rows(isolations2018, isolations2019, isolations2020)

### organize
	## rename columns to lowercase
	data.iso.in = data.iso.in %>% rename(
		date=Date, block=Block, irrigation=Irrigation, cultivar=Cultivar, inoculum=Inoculum, plant=Plant, 
		root_mp=Roots_Mp, root_none=Roots_none, root_total=Total_Roots, crown_mp=Crown_Mp, crown_none=Crown_none, crown_total=Total_Crown, random=Random, season=Season)
	
	## convert column types
	data.iso.in = data.iso.in %>% mutate(date=mdy(date), block=as.character(block), plant=as.integer(plant) )		

	## convert to long
		# gather
		data.iso.in = data.iso.in %>% gather(key="variable", value="n_pieces", -date, -block, -irrigation, -cultivar, -inoculum, -plant, -random, -season)
		
		# convert to integer
		data.iso.in = data.iso.in %>% mutate(n_pieces=as.integer(n_pieces) ) 
		
		# separate variable
		data.iso.in = data.iso.in %>% separate(variable, into=c("tissue","result"), remove=TRUE)
		
		# spread
		data.iso.in = data.iso.in %>% spread(key=result, value=n_pieces)
		
		# rename
		data.iso.in = data.iso.in %>% rename(pieces_mp=mp, pieces_none=none, pieces_total=total)

	## calculate percent isolation incidence
		# calculate
		data.iso.in = data.iso.in %>% mutate(percent_mp=as.integer( round( (pieces_mp/pieces_total), digits=2) * 100) )
		
		# fix percent column for rows on first two dates with pieces_total == NA
		data.iso.in = data.iso.in %>% mutate(percent_mp=replace(percent_mp, pieces_mp == 0 & is.na(pieces_total), 0) )
		
	## adjust random column
		# make new column
		data.iso.in = data.iso.in %>% mutate(sample_type=case_when(
			(random %in% c("Y","yes") ) ~ "random",
			(random %in% c("N","no" ) ) ~ "diagnosis") )
			
		# remove old column
		data.iso.in = data.iso.in %>% select(-random)
		
		# fix incorrectly labeled plant
		data.iso.in = data.iso.in %>% mutate(sample_type=replace(
			sample_type, 
			date == "2020-06-23" & block == "4" & irrigation == "Low" & cultivar == "Fronteras" & inoculum == "Control" & plant == 18,
			"random") )
			       
	## Fix lowercase optimal in 2018-2019 
	data.iso.in = data.iso.in %>% mutate(irrigation=replace(irrigation, irrigation == "optimal", "Optimal") )
	
	## fix roots
	data.iso.in = data.iso.in %>% mutate(tissue=replace(tissue, tissue == "roots", "root") )

	## change order of columns
	data.iso.in = data.iso.in %>% select(season, irrigation, cultivar, inoculum, block, date, sample_type, plant, tissue, pieces_mp, pieces_none, pieces_total, percent_mp)

	## export
	write_csv(data.iso.in, file="./2_data_curated/isolation_import_step-1_for-check.csv", col_names=T, na="", append=F)	
	
	
#####################################
##### IMPORT - DISEASE SEVERITY #####
#####################################

### upload
	ratings1 = read_csv(file="./2_data/Field_Severity_Ratings_2018-2019 v2.csv", col_names=T, na="")
	ratings2 = read_csv(file="./2_data/Field_Severity_Ratings_2019-2020 v3.csv", col_names=T, na="")
	ratings3 = read_csv(file="./2_data/Field_Severity_Ratings_2020-2021 v3.csv", col_names=T, na="")

### bind
	## remove mite ratings from 2021 data
#	ratings3 = ratings3 %>% select(-`M-4/17/2021`, -`M-5/31/2021`)

	## gather
	ratings1 = ratings1 %>% gather(key="date", value="severity", -Cultivar, -Inoculum, -Irrigation, -Block, -Plant)
	ratings2 = ratings2 %>% gather(key="date", value="severity", -Cultivar, -Inoculum, -Irrigation, -Block, -Plant)

	## gather 2020-21
		# gather
		ratings3 = ratings3 %>% gather(key="date", value="severity", -Cultivar, -Inoculum, -Irrigation, -Block, -Plant)
		
		# add column to distinguish severity ratings
		ratings3 = ratings3 %>% mutate(rating=case_when(
			(date %in% c("M-4/17/2021","M-5/31/2021") ) ~ "mite",
			(!(date %in% c("M-4/17/2021","M-5/31/2021") )) ~ "charcoal rot") )
			
		# homogenize date format (although lubridate might be able to handle heterogeneous formats)
		ratings3 = ratings3 %>% mutate(date=replace(date, date == "M-4/17/2021", "17-Apr-21") )
		ratings3 = ratings3 %>% mutate(date=replace(date, date == "M-5/31/2021", "31-May-21") )
	
	## add season
	ratings1 = ratings1 %>% mutate(season="2018-2019", rating="charcoal rot" )
	ratings2 = ratings2 %>% mutate(season="2019-2020", rating="charcoal rot" )
	ratings3 = ratings3 %>% mutate(season="2020-2021")

	## bind
	data.dis.in = bind_rows(ratings1, ratings2, ratings3)

### organize
	## rename columns to lowercase
	data.dis.in = data.dis.in %>% rename(cultivar=Cultivar, inoculum=Inoculum, irrigation=Irrigation, block=Block, plant=Plant)
	
	## reorder columns
	data.dis.in = data.dis.in %>% select(season, irrigation, cultivar, inoculum, block, plant, rating, date, severity)
	
	## change column type
	data.dis.in = data.dis.in %>% mutate(block=as.character(block), plant=as.integer(plant), date=dmy(date))
	
	## clean up ratings		
		# notes that were both dead and sampled, indicating plant was observed to be dead on sample date
		data.dis.in = data.dis.in %>% mutate(severity=replace(severity, severity %in% c("dead, sample", "sample, dead"), "dead_sampled") )

		# notes with sample plus other; use "sampled" because "sample" is a protected word in R
		data.dis.in = data.dis.in %>% mutate(severity=replace(severity, severity == "flower, sample", "sampled") )

		# noted with replaced
		data.dis.in = data.dis.in %>% mutate(severity=replace(
			severity, 
			severity %in% c("falling to side, replaced", "out of ground, replaced", "partially removed from hole, replaced",
				"roots up, replaced", "up out of hole, replaced", "up, replaced"),
			"replaced") )

		# dead
		data.dis.in = data.dis.in %>% mutate(severity=replace(severity, severity == "d", "dead") )
		
		# gone in 2018-19, and 2020-21 mite
		data.dis.in = data.dis.in %>% mutate(severity=replace(severity, severity == "x", "gone") )
		
		# notes with other - remove
		data.dis.in = data.dis.in %>% mutate(severity=replace(
			severity, 
			severity %in% c("dying", "flower", "flower, sensor", "little green", "no dirt around roots", "no green",
				"not looking good", "out of hole", "really small", "sensor", "small", "very small",
				"replaced"),
			NA) )
			
		# check
		data.dis.in %>% group_by(severity) %>% summarize(ct=n()) %>% print(n=Inf)

	## Fix missing rating
	data.dis.in = data.dis.in %>% mutate(severity=replace(
		severity, 
		season == "2019-2020" & cultivar == "Fronteras" & irrigation == "Optimal" & inoculum == "Control" & block == "2" & plant == "9" & date == "2020-04-28",
		"0") )


######################################################
##### SEVERITY - MANUAL QUALITY CONTROL - OUTPUT #####
######################################################

# purpose is to get accurate status of plants (sampled, dead, etc) by comparison to events from isolation data

### disease severity
	## split each season
	data.dis.in.1 = data.dis.in %>% filter(season == "2018-2019")
	data.dis.in.2 = data.dis.in %>% filter(season == "2019-2020")
	data.dis.in.3 = data.dis.in %>% filter(season == "2020-2021")
	
	## add suffix to date column for mite ratings
		# add new column
		data.dis.in.3 = data.dis.in.3 %>% mutate(date_new=case_when(
			(rating == "charcoal rot") ~ paste(date, "_cr", sep=""),
			(rating == "mite") ~ paste(date, "_mite", sep="") ) )
			
		# remove column and rename new column
		data.dis.in.3 = data.dis.in.3 %>% select(-date) %>% rename(date=date_new)

	## remove rating column
	data.dis.in.1 = data.dis.in.1 %>% select(-rating)
	data.dis.in.2 = data.dis.in.2 %>% select(-rating)
	data.dis.in.3 = data.dis.in.3 %>% select(-rating)

	## spread
	data.dis.in.1 = data.dis.in.1 %>% spread(key=date, value=severity)
	data.dis.in.2 = data.dis.in.2 %>% spread(key=date, value=severity)
	data.dis.in.3 = data.dis.in.3 %>% spread(key=date, value=severity)

	## add column to indicate data type 
	data.dis.in.1 = data.dis.in.1 %>% mutate(type="severity_orig")
	data.dis.in.2 = data.dis.in.2 %>% mutate(type="severity_orig")
	data.dis.in.3 = data.dis.in.3 %>% mutate(type="severity_orig")

### isolation data
	## split each season	
	data.iso.in.1 = data.iso.in %>% filter(season == "2018-2019") 
	data.iso.in.2 = data.iso.in %>% filter(season == "2019-2020") 
	data.iso.in.3 = data.iso.in %>% filter(season == "2020-2021")

	## add suffix to date column to align with disease data
		# add new column
		data.iso.in.3 = data.iso.in.3 %>% mutate(date_new=paste(date, "_cr", sep="") )
			
		# remove column and rename new column
		data.iso.in.3 = data.iso.in.3 %>% select(-date) %>% rename(date=date_new)

	## collapse to list of individual plants
	data.iso.in.1 = data.iso.in.1 %>% distinct(season, irrigation, cultivar, inoculum, block, plant, date, sample_type)
	data.iso.in.2 = data.iso.in.2 %>% distinct(season, irrigation, cultivar, inoculum, block, plant, date, sample_type)
	data.iso.in.3 = data.iso.in.3 %>% distinct(season, irrigation, cultivar, inoculum, block, plant, date, sample_type)

	## spread
	data.iso.in.1 = data.iso.in.1 %>% spread(key=date, value=sample_type)
	data.iso.in.2 = data.iso.in.2 %>% spread(key=date, value=sample_type) 
	data.iso.in.3 = data.iso.in.3 %>% spread(key=date, value=sample_type)

	## add column to indicate data type
	data.iso.in.1 = data.iso.in.1 %>% mutate(type="sample_type")
	data.iso.in.2 = data.iso.in.2 %>% mutate(type="sample_type")
	data.iso.in.3 = data.iso.in.3 %>% mutate(type="sample_type")

### bind
	data.comb.1 = bind_rows(data.dis.in.1, data.iso.in.1)
	data.comb.2 = bind_rows(data.dis.in.2, data.iso.in.2)
	data.comb.3 = bind_rows(data.dis.in.3, data.iso.in.3)

### export
	# !!! manual quality control completed, would need to adjust spreadsheets if there are any changes
#	write_csv(data.comb.1, file="./2_data_curated/severity-isolation_manual-qual-control_step-1_2019.csv", col_names=T, na="", append=F)	
#	write_csv(data.comb.2, file="./2_data_curated/severity-isolation_manual-qual-control_step-1_2020.csv", col_names=T, na="", append=F)	
#	write_csv(data.comb.3, file="./2_data_curated/severity-isolation_manual-qual-control_step-1_2021.csv", col_names=T, na="", append=F)	


