##########################################
# STRAWBERRY Macrophomina Abiotic Stress #
# Design of Experiments                  #
##########################################

library(agricolae)
library(dplyr)
library(readr)
library(tidyr)

############################################
# A. Experiment 1 - Drought Stress - Field #
############################################

### set up parameters
	## treatments
	e1r1.cult.inoc = c("Fronteras_inoculated","Fronteras_control","Monterey_inoculated","Monterey_control")
	e1r1.irri = c("low","optimal","high")
	e1r1.reps = 4
	
	## calculate number of experimental units
	e1r1.units = length(e1r1.irri) * length(e1r1.cult.inoc) * e1r1.reps

### get randomization
	## set random number seed *** !!! USE DIFFERENT SEED EACH TIME !!! ***
	## obtained from blind selection of random number in "A Million Random Digits With 100,000 Normal Deviates" by RAND Corporation
	e1r1.seed = 9800
	
	## randomize
	e1r1.des.out = design.split(trt1=e1r1.irri, trt2=e1r1.cult.inoc, r=e1r1.reps, design="rcbd", serie=2, seed=e1r1.seed, kinds="Super-Duper", first=TRUE, randomization=TRUE)		

	## extract design and add exp_unit numbers
	e1r1.des = as_tibble(e1r1.des.out$book) %>% mutate('exp_unit'=c(1:e1r1.units))
	
	## rename columns
	e1r1.des = e1r1.des %>% rename('irrigation'=e1r1.irri, 'cultivar_inoculum'=e1r1.cult.inoc)
	
	## separate cultivar_inoculum
	e1r1.des = e1r1.des %>% separate(cultivar_inoculum, into=c("cultivar","inoculum"), sep="_")

	## same seed = same randomization, every time; append seed and datetime to filename to prevent accidentally overwriting randomization
	write_csv(e1r1.des, path=paste("./1_methods/2020_SB-dis-resist_e1r1_randomized_", "seed-", e1r1.seed, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".csv", sep=""), na="", col_names=T, append=F)
	
	