##################################################
# STRAWBERRY Macrophomina            			 #
# Influence of soil moisture on disease		     #
# Summarize and visualize 						 #
# Greenhouse - data logger						 #
##################################################

## built on Docker putmanlab/exploratory-analysis:420.0

if (!require(conflicted)) {
  install.packages("conflicted")
  library(conflicted)
}

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/strawberry-macrophomina-soilmoisture")

## removed - could not find data file

### import
#data.in = read_csv(file="./2_data/5 2021-04-23 17_11_07 -0700.csv", col_names=T)
#
#data.in = data.in %>% select(datetime=`Date Time, GMT -0800`, temp=`Temp - Avg, °F`, rh=`RH - Avg, %`)
#
#data.df = data.in %>% mutate(datetime=ymd_hms(datetime, tz="Etc/GMT+8"))
#
### 
#data.df = data.df %>% group_by(datetime=ceiling_date(datetime, "hour")) %>% summarize(temp=mean(temp, na.rm=T), rh=mean(rh, na.rm=T))
#
#data.df = data.df %>% gather(key="variable", value="value", -datetime)
#
#plot.dat = ggplot(data.df, aes(x=datetime, y=value)) +
#	geom_line() +
#	facet_grid(variable ~ ., scales="free_y") +
#	theme_bw()
#ggplot2::ggsave(file="./4_results/z_not-shown_greenhouse_logger_5.png", device="png", plot=plot.dat, width=10, height=6, units="in")
