#########################################################
# STRAWBERRY Macrophomina            			 		#
# Influence of soil moisture on disease		     		#
# Summarize and visualize 						 		#
# Mite severity - isolation correlation				    #
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
	summ.diag = read_csv("./2_data_curated/z_not-shown_summary_main-plot_isolation_diagnosis-random.csv", col_names=T, na="NA")
	summ.mite = read_csv("./2_data_curated/z_not-shown_summary_main-plot_mite-severity.csv", col_names=T, na="NA")
	
## join
	data.summ = summ.mite %>% left_join(summ.diag, by=c(c("season","irrigation","block")) )
	

#####################
##### VISUALIZE #####
#####################

###
	plot.1 = ggplot(data.summ, aes(x=mite_severity, y=percent_mp, shape=irrigation, color=irrigation) ) +
		geom_point() +
		geom_smooth(aes(color=irrigation, linetype=irrigation), method="lm", size=0.4, se=F) +
		facet_grid(rows=vars(summ_diag), cols=vars(summ_mite), scales="free_x" ) +
		theme_bw()
		
	ggplot2::ggsave(file="./4_results/z_not-shown_summary_correlate_mite-diagnosis.png", device="png", plot=plot.1, width=6, height=8, units="in", dpi=600)

