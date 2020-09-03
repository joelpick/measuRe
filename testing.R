rm(list=ls())
source("~/Dropbox/measuRe/measuRe.R")
source("~/Dropbox/measuRe/length_area_functions.R")
source("~/Dropbox/measuRe/plot_functions.R")


all_data <- measuRe("~/Dropbox/measuRe/test", x11=FALSE)

extract_length(all_data)

extract_area(all_data)
 
plot_area(3,all_data,"~/Dropbox/measuRe/test")



install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
setwd("~/Dropbox")
create("measuRe")
setwd("~/Dropbox/measuRe")
document()