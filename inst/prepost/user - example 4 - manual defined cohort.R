rm(list=ls())
library(RODBC)
library(lubridate)
library(tidyverse)
library(knitr)
library(scales)
library(writexl)
library(bupaR)
library(processmapR)
setwd("S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/tools/internal/prepost_activity/")


# 1. DEFINITION OF COHORT OF INTEREST

# this must be a data frame called 'cohort'
# the first column should be named 'nhs_number' and the second 'index_event_time'
# it can be uploaded (read.csv) or created here in the code
# e.g. all those who have had an ED attendance in a given week


cohort<-read.csv("cohort - example 4.csv") %>%
  mutate(index_event_time=as.POSIXct(index_event_time))


# 2. DEFINITION OF PRE-POST WINDOW COVERAGE

# need to define the window length pre and post, in hours
# these are specified in the variables 'pre_length' and 'post_length'
# also need to define the activity types to cover in the window
# this is done in variable 'activity_coverage'
# the first column 'activity_name' contains the user-specified activity type name
# the second column 'activity_filter' contains the swd activity table filtering

pre_length<-48
post_length<-48

activity_coverage<-read.csv("activity_coverage - example 4.csv")


# 3. REPORT ATTRIBUTES

# define what appears on the word doc outputted

report_title<-"What happens 48 hours before and after some random / user-supplied index events"
report_author<-"Some One (some.one123@nhs.net)"



source(paste0(getwd(),"/scripts/code.R"))

