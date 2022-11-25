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

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_cohort<-paste("select nhs_number,arr_date as index_event_time
  from MODELLING_SQL_AREA.dbo.swd_activity
  where arr_date >= '2022-08-01'
  and arr_date <= '2022-08-07'
  and pod_l1='999'
  and pod_l2b='see & treat'")
cohort<-sqlQuery(con,string_cohort)
close(con)


# 2. DEFINITION OF PRE-POST WINDOW COVERAGE

# need to define the window length pre and post, in hours
# these are specified in the variables 'pre_length' and 'post_length'
# also need to define the activity types to cover in the window
# this is done in variable 'activity_coverage'
# the first column 'activity_name' contains the user-specified activity type name
# the second column 'activity_filter' contains the swd activity table filtering

pre_length<-96
post_length<-96

activity_coverage<-data.frame(activity_name=character(0),activity_filter=character(0)) %>%
  add_row(activity_name="A+E",activity_filter="pod_l1=='secondary' & pod_l2a=='ae'") %>%
  add_row(activity_name="999 (all types)",activity_filter="pod_l1=='999'") %>%
  add_row(activity_name="111",activity_filter="pod_l1=='111'") %>%
  add_row(activity_name="GP",activity_filter="pod_l1=='primary_care_contact'")


# 3. REPORT ATTRIBUTES

# define what appears on the word doc outputted

report_title<-"What happens 96 hours before and after 999 see & treat calls within the first week of August 2022"
report_author<-"Some One (some.one123@nhs.net)"



source(paste0(getwd(),"/scripts/code.R"))

