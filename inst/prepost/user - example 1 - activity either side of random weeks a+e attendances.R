rm(list=ls())
library(RODBC)
library(lubridate)
library(tidyverse)
library(knitr)
library(scales)
library(writexl)
library(bupaR)
library(processmapR)


processmapR::tr


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
  and prov_code in ('ra700','rvj00')
  and pod_l2a = 'ae'")
cohort<-sqlQuery(con,string_cohort)
close(con)


# 2. DEFINITION OF PRE-POST WINDOW COVERAGE

# need to define the window length pre and post, in hours
# these are specified in the variables 'pre_length' and 'post_length'
# also need to define the activity types to cover in the window
# this is done in variable 'activity_coverage'
# the first column 'activity_name' contains the user-specified activity type name
# the second column 'activity_filter' contains the swd activity table filtering

pre_length<-72
post_length<-72

activity_coverage<-data.frame(activity_name=character(0),activity_filter=character(0)) %>%
  add_row(activity_name="A+E",activity_filter="pod_l1=='secondary' & pod_l2a=='ae'") %>%
  add_row(activity_name="OP T+O",activity_filter="pod_l1=='secondary' & pod_l2a=='op' & spec_l1a=='110'") %>%
  add_row(activity_name="Community in-person",activity_filter="pod_l1=='community' & pod_l3=='physical'")


# 3. REPORT ATTRIBUTES

# define what appears on the word doc outputted

report_title<-"What happens 72 hours before and after A+E attendances within the first week of August 2022"
report_author<-"Some One (some.one123@nhs.net)"



source(paste0(getwd(),"/scripts/code.R"))

