# scale_fill_manual
# purrr::set_names(letters, letters)
# names=names, values=colours

options(dplyr.summarise.inform=FALSE)

systime<-Sys.time()
systime_clean<-gsub(":","-",systime)
print(paste0("Outputs will be deposited in folder: ",systime_clean),quote=FALSE)
print(paste0("Running..."),quote=FALSE)
dir.create(systime_clean)

# DATA CLEANING

cohort<-cohort %>%
  mutate(instance_id=1:nrow(.))


# get all activities in specified coverage window
con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_activity<-paste("select *
  from MODELLING_SQL_AREA.dbo.swd_activity
  where nhs_number in (",toString(cohort$nhs_number),")
  and arr_date >= '",format(as.POSIXlt(min(cohort$index_event_time))-hours(pre_length),"%Y-%m-%d %H:%M:%S"),"'
  and arr_date <= '",format(as.POSIXlt(max(cohort$index_event_time))+hours(post_length),"%Y-%m-%d %H:%M:%S"),"'
")
act<-sqlQuery(con,string_activity)
close(con)

# strip out the activities that duplicates the index 'event' or not within the various windows or permitted activity types
dat<-left_join(cohort,act,by="nhs_number") %>%
  filter(arr_date!=index_event_time) %>%
  filter(arr_date>=index_event_time-hours(pre_length) & arr_date<=index_event_time+hours(post_length))
dat2<-do.call("rbind",lapply(1:nrow(activity_coverage),function(i) {
  dat %>%
    mutate(activity_name=ifelse(!!rlang::parse_expr(activity_coverage$activity_filter[i]),activity_coverage$activity_name[i],"NA")) %>%
    filter(!activity_name=="NA") %>%
    select(nhs_number,index_event_time,instance_id,arr_date,activity_name)
})) %>%
  arrange(instance_id) %>%
  rename(activity_time=arr_date)

write_xlsx(list(`activity_log`=dat2),paste0(getwd(),"/",systime_clean,"/activity_log.xlsx"))

rm(act,dat)

print(paste0("Finished activity log creation..."),quote=FALSE)

################################################
################################################
# OUTPUT 1: DESCRIPTIVE ON COHORT

# figure for: age, sex, imd, cambridge score

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_attribute_cohort<-paste("select nhs_number,age,sex,lsoa,attribute_period
  from MODELLING_SQL_AREA.dbo.primary_care_attributes
  where nhs_number in (",toString(cohort$nhs_number),")
")
cohort_atts<-sqlQuery(con,string_attribute_cohort)
close(con)

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_imd_cohort<-paste("select [Index of Multiple Deprivation (IMD) Decile] as imd,[LSOA code] as lsoa
  from Analyst_SQL_Area.dbo.tbl_BNSSG_Datasets_LSOA_IMD_2019
  where [LSOA code] in (",toString(paste0("'",unique(cohort_atts$lsoa),"'")),")")
cohort_imd<-sqlQuery(con,string_imd_cohort)
close(con)

cohort_atts1<-cohort_atts %>%
  left_join(cohort_imd,by="lsoa") %>%
  select(-lsoa)

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_cms_cohort<-paste("select nhs_number,attribute_period,segment
  from MODELLING_SQL_AREA.dbo.New_Cambridge_Score
  where nhs_number in (",toString(cohort$nhs_number),")
")
cohort_cms<-sqlQuery(con,string_cms_cohort)
close(con)

cohort_atts2<-cohort %>%
  left_join(cohort_atts1,by="nhs_number") %>%
  mutate(across(c("age","sex","imd"),as.factor)) %>%
  pivot_longer(cols=c(age,sex,imd),names_to="metric",values_to="value") %>%
  mutate(diff=abs(difftime(index_event_time,attribute_period,units="days"))) %>%
  group_by(instance_id,metric) %>%
  filter(!is.na(value)) %>%
  arrange(diff) %>%
  slice(1) %>%
  pivot_wider(names_from=metric,values_from=value) %>%
  select(-c(attribute_period,diff)) %>%
  left_join(cohort_cms,by="nhs_number") %>%
  mutate(diff=abs(difftime(index_event_time,attribute_period,units="days"))) %>%
  group_by(instance_id) %>%
  filter(!is.na(segment)) %>%
  arrange(diff) %>%
  slice(1) %>%
  select(-c(attribute_period,diff)) %>%
  na.omit() %>%
  filter(sex %in% c("Male","Female")) %>%
  ungroup() %>%
  select(nhs_number,instance_id,age,imd,sex,segment) %>%
  mutate(grp="Patients in cohort")

rm(cohort_atts,cohort_atts1,cohort_imd,cohort_cms)

##########
# for general popn

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_attribute_popn<-paste("select nhs_number,age,sex,lsoa
  from MODELLING_SQL_AREA.dbo.swd_attribute
")
popn_atts<-sqlQuery(con,string_attribute_popn)
close(con)

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_imd_popn<-paste("select [Index of Multiple Deprivation (IMD) Decile] as imd,[LSOA code] as lsoa
  from Analyst_SQL_Area.dbo.tbl_BNSSG_Datasets_LSOA_IMD_2019
  where [LSOA code] in (",toString(paste0("'",unique(popn_atts$lsoa),"'")),")")
popn_imd<-sqlQuery(con,string_imd_popn)
close(con)

popn_atts1<-popn_atts %>%
  mutate(lsoa=toupper(lsoa)) %>%
  left_join(popn_imd,by="lsoa") %>%
  select(-lsoa)

con<-odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
string_cms_popn<-paste("select nhs_number,segment
  from MODELLING_SQL_AREA.dbo.New_Cambridge_Score
  where attribute_period IN (SELECT max(attribute_period) From MODELLING_SQL_AREA.dbo.New_Cambridge_Score)
")
popn_cms<-sqlQuery(con,string_cms_popn)
close(con)

popn_atts2<-popn_atts1 %>%
  left_join(popn_cms,by="nhs_number") %>%
  select(-nhs_number) %>%
  mutate(grp="General BNSSG population") %>%
  mutate(across(c("age","sex","imd"),as.factor)) %>%
  mutate(sex=str_to_title(sex)) %>%
  filter(sex %in% c("Male","Female")) %>%
  na.omit()

rm(popn_atts,popn_atts1,popn_imd,popn_cms)

##########
# join

descr<-rbind(cohort_atts2 %>% select(-c(nhs_number,instance_id)),popn_atts2) %>%
  mutate(grp=factor(grp,levels=c("Patients in cohort","General BNSSG population")))

descr_age<-descr %>%
  mutate(age=as.numeric(age)) %>%
  mutate(age=case_when(age<10 ~'0-9',
                       age>=10 & age<20 ~'-19',
                       age>=20 & age<30  ~'20s',
                       age>=30 & age<40  ~'30s',
                       age>=40 & age<50  ~'40s',
                       age>=50 & age<60  ~'50s',
                       age>=60 & age<70  ~'60s',
                       age>=70 & age<80  ~'70s',
                       age>=80 & age<90  ~'80s',
                       TRUE  ~'90+')) %>%
  group_by(grp,age) %>%
  summarise(prop=n()) %>%
  group_by(grp) %>%
  mutate(prop=prop/sum(prop)) %>%
  mutate(metric="Age") %>%
  rename("xval"="age") %>%
  mutate(xval=factor(xval,levels=c('0-9','-19','20s','30s','40s',
                                   '50s','60s','70s','80s','90+')))

descr_sex<-descr %>%
  group_by(grp,sex) %>%
  summarise(prop=n()) %>%
  group_by(grp) %>%
  mutate(prop=prop/sum(prop)) %>%
  mutate(metric="Sex") %>%
  rename("xval"="sex")

descr_imd<-descr %>%
  group_by(grp,imd) %>%
  summarise(prop=n()) %>%
  group_by(grp) %>%
  mutate(prop=prop/sum(prop)) %>%
  mutate(metric="IMD Decile") %>%
  rename("xval"="imd")

descr_segment<-descr %>%
  group_by(grp,segment) %>%
  summarise(prop=n()) %>%
  group_by(grp) %>%
  mutate(prop=prop/sum(prop)) %>%
  mutate(metric="Core Segment") %>%
  rename("xval"="segment") %>%
  mutate(xval=factor(xval,levels=c("1","2","3","4","5")))

descr_plot<-rbind(descr_age,descr_sex,descr_imd,descr_segment) %>%
  mutate(metric=factor(metric,levels=c("Age","Sex","IMD Decile","Core Segment"))) %>%
  ggplot(aes(x=xval,y=prop,fill=grp)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~metric,scales="free") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom")

print(paste0("Finished descriptive summary of cohort..."),quote=FALSE)

################################################
# OUTPUT 2: ACTIVITY SUMMARY TABLES

tbl0<-expand.grid(instance_id=cohort$instance_id,activity_name=activity_coverage$activity_name) %>%
  left_join(dat2 %>%
              group_by(instance_id,activity_name) %>%
              summarise(bf=sum(activity_time<index_event_time),af=sum(activity_time>index_event_time)),
            by=c("instance_id","activity_name")) %>%
  replace(is.na(.),0)

tbl_summ_bf<-tbl0 %>%
  group_by(activity_name) %>%
  summarise(`Mean activity per instance`=round(mean(bf),digits=2),
            `Instances with >0 activity`=paste0(round(100*sum(bf>0)/n(),digits=1),"%"),
            `Mean activity per instance with >0 activity`=round(mean(bf[bf>0]),digits=2)) %>%
  rename("Activity"="activity_name")

kable(tbl_summ_bf,caption="Summary of activity volumes BEFORE the index event",
      align=c("l","r","r","r"))



tbl_summ_af<-tbl0 %>%
  group_by(activity_name) %>%
  summarise(`Mean activity per instance`=round(mean(af),digits=2),
            `Instances with >0 activity`=paste0(round(100*sum(af>0)/n(),digits=1),"%"),
            `Mean activity per instance with >0 activity`=round(mean(af[af>0]),digits=2)) %>%
  rename("Activity"="activity_name")

kable(tbl_summ_af,caption="Summary of activity volumes AFTER the index event",
      align=c("l","r","r","r"))

print(paste0("Finished activity summary tables..."),quote=FALSE)

################################################
# OUTPUT 3: THEOGRAPHS

dat_theo<-dat2 %>%
  mutate(activity_time_diff=as.numeric(difftime(activity_time,index_event_time,units="days"))) %>%
  select(-c(index_event_time,activity_time)) %>%
  bind_rows(cohort %>%
              mutate(activity_name="Index event") %>%
              rename(activity_time_diff=index_event_time) %>%
              mutate(activity_time_diff=0)) %>%
  mutate(activity_name=factor(activity_name,levels=c("Index event",activity_coverage$activity_name)))

theo_samples<-dat_theo %>%
  group_by(instance_id) %>%
  summarise(activity_count=n()) %>%
  arrange(desc(activity_count)) %>%
  slice(1:min(30,nrow(.))) %>%
  .$instance_id

theo_plot<-dat_theo %>%
  filter(instance_id %in% theo_samples) %>%
  arrange(instance_id) %>%
  left_join(cohort_atts2 %>% select(-c(instance_id,grp)),by="nhs_number") %>%
  mutate(instance_id=paste0(age,"Y ",sex," IMD",imd," CS",segment," (#",instance_id,")")) %>%
  mutate(instance_id=factor(instance_id,levels=rev(unique(instance_id)))) %>%
  ggplot() +
  geom_hline(aes(yintercept=instance_id),colour="lightgrey",alpha=0.2) +
  geom_point(aes(x=activity_time_diff,y=factor(instance_id),shape=activity_name,colour=activity_name)) +
  scale_y_discrete() +
  scale_x_continuous(breaks=pretty_breaks(),limits=c(-pre_length/24,post_length/24)) +
  scale_shape_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
  scale_colour_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
  #scale_shape_discrete(drop=FALSE) +
  #scale_colour_discrete(drop=FALSE) +
  xlab("Time relevant to index event (in days)") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        legend.position="bottom",
        legend.title=element_blank()) #+
  #guides(colour=guide_legend(ncol=1),shape=guide_legend(ncol=1))

################
# full

theo_full_chnk<-60
theo_ids_chnk<-length(unique(dat_theo$instance_id))/theo_full_chnk
theo_res_splt<-split(sort(unique(dat_theo$instance_id)),rep(1:ceiling(theo_ids_chnk),c(rep(theo_full_chnk,floor(theo_ids_chnk)),theo_full_chnk*theo_ids_chnk %% 1)))

pdf(paste0(getwd(),"/",systime_clean,"/theographs_full.pdf"),height=8,width=13)
for (i in 1:length(theo_res_splt)) {
  print(
    x<-dat_theo %>%
      filter(instance_id %in% theo_res_splt[[i]]) %>%
      arrange(instance_id) %>%
      left_join(cohort_atts2 %>% select(-c(instance_id,grp)),by="nhs_number") %>%
      mutate(instance_id=paste0(age,"Y ",sex," IMD",imd," CS",segment," (#",instance_id,")")) %>%
      mutate(instance_id=factor(instance_id,levels=rev(unique(instance_id)))) %>%
      ggplot() +
      geom_hline(aes(yintercept=instance_id),colour="lightgrey",alpha=0.2) +
      geom_point(aes(x=activity_time_diff,y=factor(instance_id),shape=activity_name,colour=activity_name)) +
      scale_y_discrete() +
      scale_x_continuous(breaks=pretty_breaks(),limits=c(-pre_length/24,post_length/24)) +
      scale_shape_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
      scale_colour_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
      #scale_shape_discrete(drop=FALSE) +
      #scale_colour_discrete(drop=FALSE) +
      xlab("Time relevant to index event (in days)") +
      theme_bw() +
      theme(axis.title.y=element_blank(),
            legend.position="bottom",
            legend.title=element_blank())
  )
}
dev.off()

print(paste0("Finished activity profiles over time..."),quote=FALSE)

################################################
# OUTPUT 4: TALLIES

tbl_bf<-tbl0 %>%
  select(-af) %>%
  pivot_wider(names_from=activity_name,values_from=bf) %>%
  select(-instance_id) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  rename("Freq"="n") %>%
  arrange(desc(Freq)) %>%
  mutate(Percent=paste0(round(100*Freq/sum(Freq)),"%"))

tbl_af<-tbl0 %>%
  select(-bf) %>%
  pivot_wider(names_from=activity_name,values_from=af) %>%
  select(-instance_id) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  rename("Freq"="n") %>%
  arrange(desc(Freq)) %>%
  mutate(Percent=paste0(round(100*Freq/sum(Freq)),"%"))

tbl_both<-tbl0 %>%
  rename("B"="bf","A"="af") %>%
  pivot_longer(cols=c(B,A),names_to="period",values_to="count") %>%
  mutate(period=factor(period,levels=c("B","A"))) %>%
  pivot_wider(names_from=c(period,activity_name),values_from=count,names_sort=TRUE,names_sep=":") %>%
  select(-instance_id) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  rename("Freq"="n") %>%
  arrange(desc(Freq)) %>%
  mutate(Percent=paste0(round(100*Freq/sum(Freq)),"%"))

write_xlsx(list(`table_before`=tbl_bf,`table_after`=tbl_af,`table_both`=tbl_both),paste0(getwd(),"/",systime_clean,"/tallies_full.xlsx"))

print(paste0("Finished activity volume tallies..."),quote=FALSE)

################################################
# OUTPUT 5: TRACE-MAPS

trace_fn<-function(dat,period) {
  dat %>%
    select(-nhs_number) %>%
    filter(if(period=="before") activity_time_diff<=0 else activity_time_diff<Inf) %>%
    filter(if(period=="after") activity_time_diff>=0 else activity_time_diff<Inf) %>%
    mutate(activity_time=as.POSIXct("1990-01-01")+seconds(round(activity_time_diff*3600))) %>%
    mutate(lifecycle_id="complete") %>%
    group_by(instance_id) %>%
    arrange(activity_time,by_group=TRUE) %>%
    mutate(instance_id2=row_number(),n_actv=n()) %>%
    group_by(instance_id,activity_name,instance_id2,lifecycle_id) %>%
    arrange(activity_time,by_group=TRUE) %>%
    mutate(activity_instance_id=cur_group_id()) %>%
    mutate(resource_id="blank") %>%
    eventlog(case_id="instance_id",
             activity_id="activity_name",
             activity_instance_id="activity_instance_id",
             resource_id="resource_id",
             lifecycle_id="lifecycle_id",
             timestamp="activity_time")
}

# before

dat_trace_bf<-trace_fn(dat_theo,"before")

plot_trace_bf<-dat_trace_bf %>%
  trace_explorer(n_traces=10,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative"))+
                 #scale_fill=ggplot2::scale_fill_discrete) +
  ylab("Distinct activity profiles BEFORE index event") +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.title.y=element_text(face="italic"),
        axis.title.x=element_blank())

pdf(paste0(getwd(),"/",systime_clean,"/trace_before_full.pdf"),height=12,width=8)
print(
  dat_trace_bf %>%
    trace_explorer(coverage=1,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
    ylab("Distinct activity profiles BEFORE index event") +
    scale_x_continuous(breaks=pretty_breaks()) +
    scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title.x=element_blank())
)
dev.off()

############
# after

dat_trace_af<-trace_fn(dat_theo,"after")

plot_trace_af<-dat_trace_af %>%
  trace_explorer(n_traces=10,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
  ylab("Distinct activity profiles AFTER index event") +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.title.y=element_text(face="italic"),
        axis.title.x=element_blank())

pdf(paste0(getwd(),"/",systime_clean,"/trace_after_full.pdf"),height=12,width=8)
print(
  dat_trace_af %>%
    trace_explorer(coverage=1,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
    ylab("Distinct activity profiles AFTER index event") +
    scale_x_continuous(breaks=pretty_breaks()) +
    scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
        axis.title.x=element_blank())
)
dev.off()

############
# both

dat_trace_both<-trace_fn(dat_theo,"both")

plot_trace_both<-dat_trace_both %>%
  trace_explorer(n_traces=10,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
  ylab("Distinct activity profiles AROUND index event") +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.title.y=element_text(face="italic"),
        axis.title.x=element_blank())

pdf(paste0(getwd(),"/",systime_clean,"/trace_both_full.pdf"),height=12,width=8)
print(
  dat_trace_both %>%
    trace_explorer(coverage=1,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
    ylab("Distinct activity profiles AROUND index event") +
    scale_x_continuous(breaks=pretty_breaks()) +
    scale_fill_manual(values=1:(length(activity_coverage$activity_name)+1),drop=FALSE) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title.x=element_blank())
)
dev.off()

print(paste0("Finished trace-plots..."),quote=FALSE)

################################################
################################################

# produce markdown word doc
pres_author<-report_author
pres_date<-format(Sys.time(),"%d %B %Y")
pres_title<-report_title
rmd_input=paste0(getwd(),"/scripts/rmd.rmd")
rmd_output=paste0(getwd(),"/",systime_clean,"/report")
rmarkdown::render(input=rmd_input,output_file=rmd_output)

# inform user of completion
print(paste0("Finished everything in ",round(difftime(Sys.time(),systime,units="mins"))," minutes"),quote=FALSE)







