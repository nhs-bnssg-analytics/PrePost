#' PrePost
#'
#' @slot nhs_number character.
#' @slot index_event_time POSIXct.
#'
#' @return PrePost object
#' @export
#'
PrePost <- setClass(

  # Class name
  Class = "PrePost",

  # Class member variables and types
  slots = c(
    nhs_number = "character",
    index_event_time = "POSIXct",
    window_pre = "numeric",
    window_post = "numeric",
    window_units = "character",
    activity_coverage = "list",
    svr_name = "character",
    svr = "server"
  ),

  prototype = list(
    nhs_number = character(),
    index_event_time = as.POSIXct(NULL),
    window_pre = 48,
    window_post = 48,
    window_units = "days",
    activity_coverage = list(),
    svr_name = "XSW"
  )
)

setMethod("initialize", "PrePost", function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@svr <- icdb::server(.Object@svr_name)
  validObject(.Object)
  .Object
})


setGeneric("get_window", function(x) standardGeneric("get_window"))
setMethod("get_window", "PrePost", function(x) list("window_pre"=x@window_pre, "window_post"=x@window_pre, "window_units"=x@window_units))


setGeneric("set_window", function(x, pre, post, units) standardGeneric("set_window"))
setMethod("set_window", "PrePost", function(x, pre, post, units="days") {
  x@window_pre   <- pre
  x@window_post  <- post
  x@window_units <- units
  x
})



setGeneric("analyse", function(x) standardGeneric("analyse"))
#' analyse
#'
#' @param PrePost PrePost object
#' @return a full report
#' @export
#'
setMethod("analyse", "PrePost", function(x) {

  # Run everything
  # Check wd and ask if appropriate to add files to
  # Otherwise ask to call set_prepost_wd()

})


gen_dat2 <- function(x){

  min_datetime = min(x@index_event_time) - lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")
  max_datetime = max(x@index_event_time) + lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")

  act <- x@svr$MODELLING_SQL_AREA$swd_activity %>%
    dplyr::select(nhs_number,arr_date) %>%
    dplyr::filter(.data$arr_date >= min_datetime,
                  .data$arr_date <= max_datetime) %>%
    icdb::run()

  # strip out the activities that duplicates the index 'event' or not within the various windows or permitted activity types
  dat<-dplyr::left_join(cohort,act,by="nhs_number") %>%
    dplyr::filter(arr_date!=index_event_time) %>%
    dplyr::filter(arr_date>=index_event_time-hours(pre_length) & arr_date<=index_event_time+hours(post_length))

  dat2<-do.call("rbind",lapply(1:nrow(x@activity_coverage),function(i) {
    dat %>%
      mutate(activity_name=ifelse(!!rlang::parse_expr(x@activity_coverage$activity_filter[i]),x@activity_coverage$activity_name[i],"NA")) %>%
      filter(!activity_name=="NA") %>%
      select(nhs_number,index_event_time,instance_id,arr_date,activity_name)
  })) %>%
    arrange(instance_id) %>%
    rename(activity_time=arr_date)

  return(dat2)
}

dat_theo <- function(x){

  dat2 <- gen_dat2(x)

  dat_theo<-dat2 %>%
    mutate(activity_time_diff=as.numeric(difftime(activity_time,index_event_time,units="days"))) %>%
    select(-c(index_event_time,activity_time)) %>%
    bind_rows(cohort %>%
                mutate(activity_name="Index event") %>%
                rename(activity_time_diff=index_event_time) %>%
                mutate(activity_time_diff=0)) %>%
    mutate(activity_name=factor(activity_name,levels=c("Index event",activity_coverage$activity_name)))

  return(dat_theo)
}

gen_tbl0 <- function(x){

  dat2 <- gen_dat2(x)

  tbl0<-expand.grid(instance_id=1:length(x@nhs_number), activity_name=x@activity_coverage$activity_name) %>%
    left_join(dat2 %>%
                group_by(instance_id,activity_name) %>%
                summarise(bf=sum(activity_time<index_event_time),af=sum(activity_time>index_event_time)),
              by=c("instance_id","activity_name")) %>%
    replace(is.na(.),0)

  return(tbl0)
}

gen_attr <- function(x, filter_ids=FALSE){

  atts <- x@svr$MODELLING_SQL_AREA$primary_care_attributes %>%
    dplyr::select(.data$nhs_number,.data$age,.data$sex,.data$lsoa,.data$attribute_period) %>%
    {if(all(ids==TRUE)) dplyr::filter(., .data$nhs_number %in% !!x@nhs_number) else .} %>%
    icdb::run()

  imd <- x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019 %>%
    dplyr::select(imd = .data$`Index of Multiple Deprivation (IMD) Decile`,
                  lsoa = .data$`LSOA Code`) %>%
    {if(all(ids==TRUE)) dplyr::filter(., .data$lsoa %in% !!atts$lsoa) else .} %>%
    icdb::run()

  atts1<-atts %>%
    dplyr::mutate(lsoa=toupper(.data$lsoa)) %>%
    dplyr::left_join(imd,by="lsoa") %>%
    dplyr::select(-lsoa)

  return(atts1)
}

trace_fn<-function(x,period) {

  dat2 <- clean_dat(x)

  dat2 <- dat2 %>%
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

  return(dat2)
}


setGeneric("run_descriptives", function(x) standardGeneric("run_descriptives"))
#' Title
#'
#' @param PrePost PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
#'
setMethod("run_descriptives", "PrePost", function(x) {

  print(paste0("Running descriptive summary of cohort..."),quote=FALSE)
  # figure for: age, sex, imd, cambridge score

  cohort<- data.frame("nhs_number"       = x@nhs_number,
                      "index_event_time" = x@index_event_time) %>%
    dplyr::mutate(instance_id=1:nrow(.))

  # The data
  cohort_atts1<-gen_attr(x, filter_ids=TRUE)

  cohort_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(nhs_number,attribute_period,segment) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  cohort_atts2<- cohort %>%
    dplyr::left_join(cohort_atts1,by="nhs_number") %>%
    dplyr::mutate(dplyr::across(c("age","sex","imd"),as.factor)) %>%
    tidyr::pivot_longer(cols=c(.data$age,.data$sex,.data$imd),names_to="metric",values_to="value") %>%
    dplyr::mutate(diff=abs(difftime(.data$index_event_time,.data$attribute_period,units="days"))) %>%
    dplyr::group_by(.data$instance_id,.data$metric) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_wider(names_from=.data$metric,values_from=.data$value) %>%
    dplyr::select(-c(attribute_period,diff)) %>%
    dplyr::left_join(cohort_cms,by="nhs_number") %>%
    dplyr::mutate(diff=abs(difftime(.data$index_event_time,.data$attribute_period,units="days"))) %>%
    dplyr::group_by(.data$instance_id) %>%
    dplyr::filter(!is.na(.data$segment)) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    dplyr::select(-c(attribute_period,diff)) %>%
    na.omit() %>%
    dplyr::filter(.data$sex %in% c("Male","Female")) %>%
    dplyr::ungroup() %>%
    dplyr::select(nhs_number,instance_id,age,imd,sex,segment) %>%
    dplyr::mutate(grp="Patients in cohort")

  rm(cohort_atts,cohort_atts1,cohort_imd,cohort_cms)

  ##########
  # for general popn
  popn_atts1<-gen_attr(x, filter_ids=FALSE)

  popn_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(nhs_number,attribute_period,segment) %>%
    dplyr::filter(attribute_period == max(.data$attribute_period, na.rm=TRUE)) %>%
    dplyr::select(-attribute_period) %>%
    icdb::run()

  popn_atts2<-popn_atts1 %>%
    dplyr::left_join(popn_cms,by="nhs_number") %>%
    dplyr::select(-nhs_number) %>%
    dplyr::mutate(grp="General BNSSG population") %>%
    dplyr::mutate(dplyr::across(c("age","sex","imd"),as.factor)) %>%
    dplyr::mutate(sex=stringr::str_to_title(sex)) %>%
    dplyr::filter(sex %in% c("Male","Female")) %>%
    na.omit()

  rm(popn_atts,popn_atts1,popn_imd,popn_cms)

  ##########
  # join

  descr<-rbind(cohort_atts2 %>% dplyr::select(-c(nhs_number,instance_id)), popn_atts2) %>%
    dplyr::mutate(grp=factor(.data$grp,levels=c("Patients in cohort","General BNSSG population")))

  descr_age<-descr %>%
    dplyr::mutate(age=as.numeric(.data$age)) %>%
    dplyr::mutate(age=dplyr::case_when(.data$age<10 ~'0-9',
                                       .data$age>=10 & .data$age<20 ~'-19',
                                       .data$age>=20 & .data$age<30  ~'20s',
                                       .data$age>=30 & .data$age<40  ~'30s',
                                       .data$age>=40 & .data$age<50  ~'40s',
                                       .data$age>=50 & .data$age<60  ~'50s',
                                       .data$age>=60 & .data$age<70  ~'60s',
                                       .data$age>=70 & .data$age<80  ~'70s',
                                       .data$age>=80 & .data$age<90  ~'80s',
                         TRUE  ~'90+')) %>%
    dplyr::group_by(.data$grp,.data$age) %>%
    dplyr::summarise(prop=dplyr::n()) %>%
    dplyr::group_by(.data$grp) %>%
    dplyr::mutate(prop=.data$prop/sum(.data$prop)) %>%
    dplyr::mutate(metric="Age") %>%
    dplyr::rename("xval"="age") %>%
    dplyr::mutate(xval=factor(.data$xval,levels=c('0-9','-19','20s','30s','40s',
                                     '50s','60s','70s','80s','90+')))

  descr_sex<-descr %>%
    dplyr::group_by(.data$grp,.data$sex) %>%
    dplyr::summarise(prop=dplyr::n()) %>%
    dplyr::group_by(.data$grp) %>%
    dplyr::mutate(prop=.data$prop/sum(.data$prop)) %>%
    dplyr::mutate(metric="Sex") %>%
    dplyr::rename("xval"="sex")

  descr_imd<-descr %>%
    dplyr::group_by(.data$grp,.data$imd) %>%
    dplyr::summarise(prop=dplyr::n()) %>%
    dplyr::group_by(.data$grp) %>%
    dplyr::mutate(prop=.data$prop/sum(.data$prop)) %>%
    dplyr::mutate(metric="IMD Decile") %>%
    dplyr::rename("xval"="imd")

  descr_segment<-descr %>%
    dplyr::group_by(.data$grp,.data$segment) %>%
    dplyr::summarise(prop=dplyr::n()) %>%
    dplyr::group_by(.data$grp) %>%
    dplyr::mutate(prop=.data$prop/sum(.data$prop)) %>%
    dplyr::mutate(metric="Core Segment") %>%
    dplyr::rename("xval"="segment") %>%
    dplyr::mutate(xval=factor(.data$xval,levels=c("1","2","3","4","5")))

  descr_plot<-rbind(descr_age,descr_sex,descr_imd,descr_segment) %>%
    dplyr::mutate(metric=factor(.data$metric,levels=c("Age","Sex","IMD Decile","Core Segment"))) %>%
    ggplot2::ggplot(ggplot2::aes(x=xval,y=prop,fill=grp)) +
    ggplot2::geom_bar(stat="identity",position="dodge") +
    ggplot2::facet_wrap(~metric,scales="free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank(),
          legend.title=ggplot2::element_blank(),
          legend.position="bottom")

  print(paste0("Finished descriptive summary of cohort..."),quote=FALSE)
  return(descr_plot)

})


setGeneric("run_activity_summary", function(x) standardGeneric("run_activity_summary"))
#' run_activity_summary
#'
#' @param PrePost PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
#'
setMethod("run_activity_summary", "PrePost", function(x) {

  tbl0<-gen_tbl0(x)

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

  return(0)
})



setGeneric("generate_theographs", function(x) standardGeneric("generate_theographs"))
#' generate_theographs
#'
#' @param PrePost PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
#'
setMethod("generate_theographs", "PrePost", function(x) {

  ################################################
  # OUTPUT 3: THEOGRAPHS
  dat_theo <- dat_theo(x)

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
  return(0)
})





setGeneric("run_activity_volume", function(x) standardGeneric("run_activity_volume"))
#' run_activity_volume
#'
#' @param PrePost PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
#'
setMethod("run_activity_volume", "PrePost", function(x) {

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

  print(paste0("Finished activity volume tallies..."),quote=FALSE)
  return(0)
})





setGeneric("generate_trace_plots", function(x) standardGeneric("generate_trace_plots"))
#' generate_trace_plots
#'
#' @param PrePost PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
#'
setMethod("generate_trace_plots", "PrePost", function(x) {

  dat_theo <- dat_theo(x)

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
  return(0)
})
