#' PrePost
#' @slot nhs_number character.
#' @slot index_event_time POSIXct.
#' @slot window_pre d
#' @slot window_post d
#' @slot window_units d
#' @slot activity_filter d
#' @slot svr_name d
#' @slot svr d
#' @return PrePost object
#' @export
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
    activity_filter = "list",
    svr_name = "character",
    svr = "list"
  ),

  prototype = list(
    nhs_number = character(),
    index_event_time = as.POSIXct(NULL),
    window_pre = 48,
    window_post = 48,
    window_units = "days",
    activity_filter = list(),
    svr_name = "XSW",
    svr = list()
  )
)

setMethod("initialize", "PrePost", function(.Object, ...) {
  .Object <- callNextMethod()
  if(length(.Object@svr)==0)
  {
    .Object@svr <- icdb::server(.Object@svr_name)
  }
  validObject(.Object)
  .Object
})


#' get_window
#' @param x object
#' @return named list with the window
#' @export
setGeneric("get_window", function(x) standardGeneric("get_window"))
#' get_window
#' @param x object
#' @return named list with the window
#' @export
setMethod("get_window", "PrePost", function(x) list("window_pre"=x@window_pre, "window_post"=x@window_pre, "window_units"=x@window_units))


#' set_window
#' @param x d
#' @param pre d
#' @param post d
#' @param units d
#' @return the updated object
#' @export
setGeneric("set_window", function(x, pre, post, units) standardGeneric("set_window"))
#' set_window
#' @param x d
#' @param pre d
#' @param post d
#' @param units d
#' @return the updated object
#' @export
setMethod("set_window", "PrePost", function(x, pre, post, units="days") {
  x@window_pre   <- pre
  x@window_post  <- post
  x@window_units <- units
  x
})


#' analyse
#' @param x a PrePost object
#' @return a full report
#' @export
setGeneric("analyse", function(x) standardGeneric("analyse"))
#' analyse
#' @param x a PrePost object
#' @return a full report
#' @export
setMethod("analyse", "PrePost", function(x) {

  run_descriptives(x)
  run_activity_summary(x, "before")
  run_activity_summary(x, "after")
  generate_theographs(x)
  run_activity_volume(x, "before")
  run_activity_volume(x, "after")
  generate_trace_plots(x, "before")
  generate_trace_plots(x, "before")
  generate_trace_plots(x, "around")

})


gen_cohort <- function(x){
  cohort<- data.frame("nhs_number"       = x@nhs_number,
                      "index_event_time" = x@index_event_time) %>%
    dplyr::mutate(instance_id=1:nrow(.))

  return(cohort)
}

gen_dat2 <- function(x){

  min_datetime = min(x@index_event_time) - lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")
  max_datetime = max(x@index_event_time) + lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")

  act <- x@svr$MODELLING_SQL_AREA$swd_activity %>%
    dplyr::filter(.data$arr_date >= min_datetime,
                  .data$arr_date <= max_datetime) %>%
    icdb::run() %>%
    dplyr::mutate(arr_date = lubridate::as_datetime(.data$arr_date),
                  dep_date = lubridate::as_datetime(.data$dep_date))

  cohort <- gen_cohort(x)

  # strip out the activities that duplicates the index 'event' or not within the various windows or permitted activity types
  dat<-dplyr::left_join(cohort, act,by="nhs_number") %>%
    dplyr::filter(.data$arr_date!=.data$index_event_time) %>%
    dplyr::filter(.data$arr_date>=.data$index_event_time-lubridate::duration(x@window_pre,  units = x@window_units) &
                  .data$arr_date<=.data$index_event_time+lubridate::duration(x@window_post, units = x@window_units))

  dat2 <- purrr::map2_df(.x = x@activity_filter,
                         .y = names(x@activity_filter),
                         .f = function(dat, filter_str, filter_name){
    dat %>%
      dplyr::mutate(activity_name=ifelse(!!rlang::parse_expr(filter_str), filter_name,NA_character_)) %>%
      dplyr::filter(!is.na(.data$activity_name)) %>%
      dplyr::select(.data$nhs_number, .data$index_event_time, .data$instance_id, .data$arr_date, .data$activity_name)

  }, dat=dat) %>%
    dplyr::arrange(.data$instance_id) %>%
    dplyr::rename(activity_time=.data$arr_date)

  return(dat2)
}

cohort_atts <- function(x){

  # The data
  cohort <- gen_cohort(x)

  atts <- x@svr$MODELLING_SQL_AREA$primary_care_attributes %>%
    dplyr::select(.data$nhs_number,.data$age,.data$sex,.data$lsoa,.data$attribute_period) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  imd <- x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019 %>%
    dplyr::select(imd = .data$`Index of Multiple Deprivation (IMD) Decile`,
                  lsoa = .data$`LSOA Code`) %>%
    dplyr::filter(.data$lsoa %in% !!atts$lsoa) %>%
    icdb::run()

  cohort_atts1<-atts %>%
    dplyr::mutate(lsoa=toupper(.data$lsoa)) %>%
    dplyr::left_join(imd, by="lsoa") %>%
    dplyr::select(-.data$lsoa)

  cohort_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(.data$nhs_number, .data$attribute_period, .data$segment) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  cohort_atts2<- cohort %>%
    dplyr::left_join(cohort_atts1, by="nhs_number") %>%
    dplyr::mutate(dplyr::across(c("age","sex","imd"),as.factor)) %>%
    tidyr::pivot_longer(cols=c(.data$age, .data$sex, .data$imd), names_to="metric", values_to="value") %>%
    dplyr::mutate(diff=abs(difftime(.data$index_event_time,.data$attribute_period,units="days"))) %>%
    dplyr::group_by(.data$instance_id,.data$metric) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_wider(names_from=.data$metric, values_from=.data$value) %>%
    dplyr::select(-c(.data$attribute_period, .data$diff)) %>%
    dplyr::left_join(cohort_cms,by="nhs_number") %>%
    dplyr::mutate(diff=abs(difftime(.data$index_event_time, .data$attribute_period, units="days"))) %>%
    dplyr::group_by(.data$instance_id) %>%
    dplyr::filter(!is.na(.data$segment)) %>%
    dplyr::arrange(.data$diff) %>%
    dplyr::slice(1) %>%
    dplyr::select(-c(.data$attribute_period, .data$diff)) %>%
    na.omit() %>%
    dplyr::filter(.data$sex %in% c("Male","Female")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$nhs_number, .data$instance_id, .data$age, .data$imd, .data$sex, .data$segment) %>%
    dplyr::mutate(grp="Patients in cohort")

  return(cohort_atts2)
}

popn_atts <- function(x){
  ##########
  # for general popn

  atts <- x@svr$MODELLING_SQL_AREA$swd_attribute %>%
    dplyr::select(.data$nhs_number,.data$age,.data$sex,.data$lsoa) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  imd <- x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019 %>%
    dplyr::select(imd  = .data$`Index of Multiple Deprivation (IMD) Decile`,
                  lsoa = .data$`LSOA Code`) %>%
    dplyr::filter(.data$lsoa %in% !!atts$lsoa) %>%
    icdb::run()

  popn_atts1<-atts %>%
    dplyr::mutate(lsoa=toupper(.data$lsoa)) %>%
    dplyr::left_join(imd, by="lsoa") %>%
    dplyr::select(-.data$lsoa)

  popn_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(.data$nhs_number, .data$attribute_period, .data$segment) %>%
    dplyr::filter(.data$attribute_period == max(.data$attribute_period, na.rm=TRUE)) %>%
    dplyr::select(-.data$attribute_period) %>%
    icdb::run()

  popn_atts2<-popn_atts1 %>%
    dplyr::left_join(popn_cms,by="nhs_number") %>%
    dplyr::select(-.data$nhs_number) %>%
    dplyr::mutate(grp="General BNSSG population") %>%
    dplyr::mutate(dplyr::across(c("age","sex","imd"),as.factor)) %>%
    dplyr::mutate(sex=stringr::str_to_title(.data$sex)) %>%
    dplyr::filter(.data$sex %in% c("Male","Female")) %>%
    na.omit()

  return(popn_atts2)
}

dat_theo <- function(x){

  cohort <- gen_cohort(x)
  dat2 <- gen_dat2(x)

  dat_theo<-dat2 %>%
    dplyr::mutate(activity_time_diff=as.numeric(difftime(.data$activity_time, .data$index_event_time, units=x@window_units))) %>%
    dplyr::select(-c(.data$index_event_time, .data$activity_time)) %>%
    dplyr::bind_rows(cohort %>%
                       dplyr::mutate(activity_name="Index event") %>%
                       dplyr::rename(activity_time_diff=.data$index_event_time) %>%
                       dplyr::mutate(activity_time_diff=0)) %>%
    dplyr::mutate(activity_name=factor(.data$activity_name,levels=c("Index event",names(x@activity_filter))))

  return(dat_theo)
}

gen_tbl0 <- function(x){

  dat2 <- gen_dat2(x)

  tbl0<-expand.grid(instance_id=1:length(x@nhs_number), activity_name=names(x@activity_filter)) %>%
    dplyr::left_join(dat2 %>%
                       dplyr::group_by(.data$instance_id, .data$activity_name) %>%
                       dplyr::summarise(bf=sum(.data$activity_time<.data$index_event_time),
                                        af=sum(.data$activity_time>.data$index_event_time)),
              by=c("instance_id","activity_name")) %>%
    replace(is.na(.),0)

  return(tbl0)
}

trace_fn <- function(dat_theo,period,units) {

  dat <- dat_theo %>%
    dplyr::select(-.data$nhs_number) %>%
    dplyr::filter(if(period=="before") .data$activity_time_diff<=0 else .data$activity_time_diff<Inf) %>%
    dplyr::filter(if(period=="after") .data$activity_time_diff>=0 else .data$activity_time_diff<Inf) %>%
    dplyr::mutate(activity_time = as.POSIXct("1990-01-01") + round(lubridate::duration(.data$activity_time_diff, units=units))) %>%
    dplyr::mutate(lifecycle_id="complete") %>%
    dplyr::group_by(.data$instance_id) %>%
    dplyr::arrange(.data$activity_time, by_group=TRUE) %>%
    dplyr::mutate(instance_id2=dplyr::row_number(),
                  n_actv=dplyr::n()) %>%
    dplyr::group_by(.data$instance_id, .data$activity_name, .data$instance_id2, .data$lifecycle_id) %>%
    dplyr::arrange(.data$activity_time, by_group=TRUE) %>%
    dplyr::mutate(activity_instance_id=dplyr::cur_group_id()) %>%
    dplyr::mutate(resource_id="blank") %>%
    bupaR::eventlog(case_id="instance_id",
             activity_id="activity_name",
             activity_instance_id="activity_instance_id",
             resource_id="resource_id",
             lifecycle_id="lifecycle_id",
             timestamp="activity_time")

  return(dat)
}


#' run_descriptives
#' @param x PrePost object
#' @return a figure
#' @export
setGeneric("run_descriptives", function(x) standardGeneric("run_descriptives"))
#' run_descriptives
#' @param x PrePost object
#' @importFrom magrittr %>%
#' @importFrom methods callNextMethod new validObject
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom icdb server
#' @return a figure
#' @export
setMethod("run_descriptives", "PrePost", function(x) {

  cohort_atts2 <- cohort_atts(x)

  popn_atts2 <- popn_atts(x)

  ##########
  # join
  descr<-rbind(cohort_atts2 %>% dplyr::select(-c(.data$nhs_number,.data$instance_id)), popn_atts2) %>%
    dplyr::mutate(grp=factor(.data$grp,levels=c("Patients in cohort","General BNSSG population")))

  descr_age<-descr %>%
    dplyr::mutate(age=as.numeric(.data$age)) %>%
    dplyr::mutate(age=dplyr::case_when(.data$age<10 ~'0-9',
                                       .data$age>=10 & .data$age<20  ~'10s',
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
    dplyr::mutate(xval=factor(.data$xval,levels=c('0-9','10s','20s','30s','40s',
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
    ggplot2::ggplot(ggplot2::aes(x=.data$xval,y=.data$prop,fill=.data$grp)) +
    ggplot2::geom_bar(stat="identity",position="dodge") +
    ggplot2::facet_wrap(~metric,scales="free") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank(),
          legend.title=ggplot2::element_blank(),
          legend.position="bottom")

  return(descr_plot)
})


#' run_activity_summary
#' @param x PrePost object
#' @param period one of c("after", "before")
#' @return a table
#' @export
setGeneric("run_activity_summary", function(x, period) standardGeneric("run_activity_summary"))
#' run_activity_summary
#' @param x PrePost object
#' @param period one of c("after", "before")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a table
#' @export
setMethod("run_activity_summary", "PrePost", function(x, period) {

  # Input checks
  stopifnot(period %in% c("after", "before"))

  tbl0<-gen_tbl0(x)

  tbl_summ<-tbl0 %>%
    dplyr::group_by(.data$activity_name) %>%
    {if(period=="before"){
      dplyr::summarise(.,
                       `Mean activity per instance`=round(mean(.data$bf),digits=2),
                       `Instances with >0 activity`=paste0(round(100*sum(.data$bf>0)/dplyr::n(), digits=1),"%"),
                       `Mean activity per instance with >0 activity`= round(mean(.data$bf[.data$bf>0]), digits=2))
    }else if(period=="after"){
      dplyr::summarise(.,
                       `Mean activity per instance`=round(mean(.data$af),digits=2),
                       `Instances with >0 activity`=paste0(round(100*sum(.data$af>0)/dplyr::n(), digits=1),"%"),
                       `Mean activity per instance with >0 activity`= round(mean(.data$af[.data$af>0]), digits=2))
    }else{
      stop("error with period parameter in run_activity_summary()")
    }} %>%
    dplyr::rename("Activity"="activity_name")

  t <- knitr::kable(tbl_summ,caption=paste0("Summary of activity volumes ", period, " the index event"),
        align=c("l","r","r","r"))

  return(t)
})


#' generate_theographs
#' @param obj PrePost object
#' @return a figure
#' @export
setGeneric("generate_theographs", function(obj) standardGeneric("generate_theographs"))
#' generate_theographs
#' @param obj PrePost object
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a figure
#' @export
setMethod("generate_theographs", "PrePost", function(obj) {

  ################################################
  # OUTPUT 3: THEOGRAPHS
  dat_theo <- dat_theo(obj)

  theo_samples<-dat_theo %>%
    dplyr::group_by(.data$instance_id) %>%
    dplyr::summarise(activity_count=dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$activity_count)) %>%
    dplyr::slice(1:min(30,nrow(.))) %>%
    dplyr::select(.data$instance_id) %>%
    dplyr::pull()

  cohort_atts2 <- cohort_atts(obj)

  ################
  # full

  theo_full_chnk<-60
  theo_ids_chnk<-length(unique(dat_theo$instance_id))/theo_full_chnk
  theo_res_splt<-split(sort(unique(dat_theo$instance_id)),rep(1:ceiling(theo_ids_chnk),c(rep(theo_full_chnk,floor(theo_ids_chnk)),theo_full_chnk*theo_ids_chnk %% 1)))
  systime_clean<-gsub("[:]","-",Sys.time())
  dir <- paste0(getwd(),"/PrePost-Results")
  if(!dir.exists(dir)) dir.create(dir)

  grDevices::pdf(paste0(dir, "/", systime_clean, "_theographs_full.pdf"),height=8,width=13)
  for (i in 1:length(theo_res_splt)) {
    print(
      x<-dat_theo %>%
        dplyr::filter(.data$instance_id %in% theo_res_splt[[i]]) %>%
        dplyr::arrange(.data$instance_id) %>%
        dplyr::left_join(cohort_atts2 %>% dplyr::select(-c(.data$instance_id,.data$grp)),by="nhs_number") %>%
        dplyr::mutate(instance_id=paste0(.data$age,"Y ",.data$sex," IMD",.data$imd," CS",.data$segment," (#",.data$instance_id,")")) %>%
        dplyr::mutate(instance_id=factor(.data$instance_id,levels=rev(unique(.data$instance_id)))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_hline(ggplot2::aes(yintercept=.data$instance_id),colour="lightgrey",alpha=0.2) +
        ggplot2::geom_point(ggplot2::aes(x=.data$activity_time_diff,y=factor(.data$instance_id),shape=.data$activity_name,colour=.data$activity_name)) +
        ggplot2::scale_y_discrete() +
        ggplot2::scale_x_continuous(breaks=scales::pretty_breaks(),limits=c(-obj@window_pre,obj@window_post)) +
        ggplot2::scale_shape_manual(values=1:(length(obj@activity_filter)+1),drop=FALSE) +
        ggplot2::scale_colour_manual(values=1:(length(obj@activity_filter)+1),drop=FALSE) +
        ggplot2::xlab(paste0("Time relevant to index event (in ", obj@window_units, ")")) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.y=ggplot2::element_blank(),
              legend.position="bottom",
              legend.title=ggplot2::element_blank())
    )
  }
  grDevices::dev.off()

  theo_plot<-dat_theo %>%
    dplyr::filter(.data$instance_id %in% theo_samples) %>%
    dplyr::arrange(.data$instance_id) %>%
    dplyr::left_join(cohort_atts2 %>% dplyr::select(-c(.data$instance_id,.data$grp)), by="nhs_number") %>%
    dplyr::mutate(instance_id=paste0(.data$age,"Y ",.data$sex," IMD",.data$imd," CS",.data$segment," (#",.data$instance_id,")")) %>%
    dplyr::mutate(instance_id=factor(.data$instance_id,levels=rev(unique(.data$instance_id)))) %>%
    ggplot2::ggplot() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=.data$instance_id),colour="lightgrey",alpha=0.2) +
    ggplot2::geom_point(ggplot2::aes(x=.data$activity_time_diff,y=factor(.data$instance_id),shape=.data$activity_name,colour=.data$activity_name)) +
    ggplot2::scale_y_discrete() +
    ggplot2::scale_x_continuous(breaks=scales::pretty_breaks(),limits=c(-obj@window_pre,obj@window_post)) +
    ggplot2::scale_shape_manual(values=1:(length(obj@activity_filter)+1),drop=FALSE) +
    ggplot2::scale_colour_manual(values=1:(length(obj@activity_filter)+1),drop=FALSE) +
    ggplot2::xlab(paste0("Time relevant to index event (in ", obj@window_units, ")")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   legend.position="bottom",
                   legend.title=ggplot2::element_blank())

  return(theo_plot)
})


#' run_activity_volume
#' @param x PrePost object
#' @param period one of c("after", "before", "around")
#' @return a table
#' @export
setGeneric("run_activity_volume", function(x, period) standardGeneric("run_activity_volume"))
#' run_activity_volume
#' @param x PrePost object
#' @param period one of c("after", "before", "around")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return a table
#' @export
setMethod("run_activity_volume", "PrePost", function(x, period) {

  # Input checks
  stopifnot(period %in% c("after", "before", "around"))

  # Get data
  tbl0 <- gen_tbl0(x)

  # Generate TALLIES table
  tbl<-tbl0 %>%
    {if(period=="after"){
      dplyr::select(., -.data$bf) %>%
      tidyr::pivot_wider(names_from=.data$activity_name,values_from=.data$af)
     }else if(period=="before"){
      dplyr::select(., -.data$af) %>%
      tidyr::pivot_wider(names_from=.data$activity_name,values_from=.data$bf)
     }else if(period=="around"){
      dplyr::rename(., "B"="bf","A"="af") %>%
      tidyr::pivot_longer(cols=c(.data$B,.data$A),names_to="period",values_to="count") %>%
      dplyr::mutate(period=factor(.data$period,levels=c("B","A"))) %>%
      tidyr::pivot_wider(names_from=c(.data$period,.data$activity_name),values_from=.data$count,names_sort=TRUE,names_sep=":")
     }else{.}
    } %>%
    dplyr::select(-.data$instance_id) %>%
    dplyr::group_by_all() %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::rename("Freq"="n") %>%
    dplyr::arrange(dplyr::desc(.data$Freq)) %>%
    dplyr::mutate(Percent=paste0(round(100*.data$Freq/sum(.data$Freq)),"%"))

  t <- knitr::kable(tbl,
                    caption=paste0("Summary of activity profiles ", period, " the index event"),
                    align=c("l","r","r","r"))

  return(t)
})


#' generate_trace_plots
#' @param x PrePost object
#' @param period one of c("after", "before", "around")
#' @return a figure
#' @export
setGeneric("generate_trace_plots", function(x, period) standardGeneric("generate_trace_plots"))
#' generate_trace_plots
#' @param x PrePost object
#' @param period one of c("after", "before", "around")
#' @importFrom magrittr %>%
#' @return a figure
#' @export
setMethod("generate_trace_plots", "PrePost", function(x, period) {

  # Input checks
  stopifnot(period %in% c("after", "before", "around"))

  # Data
  dat_theo  <- dat_theo(x)
  dat_trace <- trace_fn(dat_theo, period, x@window_units)

  # Plot
  dat_trace_plot<-dat_trace %>%
    processmapR::trace_explorer(n_traces=6, show_labels=FALSE, coverage_labels=c("absolute","relative","cumulative"))+
    ggplot2::ylab(paste0("Distinct activity profiles ", period, " index event")) +
    ggplot2::scale_x_continuous(breaks=scales::pretty_breaks()) +
    ggplot2::scale_fill_manual(values=1:(length(x@activity_filter)+1),drop=FALSE) +
    ggplot2::theme(legend.position="bottom",
          legend.title=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_text(face="italic"),
          axis.title.x=ggplot2::element_blank())

  # Save as pdf
  systime_clean<-gsub("[:]","-",Sys.time())
  dir <- paste0(getwd(),"/PrePost-Results")
  if(!dir.exists(dir)) dir.create(dir)
  grDevices::pdf(paste0(dir, "/",systime_clean,"_trace_", period, "_full.pdf"),height=12,width=8)
  print(
    dat_trace %>%
      processmapR::trace_explorer(coverage=1,show_labels=FALSE,coverage_labels=c("absolute","relative","cumulative")) +
      ggplot2::ylab(paste0("Distinct activity profiles ", period, " index event"))+
      ggplot2::scale_x_continuous(breaks=scales::pretty_breaks()) +
      ggplot2::scale_fill_manual(values=1:(length(x@activity_filter)+1),drop=FALSE) +
      ggplot2::theme(legend.position="bottom",
            legend.title=ggplot2::element_blank(),
            axis.title.x=ggplot2::element_blank())
  )
  grDevices::dev.off()

  return(dat_trace_plot)
})
