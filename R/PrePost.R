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
    svr = "server"
  ),

  prototype = list(
    nhs_number = character(),
    index_event_time = as.POSIXct(NULL),
    window_pre = 48,
    window_post = 48,
    window_units = "days",
    activity_coverage = list(),
    svr = icdb::server("XSW")
  )
)

setMethod("initialize", "PrePost", function(.Object, ...) {
  .Object <- callNextMethod()
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


setGeneric("run_descriptives", function(x) standardGeneric("run_descriptives"))
#' Title
#'
#' @param PrePost
#' @importFrom magrittr %>%
#' @return a figure
#' @export
#'
setMethod("run_descriptives", "PrePost", function(x) {

  print(paste0("Running descriptive summary of cohort..."),quote=FALSE)
  # figure for: age, sex, imd, cambridge score

  # DATA CLEANING
  cohort<- data.frame("nhs_number"       = x@nhs_number,
                      "index_event_time" = x@index_event_time) %>%
    dplyr::mutate(instance_id=1:nrow(.))


  cohort_atts <- x@svr$MODELLING_SQL_AREA$primary_care_attributes %>%
    dplyr::select(nhs_number,age,sex,lsoa,attribute_period) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  cohort_imd <- x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019 %>%
    dplyr::select(imd = `Index of Multiple Deprivation (IMD) Decile`,
                  lsoa = `LSOA Code`) %>%
    dplyr::filter(.data$lsoa %in% !!cohort_atts$lsoa) %>%
    icdb::run()

  cohort_atts1<-cohort_atts %>%
    dplyr::left_join(cohort_imd,by="lsoa") %>%
    dplyr::select(-lsoa)

  cohort_atts1<-cohort_atts %>%
    dplyr::left_join(cohort_imd,by="lsoa") %>%
    dplyr::select(-lsoa)

  cohort_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(nhs_number,attribute_period,segment) %>%
    dplyr::filter(.data$nhs_number %in% !!x@nhs_number) %>%
    icdb::run()

  cohort_atts2<- cohort %>%
    dplyr::left_join(cohort_atts1,by="nhs_number") %>%
    dplyr::mutate(dplyr::across(c("age","sex","imd"),as.factor)) %>%
    tidyr::pivot_longer(cols=c(age,sex,imd),names_to="metric",values_to="value") %>%
    dplyr::mutate(diff=abs(difftime(index_event_time,attribute_period,units="days"))) %>%
    dplyr::group_by(.data$instance_id,.data$metric) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_wider(names_from=metric,values_from=value) %>%
    dplyr::select(-c(attribute_period,diff)) %>%
    dplyr::left_join(cohort_cms,by="nhs_number") %>%
    dplyr::mutate(diff=abs(difftime(.data$index_event_time,.data$attribute_period,units="days"))) %>%
    dplyr::group_by(.data$instance_id) %>%
    dplyr::filter(!is.na(segment)) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    dplyr::select(-c(attribute_period,diff)) %>%
    na.omit() %>%
    dplyr::filter(sex %in% c("Male","Female")) %>%
    dplyr::ungroup() %>%
    dplyr::select(nhs_number,instance_id,age,imd,sex,segment) %>%
    dplyr::mutate(grp="Patients in cohort")

  rm(cohort_atts,cohort_atts1,cohort_imd,cohort_cms)


  ##########
  # for general popn

  popn_atts <- x@svr$MODELLING_SQL_AREA$swd_attribute %>%
    dplyr::select(nhs_number,age,sex,lsoa) %>%
    icdb::run()

  popn_imd <- x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019 %>%
    dplyr::select(imd = `Index of Multiple Deprivation (IMD) Decile`,
                  lsoa = `LSOA Code`) %>%
    icdb::run()

  popn_atts1<-popn_atts %>%
    dplyr::mutate(lsoa=toupper(lsoa)) %>%
    dplyr::left_join(popn_imd,by="lsoa") %>%
    dplyr::select(-lsoa)

  popn_cms <- x@svr$MODELLING_SQL_AREA$New_Cambridge_Score %>%
    dplyr::select(nhs_number,attribute_period,segment) %>%
    dplyr::filter(attribute_period == max(attribute_period, na.rm=TRUE)) %>%
    dplyr::select(-attribute_period) %>%
    icdb::run()

  popn_atts2<-popn_atts1 %>%
    dplyr::left_join(popn_cms,by="nhs_number") %>%
    dplyr::select(-nhs_number) %>%
    dplyr::mutate(grp="General BNSSG population") %>%
    dplyr::mutate(across(c("age","sex","imd"),as.factor)) %>%
    dplyr::mutate(sex=stringr::str_to_title(sex)) %>%
    dplyr::filter(sex %in% c("Male","Female")) %>%
    na.omit()

  rm(popn_atts,popn_atts1,popn_imd,popn_cms)

  ##########
  # join

  descr<-rbind(cohort_atts2 %>% dplyr::select(-c(nhs_number,instance_id)), popn_atts2) %>%
    dplyr::mutate(grp=factor(grp,levels=c("Patients in cohort","General BNSSG population")))

  descr_age<-descr %>%
    dplyr::mutate(age=as.numeric(age)) %>%
    dplyr::mutate(age=dplyr::case_when(age<10 ~'0-9',
                         age>=10 & age<20 ~'-19',
                         age>=20 & age<30  ~'20s',
                         age>=30 & age<40  ~'30s',
                         age>=40 & age<50  ~'40s',
                         age>=50 & age<60  ~'50s',
                         age>=60 & age<70  ~'60s',
                         age>=70 & age<80  ~'70s',
                         age>=80 & age<90  ~'80s',
                         TRUE  ~'90+')) %>%
    dplyr::group_by(grp,age) %>%
    dplyr::summarise(prop=n()) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(prop=prop/sum(prop)) %>%
    dplyr::mutate(metric="Age") %>%
    dplyr::rename("xval"="age") %>%
    dplyr::mutate(xval=factor(xval,levels=c('0-9','-19','20s','30s','40s',
                                     '50s','60s','70s','80s','90+')))

  descr_sex<-descr %>%
    dplyr::group_by(grp,sex) %>%
    dplyr::summarise(prop=n()) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(prop=prop/sum(prop)) %>%
    dplyr::mutate(metric="Sex") %>%
    dplyr::rename("xval"="sex")

  descr_imd<-descr %>%
    dplyr::group_by(grp,imd) %>%
    dplyr::summarise(prop=n()) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(prop=prop/sum(prop)) %>%
    dplyr::mutate(metric="IMD Decile") %>%
    dplyr::rename("xval"="imd")

  descr_segment<-descr %>%
    dplyr::group_by(grp,segment) %>%
    dplyr::summarise(prop=n()) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(prop=prop/sum(prop)) %>%
    dplyr::mutate(metric="Core Segment") %>%
    dplyr::rename("xval"="segment") %>%
    dplyr::mutate(xval=factor(xval,levels=c("1","2","3","4","5")))

  descr_plot<-rbind(descr_age,descr_sex,descr_imd,descr_segment) %>%
    dplyr::mutate(metric=factor(metric,levels=c("Age","Sex","IMD Decile","Core Segment"))) %>%
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




# min_datetime = min(x@index_event_time) - lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")
# max_datetime = max(x@index_event_time) + lubridate::time_length(lubridate::duration(x@window_pre, x@window_units) ,"seconds")
#
# act <- x@svr$MODELLING_SQL_AREA$swd_activity %>%
#   dplyr::select(nhs_number,arr_date) %>%
#   dplyr::filter(.data$arr_date >= min_datetime,
#                 .data$arr_date <= max_datetime) %>%
#   icdb::run()
#
# # strip out the activities that duplicates the index 'event' or not within the various windows or permitted activity types
# dat<-dplyr::left_join(cohort,act,by="nhs_number") %>%
#   dplyr::filter(arr_date!=index_event_time) %>%
#   dplyr::filter(arr_date>=index_event_time-hours(pre_length) & arr_date<=index_event_time+hours(post_length))
# dat2<-do.call("rbind",lapply(1:nrow(x@activity_coverage),function(i) {
#   dat %>%
#     mutate(activity_name=ifelse(!!rlang::parse_expr(x@activity_coverage$activity_filter[i]),x@activity_coverage$activity_name[i],"NA")) %>%
#     filter(!activity_name=="NA") %>%
#     select(nhs_number,index_event_time,instance_id,arr_date,activity_name)
# })) %>%
#   arrange(instance_id) %>%
#   rename(activity_time=arr_date)
#
# write_xlsx(list(`activity_log`=dat2),paste0(getwd(),"/",systime_clean,"/activity_log.xlsx"))
#
# rm(act,dat)
#
# print(paste0("Finished activity log creation..."),quote=FALSE)
#
#


