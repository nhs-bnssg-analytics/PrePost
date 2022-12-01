#' Title
#'
#' @param digits
#'
#' @return d
#' @export
check_digit <- function(digits){
  ## Weights used in algorithm
  weights <- c(10,9,8,7,6,5,4,3,2)

  ## Scale the digits by the weights and add up
  total <- sum(weights * digits)

  ## Divide by 11 to get check digit
  11 - (total %% 11)
}

#' Title
#'
#' @return d
#' @export
random_nhs_number <- function(){
  ## Generate 9 random digits to use
  digits <- sample(10,9) - 1

  check <- check_digit(digits)

  ## If the check digit is three, use four. Otherwise
  ## use three. This will always result in an invalid
  ## NHS number (do not use zero, which will be valid
  ## if the check comes out 11). The choice here is
  ## arbitrary -- there are many ways to make an invalid
  ## NHS number
  if (check == 3)
  {
    check <- 4
  }
  else
  {
    check <- 3
  }

  ## Generate and return the number
  paste0(c(digits, as.character(check)), collapse = "")
}

#' Title
#'
#' @param nhs_number d
#'
#' @return d
#' @export
nhs_number_valid <- function(nhs_number){
  digits <- as.numeric(strsplit(as.character(nhs_number), "")[[1]])

  check <- check_digit(digits[-10])

  if (check == 10)
  {
    ## If the check digit is 10, the NHS number is invalid
    FALSE
  }
  else if (check == 11)
  {
    ## THe NHS number is valid if the check digit in the
    ## number is 0
    digits[[10]] == 0
  }
  else
  {
    ## The NHS number is valid if the check digit agrees
    ## with the NHS number
    digits[[10]] == check
  }

}

#' Title
#'
#' @param filename d
#' @param seed d
#' @param n d
#'
#' @return d
#' @export
gen_example_cambridge <- function(filename, seed = 1, n = 50){
  #x@svr$MODELLING_SQL_AREA$New_Cambridge_Score
  stopifnot(n >= 1)

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random NHS number (not the real format)
  nhs_number <- replicate(n, random_nhs_number())
  segment    <- sample(1:5, size=n, replace = TRUE)
  date_y     <- sample(2018:2020, size=n, replace = TRUE)
  date_m     <- sample(1:12, size=n, replace = TRUE)
  date_str   <- paste0(date_y,"-",date_m,"-1 00:00:00")
  attribute_period <- as.POSIXct(lubridate::ymd_hms(date_str))
  attribute_period <- rep(max(attribute_period), length(attribute_period)) %>%
    as.character()

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(nhs_number = nhs_number,
                        segment    = segment,
                        attribute_period = attribute_period)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "New_Cambridge_Score")) {
    DBI::dbRemoveTable(con, "New_Cambridge_Score")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="New_Cambridge_Score",
                    value = tbl, overwrite = TRUE)

  DBI::dbDisconnect(con)
  message("Written generated Cambridge Score data to '", path, "'")
}

#' Title
#'
#' @param filename d
#' @param seed d
#' @param n d
#'
#' @return d
#' @export
gen_example_swd_activity <- function(filename, seed = 1, n = 50){
  #x@svr$MODELLING_SQL_AREA$swd_activity
  stopifnot(n >= 1)

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random NHS number (not the real format)
  nhs_number <- replicate(n, random_nhs_number())

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(nhs_number = nhs_number)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "swd_activity")) {
    DBI::dbRemoveTable(con, "swd_activity")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="swd_activity",
                    value = tbl, overwrite = TRUE)

  DBI::dbDisconnect(con)
  message("Written generated SWD activity data to '", path, "'")
}

#' Title
#'
#' @return d
#' @export
get_some_lsoa_codes <- function(){
  return(c("E01014725",
           "E01014575",
           "E01014692",
           "E01033347",
           "E01014645",
           "E01014498",
           "E01014617",
           "E01014539",
           "E01014670",
           "E01014640"))
}

#' Title
#'
#' @param filename d
#' @param seed d
#' @param n d
#'
#' @return d
#' @export
gen_example_swd_attributes_hist <- function(filename, seed = 1, n = 50){
  #x@svr$MODELLING_SQL_AREA$primary_care_attributes
  stopifnot(n >= 1)

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random NHS number (not the real format)
  nhs_number <- replicate(n, random_nhs_number())
  age        <- sample(18:100, size=n, replace = TRUE)
  sex        <- sample(c("Male","Female"), size=n, replace = TRUE)
  lsoa       <- sample(get_some_lsoa_codes(), size=n, replace = TRUE)
  date_y     <- sample(2018:2020, size=n, replace = TRUE)
  date_m     <- sample(1:12, size=n, replace = TRUE)
  date_str   <- paste0(date_y,"-",date_m,"-1 00:00:00")
  attribute_period <- as.POSIXct(lubridate::ymd_hms(date_str)) %>%
    as.character()

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(nhs_number = nhs_number,
                        age = age,
                        sex = sex,
                        lsoa = lsoa,
                        attribute_period = attribute_period)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "primary_care_attributes")) {
    DBI::dbRemoveTable(con, "primary_care_attributes")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="primary_care_attributes",
                    value = tbl, overwrite = TRUE)

  DBI::dbDisconnect(con)
  message("Written generated SWD attributes history data to '", path, "'")
}

#' Title
#'
#' @param filename d
#' @param seed d
#' @param n d
#'
#' @return d
#' @export
gen_example_swd_attributes <- function(filename, seed = 1, n = 50){
  #x@svr$MODELLING_SQL_AREA$swd_attribute
  stopifnot(n >= 1)

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random NHS number (not the real format)
  nhs_number <- replicate(n, random_nhs_number())
  age        <- sample(18:100, size=n, replace = TRUE)
  sex        <- sample(c("Male","Female"), size=n, replace = TRUE)
  lsoa       <- sample(get_some_lsoa_codes(), size=n, replace = TRUE)

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(nhs_number = nhs_number,
                        age = age,
                        sex = sex,
                        lsoa = lsoa)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "swd_attribute")) {
    DBI::dbRemoveTable(con, "swd_attribute")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="swd_attribute",
                    value = tbl, overwrite = TRUE)

  DBI::dbDisconnect(con)
  message("Written generated SWD attributes data to '", path, "'")
}

#' Title
#'
#' @param filename d
#' @param seed d
#'
#' @return d
#' @export
gen_example_imd_lsoa <- function(filename, seed = 1){
  #x@svr$Analyst_SQL_Area$tbl_BNSSG_Datasets_LSOA_IMD_2019

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random  data
  imd  <- sample(1:10, size=length(get_some_lsoa_codes()), replace = TRUE)
  lsoa <- get_some_lsoa_codes()

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(`Index of Multiple Deprivation (IMD) Decile` = imd,
                        `LSOA Code` = lsoa)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "tbl_BNSSG_Datasets_LSOA_IMD_2019")) {
    DBI::dbRemoveTable(con, "tbl_BNSSG_Datasets_LSOA_IMD_2019")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="tbl_BNSSG_Datasets_LSOA_IMD_2019",
                    value = tbl, overwrite = TRUE)

  DBI::dbDisconnect(con)
  message("Written generated IMD data to '", path, "'")
}

#' Title
#'
#' @param filename d
#' @param seed d
#' @param n d
#'
#' @return d
#' @export
gen_example_cohort <- function(filename, seed = 1, n = 50){
  stopifnot(n >= 1)

  ## Create the gendata/ folder if it does not exist
  if (!dir.exists("gendata"))
  {
    dir.create("gendata")
  }

  ## Set the seed so that subsequent operations are repeatable
  set.seed(seed)

  ## Generate a random NHS number (not the real format)
  nhs_number <- replicate(n, random_nhs_number())

  ## Generate random start and end times for each episode. Episodes
  ## do not overlap in this example, and each one starts when the previous
  ## one ends
  start_date       <- lubridate::ymd("2018-1-1")
  end_date         <- lubridate::ymd("2020-1-1")
  index_event_time <- lubridate::as_datetime(
    stats::runif(n,
                 as.numeric(as.POSIXct(start_date)),
                 as.numeric(as.POSIXct(end_date)))) %>%
    as.character()

  ## Make the data frame with the episode data
  tbl <- tibble::tibble(nhs_number = nhs_number,
                        index_event_time = index_event_time)

  ## Create the database
  path <- paste0("gendata/", filename)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  ## In case the file already exists and the table exists, remove the table
  if (DBI::dbExistsTable(con, "COHORT_SYNTH")) {
    DBI::dbRemoveTable(con, "COHORT_SYNTH")
  }

  ## Make a new table from the data frame
  DBI::dbWriteTable(con, name="COHORT_SYNTH",
                    value = tbl, overwrite = TRUE)


  DBI::dbDisconnect(con)
  message("Written generated cohort data to '", path, "'")
}

#' prepost_example
#'
#' @return side effects, creates data
#' @export
#'
prepost_example <- function(){
  n=1000
  gen_example_cohort("cohort.db", n=n)
  gen_example_imd_lsoa("Analyst_SQL_Area.db")
  gen_example_swd_attributes("MODELLING_SQL_AREA.db", n=n)
  gen_example_swd_attributes_hist("MODELLING_SQL_AREA.db", n=n)
  gen_example_swd_activity("MODELLING_SQL_AREA.db", n=n)
  gen_example_cambridge("MODELLING_SQL_AREA.db", n=n)
}

#' example_server
#'
#' @return an example server
#' @export
#'
example_server <- function(){
  srv <- list(
    "cohort" = icdb::server(config = system.file("extdata", "sqlite_cohort.yml", package="PrePost")),
    "MODELLING_SQL_AREA" = icdb::server(config = system.file("extdata", "sqlite_MODELLING_SQL_AREA.yml", package="PrePost")),
    "Analyst_SQL_Area" = icdb::server(config = system.file("extdata", "sqlite_Analyst_SQL_Area.yml", package="PrePost"))
  )
  return(srv)
}
