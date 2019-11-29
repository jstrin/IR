#' read_FRETF
#'
#' Function for loading the Freshman Cohorts files (FRETF)
#' flat file (.dat) from e-Campus.
#'
#' The Freshman Cohort file is used for reporting retention
#' and graduation rate
#'
#' To retireve the dat file:
#'
#' 1.	Login to eCampus.
#' 2.	Select:  URI Interfaces / Institutional Reporting / Process / Student File Download.
#'
#'
#'
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @import here
#'
#'
#' @export read_FRETF
#'



read_FRETF <- function(file, dir=NA){
  if(is.na(dir)){
    file.dir <- here::here(file)
  }else{
    file.dir <- paste(dir, file, sep="/")
  }



  c.Year <- str_sub(file, 6,7)
  read_fwf(file = file.dir, col_positions =
             fwf_widths(c(9,1,1,1,1,4,3,3,1,1,9,10,10,10,10,5,1),
                        col_names = c("ID", "sex", "race", "TD", "centenial",
                                      "SAT_verbal_math", "term", "enrolled_credits",
                                      "full_part", "omit_from_cohort", "nid",
                                      "acad_prog1", "acad_plan1", "acad_prog2",
                                      "acad_plan2", "resid", "filler")),
           col_types = cols(
             ID = "i",
             sex = "c",
             race = "c",
             TD = "c",
             centenial = "c",
             SAT_verbal_math = "n",
             term = "c",
             enrolled_credits = "n",
             full_part = "c",
             omit_from_cohort = "c",
             nid = "c",
             acad_prog1 = "c",
             acad_plan1 = "c",
             acad_prog2 = "c",
             acad_plan2 = "c",
             resid = "c",
             filler = "c"

           )) %>%
    mutate(Cohort = paste0("Fall", c.Year)) %>%
    mutate(race = case_when(race %in% c("A") ~ "Asian",
                            race %in% c("B") ~ "Black",
                            race %in% c("C") ~ "White",
                            race %in% c("H") ~ "Latinx",
                            race %in% c("I") ~ "Native American",
                            race %in% c("N") ~ "NRA",
                            race %in% c("U") ~ "Unknown",
                            race %in% c("W") ~ "White",
                            race %in% c("P") ~ "Pac. Islander",
                            race %in% c("T") ~ "Two or more"),
           TD_any = case_when( TD %in% c("A", "S") ~ "Y",
                               TRUE ~ "N"),
           TD_schoarship =case_when( TD %in% c( "S") ~ "Y",
                                     TRUE ~ "N"),
           centenial = case_when(centenial %in% c("C") ~ "Y",
                                 TRUE ~ "N")) %>%
    select(-nid)
}
