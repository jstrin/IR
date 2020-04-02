#' read_PSW01
#'
#' Function for loading the Student Enrollment History
#' flat file (.dat) from e-Campus.
#'
#' The Student Enrollment History file is used for Final Enrollment
#' Reports and other reports involving headcount, age distribution,
#' credits, etc.
#'
#' To retireve the dat file:
#'
#' 1.	Login to eCampus.
#' 2.	Select:  URI Interfaces / Institutional Reporting / Process / Student File Download.
#' 3.	Run the process after entering a TERM (cyym) at the prompt.
#' 4.	At completion, run FTP to transfer the dataset to the PC (translate to ASCII format).
#' 5.	Rename as  PSW01-tyy-mmmdd.DAT  where t is term (F, S, or SUM), yy are the term year
#' digits, mmm is the month of the extract, and dd is the day of the extract.  Example:
#' PSW01-F03-OCT15.DAT.
#' 6.	Clean up any erroneous/missing data and save a new copy.
#'
#'
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @import here
#'
#'
#' @export read_PSW01
#'



read_PSW01 <- function(file, dir=NA){
  if(is.na(dir)){
    file.dir <- here::here(file)
  }else{
    file.dir <- paste(dir, file, sep="/")
  }


  if(str_sub(file, 7,9) %in% c("SUM")){
    c.Sem <- "Summer"
  }else{
    if(str_sub(file, 7,7) %in% c("F")){
      c.Sem <- "Fall"
    }else{
      if(str_sub(file, 7,7) %in% c("S")){
        c.Sem <- "Spring"
      }else{
      stop("Unknown semester. Check that file is named correctly:  PSW01-tyy-mmmdd.DAT
           where t is term (F, S, or SUM), yy are the term year digits, mmm
           is the month of the extract, and dd is the day of the extract")
    }}
  }
  c.Year <- str_sub(file, 8,9)
  read_fwf(file = file.dir, col_positions =
             fwf_widths(c(11,20,30,30,30,
                          8,1,1,30,1,
                          30,55,55,55,55,
                          30,6,12,24,4,
                          4, 4,5,4,5,
                          8,1,4,4,30,
                          4,3,6,9,8,
                          30,4,3,6,9,
                          8,30,4,3,6,
                          9,8,4,3, 30,
                          4,1,1,8,8,
                          8,8,8,8,8,
                          8,8,8,8,8,
                          8,8,8,8,11,
                          4,5,10,5,10,
                          3,5,30,11,11,
                          10),
                        col_names = c("ID", "NID", "last", "first",
                                      "mid", "birthdate", "sex",
                                      "race", "race_desc", "citizen",
                                      "country", "home_addr1",
                                      "home_addr2", "home_addr3",
                                      "home_addr4", "home_city",
                                      "home_state", "home_postal",
                                      "home_phone", "cent", "td",
                                      "sport_one", "athlcd_one",
                                      "sport_two", "athlcd_two",
                                      "dt_of_death", "FERPA",
                                      "acad_standing_actn",
                                      "acad_standing_term",
                                      "ed_backgrd1", "ext_career1",
                                      "eb_school_type1",
                                      "eb_atp_cd1", "ext_gpa1",
                                      "ext_deg_dt1", "ed_bckgrd2",
                                      "ext_career2",
                                      "eb_school_type2",
                                      "eb_atp_cd2", "ext_gpa2",
                                      "ext_deg_dt2",
                                      "ed_bckgrd3", "ext_career3",
                                      "eb_school_type3",
                                      "eb_atp_cd3", "ext_gpa3",
                                      "ext_deg_dt3",
                                      "admit_term", "admit_type",
                                      "admit_type_descr",
                                      "entry_cd", "off_campus_study",
                                      "continuous_reg", "class_size",
                                      "class_rank", "percentile",
                                      "decile_rank", "quintile_rank",
                                      "sat_verb_score",
                                      "sat_math_score", "GRE_anly_score",
                                      "gre_quant_score", "gre_verb_score",
                                      "gre_quant_pct", "gre_verb_pct",
                                      "gmat_anly_score", "gmat_verb_score",
                                      "gmat_quant_score", "gmat_totl_score",
                                      "unt_taken_prgrss", "acad_career",
                                      "acad_prog1", "acad_plan1",
                                      "acad_prog2", "acad_plan2",
                                      "acad_level_eot", "residency",
                                      "u_descr_residency", "cur_gpa",
                                      "cum_gpa", "study_agreement")),
           col_types = cols(
             birthdate = col_date(format="%Y%m%d"),
             dt_of_death = col_date(format = "%Y%m%d"),
             home_addr4 = col_character(),
             race = col_character(),
             citizen = col_character(),
             ext_deg_dt1 = col_date(format="%Y%m%d"),
             ext_deg_dt2 = col_date(format="%Y%m%d"),
             ext_deg_dt3 = col_date(format="%Y%m%d"),
             athlcd_one = col_character(),
             athlcd_two = col_character(),
             sport_one = col_character(),
             sport_two = col_character(),
             eb_atp_cd3 = col_character(),
             home_addr1 = col_character(),
             home_addr2 = col_character(),
             home_addr3 = col_character(),
             home_addr4 = col_character(),
             home_phone = col_character(),
             home_city = col_character()
           )) %>%
    mutate(Sem = c.Sem,
           CY = c.Year)
}
