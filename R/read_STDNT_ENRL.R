#' read_STDNT_ENRL
#'
#' Function for loading the Course Enrollment Files
#' flat file (.dat) from e-Campus.
#'
#'
#'
#' files are located: Data/Raw/Course Enrollment/
#'
#'
#'
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#' @importFrom readr read_fwf
#' @import dplyr
#' @import here
#'
#'
#' @export read_STDNT_ENRL
#'


read_STDNT_ENRL <- function(file, data.dir = NA){
  if(is.na(data.dir)){
    file.dir <- here::here(file)
  }else{
    file.dir <- paste(data.dir, file, sep="/")
  }

  read_fwf(file.dir, col_positions = fwf_widths(c(11,20,30,30,30,8,4,8,
                                       10,4,6,4,6,3,4,5,10,5,
                                       10,5,1,1,1,7,3,5),
           col_names =c("ID", "nat_id", "last", "first", "middle",
                        "dob", "YearSem", "subj", "crs_no", "section_id",
                        "crs_id", "crs_offer_nbr", "class_nbr", "acad_level",
                        "entrycd", "acad_prog1", "acad_plan1", "acad_prog2",
                        "acad_plan2", "resid", "sex", "race", "citizen",
                        "units_taken", "grade_alpha", "grade")),
           col_types = cols(
             ID = "c",
             nat_id ="c",
             last = "c",
             first = "c",
             dob = col_date(format = "%Y%m%d"),
             YearSem = "i",
             subj = "c",
             crs_no = "c",
             section_id = "c",
             crs_offer_nbr = "c",
             class_nbr = "c",
             acad_level = "c",
             entrycd = "c",
             acad_prog1 = "c",
             acad_plan1 = "c",
             acad_prog2 = "c",
             acad_plan2 = "c",
             resid = "c",
             sex = "c",
             race = "c",
             citizen = "c",
             units_taken = "c",
             grade_alpha = "c",
             grade = "d"

           )) %>%
          select(-nat_id)

}
