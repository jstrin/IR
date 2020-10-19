#' read_GRADyyyy
#'
#' Function for loading the Graduation lists
#' flat file (.dat) from e-Campus.
#'
#'
#'
#' To retireve the dat file:
#'
#'
#'
#' @importFrom stringr str_sub str_detect
#' @importFrom magrittr %>%
#' @importFrom readr read_fwf
#' @import dplyr
#' @import here
#'
#'
#' @export read_GRADyyyy
#'


read_GRADyyyy <- function(file, dir=NA){
  AY <- as.numeric(str_sub(file, -6,-5))
  if(AY > 50){
    format <- "legacy"
    AY_out <- 1900+AY
  }else{
    AY_out <- 2000+AY
    if(AY<4){
      format <- "legacy"
    }else{
      if(AY<12){
        format<- "pre2012"
      }else{
        format <- "post2011"
      }
    }
  }

  if(is.na(dir)){
    file.dir = here::here(file)
  }else{
    file.dir <- paste(dir, file, sep ="/")
  }

  if(format == "legacy"){
    df_out <- read_fwf(file = file.dir, col_positions =
                         fwf_widths(c(9,1,2,2,2,1,2,2,1,8,8,3,6,2,2,8,8,3,3,3,4,4,3,3,3,14,12,1,1),
                                    col_names = c("nid", "sex", "birth_year", "birth_month", "birth_day",
                                                  "race_legacy", "entry_year", "entry_month","entry_status", "entry_major1",
                                                  "entry_major2", "tuition_code", "FICE_number", "grad_year",
                                                  "grad_month", "grad_major1", "grad_major2", "sat_verb", "sat_math",
                                                  "gpa_pred", "hs_class", "hs_rank", "gpa", "gre_verb", "gre_quant",
                                                  "last", "first", "middle", "dual")),
                       col_types = cols(
                         gpa_pred = "n",
                         gpa = "n",
                         sat_verb = "n",
                         sat_math = "n",
                         gre_verb = "n",
                         gre_quant = "n",
                         grad_month = "c",
                         grad_year = "n",
                         entry_year = "n",
                         entry_month = "c",
                         race = "c"
                       )) %>%
      mutate(dob = as.numeric(paste0("19", birth_year,  birth_month,  birth_day)),
             dual = case_when(dual==0 ~ as.character(NA),
                              dual == 1 ~ "Y")) %>%
      select(-hs_class, -hs_rank) %>%
      mutate(entryCY = case_when(entry_year > 5 ~ 1900 +entry_year,
                                 TRUE ~ 2000+entry_year),
             entryAY = case_when( entry_month %in% c("06", "07", "09") ~ entryCY + 1,
                                  TRUE ~ entryCY),
             entrySem = case_when(entry_month %in% c("01") ~ "Spring",
                                  entry_month %in% c("06", "07") ~ "Summer",
                                  entry_month %in% c("09") ~ "Fall"),
             gradCY = case_when(grad_year > 5 ~ 1900 +grad_year,
                                TRUE ~ 2000+grad_year),
             gradSem = case_when(grad_month %in% c("01", "02") ~ "Spring",
                                 grad_month %in% c("06", "05") ~ "Summer",
                                 grad_month %in% c("09", "08") ~ "Fall"),
             gradAY = AY_out) %>%
      mutate(Degree1 = str_sub(grad_major1, -3),
             Degree2 = str_sub(grad_major2, -3)) %>%
      mutate(Bachelors = case_when(Degree1 %in% c("BOS", "BFA", "BGS", "BLA", "BOA", "BOM") ~ "Bach",
                                   Degree2 %in% c("BOS", "BFA", "BGS", "BLA", "BOA", "BOM") ~ "Bach"),
             Masters = case_when(Degree1 %in% c("MBA", "MCP", "MLS", "MOA", "MOM", "MOO", "MOS", "MPA", "MMA") ~ "Masters",
                                 Degree2 %in% c("MBA", "MCP", "MLS", "MOA", "MOM", "MOO", "MOS", "MPA", "MMA") ~ "Masters"),
             PhD = case_when(Degree1 %in% c("PHD") ~ "PhD",
                             Degree2 %in% c("PHD") ~ "PhD"),
             ProfDoc = case_when(Degree1 %in% c("PMD") ~ "ProfDoc",
                                 Degree2 %in% c("PMD") ~ "ProfDoc"),
             Cert = case_when(Degree1 %in% c("TCP") ~ "Cert",
                              Degree2 %in% c("TCP") ~ "Cert"))

  }
  if(format == "pre2012"){
    df_out<- read_fwf( file = file.dir, col_positions =
                         fwf_widths(c(9,1,9,1,1,1,8,1,4,1,10,10,5,6,4,1,10,10,4,4,3,4,4,3,3,3,20,15,1,10,2,1),
                                    col_names = c("ID", "blank", "nid", "sex", "race", "cit", "dob", "blank2",
                                                  "entterm", "entryst", "entplan1", "entplan2", "tuition",
                                                  "transfer_ets", "grad_term", "blank3", "grad_plan1", "grad_plan2",
                                                  "sat_verb", "sat_math", "blank4", "hs_class", "hs_rank", "gpa", "gre_verb",
                                                  "gre_quant", "last", "first", "middle", "degree", "degree_level",
                                                  "dual")),
                       col_types = cols(
                         gpa = "n",
                         sat_verb = "n",
                         sat_math = "n",
                         gre_verb = "n",
                         gre_quant = "n",
                         dob = "n",
                         transfer_ets = "c",
                         grad_term = "c",
                         entterm = "c",
                         race = "c"

                       )) %>%
      #    select(-starts_with("blank")) %>%
      select(-hs_class, -hs_rank) %>%
      mutate(entCY_temp = as.numeric(str_sub(entterm, 1,2))) %>%
      mutate(entryCY = case_when(entCY_temp >30~entCY_temp + 1900,
                                 TRUE ~ entCY_temp + 2000),
             entryAY = case_when(str_sub(entterm, 3,4) %in% c("06", "07", "09") ~ entryCY +1,
                                 TRUE ~ entryCY),
             gradCY = as.numeric(str_sub(grad_term,1,2))+ 2000,
             gradSem = case_when(str_sub(grad_term,3,4) %in% c("01", "02") ~ "Spring",
                                 str_sub(grad_term,3,4) %in% c("06", "05") ~ "Summer",
                                 str_sub(grad_term,3,4) %in% c("09", "08") ~ "Fall"),
             gradAY = AY_out) %>%
      mutate(Bachelors = case_when(degree_level == "05" ~ "Bach"),
             Masters = case_when(degree_level == "07" ~ "Masters"),
             PhD = case_when(str_detect(degree, "PHD")~ "PhD",
                             degree %in% c("PHAPS", "MAP") ~ "PHD"),
             ProfDoc = case_when(degree_level == "10" ~ "ProfDoc",
                                 degree %in% c("DPT", "DNP", "AUD") ~ "ProfDoc"),
             Cert = case_when(degree_level == "06" ~ "Cert"))
  }
  if(format == "post2011"){
    df_out<-  read_fwf( file = file.dir, col_positions =
                          fwf_widths(c(9,1,9,1,1,1,8,1,4,1,10,10,5,6,4,1,10,10,4,4,3,4,4,3,3,3,20,15,1,10,2,1,1,10,10,10,10),
                                     col_names = c("ID", "blank", "nid", "sex", "race", "cit", "dob", "blank2",
                                                   "entterm", "entryst", "entplan1", "entplan2", "tuition",
                                                   "transfer_ets", "grad_term", "blank3", "grad_plan1", "grad_plan2",
                                                   "sat_verb", "sat_math", "blank4", "hs_class", "hs_rank", "gpa", "gre_verb",
                                                   "gre_quant", "last", "first", "middle", "degree", "degree_level",
                                                   "dual","blank5", "grad_plan3", "grad_minor1", "grad_minor2", "grad_minor3")),
                        col_types = cols(
                          gpa = "n",
                          sat_verb = "n",
                          sat_math = "n",
                          gre_verb = "n",
                          gre_quant = "n",
                          dob = "n",
                          transfer_ets = "c",
                          grad_term = "c",
                          race = "c"
                        )) %>%
      #   select(-starts_with("blank")) %>%
      select(-hs_class, -hs_rank)%>%
      mutate(entCY_temp = as.numeric(str_sub(entterm, 1,2))) %>%
      mutate(entryCY = case_when(entCY_temp >30~entCY_temp + 1900,
                                 TRUE ~ entCY_temp + 2000),
             entryAY = case_when(str_sub(entterm, 3,4) %in% c("06", "07", "09") ~ entryCY +1,
                                 TRUE ~ entryCY),
             gradCY = as.numeric(str_sub(grad_term,1,2))+ 2000,
             gradSem = case_when(str_sub(grad_term,3,4) %in% c("01", "02") ~ "Spring",
                                 str_sub(grad_term,3,4) %in% c("06", "05") ~ "Summer",
                                 str_sub(grad_term,3,4) %in% c("09", "08") ~ "Fall"),
             gradAY = AY_out)%>%
      mutate(Bachelors = case_when(degree_level == "05" ~ "Bach"),
             Masters = case_when(degree_level == "07" ~ "Masters"),
             PhD = case_when(str_detect(degree, "PHD")~ "PhD",
                             degree %in% c("PHAPS", "MAP") ~ "PHD"),
             ProfDoc = case_when(degree_level == "10" ~ "ProfDoc",
                                 degree %in% c("DPT", "DNP", "AUD") ~ "ProfDoc"),
             Cert = case_when(degree_level == "06" ~ "Cert"))
  }
  df_out
}
