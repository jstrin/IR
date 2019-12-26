#' read_internal_payroll
#'
#' Internal payroll report uses a non-standard data format
#' this function loads and reformats the data to facilitate
#' analysis
#'
#' @importFrom readxl read_excel
#' @import dplyr
#'
#'
#'
#'
#' @export read_internal_payroll



read_internal_payroll <- function(file, data_dir = NULL){

  if(!is.null(data_dir)){
    file_path <- paste0(data_dir, file)
  }else{
    file_path <- here::here(file)
  }



# Load Internal payroll data downloaded from Ecampus
dfInternalPayroll <- read_excel(file_path, sheet = 1, skip = 6,
                              col_names = c("Emplid", "Empl_rcd", "Name", "Jobcode", "end_date",
                                            "earnings_acct", "acct", "amt", "total", "blank", "check"),
                              col_types = c(Emplid = "text",
                                            Empl_rcd = "text",
                                            Name = "text",
                                            Jobcode = "text",
                                            end_date = "date",
                                            earnings_acct = "text",
                                            acct = "text",
                                            amt = "text",
                                            total = "text",
                                            blank = "text",
                                            check = "text")) %>%
  select(-blank) %>%
  mutate(index = row_number())



split_index <-  dfInternalPayroll %>%
  # mutate(index = row_number()) %>%
  filter(total == "Total") %>%
  select(index) %>%
  unlist()

dfInternal_base <- dfInternalPayroll %>%
  filter(!is.na(Emplid)) %>%
  filter(index < split_index)%>%
  mutate(amt = as.numeric(amt)) %>%
  group_by(Emplid, Empl_rcd, Name, Jobcode, end_date, earnings_acct, acct) %>%
  summarise(count = n(),
            amt = sum(amt)) %>%
  ungroup()

dfInternal_secondary <- dfInternalPayroll %>%
  filter(!is.na(Emplid)) %>%
  filter(index > split_index) %>%
  rename(Fica = amt,  acct2 = acct) %>%
  mutate(Fica = as.numeric(Fica)) %>%
  group_by(Emplid, Empl_rcd, Name, Jobcode, end_date, earnings_acct, acct2) %>%
  summarise(count = n(),
            Fica = sum(Fica)) %>%
  ungroup()

dfInternal <- full_join(dfInternal_base, dfInternal_secondary,
                        by = c("Emplid", "Empl_rcd", "Name", "Jobcode", "end_date", "earnings_acct"))


dfInternal
}
