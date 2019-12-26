#' get_data_dir
#'
#' When working in the URI sharepoint structure
#' function will return the path to the data director
#'
#'
#'
#'
#'
#'
#'
#' @importFrom stringr str_sub str_locate str_detect
#' @importFrom magrittr %>%
#' @import dplyr
#'
#'
#' @export get_data_dir
#'

get_data_dir <- function(folder_path = NULL){

  stopifnot( str_detect(here::here(), "Institutional Research - Documents") )

  if(is.null(folder_path)){
  paste0(str_sub(here::here(), 1,
          str_locate(here::here(), "Institutional Research - Documents")[1,"end"] ), "/Data")
  }else{
    paste0(str_sub(here::here(), 1,
                   str_locate(here::here(), "Institutional Research - Documents")[1,"end"] ), "/Data/", folder_path)
  }


}
