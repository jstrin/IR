#' sharepoint_dir
#'
#' Function for specifing the correct shared directory
#'
#'
#'
#'
#' @importFrom stringr str_sub str_locate str_detect
#' @import here
#'
#'
#' @export sharepoint_dir
#'
#'

sharepoint_dir <- function(new_path = NULL, root_path = NULL) {
  if(is.null(root_path)){
    root_path <- here::here()
  }
  if(!str_detect(root_path, "Institutional Research - Documents/")){
    print(paste0("Sharepoint not on root_path: '", root_path, "'"))
  }else{
    i.end <-  str_locate(root_path, "Research - Documents")[[2]] - nchar(root_path)

  paste0(str_sub(root_path, 1, i.end), new_path)
  }
}
