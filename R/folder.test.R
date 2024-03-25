#' Title: valid.folder.path
#'
#' @param file_path
#'
#' @return
#' @export
valid.folder.path <-
  function(file_path){
    if(is.null(file_path)){
      # No folder specified
      return(FALSE)
    }
    if(!dir.exists(file_path)){
      # Folder does not exist
      return(FALSE)
    }
    return(TRUE)
  }

#' Title file_path
#'
#' @param file_path
#'
#' @return
#' @export

valid.file.path <-
  function(file_path){
    if(is.null(file_path)){
      # No file specified
      return(FALSE)
    }
    if(!file.exists(file_path)){
      # File does not exist
      return(FALSE)
    }
    return(TRUE)
  }
