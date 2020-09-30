#' Return ImegeJ executable based on platform. Darwin, Windows and Linux are supported
#'
#' @return full path of ewxecutable of ImageJ
#' @export
#'
#' @examples
#' ij.path()
#' 
ij.bin <- function(){
  result <- switch(
    Sys.info()["sysname"], 
    Darwin = {
      file.path(par_IJ.path(), "ImageJ-macosx")
    }, 
    Windows = {
      file.path(par_IJ.path(), "ImageJ-win64.exe")
    }, 
    Linux = {
      file.path(par_IJ.path(), "ImageJ-linux64")
    }, 
    stop("OS not supported by bemoviu!"))
  ##
  return(result)
}