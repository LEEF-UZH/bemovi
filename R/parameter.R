#' Template function to assign value to parameter in the package wide cache
#'
#' assign the function to a new cvariable and the name of the function woll be used for the parameter name. e.g:
#' \itemize{
#'    \item{fps <- par_template}
#' }
#' @param value if missing, the value of the parameter will be returned, \code{NULL} if the parameter does not exist; if specified, the parameter will be set to the value
#'
#' @return the (new) value of the argument
#' 
#' @export
#' 
#' @examples
par_template <- function(value) {
  parName <- match.call()[[1]]
  parName <- as.character(parName)
  parName <- tail(parName, 1)
  parName <- gsub("par_", "", parName)
  if ( missing(value) ) { 
    if (!exists(parName, envir = .BEMOVI_CACHE, inherits = FALSE)) {
      stop("Parameter '", parName, "' not set!\n", "Set by using '", parName, "(value)' before usage!")
    }
  } else {
    assign(parName, value, envir = .BEMOVI_CACHE)
  }
  result <- base::get(parName, envir = .BEMOVI_CACHE, inherits = FALSE)
  return( result )
} 

#' Save parameter into \code{.yaml} file
#'
#' @param file name of parameter file
#'
#' @importFrom yaml write_yaml
#' @return invisibly \code{TRUE}
#' @export
#'
#' @examples
save_parameter <- function(file = "parameter.yaml") {
  yaml::write_yaml(
    x = as.list(.BEMOVI_CACHE),
    file = file
  )
  invisible(TRUE)
}

#' Load parameter from \code{file}
#'
#' @param file name of parameter file
#'
#' @return invisibly TRUE
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
load_parameter <- function(file = "parameter.yaml") {
  p <- yaml::read_yaml( file )
  list2env(p, envir = .BEMOVI_CACHE)
  invisible( TRUE )
}


#' Print the bemovi parameter
#'
#' @param echo if \code{TRUE} print the parameter, if \code{FALSE}  just return them as list
#' @param print_as_yaml Print in yaml formated text; \code{~} stands for NULL
#' 
#' @return invisible returns list of parameter for further processing
#' @importFrom yaml as.yaml
#' @export
#'
#' @examples
print_parameter <- function( 
  print_as_yaml = TRUE, 
  echo = TRUE
) {
  result <- as.list(.BEMOVI_CACHE)
  if (echo) {
    if (print_as_yaml) {
      cat(yaml::as.yaml(result))
    } else {
      print(result)
    }
  }
  return( invisible(result) )
}

# Folder Structure Parameter ----------------------------------------------

## Folder structure
#' @name par_to.data
#' @title parameter to.data
#' @export
par_to.data <- par_template 
par_to.data(NULL)
#
#' @name par_video.description.folder
#' @title parameter video.description.folder
#' @export
par_video.description.folder <- par_template
par_video.description.folder( "0 - video description" )
#
#' @name par_raw.video.folder
#' @title parameter raw.video.folder
#' @export
par_raw.video.folder <- par_template 
par_raw.video.folder( "1 - raw" )
#
#' @name par_particle.data.folder
#' @title parameter particle.data.folder
#' @export
par_particle.data.folder <- par_template
par_particle.data.folder( "2 - particle data" )
#
#' @name par_trajectory.data.folder
#' @title parameter trajectory.data.folder
#' @export
par_trajectory.data.folder <- par_template
par_trajectory.data.folder( "3 - trajectory data" )
#
#' @name par_temp.overlay.folder
#' @title parameter temp.overlay.folder
#' @export
par_temp.overlay.folder <- par_template
par_temp.overlay.folder( "4a - temp overlays" )
#
#' @name par_overlay.folder
#' @title parameter overlay.folder
#' @export
par_overlay.folder <- par_template
par_overlay.folder( "4 - overlays" )
#
#' @name par_merged.data.folder
#' @title parameter merged.data.folder
#' @export
par_merged.data.folder <- par_template
par_merged.data.folder( "5 - merged data" )
##
#' @name par_ijmacs.folder
#' @title parameter ijmacs.folder
#' @export
par_ijmacs.folder <- par_template
par_ijmacs.folder( "ijmacs" )


# Paths to existing files and binaries ------------------------------------


#' @name par_video.description.file
#' @title parameter video.description.file
#' @export
par_video.description.file <- par_template
par_video.description.file( "video.description.txt" )
#
#' @name par_java.path
#' @title parameter java.path
#' @export
par_java.path <- par_template
par_java.path(NULL)
# 
#' @name par_to.particlelinker
#' @title parameter to.particlelinker
#' @export
par_to.particlelinker <- par_template
par_to.particlelinker(NULL)
#
#' @name par_IJ.path
#' @title parameter IJ.path
#' @export
par_IJ.path <- par_template
par_IJ.path(NULL)
#


# Other Parameter ---------------------------------------------------------

#' @name par_min_size
#' @title parameter min_size
#' @export
par_min_size <- par_template
par_min_size(0)
#
#' @name par_max_size
#' @title parameter max_size
#' @export
par_max_size <- par_template
par_max_size(10000)
#

#' @name par_min_area
#' @title parameter min_area
#' @export
par_min_area <- par_template
par_min_area(250)
#
#' @name par_max_area
#' @title parameter max_area
#' @export
par_max_area <- par_template
par_max_area(50000)
#



#' @name par_pixel_to_scale
#' @title parameter pixel_to_scale
#' @export
par_pixel_to_scale <- par_template
par_pixel_to_scale(NULL)
#
#' @name par_fps
#' @title parameter fps
#' @export
par_fps <- par_template
par_fps(NULL)
#

#' @name par_thresholds
#' @title parameter thresholds
#' @export
par_thresholds <- par_template
par_thresholds(c(10, 255))
#

#' @name par_width
#' @title parameter width
#' @export
par_width <- par_template
par_width(NULL)
#
#' @name par_height
#' @title parameter height
#' @export
par_height <- par_template
par_height(NULL)
#

#' @name par_difference.lag
#' @title parameter difference.lag
#' @export
par_difference.lag <- par_template
par_difference.lag(NULL)
#

#' @name par_net_filter
#' @title parameter net_filter
#' @export
par_net_filter <- par_template
par_net_filter(NULL)
#
#' @name par_duration_filter
#' @title parameter duration_filter
#' @export
par_duration_filter <- par_template
par_duration_filter(NULL)
#
#' @name par_detect_filter
#' @title parameter detect_filter
#' @export
par_detect_filter <- par_template
par_detect_filter(NULL)
#
#' @name par_median_step_filter
#' @title parameter median_step_filter
#' @export
par_median_step_filter <- par_template
par_median_step_filter(NULL)
#

#' @name par_memory
#' @title parameter memory 
#' @export
par_memory <- par_template
par_memory(512)
#

#' @name par_linkrange
#' @title parameter linkrange 
#' @export
par_linkrange <- par_template
par_linkrange(1)
#

#' @name par_disp
#' @title parameter disp 
#' @export
par_disp <- par_template
par_disp(10)

#' @name par_disp
#' @title parameter disp 
#' @export
par_extrapolation.factor <- par_template
par_extrapolation.factor(13.84)



# Timeout for system calls in seconds -------------------------------------

#
#' @name par_timeout
#' @title parameter timeout. If 0, no timeout
#' @export
par_timeout <- par_template
par_timeout(0)

# Crop in Pixels ----------------------------------------------------------

#
#' @name par_crop_pixels
#' @title Cropping of identified and measured particles.
#' @details This function returns a list with four elements, namely \code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}.
#'   To set this parameter \bold{all need to be set in a named list!}
#'   If a value is \code{NULL} (\code{~} in the parameter file), it is assumed to be the maximum or minimum value.  
#' @export
par_crop_pixels <- par_template
par_crop_pixels(list(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf))
fix_crop_pixels <- function(crop_pixels = par_crop_pixels()) {
  if (is.null(crop_pixels$xmin)) {
    crop_pixels$xmin <- 0
  }
  if (is.null(crop_pixels$xmax)) {
    crop_pixels$xmin <- Inf
  }
  if (is.null(crop_pixels$ymin)) {
    crop_pixels$xmin <- 0
  }
  if (is.null(crop_pixels$ymax)) {
    crop_pixels$xmin <- Inf
  }
  return(crop_pixels)
}


# Names of the output files ----------------------------------------------

#
#' @name par_particle
#' @title Name of organised particles file
#' @export
par_particle <- par_template
par_particle("particle.rds")

#
#' @name par_trajectory
#' @title Name of organised trajectory file
#' @export
par_trajectory <- par_template
par_trajectory("trajectory.rds")

#
#' @name par_master
#' @title Name of Master file
#' @export
par_master <- par_template
par_master("Master.rds")

#
#' @name par_morph_mvt
#' @title Name of Morph_mvt file
#' @export
par_morph_mvt <- par_template
par_morph_mvt("Morph_mvt.rds")

