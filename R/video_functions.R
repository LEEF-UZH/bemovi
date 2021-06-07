#' Extract delays from the \code{.cxd} file
#'
#' This function reads the metadata from the \code{.cxd} file and returns the 
#' delays between the images were taken.
#' @param file file name, of the \code{.cxd} file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return a vector of the length of number of frames -1 specifying the delay between the images taken.
#' @export
#'
#' @examples
get_delays_cxd <- function(
  file,
  mc.cores = par_mc.cores()
) {
  file <- normalizePath(file)
  
  if ((length(file) == 1) && dir.exists(file)) {
    delays <- get_delays_cxd(
      file = list.files(
        file,
        pattern = "\\.cxd$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    )
    return(delays)
  } else if (length(file) > 1) {
    delays <- parallel::mclapply(
      file,
      FUN = get_delays_cxd,
      mc.cores = mc.cores
    )
    names(delays) <- basename(file)
    return(delays)
  }
  
  file <- normalizePath(file)
  
  arguments <- paste0(
    " -nopix",
    " -no-upgrade",
    " '", file, "'"
  )
  md <- system2(
    command = par_showinf(),
    args = arguments,
    stdout = TRUE
  )  
  md <- grep("Time_From_Last", md, value = TRUE)
  delay <- sapply(
    strsplit(md, ": "),
    "[",
    2
  )[-1]
  delay <- as.numeric(delay)
  return(delay)  
}


#' Extract fps (frames per second) from the \code{.cxd} file
#'
#' The fps is based on the results of \code{get_delays_cxd(file)} by averaging these and
#' calculating from this mean the fps.
#' @param file  file name, of the \code{.cxd} file
#'
#' @return the fps (frames per second) of the video
#' @export
#'
#' @examples
get_fps_cxd <- function(
  file,
  mc.cores = par_mc.cores()
) {
  if (length(file) != 1) {
    stop("File has to be of length 1")
  }
  
  file <- normalizePath(file)
  
  x <- get_delays_cxd(file, mc.cores)
  fps <- 1/mean(x)
  return(fps)
}

#' Extract fps (frames per second) using ffmpeg
#'
#' The fps is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but at the moment only supported for \code{avi} files.
#' @param file  file name(s), of the video (\code{avi}) file(s) or directory in which the video files are
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the fps of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_fps_avi <- function(
  file,
  mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)

  if ((length(file) == 1) && dir.exists(file)) {
    fps <- get_fps_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    ) 
    return(simplify2array(fps))
  } else if (length(file) > 1) {
    fps <- parallel::mclapply(
      file,
      FUN = get_fps_avi,
      mc.cores = mc.cores
    )
    return(simplify2array(fps))
  }
  
  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = par_ffmpeg(),
      args = arguments,
      stderr = TRUE
    )
  )  
  fps <- grep(
    "fps", 
    output, 
    value = TRUE
  )
  fps <- strsplit(
    fps,
    ","
  )[[1]]
  fps <- grep(
    "fps",
    fps,
    value = TRUE
  )
  fps <- gsub("fps", "", fps)
  fps <- as.numeric(fps)
  names(fps) <- basename(file)
  return(simplify2array(fps))
}

#' Extract duration in seconds using ffmpeg
#'
#' The duration is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the duration (in seconds) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_duration_avi <- function(
  file,
  mc.cores = par_mc.cores()
) {

  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    duration <- get_duration_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    ) 
    return(simplify2array(duration))
  } else if (length(file) > 1) {
    duration <- parallel::mclapply(
      file,
      FUN = get_duration_avi,
      mc.cores = mc.cores
    )
    return(simplify2array(duration))
  }
  
  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = par_ffmpeg(),
      args = arguments,
      stderr = TRUE
    )
  )  
  duration <- grep(
    "Duration", 
    output, 
    value = TRUE
  )
  duration <- strsplit(
    duration,
    ","
  )[[1]]
  duration <- grep(
    "Duration",
    duration,
    value = TRUE
  )
  duration <- gsub("Duration:", "", duration)
  duration <- strsplit(
    duration, 
    ":"
  )[[1]]
  duration <- as.numeric(duration)
  duration <- duration[[1]] * 60 * 60 + duration[[2]] * 60 + duration[[3]]
  names(duration) <- basename(file)
  return(simplify2array(duration))
}

#' Extract height in pixels using ffmpeg
#'
#' The height is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the height (in pixels) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_height_avi <- function(
  file,
  
  mc.cores = par_mc.cores()
) {
  
  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    height <- get_height_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    ) 
    return(simplify2array(height))
  } else if (length(file) > 1) {
    height <- parallel::mclapply(
      file,
      FUN = get_height_avi,
      mc.cores = mc.cores
    )
    return(simplify2array(height))
  }
  
  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = par_ffmpeg(),
      args = arguments,
      stderr = TRUE
    )
  )  
  height <- grep(
    "([0-9]+)x([0-9]+)", 
    output, 
    value = TRUE
  )
  height <- strsplit(
    height,
    ","
  )[[1]]
  height <- grep(
    "([0-9]+)x([0-9]+)",
    height,
    value = TRUE
  )
  height <- gsub(" ", "x", trimws(height[-1]))
  height <- strsplit(
    height, 
    "x"
  )[[1]]
  height <- as.numeric(height[[2]])
  names(height) <- basename(file)
  return(simplify2array(height))
}

#' Extract width in pixels using ffmpeg
#'
#' The width is based on using the output of \code{ffmpeg} and
#' should therefore work for all ffmpeg supported video formats,
#' but is only implemented for \code{.avi} files.
#' @param file  file name, of the video file
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#'
#' @return named vector with the width (in pixels) of the video(s) and the names are the file names
#' @export
#'
#' @examples
get_width_avi <- function(
  file,
  mc.cores = par_mc.cores()
) {
  
  file <- normalizePath(file)
  if ((length(file) == 1) && dir.exists(file)) {
    width <- get_width_avi(
      file = list.files(
        file,
        pattern = "\\.avi$",
        full.names = TRUE
      ),
      mc.cores = mc.cores
    ) 
    return(simplify2array(width))
  } else if (length(file) > 1) {
    width <- parallel::mclapply(
      file,
      FUN = get_width_avi,
      mc.cores = mc.cores
    )
    return(simplify2array(width))
  }
  
  arguments <- paste0(
    " -i '", file, "'"
  )
  output <- suppressWarnings(
    system2(
      command = par_ffmpeg(),
      args = arguments,
      stderr = TRUE
    )
  )  
  width <- grep(
    "([0-9]+)x([0-9]+)", 
    output, 
    value = TRUE
  )
  width <- strsplit(
    width,
    ","
  )[[1]]
  width <- grep(
    "([0-9]+)x([0-9]+)",
    width,
    value = TRUE
  )
  width <- gsub(" ", "x", trimws(width[-1]))
  width <- strsplit(
    width, 
    "x"
  )[[1]]
  width <- as.numeric(width[[1]])
  names(width) <- basename(file)
  return(simplify2array(width))
}


#' Function to convert the video files to .avi format using lossless conversion
#'
#' Function uses bftools to convert \code{cxd} files to \code{avi} and ffmpeg to 
#' compress these to lossles avi
#' @param cxd_file one or more \code{cxd} file to be converted or a directory with \code{.cxd} files.
#' @param avi_dir directory for the converted \code{cxd} files and the metadata files
#' @param compression_level compression level - defaults to 5
#' @param ffmpeg execuable ffmpeg. May have to be including path.
#' @param bfconvert executable bfconvert from bftools. May have to be including path.
#' @param showinf executable showinf from bftools. May have to be including path.
#' @param mc.cores Number of cores to be used when more than one xd file is given.
#' @return returns nothing (NULL)
#' 
#' @importFrom parallel mclapply
#' @export

convert_cxd_to_avi <- function(
  cxd_file,
  avi_dir,
  compression_level = 5,
  ffmpeg = par_ffmpeg(),
  bfconvert = par_bfconvert(),
  showinf = par_showinf(),
  mc.cores = par_mc.cores()
) {
  if (length(avi_dir) != 1) {
    stop(" 'avi_dir' has to be of length 1!")
  }
  
  cxd_file <- normalizePath(cxd_file)
  avi_dir <- normalizePath(avi_dir)
  
  if (dir.exists()) {
    convert_cxd_to_avi(
      cxd_file = list.files(
        cxd_file, 
        pattern = "\\.cxd$"
      ),
      avi_dir = avi_dir,
      ffmpeg = ffmpeg,
      bfconvert = bfconvert,
      showinf = showinf,
      mc.cores = mc.cores
    ) 
    return(invisible(NULL))
  } else if (length(cxd_file) > 1) {
    message("<<< BEGIN mclapply convert to avi")
    message("    mc.cores = ", mc.cores)
    parallel::mclapply(
      cxd_file,
      convert_cxd_to_avi,
      avi_dir = avi_dir,
      ffmpeg = ffmpeg,
      bfconvert = bfconvert,
      showinf = showinf,
      mc.cores = mc.cores
    )
    message(">>> END mclapply convert to avi")
    return(invisible(NULL))
  }
  
  message("    BEGIN converting ", cxd_file)
  cxd_metadata_file <- file.path(
    avi_dir, 
    paste0(basename(cxd_file), ".metadata")
  )
  
  tmpdir <- tempfile()
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  avi_file <- file.path(
    avi_dir,
    gsub("\\.cxd$", ".avi", basename(cxd_file))
  )
  avi_conv_tmp <- file.path(tmpdir, paste0("conv_", basename(avi_file)))
  avi_tmp <- file.path(tmpdir, basename(avi_file))
  cxd_metadata_tmp <- file.path(tmpdir, basename(cxd_metadata_file))
  
  on.exit({unlink(tmpdir)})

  message("      Extracting Metadata from ", cxd_file, " -->> ", cxd_metadata_tmp)
  arguments <- paste0(
    " -nopix",
    " -no-upgrade",
    " '", cxd_file, "'"
  )
  system2(
    command = showinf,
    args = arguments,
    stdout = cxd_metadata_tmp
  )
  
  message("      Converting ", cxd_file, " -->> ", avi_conv_tmp)
  arguments <- paste0(
    " -overwrite",
    " -no-upgrade ", 
    " '", cxd_file, "'",
    " '", avi_conv_tmp, "'"
  )
  system2(
    command = bfconvert,
    args = arguments,
    stdout = NULL
  )

  message("      Compressing ", avi_conv_tmp, " -->>", avi_tmp)
  fps <- get_fps_cxd(cxd_file)
  arguments <- paste0(
    " -i '", avi_conv_tmp, "'",
    " -y",
    " -vcodec png",
    " -vf 'setpts=N/", fps, "/TB'", 
    " -r ", fps, 
    " -compression_level ", compression_level,
    " -vtag 'PNG ' ",
    " '", avi_tmp, "'"
  )
  system2(
    command = ffmpeg,
    args = arguments,
    stdout = NULL
  )

  message("      Moving ", basename(avi_tmp), " -->> ", basename(avi_tmp))
  dir.create(dirname(avi_file), showWarnings = FALSE, recursive = TRUE)
  file.rename(
    from = avi_tmp,
    to = avi_file
  )
  file.rename(
    from = cxd_metadata_tmp,
    to = cxd_metadata_file
  )
  message("    END converting ", cxd_file)
  
  invisible(NULL)
}
