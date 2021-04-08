#' Function to create a new video with the extracted trajectories overlayed onto the original video
#'
#' A function to overlay the extracted trajectories onto the original video, using plots created in R and then processed in
#' ImageJ; two visualization types are available
#'
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param raw.video.folder directory with the raw video files
#' @param temp.overlay.folder temporary directory to save the overlay created
#'   with R
#' @param overlay.folder directory where the overlay videos are saved
#' @param width width of the raw video
#' @param height height of the raw video
#' @param difference.lag numeric value specifying the offset between two video
#'   frames to compute the difference image
#' @param type string indicating the visualization type (i.e. 'label' or
#'   'traj'): either the overlay is showing the trajectory ID and outlines the
#'   detected particle (type='label') or the whole trajectory remains plotted
#'   (type='traj').
#' @param predict_spec logical or character If \code{TRUE}, the Master.rds file must
#'   have a column called predict_spec, indicating the species to which the
#'   trajectory belongs. If a \code{character}, the Master.rds file must
#'   have a column called the value of the \code{character} value, indicating the species to which the
#'   trajectory belongs
#' @param ffmpeg command to run ffmpeg. The default is \code{ffmpeg}.  It can include a path.
#' 
#' 
#' @export

create_overlays <- function(
  to.data = par_to.data(),
  merged.data.folder = par_merged.data.folder(),
  raw.video.folder = par_raw.video.folder(),
  temp.overlay.folder = par_temp.overlay.folder(),
  overlay.folder = par_overlay.folder(),
  width = par_width(),
  height = par_height(),
  difference.lag = par_difference.lag(),
  type = "traj",
  predict_spec = FALSE,
  ffmpeg = "ffmpeg"
) {
  if (isTRUE(predict_spec)) {
    predict_spec_field <- "predict_spec"
  } else if (is.character(predict_spec)) {
    predict_spec_field <- predict_spec
    predict_spec <- TRUE
  } else {
    stop("predict_spec has to be TRUE, FALSE, or a character vector of length 1")
  }

  # Helper function ---------------------------------------------------------

  plot_crop_frame <- function() {
    cf <- fix_crop_pixels(par_crop_pixels())
    cf[["xmax"]][is.infinite(cf[["xmax"]])] <- width
    cf[["ymax"]][is.infinite(cf[["ymax"]])] <- height
    rect(
      xleft = cf$xmin, 
      ybottom = cf$ymin, 
      xright = cf$xmax, 
      ytop = cf$ymax, 
      border = "red", 
      lwd = 4
    ) 
  }

  # trajectory.data<-trajectory<-ijmacs.folder<-NULL

  # video.dir <- file.path(to.data, raw.video.folder)

  trajectory.data <- readRDS(file = file.path(to.data, merged.data.folder, par_master()))
  file_names <- unique(trajectory.data$file)

  ## create directories for overlays
  dir.create(file.path(to.data, temp.overlay.folder), showWarnings = FALSE)
  dir.create(file.path(to.data, overlay.folder), showWarnings = FALSE)
  
  lapply(
    1:length(file_names),
    function(i) {
      dir.create(file.path(to.data, temp.overlay.folder, file_names[i]), showWarnings = FALSE)
      trajectory.data_tmp <- subset(trajectory.data, file == file_names[i])
      j <- 1
      
      if (type == "traj") {
        while (j <= max(trajectory.data$frame)) {
          png(
            file.path(to.data, temp.overlay.folder, file_names[i], paste("frame_", j, ".png")), 
            width = as.numeric(width), 
            height = as.numeric(height), 
            bg = "transparent"   # quality = 100
          )
          par( mar = rep(0, 4), xaxs = c("i"), yaxs = c("i") )
          
          if (predict_spec == FALSE) {
            print <- subset(
              trajectory.data_tmp, 
              trajectory.data_tmp$frame <= j, 
              select = c("X", "Y", "trajectory")
            )
            
            ## plot the particle(s) so long as there are some
            if (length(print[, 1]) != 0) {
              plot(
                print$X, 
                print$Y, 
                xlim = c(0, as.numeric(width)), 
                ylim = c(as.numeric(height), 0), 
                col = "blue", 
                pch = 15, 
                cex = 1, 
                asp = 1
              )
            }
            
            ## otherwise just plot the empty frame
            if (length(print[, 1]) == 0) {
              plot(
                NA, 
                NA, 
                xlim = c(0, as.numeric(width)), 
                ylim = c(as.numeric(height), 0), 
                col = "blue", 
                pch = 1, 
                cex = 6, 
                asp = 1
              )
            }
          } else { # if (predict_spec == TRUE) {
            
            print <- subset(
              trajectory.data_tmp, 
              trajectory.data_tmp$frame <= j, 
              select = c("X", "Y", "trajectory", predict_spec_field)
            )
            
            ## plot the particle(s) so long as there are some
            if (length(print[, 1]) != 0) {
              plot(
                print$X, print$Y,
                xlim = c(0, as.numeric(width)),
                ylim = c(as.numeric(height), 0),
                col = as.factor(print[, ..predict_spec_field]),  #as.factor(print[predict_spec_field])
                pch = 15,
                cex = 1,
                asp = 1
              )
            }
            
            ## otherwise just plot the empty frame
            if (length(print[, 1]) == 0) {
              plot(
                NA, 
                NA, 
                xlim = c(0, as.numeric(width)), 
                ylim = c(as.numeric(height), 0), 
                col = "blue", 
                pch = 1, 
                cex = 1, 
                asp = 1
              )
            }
          }
            
          plot_crop_frame()

          dev.off()
          j <- j + 1
        }
      }
      
      if (type == "label") {
        while (j <= max(trajectory.data$frame)) {
          
          png(
            file.path(to.data, temp.overlay.folder, file_names[i], paste0("frame_", j, ".png")), 
            width = as.numeric(width), 
            height = as.numeric(height), 
            bg = "transparent"   # quality = 100
          )
          par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
          
          if (predict_spec == FALSE) {
            print <- subset(trajectory.data_tmp, trajectory.data_tmp$frame == j, select = c("X", "Y", "trajectory"))
            
            ## plot the particle(s) so long as there are some
            if (length(print[, trajectory, ]) != 0) {
              plot(
                print$X, 
                print$Y, 
                xlim = c(0, as.numeric(width)), 
                ylim = c(as.numeric(height), 0), 
                col = "blue", 
                pch = 1, 
                lwd = 4,
                cex = 6, 
                asp = 1
              )
              text(
                print$X, 
                print$Y - 20, 
                print$trajectory, 
                cex = 2, 
                lwd = 4,
                col = "red"
              )
            }
            
            ## otherwise just plot the empty frame
            if (length(print[, trajectory, ]) == 0) {
              plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
            }
          } else { # if (predict_spec == TRUE){
            
            print <- subset(
              trajectory.data_tmp, 
              trajectory.data_tmp$frame == j, 
              select = c("X", "Y", "trajectory", predict_spec_field)
            )
            
            ## plot the particle(s) so long as there are some
            if (length(print[, trajectory, ]) != 0) {
              plot(
                print$X,
                print$Y,
                xlim = c(0, as.numeric(width)),
                ylim = c(as.numeric(height), 0),
                col = as.factor(print[, ..predict_spec_field][[1]]),   # as.factor(print[predict_spec_field]),
                pch = 1,
                lwd = 4,
                cex = 6,
                asp = 1
              )
              text(
                print$X,
                print$Y - 20, 
                print$trajectory, 
                lwd = 4,
                cex = 2, 
                col = as.numeric(print[, ..predict_spec_field][[1]])   # as.numeric(print[predict_spec_field])
              )
            }
            
            ## otherwise just plot the empty frame
            if (length(print[, trajectory, ]) == 0) {
              plot(
                NA, 
                NA,
                xlim = c(0, as.numeric(width)), 
                ylim = c(as.numeric(height), 0), 
                col = "blue", 
                pch = 1,
                cex = 6, 
                asp = 1
              )
            }
          }
          
          plot_crop_frame()
          
          dev.off()
          j <- j + 1
        }
      }
      

      args <- paste0(
        "-start_number 1 ",
        "-framerate 10 ",
        "-i '", file.path(to.data, temp.overlay.folder, file_names[i], "frame_%d.png"), "' ",
        "-vcodec png ", 
        "'", file.path(to.data, temp.overlay.folder, file_names[i], "overlay.avi"), "'"
      )
            
      system2(
        command = ffmpeg, 
        args = args
      )
      

      # Create overlay ----------------------------------------------------------


      args <- paste0(
        "-i '", file.path(raw.video.folder, paste0(file_names[i], ".avi")), "' ",
        "-i '", file.path(to.data, temp.overlay.folder, file_names[i], "overlay.avi"), "' ",
        "-filter_complex 'overlay=x=0:y=0' ",
        "'", file.path(to.data, par_overlay.folder(), paste0(file_names[i], "_overlay.avi")), "' "
      )
      
      system2(
        command = ffmpeg, 
        args = args
      )
      
    }
  )
  
}
