#' Function to make overlays using the subtitle files,
#'
#' Function creates subtitle commands for every cell in every frame of the videos, using the x/y coordinates.
#' Then ffmpeg is called to burn the overlay subtitles on, and save a compressed video.
#' @param to.data path to the working directory
#' @param traj.data filtered trajectory file
#' @param raw.avi.folder path to the folder containing the converted and compressed .avi files
#' @param temp.overlay.folder  temporary directory to save the overlay subtitles (.ssa files)
#' @param overlay.folder directory where the overlay videos are saved
#' @param fps framerate in the original video
#' @param vid.length duration in seconds of the video
#' @param width width in pixels of the original videos
#' @param height height in pixels of the original video
#' @param tools.path path to the dependency folder
#' @param overlay.type option for the overlays. Overlays can either be shown as
#' trajectory numbers ("number"), circle ("circle") or both ("both"). Defaults to "both".
#' @return returns nothing (NULL)
#' @importFrom data.table fwrite
#' @export

create_overlays_subtitle <- function(
  to.data = par_to.data(),
  merged.data.folder = par_merged.data.folder(),
  avi.video.folder = par_raw.video.folder(),
  temp.overlay.folder = par_temp.overlay.folder(),
  overlay.folder = par_overlay.folder(),
#  width
# height
  difference.lag = par_difference.lag(),
#  type = "traj",
#  predict_spec = FALSE,
  ffmpeg = "ffmpeg",
  ## new arguments
  master = par_master(),
  overlay.type = "both",
  mc.cores = par_mc.cores()
) {

  # Check if overlay type is valid
  if (!(overlay.type %in% c("circle", "number", "both"))) {
    stop("Wrong overlay type specified. Please choose 'circle', 'number', or 'both'")
  }


  #Check if overlay type is valid
  if (! overlay.type %in% c("circle", "number", "both")) {
    stop("Wrong overlay type specified. Please choose 'circle', 'number', or 'both'")
  }

  traj.data <- readRDS(file = file.path(to.data, merged.data.folder, master))
  avi_files <- unique(traj.data$file)

  # get fps for all avi files and add to traj.data
  fps <- get_fps_avi(file.path(avi.video.folder, paste0(avi_files, ".avi")), mc.cores = mc.cores)
  fps <- data.frame(
    file = gsub(pattern = "\\.avi$", "", names(fps)),
    fps = fps
  )
  row.names(fps) <- NULL
  traj.data <- merge(
    traj.data,
    fps,
    by = "file",
    all.x = TRUE,
    all.y = FALSE
  )
  rm(fps)

  # get duration for all avi files and add to traj.data
  duration <- get_duration_avi(file.path(avi.video.folder, paste0(avi_files, ".avi")), mc.cores = mc.cores)
  duration <- data.frame(
    file = gsub(pattern = "\\.avi$", "", names(duration)),
    duration = duration
  )
  row.names(duration) <- NULL
  traj.data <- merge(
    traj.data,
    duration,
    by = "file",
    all.x = TRUE,
    all.y = FALSE
  )
  rm(duration)

  # get width for all avi files and add to traj.data
  width <- get_width_avi(file.path(avi.video.folder, paste0(avi_files, ".avi")), mc.cores = mc.cores)
  width <- data.frame(
    file = gsub(pattern = "\\.avi$", "", names(width)),
    width = width
  )
  row.names(width) <- NULL
  traj.data <- merge(
    traj.data,
    width,
    by = "file",
    all.x = TRUE,
    all.y = FALSE
  )
  rm(width)

  # get height for all avi files and add to traj.data
  height <- get_height_avi(file.path(avi.video.folder, paste0(avi_files, ".avi")), mc.cores = 6)
  height <- data.frame(
    file = gsub(pattern = "\\.avi$", "", names(height)),
    height = height
  )
  row.names(height) <- NULL
  traj.data <- merge(
    traj.data,
    height,
    by = "file",
    all.x = TRUE,
    all.y = FALSE
  )
  rm(height)


  # Define in the filtered trajectory data the start and end time of each observation,
  # as well as the numeric ID of the observation
  traj.data$starttime <- traj.data$frame * (1 / traj.data$fps)
  traj.data$endtime <- traj.data$starttime + (1 / traj.data$fps)
  traj.data$ID.vid <- traj.data$trajectory

  #  Generate the header for the subtitle file
  traj.data$header <- paste(
    "[Script Info]", "ScriptType: v4.00+", "Collisions: Normal",
    paste0("PlayResX: ", traj.data$width),
    paste0("PlayResY: ", traj.data$height),
    paste0("Timer: ", traj.data$duration),
    "",
    "[V4+ Styles]",
    "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, ",
    " Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle,",
    " BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding",
    "Style: Numbers,Arial,24,65535,65535,65535,65535,0,0,0,0,100,100,0,0.00,1,1,0,2,0,0,0,0",
    "Style: Circle,Arial,80,&H0000FF,&H0000FF,&H0000FF,&H0000FF,-1,0,0,0,100,100,0,0.00,1,1,0,2,0,0,0,0",
    "",
    "[Events]",
    "Format: Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text",
    sep = "\n"
  )

  # Generate a subtitle line for each observation
  traj.data$subtitle_number <- paste0(
    "Dialogue: 2,0:00:", sprintf("%05.2f", traj.data$starttime), ",0:00:", sprintf("%05.2f", traj.data$endtime),
    ",Numbers,,0000,0000,0000,,{\\pos(", abs(round(traj.data$X)), ", ", abs(round(traj.data$Y)), ")}", traj.data$ID.vid
  )
  traj.data$subtitle_circle <- paste0(
    "Dialogue: 1,0:00:", sprintf("%05.2f", traj.data$starttime), ",0:00:", sprintf("%05.2f", traj.data$endtime),
    ",Circle,,0000,0000,0000,,{\\pos(", abs(round(traj.data$X)), ", ",
    ifelse(
      abs(round(traj.data$Y)) < traj.data$height,
      abs(round(traj.data$Y)) + 40,
      abs(round(traj.data$Y))
    ), ")}", "O"
  )

  # Make the folder to store the subtitle files, and generate the subtitle file for each file with observed cells
  dir.create(file.path(to.data, temp.overlay.folder), showWarnings = FALSE)
  parallel::mclapply(
    unique(traj.data$file),
    function(i) {
      traj_id <- which(traj.data$file == i)
      if (overlay.type == "circle") {
        ssa <- c(
          traj.data[traj_id[[1]], "header"][[1]],
          unlist(traj.data[traj_id, "subtitle_circle"])
        )
      } else if (overlay.type == "number") {
        ssa <- c(
          traj.data[traj_id[[1]], "header"][[1]],
          unlist(traj.data[traj_id, "subtitle_number"])
        )
      } else if (overlay.type == "both") {
        ssa <- c(
          traj.data[traj_id[[1]], "header"][[1]],
          unlist(traj.data[traj_id, "subtitle_number"]),
          unlist(traj.data[traj_id, "subtitle_circle"])
        )
      }
      writeLines(
        ssa,
        file.path(to.data, temp.overlay.folder, paste0(i, ".ssa"))
      )
    },
    mc.cores = mc.cores
  )

  # Create a folder to store the overlay videos
  dir.create(file.path(to.data, overlay.folder), showWarnings = FALSE)

  # For each of the files with observed cells,
  # burn the subtitles onto the avi file,
  # and store the resulting file in the overlay folder
  for (file in unique(traj.data$file)) {
    avi_in <- normalizePath(
      file.path(avi.video.folder, paste0(file, ".avi"))
    )
    ssa_in <-  normalizePath(
      file.path(to.data, temp.overlay.folder, paste0(file, ".ssa"))
    )
    avi_out <-  normalizePath(
      file.path(to.data, overlay.folder, paste0(file, ".avi")),
      mustWork = FALSE
    )
    arguments <- paste0(
      " -i '", avi_in, "'",
      " -vf 'ass=", ssa_in, "'",
      " -b:v 50M",
      " -c:a copy",
      " -y '", avi_out, "'"
    )
    system2(
      command = ffmpeg,
      args = arguments
    )
  }

  return(NULL)
}
