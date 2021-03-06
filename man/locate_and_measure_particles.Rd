% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Locate_and_measure_particles.R
\name{locate_and_measure_particles}
\alias{locate_and_measure_particles}
\title{Function to extract morphological measurements and X- and Y-coordinates for moving particles}
\usage{
locate_and_measure_particles(
  to.data = par_to.data(),
  raw.video.folder = par_raw.video.folder(),
  particle.data.folder = par_particle.data.folder(),
  difference.lag = par_difference.lag(),
  min_size = par_min_size(),
  max_size = par_max_size(),
  thresholds = par_thresholds(),
  crop_pixels = par_crop_pixels(),
  IJ.path = par_IJ.path(),
  memory = par_memory(),
  ijmacs.folder = par_ijmacs.folder(),
  pixel_to_scale = par_pixel_to_scale(),
  java.path = par_java.path()
)
}
\arguments{
\item{to.data}{path to the working directory}

\item{raw.video.folder}{directory with the raw video files}

\item{particle.data.folder}{directory to which the data is saved as a text file}

\item{difference.lag}{numeric value specifying the offset between two video frames to 
compute the difference image. If 0, then no differencing applied.}

\item{min_size}{minimum size for detection of particles}

\item{max_size}{maximum size for detection of particles}

\item{thresholds}{vector containing the min and max threshold values (defaults to c(10,255))}

\item{crop_pixels}{pixels to which the particle data should be cropped}

\item{IJ.path}{path to ImageJ folder, containing the 'ij.jar' executable}

\item{memory}{numeric value specifying the amount of memory available to ImageJ (defaults to 512)}

\item{ijmacs.folder}{directory for the macro to for ImageJ}

\item{pixel_to_scale}{TODO}
}
\value{
saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory and then assembles the data into a single database 
called 'particle.rds'. This data.frame contains information about the following properties: the area (transversal cut), 
the mean, minimum and maximum of the grey value, the perimeter, width, length and angle with the dominant-axis of a fitted ellipse, and finally shape parameters such as
circularity, aspect ratio, roundness and solidity. For details of the morphological output, please refer to http://rsbweb.nih.gov/ij/docs/guide/146-30.html
}
\description{
Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
}
