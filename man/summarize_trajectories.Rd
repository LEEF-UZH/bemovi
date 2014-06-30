\name{summarize_trajectories}
\alias{summarize_trajectories}
\title{Function to summarize the mean morphology and movement and its variability on a trajectory level}
\usage{
summarize_trajectories(data)
}
\arguments{
  \item{data}{Dataframe with the information on morphology
  and movement for each frame}
}
\value{
Saves the aggregated information on each trajectory to the
disk
}
\description{
Takes the data comprising the information for each frame
and calculates mean and sd along the trajectory
}
