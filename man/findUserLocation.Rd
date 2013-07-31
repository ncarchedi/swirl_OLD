\name{findUserLocation}
\alias{findUserLocation}
\title{Find current course, module, and row}
\usage{
  findUserLocation(progress.file.path)
}
\arguments{
  \item{progressFilePath}{Complete file path for user's
  progress file}
}
\value{
  currentLocation List containing current course directory
  name, course name, module, and row, respectively
}
\description{
  Given the file path for user's progress file, this
  function returns a list containing the current course,
  module, and row of content for that user.
}

