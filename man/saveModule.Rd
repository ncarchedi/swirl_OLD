\name{saveModule}
\alias{saveModule}
\title{Saves module to package}
\usage{
  saveModule(moduleName, excelDir, courseDirectory)
}
\arguments{
  \item{moduleName}{character vector of module names (e.g.
  "Module3")}

  \item{excelDir}{full path to directory where Excel
  spreadsheets are stored}

  \item{courseDirectory}{full path to course directory
  within package directory}
}
\description{
  Loads module content and info directly from Excel
  spreadsheets (xlsx) based on specified module name(s) and
  directory and saves content and info to two data frames
  in a single rda file for each module. The rda file is
  saved in the designated course directory.
}

