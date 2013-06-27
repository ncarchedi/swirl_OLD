\name{getUserFileNames}
\alias{getUserFileNames}
\title{Gets user file names}
\usage{
  getUserFileNames(username)
}
\arguments{
  \item{username}{character string containing valid swirl
  username, which is just the part of an email address
  before the "at" symbol.}
}
\value{
  List containing the expected full file paths of the user
  info file and user progress file, respectively
}
\description{
  Takes username as input and returns a list containing the
  appropriate user file names. The function does not
  attempt to verify whether these files exist.
}

