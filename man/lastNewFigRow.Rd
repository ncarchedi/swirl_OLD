\name{lastNewFigRow}
\alias{lastNewFigRow}
\title{Finds row of last "new" figure}
\usage{
  lastNewFigRow(content.table, row.start)
}
\arguments{
  \item{content.table}{data.frame containing a complete
  swirl module}

  \item{row.start}{numeric specifying the row that the user
  is starting on}
}
\description{
  Finds the row number of the last "new" figure in the
  specified module. Used for when a user is resuming
  progress on a partially completed module and a previous
  figure needs to be loaded. It must be "new" since any
  "addition" figures expect an existing "new" figure is
  already in place.
}

