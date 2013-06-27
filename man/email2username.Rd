\name{email2username}
\alias{email2username}
\title{Gets username from email address}
\usage{
  email2username(email)
}
\arguments{
  \item{email}{character string representing a valid email
  address}
}
\value{
  Character string containing the portion of the email
  address prior to the "at" symbol
}
\description{
  Extracts a swirl username from a given email address.
  Does not validate format of email address.
}

