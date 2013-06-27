\name{userInput}
\alias{userInput}
\title{Gets user input}
\usage{
  userInput(question,
    type = c("exact", "range", "text", "command", "multiple"),
    choices = "", correct, hint = "", progress.file.path)
}
\arguments{
  \item{question}{character string representing the
  question body}

  \item{type}{character string specifying the type of
  question}

  \item{choices}{character vector of choices, only
  specified for question type "multiple"}

  \item{correct}{character string representing the correct
  answer}

  \item{hint}{character string of hint to be shown to user
  after incorrect response}

  \item{progress.file.path}{full file path to file where
  progress is recorded}
}
\description{
  Solicits a response from user depending on the type of
  question specified. Also writes corresponding progress
  data to progress file.
}

