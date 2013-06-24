\name{userInput}
\alias{userInput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
userInput(question, type = c("exact", "range", "text", "command", "multiple"), choices = "", correct, hint = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{question}{
%%     ~~Describe \code{question} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{choices}{
%%     ~~Describe \code{choices} here~~
}
  \item{correct}{
%%     ~~Describe \code{correct} here~~
}
  \item{hint}{
%%     ~~Describe \code{hint} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (question, type = c("exact", "range", "text", "command", 
    "multiple"), choices = "", correct, hint = "") 
{
    cat("\n", question, sep = "")
    if (type == "exact") {
        repeat {
            cat("\n\n")
            str.ans <- readline("ANSWER: ")
            if (str.ans == "Swirl" | str.ans == "swirl()") {
                tryAgain(hint)
                recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                cat("\n\n", question, sep = "")
            }
            else if (str.ans != "") {
                recordString(my.string = str.ans, text.file.name = progress.file.name)
                eval.ans <- tryCatch(expr = as.double(eval(parse(text = str.ans))), 
                  error = function(e) {
                    cat("\nNot a valid input!\n")
                    return(-99999)
                  })
                if (!identical(eval.ans, -99999) & !grepl("<-", 
                  str.ans)) {
                  eval.ans <- round(eval.ans, 5)
                  print(eval.ans)
                }
                correct <- round(as.double(correct), 5)
                if (identical(eval.ans, correct)) {
                  praise()
                  recordIsCorrect(is.correct = TRUE, text.file.name = progress.file.name)
                  break
                }
                else {
                  tryAgain(hint)
                  recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                }
            }
            else {
                cat("\nPlease enter a response!")
            }
        }
    }
    else if (type == "range") {
        repeat {
            cat("\n\n")
            str.ans <- readline("ANSWER: ")
            if (str.ans == "Swirl" | str.ans == "swirl()") {
                tryAgain(hint)
                recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                cat("\n\n", question, sep = "")
            }
            else if (str.ans != "") {
                recordString(my.string = str.ans, text.file.name = progress.file.name)
                num.ans <- tryCatch(expr = as.numeric(eval(parse(text = str.ans))), 
                  error = function(e) {
                    cat("\nNot a valid input!\n")
                    return(-99999)
                  }, warning = function(w) {
                    cat("\nNot a valid input!\n")
                    return(-99999)
                  })
                if (!identical(num.ans, -99999) & !grepl("<-", 
                  str.ans)) {
                  print(num.ans)
                }
                correct <- as.numeric(correct)
                if (num.ans >= correct[1] && num.ans <= correct[2]) {
                  praise()
                  recordIsCorrect(is.correct = TRUE, text.file.name = progress.file.name)
                  break
                }
                else {
                  tryAgain(hint)
                  recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                }
            }
            else {
                cat("\nPlease enter a response!")
            }
        }
    }
    else if (type == "text") {
        repeat {
            cat("\n\n")
            str.ans <- readline("ANSWER: ")
            if (str.ans == "Swirl" | str.ans == "swirl()") {
                tryAgain(hint)
                recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                cat("\n\n", question, sep = "")
            }
            else if (str.ans != "") {
                recordString(my.string = str.ans, text.file.name = progress.file.name)
                lower.ans <- tryCatch(expr = tolower(str.ans), 
                  error = function(e) {
                    cat("\nNot a valid input!\n")
                    return(-99999)
                  })
                correct <- tolower(correct)
                if (lower.ans \%in\% correct) {
                  praise()
                  recordIsCorrect(is.correct = TRUE, text.file.name = progress.file.name)
                  break
                }
                else {
                  tryAgain(hint)
                  recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                }
            }
            else {
                cat("\nPlease enter a response!")
            }
        }
    }
    else if (type == "command") {
        repeat {
            cat("\n\n")
            str.ans <- readline("ANSWER: ")
            if (str.ans == "Swirl" | str.ans == "swirl()") {
                tryAgain(hint)
                recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                cat("\n\n", question, sep = "")
            }
            else if (str.ans != "") {
                recordString(my.string = str.ans, text.file.name = progress.file.name)
                eval.ans <- tryCatch(expr = eval(parse(text = str.ans)), 
                  error = function(e) {
                    cat("\nNot a valid input!\n")
                    return(-99999)
                  })
                if (!identical(eval.ans, -99999)) {
                  if (grepl("<-", str.ans)) {
                    new.str.ans <- sub("<-", "<<-", str.ans)
                  }
                  else {
                    print(eval(parse(text = str.ans)))
                    new.str.ans <- str.ans
                  }
                }
                if (identical(str.ans, correct)) {
                  eval(parse(text = new.str.ans))
                  praise()
                  recordIsCorrect(is.correct = TRUE, text.file.name = progress.file.name)
                  break
                }
                else {
                  tryAgain(hint)
                  recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
                }
            }
            else {
                cat("\nPlease enter a response!")
            }
        }
    }
    else if (type == "multiple") {
        repeat {
            cat("\n")
            str.ans <- select.list(choices = choices)
            recordString(my.string = str.ans, text.file.name = progress.file.name)
            str.ans <- tolower(str.ans)
            correct <- tolower(correct)
            if (identical(str.ans, correct)) {
                praise()
                recordIsCorrect(is.correct = TRUE, text.file.name = progress.file.name)
                break
            }
            else {
                tryAgain(hint)
                recordIsCorrect(is.correct = FALSE, text.file.name = progress.file.name)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
