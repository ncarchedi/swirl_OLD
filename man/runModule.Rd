\name{runModule}
\alias{runModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runModule(module.dir, module.name, row.start)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module.dir}{
%%     ~~Describe \code{module.dir} here~~
}
  \item{module.name}{
%%     ~~Describe \code{module.name} here~~
}
  \item{row.start}{
%%     ~~Describe \code{row.start} here~~
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
function (module.dir, module.name, row.start) 
{
    mod.info.path <- file.path(module.dir, paste(module.name, 
        "_info.csv", sep = ""))
    mod.content.path <- file.path(module.dir, paste(module.name, 
        ".csv", sep = ""))
    mod.info <- read.csv(file = mod.info.path, colClasses = "character", 
        header = FALSE)[1:5, 1:2]
    packages.as.chars <- unlist(strsplit(mod.info[4, 2], ", ", 
        fixed = T))
    for (package in packages.as.chars) {
        suppressWarnings(suppressPackageStartupMessages(require(package, 
            character.only = TRUE)))
    }
    datasets.as.chars <- unlist(strsplit(mod.info[5, 2], ", ", 
        fixed = T))
    data(list = datasets.as.chars, envir = .GlobalEnv)
    mod <- read.csv(file = mod.content.path, colClasses = "character")[, 
        1:9]
    last.row <- max(which(mod$Output.Type != ""))
    mod <- mod[1:last.row, ]
    loc <- grep("<-", mod$Correct.Answer)
    if (length(loc) > 0) {
        for (row in loc) {
            if (row.start > row) {
                assignment <- scan(text = mod$Correct.Answer[row], 
                  what = character(), sep = "\n", quiet = TRUE)
                new.str.ans <- sub("<-", "<<-", assignment)
                eval(parse(text = new.str.ans))
            }
        }
    }
    if (mod$Figure[row.start] == "" | mod$Figure.Type[row.start] == 
        "addition") {
        fig.loc <- which(mod$Figure != "")
        if (length(fig.loc) > 0) {
            if (row.start > min(fig.loc)) {
                new.plot.row <- lastNewFigRow(mod, row.start)
                sub.fig.loc <- fig.loc[fig.loc >= new.plot.row & 
                  fig.loc <= row.start]
                for (row in sub.fig.loc) {
                  fig.path <- file.path(module.dir, "Figures", 
                    mod$Figure[row])
                  source(file = fig.path, local = TRUE)
                }
                cat("\nI'm displaying the previous plot in case you need to refer back to it.\n")
            }
        }
    }
    if (row.start == 1) {
        cat(module.name, "\n", file = progress.file.name)
    }
    for (i in row.start:nrow(mod)) {
        cat("row", i, "\n", sep = "", file = progress.file.name, 
            append = TRUE)
        cat("output.type", mod$Output.Type[i], "\n", sep = " ", 
            file = progress.file.name, append = TRUE)
        suppress.progress.bar <- 0
        if (mod$Output.Type[i] == "text") {
            cat("\n", mod$Output[i], sep = "")
            readline("\n...")
        }
        else if (mod$Output.Type[i] == "question") {
            q <- mod$Output[i]
            ans.type <- mod$Question.Type[i]
            ch <- scan(text = mod$Choices[i], what = character(), 
                sep = "\n", quiet = TRUE)
            correct.ans <- scan(text = mod$Correct.Answer[i], 
                what = character(), sep = "\n", quiet = TRUE)
            h <- mod$Hint[i]
            ptm <- proc.time()
            userInput(question = q, type = ans.type, choices = ch, 
                correct = correct.ans, hint = h)
            time.on.question <- proc.time() - ptm
            cat("time.on.question", time.on.question[3], "\n", 
                sep = " ", file = progress.file.name, append = TRUE)
        }
        else if (mod$Output.Type[i] == "figure") {
            cat("\n", mod$Output[i], sep = "")
            fig.path <- file.path(module.dir, "Figures", mod$Figure[i])
            source(file = fig.path, local = TRUE)
            readline("\n...")
        }
        else if (mod$Output.Type[i] == "video") {
            cat("\n", mod$Output[i], "\n\n", sep = "")
            resp <- readline("ANSWER: ")
            if (isYes(response = resp)) {
                browseURL(mod$Video[i])
                readline("\nPress Enter when you are done watching...")
            }
            suppress.progress.bar <- 1
        }
        if (suppress.progress.bar == 0) 
            progressMarkers(current.row = i, total.rows = nrow(mod))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
