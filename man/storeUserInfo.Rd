\name{storeUserInfo}
\alias{storeUserInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
storeUserInfo()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    cat("\nI'm really glad to have you on board. We'll be spending quite a bit of time together, so we should get to know each other before we dive in. What's your name?\n\n")
    name <- readline("NAME: ")
    cat("\nIt's a pleasure to meet you, ", name, ". ", sep = "")
    cat("What's a good email address for you? I promise not to send you any junk.\n\n")
    email <- readline("EMAIL: ")
    repeat {
        cat("\nThanks, ", name, ". I have your email address as ", 
            email, ". Is this correct?\n\n", sep = "")
        confirm.email <- readline("ANSWER: ")
        if (isYes(confirm.email)) {
            cat("\nGreat! Let's cover a couple of quick housekeeping items before we begin our first lesson.\n")
            break
        }
        else {
            cat("\nNo problem. Let's try again. What is your correct email address?\n\n")
            email <- readline("EMAIL: ")
        }
    }
    username <- unlist(strsplit(email, split = "@"))[1]
    user.info.file.name <- file.path(path.package("swirl"), "user_data", 
        paste(username, "_info.txt", sep = ""))
    cat(name, email, sep = "\n", file = user.info.file.name)
    progress.file.name <- file.path(path.package("swirl"), "user_data", 
        paste(username, "_progress.txt", sep = ""))
    return(list(user.info.file.name, progress.file.name))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
