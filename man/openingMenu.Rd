\name{openingMenu}
\alias{openingMenu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
openingMenu()
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
    cat("\nWelcome! My name is Swirl and I'll be your host today! Please select the option below that applies to you:\n")
    status.choices <- c("I'm an existing user!", "This is my first time!", 
        "I'm just a boring admin...")
    status <- select.list(choices = status.choices)
    if (status == status.choices[1]) {
        cat("\nGreat! What's your email address?")
        email <- readline("\nEMAIL: ")
        username <- strsplit(email, split = "@")[[1]][1]
        user.files <- getUserFileNames(username)
        user.file.name <- user.files[[1]]
        progress.file.name <- user.files[[2]]
        if (all(file.exists(user.file.name, progress.file.name))) {
            user.info <- scan(file = user.file.name, what = character(), 
                quiet = TRUE)
            user.name <- user.info[1]
            user.email <- user.info[2]
            cat("\nWelcome back, ", user.name, "! It's great to see you again. Would you like to begin where you left off?\n\n", 
                sep = "")
            resp <- readline("ANSWER: ")
            if (isYes(resp)) {
                progress <- scan(file = progress.file.name, what = character(), 
                  quiet = TRUE)
                module.start <- progress[1]
                index <- max(grep("row", progress))
                row.as.string <- progress[index]
                row.start <- as.numeric(gsub("\\D", "", row.as.string))
            }
            else {
                cat("\nAre you sure you'd like to start over and lose any progress you've made?\n\n")
                resp <- readline("ANSWER: ")
                if (isYes(resp)) {
                  module.start <- "Module1"
                  row.start <- 1
                }
                else {
                  cat("\nOkay, then we'll start where you left off!\n")
                  progress <- scan(file = progress.file.name, 
                    what = character(), quiet = TRUE)
                  module.start <- progress[1]
                  index <- max(grep("row", progress))
                  row.as.string <- progress[index]
                  row.start <- as.numeric(gsub("\\D", "", row.as.string))
                }
            }
        }
        else {
            cat("\nI'm sorry, but for some reason, I can't find your records. You'll have to log in as a new user and start from the beginning.\n")
            start <- openingMenu()
            module.start <- start[[1]]
            row.start <- start[[2]]
            user.files <- start[[3]]
        }
    }
    else if (status == status.choices[2]) {
        module.start <- "Module1"
        row.start <- 1
        user.files <- storeUserInfo()
        cat("\nFirst off, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue. Also, as you've probably figured out, when you see 'ANSWER:', that means it's your turn to enter a response, then press Enter to continue.")
        cat("\n\nFinally, you'll notice a progress bar that shows you how far you've made it through the lesson. Remember you can stop at any time by pressing the Esc key and you're progress will be saved. Let's get started!")
        readline("\n...  <-- That's your cue to press Enter to continue")
    }
    else if (status == status.choices[3]) {
        cat("\nPlease enter the admin password for top secret access.")
        password <- readline("\nPASSWORD: ")
        if (password == "swirladmin") {
            cat("\nWelcome, Mr. or Mrs. Important. On which module would you like to begin?")
            module.start <- readline("\nANSWER: ")
            cat("\nAnd which row of the content table would you like to start on?")
            row.start <- as.numeric(readline("\nANSWER: "))
            username <- "admin"
            user.files <- getUserFileNames(username)
        }
        else {
            cat("\nSorry, wrong password!\n")
            start <- openingMenu()
            module.start <- start[[1]]
            row.start <- start[[2]]
            user.files <- start[[3]]
        }
    }
    return(list(module.start, row.start, user.files))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
