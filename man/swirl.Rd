\name{swirl}
\alias{swirl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
swirl()
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
    module.dir <- file.path(path.package("swirl"), "Open_Intro_Modules")
    start <- openingMenu()
    module.start <- start[[1]]
    row.start <- start[[2]]
    files <- unlist(start[[3]])
    user.info.file.name <- files[[1]]
    progress.file.name <- files[[2]]
    master.module.list <- list("Module1")
    mod.num <- which(master.module.list == module.start)
    for (i in mod.num:length(master.module.list)) {
        module.name <- master.module.list[[i]]
        runModule(module.dir, module.name, row.start)
        row.start <- 1
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
