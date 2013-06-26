### HELPER FUNCTIONS

recordString <- function(my.string, text.file.path) {
  # Function to record (appends) a string to designated text file
  # Used for recording user responses in progress files
  cat("user.answer", my.string, "\n", file=text.file.path, append=TRUE)
}

recordIsCorrect <- function(is.correct, text.file.path) {
  # Function to record TRUE or FALSE to a text file
  # Used to denote whether user's answer is right or wrong
  cat("is.correct", is.correct, "\n", file=text.file.path, append=TRUE)
}

userInput <- function(question, type=c("exact", "range", "text", "command", "multiple"), 
                      choices="", correct, hint="", progress.file.path) {
  # Accepts the correct answer, type of answer, and a hint
  # as arguments and executes the appropriate input sequence
  
  # Print question
  cat("\n", question, sep="")
  
  ### For exact answer type
  if(type=="exact") {
    repeat {
      cat("\n\n")
      str.ans <- readline("ANSWER: ")
      
      # First make sure the user does not enter 'Swirl' or 'swirl()'
      if(str.ans == "Swirl" | str.ans == "swirl()") {
        tryAgain(hint)
        recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        cat("\n\n", question, sep="")
        
      } else if(str.ans != "") {
        recordString(my.string=str.ans, text.file.path=progress.file.path)
        eval.ans <- tryCatch(expr=as.double(eval(parse(text=str.ans))),
                             error=function(e) {
                               cat("\nNot a valid input!\n")
                               return(-99999)
                             }
        )
        if(!identical(eval.ans, -99999) & !grepl("<-", str.ans)) {
          eval.ans <- round(eval.ans, 5)
          print(eval.ans)  # Print user answer
        }
        correct <- round(as.double(correct), 5)
        if(identical(eval.ans, correct)) {
          praise()
          recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
          break
        } else {
          tryAgain(hint)
          recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        }
      } else {
        cat("\nPlease enter a response!")
      }
    }
  } else if(type=="range") {  ### For range answer type
    repeat {
      cat("\n\n")
      str.ans <- readline("ANSWER: ")
      
      # First make sure the user does not enter 'Swirl' or 'swirl()'
      if(str.ans == "Swirl" | str.ans == "swirl()") {
        tryAgain(hint)
        recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        cat("\n\n", question, sep="")
        
      } else if(str.ans != "") {
        recordString(my.string=str.ans, text.file.path=progress.file.path)
        num.ans <- tryCatch(expr=as.numeric(eval(parse(text=str.ans))),
                            error=function(e) {
                              cat("\nNot a valid input!\n")
                              return(-99999)
                            },
                            warning=function(w) {
                              cat("\nNot a valid input!\n")
                              return(-99999)
                            }
        )
        if(!identical(num.ans, -99999) & !grepl("<-", str.ans)) {
          print(num.ans)  # Print user answer
        }
        correct <- as.numeric(correct)
        if(num.ans >= correct[1] && num.ans <= correct[2]) {
          praise()
          recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
          break
        } else {
          tryAgain(hint)
          recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        }
      } else {
        cat("\nPlease enter a response!")
      }
    }
  } else if(type=="text") {  ### For text answer type
    repeat {
      cat("\n\n")
      str.ans <- readline("ANSWER: ")
      
      # First make sure the user does not enter 'Swirl' or 'swirl()'
      if(str.ans == "Swirl" | str.ans == "swirl()") {
        tryAgain(hint)
        recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        cat("\n\n", question, sep="")
        
      } else if(str.ans != "") {
        recordString(my.string=str.ans, text.file.path=progress.file.path)
        lower.ans <- tryCatch(expr=tolower(str.ans),
                              error=function(e) {
                                cat("\nNot a valid input!\n")
                                return(-99999)
                              }
        )
        correct <- tolower(correct)
        if(lower.ans %in% correct) {
          praise()
          recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
          break
        } else {
          tryAgain(hint)
          recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        }
      } else {
        cat("\nPlease enter a response!")
      }
    }
  } else if(type=="command") {  ### For command answer type
    repeat {
      cat("\n\n")
      str.ans <- readline("ANSWER: ")
      
      # First make sure the user does not enter 'Swirl' or 'swirl()'
      if(str.ans == "Swirl" | str.ans == "swirl()") {
        tryAgain(hint)
        recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        cat("\n\n", question, sep="")
        
      } else if(str.ans != "") {
        recordString(my.string=str.ans, text.file.path=progress.file.path)
        eval.ans <- tryCatch(expr=eval(parse(text=str.ans)),
                             error=function(e) {
                               cat("\nNot a valid input!\n")
                               return(-99999)
                             }
        )
        if(!identical(eval.ans, -99999)) {
          if(grepl("<-", str.ans)) {        
            new.str.ans <- sub("<-", "<<-", str.ans)
          } else {
            print(eval(parse(text=str.ans)))  # Print evaluated command since it is valid and isn't an assignment
            new.str.ans <- str.ans
          }
        }
        if(identical(str.ans, correct)) {
          eval(parse(text=new.str.ans))
          praise()
          recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
          break
        } else {
          tryAgain(hint)
          recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
        }
      } else {
        cat("\nPlease enter a response!")
      }
    }
  } else if(type=="multiple") {
    repeat {
      cat("\n")
      str.ans <- select.list(choices=choices)
      recordString(my.string=str.ans, text.file.path=progress.file.path)
      
      str.ans <- tolower(str.ans)
      correct <- tolower(correct)
      if(identical(str.ans, correct)) {
        praise()
        recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
        break
      } else {
        tryAgain(hint)
        recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
      }
    }
  }
}

isYes <- function(response) {
  # Define isYes function - accepts response to yes/no questions and 
  # returns TRUE if yes and FALSE if no
  yes <- NA
  if(!identical(response, character(0))) {
    if(tolower(substr(response, 1, 1)) == "y") {
      yes <- TRUE
    } else if(tolower(substr(response, 1, 1)) == "n") {
      yes <- FALSE
    }
  }    
  if(identical(yes, NA)) {
    cat("\nPlease type 'yes' or 'no'.\n\n")
    response <- readline("ANSWER: ")
    isYes(response)
  } else {
    return(yes)
  }
}

praise <- function() {
  # Prints random praise
  praise.options <- scan(file=file.path(path.package("swirl"), "praise.txt"), what=character(), quiet=TRUE)
  i <- sample(1:length(praise.options), 1)
  cat("\n", praise.options[[i]], "\n", sep="")
}

tryAgain <- function(hint="") {
  # Prints encouragement to try again with hint if desired
  tryagain.options <- scan(file=file.path(path.package("swirl"), "try_again.txt"), what=character(), quiet=TRUE)
  i <- sample(1:length(tryagain.options), 1)
  cat("\n", tryagain.options[[i]], " ", hint, sep="")
}

progressMarkers <- function(current.row, total.rows) {
  # Function to generate visual progress markers
  percent.complete <- round((current.row/total.rows)*100)
  cat("\n")
  if(total.rows >= 10) {
    total.marks <- 10
    ticks <- round(total.rows/total.marks)
    marks <- floor(current.row/ticks)
    cat("PROGRESS: << ", rep("(X) ", marks), rep("( ) ", total.marks-marks), ">> ", sep="")
  }
  cat("(", percent.complete, "% Complete)\n", sep="")
}

lastNewFigRow <- function(content.table, row.start) {
  # Function to find the row number of the last "new" figure in module
  fig.loc <- which(content.table$Figure != "")  # Find locations of figures in module
  last.fig.row <- fig.loc[max(which(fig.loc < row.start))]
  if(content.table$Figure.Type[last.fig.row]=="new") {  # If last figure is "new" then just source it
    return(last.fig.row)
  } else {  # If last figure is an addition, use recursion to find last "new" figure row
    lastNewFigRow(content.table, last.fig.row)
  }
}

storeUserInfo <- function() {
  # Function that stores first time user info in a new text file and returns location of file
  # Also creates progress file and returns location of file
  cat("\nI'm really glad to have you on board. We'll be spending quite a bit of time together, so we should get to know each other before we dive in. What's your name?\n\n")
  
  # Get user name
  name <- readline("NAME: ")
  cat("\nIt's a pleasure to meet you, ", name, ". ", sep="")
  
  # Get user email and confirm
  cat("What's a good email address for you? I promise not to send you any junk.\n\n")
  email <- readline("EMAIL: ")
  repeat {  
    cat("\nThanks, ", name, ". I have your email address as ", email, ". Is this correct?\n\n", sep="")
    confirm.email <- readline("ANSWER: ")
    if(isYes(confirm.email)) {
      break
    } else {
      cat("\nNo problem. Let's try again. What is your correct email address?\n\n")
      email <- readline("EMAIL: ")
    }
  }
  # Save user info to text file with unique name
  username <- email2username(email)
  user.info.file.path <- file.path(path.package("swirl"), "user_data", paste(username,"_info.txt", sep=""))
  cat(name, email, sep="\n", file=user.info.file.path)
  
  # Create new progress file for user
  progress.file.path <- file.path(path.package("swirl"), "user_data", paste(username,"_progress.txt", sep=""))
  
  # Return both file names
  return(list(user.info.file.path, progress.file.path))
}

getUserFileNames <- function(username) {
  # Takes username as input and returns a list containing the appropriate user file names
  user.info.file.path <- file.path(path.package("swirl"), "user_data", paste(username,"_info.txt", sep=""))
  progress.file.path <- file.path(path.package("swirl"), "user_data", paste(username,"_progress.txt", sep=""))
  return(list(user.info.file.path, progress.file.path))
}

runModule <- function(module.dir, module.name, row.start, progress.file.path) {
  ### Runs module given by module.name, beginning at row given by row.start
  
  # Determine file extention for current module info and content
  mod.info.path <- file.path(module.dir, paste(module.name, "_Info.csv", sep=""))
  mod.content.path <- file.path(module.dir, paste(module.name, ".csv", sep=""))
  
  # Read in module info
  mod.info <- read.csv(file=mod.info.path, colClasses="character",
                       header=FALSE)[1:5,1:2]
  # Read in character vector of package names
  packages.as.chars <- unlist(strsplit(mod.info[4,2], ", ", fixed=T))
  
  # Load packages
  for(package in packages.as.chars) {
    suppressWarnings(suppressPackageStartupMessages(require(package, character.only=TRUE)))
  }
  
  # Read in character vector of data sets needed for module
  datasets.as.chars <- unlist(strsplit(mod.info[5,2], ", ", fixed=T))
  data(list=datasets.as.chars, envir=.GlobalEnv)
  
  # Load content from csv file -- first 9 columns
  mod <- read.csv(file=mod.content.path, colClasses="character")[,1:9]
  
  # Find end of content and trim empty rows after this
  last.row <- max(which(mod$Output.Type != ""))
  mod <- mod[1:last.row, ]
  
  # If starting after user was supposed to define variable, define it for them
  loc <- grep("<-", mod$Correct.Answer)  # Find locations of assignment among correct answers in module
  if(length(loc) > 0) {
    for(row in loc) {
      # For each assignment, execute make global and execute so variable available for user by user
      if(row.start > row)  {
        assignment <- scan(text=mod$Correct.Answer[row], what=character(), sep="\n", quiet=TRUE)
        new.str.ans <- sub("<-", "<<-", assignment)
        eval(parse(text=new.str.ans))
      }
    }
  }
  
  # Also if current row not a new figure, bring up the last "new" figure
  # plus "additions" in case needed for upcoming question
  if(mod$Figure[row.start]=="" | mod$Figure.Type[row.start]=="addition") {
    fig.loc <- which(mod$Figure != "")  # Find locations of figures in module
    if(length(fig.loc) > 0) {
      if(row.start > min(fig.loc)) {
        new.plot.row <- lastNewFigRow(mod, row.start)
        sub.fig.loc <- fig.loc[fig.loc >= new.plot.row & fig.loc <= row.start]
        for(row in sub.fig.loc) {
          fig.path <- file.path(module.dir, "Figures", mod$Figure[row])
          source(file=fig.path, local=TRUE)
        }
        cat("\nI'm displaying the previous plot in case you need to refer back to it.\n")
      }
    }
  }
  
  # Print module number to progress file if starting from beginning
  if(row.start==1) {
    cat(module.name, "\n", file=progress.file.path)
  }
  
  ### Read content from table
  for(i in row.start:nrow(mod)) {
    # Print row number to progress file
    cat("row", i, "\n", sep="", file=progress.file.path, append=TRUE)
    cat("output.type", mod$Output.Type[i], "\n", sep=" ", file=progress.file.path, append=TRUE)
    
    # Indicator to suppress progress bar
    suppress.progress.bar <- 0
    
    # Take action based on type of output
    if(mod$Output.Type[i] == "text") {  ### TEXT
      cat("\n", mod$Output[i], sep="")
      readline("\n...")
      
    } else if(mod$Output.Type[i] == "question") {  ### QUESTION
      q <- mod$Output[i]
      ans.type <- mod$Answer.Type[i]
      ch <- scan(text=mod$Choices[i], what=character(), sep="\n", quiet=TRUE)
      correct.ans <- scan(text=mod$Correct.Answer[i], what=character(), sep="\n", quiet=TRUE)
      h <- mod$Hint[i]
      # Measure time taken to get correct answer
      # Start the clock
      ptm <- proc.time()
      userInput(question=q, type=ans.type, choices=ch, correct=correct.ans, hint=h,
                progress.file.path)
      # Record time taken - total elapsed time used
      time.on.question <- proc.time() - ptm
      cat("time.on.question", time.on.question[3], "\n", sep=" ", file=progress.file.path, append=TRUE)
      
    } else if(mod$Output.Type[i] == "figure") {  ### FIGURE
      cat("\n", mod$Output[i], sep="")
      fig.path <- file.path(module.dir, "Figures", mod$Figure[i])
      source(file=fig.path, local=TRUE)
      readline("\n...")
      
    } else if(mod$Output.Type[i] == "video") {  ### VIDEO
      cat("\n", mod$Output[i], "\n\n", sep="")
      resp <- readline("ANSWER: ")
      if(isYes(response=resp)) {
        browseURL(mod$Video[i])
        readline("\nPress Enter when you are done watching...")
      }
      suppress.progress.bar <- 1
    }
    
    # Print progress markers
    if(suppress.progress.bar==0) progressMarkers(current.row=i, total.rows=nrow(mod))
  }
}

email2username <- function(email) {
  username <- strsplit(email, split="@")[[1]][1]
  return(username)
}