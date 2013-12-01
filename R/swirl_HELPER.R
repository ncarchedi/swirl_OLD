### HELPER FUNCTIONS

#' Records (appends) a string to designated text file
#' 
#' In the swirl function, it is used to output progress to a progress file.
#' 
#' @param my.string character string
#' @param text.file.path full file path of text file where string is recorded
recordString <- function(my.string, text.file.path) {
  cat("user.answer", my.string, "\n", file=text.file.path, append=TRUE)
}

#' Records whether correct answer
#' 
#' Records TRUE or FALSE to designated text file. In the swirl function, it is 
#' used to denote whether user's answer is right or wrong.
#' 
#' @param is.correct logical, TRUE or FALSE
#' @param text.file.path full file path of text file where string is recorded
recordIsCorrect <- function(is.correct, text.file.path) {
  cat("is.correct", is.correct, "\n", file=text.file.path, append=TRUE)
}

#' Determines whether yes or no
#' 
#' Determines whether a response to a yes/no question is yes or no.
#' 
#' @param response character vector containing response to a yes/no question
#' @return Logical, TRUE or FALSE
isYes <- function(response) {
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

#' Prints praise message
#' 
#' Prints random message of praise from text file containing several messages.
praise <- function() {
  praise.options <- scan(file=file.path(path.package("swirl"), "praise.txt"), what=character(), quiet=TRUE)
  i <- sample(1:length(praise.options), 1)
  cat("\n", praise.options[[i]], "\n", sep="")
}

#' Prints encouragement and hint
#' 
#' Prints random message of encouragement for user to try again.
#' 
#' @param hint a character string containing a hint to the user
tryAgain <- function(hint="") {
  tryagain.options <- scan(file=file.path(path.package("swirl"), "try_again.txt"), what=character(), quiet=TRUE)
  i <- sample(1:length(tryagain.options), 1)
  cat("\n", tryagain.options[[i]], " ", hint, sep="")
}

#' Prints progress markers
#' 
#' Generates visual progress markers in console.
#' 
#' @param current.row numeric representing the current row that the user is on
#' in the module spreadsheet
#' @param total.rows numeric representing the total number of rows in the module
#' spreadsheet
progressMarkers <- function(current.row, total.rows) {
  
  percent.complete <- round((current.row/total.rows)*100)
  cat("\n")
  if(total.rows >= 10) {
    total.slots <- 10
    ticks <- round(total.rows/total.slots)
    marks <- ifelse(round(current.row/ticks) > 10, 10, round(current.row/ticks))
    cat("PROGRESS: << ", rep("(X) ", marks), rep("( ) ", total.slots-marks), 
        ">> ", sep="")
  }
  cat("(", percent.complete, "% Complete)\n", sep="")
}

#' Finds row of last "new" figure
#' 
#' Finds the row number of the last "new" figure in the specified module. Used 
#' for when a user is resuming progress on a partially completed module and a 
#' previous figure needs to be loaded. It must be "new" since any "addition"
#' figures expect an existing "new" figure is already in place.
#' 
#' @param content.table data.frame containing a complete swirl module
#' @param row.start numeric specifying the row that the user is starting on
lastNewFigRow <- function(content.table, row.start) {
  fig.loc <- which(content.table$Figure != "")  # Find locations of figures in module
  last.fig.row <- fig.loc[max(which(fig.loc < row.start))]
  if(content.table$Figure.Type[last.fig.row]=="new") {  # If last figure is "new" then just source it
    return(last.fig.row)
  } else {  # If last figure is an addition, use recursion to find last "new" figure row
    lastNewFigRow(content.table, last.fig.row)
  }
}

#' Stores user info
#' 
#' Stores first time user info in a new text file and returns the file path. 
#' Also creates a progress file and returns location of it.
#' 
#' @return List containing the full file paths of the user info file and user
#' progress file, respectively
storeUserInfo <- function() {
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

#' Gets user file names
#' 
#' Takes username as input and returns a list containing the appropriate user 
#' file names. The function does not attempt to verify whether these files exist.
#' 
#' @param username character string containing valid swirl username, which is
#' just the part of an email address before the "at" symbol.
#' @return List containing the expected full file paths of the user info file 
#' and user progress file, respectively
getUserFileNames <- function(username) {
  # Define user data directory path
  userDataPath <- file.path(path.package("swirl"), "user_data")
  
  # Define user data file paths
  user.info.file.path <- file.path(userDataPath, paste(username,"_info.txt", sep=""))
  progress.file.path <- file.path(userDataPath, paste(username,"_progress.txt", sep=""))
  
  return(list(user.info.file.path, progress.file.path))
}

#' Gets username from email address
#' 
#' Extracts a swirl username from a given email address. Does not validate
#' format of email address.
#' 
#' @param email character string representing a valid email address
#' @return Character string containing the portion of the email address prior to
#' the "at" symbol
email2username <- function(email) {
  username <- strsplit(email, split="@")[[1]][1]
  return(username)
}

#' Find trouble tags
#' 
#' Reviews user progress file for questions that the user struggled with and
#' returns a list of tags associated with those questions
#' 
#' @param progressFilePath path to user progress file
#' @return tags list of tags associated with questions with which the user
#' struggled
findTroubleTags <- function(progressFilePath) {
  lines <- readLines(progressFilePath)
  
  # Find beginning of current module
  modBegin <- grep("Module[0-9]+", lines)
  modBegin <- ifelse(length(modBegin) > 0, max(modBegin), 1)
  
  # Get rid of everything in variable lines that comes before current module
  lines <- lines[modBegin:length(lines)]
  
  qbegin <- grep("output.type question", lines, fixed=TRUE)
  newRows <- grep("row[0-9]+", lines)
  qend <- c(rep(NA, length(qbegin)))
  
  for(i in 1:length(qbegin)) {
    biggerThan <- newRows > qbegin[i]
    if(all(qbegin[i] > newRows)) {
      next
    } else {
      qend[i] <- newRows[min(which(biggerThan == TRUE))]
    }
  }
  
  falseAns <- grep("is.correct FALSE", lines, fixed=TRUE)
  numFalse <- c(rep(NA, length(qbegin)))
  
  for(i in 1:length(qbegin)) {
    numFalse[i] <- sum(falseAns > qbegin[i] & falseAns < qend[i])
  }
  
  flags <- c(rep(FALSE, length(qbegin)))
  for(qnum in 1:length(flags)) {
    if(numFalse[qnum] > 2) {
      flags[qnum] <- TRUE
    }
  }
  
  if(any(flags==TRUE)) {
    rowFlagged <- qbegin[flags == TRUE] - 1
    linesFlagged <- lines[rowFlagged]
    splitLines <- strsplit(linesFlagged, " ")
    
    tags <- list()
    for(i in 1:length(linesFlagged)) {
      tags[[i]] <- paste(splitLines[[i]][3:length(splitLines[[i]])], collapse=" ")
    }
  } else {
    tags <- NA
  }
  
  # Return list of unique tags -- no duplicates
  return(unlist(unique(tags)))
}

#' Choose course to begin
#' 
#' Prompts the user to choose the course they would like to take and returns the
#' corresponding directory name and course name.
#' 
#' @return list containing directory name for chosen course and the course name
chooseCourse <- function() {
  courseDirList <- list.files(path=file.path(path.package("swirl"), "Courses"))
  courseNames <- gsub("_", " ", courseDirList)
                           
  cat("\nWhich course would you like to take?\n")
  courseName <- select.list(courseNames, graphics=FALSE)
  courseDirName <- file.path("Courses", gsub(" ", "_", courseName))
  
  return(list(courseDirName, courseName))
}

#' Find current course, module, and row
#' 
#' Given the file path for user's progress file, this function returns a list
#' containing the current course, module, and row of content for that user.
#' 
#' @param progressFilePath Complete file path for user's progress file
#' @return currentLocation List containing current course directory name,
#' course name, module, and row, respectively
findUserLocation <- function(progress.file.path) {
  
  # Get progress from progress file
  progress <- readLines(progress.file.path, warn=FALSE)
  
  # Find current course name
  courseStartIndex <- max(grep("COURSENAME", progress))
  course.start <- gsub("COURSENAME ", "", progress[courseStartIndex])
  courseDirName <- file.path("Courses", gsub(" ", "_", course.start))
  
  # Find current module
  modStartIndex <- max(grep("Module[0-9]+", progress))
  module.start <- progress[modStartIndex]
  
  # Find number of last row
  index <- max(grep("row", progress))
  row.as.string <- progress[index]
  row.start <- as.numeric(gsub("\\D", "", row.as.string))
  
  currentLocation <- list(list(courseDirName, course.start), module.start, row.start)
  
  return(currentLocation)
}
