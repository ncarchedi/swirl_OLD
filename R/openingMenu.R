#' Displays opening menu options
#' 
#' Displays opening menu options for user and returns the user's starting 
#' location, including module name and row number.
#' 
#' @return List containing the starting module name, row number, and full paths
#' of both the user info and user progress files, as well as a list of course
#' directory name and course name. Note that the return value is 
#' a list with four elements, the third and fourth of which are lists with two
#' elements each.
openingMenu <- function() {  
  cat("\n\nPlease select the option below that applies to you:\n")
  status.choices <- c("I'm an existing user!", "This is my first time!", 
                      "I'm just a boring admin...")
  status <- select.list(choices=status.choices)
  
  if (status == status.choices[1]) {  ### EXISTING USER
    # Get email from user
    cat("\nGreat! What's your email address?")
    email <- readline("\nEMAIL: ")
    username <- email2username(email)
    user.files <- getUserFileNames(username)
    user.file.path <- user.files[[1]]
    progress.file.path <- user.files[[2]]
    
    # Check if user info and progress files exist
    if(all(file.exists(user.file.path, progress.file.path))) {
      # Get user info from saved user_info text file
      user.info <- readLines(user.file.path, warn=FALSE)
      user.name <- user.info[1]
      user.email <- user.info[2]
      
      userLeftOff <- findUserLocation(progress.file.path)
      
      course.start <- userLeftOff[[1]]  # List of course dir name and course name
      module.start <- userLeftOff[[2]]
      row.start <- userLeftOff[[3]]
      
      cat("\nWelcome back, ", user.name, "! It's great to see you again. Would you like to begin where you left off?\n\n", sep="")
      resp <- readline("ANSWER: ")
      
      if(!isYes(resp)) {
        cat("\nAre you sure you'd like to start over and lose any progress you've made?\n\n")
        resp <- readline("ANSWER: ")
        if(isYes(resp)) {
          module.start <- "Module1"
          row.start <- 1
          course.start <- chooseCourse() # Course dir name and course name
        }
      }
    } else {  # If can't locate records for user
      cat("\nI'm sorry, but for some reason, I can't find your records. You'll have to log in as a new user and start from the beginning.")
      start <- openingMenu()
      module.start <- start[[1]]
      row.start <- start[[2]]
      user.files <- start[[3]]
      course.start <- start[[4]]
    }
    
  } else if (status == status.choices[2]) {  ### NEW USER
    # Get user info and corresponding file names
    user.files <- storeUserInfo()
    
    if(all(file.exists(unlist(user.files)))) {
      cat("\nI'm sorry, but it looks like I already have your email address on file. Please log in as an existing user.")
      start <- openingMenu()
      module.start <- start[[1]]
      row.start <- start[[2]]
      user.files <- start[[3]]
      course.start <- start[[4]]
    } else {
      module.start <- "Module1"
      row.start <- 1
      progress.file.path <- user.files[[2]]
      course.start <- chooseCourse() # Course dir name and course name
      
      # Quick housekeeping items
      cat("\nGreat! Let's cover a couple of quick housekeeping items before we begin our first lesson.\n")
      cat("\nFirst off, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue. Also, as you've probably figured out, when you see 'ANSWER:', that means it's your turn to enter a response, then press Enter to continue.")
      cat("\n\nFinally, you'll notice a progress bar that shows you how far you've made it through the lesson. Remember you can stop at any time by pressing the Esc key and your progress will be saved. Let's get started!")
      readline("\n...  <-- That's your cue to press Enter to continue")
    }
    
  } else if (status == status.choices[3]) {  ### ADMIN
    # Check admin password, which is swirladmin
    cat("\nPlease enter the admin password for top secret access.")
    password <- readline("\nPASSWORD: ")
    
    if (password == "swirladmin") {
      
      cat("\nWelcome, Mr. or Mrs. Important!\n")
      course.start <- chooseCourse()
      cat("\nOn which module would you like to begin?")
      module.start <- readline("\nANSWER: ")
      cat("\nAnd which row of the content table would you like to start on?")
      row.start <- as.numeric(readline("\nANSWER: "))
      
      # Use default admin file names
      username <- "admin"
      user.files <- getUserFileNames(username)
      
      # Write course name and module name to top of NEW progress file
      cat("COURSENAME ", course.start[[2]], "\n", sep="", file=user.files[[2]])
      cat(module.start, "\n", sep="", file=user.files[[2]], append=TRUE)
      
      # Send back to openingMenu if wrong password
    } else {  
      cat("\nSorry, wrong password!\n")
      start <- openingMenu()
      module.start <- start[[1]]
      row.start <- start[[2]]
      user.files <- start[[3]]
      course.start <- start[[4]]
    }
  }
  return(list(module.start, row.start, user.files, course.start))
}
