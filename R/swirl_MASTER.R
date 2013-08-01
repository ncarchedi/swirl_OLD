#' Runs the swirl program
#' 
#' This function is the main component of the swirl interactive learning 
#' system. It gets all necessary information from the user and reads all 
#' module content directly from the location specified by the user. When the
#' user quits the program via the Esc key, all progress is saved so he 
#' or she can resume their session at a later time.
#' 
#' @export
#' @author Nicholas A. Carchedi
swirl <- function() {
  tryCatch({
    
    # Run openingMenu, which returns module name and row number on which to begin
    cat("\nWelcome! My name is Swirl and I'll be your host today!")
    start <- openingMenu()
    module.start <- start[[1]]  # Character string
    row.start <- start[[2]]  # Numeric
    
    # Define user data directory path
    userDataPath <- file.path(path.package("swirl"), "user_data")
    
    # Check if user_data directory exists and if not, create it
    if(!file.exists(userDataPath)) {
      dir.create(userDataPath)
    }
    
    # Set names of files where user info and progress can be found
    files <- unlist(start[[3]])
    user.info.file.name <- files[1]
    progress.file.name <- files[2]
    
    course.start <- start[[4]][[1]]  # This is the course directory name
    courseName <- start[[4]][[2]] # This is the actual name of the course
    
    ##### SET DIRECTORY WHERE MODULES OF INTEREST ARE LOCATED #####
    course.dir <- file.path(path.package("swirl"), course.start)
    
    # Create master module list and get element number of starting module
    modules <- dir(course.dir, pattern="[a-zA-Z]+[0-9]+\\.csv")
    master.module.list <- gsub(".csv", "", as.list(modules))
    mod.num <- which(master.module.list==module.start)
    
    # Start running modules beginning with starting module
    for(i in mod.num:length(master.module.list)) {
      # Run module i
      module.name <- master.module.list[[i]]
      runModule(course.dir, module.name, row.start, progress.file.name, 
                courseName=courseName)
      
      # Suggest topics to review
      if(file.exists(progress.file.name)) {
        taggedTopics <- findTroubleTags(progressFilePath=progress.file.name)
        if(!identical(taggedTopics, NA)) {
          cat("\nIt appears that you struggled with the following topics:",
              paste(taggedTopics, collapse=", "), "\n")
          cat("\nWhich of these topics would you like to review?\n")
          options <- c(taggedTopics, "I'm good to go!")
          tags2Review <- select.list(options, multiple=TRUE)
          
          if(!identical(tags2Review, "I'm good to go!")) {
            # Run module in review mode for tags of interest
            runModule(course.dir, module.name, row.start=1, progress.file.name,
                      review=TRUE, tags=tags2Review)
            cat("\nYou've completed your review for the topics you selected!\n")
          }
        }
      }
      
      # Ask user if they want to continue with next module, if it exists
      if(i < length(master.module.list)) {
        cat("\nWould you like to continue on with the next module?")
        continue <- readline("\nANSWER: ")
        if(isYes(continue)) {
          # Reset row start to 1
          row.start <- 1
        } else {
          cat("\nThanks for stopping by! Your progress has been saved....\n\n")
          break
        }        
      }
    }
  }, interrupt = function(ex) {  # If user presses Esc key
    cat("\nThanks for stopping by! Your progress has been saved....\n\n")
  }, error = function(ex) {  # If program stops due to error
    cat("\nAn error was detected. Please notify the administator.\n\n")
  }, finally = {  # No matter how program ends, close all open file connections
    closeAllConnections()
  })
}