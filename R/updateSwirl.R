#' Updates swirl package
#' 
#' Updates swirl package with warning that the update will 
#' overwrite all user progress and info
#' 
update_swirl <- function() {
  require(devtools)
  cat("\nAre you sure you'd like to update swirl now and lose any progress you've made?\n\n")
  resp <- readline("ANSWER: ")
  if(isYes(resp)) {
    install_github(repo="swirl", username="ncarchedi")
  } else {
    cat("\nNo problem! You will be prompted to update every time you start swirl. Type 'swirl()' now to restart the program.\n")
  }
}

#' Prompts user to update
#' 
#' Prompts user to update swirl package with warning that the update will 
#' overwrite all user progress and info
#' 
promptUpdate <- function() {  
  cat("\nHi there! Since swirl is still young, we are constantly fixing things up and adding new content. It is recommended that you update your version at least once a month so you have full access to the latest and greatest features!\n")
  cat("\nHowever, please note that updating your version of swirl will cause you to lose your progress if you are in the middle of a course. Would you like to update swirl now?\n\n")
  resp <- readline("ANSWER: ")
  
  if(isYes(resp)) {
    cat("\nAre you sure you'd like to update swirl now and lose any progress you've made?\n\n")
    resp <- readline("ANSWER: ")
    if(isYes(resp)) {
      cat("\nOkay! Please hit the Esc key and type 'update_swirl()'. The update may take a minute or two. Once the update is complete, type 'swirl()' and hit Enter to restart the program.\n\n")
      return(TRUE)
    } else {
      cat("\nNo problem! You will be prompted to update every time you start swirl.\n\n")
      return(FALSE)
    }
  } else {
    cat("\nNo problem! You will be prompted to update every time you start swirl.\n\n")
    return(FALSE)
  }
}