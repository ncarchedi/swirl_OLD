#' Updates swirl package
#' 
#' Prompts user to update swirl package with warning that the update will 
#' overwrite all user progress and info
#' 
updateSwirl <- function() {
  require(devtools)
  
  cat("\nHi there! Since swirl is still young, we are constantly fixing things up and adding new content. It is recommended that you update your version at least once a month so you have full access to the latest and greatest features!\n")
  cat("\nHowever, please note that updating your version of swirl will cause you to lose your progress if you are in the middle of a course. Would you like to update swirl now?\n\n")
  resp <- readline("ANSWER: ")
  
  if(isYes(resp)) {
    cat("\nAre you sure you'd like to update swirl now and lose any progress you've made?\n\n")
    resp <- readline("ANSWER: ")
    if(isYes(resp)) {
      cat("\nI'm updating swirl now!!!!\n")
      #install_github(repo="swirl", username="ncarchedi")
    } else {
      cat("\nNo problem! You will be prompted to update every time you start swirl.\n")
    }
  } else {
    cat("\nNo problem! You will be prompted to update every time you start swirl.\n")
  }
}