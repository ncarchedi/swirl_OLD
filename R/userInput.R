#' Gets user input
#' 
#' Solicits a response from user depending on the type of question specified. 
#' Also writes corresponding progress data to progress file.
#' 
#' @param question character string representing the question body
#' @param type character string specifying the type of question
#' @param choices character vector of choices, only specified for question type
#' "multiple"
#' @param correct character string representing the correct answer
#' @param hint character string of hint to be shown to user after incorrect response
#' @param progress.file.path full file path to file where progress is recorded
#' @return strikes number of incorrect responses before getting correct response
userInput <- function(question, type=c("exact", "range", "text", "command", "multiple"), 
                      choices="", correct, hint="", progress.file.path) {
  # Initialize strikes to zero
  strikes <- 0
  
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
        strikes <- strikes + 1
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
          strikes <- strikes + 1
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
        strikes <- strikes + 1
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
          strikes <- strikes + 1
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
        strikes <- strikes + 1
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
          strikes <- strikes + 1
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
        strikes <- strikes + 1
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
        } else if(identical(gsub(" ", "", str.ans), gsub(" ", "", correct))) {
          cat("\nDid you mean \'", correct, "\'?\n", sep="")
          resp <- readline("\nANSWER: ")
          if(isYes(resp)) {
            eval(parse(text=gsub("<-", "<<-", correct)))
            praise()
            recordIsCorrect(is.correct=TRUE, text.file.path=progress.file.path)
            break
          } else {
            tryAgain(hint)
            recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
            strikes <- strikes + 1
          }
        } else {
          tryAgain(hint)
          recordIsCorrect(is.correct=FALSE, text.file.path=progress.file.path)
          strikes <- strikes + 1
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
        strikes <- strikes + 1
      }
    }
  }
  return(strikes)
}
