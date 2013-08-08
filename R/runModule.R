#' Runs module
#' 
#' Runs specified swirl module beginning at specified row.
#' 
#' @param courseDir full path of directory where module is located
#' @param module.name character string specifying the name of the module
#' (Ex: "Module1")
#' @param row.start numeric specifying on which row of the module to begin
#' @param progress.file.path full file path of the user progress file
runModule <- function(courseDir, module.name, row.start, progress.file.path, 
                      review=FALSE, tags=NA, courseName) {  
  
  # Determine file extention for rda file
  rdaPath <- file.path(courseDir, paste(module.name, ".rda", sep=""))
  
  # Load module content and info
  load(rdaPath)
  
  # Change all NA to blanks for backwards compatability
  mod[is.na(mod)] <- ""
  mod.info[is.na(mod.info)] <- ""
  
  ### Do all of these next things only if this is not a module review
  if(review == FALSE) {
    
    # Read in character vector of package names
    packages.as.chars <- unlist(strsplit(mod.info[3,2], ", ", fixed=T))
    
    # Load packages
    for(package in packages.as.chars) {
      if (!package %in% installed.packages()) {
        install.packages(package, quiet=TRUE)
      }
      suppressWarnings(suppressPackageStartupMessages(require(package, character.only=TRUE)))
    }
    
    # Read in character vector of data sets needed for module
    datasets.as.chars <- unlist(strsplit(mod.info[4,2], ", ", fixed=T))
    data(list=datasets.as.chars, envir=.GlobalEnv)
    
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
            fig.path <- file.path(courseDir, "Figures", mod$Figure[row])
            source(file=fig.path, local=TRUE)
          }
          cat("\nI'm displaying the previous plot in case you need to refer back to it.\n")
        }
      }
    }
    
    # Print course name to progress file if starting on first row of first module
    if(row.start==1 & module.name=="Module1") {
      cat("COURSENAME ", courseName, "\n", sep="", file=progress.file.path, append=TRUE)
    }
    
    # Print module name to progress file if starting on first row
    if(row.start==1) {
      cat(module.name, "\n", sep="", file=progress.file.path, append=TRUE)
    }
    
    ### This is only run if a module review
  } else {
    
    # Find end of content and trim empty rows after this
    last.row <- max(which(mod$Output.Type != ""))
    mod <- mod[1:last.row, ]
    
    # Now find indexes of all rows with tags of interest
    tagIndex <- which(mod$Tag %in% tags == TRUE)
    
    # Reassign subset of module to mod variable
    mod <- mod[tagIndex, ]
    
    if(row.start==1) {
      cat("REVIEW\n", file=progress.file.path, append=TRUE)
    }
  }
  
  ### Read content from table
  for(i in row.start:nrow(mod)) {
    # Print row number to progress file
    cat("row", i, " tag ", mod$Tag[i],"\n", sep="", file=progress.file.path, append=TRUE)
    cat("output.type", mod$Output.Type[i], "\n", sep=" ", file=progress.file.path, append=TRUE)
    
    # Indicator to suppress progress bar
    suppress.progress.bar <- 0
    
    # Take action based on type of output
    if(mod$Output.Type[i] == "text") {  ### TEXT
      cat("\n", mod$Output[i], sep="")
      readline("\n...")
      
    } else if(mod$Output.Type[i] == "question") {  ### QUESTION
      if(!exists("proceed2Mandatory")) {
        proceed2Mandatory <- FALSE
      }
      
      # Skip question of it's optional and the user was not flagged on last one
      if(mod$Status[i] == "optional" && proceed2Mandatory == TRUE) {
        next
      } else {
        q <- mod$Output[i]
        ans.type <- mod$Answer.Type[i]
        ch <- unlist(strsplit(mod$Choices[i], ";\\s?"))
        correct.ans <- unlist(strsplit(mod$Correct.Answer[i], ";\\s?"))
        h <- mod$Hint[i]
        # Measure time taken to get correct answer
        # Start the clock
        ptm <- proc.time()
        strikes <- userInput(question=q, type=ans.type, choices=ch, 
                             correct=correct.ans, hint=h, progress.file.path)
        # Record time taken - total elapsed time used
        time.on.question <- proc.time() - ptm
        cat("time.on.question", time.on.question[3], "\n", sep=" ", 
            file=progress.file.path, append=TRUE)
        
        # If user gets question wrong 3 or more times then flag it in progress file
        # and go on to next row of content as usual
        
        if(strikes >= 3) {
          cat("flagged\n", file=progress.file.path, append=TRUE)
          proceed2Mandatory <- FALSE
          
          # If user does not get flagged on question, then signal to proceed to next
          # mandatory question
          
        } else {
          proceed2Mandatory <- TRUE
        }
      }
      
    } else if(mod$Output.Type[i] == "figure") {  ### FIGURE
      cat("\n", mod$Output[i], sep="")
      fig.path <- file.path(courseDir, "Figures", mod$Figure[i])
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