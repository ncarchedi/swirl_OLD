#' Saves module to package
#' 
#' Loads module content and info directly from Excel spreadsheets (xlsx) based
#' on specified module name(s) and directory and saves content and info to two 
#' data frames in a single rda file for each module. The rda file is saved in 
#' the designated course directory.
#' 
#' @param moduleName character vector of module names (e.g. "Module3")
#' @param excelDir full path to directory where Excel spreadsheets are stored
#' @param courseDirectory full path to course directory within package directory
#' 
saveModule <- function(moduleName, excelDir, courseDirectory) {
  
  for(m in moduleName) {
    # Construct complete file paths for both content and info Excel spreadsheets
    contentPath <- file.path(excelDir, paste(m, ".xlsx", sep=""))
    infoPath <- file.path(excelDir, paste(m, "_Info.xlsx", sep=""))
    
    # Check if Excel file paths exist
    if(!all(file.exists(contentPath, infoPath))) {
      message <- paste("The content and/or info cannot be found for", m, "in the specified directory!")
      stop(message)
    } else {
      # If Excel file path exists, load required package
      require(XLConnect)
      
      # Create rda file path
      rdaName <- paste(m, ".rda", sep="")
      rdaPath <- file.path(courseDirectory, rdaName)
      
      # Load content and info spreadsheets into separate data frames
      mod <- readWorksheetFromFile(contentPath, sheet="Sheet1")
      mod.info <- readWorksheetFromFile(infoPath, sheet="Sheet1")
      
      # Save data frame as rda file to specified course directory
      save(mod, mod.info, file=rdaPath)
    }
  }
}

moduleName <- c("Module3")
excelDir <- "C:\\Users\\Nick\\Dropbox\\swirl_shared\\Boot Camp 2"
courseDirectory <- "C:\\Users\\Nick\\Dropbox\\R_Working_Directory\\My_Packages\\swirl\\inst\\Courses\\Mathematical_Biostatistics_Boot_Camp_2"

#saveModule(moduleName, excelDir, courseDirectory)
