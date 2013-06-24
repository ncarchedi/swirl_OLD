swirl <- function() {
  
  tryCatch({
    ##### SET DIRECTORY WHERE MODULES OF INTEREST ARE LOCATED #####
    module.dir <- file.path(path.package("swirl"), "inst", "Open_Intro_Modules")
    
    # Run openingMenu, which returns module name and row number on which to begin
    start <- openingMenu()
    module.start <- start[[1]]  # Character string
    row.start <- start[[2]]  # Numeric
    
    # Set names of files where user info and progress can be found
    files <- unlist(start[[3]])
    user.info.file.name <- files[[1]]
    progress.file.name <- files[[2]]
    
    # Create master module list and get element number of starting module
    master.module.list <- list("Module1") # Just one module for now
    mod.num <- which(master.module.list==module.start)
    
    # Start running modules beginning with starting module
    for(i in mod.num:length(master.module.list)) {
      # Run module i
      module.name <- master.module.list[[i]]
      runModule(module.dir, module.name, row.start, progress.file.name)
      # Reset row start to 1
      row.start <- 1
    }
  }, interrupt = function(ex) {  # If user presses Esc key
    cat("\nThanks for stopping by! Your progress has been saved....\n\n")
  }, error = function(ex) {  # If program stops due to error
    cat("\nAn error was detected. Please notify the administator.\n\n")
  }, finally = {  # No matter how program ends, close all open file connections
    closeAllConnections()
  })
}