#' Set default R
#'
#' Function to insert line to initialize default packages automatically when R starts.
#'
#' @param pkgs character vector of package names to start with R
#'
#' @importFrom stringr str_c
#' @importFrom utils installed.packages
#'
#' @export
set_default_pkgs <- function(pkgs = NULL){
  # Keep the current default packages
  pkgs <- unique(c(getOption("defaultPackages"), pkgs))
  NEWFILE <- TRUE

  if(is.null(pkgs)) return(NULL)

  # get path to .Rprofile & read in lines
  tmp <- "/.Rprofile"
  rhome <- R.home(component = "home") #"/usr/lib/R"
  outPath <- paste0(rhome, "/etc", tmp)

  #if exists, then just add new packages
  if(file.exists(outPath)){
    rprof <- readLines(outPath)
    NEWFILE <- FALSE
  }else{
    file.create(outPath)  # need to create it
    ntmp <- paste0("/install_scripts/local", tmp)
    rprof <- readLines(paste0(system.file("ext", package = "spawnr"), ntmp))
  }
  # set permission for file
  system(paste0("chmod -R 777 ", outPath))

  # Ignore if not installed OR already loaded (i.e. already a default package)
  ind <- which(pkgs %in% row.names(utils::installed.packages()))
  if(length(ind) == 0)
    return(FALSE)

  if(NEWFILE != FALSE){
    pkg.string <- stringr::str_c("c(", paste0("'", pkgs[ind], collapse = "',"), "')")
    new_line <- stringr::str_c("options(defaultPackages = ", pkg.string, ")")
    output <- c(new_line, rprof)
    message("\nSuccess. Following file added:\n")
    writeLines(output) # writes to console
  }else{
    warning(".Rprofile already exists at location... ", outPath)
    return(FALSE)
  }
  ## change permission and write
  if(get_os() == "linux"){
    writeLines(output, outPath)
  }else{
    message("Need to implement file permission override in Windows")
    return(FALSE)
  }
  return(TRUE)
}





