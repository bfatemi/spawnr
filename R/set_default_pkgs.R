#' Set default R
#'
#' Function to insert line to initialize default packages automatically when R starts.
#'
#' @param pkgs character vector of package names to start with R
#'
#' @return boolean
#' @export
set_default_pkgs <- function(pkgs = NULL){
  if(is.null(pkgs)) return(NULL)

  # get path to .Rprofile & read in lines
  tmp <- "/.Rprofile"
  rdir <- paste0(R.home(component = "home"), "/etc")
  outPath <- paste0(rdir, tmp)

  #if exists, then just add new packages
  if(file.exists(outPath)){
    rprof <- readLines(outPath)
  }else{
    ntmp <- paste0("/install_scripts", tmp)
    rprof <- readLines(paste0(system.file("ext", package = "spawnr"), ntmp))
  }

  # if not installed, then ignore
  ind <- which(pkgs %in% row.names(installed.packages()))
  if(length(ind) == 0)
    return(FALSE)

  pkg.string <- str_c("c(", paste0("'", pkgs[ind], collapse = "',"), "')")
  new_line <- str_c("options(defaultPackages = ", pkg.string, ")")
  output <- c(new_line, rprof)

  message("\nSuccess. Following file added:\n\n")
  writeLines(output) # writes to console

  ## change permission and write
  if(get_os() == "linux"){
    system(paste0("sudo chmod -R 777 ", rdir))
    writeLines(output, outPath)
  }else{
    message("Need to implement file permission override in Windows")
    return(FALSE)
  }
  return(TRUE)
}

#' @describeIn set_default_pkgs helper function to lookup OS
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
