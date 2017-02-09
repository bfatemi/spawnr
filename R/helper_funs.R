#' Generate Cloud-Init File
#'
#' Function to generate a cloud-init file that can be used along with the digitalocean API
#' to fully launch and depeloy a full scale R environment. This function also references
#' files within this package that can be customized. These files are run on init, and include
#' an R script to install basic packages, and another file that can contain arbitrary code to run.
#'
#' @details
#' If \code{init_rfiles} is being provided, keep the following in mind:
#'    - Should be one or more filenames only
#'    - The files should be built with the package and placed in inst/ext/install_scripts
#'    - Any additional files required by the script when it's running on the server should be here: inst/ext/install_scripts/local
#'
#' If \code{usr} is not provided, default username is 'ruser', and password is not set (for now)
#'
#' If \code{GITHUB_PAT} is not provided as part of \code{...}, the default behavior is to use Sys.getenv.
#'
#' @param pubkey_path Path to .ssh file in the system that has entries for the public key
#' @param usr A string of length one naming the user to create on the server at launch. Defaults to 'ruser'
#' @param init_rfiles An optional character vector naming R files that should be run on the server to
#'              setup the environment. See details for important information regarding parameter.
#' @param ... Optional named arguments representing server env vars to set on boot
#' @param console A boolean (default is false) to specify whether to print the output to console.
#' Note this will happen if Rstudio is not detected regardless of the value of this paramater
#' @return Prints cloud-init file on console to use with DigitalOceans web UI to launch server
#'
#' @import stringr
#' @import data.table
#' @import easydata
#' @import utils
#' @importFrom pryr named_dots
#' @examples
#' \dontrun{
#' ## Not tested for systems other than windows
#'
#' # To ensure public key is correct, you can run this to see:
#' read_pubkey()
#'
#' # otherwise, the following function call will print the cloud init file on console
#' make_cloud_init()
#'
#' # Create cloud init file
#' pat <- "[PLACEHOLDER]"
#' rfile <- "init_server.R"
#' make_cloud_init(GITHUB_PAT = pat, init_rfiles = rfile)
#'
#'
#' ## DEVELOPMENT - INTERNAL USE
#'
#' # To complete remove write files chunk pass NULL to get_wfchunk. To instruct
#' # the cloud-init script to create a blank file on the server, pass in as an argument
#' # a name of a file that does not exist in the inst/install_scripts directory of the
#' # local package
#'
#' # get_wfchunk(NULL) # makes wf chunk NULL
#' # get_wfchunk("BLAH") # create empty file on server
#' }
#' @name helper_funs
NULL
## Get public key path and read if it exists


#' @describeIn helper_funs Helper function that returns public key stored on local system
#' @export
get_pubkey <- function(pubkey_path=NULL, choice = NULL){

  ## if path is not provided, attempt to construct it
  if(is.null(pubkey_path))
    pubkey_path <- str_c(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH"), ".ssh\\", "known_hosts")

  if(!file.exists(pubkey_path))
    stop("No public keys found at path: ", pkey_path)

  # read key and clean
  ll_pkey <- lapply(readLines(pubkey_path), str_extract, pattern="(?=ssh\\-rsa).+")
  if(length(ll_pkey)==0)
    stop("No saved public keys in file: ", pubkey_path)

  ## If there is more than one, give user choice to select 1 or all
  if(length(ll_pkey) > 1){
    if(is.null(choice)){
      cat("\nSelect one or more public keys to initialize on the server:\n\n")
      tmp <- str_c(paste0("\nEnter ", 1:length(ll_pkey), " For Key: "), "\n\n", ll_pkey, "\n")
      tmp2 <- c(tmp, paste0("\nEnter ", length(ll_pkey)+1, " For ALL Keys"))
      prmpt <- paste0(tmp2, collapse = "")
      cat(prmpt)
      response <- readline("Enter response: ")

      # should catch all errors regarding invalid response
      if(response == length(ll_pkey)+1)
        return(ll_pkey)
      return(ll_pkey[[as.numeric(response)]])
    }
    return(ll_pkey[[choice]])
  }
  return(ll_pkey)
}


#' @describeIn helper_funs Helper function that returns public key stored on local system
#' @export
get_template <- function(){
  ## break template into named list elements
  tpath <- system.file("ext", "template.yml", package = "spawnr")                   # find file path
  ci_template <- readLines(tpath)                                                   # read in template contents
  ct <- ci_template[!stringr::str_detect(ci_template, "^##.+$")]                    # remove comments
  index <- which(stringr::str_detect(ct, "^#--$"))                                  # identify position of each block
  result <- easydata::split_by_index(ct, index, include_at_index = FALSE)           # split into list of blocks
  lnames <- sapply(result, function(i) stringr::str_extract(i[1], "^[^: \\n]+"))    # extract block var name and set as list element name
  lnames[stringr::str_detect(lnames, "#cloud-config$")] <- "header"                 # change name to 'header' for first element
  names(result) <- lnames                                                           # set name and return list
  result
}


#' @describeIn helper_funs helper function to lookup OS
#' @export
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
