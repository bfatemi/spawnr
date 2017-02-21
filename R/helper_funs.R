#' Helper Functions
#'
#' Read in template cloud config or get public key
#'
#' @param pubkey_path Path to .ssh file in the system that has entries for the public key
#' @param choice TBD
#'
#' @import data.table
#' @importFrom easydata split_by_index
#' @importFrom stringr str_extract str_detect str_c
#'
#' @name helper_funs
NULL


#' @describeIn helper_funs Helper function that returns public key stored on local system
#' @export
get_pubkey <- function(pubkey_path=NULL, choice = NULL){

  ## if path is not provided, attempt to construct it
  if(is.null(pubkey_path))
    pubkey_path <- stringr::str_c(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH"), "\\.ssh\\", "known_hosts")

  if(!file.exists(pubkey_path))
    stop("No public keys found at path: ", pubkey_path)

  # read key and clean
  ll_pkey <- lapply(readLines(pubkey_path), stringr::str_extract, pattern="(?=ssh\\-rsa).+")
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
