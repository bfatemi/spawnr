#' Generate Cloud-Init File
#'
#' Function to generate a cloud-init file that can be used along with the digitalocean API
#' to fully launch and depeloy a full scale R environment. This function also references
#' files within this package that can be customized. These files are run on init, and include
#' an R script to install basic packages, and another file that can contain arbitrary code to run.
#'
#' @param pubkey_path Path to .ssh file in the system that has entries for the public key
#' @param usr A string of length one naming the user to create on the server at launch. Defaults to 'ruser'
#'
#' @return Prints cloud-init file on console to use with DigitalOceans web UI to launch server
#'
#' @import stringr
#' @import data.table
#' @import easydata
#'
#' @examples
#' # Not tested for systems other than windows
#' # To ensure public key is correct, you can run this to see:
#' read_pubkey()
#'
#' #otherwise, the following function call will print the cloud init file on console
#' make_cloud_init()
#' @name make_cloud
NULL

#' @describeIn make_cloud Helper function that returns public key stored on local system
#' @export
read_pubkey <- function(pubkey_path=NULL){
  if(is.null(pubkey_path)){
    dr <- Sys.getenv("HOMEDRIVE")
    hp <- Sys.getenv("HOMEPATH")
    pk <- "\\.ssh\\id_rsa.pub"
    pubkey_path <- paste0(dr, hp, pk)
    tryCatch({
      pubkey <- readLines(pubkey_path)
    }, error=function(c){
      stop("Issue reading public key.
           Check path and if wrong, provide pubkey via arg:\n", pubkey_path)
    })
  }
  return(pubkey)
}

#' @describeIn make_cloud Generates cloud_init file on console for now.
#' @export
make_cloud_init <- function(pubkey_path=NULL, usr="ruser"){

  ## Construct cloud init file
  ##
  tpath <- system.file("ext", "template.yml", package = "spawnr")
  ci_template <- readLines(tpath)     # template stored in package folder
  pubkey <- read_pubkey(pubkey_path)  # read public key
  tb <- "  "                          # tab spacing to keep essential formatting

  ## break into chunks and handle individually
  index <- which(str_detect(ci_template, "^#--$"))
  chunks <- easydata::split_by_index(ci_template, index, include_at_index = FALSE)

  ##
  ## Username & public key
  ##
  user <- chunks[[1]][-1]
  ind <- which(str_detect(user, paste0(tb, "- name:")))
  user[ind] <- paste0(user[ind], " ", usr)

  ind <- which(str_detect(user, "ssh-authorized-keys:"))
  user <- c(user[1:(ind-1)], c(user[ind], paste0(tb, tb,tb, "- ", pubkey)))

  if((ind+1)<length(user))
    user <- c(user, user[(ind+1):length(user)])

  ##
  ## packages
  ##
  packages <- chunks[which(str_detect(chunks, "packages:"))][[1]]
  pkgs <- c("apache2", "build-essential", "libxml2-dev", "libcurl4-openssl-dev")
  packages <- c(packages, paste0(paste0(tb, "- ", collapse = ""), pkgs))

  ##
  ## write files
  ##
  writefiles <- chunks[which(str_detect(chunks, "write_files:"))][[1]]
  wfp <- writefiles[2]
  wfc <- writefiles[3]

  dpath <- paste0(system.file("ext", "install_scripts", package = "spawnr"), "/")
  fn1 <- "install_rstudio_ocpu.sh"
  fn2 <- "install_packages.R"
  fn3 <- "run_on_server.R"

  c1 <- readLines(paste0(dpath, fn1))
  c2 <- readLines(paste0(dpath, fn2))
  c3 <- readLines(paste0(dpath, fn3))

  TB <- paste0(tb, tb, tb, collapse = "")
  block1 <- c(paste0(wfp, " /", fn1), c(wfc, paste0(TB, c1)))
  block2 <- c(paste0(wfp, " /", fn2), c(wfc, paste0(TB, c2)))
  block3 <- c(paste0(wfp, " /", fn3), c(wfc, paste0(TB, c3)))

  writefiles <- c(writefiles[1], block1, block2, block3)

  ###
  ### MERGE ALL TOGETHER
  ###
  result <- c(user, chunks[[2]], packages, writefiles, chunks[[5]])

  # Ensure there is a final new line
  if(result[length(result)] != "") result <- c(result, "")

  # Add calls to the write files
  cfile <- c(result[-length(result)],
             paste0(tb, "- Rscript --vanilla /install_packages.R > ins_standard_packs_log"),
             paste0(tb, "- Rscript --vanilla /run_on_server.R > post_pubkey_log"),
             chunks[[6]],
             "")

  # print result on console for easy copy and paste
  # modify later to return so that R can launch
  writeLines(cfile)
}

