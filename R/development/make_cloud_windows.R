library(easydata)
library(data.table)
library(stringr)

read_pubkey <- function(pubkey_path=NULL){
  if(is.null(pubkey_path)){
    dr <- Sys.getenv("HOMEDRIVE")
    hp <- Sys.getenv("HOMEPATH")
    pk <- "\\.ssh\\id_rsa.pub"
    pubkey_path <- paste0(dr, hp, pk)
    tryCatch({
      pubkey <- readLines(pubkey_path)
    }, error=function(c){
      stop("Issue reading public key. Check path and if wrong, provide pubkey via arg:\n", path)
    })
  }
  return(pubkey)
}

make_cloud_init <- function(pubkey_path=NULL, usr="ruser"){

  ## Construct cloud init file
  ##
  ci_template <- readLines("inst/ext/template.yml") # template stored in package folder
  pubkey <- read_pubkey(pubkey_path)                # read public key
  tb <- "  "                                        # tab spacing to keep essential formatting

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

  dpath <- "inst/ext/install_scripts/"
  fn1 <- "silent-install-ocpu.sh"
  fn2 <- "inst_standard_packs.R"
  fn3 <- "post_pubkey.R"
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
             paste0(tb, "- Rscript --vanilla /inst_standard_packs.R > ins_standard_packs_log"),
             paste0(tb, "- Rscript --vanilla /post_pubkey.R > post_pubkey_log"),
             chunks[[6]],
             "")

  # print result on console for easy copy and paste
  # modify later to return so that R can launch
  writeLines(cfile)
}

