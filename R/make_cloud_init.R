library(stringr)
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
#' @name make_cloud_init
NULL

#' @describeIn make_cloud_init Generates cloud_init file on console for now.
#' @export
make_cloud_init <- function(pubkey_path=NULL, usr=NULL, init_rfiles=NULL, console=FALSE, ...){
  env.vars <- pryr::named_dots(...)
  if(is.null(usr))
    usr <- "ruser" # set username if null

  # if init_files is not explicitly provided, search the install_scripts directory for .R
  # and use those files. install_scripts dir is intended on storing all files required for
  # initialization
  if(is.null(init_rfiles)){
    ifiles <- list.files(system.file("ext", "install_scripts", package = "spawnr"))
    ind <- which(stringr::str_detect(ifiles, "^.+\\.R$"))
    if(length(ind) > 0)
      init_rfiles <- ifiles[ind]
  }

  ## Add boot chunk since it uses environmental variable
  ##   - if R_PROFILE path not included, add it to the list of vars to set
  ##
  if(!"GITHUB_PAT" %in% names(env.vars)){
    tmp <- Sys.getenv("GITHUB_PAT")
    if(tmp == "")
      stop("Need either ghpat as argument or GITHUB_PAT set as system variable.")
    env.vars <- c(env.vars, list(GITHUB_PAT = tmp))
  }
  if(!"R_PROFILE" %in% names(env.vars))
    env.vars <- c(env.vars, list(R_PROFILE = "/usr/lib/R/etc/.Rprofile"))

  if(any(!str_detect(names(env.vars), "^[A-Za-z_]+$"))){
    stop("Invalid var name...
         Ensure environ vars are named with chars or _ only")
  }

  ## Construct cloud init file
  ##
  tmp <- get_template()
  on_boot <- list(bootcmd = do.call(get_onboot_vars, env.vars))
  chunks <- c(tmp[names(tmp) == "header"], on_boot, tmp[names(tmp) != "header"])

  ## Username & public key
  ##
  tb <- "  " # tab spacing for formatting
  ind <- which(str_detect(chunks$users, paste0(tb, "- name:")))
  chunks$users[ind] <- paste0(chunks$users[ind], " ", usr)

  pubkey <- read_pubkey(pubkey_path)
  ind <- which(stringr::str_detect(chunks$users, "ssh-authorized-keys:"))
  tmp <- c(chunks$users[ind], paste0(tb, tb,tb, "- ", pubkey))
  chunks$users <- c(chunks$users[-ind], tmp)

  ## Linux packages to install via-cloud-init
  ##
  pkgs <- c("apache2", "build-essential", "libxml2-dev", "libcurl4-openssl-dev")
  chunks$packages <- c(chunks$packages, paste0(paste0(tb, "- ", collapse = ""), pkgs))

  ## Write Files... write installation scripts, saved locally, on the server
  ##    - if providing additional scripts outside of default (install..ocpu.sh),
  ##      then we need to add extra commands to 'runcmd'
  ##
  inst.files <- c("install_rstudio_ocpu.sh", init_rfiles)
  chunks$write_files <- get_wfchunk(inst.files)

  # add additional cmd to runcmd based on new install scripts
  if(!is.null(init_rfiles)){
    bname <- stringr::str_replace(init_rfiles, "\\.R", "")
    add_command <- paste0(tb, "- Rscript --vanilla /", init_rfiles, " > R_", bname, "_log")
    chunks$runcmd <- c(chunks$runcmd, add_command)
  }

  ### MERGE ALL TOGETHER & PRINT
  ###   - Integrate later with DO api
  merged <- unlist(lapply(1:length(chunks), function(i) do.call("[[", list(chunks, i))))

  if(rstudioapi::isAvailable() & !console){

    # Store the last generated copy of the cloud-init file in a package directory 'log'
    logdir <- system.file("log", package = "spawnr")
    fname <- "/cloud-init.yml"
    fpath <- paste0(logdir, fname)

    if(!dir.exists(logdir)){

      # create log dir if it doesnt exist (it should)
      dir.create("inst/log")
    }else{

      # create file if it doesnt exist
      if(!file.exists(fpath)){
        file.create(fpath)
      }
    }
    writeLines(merged, fpath)
    rstudioapi::navigateToFile(fpath)
    cat("\nGenerated cloud-init file located here:\n\n", fpath, "\n\n")
    return(TRUE)
  }
  cat("\nRStudio api not available. Writing cloud-init file to console:\n\n")
  writeLines(merged)
  return(TRUE)
}


#' @describeIn make_cloud_init Helper function that returns public key stored on local system
#' @export
read_pubkey <- function(pubkey_path=NULL){

  ## if path is not provided, attempt to construct it
  if(is.null(pubkey_path)){

    dr <- Sys.getenv("HOMEDRIVE")
    hp <- Sys.getenv("HOMEPATH")

    if(get_os() == "windows"){
      pk <- ".ssh\\known_hosts"
    }else{
      pk <- ".ssh\\id_rsa.pub"
    }
    pubkey_path <- paste0(dr, hp, pk)
  }

  ## using path, read in local public key
  tryCatch({
    pubkey <- readLines(pubkey_path)
  }, error=function(c){
    stop("Issue reading public key.
         Check path and if wrong, provide pubkey via arg:\n", pubkey_path)
  })

  if(length(pubkey)==0)
    stop("No saved public keys in file: ", pubkey_path)

  ## If there is more than one, give user choice to select 1 or all
  if(length(pubkey) > 1){
    cat("\nSelect one or more public keys to initialize on the server:\n\n")
    tmp <- str_c(paste0("\nEnter ", 1:length(pubkey), " For Key: "), "\n\n", pubkey, "\n")
    tmp2 <- c(tmp, paste0("\nEnter ", length(pubkey)+1, " For ALL Keys"))
    prmpt <- paste0(tmp2, collapse = "")
    cat(prmpt)
    response <- readline("Enter response: ")

    # should catch all errors regarding invalid response
    if(response == length(pubkey)+1)
      return(pubkey)
    else if(!response %in% 1:length(pubkey))
      stop("Invalid response. Stopping code execution")

    # Use selection to return the appropriate public key
    tryCatch({
      # Manage errors due to invalid user response
      ret <- pubkey[as.numeric(response)]
      return(ret)
    }, error=function(c){
      stop("Error reading public key and selection. Aborting")
    })
  }
  return(pubkey)
}

#' @describeIn make_cloud_init Helper function that returns public key stored on local system
#' @export
get_template <- function(){
  tpath <- system.file("ext", "template.yml", package = "spawnr")
  ci_template <- readLines(tpath)     # template stored in package folder

  ## break template into named list elements
  index <- which(stringr::str_detect(ci_template, "^#--$"))
  tmp <- easydata::split_by_index(ci_template, index, include_at_index = FALSE)
  lnames <- sapply(tmp, function(i) stringr::str_extract(i[1], "^[^: \\n]+"))

  # change name to something more general for header entry
  lnames[stringr::str_detect(lnames, "#cloud-config$")] <- "header"

  # drop extra comment lines
  keepInd <- !stringr::str_detect(lnames, "^##$")
  result <- tmp[keepInd]
  names(result) <- lnames[keepInd]
  result
}

#' @describeIn make_cloud_init Helper function that constructs the write_files block in cloud-init
#' @export
get_wfchunk <- function(init_rfiles=NULL){
  if(is.null(init_rfiles)){
    warning("No fnames given. Setting writefiles chunk to NULL...")
    return(NULL)
  }
  ## Save tab variables, just for clarity
  t   <- "  "
  tt  <- paste0(t, t, collapse = "")
  ttt <- paste0(t, t, t, collapse = "")

  # set path, read lines, and bind together (for each file)
  tmp <- lapply(init_rfiles, function(i){
    path <- paste0(t, "- path: /", i)
    content <- c(path, paste0(tt, "content: |"), paste0(ttt, get_inst_lines(i)))
  })
  result <- c("write_files:", unlist(tmp))
  return(result)
}

#' @describeIn make_cloud_init Helper function used by \code{get_wfchunk}
#' @export
get_inst_lines <- function(init_rfiles=NULL){
  if(is.null(init_rfiles)){
    warning("No local filename to read lines Returning NULL")
    return(NULL)
  }
  local_dir <- paste0(system.file("ext", "install_scripts", package = "spawnr"), "/")
  local_fpath <- paste0(local_dir, init_rfiles)

  if(file.exists(local_fpath) == FALSE){
    warning("file not found in local dir: ",
            local_fpath,
            "... return NULL")
    return(NULL)
  }
  ## Read in file & remove blank lines
  tmp <- readLines(local_fpath)
  lines <- tmp[!stringr::str_detect(tmp, " +$|^$")]

  # Remove comments only if .R file
  if(stringr::str_detect(init_rfiles, ".+\\.R$"))
    lines <- lines[!stringr::str_detect(lines, "^#.+$| +$|^$")]
  return(lines)
}

#' @describeIn make_cloud_init Helper fn to construct the bootcmd chunk with env vars
#' @export
get_onboot_vars <- function(...){
  args <- list(...)
  if(length(args)==0)
    return(NULL)
  tmp <- as.character(mapply(function(x, i) paste0(x, "=", i),
                             names(args),
                             args,
                             SIMPLIFY = TRUE))
  res <- c("bootcmd:", paste0("  - echo '", tmp, "' >> /etc/environment"))
  return(res)
}



