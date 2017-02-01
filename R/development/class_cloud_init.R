
library(R6)

## Save tab variables, just for clarity
t   <- "  "
tt  <- paste0(t, t, collapse = "")
ttt <- paste0(t, t, t, collapse = "")


# cloud_cmd <- function(param, value){
#   paste0(param, ": ", value)
# }

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

BOOTCMD <- R6Class(
  classname = "bootcmd",
  public = list(
    add = function(...) private$cmd <- c(private$cmd, do.call("quote", list(substitute(...)))),
    get_text = function(){
      if(length(private$cmd)==0) return(NULL)
      txt <- c("bootcmd:", paste0(t, "- ", private$cmd))
      return(txt)
    },
    get_cmds = function() private$cmd
  ),
  private = list(cmd = NULL)
)

USERS <- R6Class(
  classname = "users",
  public = list(
    name = "ruser",
    groups = "sudo",
    sudo = "['ALL=(ALL) NOPASSWD:ALL']",
    shell = "/bin/bash",
    `ssh-authorized-keys` = NULL,
    add_ssh = function(path=NULL){
      pubkey <- tryCatch({
        get_pubkey(path)
      }, error = function(c){
        warning("error reading public key... will not set key")
        NULL
      })
      self$`ssh-authorized-keys` <- pubkey
      return(self)
    },
    initialize = function() self$add_ssh(),
    get_text = function(){
      ## check if ssh key exists, otherwise keep NULL for sub-block
      ssh_txt <- NULL
      if(!is.null(self$`ssh-authorized-keys`))
        ssh_txt <- c(paste0(tt, "ssh-authorized-keys:"), paste0(ttt, "- ", self$`ssh-authorized-keys`))

      txt <- c("users:",
               paste0(t, "- name: ", self$name),
               paste0(tt, "groups: ", self$groups),
               paste0(tt, "sudo: ", self$sudo),
               paste0(tt, "shell: ", self$shell),
               ssh_txt)
      return(txt)
    }
  )
)

WRITE_FILES <- R6Class(
  classname = "write_files",
  public = list(
    server_files = NULL,
    add_server_file = function(fpath){
      tmp <- readLines(fpath)
      lines <- tmp[!stringr::str_detect(tmp, " +$|^$")]
      fname <- str_replace(fpath, ".+(?=/+)/", "")

      # if file is already stored in contents, replace it
      if(fname %in% names(private$content)){
        ind <- -which(names(private$content) == fname)
        private$content <- private$content[ind]
        self$server_files <- self$server_files[ind]
      }
      server_path <- paste0("/", fname)

      # Remove comments only if .R file
      if(stringr::str_detect(fpath, ".+\\.R$"))
        lines <- lines[!stringr::str_detect(lines, "^#.+$| +$|^$")]

      content        <- list(lines)
      path           <- list(server_path)
      names(content) <- fname
      names(path)    <- fname
      private$content   <- c(private$content, content)
      self$server_files <- c(self$server_files, path)
    },
    # ensure any files in install_scripts is added by default
    initialize = function(){
      local_dir <- paste0(system.file("ext", "copy_to_server", package = "spawnr"), "/")
      files <- list.files(local_dir, full.names = TRUE)
      if(length(files) == 0) return(NULL)
      lapply(files, self$add_server_file)
      return(self)
    },
    get_text = function(){
      if(length(self$server_files)==0) return(NULL)
      paths <- self$server_files
      fnames <- names(paths)
      fcontents <- private$content

      private$queue <- NULL
      for(i in 1:length(paths)){
        k <- paths[i][[1]]
        x1 <- paste0(t, "- path: ", k, collapse="")
        x2 <- paste0(tt, "content: |", collapse="")
        x3 <- stringr::str_c(ttt, fcontents[fnames[i]][[1]])
        private$queue <- c(private$queue, x1, x2, x3)
      }
      txt <- c("write_files:", private$queue)
      return(txt)
    },
    get_contents = function() private$content
  ),
  private = list(
    path = NULL,
    content = NULL,
    queue = c()
  )
)

POWER_STATE <- R6Class(
  classname = "power_state",
  public = list(
    mode = "reboot",
    message = "Bye Bye",
    timeout = "30",
    condition = "True",
    get_text = function(){
      txt <- c("power_state:",
               paste0(t, "mode: ", self$mode),
               paste0(t, "message: ", self$message),
               paste0(t, "timeout: ", self$timeout),
               paste0(t, "condition: ", self$condition))
      return(txt)
    }
  )
)

RUNCMD <- R6Class(
  classname = "runcmd",
  public = list(
    add_cmd = function(...) private$cmd <- c(private$cmd, do.call("quote", list(substitute(...)))),
    get_text = function(){
      if(length(private$cmd)==0) return(NULL)
      txt <- c("runcmd:", paste0(t, "- ", private$cmd))
      return(txt)
    },
    get_cmds = function() private$cmd
  ),
  private = list(cmd = NULL)
)

PACKAGES <- R6Class(
  classname = "packages",
  public = list(
    add = function(pkg) private$packages <- c(private$packages, pkg),
    get_text = function(){
      if(length(private$packages)==0) return(NULL)
      txt <- c("packages:", paste0(t, "- ", private$packages))
      return(txt)
    }
  ),
  private = list(packages = NULL)
)

PKG_UPGRADE <- R6Class(
  classname = "package_upgrade",
  public = list(get_text = function() "package_upgrade: true")
)

CLOUDCONFIG <- R6Class(
  classname = "cloud-config",

  public = list(
    BOOTCMD     = NULL,
    USERS       = NULL,
    PKG_UPGRADE = NULL,
    PACKAGES    = NULL,
    WRITE_FILES = NULL,
    RUNCMD      = NULL,
    POWER_STATE = NULL,

    initialize = function(){
      self$BOOTCMD     <- BOOTCMD$new()
      self$USERS       <- USERS$new()         # new user
      self$PKG_UPGRADE <- PKG_UPGRADE$new()   # add new package upgrade command
      self$PACKAGES    <- PACKAGES$new()      # new packages chunk
      self$WRITE_FILES <- WRITE_FILES$new()   # new writefile. Note this will install any file in the package dir: copy_to_server/ by default
      self$RUNCMD      <- RUNCMD$new()        # new runcmd chunk
      self$POWER_STATE <- POWER_STATE$new()   # new power_state chunk
    },

    list_blocks = function(){
      bnames <- names(CLOUDCONFIG$public_fields)
      blks <- c(HEADER = private$header, sapply(bnames, function(i) do.call("$", list(self, i))$get_text()))
      return(blks)
    },
    make_file = function(){
      fpath <- paste0(system.file("log", package = "spawnr"), "/cloud_init.yml")

      ## if rstudio is active, write to file and open up. Else write to console
      if(rstudioapi::hasFun("navigateToFile")){
        lines <- unlist(cc$list_blocks())
        writeLines(lines, fpath)
        rstudioapi::navigateToFile(fpath)
      }else{
        writeLines(unlist(self$list_blocks()))
      }

    }
  ),
  private = list(header = "#cloud-config")
)

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


## Create brand new cloud-config file
cc <- CLOUDCONFIG$new()

## Add boot commands
cc$BOOTCMD$add("echo 'GITHUB_PAT=018ec6a3d3927f773a0d1b0adf516fa36b300929' >> /etc/environment")
cc$BOOTCMD$add("echo 'R_PROFILE=/usr/lib/R/etc/.Rprofile' >> /etc/environment")

# Add and R file to copy to the server after its configured
fpath <- system.file("ext", "install_scripts", "init_server.R", package = "spawnr")
cc$WRITE_FILES$add_server_file(fpath)

# add packages
cc$PACKAGES$add("apache2")
cc$PACKAGES$add("build-essential")
cc$PACKAGES$add("libxml2-dev")
cc$PACKAGES$add("libcurl4-openssl-dev")
cc$PACKAGES$add("libprotobuf-dev")
cc$PACKAGES$add("protobuf-compiler")

## add commands to runcmd
cc$RUNCMD$add_cmd('echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list')
cc$RUNCMD$add_cmd("gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9")
cc$RUNCMD$add_cmd("gpg -a --export E084DAB9 | sudo apt-key add -")
cc$RUNCMD$add_cmd("sudo add-apt-repository -y ppa:opencpu/opencpu-1.6")
cc$RUNCMD$add_cmd("sudo apt-get update -y")
cc$RUNCMD$add_cmd("sudo apt-get install -y r-base r-base-dev libssl-dev libcurl4-gnutls-dev libssh2-1-dev")
cc$RUNCMD$add_cmd("chmod +x /setup_rstudio_ocpu.sh")
cc$RUNCMD$add_cmd("sudo /setup_rstudio_ocpu.sh")
cc$RUNCMD$add_cmd("Rscript --vanilla /init_server.R > R_init_server_log")


cc$make_file()

