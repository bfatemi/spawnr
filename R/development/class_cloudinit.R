


## Save tab variables, just for clarity
t   <- "  "
tt  <- paste0(t, t, collapse = "")
ttt <- paste0(t, t, t, collapse = "")


cloud_cmd <- function(param, value){
  paste0(param, ": ", value)
}


CC <- R6Class(
  classname = "cloud-config",
  public = list(
    add_block_class = function(...){
      ll <- list(...)
      tmp_names <- names(ll)

      # remove first the classes we want to overwrite
      ind <- which(private$cnames %in% tmp_names)
      if(length(ind) > 0){
        private$block_classes <- private$block_classes[-ind]
        private$cnames        <- private$cnames[-ind]
      }
      private$block_classes <- c(private$block_classes[ind], unlist(ll))
      private$cnames        <- c(private$cnames[ind], unlist(lapply(ll, names)))
    },
    construct = function(){
      res <- c(
        private$header,
        sapply(private$block_classes, function(bc) bc$get_text_chunk())
      )
      return(unlist(res[!sapply(res, is.null)]))
    }
  ),
  private = list(
    header = "#cloud-config",
    cnames = NULL,
    block_classes = NULL
  )
)

###
### BOOTCMD
###

BC <- R6Class(
  classname = "bootcmd",
  public = list(
    add = function(...){
      private$cmd <- c(private$cmd, do.call("quote", list(substitute(...))))
    },
    get_text_chunk = function(){
      if(length(private$cmd)==0)
        return(NULL)

      c("bootcmd:", paste0(t, "- ", private$cmd))
    },
    get_cmds = function(){
      private$cmd
    }
  ),
  private = list(
    cmd = NULL
  )
)



###
### USERS
###


USR <- R6Class(
  classname = "users",
  public = list(
    name = "ruser",
    groups = "sudo",
    sudo = "['ALL=(ALL) NOPASSWD:ALL']",
    shell = "/bin/bash",
    `ssh-authorized-keys` = NULL,
    add_ssh = function(path=NULL){
      tryCatch({
        pubkey <- get_pubkey(path)
      }, error = function(c){
        warning("error reading public key... will not set key")
        return(NULL)
      })
      if(length(pubkey) == 0){
        warning("no public key found")
        return(NULL)
      }
      self$`ssh-authorized-keys` <- pubkey
      return(self)
    },
    initialize = function(){
      self$add_ssh()
    },
    get_text_chunk = function(){
      c(
        "users:",
        paste0(t, "- name: ", self$name),
        paste0(tt, "groups: ", self$groups),
        paste0(tt, "sudo: ", self$sudo),
        paste0(tt, "shell: ", self$shell),
        c(paste0(tt, "ssh-authorized-keys:"), paste0(ttt, "- ", self$`ssh-authorized-keys`))
      )
    }
  )
)



###
### WRITE_FILES
###

WF <- R6Class(
  classname = "write_files",
  public = list(
    server_files = NULL,
    add_server_file = function(fpath){
      # fpath <- files[1]
      tmp <- readLines(fpath)                            # Read in file
      lines <- tmp[!stringr::str_detect(tmp, " +$|^$")]  # remove blank lines
      fname <- str_replace(fpath, ".+(?=/+)/", "")
      server_path <- paste0("/", fname)

      # Remove comments only if .R file
      if(stringr::str_detect(fpath, ".+\\.R$"))
        lines <- lines[!stringr::str_detect(lines, "^#.+$| +$|^$")]

      content <- list(lines)
      names(content) <- fname

      path <- list(server_path)
      names(path) <- fname

      private$content <<- c(private$content, content)
      self$server_files <<- c(self$server_files, path)
    },
    initialize = function(){
      # ensure any files in install_scripts is added by default
      local_dir <- paste0(system.file("ext", "copy_to_server", package = "spawnr"), "/")
      files <- list.files(local_dir, full.names = TRUE)

      if(length(files) == 0){
        return(NULL)
      }
      lapply(files, self$add_server_file)
      return(self)
    },
    get_contents = function(){
      return(private$content)
    },
    get_text_chunk = function(){

      if(length(self$server_files)==0)
        return(NULL)

      paths <- self$server_files
      fnames <- names(paths)
      fcontents <- private$content

      for(i in 1:length(paths)){
        k <- paths[i][[1]]
        x1 <- paste0(t, "- path: ", k, collapse="")
        x2 <- paste0(tt, "content: |", collapse="")
        x3 <- stringr::str_c(ttt, fcontents[fnames[i]][[1]])
        private$queue <- c(private$queue, x1, x2, x3)
      }
      c("write_files:", private$queue)
    }
  ),
  private = list(
    path = NULL,
    content = NULL,
    queue = c()
  )
)


###
### POWER_STATE
###

PS <- R6Class(
  classname = "power_state",
  public = list(
    mode = "reboot",
    message = "Bye Bye",
    timeout = "30",
    condition = "True",
    get_text_chunk = function(){
      c(
        "power_state:",
        paste0(t, "mode: ", self$mode),
        paste0(t, "message: ", self$message),
        paste0(t, "timeout: ", self$timeout),
        paste0(t, "condition: ", self$condition)
      )
    }
  )
)



###
### RUNCMD
###

RC <- R6Class(
  classname = "runcmd",
  public = list(
    add = function(...){
      private$cmd <- c(private$cmd, do.call("quote", list(substitute(...))))
    },
    get_text_chunk = function(){
      if(length(private$cmd)==0)
        return(NULL)

      c("runcmd:", paste0(t, "- ", private$cmd))
    },
    get_cmds = function(){
      private$cmd
    }
  ),
  private = list(
    cmd = NULL
  )
)


###
### PACKAGES
###

PKG <- R6Class(
  classname = "packages",
  public = list(
    add = function(pkg) private$packages <- c(private$packages, pkg),
    get_text_chunk = function(){
      if(length(private$packages)==0)
        return(NULL)
      lines <- c("packages:", paste0(t, "- ", private$packages))
      return(lines)
    }
  ),
  private = list(
    packages = NULL
  )
)


PU <- R6Class(
  classname = "package_upgrade",
  public = list(
    get_text_chunk = function() "package_upgrade: true"
  )
)


###
### START
###

bc  <- BC$new()
usr <- USR$new()  # new user
pkg <- PKG$new()  # new packages chunk
wf  <- WF$new()   # new writefile. Note this will install any file in the package dir: copy_to_server/ by default
ps  <- PS$new()   # new power_state chunk
rc  <- RC$new()   # new runcmd chunk
pu  <- PU$new()   # add new package upgrade command


# Add new boot commands
bc$add("echo 'GITHUB_PAT=018ec6a3d3927f773a0d1b0adf516fa36b300929' >> /etc/environment")
bc$add("echo 'R_PROFILE=/usr/lib/R/etc/.Rprofile' >> /etc/environment")

# Add new file
fpath <- system.file("ext", "install_scripts", "init_server.R", package = "spawnr")
wf$add_server_file(fpath)


## add commands to runcmd
rc$add('echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list)')
rc$add("gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9")
rc$add("gpg -a --export E084DAB9 | sudo apt-key add -")
rc$add("sudo add-apt-repository -y ppa:opencpu/opencpu-1.6")
rc$add("sudo apt-get update -y")
rc$add("sudo apt-get install -y r-base r-base-dev libssl-dev libcurl4-gnutls-dev libssh2-1-dev")
rc$add("chmod +x /install_rstudio_ocpu.sh")
rc$add("sudo /install_rstudio_ocpu.sh")
rc$add("Rscript --vanilla /init_server.R > R_init_server_log")

# add packages
pkg$add("apache2")
pkg$add("build-essential")
pkg$add("libxml2-dev")
pkg$add("libcurl4-openssl-dev")


## Complete file with text chunks
cfile <- CC$new()
cfile$add_block_class(bc, usr, pu, pkg, wf, rc, ps)
writeLines(cfile$construct())



