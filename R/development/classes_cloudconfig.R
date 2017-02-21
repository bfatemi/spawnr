#' Internal Cloud-Init Block Classes
#'
#' Classes used to construct each block of the total cloud init file.
#'
#' @name cc_block_classes
NULL

## Save tab variables, just for clarity
t   <- "  "
tt  <- paste0("  ", "  ", collapse = "")
ttt <- paste0("  ", "  ", "  ", collapse = "")

# cc_block_classes block for boot -----------------------------------------


BOOTCMD <- R6::R6Class(
  classname = "bootcmd",
  public = list(
    add = function(...){
      private$cmd <- c(private$cmd, do.call("quote", list(substitute(...))))
      self$get_text()
    },
    get_text = function(){
      if(length(private$cmd)==0) return(NULL)
      txt <- c("bootcmd:", paste0(t, "- ", private$cmd))
      return(txt)
    },
    get_cmds = function() private$cmd
  ),
  private = list(cmd = NULL)
)


# cc_block_classes block for powerstate -----------------------------------


POWER_STATE <- R6::R6Class(
  classname = "power_state",
  public = list(
    mode = "reboot",
    message = "Bye Bye",
    timeout = "30",
    condition = "True",
    get_text = function(){
      txt <- c("power_state:",
               paste0("  ", "mode: ", self$mode),
               paste0("  ", "message: ", self$message),
               paste0("  ", "timeout: ", self$timeout),
               paste0("  ", "condition: ", self$condition))
      return(txt)
    }
  )
)



# cc_block_classes block for run commands ---------------------------------


RUNCMD <- R6::R6Class(
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



# cc_block_classes block for userconfig -----------------------------------


USERS <- R6::R6Class(
  classname = "users",
  public = list(
    name = "ruser",
    groups = "sudo",
    sudo = "['ALL=(ALL) NOPASSWD:ALL']",
    shell = "/bin/bash",
    `ssh-authorized-keys` = NULL,

    add_ssh = function(path=NULL){
      self$`ssh-authorized-keys` <- get_pubkey(choice = 1)
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



# cc_block_classes block for files to write on server ---------------------


WRITE_FILES <- R6::R6Class(
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
    initialize = function(files = NULL){
      if(is.null(files)){
        pdir <- system.file("ext", "copy_to_server", package = "spawnr")
        local_dir <- paste0(pdir, "/")
        files <- list.files(local_dir, full.names = TRUE)
      }
      if(length(files) == 0)
        return(NULL)
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




# cc_block_classes block for packages to install --------------------------


PACKAGES <- R6::R6Class(
  classname = "packages",
  public = list(
    initialize = function(pkgs=NULL){
      if(!is.null(pkgs))
        lapply(pkgs, self$add)
      self$get_text()
    },
    add = function(pkg) private$packages <- c(private$packages, pkg),
    get_text = function(){
      if(length(private$packages)==0) return(NULL)
      txt <- c("packages:", paste0(t, "- ", private$packages))
      return(txt)
    }
  ),
  private = list(packages = NULL)
)



# cc_block_classes block for package upgrade ------------------------------


PKG_UPGRADE <- R6::R6Class(
  classname = "package_upgrade",
  public = list(get_text = function() "package_upgrade: true")
)



# cc_block_classes class to bundle all blocks and produce desired  --------


CLOUDCONFIG <- R6::R6Class(
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
      txt <- sapply(bnames, function(i){
        blk <- do.call("$", list(self, i))
        if(is.null(blk))
          return(NULL)
        blk$get_text()
      })
      blks <- c(HEADER = private$header, txt)
      return(blks)
    },

    make_file = function(ll=NULL, gotofile=FALSE){
      if(is.null(ll)){
        lines <- unlist(self$list_blocks())
      }else{
        lines <- unlist(ll)
      }

      fpath <- system.file("log", package = "spawnr")
      if(fpath == ""){
        fpath <- paste0(system.file(package = "spawnr"), "/log")
        dir.create(fpath)
      }
      fpath <- paste0(fpath, "/cloud_init.yml")
      lines <- unlist(self$list_blocks())
      writeLines(lines, fpath)

      ## if rstudio is active, write to file and open up. Else write to console
      if(gotofile){
        if(rstudioapi::hasFun("navigateToFile")){
          rstudioapi::navigateToFile(fpath)
        }else{
          writeLines(unlist(self$list_blocks()))
        }
      }
      return(fpath)
    }
  ),
  private = list(header = "#cloud-config")
)



