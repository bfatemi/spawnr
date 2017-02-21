#' Cloud-Config YAML File
#'
#' Functions to generate the cloud-config file required to initialize a new cloud-r server.
#'
#' @param boot_lines character vector of commands to run at boot
#' @param ssh_path path to desired ssh public key (for each user)
#' @param pkgs apt-get packages to install
#' @param write_path path on the remote server to write new file
#' @param content contents of new file to write on remote server
#' @param cmd_lines character vector of commands to run after remote server up and running
#' @param reboot boolean. TRUE (default) to reboot cloud server after init
#'
#' @importFrom yaml yaml.load
#'
#' @name cloud_config
NULL

#' @describeIn cloud_config bootcmd
#' @export
cc_bootcmd <- function(boot_lines = NULL){
  yml <- yaml::yaml.load("bootcmd:")
  yml$bootcmd <- boot_lines
  if(is.null(boot_lines)) return(NULL)
  return(yml)
}


#' @describeIn cloud_config user block
#' @export
cc_users <- function(ssh_path = NULL){
  txt <- "users:\n- name: ruser\n  groups: sudo\n  sudo: ['ALL=(ALL) NOPASSWD:ALL']\n  shell: /bin/bash\n  ssh-authorized-keys:"
  yml <- yaml::yaml.load(txt)

  if(!is.null(ssh_path)){
    yml$`ssh-authorized-keys` <- readLines(ssh_path)
  }else{
    yml$`ssh-authorized-keys` <- NULL
  }
  yml[!sapply(yml, is.null)]
  return(yml)
}

#' @describeIn cloud_config apt-get packages
#' @export
cc_packages <- function(pkgs = NULL){
 txt <- "package_upgrade: true\npackages:"
 yml <- yaml::yaml.load(txt)
 yml$packages <- pkgs
 yml[!sapply(yml, is.null)]
}

#' @describeIn cloud_config write files on server
#' @export
cc_write_files <- function(write_path = NULL, content = NULL){
  txt <- "write_files:\n- path:\n  content: "
  yml <- yaml::yaml.load(txt)

  yml$write_files[[1]]$path <- write_path
  yml$write_files[[1]]$content <- content

  if(is.null(write_path))
    return(NULL)
  return(yml)
}


#' @describeIn cloud_config run commands
#' @export
cc_runcmd <- function(cmd_lines = NULL){
  yml <- yaml::yaml.load("runcmd:")
  yml$runcmd <- cmd_lines
  if(is.null(cmd_lines)) return(NULL)
  return(yml)
}


#' @describeIn cloud_config used for reboot
#' @export
cc_power_state <- function(reboot = TRUE){
  if(reboot == FALSE)
    return(NULL)

  txt <- "power_state:\n- mode: reboot\n  message: Bye Bye\n  timeout: 30\n  condition: True"
  yml <- yaml::yaml.load(txt)
  yml
}


# fpath <- cc_rstudio_server()
# rstudioapi::navigateToFile(fpath)
# yml_obj <- yaml::yaml.load_file(fpath)
#
#
# cc <- yaml::yaml.load_file("inst/ext/temp.yml")
# cc$bootcmd
# cc_bootcmd()$bootcmd
#
# cc$users
# cc_users()$users
#
# cc$package_upgrade
# cc$packages
# cc_packages()
#
#
# cc$write_files
# cc_write_files()
#
# cc$runcmd
# cc_runcmd()
#
# cc$power_state
# cc_power_state()$power_state
#
# ssh_path = "C:\\Users\\Bobbyf\\.ssh\\known_hosts"
