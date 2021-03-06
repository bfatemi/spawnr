new_lines <- c(
  '#!/bin/bash',
  'sudo debconf-set-selections <<< "postfix postfix/mailname string hpds.io"',
  'sudo debconf-set-selections <<< "postfix postfix/main_mailer_type string \'Internet Site\'"',
  'sudo apt-get install -y postfix',
  'sudo apt-get install -y opencpu',
  'sudo apt-get install -y rstudio-server',
  'sudo rstudio-server restart'
)
writeLines(new_lines, "inst/ext/copy_to_server/setup_rstudio_ocpu.sh")
