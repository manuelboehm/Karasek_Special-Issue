################
##### Update R and packages #####
################

# Check for Updates of R
# install.packages("installr")
library(installr)
updateR() 

# Check for Updates of packages
update.packages()


################
# install packages from other resources
################

# remotes::install_github("crsh/citr")
# remotes::install_github("paleolimbot/rbbt")
# remotes::install_github("ycphs/openxlsx")

## not used any more
# remotes::install_github("wviechtb/esmpack")
# remotes::install_github("Pakillo/grateful")
# remotes::install_github("ropensci/RefManageR")


################
##### problem tinytex #####
################

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
# update tinytex if problems occur
tinytex::reinstall_tinytex()


################
##### Github Setup 
################
# git config
use_git_config(user.name = "manuelboehm", user.email = "boehm.manuel@outlook.de")
# 
use_git()

# github config
create_github_token()
gitcreds_set()
use_github()

# Github-Implementation
# use_git_config(user.name = "manuelboehm", user.email = "boehm.manuel@outlook.de")

# tutorial
# https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

################
##### install wordcountaddin
################

devtools::install_github("benmarwick/wordcountaddin",  type = "source", dependencies = TRUE)


