################################################################################
# setup
library(crayon)

exec <- function(cmd, temp.dir = NULL) {
  cat(green(paste0("== RUNNING: ",cmd,"\n")))
  out <- system(cmd, 
                ignore.stdout = TRUE, 
                ignore.stderr = FALSE)
  if (out != 0) {
    # remove temp dir and output
    cat(yellow(paste0("== REMOVING: ",temp.dir,"\n")))
    unlink(temp.dir, recursive = TRUE)
    stop(red(paste0("== ERROR: ",cmd)))
  }
}

################################################################################
# deployment test

## temp
dir <- tempdir()
setwd(dir)

## clone
exec("git clone --depth=1 git@github.com:cayek/TESS3Article.git")
setwd("TESS3Article")
tmp.dir <- paste0(dir,"/TESS3Article")

## retrieve binary data
exec("make pull_binary", temp.dir = tmp.dir)

## pull docker image 
## RMK : stop and rm container can be necessary before running
exec("make pull", temp.dir = tmp.dir)
exec("make run", temp.dir = tmp.dir)

## make
### figure1
exec("docker exec tess3_article bash -c 'cd Figure1/; make figures'" , temp.dir = tmp.dir)

### figure2
exec("docker exec tess3_article bash -c 'cd Figure2/; make figures'" , temp.dir = tmp.dir)

### figure3
exec("docker exec tess3_article bash -c 'cd Figure3/; make figures'" , temp.dir = tmp.dir)

### figure4
exec("docker exec tess3_article bash -c 'cd Figure4/; make figures'" , temp.dir = tmp.dir)

### figure5
exec("docker exec tess3_article bash -c 'cd Figure5/; make figures'" , temp.dir = tmp.dir)
exec("docker exec tess3_article bash -c 'cd Figure5/; make rmarkdowns'" , temp.dir = tmp.dir)

## stop docker container
exec("make stop", temp.dir = tmp.dir)
exec("make rm_container", temp.dir = tmp.dir)

cat(bgCyan("Ok for deployment !\n"))
