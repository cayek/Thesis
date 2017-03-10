#!/bin/sh
dest=/home/cayek/Projects/Thesis
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

echo "${green}post-receive hook${reset}"

while read oldrev newrev ref
do
    if [[ $ref =~ .*/master$ ]];
    then
        echo "${green}Master ref received.  Deploying master branch to production...${reset}"
        git --work-tree=$dest --git-dir=/home/cayek/Gits/2017/Thesis.git checkout -f
	      cd $dest
        make Rpackage_install
    else
        echo "${green}Ref $ref successfully received.  Doing nothing: only the master br
anch may be deployed on this server.${reset}"
    fi
done
