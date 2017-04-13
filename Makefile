red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

HOSTNAME=`hostname`

.PHONY: Rpackage_install_nodep krakenator_push_hook krakenator_deploy Rpackage_install krakenator_R fin_de_journee


# Rpackage
Rpackage_install_nodep:
	R -e 'devtools::install(pkg = "./ThesisRpackage/", dependencies = FALSE)'

Rpackage_install:
	R CMD INSTALL --no-multiarch --with-keep.source ThesisRpackage

# krakenator 
krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

krakenator_deploy:
	git status
	git commit --allow-empty -am "deploy on krakenator"
	git push krakenator master

krakenator_R: 
	ssh -X -t cayek@krakenator "cd ~/Projects/Thesis/; screen R"

## Data/
krakenator_mount_data:
	echo "${green}=====Mount Data/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/Data Data -o allow_other; \
	fi

krakenator_umount_data:
	echo "${green}=====Umount Data/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sudo umount Data/; \
	fi

## OUTPUT
krakenator_mount_OUTPUT:
	echo "${green}=====Mount OUTPUT/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/OUTPUT OUTPUT -o allow_other; \
	fi

krakenator_umount_OUTPUT:
	echo "${green}=====Umount OUTPUT/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sudo umount OUTPUT; \
	fi

# patator
patator_push_hook:
	scp ./hooks/post-receive.sh cayek@patator:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

patator_deploy:
	git status
	git commit --allow-empty -am "deploy on patator"
	git push patator master

patator_R: 
	ssh -X -t cayek@patator "cd ~/Projects/Thesis/; screen R"


# kimsufi
kimsufi_push_hook:
	scp ./hooks/post-receive.sh cayek@176.31.253.205:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

kimsufi_deploy:
	git status
	git commit --allow-empty -am "deploy on kimsufi"
	git push kimsufi master

# Biblio
biblio_sync:
	echo "${green}=====BIBLIO SYNC=====${reset}"
	cd Biblio; git add -A
	cd Biblio; git commit --allow-empty -am "Sync"
	cd Biblio; git pull
	cd Biblio; git push
	echo "${green}=====BIBLIO STATUS=====${reset}"
	cd Biblio; git status

# End of day
fin_de_journee: biblio_sync
	echo "${green}===== git status ======${reset}"
	git status
	echo "${green}===== git commit and push ======${reset}"
	git commit --allow-empty -am "Fin de journee"
	git push
	echo "${green}===== git status ======${reset}"
	git status

# Go
GO: biblio_sync krakenator_mount_OUTPUT krakenator_mount_data
	echo "${green}===== git status ======${reset}"
	git status
	echo "${green}===== git pull ======${reset}"
	git pull
