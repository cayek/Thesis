red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

.PHONY: Rpackage_install_nodep krakenator_push_hook krakenator_deploy Rpackage_install krakenator_R fin_de_journee


# Rpackage

Rpackage_install_nodep:
	R --vanilla -e 'devtools::install(pkg = "./Rpackage/", dependencies = FALSE)'

Rpackage_install:
	R --vanilla -e 'devtools::install(pkg = "./Rpackage/")'

# krakenator 

krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

krakenator_deploy:
	git status
	git commit --allow-empty -am "deploy on krakenator"
	git push krakenator master

krakenator_R: 
	ssh -X -t cayek@krakenator "cd ~/Projects/Thesis/Rpackage; screen R"

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
GO: biblio_sync
	echo "${green}===== git status ======${reset}"
	git status
	echo "${green}===== git pull ======${reset}"
	git pull
