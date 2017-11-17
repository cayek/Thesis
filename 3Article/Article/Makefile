red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`
.Phony: mount_Figs

## mount Figs dir on krakenator
mount_Figs:
	echo "${green}=====Mount Figs/=====${reset}"
	if [ $(HOSTNAME) == "timc-bcm-15.imag.fr" ] ; then \
		sshfs cayek@krakenator.imag.fr:/home/cayek/Projects/Thesis/3Article/Article/Figs Figs/ -o allow_other; \
	fi
