.PHONY: Rpackage_install_nodep krakenator_push_hook krakenator_deploy Rpackage_install krakenator_R fin_de_journee


# Rpackage

Rpackage_install_nodep:
	R --vanilla -e 'devtools::install(pkg = "./Rpackage/", dependencies = FALSE)'

Rpackage_install:
	R --vanilla -e 'devtools::install(pkg = "./Rpackage/")'

# krakenator 

krakenator_push_hook:
	scp ./hooks/post-receive.sh cayek@krakenator:~/GitRepo/Article3.git/hooks/post-receive

krakenator_deploy:
	git status
	git commit --allow-empty -am "deploy on krakenator"
	git push krakenator_deploy master

krakenator_R: 
	ssh -X -t cayek@krakenator "cd ~/Projects/Article3/Article3Package; screen R"

# kimsufi

kimsufi_push_hook:
	scp ./hooks/post-receive.sh cayek@176.31.253.205:/home/cayek/Gits/2017/Thesis.git/hooks/post-receive

kimsufi_deploy:
	git status
	git commit --allow-empty -am "deploy on kimsufi"
	git push kimsufi master


# End of day
fin_de_journee:
	git status
	git commit -am --allow-empty -am "Fin de journee"
	git push
