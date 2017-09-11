.PHONY: all test_deployment figures env_R env_latex

all:

################################################################################
# figures and article

figures:
	$(MAKE) figures -C Figure1
	$(MAKE) figures -C Figure2
	$(MAKE) figures -C Figure3
	$(MAKE) figures -C Figure4
	$(MAKE) figures -C Figure5

article: figures
	$(MAKE) -C Article

################################################################################
# docker
IMAGE_NAME = cayek/tess3_article:latest
CONTAINER_NAME = tess3_article

include	docker.mk

################################################################################
# deployment

test_deployment:
	Rscript testdeployment.R

###############################################################################
# environment

env_R: Environment/dependencies.R Environment/R_dependencies.sh
	bash Environment/R_dependencies.sh
	Rscript --vanilla Environment/dependencies.R

env_latex: Environment/latex_dependencies.sh
		bash Environment/latex_dependencies.sh
