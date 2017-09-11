FROM cayek/dojo:latest

################################################################################
# ensempl api

## change working directory
# WORKDIR $HOME

## install ensembl dependencies
RUN apt-get update && \
    apt-get install -y mysql-client libmysqlclient-dev libssl-dev
RUN cpan DBI DBD::mysql

## clone git repositories
RUN mkdir -p src
WORKDIR $HOME/src
RUN git clone https://github.com/bioperl/bioperl-live.git
WORKDIR $HOME/src/bioperl-live
RUN git checkout bioperl-release-1-6-901
WORKDIR $HOME/src
RUN git clone https://github.com/Ensembl/ensembl-git-tools.git
ENV PATH $HOME/src/ensembl-git-tools/bin:$PATH
RUN git ensembl --clone api
RUN git clone https://github.com/Ensembl/ensembl-tools.git

## update bash profile
RUN echo >> $HOME/.profile && \
echo '# set ensembl perl libraries' >> $HOME/.profile && \
echo PERL5LIB=\$PERL5LIB:$HOME/src/bioperl-live >> $HOME/.profile && \
echo PERL5LIB=\$PERL5LIB:$HOME/src/ensembl/modules >> $HOME/.profile && \
echo PERL5LIB=\$PERL5LIB:$HOME/src/ensembl-compara/modules >> $HOME/.profile && \
echo PERL5LIB=\$PERL5LIB:$HOME/src/ensembl-variation/modules >> $HOME/.profile && \
echo PERL5LIB=\$PERL5LIB:$HOME/src/ensembl-funcgen/modules >> $HOME/.profile && \
echo export PERL5LIB >> $HOME/.profile && \
echo >> $HOME/.profile && \
echo '# set ensembl tools in path' >> $HOME/.profile && \
echo PATH=$HOME/src/ensembl-git-tools/bin:\$PATH && \
echo PATH=$HOME/src/ensembl-tools/scripts/assembly_converter:\$PATH >> $HOME/.profile && \
echo PATH=$HOME/src/ensembl-tools/scripts/id_history_converter:\$PATH >> $HOME/.profile && \
echo PATH=$HOME/src/ensembl-tools/scripts/region_reporter:\$PATH >> $HOME/.profile && \
echo PATH=$HOME/src/ensembl-tools/scripts/variant_effect_predictor:\$PATH >> $HOME/.profile && \
echo export PATH >> $HOME/.profile

## setup environment
ENV PERL5LIB $PERL5LIB:$HOME/src/bioperl-live:$HOME/src/ensembl/modules:$HOME/src/ensembl-compara/modules:$HOME/src/ensembl-variation/modules:$HOME/src/ensembl-funcgen/modules
ENV PATH $HOME/src/ensembl-tools/scripts/assembly_converter:$HOME/src/ensembl-tools/scripts/id_history_converter:$HOME/src/ensembl-tools/scripts/region_reporter:$HOME/src/ensembl-tools/scripts/variant_effect_predictor:$PATH

################################################################################
# vep installation

#WORKDIR /root

#ADD https://github.com/Ensembl/ensembl-tools/archive/release/84.zip /root/

#RUN apt-get update && \
#    apt-get install -y unzip tabix curl && \
#    unzip 84.zip && \
#    rm 84.zip

#RUN cpan Archive::Extract DBI CGI Archive::Zip File::Copy::Recursive   # JSON Sereal -- these 2 additional perl modules are used by VEP tests. Leaving them out for now until they re needed

#RUN cd /root/ensembl-tools-release-84/scripts/variant_effect_predictor && \
#    perl INSTALL.pl --AUTO ap --PLUGINS all --NO_HTSLIB

################################################################################
# R package

ADD ./Environment/R_dependencies.sh /tmp/R_dependencies.sh
ADD ./Environment/dependencies.R /tmp/dependencies.R

RUN /bin/bash /tmp/R_dependencies.sh
RUN Rscript --vanilla /tmp/dependencies.R
################################################################################
# latex

ADD ./Environment/latex_dependencies.sh /tmp/latex_dependencies.sh
RUN bash /tmp/latex_dependencies.sh
