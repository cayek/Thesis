apt-get update && apt-get install -y texlive-science imagemagick latexdiff

 tlmgr init-usertree \
 && tlmgr option repository ftp://tug.org/historic/systems/texlive/2013/tlnet-final \
#      && tlmgr --no-persistent-downloads update --all \
     && tlmgr --no-persistent-downloads install mathdesign \
     && tlmgr --no-persistent-downloads install babel-french \
     && tlmgr --no-persistent-downloads install graphics-def \
     && tlmgr --no-persistent-downloads install xcolor \
     &&	tlmgr --no-persistent-downloads install preview \
     &&	tlmgr --no-persistent-downloads install tikz-cd \
     &&	tlmgr --no-persistent-downloads install pgf \
     && tlmgr --no-persistent-downloads install ifmtarg \
     && tlmgr --no-persistent-downloads install xifthen \
     && tlmgr --no-persistent-downloads install lineno \
     && tlmgr --no-persistent-downloads install lettrine \
     && tlmgr --no-persistent-downloads install datetime \
     && tlmgr --no-persistent-downloads install etoolbox \
     && tlmgr --no-persistent-downloads install fmtcount \
     && tlmgr --no-persistent-downloads install enumitem \
     && tlmgr --no-persistent-downloads install authblk \
     && tlmgr --no-persistent-downloads install lineno
