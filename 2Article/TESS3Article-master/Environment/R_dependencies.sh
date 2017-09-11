# Install some external dependencies.
apt-get update \
  && apt-get install -y libcurl4-openssl-dev libgdal1-dev libproj-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
