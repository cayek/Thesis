# TESS3 article scripts

R scripts, data and latex script used for the TESS3 article. Experiments presented in this paper can be reproduced with the script present in this repository.

## How to run R scripts

Each figure subdirectory contain the following subdirectory.

### Experiments

You can build experiments with this command line:
```
make experiments
```
However, running these scripts can be long. Script outputs are provided in RData files.

### Figures

You can build article figures with this command line:
```
make figures
```

### Rmarkdown

You can build rmarkdowns documents with this command line:
```
make rmarkdowns
```

## How to compile the article

The latex sources of the article can be compiled using this command:
```
make article
```

## Docker image

We provide a docker image which embed the software environment used for this project. Thus every build command can be run in the docker container. You can run a the container shell with this command lines:
```
make docker_run
make docker_shell
```
