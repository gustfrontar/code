# OBS-TOOLS

Utility to download and process observational data for multiple users:
1. Data assimilation with WRF-LETKF and SCALE-LETKF
2. Forecast calibration

## REQUIREMENTS
- **Anaconda environment**: obs-tools
- **Git submodules**: [lib-utils](https://gitlab.smn.gov.ar/dmsr/utils/lib-utils.git)
- **Previous dependencies**: None

## INSTALLATION
1. As a module itself
```
git clone https://gitlab.smn.gov.ar/dmsr/tools/obs-tools.git
cd obs-tools/bin
```
Edit `config` file accordingly
```
cd ./install
./setup.sh <experiment_name>
```
2. As a git submodule
```
git submodule add https://gitlab.smn.gov.ar/dmsr/tools/obs-tools.git obs-tools
```

## USAGE

### Run experiment

```
cd ./bin/run
./run.sh <experiment_name> <time_increment (seconds, optional)> 
```

The job execution involves a sequential chain of 3 processes with the following execution logic:

- **run_setup.sh**: saves the LOG files and update the configuration. Runs on login node
- **run_download.sh**: gets observations from multiple data sources sotred in the HPC, ftp, web, etc.. Runs on the login node
- **run_process.sh**: processes the observations in the format requiered for the user. Runs on compute nodes


#### Exit codes
The exit code of a process can be:

- **0**: the process ended with OK. The job continues running.
- **1**: the process ended with ERROR. The job is terminated.
- **2**: the process ended with WARNING. THe job continues running.

#### Log files

Monitoring of the processes can be done from the files located in `RUNs/<experiment_name>/LOGS`:

- **main.log**: contains columns indicating the process order (STEP), the process name (PROCESS), the start (INI_TIMESTAMP) and end (END_TIMESTAMP) timestamp of process execution, the status of the process associated with its exit code (STATUS) and relevant information regarding the process (INFO).
- **main_PROCESS.log**: contains columns indicating the subprocess name or SLURM array number (ARRAY), the subprocess name (ID), the start (INI_TIMESTAMP) and end (END_TIMESTAMP) timestamp of subprocess execution, the status of the subprocess associated with its exit code (STATUS) and relevant information regarding the subprocess (INFO).

### Create new experiment
```
cd ./bin/install
./setup.sh <experiment_name>
```

## REPO MANAGMENT

For further information, requests or contributions, please contact the repository manager Federico Cutraro: fcutraro@smn.gob.ar

### TODO
- Agregar archivos de monitoreo con el valor medio, minimo y maximo de las observaciones
- Implementar 4D para SCALE
- Definir cómo tratamos, en la tabla de observaciones recibidas, los casos en que no llego el dato para diferenciarlo del caso en que llegó pero no hay observaciones útiles.
