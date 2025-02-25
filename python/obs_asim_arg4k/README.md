# EXPERIMENTO OBS_ASIM_ARG4K

Procesa observaciones para el sistema de asimilación de datos (WRF-LETKF) en el dominio de Argentina de 4km de resolución.

## REQUERIMIENTOS

- **Entornos de anaconda**: obs-tools
- **Git submódulos**: [obs-tools](https://gitlab.smn.gov.ar/dmsr/tools/obs-tools.git)

## CONSIDERACIONES CONTROL-M
- **Dependencias previas**: obs_download
- **Forma de ejecución**: cada 30 minutos luego de satisfacer la dependencia.
- **Comandos previos**: es necesario setear la variable `Pre-execution command` como `cd /data/share/usr/prod/SAP.SMN-ANA/obs_asim_arg4k/bin/run


### PROCESOS
La ejecucion completa del paquete implica una cadena de 2 procesos secuenciales con la siguiente lógica:

![](./.DOCs/README_workflows-obs_asim_arg4k.png)

- **run_setup.sh**: resguarda los archivos LOG y actualiza la configuración. Ejecuta en el nodo login.
- **run_process.sh**: procesa los archivos de observaciones en el formato requerido para el sistema de asimilación WRF-LETKF. Ejecuta en los nodos de cómputo.


#### Códigos de salida
- **0**: el proceso terminó OK. El job sigue corriendo.
- **1**: el proceso terminó con ERROR. El job se termina.
- **2**: el proceso terminó con WARNING. El job sigue corriendo.


####  Archivos log 
El monitoreo de los procesos se puede hacer a partir de los archivos ubicados en la carpeta `RUNs/LOGS` del experimento.

- **main.log**: contiene columnas tabuladas indicando el orden del proceso (STEP), el nombre del proceso (PROCESS), el timestamp de inicio (INI_TIMESTAMP) y fin (END_TIMESTAMP) de ejecución del proceso, el estado del proceso asociado a su código de salida (STATUS) y la información relevente respecto del proceso (INFO).
- **main_PROCESS.log**: contiene columnas tabuladas indicando el nombre del subproceso o el numero de array de SLURM (ARRAY), el nombre del subproceso (ID), el timestamp de inicio (INI_TIMESTAMP) y fin (END_TIMESTAMP) de ejecución del subproceso, el estado del subproceso asociado a su código de salida (STATUS) y la información relevente respecto del subproceso (INFO).


### INSTALACION

### Inicial
Para realizar la instalación inicial del paquete:

```shell
git clone https://gitlab.smn.gov.ar/dmsr/dit/sap.smn-ana/obs_asim_arg4k.git
cd obs_asim_arg4k
git submodule update --init --recursive
```
```shell
cd ./bin/install
./install.sh obs_asim_arg4k
```

## ACTUALIZACIONES
Para realizar una actualización del paquete:

```shell
cd obs_asim_arg4k
git pull origin master
git submodule update --init --recursive
cd ./bin/pases
./run_pase.sh
 ```

### USO
Para realizar una ejecución del paquete por terminal:

```shell
cd ./bin/run
./run.sh obs_asim_arg4k
```
