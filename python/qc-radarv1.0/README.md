# Control de Calidad de datos de Radar Meteorológico 

Este módulo realiza el control de calidad y el superobbing de los datos de radar meteorologico.

## Indice
- [**Instalación**](#instalación)
- [**Configuración**](#configuracion-del-sistema)
- [**Módulo de Control de Calidad**](#módulo-de-control-de-calidad)
- [**Estructura del código**](#estructura-del-codigo)
- [**Agregar un radar**](#consideraciones-al-agregar-un-radar)

## Instalación
### Modulos de python necesarios.
 Los siguiente modulos fueron probados y son compatibles.
 
 * arm_pyart
 * cartopy
 * elevation
 * gdal
 * scikit-learn
    
### Obtener el último código fuente

El último código fuente de qc-radar se puede obtener del repositorio de GitLab, https://gitlab.smn.gob.ar/ID/qc-radarV1.0.

El último código fuente puede bajarse usando 

    $ git clone https://gitlab.smn.gob.ar/ID/qc-radarV1.0.git


### Compilar los módulos de fortran

    cd utils
    ./make_gfortran.sh

## Configuracion del Sistema
### Editar archivos de configuracion del Sistema

- **config.ini**
Especialmente las secciones 
    [System]
    [SLURM]



### Descripcion de los modos de ejecucion


Hay dos modos de uso implementados:

- **Slurm**: Corre el control de calidad encolando el proceso en un nodo de HM.
- **Local**: Corre el control de calidad de manera local.

que deben configurarse en la variable `MODO` en la seccion `System` del `config.ini`

### Archivos de configuracion de los filtros.
Por cada configuracion debe existir un archivo `.qcr` y todas las configuraciones deben listarse en la variable `USERCONF` en la seccion `System` del `config.ini`
Por ejemplo:

- **asimilacion.qcr**: Se setean la configuracion de los filtros del control de calidad y otras variables como por ejemplo radares y path de salida que se utilizaran en asimilacion.
- **tiemporeal.qcr**: Se setean la configuracion de los filtros del control de calidad y otras variables como por ejemplo radares y path de salida que se utilizaran en tiempo real.


## Módulo de Control de Calidad

## Modo de uso
Debe cargarse previamente el entonro de conda creado


    source activate qc-radar
o
    
    conda activate qc-radar

y luego ejecutar el archivo master.py
    
    python master.py
    
## Estructura del Codigo
### Diseño:

Va a existir un master que cree un dataframe y workers que actúen en consecuencia de lo que figure en el mismo. Es decir, que ejecute el QC para aquellos archivos disponibles pero no lo haga para los archivos en proceso o que ya han pasado por el QC.

<img src="./DOCs/diagrama.png" align="center" >

#### Master 
- **Buscar Volumenes**:
	Se recorre el directorio en busca de NUEVOS archivos de radar (radarID, tiempo, variable, estrategia, volumen) y crea un registro del DF para cada caso encontrado.

- **Despachar Workers**:
Genera una lista de variables disponibles para cada (RadarID, fechaini, config) en el DF
Para cada elemento de la lista de 1) se verifica que las variables disponibles son al menos las que están en config:varlistmin, entonces lanza un Worker (Radar ID,Fecha, config) 
Cambiar estado a “Procesando” para todos los registros asociados a cada worker creado en 2) ---> Escribir el DF

 - **Escribir DF**
Identificar todos los volúmenes de cada config que estén completos y procesados (se procesaron todas las variables que figuran en config:varlist) y eliminarlos del DF
Eliminar del DF todos los registros más viejos que : config:ventana_espera_variable
Actualizar el archivo físico del DF

#### Workers
Los workers leen los archivos de entrada, así se distribuye y paraleliza el I/O. Cada worker crea el Objeto Radar 

- **Inicio**:
parámetros de entrada:
RadarID, fechaini, config
busca todos los archivos de variables disponibles para (RadarID, estrategia, volumen, fechaini)
	
- **Procesar Volumen**:
La magia de qc-radar

- **Escribir DF**:
Para todos los registros (RadarID, estrategia, volumen, fechaini, config, VARIABLES) cambia el estado a Terminado.
Y escribe el archivo físico DF

### Estructura del DataFrame
El dataframe es una tabla con el siguiente encabezado

| file_input | file_output | fecha_ini | config | state | radar_id | estrategia | volumen | variable | file_ext |
|-|-|-|-|-|-|-|-|-|-|

- **Input file** : Nombre de entrada del archivo RAW que hay que procesar y está disponible
- **Output file**: Nombre del archivo de salida común a todas las variables. Representa un volumen de radar
- **fecha_ini**: Tiempo de inicialización del volumen al que pertenece el Input File
- **Config**: Configuración de los filtros que se aplican y la parametrización de los mismos. Podría ser un nombre de referencia, por ejemplo “asimilación” o “tiempoReal”
- **State**: estado de procesamiento. Por ejemplo D: disponible para procesar; T :terminado; P: procesando, etc.
- **radar_ID**: Identificador único de Radar
- **estrategia**: Identificado de estrategia
- **volumen**: Identificador de volumen
- **variable**: Variable  de radar
- **File EXT**: Extensión del archivo InputFile. Para saber cómo leerlo

Por ejemplo:

| radar_id | fecha_ini | file_input | file_output | file_ext | config | state |
|-|-|-|-|-|-|-|
| RMA1 | 2020-08-14T12:00:00Z | /path/to/RMA1_fecha_DBZH.H5 | /path/to/RMA1_fecha.nc | H5 | tiempoReal | P |
| RMA1 | 2020-08-14T12:00:00Z | /path/to/RMA1_fecha_DBZH.H5 | /otro/path/to/RMA1_fecha.nc | H5 | asimilacion | T |
| PAR | 2020-08-14T12:00:04Z | /path/to/PAR_fecha.vol | /path/to/PAR_fecha.nc | vol | asimilacion | D |

### super_radar

Los datos de entrada son objetos radar que son extendidos a un objeto más abarcativo llamado `super_radar`. 

### filtros

Toda la información del `super_radar` será usada para calcular los filtros. Cada filtro es una `class` de python que hereda métodos de una clase que contiene métodos más generales y se llama `BaseFilter`. 

#### BaseFilter

La clase `BaseFilter` tiene los métodos necesarios para actualizar campos corregidos, que se guardan en el atributo `output` y también los métodos para actualizar el objeto radar inicial con los campos corregidos.

#### Filtros de control de calidad

Cada filtro de control de calidad tiene una clase asociada con el nombre `NombreFilter` donde `Nombre` es `Dealiasing`, `DopplerRef`, `Attenuation` etc. Cada clase tiene métodos para calcular índices, correr el filtro y actualizar el `output`

## Consideraciones al agregar un radar

Agregar un radar es más que agregarlo al archivo de configuración de los filtros. En particular es necesario agregar información relativa al radar en el archivo `utils/radar.py` dentro de las funciones:

- `get_strat`
- `get_nyquist_velocity`
- `correct_radar_altitude`





