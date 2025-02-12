## Anaconda/Python en Rayo

### ¿Dónde está?

El directorio que contiene a Anaconda y Python2 dedicado a radar se encuentra en

```
/share/apps/anaconda2_radar
```

### ¿Cómo lo ejecuto?

Agregar a `~/.bash_profile` la siguiente línea

```bash
export PATH=”/share/apps/anaconda2_radar/bin:$PATH”
```

Para finalizar y asentar los cambios realizados, escribir

```bash
source ~/.bash_profile
```

*Nota:* Si no anda, sacar las comillas del `export` y ejecutar `source` nuevamente.

Ahora se puede ejecutar `python` desde la consola.

### ¿Dónde poner datos?

Está montado ms-36 como `/ms-36` igual que en los otros servidores.

### ¿Cómo usar Python en Rayo?

La idea en general (aplicable tanto a Rayo como a una computadora local) es usar el entorno `root` y que este sea el entorno más *limpio*, con las librerías que sabemos y probamos que andan. Ojo, el nombre no refiere al usuario *root* de Rayo, se llama así al entorno por defecto de Anaconda, o sea que cuando inician sesión en Rayo y ejecutan `python` va a iniciar el binario que está en `anaconda2_radar/bin`.

Las librerías instaladas se ven con

```
conda list
```

si antes no falló el import dentro de Python.

Si la lista es muy grande y/o no tienen ganas de buscar pueden hacer, por ejemplo para buscar PyART

```
conda list | grep pyart
```

donde `|` es una barra vertical conocida como pipe o tubo (pipe dirige la salida de un comando hacia la entrada de otro) y `grep` busca la palabra `pyart` en `conda list`.

#### Entornos virtuales

Al mismo tiempo hay 2 entornos virtuales en Python 2 y 3 para probar las actualizaciones o nuevas instalaciones y verificar que las dependencias que traigan las mismas no rompan nada. En principio, el entorno de Python2 va a ser un clon del entorno root con algunas pocas librerías en modo *test*; Python3 va a tener otro Anaconda con la mayor similitud posible a root para los valientes que quieran abandonar Python2.

Para ver qué entornos hay disponibles se usa el comando

```
conda env list
```

Los entornos se activan con un comando en la terminal de Rayo. Por ejemplo para iniciar el entorno de prueba en Python2 hay que escribir

```bash
source activate py2k
```

ó

```bash
source activate py3k  # Para Python3
```


Luego va a aparecer el nombre del entorno al lado del prompt

```
(py2k) [mrugna@rayo ~]$
```

Para salir del entorno hay que escribir

```
source deactivate
```
---
Se pueden ver las librerías instaladas en cada entorno para ver si hay alguna versión de prueba **sin necesidad de activar el entorno** agregando el nombre del entorno al comando `list` de conda

```
conda list -n py2k
```

De la misma manera se puede buscar una librería específica usando pipe y grep

```
conda list -n py2k | grep wradlib
```

Sé que esto es más engorroso pero aseguramos que el entorno root no se rompa, porque una vez que se rompió es bastante complicado volver hacia atrás. Por eso la pregunta siguiente.

### ¿Qué está instalado en Rayo? (alias para ¿Qué versiones específicas hay en el root de Rayo?)

Está instalado *Miniconda* (versión reducida de Anaconda) que no trae nada más que python, pip y conda. Esto significa que hay que instalar todas las librerías necesarias a la fuerza y la lista final puede diferir de las instalaciones locales de Anaconda.

Voy a agregar acá las librerías más 'importantes' para hacer un seguimiento del root. Eventualmente esta tabla se puede mudar a un documento externo (ej. Google Docs) para hacer la edición más rápida.

Librería | Versión | Comentarios
:---: | :---: | :---:
NumPy | 1.11.1 | ---
mkl | 11.3.3 | ojo por si no funciona
