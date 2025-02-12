## Pasos para convertir BUFR a CFRadial

Importar en una consola a la librería RadarMeteo

```
$ ipython
```

```python
>>> from RadarMeteo.conversores.RMA import bufr2cfradial

>>> path = '/path/to/data'

>>> bufr2cfradial(path)
```
