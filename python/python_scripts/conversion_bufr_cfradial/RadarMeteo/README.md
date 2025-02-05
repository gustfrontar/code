## **Argentina Radar Team - SMN/CIMA/INTA**

### **Team Local**

**Dr. Luciano Vidal** - Departamento de Investigación y Desarrollo / Servicio Meteorológico Nacional (SMN) - Argentina

**Dra. Paola Salio** - CIMA/CONICET - UMI IFAECI - Argentina

**Lic. Sofía Ruiz Suarez** (Graduada en Matemática) - Departamento de Investigación y Desarrollo / Servicio Meteorológico Nacional (SMN) - Argentina

**Bach. Cs. Atm. Martín Rugna** (Estudiante de grado en Meteorología) - Departamento de Investigación y Desarrollo / Servicio Meteorológico Nacional (SMN) - Argentina

**Lic. Romina Mezher** (Estudiante de doctorado en Meteorología) - Instituto de Clima y Agua / Instituto Nacional de Tecnología Agropecuaria (INTA) - Argentina

### **Consultor científico externo**
**Prof. Steve W. Nesbitt** (PhD) - Cloud Systems Research Group / Department of Atmospheric Sciences - University of Illinois at Urbana-Champaign (USA)


### **Descripción**

Repositorio para algoritmos y productos basados en datos de radares meteorológicos de simple y doble polarización en Argentina.

### **Clonando el repositorio**

Como el repositorio es privado, para clonarlo es necesario usar usuario y contraseña.
La mejor manera de hacerlo es la siguiente

```
git clone https://username@gitlab.smn.gov.ar/ID/RadarMeteo.git
```

### **Modo de uso**

Ir a la carpeta `RadarMeteo` y escribir el siguiente comando para instalar (o reinstalar en caso de haber hecho `git pull`)

```bash
python setup.py install --user
```

Luego, para utilizar en cualquier directorio de trabajo se importa usando

```python
import radarsmn

# Algunas variantes
from radarsmn import conversores
from radarsmn.conversores import RMA
```

Se puede consultar la documentación dentro de un intérprete:

- python
    - `help(radarsmn)`
- ipython
    - `radarsmn?`

#### Requerimientos

Por ahora, los requerimientos son:

- PyART
    - Instala Matplotlib
- Wradlib
    - Necesario para leer H5
- h5py

Es necesario actualizar esta lista conforme se vayan agregando modulos.
