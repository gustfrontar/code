**ESTRATEGIA DE ESCANEO RMA1 9005: descripción**

Actualmente el radar RMA1 se encuentra configurado con la estrategia de escaneo 9005.
La misma cuenta con 4 (cuatro) volúmenes a distintos rangos cuya descripción se
detalla a continuación:

**01**: Volumen rango máximo 240km con 12 elevaciones (0.5°, 0.9°, 1.3°, 1.9°, 2.3°,
3.0°, 3.5°, 5.0°, 6.9°, 9.1°, 11.8° y 15.1°)
Variables disponibles: CM, KDP, PHIDP, RHOHV, TH, TV, VRAD y WRAD

**02**: Volumen rango máximo 120km con 8 elevaciones (0.5°, 1.3°, 2.3°, 3.5°, 5.0°,
11.8°, 15.1° y 19.2°)
Variables disponibles: CM, KDP, PHIDP, RHOHV, TH, TV, VRAD y WRAD

**03**: Volumen rango máximo 480km con 4 elevaciones (2.0°, 3.0°, 4.0° y 5.0°)
Variables disponibles: CM, KDP, PHIDP, RHOHV, TH, TV, VRAD y WRAD

**04**: Volumen calibración ZDR rango máximo 36km con 10 elevaciones a 90°
Variables disponibles: TH, TV, VRAD y WRAD

El nombre del archivo BUFR se estructura de la siguiente manera:

**RM1_9005_01_TH_yyyymmddTHHMMSSZ.BUFR**

Donde **RMA1** se refiere al nombre del radar, **9005** el número de estrategia con la
que se generó el archivo, **01** es el número que corresponde al tipo de escaneo
(ver descripción anterior), **TH** hace referencia a la variable (en este caso la
reflectividad horizontal), **yyyy** es el año, **mm** el mes, **dd** el día, **HH** la hora, **MM**
los minutos y **SS** los segundos. Este tiempo corresponde al momento de inicio del
escaneo del volumen.