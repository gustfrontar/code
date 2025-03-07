### TODO ###
- RADARC: agregar id\_obs para cada radar (probar el codigo del LETKF) de manera de poder identificar en los archivos obs.dat a que radar pertenece cada obs de reflectividad asimiladas
- Revisar SO datos convencionales (similar a radar)
- Armar una clase para la reticula del SO? O una para el SO?
- Dowload AIRCFT with NRT=1

### CONSIDERACIONES ###
- Los slots los consideramos (] y centrados
- Los datos se redondean a 4 decimales
- *G16DMW*:
   1. Corresponden a la segunda imagen empleada para derivar los vientos (no se hace mas la correccion)
   2. Se combinan los canales disponibles en un solo archivo
- *ASCATW*:
   1. El nivel se setea segun los metadatos del archivo netCDF
- *ADPUPA*:
   1. Los errores se obtienen de WRFDA
 
### PREGUNTAS ###
- HR de superficie: el efso dio que la asimilación impacta negativamente (al menos con 20km de resolución). Ver si se asimilan como Td tal vez. Experimentos de sensibilidad para ver si se asimilan o no, si aumentamos el error. Por ahora seguimos como estan.

- Definir cómo trabajar con las observaciones de los slots 1 y 7: las repetimos? vemos de abrir el último binario y solo incluir en el slot 1 los datos que no estuvieron en el slot 7 del ciclo anterior? Los datos se empiezan a procesar una hora y media después de la hora actual. Redefinir los slot para no usar el 1 y no repetir observaciones. Chequear si llegan los sondeos!
