C Lee el texto separado por ; y lo pasa a texto separado por espacios.
C Solo escribe los datos D, no incluye los 0 U ni los undef.

C**********************************************************************
C DECLARACION DE VARIABLES
C**********************************************************************
      character*5 id
      character*2 mes
      character*2 dia
      real hora,lat,lon,alt,vel,dir
     & ,tem,td,stn_pres,pres,pp,nub 
       integer anio
       
    
            
         
       open(15,file='in.txt',status='unknown'
     &  ,form='formatted')
       open(20,file='out.txt',
     &  status='unknown',form='formatted')

       read(15,*)
       read(15,*)
       read(15,*)
       read(15,*)
       read(15,*) 
       read(15,*)
       read(15,*)
       read(15,*)
       read(15,*)
       read(15,*)


100    read(15,*,END=200)anio,mes,dia,hora,id,lat
     & ,lon,alt,vel,cero,dir,cero_u,tem,cero_d,td
     & ,cero_t,stn_pres,cero_c,pres,cero_c,pp,cero_s,nub,cero_si 
      
       if(id(1:1).eq."8")then
       write(20,150)anio,mes,dia,id,lat,lon,hora,pp
       endif

       GOTO 100

200    write(*,*)"Fin del programa"

150    format(i4,a2,a2,a7,f7.2,f7.2,f6.0,f8.2)

       stop 
       end

     

