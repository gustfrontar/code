c     EL FORMATO DE LECTURA CORRESPONDE A LOS DATOS DE WYOMING (EL FORMATO DE LECTURA ES FIJO, ver format 15)
c     EL FORMATO DE SALIDA ES EL QUE REQUIERE EL ALGORITMO DE MATLAB.
C     Autor: Juan Ruiz ----- 2006
C*************************************************************************************************************

      parameter (k=500,il=1,kl=1,nvars=7,nsond=2)

      real dir(k),vel(k),z(k),tc(k),hr(k),p(k),td(k),q(k)
      real out(nsond,k,nvars),nivel(k)
      INTEGER ndatos
      character*20 archivo_in,archivo_out


      archivo_in="./sondeo.input"
      archivo_out="./matlab/input.txt"

      call read(archivo_in,ndatos,p,z,tc,td,hr,
     & q,dir,vel,k)
 
c     Escritura de datos en un archivo binario
      call write(archivo_out,ndatos,z,tc,td,hr,
     & q,dir,vel,p,k)


        stop
        end



c     ************************************************************
c     ************************************************************
c     SUBRUTINA LECTURA DE DATOS
      subroutine read (file_name
     &,n,pr,zr,tcr,tdr,hrr,qr,dirr,velr,k)
c     Declaracion de variables
      integer k !Si se compila con intel hay que comentar esta linea.
      real dirr(k),velr(k),zr(k),tcr(k)
     & ,hrr(k),pr(k),tdr(k),qr(k)
      character*20 file_name,nada
      integer n

      !write(*,*)"Inicio de la lectura de datos: ",file_name

      open(50,file=file_name,status='unknown')
        i=1
        !leo el encabezado que tiene siempre 10 lineas.
        do jj=1,10
        read(50,*)nada
        enddo
10      read(50,15,END=20,ERR=20)pr(i),zr(i),tcr(i),tdr(i),hrr(i),
     & qr(i),dirr(i),velr(i)

        if(qr(i).eq.0)then
        qr(i)=-999
        endif
        if(hrr(i).eq.0)then
        tdr(i)=-999
        endif 
        if(hrr(i).eq.0)then
        hrr(i)=-999
        endif
        if(tcr(i).eq.0)then 
        tcr(i)=-999
        endif
        if(dirr(i).eq.0.and.velr(i).eq.0)then
        dirr(i)=-999
        velr(i)=-999
        endif
        if(pr(i).lt.pr(i-1).or.i.eq.1)then
!        write(*,15)pr(i),zr(i),tcr(i),tdr(i),hrr(i),
!     & qr(i),dirr(i),velr(i)
        i=i+1
        endif


        goto 10
15      format(f8.2,f8.0,f7.2,f8.2,f6.0,f7.2,f7.0,f6.0)
20        n=i-1
!        write(*,*)n
        close (50)
        return

        end

c     ************************************************************
c     ************************************************************
c     SUBRUTINA ESCRITURA DE DATOS
      subroutine write (file_name,n,zr,tcr,tdr,hrr,qr,dirr,velr,pr,k)
c     Declaracion de variables
      integer k
      real dirr(k),velr(k),zr(k),tcr(k),out(k,10)
     & ,hrr(k),pr(k),tdr(k),qr(k),error(k)
      character*20 file_name
      integer n


      open(60,
     &form='formatted',
     &status='UNKNOWN',file=file_name) 

c     ESCRITURA DE DATOS EN FORMATO  ASCII
      do j=1,n
      if(j.eq.1.or.pr(j).ne.pr(j-1))then
      if(pr(j).ne.-999)then
       if(tcr(j).ne.-999)then  
        if(hrr(j).ne.-999)then
         write(60,70)pr(j),tcr(j),hrr(j),dirr(j),velr(j)
        else
         if(pr(j).le.350)then
!Si la humedad es undef pero el nivel está por encima de 350 rellenamos la humedad con 01%
!La idea de esto es poder continuar el CAPE hasta niveles superiores en donde las mediciones 
!de humedad frecuentemente no están disponibles.
         hrr(j)=1.
         write(60,70)pr(j),tcr(j),hrr(j),dirr(j),velr(j)
         endif
        endif
70      format(f7.1,f6.1,f6.0,f6.0,f7.1)
        
      
       endif
      endif
      endif
 
      enddo

        close (60)
        return

        end




