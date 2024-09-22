MODULE MINIMUN_TOOLS

!==========================================================================
!Este modulo encuentra todos los minimos locales en un campo bidimensional
!asumiendo que se trata de una reticula global regular
!==========================================================================
!El modulo lee las matrices de datos y de la climatologia. Tambien puede
!leer variables adicionales que este en la misma reticula.

!La informacion que se genera es la siguiente:
!nminimos=nminimos;                %N de minimos.
!mini                              %i de cada minimo.
!minj                              %j de cada minimo.
!id                                %id de cada minimo

!PARA MAS INFORMACION SOBRE LOS METODOS DE CALCULO DE LAS DIFERENTES
!CANTIDADES VER LOS COMENTARIOS DEL CODIGO.
IMPLICIT NONE
!=============================================
!GRID CHARACTERISTICS
!=============================================
PUBLIC
INTEGER, PARAMETER :: nx=360*2 !180 !Grid dimensions. 
INTEGER, PARAMETER :: ny=180*2  !91  !Grid dimensions.

!=============================================
!CONFIGURATION PARAMETERS
!=============================================

INTEGER, PARAMETER :: MAXM=200  !Maximun number of minimuns.
INTEGER, PARAMETER :: MAXSISSIZE=20*nx
INTEGER, PARAMETER :: NF=1      !Number of additional fields.
REAL   , PARAMETER :: WEXP=1    !Weightning exponent for system centroid computation
LOGICAL, PARAMETER :: DEBUG=.FALSE.
REAL   , PARAMETER :: UMB_ANOM=-20.
REAL   , PARAMETER :: UMB_MINBARRIER=0.
REAL   , PARAMETER :: UMB_CERCANIA=1000e3
REAL   , PARAMETER :: INTERP_TYPE=0                !INTERP TIME FOR SUBGRID MINIMUN 1, MEAN, 2 LEASTE SQUARES QUADRATIC INTERPOLATION
LOGICAL, PARAMETER :: READ_ADDITIONAL_DATA=.FALSE. !If additional data will be present and we have to consider it.
LOGICAL, PARAMETER :: YREV=.TRUE.                  !YREV ON INPUT DATA?
!==============================================
!OTHER VARIABLE DECLARATIONS
!==============================================
!VARIABLES FOR INPUT DATA
REAL,ALLOCATABLE     ::  FIELD(:,:)
REAL,ALLOCATABLE     ::  ADDITIONALFIELD(:,:,:)
REAL,ALLOCATABLE     ::  FIELDLAT(:,:),FIELDLON(:,:)
!VARIABLES FOR FINDING MINIMUNS AND ITS ASSOCIATED SYSTEMS
INTEGER,ALLOCATABLE  ::  MASCARA(:,:)
INTEGER,ALLOCATABLE  ::  MASCARA_MINIMOS(:,:)
INTEGER,ALLOCATABLE  ::  MIN_I(:),MIN_J(:)
REAL, ALLOCATABLE    ::  MINLAT(:),MINLON(:),MINLATINT(:),MINLONINT(:)
REAL, ALLOCATABLE    ::  MINLATCENT(:),MINLONCENT(:)   !Latitude and longitud of system centroid.
INTEGER, ALLOCATABLE ::  MIN_GRID_SIZE(:)  !System size in number of grid points.
INTEGER, ALLOCATABLE ::  MIN_INDEX(:,:)    !List of grid points (columnwise index) that are part of a system.
REAL,ALLOCATABLE     ::  MIN_AREA(:)       !System area (in sq kilometers).
REAL,ALLOCATABLE     ::  MEAN_ANOM_SIS(:)  !System mean anomaly.
REAL,ALLOCATABLE     ::  MIN_ANOM_SIS(:)   !System min anomaly.
INTEGER     ::  NMINIMOS
INTEGER,ALLOCATABLE  ::  MIN_ID(:)

REAL,ALLOCATABLE     ::  MIN_ADDITIONAL_DATA(:,:) !Minimun additional data over each system 
REAL,ALLOCATABLE     ::  MAX_ADDITIONAL_DATA(:,:) !Maximun additional data over each system
REAL,ALLOCATABLE     ::  MEAN_ADDITIONAL_DATA(:,:)!Mean additional data over each system.

CONTAINS
!===========================================================================
SUBROUTINE ALLOCATE_ARRAYS()

ALLOCATE(FIELD(ny,nx))
ALLOCATE(LAPLACIAN(ny,nx))
ALLOCATE(MASCARA(ny,nx))
ALLOCATE(MASCARA_MINIMOS(ny,nx))
ALLOCATE(MIN_I(MAXM),MIN_J(MAXM))
ALLOCATE(FIELDLAT(ny,nx),FIELDLON(ny,nx))
ALLOCATE(MIN_ID(MAXM))
ALLOCATE(MIN_LAP(MAXM))
ALLOCATE(MINLON(MAXM),MINLAT(MAXM))
ALLOCATE(MINLONCENT(MAXM),MINLATCENT(MAXM))
ALLOCATE(MINLONINT(MAXM),MINLATINT(MAXM))
ALLOCATE(MIN_GRID_SIZE(MAXM))
ALLOCATE(MIN_INDEX(MAXM,MAXSISSIZE))
ALLOCATE(MIN_AREA(MAXM))
ALLOCATE(MEAN_ANOM_SIS(MAXM))
ALLOCATE(MIN_ANOM_SIS(MAXM))
IF(READ_ADDITIONAL_DATA)THEN
ALLOCATE(ADDITIONALFIELD(ny,nx,nf))
ALLOCATE(MIN_ADDITIONAL_DATA(MAXM,nf))
ALLOCATE(MAX_ADDITIONAL_DATA(MAXM,nf))
ALLOCATE(MEAN_ADDITIONAL_DATA(MAXM,nf))
ENDIF

RETURN
END SUBROUTINE ALLOCATE_ARRAYS

!===========================================================================
SUBROUTINE READ_DATA()
!This subrutine will read the data, the corresponding climatology and any 
!additional data. 
IMPLICIT NONE
CHARACTER*20  :: DATAFILE='datainput.bin',CLIMFILE='climinput.bin',ADDFILE='addinput.bin'
CHARACTER*20  :: LATFILE='latinput.bin',LONFILE='loninput.bin'
INTEGER       :: iolen,IREC,ii,i,j
REAL          :: CLIMFIELD(ny,nx)

CLIMFIELD=0.0

INQUIRE(IOLENGTH=iolen) iolen

OPEN(UNIT=61,FILE=DATAFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen)
!OPEN(UNIT=61,FILE=DATAFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen) 
   IF(YREV)THEN
    READ(61,IREC=1)((FIELD(ny-j+1,i),i=1,nx),j=1,ny)
    READ(61,IREC=2)((LAPLACIAN(ny-j+1,i),i=1,nx),j=1,ny)
    ELSE
    READ(61,IREC=1)((FIELD(j,i),i=1,nx),j=1,ny)
    READ(61,IREC=2)((LAPLACIAN((j,i),i=1,nx),j=1,ny)
    ENDIF


CLOSE(61)

!CLIMATOLOGY IS STORED IN A DIFFERENT ORDER.
OPEN(UNIT=62,FILE=CLIMFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen)
    READ(62,rec=1)((CLIMFIELD(j,i),j=1,ny),i=1,nx)
CLOSE(62)

IF(READ_ADDITIONAL_DATA)THEN
OPEN(UNIT=63,FILE=ADDFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen)
    DO  ii=1,nf
    IF(YREV)THEN
    READ(63,rec=ii)((ADDITIONALFIELD(ny-j+1,i,ii),i=1,nx),j=1,ny)
    ELSE
    READ(63,rec=ii)((ADDITIONALFIELD(j,i,ii),i=1,nx),j=1,ny)
    ENDIF
    ENDDO
CLOSE(63)
ENDIF
OPEN(UNIT=64,FILE=LATFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen)
    IF(YREV)THEN
    READ(64,rec=1)((FIELDLAT(ny-j+1,i),i=1,nx),j=1,ny)
    ELSE
    READ(64,rec=1)((FIELDLAT(j,i),i=1,nx),j=1,ny)
    ENDIF
CLOSE(64)
OPEN(UNIT=65,FILE=LONFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen)
    READ(65,rec=1)((FIELDLON(j,i),i=1,nx),j=1,ny)
CLOSE(65)

!Compute the anomalies and store them in the FIELD variable for further computations.
FIELD=FIELD-CLIMFIELD

RETURN
END SUBROUTINE READ_DATA

!===========================================================================
SUBROUTINE WRITE_DATA()
!This subroutine will write the mask generated and also the one with the possition of the minima.
IMPLICIT NONE
CHARACTER*20  :: MASKFILE='mascaraout.bin',MMASKFILE='mmascaraout.bin'
INTEGER       :: iolen,IREC,ii,i,j,FLAG,jj
REAL          :: CLIMFIELD(ny,nx)

INQUIRE(IOLENGTH=iolen) iolen

IF(DEBUG)THEN !WRITE OUT THE MASK FOR DEBUG POURPOSES
OPEN(UNIT=71,FILE=MASKFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen,STATUS='UNKNOWN')
    WRITE(71,rec=1)((REAL(MASCARA(j,i)),i=1,nx),j=1,ny)
CLOSE(71)
OPEN(UNIT=72,FILE=MMASKFILE,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nx*ny*iolen,STATUS='UNKNOWN')
    WRITE(72,rec=1)((REAL(MASCARA_MINIMOS(j,i)),i=1,nx),j=1,ny)
CLOSE(72)
ENDIF

!WE WILL WRITE ALL THE INFORMATION ABOUT THE MINIMUNS AND ALSO A REDUCED WAY TO STORE THE MASK.
!ALL VARIABLES WILL BE WRITEN OUT IN SEQUENTIAL MODE, REAL SINGLE PRECISION.

OPEN(UNIT=71,FILE='./minimun_out.bin',FORM='UNFORMATTED',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')

!FIRST WRITE OUT GRID DIMENSIONS
WRITE(71)REAL(nx)
WRITE(71)REAL(ny)
WRITE(71)REAL(nf)
!WRITE NUMBER OF MINIMUNS
WRITE(71)REAL(NMINIMOS)
!WRITE A FLAG FOR THE PRESENCE OF ADDITIONAL DATA (1 IF PRESENT, 0 IF NOT PRESENT)
IF(READ_ADDITIONAL_DATA)THEN
FLAG=1.
WRITE(71)FLAG
ELSE
FLAG=0.
WRITE(71)FLAG
ENDIF
!WRITE SYSTEM LONGITUDES AND LATITUDES.
WRITE(71)MINLON(1:NMINIMOS)
WRITE(71)MINLAT(1:NMINIMOS)
WRITE(71)MINLONINT(1:NMINIMOS)
WRITE(71)MINLATINT(1:NMINIMOS)
WRITE(71)MINLONCENT(1:NMINIMOS)
WRITE(71)MINLATCENT(1:NMINIMOS)
!WRITE SUBINDICES CORRESPONDING TO MINIMUN POSSITION
WRITE(71)REAL(MIN_I(1:NMINIMOS))
WRITE(71)REAL(MIN_J(1:NMINIMOS))
!WRITE SYSTEM ID
WRITE(71)REAL(MIN_ID(1:NMINIMOS))
!WRITE SYSTEM AREA
WRITE(71)MIN_AREA(1:NMINIMOS)
WRITE(71)MIN_LAP(1:NMINIMOS)
!WRITE MINIMUN/MEAN
WRITE(71)MIN_ANOM_SIS(1:NMINIMOS)
WRITE(71)MEAN_ANOM_SIS(1:NMINIMOS)
!IF IS PRESENT READ ADDITIONAL DATA
IF(READ_ADDITIONAL_DATA)THEN
DO JJ=1,nf
   WRITE(71)MIN_ADDITIONAL_DATA(1:NMINIMOS,JJ)
   WRITE(71)MAX_ADDITIONAL_DATA(1:NMINIMOS,JJ)
   WRITE(71)MEAN_ADDITIONAL_DATA(1:NMINIMOS,JJ)
ENDDO
ENDIF
!NOW I WILL WRITE THE SUBINDICES CORRESPONDING TO EACH SYSTEM.
!FIRST THE NUMBER OF GRID POINTS CORRESPONDING TO EACH SYSTEM
WRITE(71)REAL(MIN_GRID_SIZE(1:NMINIMOS))
!THEN THE LIST OF SUBINDICES FOR EACH SYSTEM
DO II=1,NMINIMOS
   WRITE(71)REAL(MIN_INDEX(II,1:MIN_GRID_SIZE(II)))
ENDDO

CLOSE(71)


RETURN
END SUBROUTINE WRITE_DATA

!===========================================================================
SUBROUTINE GET_LOCAL_MINIMA()
!THIS SUBROUTINE FINDS THE POSITION OF THE MINIMUNS IN A FIELD 
IMPLICIT NONE
!COUNTERS
INTEGER     ::  i,j,ii,jj
INTEGER     ::  testmin,indexi,indexj

MASCARA=0
MASCARA_MINIMOS=0
NMINIMOS=0
MIN_I=0
MIN_J=0

DO j=1,nx
    DO i=2,ny-1
       !Si el punto no califica ni siquiera para ser parte de un sistema
       !entonces no me fijo si es o no un minimo. 
       IF(FIELD(i,j) < UMB_ANOM )THEN
       testmin=0
       DO ii=-1,1
           DO jj=-1,1
                   indexi=i+ii
                   indexj=j+jj
                   !USE GLOBAL BOUNDARY CONDITIONS
                   CALL GLOBAL_BOUNDARY_FUN(indexi,indexj,ny,nx)
                IF(FIELD(indexi,indexj) .LT. FIELD(i,j) )THEN
                TESTMIN=1
                ENDIF
               
           ENDDO
       ENDDO
       !El punto en cuestion era un minimo?
       IF(testmin.EQ.0)THEN
       NMINIMOS=NMINIMOS+1
       MIN_I(NMINIMOS)=i
       MIN_J(NMINIMOS)=j
       MIN_LAP(NMINIMOS)=LAPLACIAN(i,j)
       MASCARA_MINIMOS(i,j)=NMINIMOS
       
       ENDIF
       
      ENDIF
  ENDDO
ENDDO

!Vamos a considerar en forma especial los minimos en el polo.
testmin=0
DO ii=1,nx
   IF(FIELD(1,1) > FIELD(2,ii))THEN
   testmin=1
   ENDIF
ENDDO
IF(testmin.EQ.0)THEN
   !El polo sur es un minimo
   NMINIMOS=NMINIMOS+1
   MIN_I(NMINIMOS)=1
   MIN_J(NMINIMOS)=1
   MIN_LAP(NMINIMOS)=LAPLACIAN(1,1)
ENDIF
testmin=0
DO ii=1,nx
   IF(FIELD(ny,1) > FIELD(ny-1,ii))THEN
   testmin=1
   ENDIF
ENDDO
IF(testmin.EQ.0)THEN
   !El polo norte es un minimo
   NMINIMOS=NMINIMOS+1
   MIN_I(NMINIMOS)=ny
   MIN_J(NMINIMOS)=1
   MIN_LAP(NMINIMOS)=LAPLACIAN(ny,1)
ENDIF

DO ii=1,NMINIMOS
MIN_ID(ii)=ii
ENDDO

!WRITE(*,*)"LA CANTIDAD DE MINIMOS ENCONTRADOS ES: ",NMINIMOS

RETURN
END SUBROUTINE GET_LOCAL_MINIMA

!===========================================================================

SUBROUTINE GET_SYSTEM()
IMPLICIT NONE
LOGICAL :: PATH_MASK(ny,nx)
LOGICAL :: CONT
REAL :: TMPMINPN,TMPMINPS,TMP_MIN
INTEGER :: TMPMINLOCPN,TMPMINLOCPS
INTEGER :: MAXITER                  !MAXIMUN PATH LENGTH (IN GRID POINTS)
INTEGER :: UNDEF
!COUNTERS
INTEGER :: i,j,ii,jj,indexi,indexj,pi,pj,pi_old,pj_old,contador,pi_o,pj_o
!THIS SUBROUTINE USES THE MASK WITH THE POSITIONS OF THE MINIMUN AND THEN COMPUTES
!THE ASSOCIATED SYSTEM TO EACH MINIMUN AND CONSTRUCTS A MASK WHERE EACH GRID POINT
!HAS THE VALUE OF THE CORRESPONDING ASSOCIATED MINIMUN.
 
!FIRST COMPUTE THE CONTINUATION OF ANY PATH THAT GOES TROUGH THE NORTH OR SOUTH POLE
!THIS STEP IS COMPUTATIONALY EXPENSIVE SO WE CANNOT AFFORD TO PERFORME IT EVERY TIME
!A PATH REACHES THE POLE

UNDEF=-999

MASCARA=MASCARA_MINIMOS

    TMPMINPN=1.e30
    TMPMINPS=1.e30
    TMPMINLOCPN=0
    TMPMINLOCPs=0
    DO ii=1,nx
       IF(FIELD(ny-1,ii) < TMPMINPN)THEN
       TMPMINPN=FIELD(ny-1,ii)
       TMPMINLOCPN=ii
       ENDIF
       IF(FIELD(2,ii) < TMPMINPS)THEN
       TMPMINPS=FIELD(2,ii)
       TMPMINLOCPS=ii
       ENDIF
     ENDDO
     
!NOW WE WILL DO A LOOP OVER ALL GRID POINTS AND IF THE FIELD VALUE IS UNDER THE
!THRESHOLD THEN WE WILL FOLLOW THE FASTEST DESCENDING PATH UNTIL WE REACH A MINIMUN
!THAT WILL BE THE ASSOCIATED MINIMUN TO THIS GRID POINT.


DO j=1,nx
  DO i=1,ny

    IF(FIELD(i,j) < UMB_ANOM .AND. MASCARA_MINIMOS(i,j) .EQ. 0)THEN
    !WE WILL NOT THAKE INTO ACCOUNT THE POSITION OF THE MINIMUNS THIS POINTS WILL REMAIN UNCHANGED

         MAXITER=AINT(REAL(nx)/2.)
       
         !THE PATH STARST AT I and J, THEN PATHI AND PATHJ STORES THE SUBSECUENTS STEPS.
         pi=i
         pj=j

         PATH_MASK=.TRUE.  !THIS MASK CONTROLS THAT THE PATH DOSNT GO BACK.
  
         PATH_MASK(pi,pj)=.FALSE.
         
         CONT=.TRUE.       !CONTROLS THE ITERATION, WHILE TRUE THE PATH WILL CONTINUE UNTIL A MINIMUN IS
                           !REACHED OR UNTIL THE MAXIMUN NUMBER OF ITERATIONS IS REACHED.
  
         CONTADOR=1
         TMP_MIN=FIELD(pi,pj)
  
      DO WHILE(CONT)
  
      !I will search in all directions which is the one corresponding to the maximun descent.
 
   
      !First we check if we are at the pole because this is a particular case.
      IF(pi .EQ. ny)THEN
       pi=ny-1
       !I assume that al the field values at ny are the same and that they represent
       !the field value at the north pole.
       TMP_MIN=TMPMINPN
       pj=TMPMINLOCPN
       pi=ny-1
       PATH_MASK(ny,:)=.FALSE.

       ELSEIF(pi .EQ. 1)THEN !REPEAT FOR THE SOUTH POLE
       pi=2
       TMP_MIN=TMPMINPS
       pj=TMPMINLOCPS
       PATH_MASK(1,:)=.FALSE.
       
       ELSE 
       !We are not at the pole, so I have to check all neighbours to see were is the direction of maximun descent. 
       pi_o=pi
       pj_o=pj
         DO ii=-1,1
           DO jj=-1,1
                !First we check that we are not evaluating pi and pj.
                IF(.NOT. ( ii .EQ. 0 .AND. jj .EQ. 0 ))THEN
                   indexi=pi_o+ii
                   indexj=pj_o+jj
                   !USE GLOBAL BOUNDARY CONDITIONS 
                   CALL GLOBAL_BOUNDARY_FUN(indexi,indexj,ny,nx)
                  
                IF(FIELD(indexi,indexj) <= TMP_MIN .AND. PATH_MASK(indexi,indexj)) THEN
                TMP_MIN=FIELD(indexi,indexj)  
                pi=indexi
                pj=indexj

                ENDIF
                ENDIF
           ENDDO 
         ENDDO
   
      ENDIF!Este es el end del if sobre si se trata de un punto del polo o de uno comun.
           !Keep counting how many steps has been performed for the current path.
           CONTADOR=CONTADOR+1

           PATH_MASK(pi,pj)=.FALSE. !Change the mask value for pi,pj so this point cannot be used again for the same path.
           
           !Test if we have reached a minimun.
           IF( MASCARA_MINIMOS(pi,pj) > 0)THEN
               MASCARA(i,j)=MASCARA_MINIMOS(pi,pj)
               CONT=.FALSE.
           ENDIF
           !Test if we haven't reached the maximun path length.
           IF(CONTADOR > MAXITER)THEN
               CONT=.FALSE.
               MASCARA(i,j)=UNDEF
           ENDIF
               
     ENDDO !END WHILE

    ENDIF  !END DEL IF SOBRE SI LA ANOMALIA ESTA POR DEBAJO DEL UMBRAL.
       
  ENDDO !END OF j loop
ENDDO  !END OF i loop 
        

RETURN
END SUBROUTINE GET_SYSTEM

!===========================================================================

SUBROUTINE GLOBAL_BOUNDARY_FUN(indexy,indexx,ny,nx)
IMPLICIT NONE
INTEGER :: indexy,indexx,ny,nx
REAL    :: aux

!APLLY GLOBAL BOUNDARY CONDITIONS TO INDEXY AND INDEXX ACCORDING TO THE GLOBAL DOMAIN SIZES NY,NX
!IF INDEXY, INDEXX ARE OUT OF THE DOMAIN LIMITS, GLOBAL BOUNDARY CONDITIONS WILL BE APPLIED TO COMPUTE
!THEIR POSITION WITHIN THE DOMAIN LIMITS.

!FIRST TEST IF INDEXY INDEXX ARE WITHIN THE DOMAIN LIMITS.
     IF(indexy <= ny .AND. indexy >= 1 .AND. indexx <= nx .AND. indexx >=1)THEN
      RETURN  !NOTHING TO BE DONE!
     ELSE

      !FIRST CHECK THE Y BOUNDARY CONDITION.             
      IF(indexy > ny)THEN
         indexx=ny-(indexx-ny)
         aux=AINT(REAL(nx)/2.)
         indexx=indexx+aux
      ENDIF 
      IF(indexy < 1)THEN
         indexy=2-indexy 
         aux=AINT(REAL(nx)/2.)
         indexx=indexx+aux
      ENDIF
      !THEN CHECK THE X BOUNDARY CONDITION.
      IF(indexx > nx)THEN
         indexx=indexx-nx
      ENDIF
      IF(indexx < 1)THEN
        indexx=nx+indexx
      ENDIF
     ENDIF

RETURN
END SUBROUTINE GLOBAL_BOUNDARY_FUN

!===========================================================================

SUBROUTINE MERGE_MINIMUN()
!This subroutine evaluates minimuns which are close to each other and test
!to see if they are part of the same minimun.

!From each minimun we start a smallest ascende path. If we get out from the 
!minimun basin (i.e. the anomaly is over the threshold) or if we reach another
!minimun the path is stoped.
!If we reach another minimun those two minimuns can be related to each other
!so we test how far they are from each other and how strong is the geopotential
!barrier within them (i.e. do we have to climb a lot from minimum A to reach minimum B?)

!If two minimums are related then we keep the deeper one.

!Start a do over the minimuns (we will start a path from each minimun.)

IMPLICIT NONE
INTEGER   :: imin,contador,contador2
REAL      :: Data_path(20*nx)
LOGICAL   :: PATH_MASK(ny,nx)
LOGICAL   :: CONT
REAL      :: tmp_min
INTEGER   :: MINIMO1,MINIMO2
REAL      :: TMPMAX,AUX,DISTANCIA
INTEGER   :: i,j,ii,jj,pathi,pathj,indexi,indexj,pathi_o,pathj_o

DO imin=1,NMINIMOS


  pathi=MIN_I(imin) !THE PATH STARTS FROM THE POSITION OF THE MINIMUM
  pathj=MIN_J(imin) 

  DATA_PATH(1)=FIELD(pathi,pathj)
  PATH_MASK=.TRUE.              
  
  PATH_MASK(pathi,pathj)=.FALSE.
  
  CONT=.TRUE.
  
  CONTADOR=1

  DO WHILE(CONT)
      
   tmp_min=0
   !Search for the direction of minimun ascent.
   pathi_o=pathi
   pathj_o=pathj
   DO ii=-1,1
           DO jj=-1,1
              indexi=pathi_o+ii
              indexj=pathj_o+jj

              !USE GLOBAL BOUNDARY CONDITIONS.
              CALL GLOBAL_BOUNDARY_FUN(indexi,indexj,ny,nx)
              IF(FIELD(indexi,indexj) < tmp_min .AND. PATH_MASK(indexi,indexj))THEN
                  tmp_min=FIELD(indexi,indexj)  
                  pathi=indexi
                  pathj=indexj
              ENDIF 
           ENDDO
   ENDDO 
           
   CONTADOR=CONTADOR+1
   !KEEP INFORMATION ABOUT THE TRACK IN THE PATH_MASK MATRIX
   PATH_MASK(pathi,pathj)=.FALSE.
   DATA_PATH(CONTADOR)=tmp_min
   !CHECK IF THE PATH HAS REACHED ANOTHER MINIMUN.
   IF( MASCARA_MINIMOS(pathi,pathj) > 0)THEN
 
   !IF WE HAVE REACHED ANOTHER MINIMUN THEN CHECK IF THEY ARE RELATED.
     MINIMO1=IMIN  !MINIMO1 IS THE MINIMUM CORRESPONDING TO THE BEGGINING OF THE PATH
     MINIMO2=MASCARA_MINIMOS(pathi,pathj) !MINIMO2 IS THE MINIMUN THAT WE HAVE REACHED.
     CONT=.FALSE.                         !WE STOP THE PATH IF WE REACHED ANOTHER MINIMUN

     !WE WILL COMPUTE THE MAXIMUN OF DATA_PATH (ANOMALIES ALONG THE PATH) AND THEN COMPARE IT TO 
     !THE VALUES OF THE ANOMALIES AT MINIMO1 AND MINIMO2.
     
     TMPMAX=MAXVAL(DATA_PATH(1:CONTADOR))
     AUX=MIN( abs(TMPMAX-DATA_PATH(1)) , abs(TMPMAX-DATA_PATH(CONTADOR)) )
 
     IF( AUX <  UMB_MINBARRIER )THEN
     !IF THIS IS TRUE THEN IT MEANS THAT IS EASY TO REACH MINIMO2 FROM MINIMO1 OR BICEVERSA.
     !LETS COMPUTE THE DISTANCE BETWEEN THIS TWO MINIMA
       CALL DISTLL(FIELDLON(MIN_I(MINIMO1),MIN_J(MINIMO1)),FIELDLAT(MIN_I(MINIMO1),MIN_J(MINIMO1)), &
     &             FIELDLON(MIN_I(MINIMO2),MIN_J(MINIMO2)),FIELDLAT(MIN_I(MINIMO2),MIN_J(MINIMO2)), &
     &             DISTANCIA)

      !WRITE(*,*)FIELDLON(MIN_I(MINIMO1),MIN_J(MINIMO1)),FIELDLAT(MIN_I(MINIMO1),MIN_J(MINIMO1))
      !WRITE(*,*)FIELDLON(MIN_I(MINIMO2),MIN_J(MINIMO2)),FIELDLAT(MIN_I(MINIMO2),MIN_J(MINIMO2))
      !WRITE(*,*)DISTANCIA,IMIN
      IF( DISTANCIA < UMB_CERCANIA )THEN
      !IF THIS IS TRUE THEN THE TWO MINIMA ARE CLOSE TO EACH OTHER AND ARE NOT SEPARATED
      !BY A STRONG GEOPOTENTIAL BARRIER.

      !LETS SEE WHICH ONE IS DEEPER...
         IF( DATA_PATH(1) < DATA_PATH(CONTADOR) )THEN
           !MINIMO1 IS DEEPER, WE SHOULD KEEP THIS ONE.
           WHERE( MASCARA .EQ. MINIMO2)
                MASCARA=MINIMO1
           ENDWHERE
           MASCARA_MINIMOS(MIN_I(MINIMO2),MIN_J(MINIMO2))=MINIMO1

           MIN_ID(MINIMO2)=MINIMO1
           MIN_I(MINIMO2)=MIN_I(MINIMO1)
           MIN_J(MINIMO2)=MIN_J(MINIMO1)
         ELSE
           !IN THIS CASE MINIMO2 IS DEEPER SO WE KEEP THIS MINIMUN.
           WHERE( MASCARA .EQ. MINIMO1)
                MASCARA=MINIMO2
           ENDWHERE
           MASCARA_MINIMOS(MIN_I(MINIMO1),MIN_J(MINIMO1))=MINIMO2

           MIN_ID(MINIMO1)=MINIMO2
           MIN_I(MINIMO1)=MIN_I(MINIMO2)
           MIN_J(MINIMO1)=MIN_J(MINIMO2)
         ENDIF 
        ENDIF
      ENDIF
    ENDIF
          
    
    !IF THE PATH REACHES A POINT WHERE THE ANOMALY IS HIGHER THAN THE 
    !SELECTED THRESHOLD WE INTERRUPT THE PATH.
    IF(tmp_min > UMB_ANOM )THEN
      CONT=.FALSE.
    ENDIF
           
     
  ENDDO !END SOBRE EL WHILE DEL PATH.
  
ENDDO  !END SOBRE EL DO DE LOS MINIMOS. 

!AFTER MERGING SOME OF THE MINIMUNS REASIGNATES ID AND RECOMPUTE THE NUMBER OF MINIMUNS.

CONTADOR2=0
!RESET MASCARA MINIMOS
MASCARA_MINIMOS=0
DO ii=1,NMINIMOS

   AUX=COUNT(MASCARA==ii)
   IF(AUX .GT. 0)THEN
   !I FOUND A SYSTEM

   CONTADOR2=CONTADOR2+1
   WHERE ( MASCARA .EQ. ii)
       MASCARA=CONTADOR2
   ENDWHERE
   MIN_I(CONTADOR2)=MIN_I(ii)
   MIN_J(CONTADOR2)=MIN_J(ii)
   MIN_ID(CONTADOR2)=MIN_ID(ii)
   MASCARA_MINIMOS(MIN_I(ii),MIN_J(ii))=CONTADOR2
!      
   ENDIF  
ENDDO
MIN_I(CONTADOR2+1:MAXM)=0
MIN_J(CONTADOR2+1:MAXM)=0
MIN_ID(CONTADOR2+1:MAXM)=0
NMINIMOS=CONTADOR2


RETURN
END SUBROUTINE MERGE_MINIMUN

!===========================================================================================

SUBROUTINE INTERP_MINIMUN_POSITION
!IN THIS SUBROUTINE THE POSITION OF THE MINIMUN IS RECOMPUTED USING THE 
!NEIGHBORS INFORMATION IN A SIMILAR WAY AS THAT PROPOSED BY M. PULIDO.
!HOWEVER IN THIS CASE THE WEIGTHED AVERAGE OF THE LATITUDE AND LONGITUDE
!VALUES IS USED INSTEAD OF 2D SPLINE INTERPOLATION.
IMPLICIT NONE
INTEGER :: mini,ii,jj,kk,aux_index,aux_index2,kk1,kk2
REAL    :: TMPLAT(9),TMPLON(9),TMPDAT(9),TMPINDEX
REAL    :: COEF(5)

MINLAT=0
MINLON=0

MINLATINT=0
MINLONINT=0

DO ii=1,NMINIMOS
    
    !GET LAT AND LON CORRESPONDIG TO THE POSITION OF THE 
    !GRID POINT WHERE THE LOCAL MINIMUN WAS FOUND.
   
    MINLAT(ii)=FIELDLAT(MIN_I(ii),MIN_J(ii))
    MINLON(ii)=FIELDLON(MIN_I(ii),MIN_J(ii))


    !NOW WE WILL USE THE VALUE OF THE ANOMALY IN THE SOURROUNDING
    !GRID POINTS TO CORRECT THE MINIMUN POSSITION.
    !A MEAN OF THE LATITUDE AND THE LONGITUDE WEIGTHED BY THE ANOMALY
    !VALUE IS GOING TO BE USED.
    
    IF( MIN_I(ii) .LE. 1 .OR. MIN_I(ii) .GE. ny)THEN
        !WE WONT DO THAT IF THE MINIMUN IS AT THE POLE.
        MINLATINT(ii)=MINLAT(ii)
        MINLONINT(ii)=MINLON(ii)
    ELSE
    TMPLAT=0
    TMPLON=0
    TMPDAT=0
        !GET THE LATITUDE AND LONGITUDE VALUES FOR THE 
        !SOURROUNDING GRID POINTS
        TMPINDEX=0
        DO jj=-1,1
            aux_index=MIN_J(ii)+jj;
            IF(aux_index < 1)THEN
                aux_index=nx-aux_index
            ELSEIF(aux_index > nx)THEN
                aux_index=aux_index-nx
            ENDIF
            DO kk=-1,1
            TMPINDEX=TMPINDEX+1
            aux_index2=MIN_I(ii)+kk
            TMPLAT(TMPINDEX)=FIELDLAT(aux_index2,aux_index)
            TMPLON(TMPINDEX)=FIELDLON(aux_index2,aux_index)
            TMPDAT(TMPINDEX)=FIELD(aux_index2,aux_index)
            ENDDO
        ENDDO
       
    IF(INTERP_TYPE .EQ. 1)THEN   !WEIGTHED AVERAGE INTERPOLATION (NOT RECOMENDED) 
    !COMPUTE THE WEIGTHED MEAN FOR THE LATITUDES
    MINLATINT(ii)=SUM(TMPDAT*TMPLAT)/SUM(TMPDAT)
    !NOW COMPUTE THE WEIGTHED MEAN FOR THE LONGITUDES
    !TAKE CARE OF LONGITUD DISCONTINUITIES 
    IF(MAXVAL(TMPLON)-MINVAL(TMPLON) .GT. 180)THEN
       IF(MAXVAL(tmplon) > 180)THEN
           WHERE (TMPLON .GT. 180)
           TMPLON=TMPLON-360
           ENDWHERE
           MINLONINT(ii)=SUM(TMPDAT*TMPLON)/SUM(TMPDAT)
           IF(MINLONINT(ii) .LT. 0 )THEN
               MINLONINT(ii)=MINLONINT(ii)+360
           ENDIF
       ELSEIF(MINVAL(TMPLON) .LT. 0)THEN
           WHERE(TMPLON .LT. 0)
           TMPLON=TMPLON+360    
           ENDWHERE
           MINLONINT(ii)=SUM(TMPDAT*TMPLON)/SUM(TMPDAT)       
           IF(MINLONINT(ii) .GT. 180)THEN
             MINLONINT(ii)=MINLONINT(ii)-360
           ENDIF
       ENDIF
    ELSE
    MINLONINT(ii)=SUM(TMPDAT*TMPLON)/SUM(TMPDAT)
    ENDIF
   ELSEIF(INTERP_TYPE .EQ. 2)THEN !LEAST SQUARES INTERPOLATION (ONLY FOR MINIMUN LOCATION)
   !DEBUG DEBUG DEBUD 
   ! DO KK1=1,3
   !    DO KK2=1,3
   !       TMPLON((KK1-1)*3+KK2)=KK1
   !       TMPLAT((KK1-1)*3+KK2)=KK2
   !    ENDDO
   ! ENDDO
   ! TMPDAT=1.*(TMPLON**2)+1.*(TMPLAT**2)
   ! CALL CUADRADOS_MINIMOS_2D(TMPLON,TMPLAT,TMPDAT,9,COEF,MINLONINT(ii),MINLATINT(ii))
    !WRITE(*,*)COEF
    !WRITE(*,*)MINLAT(ii),MINLATINT(ii),MINLON(ii),MINLONINT(ii)
   ! STOP
    !END DEBUG
       
       !FIRST CONSIDER CYCLIC BOUNDARY CONDITIONS
      IF(MAXVAL(TMPLON)-MINVAL(TMPLON) .GT. 180)THEN
       IF(MAXVAL(tmplon) > 180)THEN
           WHERE (TMPLON .GT. 180)
           TMPLON=TMPLON-360
           ENDWHERE
           CALL CUADRADOS_MINIMOS_2D(TMPLON,TMPLAT,TMPDAT,9,COEF,MINLONINT(ii),MINLATINT(ii))
             !CHECK IF THE SOLUTION FALLS WITHIN THE SMALL REGION.
             IF(MINLONINT(ii) .GT. MAXVAL(TMPLON) .OR. MINLONINT(ii) .LT. MINVAL(TMPLON)   &
             & .OR. MINLATINT(ii) .GT. MAXVAL(TMPLAT) .OR. MINLATINT(ii) .LT. MINVAL(TMPLAT))THEN
              MINLONINT(ii)=MINLON(ii)
              MINLATINT(ii)=MINLAT(ii)
             ENDIF
           IF(MINLONINT(ii) .LT. 0 )THEN
               MINLONINT(ii)=MINLONINT(ii)+360
           ENDIF
       ELSEIF(MINVAL(TMPLON) .LT. 0)THEN
           WHERE(TMPLON .LT. 0)
           TMPLON=TMPLON+360
           ENDWHERE
           CALL CUADRADOS_MINIMOS_2D(TMPLON,TMPLAT,TMPDAT,9,COEF,MINLONINT(ii),MINLATINT(ii))
             !CHECK IF THE SOLUTION FALLS WITHIN THE SMALL REGION.
             IF(MINLONINT(ii) .GT. MAXVAL(TMPLON) .OR. MINLONINT(ii) .LT. MINVAL(TMPLON)   &
             & .OR. MINLATINT(ii) .GT. MAXVAL(TMPLAT) .OR. MINLATINT(ii) .LT. MINVAL(TMPLAT))THEN
              MINLONINT(ii)=MINLON(ii)
              MINLATINT(ii)=MINLAT(ii)
             ENDIF
           IF(MINLONINT(ii) .GT. 180)THEN
             MINLONINT(ii)=MINLONINT(ii)-360
           ENDIF
       ENDIF
    ELSE
    !WRITE(*,*)TMPLON
    !WRITE(*,*)TMPLAT
    !WRITE(*,*)TMPDAT
    CALL CUADRADOS_MINIMOS_2D(TMPLON,TMPLAT,TMPDAT,9,COEF,MINLONINT(ii),MINLATINT(ii))
            !CHECK IF THE SOLUTION FALLS WITHIN THE SMALL REGION.
             IF(MINLONINT(ii) .GT. MAXVAL(TMPLON) .OR. MINLONINT(ii) .LT. MINVAL(TMPLON)   &
             & .OR. MINLATINT(ii) .GT. MAXVAL(TMPLAT) .OR. MINLATINT(ii) .LT. MINVAL(TMPLAT))THEN
              MINLONINT(ii)=MINLON(ii)
              MINLATINT(ii)=MINLAT(ii)
             ENDIF

    ENDIF

    ELSE
      WRITE(*,*)"NOT RECOGNIZED INTERPOLATION OPTION"
      STOP
    ENDIF
  ENDIF

  !WRITE(*,*)COEF
  !WRITE(*,*)MINLAT(ii),MINLATINT(ii),MINLON(ii),MINLONINT(ii)
       
ENDDO

RETURN
END SUBROUTINE INTERP_MINIMUN_POSITION

!===========================================================================================

SUBROUTINE GET_SYSTEM_INFO
IMPLICIT NONE
REAL :: DELTA_LAT(ny,nx),DELTA_LON(ny,nx),BOX_SIZE(ny,nx),PI
LOGICAL :: MASK(ny,nx)
INTEGER :: ii,ix,iy,ivar

PI=acos(-1.)
!THIS SUBROUTINE WILL COMPUTE SOME SYSTEM PROPERTIES
!LIKE SIZE, INTENSITY, CENTROID POSSITION, ETC.

!FIRST WE COMPUTE THE AREA OF EACH GRID BOX

DELTA_LAT(2:ny,1:nx)=FIELDLAT(2:ny,:)-FIELDLAT(1:ny-1,:)
DELTA_LON(1:ny,2:nx)=FIELDLON(:,2:nx)-FIELDLON(:,1:nx-1)

DELTA_LON(:,1)=FIELDLON(:,2)-FIELDLON(:,1)
DELTA_LAT(1,:)=FIELDLAT(2,:)-FIELDLAT(1,:)

!CORRECT FOR THE DISCONTINUITY IN LONGITUDE.
WHERE (DELTA_LON .GT. 360)
   DELTA_LON=DELTA_LON-360
ENDWHERE
!COMPUTE THE BOX SIZE ASSOCIATED WITH EACH GRID POINT
BOX_SIZE=(111000.**2)*(DELTA_LON*cos(FIELDLAT*PI/180)*DELTA_LAT)



!START A LOOP OVER THE DIFFERENT SYSTEMS TO GET SYSTEM PROPERTIES.
MIN_GRID_SIZE=0
DO ii=1,NMINIMOS
    MASK=.FALSE.  !INITIALIZE THE LOGICAL MASK.
    !FIND THE SUBSCRIPTS CORRESPONDING TO EACH SYSTEM (USING A COLUMNWISE CRITERIA)
    DO ix=1,nx
       DO iy=1,ny
          IF(MASCARA(iy,ix) .EQ. ii)THEN
            MIN_GRID_SIZE(ii)=MIN_GRID_SIZE(ii)+1
            MIN_INDEX(ii,MIN_GRID_SIZE(ii))=(ix-1)*ny+iy
            MASK(iy,ix)=.TRUE. !SET THE LOGICAL MASK TO TRUE
            !WE WILL USE THIS MASK LATER TO COMPUTE SYSTEM PROPERTIES.
          ENDIF 

       ENDDO
    ENDDO

    !COMPUTE SYSTEM AREA.
    MIN_AREA(ii)=SUM(BOX_SIZE,MASK)
    !COMPUTE MEAN SYSTEM ANOMALY
    MEAN_ANOM_SIS(ii)=SUM(FIELD,MASK)/REAL(COUNT(MASK))
    !COMPUTE MIN SYSTEM ANOMALY
    MIN_ANOM_SIS(ii)=MINVAL(FIELD,MASK)


    !COMPUTE THE SYSTEM CENTROID IN LAT AND LON.
    MINLATCENT(ii)=SUM(FIELDLAT*(FIELD**WEXP),MASK)/SUM(FIELD**WEXP,MASK)
    !COMPUTE THE SYSTEM CENTROID IN LONGITUD TAKING INTO ACCOUNT LONGITUDE DISCONTINUITY
    IF(MAXVAL(FIELDLON,MASK)-MINVAL(FIELDLON,MASK) .GT. 180)THEN
      
       IF(MAXVAL(FIELDLON,MASK) .GT. 180)THEN
           WHERE(MASK .AND. FIELDLON .GT. 180)
           !WARNING!! THIS WILL MODIFY THE VALUES OF FIELDLON.
                FIELDLON=FIELDLON-360
           ENDWHERE
           MINLONCENT(ii)=SUM(FIELDLON*(FIELD**WEXP),MASK)/SUM(FIELD**WEXP,MASK)
           IF(MINLONCENT(ii) .LT. 0)THEN
               MINLONCENT(ii)=MINLONCENT(ii)+360
           ENDIF
        ELSEIF(MINVAL(FIELDLON,MASK) .LT. 0)THEN
           WHERE(MASK .AND. FIELDLON .LT. 0)
                FIELDLON=FIELDLON+360
           ENDWHERE
           MINLONCENT(ii)=SUM(FIELDLON*(FIELD**WEXP),MASK)/SUM(FIELD**WEXP,MASK)
         IF(MINLONCENT(ii) .GT. 180)THEN
             MINLONCENT(ii)=MINLONCENT(ii)-360
         ENDIF
       ENDIF
    ELSE
    MINLONCENT(ii)=SUM(FIELDLON*(FIELD**WEXP),MASK)/SUM(FIELD**WEXP,MASK)
    ENDIF

    !COMPUTE MEAN, MIN AND MAX FOR ALL THE AVAILABLE ADDITIONAL DATA.

    IF(READ_ADDITIONAL_DATA)THEN
      DO IVAR=1,NF
        MIN_ADDITIONAL_DATA(ii,IVAR)=MINVAL(ADDITIONALFIELD(:,:,IVAR),MASK)
        MAX_ADDITIONAL_DATA(ii,IVAR)=MAXVAL(ADDITIONALFIELD(:,:,IVAR),MASK)
        MEAN_ADDITIONAL_DATA(ii,IVAR)=SUM(ADDITIONALFIELD(:,:,IVAR),MASK)/REAL(COUNT(MASK))
      ENDDO
    ENDIF

    !WRITE(*,*)MINLAT(ii),MINLATCENT(ii),MINLON(ii),MINLONCENT(ii),MIN_AREA(ii)
     
ENDDO  !END DO OVER THE SYSTEMS.

RETURN
END SUBROUTINE GET_SYSTEM_INFO

!===========================================================================================

SUBROUTINE DISTLL(alon,alat,blon,blat,dist)
IMPLICIT NONE
REAL  :: alon,alat,blon,blat,pi,re,lon1,lon2,lat1,lat2
REAL  :: cosd,dist

pi=ACOS(-1.)
re=6371.3e3

    lon1 = alon * pi /180
    lon2 = blon * pi /180
    lat1 = alat * pi /180
    lat2 = blat * pi /180

    cosd = SIN(lat1)*SIN(lat2) + COS(lat1)*COS(lat2)*COS(lon2-lon1)
    cosd = MIN(1.0,cosd)
    cosd = MAX(-1.0,cosd)

    dist = ACOS( cosd ) * re

RETURN
END SUBROUTINE DISTLL


!===========================================================================================

SUBROUTINE CUADRADOS_MINIMOS_2D(X,Y,Z,NP,COEFICIENTES,XMIN,YMIN)
!GIVEN A 2D FIELD DESCRIVED BY X,Y AND Z THIS SUBROUTINE COMPUTES THE 
!LAST SQUARES 2D CUADRATIC FIT TO THE Z FIELD AND COMPUTES THE POSITION OF
!THE MINIMUN CORRESPONDING TO THE FITTING FUNCTION AND THE COEFFICIENTES
! Z=C(1)X^2+C(2)X+C(3)Y^2+C(4)Y+C(5)
!NP IS THE DIMENSION OF X,Y AND Z WHICH ARE VECTORS.

IMPLICIT NONE
INTEGER,INTENT(IN)   :: NP                !NUMBER OF ELEMENTS IN X,Y AND Z.
REAL   ,INTENT(INOUT):: X(NP),Y(NP),Z(NP) !INPUT X,Y AND Z.
REAL   ,INTENT(OUT)  :: COEFICIENTES(5) 
REAL   ,INTENT(OUT)  :: XMIN,YMIN         !X AND Y POSSITION OF THE MINIMUN OF THE FITTING PARABOLA
REAL                 :: B(5),C(5,5),INVC(5,5)
INTEGER              :: ii
REAL                 :: XMEAN,YMEAN,ZMEAN,TMPCOEF(5)


!FIRST SUBSTRACT THE MEAN FROM X, Y AND Z TO IMPROVE CONVERGENCE.

XMEAN=SUM(X)/REAL(NP)
YMEAN=SUM(Y)/REAL(NP)
ZMEAN=SUM(Z)/REAL(NP)

X=X-XMEAN
Y=Y-YMEAN
Z=Z-ZMEAN

B(1)=sum(Z*(X**2))
B(2)=sum(Z*X)
B(3)=sum(Z*(Y**2))
B(4)=sum(Z*Y)
B(5)=sum(Z)

c(1,1)=sum(x**4)
c(1,2)=sum(x**3)
c(1,3)=sum((x**2)*(y**2))
c(1,4)=sum(y*(x**2))
c(1,5)=sum(x**2)
c(2,1)=sum(x**3)
c(2,2)=sum(x**2)
c(2,3)=sum((y**2)*x)
c(2,4)=sum(y*x)
c(2,5)=sum(x)
c(3,1)=sum((x**2)*(y**2))
c(3,2)=sum(x*(y**2))
c(3,3)=sum(y**4)
c(3,4)=sum(y**3)
c(3,5)=sum(y**2)
c(4,1)=sum((x**2)*y)
c(4,2)=sum(x*y)
c(4,3)=sum(y**3)
c(4,4)=sum(y**2)
c(4,5)=sum(y)
c(5,1)=sum(x**2)
c(5,2)=sum(x)
c(5,3)=sum(y**2)
c(5,4)=sum(y)
c(5,5)=NP

!WRITE(*,*)B

!DO II=1,5
!
! WRITE(*,*)C(II,:)
!
!ENDDO 


CALL MTX_INV_RG(5,C,INVC)

DO ii=1,5

  COEFICIENTES(ii)=SUM(INVC(ii,:)*B(:))

ENDDO



!COMPUTE MINIMUN POSSITION IN X AND Y.
XMIN=-COEFICIENTES(2)/(2*COEFICIENTES(1))+XMEAN
YMIN=-COEFICIENTES(4)/(2*COEFICIENTES(3))+YMEAN

!RECOMPUTE COEFFICIENTS SO THEY CAN BE USED WITH THE FULL FIELDS NOT 
!JUST THE PERTURBATIONS 

TMPCOEF(1)=COEFICIENTES(1)
TMPCOEF(2)=COEFICIENTES(2)-2*XMEAN
TMPCOEF(3)=COEFICIENTES(3)
TMPCOEF(4)=COEFICIENTES(4)-2*YMEAN
TMPCOEF(5)=COEFICIENTES(1)*(XMEAN**2)-COEFICIENTES(2)*XMEAN+COEFICIENTES(3)*(YMEAN**2)-COEFICIENTES(4)*YMEAN+COEFICIENTES(5)+ZMEAN

COEFICIENTES=TMPCOEF

X=X+XMEAN
Y=Y+YMEAN
Z=Z+ZMEAN

END SUBROUTINE CUADRADOS_MINIMOS_2D

!=========================================================================================

!=======================================================================
!  Compute inverse of a real matrix (not necessarily symmetric)
!    INPUT
!      INTEGER :: n            : dimension of matrix
!      REAL(r_size) :: aa(n,n) : input matrix (real symmetric)
!    OUTPUT
!      REAL(r_size) :: ff(n,n) : square root of a
!*** COPIED FROM 'A0568.NEW.FORT(MTXINV)' ON 1989.10.1
!    changed to free format by H.Yoshimura 2000.06.27
!    adapted by T.Miyoshi on 2005.10.31
!=======================================================================
SUBROUTINE mtx_inv_rg(n,aa,ff)
!
!##  MATRIX INVERSION
!##  AA IS THE MATRIX TO BE INVERTED
!##  FF IS THE INVERSE OF AA
!
  INTEGER,INTENT(IN) :: n
  REAL,INTENT(IN) :: aa(n,n)
  REAL,INTENT(OUT) :: ff(n,n)
!
  REAL :: a(n,n)
  REAL :: b(n,n)
  REAL :: x(n,n)

  REAL :: c,cc,xx
  INTEGER :: i,j,n1,k,kp,jx,ii,jr,jp
!-------------------------------------------------------
  n1=n-1
!
  do i=1,n
    do j=1,n
      a(i,j)=aa(i,j)
    end do
  end do
  do i=1,n
    do j=1,n
      b(i,j)=0.d0
      if( i == j ) b(i,j)=1.d0
    end do
  end do
!
  do j=1,n
    c=abs(a(1,j))
    do i=2,n
      c=max(c,abs(a(i,j)))
    end do
    c=1.d0/c
    do i=1,n
      a(i,j)=a(i,j)*c
    end do
    b(j,j)=b(j,j)*c
  end do
!
  do k=1,n1
    c=abs(a(k,k))
    kp=k+1
    jx=k
    do j=kp,n
      cc=abs(a(k,j))
      if ( cc < c ) cycle
      c=cc
      jx=j
    end do
    do i=k,n
      c=a(i,k)
      a(i,k)=a(i,jx)
      a(i,jx)=c
    end do
    do i=1,n
      c=b(i,k)
      b(i,k)=b(i,jx)
      b(i,jx)=c
    end do
    do j=kp,n
      c=a(k,j)/a(k,k)
      do ii=1,n
        b(ii,j)=b(ii,j)-c*b(ii,k)
      end do
      do i=k,n
        a(i,j)=a(i,j)-c*a(i,k)
      end do
    end do
  end do
!
  do ii=1,n
    x(ii,n)=b(ii,n)/a(n,n)
    do j=1,n1
      jr=n-j
      jp=jr+1
      xx=0.d0
      do i=jp,n
        xx=xx+a(i,jr)*x(ii,i)
      end do
      x(ii,jr)=(b(ii,jr)-xx)/a(jr,jr)
    end do
  end do
!
  do i=1,n
    do j=1,n
      ff(i,j)=x(i,j)
    end do
  end do
!
END SUBROUTINE mtx_inv_rg


END MODULE  MINIMUN_TOOLS
