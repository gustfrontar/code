function [ MASCARA ] = get_system_fun( field, )
%THIS SUBROUTINE USES THE MASK WITH THE POSITIONS OF THE MINIMUN AND THEN COMPUTES
%THE ASSOCIATED SYSTEM TO EACH MINIMUN AND CONSTRUCTS A MASK WHERE EACH GRID POINT
%HAS THE VALUE OF THE CORRESPONDING ASSOCIATED MINIMUN.
 
%FIRST COMPUTE THE CONTINUATION OF ANY PATH THAT GOES TROUGH THE NORTH OR SOUTH POLE
%THIS STEP IS COMPUTATIONALY EXPENSIVE SO WE CANNOT AFFORD TO PERFORME IT EVERY TIME
%A PATH REACHES THE POLE
PATH_MASK(ny,nx)=NaN(size(field));


UNDEF=-999;

MASCARA=MASCARA_MINIMOS;

    TMPMINPN=1.e30;
    TMPMINPS=1.e30;
    TMPMINLOCPN=0;
    TMPMINLOCPs=0;
    
    
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


end

