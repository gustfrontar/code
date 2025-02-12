 SUBROUTINE conj_grad

!  Conjugate gradient method using a line minimization

   use grid
   use switches, only : ncgit
!    use friction  !only : rdrsat,idrsat
   use impr, only: imprecg
   IMPLICIT NONE

!!Implicit REAL, INTENT(INOUT)   :: guess(nctrl)

   INTEGER      :: icg,ictrl,ibanbeta
   REAL(model)  :: r1(nctrl),dir(nctrl),dirold(nctrl)
   REAL(model)  :: gradcost(nctrl)
   REAL(model)  :: sq_rold,sq_rnew,sq_r0,beta
   REAL(model)  :: rold(nctrl),fbr,cost,costold
   INTEGER      ::  il, im, ilev, lenstr
   REAL(model)  :: ang

!**** Cosas de impresion
!         Deberian estar en otra parte
   expname=case_str() !function in ctrlstt

   fstin2sectt=.TRUE. ! to control first input to sec_test
                      ! it is NOT a switch

!*****compute the initial values
   CALL costfn(cost,gradcost) !calcula gradcost

   icg=0

!   Save fields
   call imprecg(icg,cost) !--> yobs, ymod, adguess,guess


   print *,'En Conj Grad: icg, J= ',icg,cost

   r1=-gradcost
   dir=r1
       sq_r0=0.0
   DO ictrl=1,nctrl
      sq_r0=sq_r0+r1(ictrl)**2
   ENDDO
     sq_rold=sq_r0
   sq_rnew=sq_rold
   rold=r1
     icg=1
   DO WHILE ( sq_rnew>ep_cg**2*sq_r0 .and. icg<ncgit+1 )

      costold=cost

!********search a guess that minimizes J in the dir direction.
      CALL secante(dir,cost,gradcost)
!    in secante it is calculated the gradcost f/new guess


!    Save fields
      call imprecg(icg,cost) !--> yobs, ymod, adguess,guess

      r1(:)=-gradcost(:)
       !********beta with the formula of Fletcher-Reeves
      IF (ibanbeta == 0) THEN
         sq_rnew=0.
         DO ictrl=1,nctrl
            sq_rnew=sq_rnew+r1(ictrl)**2
         ENDDO
                   IF (abs(sq_rold) > 1.e-30) then
            IF (float(icg-1)/float(nctrl) == (icg-1)/(nctrl).and.icg /= 1) then
               beta=0.
                print *,'new search'
            ELSE
               beta=sq_rnew/sq_rold
            ENDIF
         ELSE
            print *,'***** sq_rold too small'
            icg=ncgit
         ENDIF

      ELSE
!*********beta with the formula of Polak-Ribiere
         sq_rnew=0.
         fbr=0.
         DO ictrl=1,nctrl
            sq_rnew=sq_rnew+r1(ictrl)**2
            fbr=fbr+r1(ictrl)*(r1(ictrl)-rold(ictrl))
         ENDDO

         IF (abs(sq_rold) > 1.e-10) then
!               if (float(icg-1)/float(nctrl) == (icg-1)/(nctrl).and.icg /= 1) then
!                  beta=0.
!                  print *,'new search'
!               else
            beta=max(fbr/sq_rold,0.)
!               endif
         ELSE
              print *,'***** sq_rold too small'
              icg=ncgit
           ENDIF
           rold=r1
        ENDIF

        dirold=dir
!********the new search direction
        dir=r1+beta*dir

        sq_rold=sq_rnew
!********test ortogonality
        ang=acos(dot_product(dir,dirold)/(sum(dir**2))**.5 &
             /(sum(dirold**2))**.5)*180./acos(-1.)

        print*,'Ortogonality: ',ang

        print *,'En Conj Grad: icg, J = ',icg,cost
        icg=icg+1

     ENDDO


     END SUBROUTINE conj_grad
