
! THESE ROUTINES ARE FOR A MOTION VECTOR COMPUTATION USING PYTHON-FORTRAN INTERFACE
! COMPILE USING: f2py -c -m test /home/jruiz/PYTHON/motion_vectors.f90
! FOR PARALLEL FUNCTIONALITY f2py -c -lgomp --f90flags="-fopenmp -lgomp" -m motion_vectors motion_vectors_parallel.f90 

!SUBROUTINE ADD_ONE(in_array,nx,ny,out_array)
!TEST SUBROUTINE

!implicit none
!INTEGER, PARAMETER :: sp = kind(1.0e0)
!INTEGER, PARAMETER :: dp = kind(1.0d0)
!INTEGER            :: i,j
!INTEGER , INTENT(IN)  :: nx , ny
!REAL(dp) , INTENT(IN)  :: in_array(nx,ny)
!REAL(dp) , INTENT(OUT) ::  out_array(nx,ny)

!DO i=1,nx
! DO j=1,ny
!    out_array(i,j)=in_array(i,j)+1
!  ENDDO
!ENDDO


!END SUBROUTINE ADD_ONE

SUBROUTINE MOTION_VECTOR(field_t0,field_t1,nx,ny,sigma,sigma_threshold,dt,dx,desp_max,u_motion,v_motion,max_corr)

IMPLICIT NONE
INTEGER, PARAMETER :: sp = kind(1.0e0)
INTEGER, PARAMETER :: dp = kind(1.0d0)
INTEGER            :: ii , jj , iii , jjj , ini_i , ini_j , end_i , end_j , contador , n_points

REAL(dp), PARAMETER :: UNDEF=-999.0d0 , min_box_fraction = 0.10d0 , min_dbz=0.0d0
INTEGER , PARAMETER :: METRIC_OPTION = 1 ! Metric option for similarity quantification (1 = correlation , ...)

!nx is the number of grid points in X direction (rows), ny is the number of grid points in the Y direction (columns)
INTEGER, INTENT(IN) :: nx , ny , desp_max
REAL(dp), INTENT(IN) :: sigma , sigma_threshold !The width of the Gaussian filter and the treshold (maximum filter width),
REAL(dp), INTENT(IN) :: field_t0(nx,ny),field_t1(nx,ny),dt,dx
REAL(dp), INTENT(OUT) :: u_motion(nx,ny) , v_motion(nx,ny) , max_corr(nx,ny) 

INTEGER  :: box_size

REAL(dp),ALLOCATABLE :: corr_matrix(:,:) , vector_1(:) , vector_2(:)
REAL(dp),ALLOCATABLE :: gaussian_weigths(:,:) , vector_gw(:)   !Gaussian weights for weighted correlation.
REAL(dp) :: tmp(2) , box_fraction , dist 
INTEGER  :: tmp_dim(1)

!First find the box size based on sigma and the threshold. In this implementation
!the box size is computed from sigma and sigma_threshold.

box_size= CEILING ( sigma_threshold * dx / sigma )

ALLOCATE(corr_matrix(desp_max*2+1,desp_max*2+1) , vector_1((box_size*2+1)**2) , vector_2((box_size*2+1)**2)  )


!Compute Gaussian weigths.

ALLOCATE( gaussian_weigths(box_size*2+1,box_size*2+1) , vector_gw((box_size*2+1)**2) )

DO ii=-box_size*2,box_size*2
  DO jj=-box_size*2,box_size*2
    dist=( ii*dx ** 2 + jj*dx ** 2 )
    gaussian_weigths( ii+box_size , jj+box_size ) = exp (-( dist / (sigma**2) ) )
  ENDDO
ENDDO

!Initialize u_motion and v_motion with UNDEF values.
u_motion=undef
v_motion=undef
max_corr=undef


!$OMP PARALLEL DEFAULT(SHARED)  PRIVATE(ii,jj,ini_i,end_i,ini_j,end_j,iii,jjj,corr_matrix & 
!$OMP&           ,vector_1,vector_2,n_points,tmp_dim,contador,box_fraction,tmp) 
!$OMP DO 
DO ii = 1,nx


  WRITE(*,*)"Processing row number ",ii
  DO jj = 1,ny

        corr_matrix=0.0d0

        ini_i=max(ii-box_size,1+desp_max)
        end_i=min(ii+box_size,nx-desp_max)
        ini_j=max(jj-box_size,1+desp_max)
        end_j=min(jj+box_size,ny-desp_max)

        n_points=(end_i-ini_i+1)*(end_j-ini_j+1)

        IF ( n_points   <= 1 )CYCLE   !No podemos calcular la velocidad en este caso.
 
          contador=0
          DO iii=ini_i,end_i
            DO jjj=ini_j,end_j
               IF( field_t0(iii,jjj) > min_dbz )THEN
                 contador = contador + 1
               ENDIF
            ENDDO
          ENDDO
        box_fraction=REAL(contador,dp)/REAL(n_points,dp)

        IF ( box_fraction < min_box_fraction )CYCLE   !No podemos calcular la velocidad en este caso.

        tmp_dim(1)=n_points
        DO iii= -desp_max,desp_max
          DO jjj= -desp_max,desp_max
 !           IF( ini_i + iii < 1 .OR. ini_j + jjj < 1 .OR. end_i + iii > nx .OR. end_j + jjj >  ny)THEN
 !             WRITE(*,*)ii,jj
 !             WRITE(*,*)ini_i+iii,end_i+iii,ini_j+jjj,end_j+jjj
 !             WRITE(*,*) "ERROR"
 !             STOP
 !           ENDIF              
            vector_1(1:n_points)=RESHAPE(field_t0(ini_i:end_i,ini_j:end_j),tmp_dim)
            vector_gw(1:n_points)=RESHAPE(gaussian_weigths(ini_i:end_i,ini_j:end_j),tmp_dim)
            vector_gw(1:n_points)=vector_gw(1:n_points)/SUM(vector_gw(1:n_points))
            vector_2(1:n_points)=RESHAPE(field_t1(ini_i+iii:end_i+iii,ini_j+jjj:end_j+jjj),tmp_dim)
            
            IF( metric_option == 1 )THEN
            !Compute the correlation between field_t0 and field_t1
            CALL CORRELATION( vector_1(1:n_points), vector_2(1:n_points) , vector_gw(1:n_points) , n_points , corr_matrix(iii+desp_max+1,jjj+desp_max+1) )
         
            ELSEIF( metric_option == 2 )THEN
            !RMSE
            corr_matrix(iii+desp_max+1,jjj+desp_max+1)=-SUM( ( vector_1(1:n_points)-vector_2(1:n_points) * vector_gw(1:n_points)  )**2 )
            ELSE
              !Non recognized option!
              CYCLE

            ENDIF

           ENDDO
         ENDDO


         !Get the location of the maximum correlation within
         !corr_matrix
         tmp=MAXLOC(corr_matrix)
         u_motion(ii,jj)= REAL(tmp(2)-desp_max -1,dp)*dx/dt
         v_motion(ii,jj)= REAL(tmp(1)-desp_max -1,dp)*dx/dt
         max_corr(ii,jj)= MAXVAL(corr_matrix)
         !WRITE(*,*)u_motion(ii,jj),v_motion(ii,jj),max_corr(ii,jj)

  ENDDO
ENDDO

!$END OMP DO
!$OMP END PARALLEL



WRITE(*,*)"FINISH MOTION VECTOR COMPUTATION"
END SUBROUTINE MOTION_VECTOR


!-----------------------------------------------------------------------
! Correlation
!-----------------------------------------------------------------------
SUBROUTINE CORRELATION(A,B,W,N,COR)
IMPLICIT NONE
INTEGER, PARAMETER :: sp = kind(1.0e0)
INTEGER, PARAMETER :: dp = kind(1.0d0)


INTEGER, INTENT(IN) :: N
REAL(dp) , INTENT(IN) :: A(N),B(N),W(N)
REAL(dp) , INTENT(OUT) :: COR

!Weighted correlation, A and B are two variables and W are the weights that 
!corresponds to each element of A and B.

REAL(dp) :: STDA , STDB , COV

  CALL STDEV(A,W,N,STDA)
  CALL STDEV(B,W,N,STDB)
  CALL COVAR(A,B,W,N,COV)

  COR = COV/STDA/STDB

END SUBROUTINE CORRELATION

!-----------------------------------------------------------------------
! Covariance
!-----------------------------------------------------------------------
SUBROUTINE COVAR(A,B,W,N,COV)
  IMPLICIT NONE
  INTEGER, PARAMETER :: sp = kind(1.0e0)
  INTEGER, PARAMETER :: dp = kind(1.0d0)
  INTEGER,INTENT(IN) :: N
  REAL(dp),INTENT(IN) :: A(N)
  REAL(dp),INTENT(IN) :: B(N)
  REAL(dp),INTENT(IN) :: W(N)
  REAL(dp),INTENT(OUT) :: COV

  REAL(dp) :: MEANA,MEANB

  MEANA=SUM(W*A)  !Weighted mean.
  MEANB=SUM(W*B)  !Weighted mean

  COV = SUM( W*(A-MEANA)*(B-MEANB) )  !Weigthed covariance

  RETURN
END SUBROUTINE COVAR

!-----------------------------------------------------------------------
! Standard deviation
!-----------------------------------------------------------------------
SUBROUTINE STDEV(A,W,N,STD)
  IMPLICIT NONE
  INTEGER, PARAMETER :: sp = kind(1.0e0)
  INTEGER, PARAMETER :: dp = kind(1.0d0)
  INTEGER,INTENT(IN) :: N
  REAL(dp),INTENT(IN) :: A(N)
  REAL(dp),INTENT(IN) :: W(N)
  REAL(dp),INTENT(OUT) :: STD

  STD = SQRT( SUM(W*(A(:) - SUM(W*A))**2) )

  RETURN
END SUBROUTINE STDEV





