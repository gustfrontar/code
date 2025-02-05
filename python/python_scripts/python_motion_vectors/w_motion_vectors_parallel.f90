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
SUBROUTINE MOTION_VECTOR(field_t0,field_t1,dt,dx,sigma,sigma_threshold,nx,ny,desp_max,u_motion,v_motion,max_corr)

IMPLICIT NONE
INTEGER, PARAMETER :: sp = kind(1.0e0)
INTEGER, PARAMETER :: dp = kind(1.0d0)
INTEGER            :: ii , jj , iii , jjj , ini_i , ini_j , end_i , end_j , contador , n_points

REAL(dp), PARAMETER :: UNDEF=-999.0d0 , min_box_fraction = 0.10d0 , min_dbz=0.0d0
INTEGER , PARAMETER :: METRIC_OPTION = 1 ! Metric option for similarity quantification (1 = correlation , ...)

REAL(dp), INTENT(IN) :: field_t0(nx,ny),field_t1(nx,ny),dt,dx,sigma,sigma_threshold
INTEGER , INTENT(IN) :: nx,ny,desp_max

REAL(dp), INTENT(OUT) :: u_motion(nx,ny) , v_motion(nx,ny) , max_corr(nx,ny) 

INTEGER  :: box_size , tmp_dim(1)

REAL(dp),ALLOCATABLE :: corr_matrix(:,:) , vector_1(:) , vector_2(:)
REAL(dp),ALLOCATABLE :: gaussian_weigths(:,:) , vector_gw(:)   !Gaussian weights for weighted correlation.
REAL(dp) :: box_fraction , dist , tmp_corr  , tmp_max_corr , meani ,meanj,weigth
INTEGER  :: maxi , maxj  , i_point

REAL(dp) , PARAMETER :: correlation_threshold = 0.2  !

WRITE(*,*) " INSIDE W MOTION VECTOR ROUTINE "
WRITE(*,*) " FIRST REF FIELD AT T 0 = ",field_t0(1,1)
WRITE(*,*) " FIRST REF FIELD AT T 1 = ",field_t1(1,1)
WRITE(*,*) " DELTA T                  ",dt
WRITE(*,*) " DELTA X                  ",dx
WRITE(*,*) " SIGMA                    ",sigma
WRITE(*,*) " SIGMA_THRESHOLD          ",sigma_threshold
WRITE(*,*) " NX ,  NY                 ",nx,ny
WRITE(*,*) " DESP MAX                 ",desp_max

!First find the box size based on sigma and the threshold. In this implementation
!the box size is computed from sigma and sigma_threshold.

box_size= CEILING ( sigma_threshold * sigma / dx )

ALLOCATE(corr_matrix(desp_max*2+1,desp_max*2+1) , vector_1((box_size*2+1)**2) , vector_2((box_size*2+1)**2)  )
!ALLOCATE(vector_1((box_size*2+1)**2) , vector_2((box_size*2+1)**2)  )

!Compute Gaussian weigths.

ALLOCATE( gaussian_weigths(box_size*2+1,box_size*2+1) , vector_gw((box_size*2+1)**2) )

DO ii=-box_size,box_size
  DO jj=-box_size,box_size
    dist=( (ii*dx) ** 2 + (jj*dx) ** 2 )
    gaussian_weigths( ii+box_size+1 , jj+box_size+1 ) = exp (-( dist / (sigma**2) ) )
  ENDDO
ENDDO


!Initialize u_motion and v_motion with UNDEF values.
u_motion=undef
v_motion=undef
max_corr=-1.0d0


!$OMP PARALLEL DEFAULT(SHARED)  PRIVATE(ii,jj,ini_i,end_i,ini_j,end_j,iii,jjj,corr_matrix & 
!$OMP&           ,vector_1,vector_2,vector_gw,n_points,tmp_dim,contador,box_fraction      &
!$OMP&           ,meani,meanj,tmp_corr,weigth,maxi,maxj) 
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

        vector_1(1:n_points)=RESHAPE(field_t0(ini_i:end_i,ini_j:end_j),tmp_dim)
        vector_gw(1:n_points)=RESHAPE(gaussian_weigths(ini_i-ii+box_size+1:end_i-ii+box_size+1,ini_j-jj+box_size+1:          &
     &                                 end_j-jj+box_size+1),tmp_dim)
        vector_gw(1:n_points)=vector_gw(1:n_points)/SUM(vector_gw(1:n_points))

        tmp_max_corr=-1.0d0
        maxi=0
        maxj=0


        DO iii= -desp_max,desp_max
          DO jjj= -desp_max,desp_max

            vector_2(1:n_points)=RESHAPE(field_t1(ini_i+iii:end_i+iii,ini_j+jjj:end_j+jjj),tmp_dim)
            !Compute the correlation between field_t0 and field_t1
            CALL CORRELATION( vector_1(1:n_points), vector_2(1:n_points) , vector_gw(1:n_points) , n_points , tmp_corr )

            corr_matrix(iii+desp_max+1,jjj+desp_max+1)=tmp_corr
            IF( tmp_corr >= tmp_max_corr )THEN
                tmp_max_corr=tmp_corr
                maxj=jjj
                maxi=iii
            ENDIF

           ENDDO
         ENDDO

        weigth=0.0d0
        meani=0.0d0
        meanj=0.0d0
        DO iii= -desp_max,desp_max 
           DO jjj= -desp_max,desp_max
             tmp_corr = corr_matrix( iii+desp_max+1,jjj+desp_max+1)
             IF( max_corr(ii,jj) - tmp_corr < correlation_threshold .AND. tmp_corr > 0.0 )THEN
                 weigth=weigth+tmp_corr**2
                 meani=meani + iii * (tmp_corr ** 2)
                 meanj=meanj + jjj * (tmp_corr ** 2)
             ENDIF
           ENDDO
        ENDDO

        IF( weigth > 0)THEN 
         meani=meani/weigth
         meanj=meanj/weigth
        ENDIF


         !Find the weighted location for the minimum.
         !meani=0.0d0
         !meanj=0.0d0
         !weigth=0.0d0
         !DO iii=maxi-1,maxi+1
         !  DO jjj=maxj-1,maxj+1 
         !    IF( iii <= desp_max  .AND. iii >= -desp_max .AND. jjj <= desp_max .AND. jjj >= -desp_max )THEN
         !       IF( corr_matrix( iii+desp_max+1 , jjj+desp_max + 1) >= 0)THEN
         !           weigth=weigth + corr_matrix( iii+desp_max+1 , jjj+desp_max + 1)**2 
         !           meani = meani + corr_matrix( iii+desp_max+1 , jjj+desp_max + 1)**2*iii
         !           meanj = meanj + corr_matrix( iii+desp_max+1 , jjj+desp_max + 1)**2*jjj
         !       ENDIF
         !    ENDIF                
         !  ENDDO
         !ENDDO
         !meani=meani/weigth
         !meanj=meanj/weigth


         !Get the location of the maximum correlation within
         !corr_matrix
         !tmp=MAXLOC(corr_matrix)
         IF( weigth > 0.0)THEN
         u_motion(ii,jj)= REAL(maxj,dp)*dx/dt
         v_motion(ii,jj)= REAL(maxi,dp)*dx/dt
         ENDIF


  ENDDO
ENDDO




!$END OMP DO
!$OMP END PARALLEL

DEALLOCATE(corr_matrix , vector_1 , vector_2  )
DEALLOCATE( gaussian_weigths , vector_gw )




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





