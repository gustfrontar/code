MODULE common_speedy
!=======================================================================
!
! [PURPOSE:] Common Information for SPEEDY model
!
! [HISTORY:]
!   10/15/2004 Takemasa Miyoshi  created
!
!=======================================================================
  USE common
  IMPLICIT NONE
  PUBLIC
!-----------------------------------------------------------------------
! General parameters
!-----------------------------------------------------------------------
  INTEGER,PARAMETER :: nlon=96
  INTEGER,PARAMETER :: nlat=48
  INTEGER,PARAMETER :: nlev=7
  INTEGER,PARAMETER ::  p=20             ! the number of adaptive observations
  INTEGER,PARAMETER :: ngrd=nlon*nlat*nlev
  INTEGER,PARAMETER :: ngpv=nlon*nlat*(nlev*4+1)
  REAL(r_size),SAVE :: lon(nlon)
  REAL(r_size),SAVE :: lat(nlat)
  REAL(r_size),SAVE :: sig(nlev)
  REAL(r_size),SAVE :: dx(nlat)
  REAL(r_size),SAVE :: dy(nlat)
  REAL(r_size),SAVE :: dy2(nlat)
  REAL(r_size),SAVE :: fcori(nlat)
  REAL(r_size),SAVE :: phi0(nlon,nlat) ! surface geopotential
  REAL(r_size),SAVE :: plev(nlev)
  INTEGER,SAVE :: nobs
  INTEGER,SAVE :: ex_obs_u(nlon,nlat,nlev)
  INTEGER,SAVE :: ex_obs_v(nlon,nlat,nlev)
  INTEGER,SAVE :: ex_obs_t(nlon,nlat,nlev)
  INTEGER,SAVE :: ex_obs_q(nlon,nlat,nlev)
  INTEGER,SAVE :: ex_obs_ps(nlon,nlat)
  INTEGER,SAVE :: ex_obs_prec(nlon,nlat)
  REAL(r_size),PARAMETER :: obs_err_u = 1.0d0
  REAL(r_size),PARAMETER :: obs_err_v = 1.0d0
  REAL(r_size),PARAMETER :: obs_err_t = 1.0d0
  REAL(r_size),PARAMETER :: obs_err_q = 1.0d-4
  REAL(r_size),PARAMETER :: obs_err_ps = 1.0d2
!  REAL(r_size),PARAMETER :: obs_err_prec = 1.0d0

  REAL(r_size),PARAMETER :: obs_err_prec = 0.05d0  ! for log rain assimilation, relative error
  REAL(r_size),ALLOCATABLE,SAVE :: obs_err(:)

CONTAINS
!-----------------------------------------------------------------------
! Set the parameters
!-----------------------------------------------------------------------
SUBROUTINE set_common_speedy
  IMPLICIT NONE
  INTEGER :: i,j,k,iobs
  !
  ! Lon, Lat, Sigma
  !
  lon(1) = 0.0d0
  DO i=2,nlon
    lon(i) = lon(i-1) + 3.75d0
  END DO

  lat(1) = -87.159d0
  lat(2) = -83.479d0
  lat(3) = -79.777d0
  lat(4) = -76.070d0
  lat(5) = -72.362d0
  lat(6) = -68.652d0
  lat(7) = -64.942d0
  lat(8) = -61.232d0
  lat(9) = -57.521d0
  lat(10)= -53.810d0
  lat(11)= -50.099d0
  lat(12)= -46.389d0
  lat(13)= -42.678d0
  lat(14)= -38.967d0
  lat(15)= -35.256d0
  lat(16)= -31.545d0
  lat(17)= -27.833d0
  lat(18)= -24.122d0
  lat(19)= -20.411d0
  lat(20)= -16.700d0
  lat(21)= -12.989d0
  lat(22)=  -9.278d0
  lat(23)=  -5.567d0
  lat(24)=  -1.856d0
  lat(25)=   1.856d0
  lat(26)=   5.567d0
  lat(27)=   9.278d0
  lat(28)=  12.989d0
  lat(29)=  16.700d0
  lat(30)=  20.411d0
  lat(31)=  24.122d0
  lat(32)=  27.833d0
  lat(33)=  31.545d0
  lat(34)=  35.256d0
  lat(35)=  38.967d0
  lat(36)=  42.678d0
  lat(37)=  46.389d0
  lat(38)=  50.099d0
  lat(39)=  53.810d0
  lat(40)=  57.521d0
  lat(41)=  61.232d0
  lat(42)=  64.942d0
  lat(43)=  68.652d0
  lat(44)=  72.362d0
  lat(45)=  76.070d0
  lat(46)=  79.777d0
  lat(47)=  83.479d0
  lat(48)=  87.159d0

  sig(1) = .95d0
  sig(2) = .835d0
  sig(3) = .685d0
  sig(4) = .51d0
  sig(5) = .34d0
  sig(6) = .2d0
  sig(7) = .08d0
!
! The pressure of each level
!
  plev(1) = 925.0d0
  plev(2) = 850.0d0
  plev(3) = 700.0d0
  plev(4) = 500.0d0
  plev(5) = 300.0d0
  plev(6) = 200.0d0
  plev(7) = 100.0d0
  !
  ! dx and dy
  !
  dx(:) = 2.0d0 * pi * re * cos(lat(:) * pi / 180.0d0) / REAL(nlon,r_size)

  DO i=1,nlat-1
    dy(i) = 2.0d0 * pi * re * (lat(i+1) - lat(i)) / 360.0d0
  END DO
  dy(nlat) = 2.0d0 * pi * re * (90.0d0 - lat(nlat)) / 180.0d0

  DO i=2,nlat
    dy2(i) = (dy(i-1) + dy(i)) * 0.5d0
  END DO
  dy2(1) = (dy(nlat) + dy(1)) * 0.5d0
  !
  ! Corioris parameter
  !
  fcori(:) = 2.0d0 * r_omega * sin(lat(:)*pi/180.0d0)
  !
  ! Surface geoptential (Read Orography file)
  !
  READ(21) phi0
  !
  ! Observation (Read Observation Location file)
  !
  READ(22) ex_obs_u
  READ(22) ex_obs_v
  READ(22) ex_obs_t
  READ(22) ex_obs_q
  READ(22) ex_obs_ps
  READ(22) ex_obs_prec

  nobs = SUM(ex_obs_u) &
    &  + SUM(ex_obs_v) &
    &  + SUM(ex_obs_t) &
    &  + SUM(ex_obs_q) &
    &  + SUM(ex_obs_ps)&
    &  + SUM(ex_obs_prec)

  ALLOCATE( obs_err(nobs) )
  iobs = 1
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_u(i,j,k)==1 ) THEN
          obs_err(iobs) = obs_err_u
          iobs=iobs+1
        END IF
      END DO
    END DO
  END DO
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_v(i,j,k)==1 ) THEN
          obs_err(iobs) = obs_err_v
          iobs=iobs+1
        END IF
      END DO
    END DO
  END DO
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_t(i,j,k)==1 ) THEN
          obs_err(iobs) = obs_err_t
          iobs=iobs+1
        END IF
      END DO
    END DO
  END DO
  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_q(i,j,k)==1 ) THEN
          obs_err(iobs) = obs_err_q
          iobs=iobs+1
        END IF
      END DO
    END DO
  END DO
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_ps(i,j)==1 ) THEN
          obs_err(iobs) = obs_err_ps
          iobs=iobs+1
        END IF
      END DO
    END DO

    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_prec(i,j)==1 ) THEN
          obs_err(iobs) = obs_err_prec
          iobs=iobs+1
        END IF
      END DO
    END DO
  RETURN
END SUBROUTINE set_common_speedy
!-----------------------------------------------------------------------
! File I/O
!-----------------------------------------------------------------------
!-- Open a grid file ---------------------------------------------------
SUBROUTINE open_grd(iunit,filename)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  CHARACTER(*),INTENT(IN) :: filename

  OPEN(iunit,file=filename,form='unformatted',access='direct',recl=nlon*nlat*4)

  RETURN
END SUBROUTINE open_grd
!-- Close a grid file --------------------------------------------------
SUBROUTINE close_grd(iunit)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit

  CLOSE(iunit)

  RETURN
END SUBROUTINE close_grd
!-- Read a grid file ---------------------------------------------------
SUBROUTINE read_grd(iunit,u,v,t,q,ps)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(OUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps(nlon,nlat)
  REAL(r_sngl) :: u4(nlon,nlat,nlev)
  REAL(r_sngl) :: v4(nlon,nlat,nlev)
  REAL(r_sngl) :: t4(nlon,nlat,nlev)
  REAL(r_sngl) :: q4(nlon,nlat,nlev)
  REAL(r_sngl) :: ps4(nlon,nlat)
  INTEGER :: i,j,k,irec

  irec=1
  DO k=1,nlev
    READ(iunit,REC=irec) ((u4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((v4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((t4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((q4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  READ(iunit,REC=irec) ((ps4(i,j),i=1,nlon),j=1,nlat)

  u = REAL(u4,r_size)
  v = REAL(v4,r_size)
  t = REAL(t4,r_size)
  q = REAL(q4,r_size)
  ps = REAL(ps4,r_size)

  RETURN
END SUBROUTINE read_grd

!-- Read a precipitation file ---------------------------------------------------
SUBROUTINE read_grd_R(iunit,rtotal)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(OUT) :: rtotal(nlon,nlat)
  REAL(r_sngl) :: r4(nlon,nlat)
  REAL(r_sngl) :: tmp(nlon*nlat)
  INTEGER :: i,j,k,irec

  irec=1
    READ(iunit,REC=irec) (tmp(i),i=1,nlon*nlat)
    r4=reshape(tmp,(/nlon,nlat/))
    rtotal=real(r4, r_size)

 end SUBROUTINE read_grd_R

SUBROUTINE read_grd_p(iunit,u,v,t,q,z,ps)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(OUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: z(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps(nlon,nlat)
  REAL(r_sngl) :: u4(nlon,nlat,nlev)
  REAL(r_sngl) :: v4(nlon,nlat,nlev)
  REAL(r_sngl) :: t4(nlon,nlat,nlev)
  REAL(r_sngl) :: q4(nlon,nlat,nlev)
  REAL(r_sngl) :: z4(nlon,nlat,nlev)
  REAL(r_sngl) :: ps4(nlon,nlat)
  INTEGER :: i,j,k,irec

  irec=1
  DO k=1,nlev
    READ(iunit,REC=irec) ((u4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((v4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((t4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((q4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    READ(iunit,REC=irec) ((z4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  READ(iunit,REC=irec) ((ps4(i,j),i=1,nlon),j=1,nlat)

  u = REAL(u4,r_size)
  v = REAL(v4,r_size)
  t = REAL(t4,r_size)
  q = REAL(q4,r_size)
  z = REAL(z4,r_size)
  ps = REAL(ps4,r_size)

  RETURN
END SUBROUTINE read_grd_p
!-- Write a grid file -------------------------------------------------
SUBROUTINE write_grd(iunit,u,v,t,q,ps)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(IN) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_sngl) :: u4(nlon,nlat,nlev)
  REAL(r_sngl) :: v4(nlon,nlat,nlev)
  REAL(r_sngl) :: t4(nlon,nlat,nlev)
  REAL(r_sngl) :: q4(nlon,nlat,nlev)
  REAL(r_sngl) :: ps4(nlon,nlat)
  INTEGER :: i,j,k,irec

  u4 = REAL(u,r_sngl)
  v4 = REAL(v,r_sngl)
  t4 = REAL(t,r_sngl)
  q4 = REAL(q,r_sngl)
  ps4 = REAL(ps,r_sngl)

  irec=1
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((u4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((v4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((t4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((q4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  WRITE(iunit,REC=irec) ((ps4(i,j),i=1,nlon),j=1,nlat)

  RETURN
END SUBROUTINE write_grd

!-- Write a precititation file -------------------------------------------------
SUBROUTINE write_grd_R(iunit,prec)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(IN) :: prec(nlon,nlat)
  REAL(r_sngl) :: prec4(nlon,nlat)
  real(r_sngl) :: tmp(nlon*nlat)
  INTEGER :: i,j,k,irec

  prec4 = REAL(prec,r_sngl)
  tmp=reshape(prec4,(/nlon*nlat/)) 
  irec=1
    WRITE(iunit,REC=irec) (tmp(i),i=1,nlon*nlat)
end SUBROUTINE write_grd_R

SUBROUTINE write_grd_p(iunit,u,v,t,q,z,ps)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: iunit
  REAL(r_size),INTENT(IN) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: z(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_sngl) :: u4(nlon,nlat,nlev)
  REAL(r_sngl) :: v4(nlon,nlat,nlev)
  REAL(r_sngl) :: t4(nlon,nlat,nlev)
  REAL(r_sngl) :: q4(nlon,nlat,nlev)
  REAL(r_sngl) :: z4(nlon,nlat,nlev)
  REAL(r_sngl) :: ps4(nlon,nlat)
  INTEGER :: i,j,k,irec

  u4 = REAL(u,r_sngl)
  v4 = REAL(v,r_sngl)
  t4 = REAL(t,r_sngl)
  q4 = REAL(q,r_sngl)
  z4 = REAL(z,r_sngl)
  ps4 = REAL(ps,r_sngl)

  irec=1
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((u4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((v4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((t4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((q4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    WRITE(iunit,REC=irec) ((z4(i,j,k),i=1,nlon),j=1,nlat)
    irec = irec + 1
  END DO
  WRITE(iunit,REC=irec) ((ps4(i,j),i=1,nlon),j=1,nlat)

  RETURN
END SUBROUTINE write_grd_p

!-----------------------------------------------------------------------
! Monitor
!-----------------------------------------------------------------------
SUBROUTINE monit_grd(u,v,t,q,ps)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  INTEGER :: k

  DO k=1,nlev
    PRINT '(I2,A)',k,'th level'
    PRINT '(A,10ES10.2)','U  ',maxval(u(:,:,k)),minval(u(:,:,k))
    PRINT '(A,10ES10.2)','V  ',maxval(v(:,:,k)),minval(v(:,:,k))
    PRINT '(A,10ES10.2)','T  ',maxval(t(:,:,k)),minval(t(:,:,k))
    PRINT '(A,10ES10.2)','Q  ',maxval(q(:,:,k)),minval(q(:,:,k))
  END DO
  PRINT '(A,10ES10.2)','PS ',maxval(ps),minval(ps)

  RETURN
END SUBROUTINE monit_grd
!-----------------------------------------------------------------------
! Random perturbation
!-----------------------------------------------------------------------
SUBROUTINE rand_grd(u,v,t,q,ps,prec)
  IMPLICIT NONE
  REAL(r_size),INTENT(OUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps(nlon,nlat)
  REAL(r_size),INTENT(OUT),optional :: prec(nlon,nlat)
  REAL(r_size) :: rnd(nlon*nlat*nlev)
  REAL(r_size) :: rnd2(nlon*nlat)

  CALL com_randn(nlon*nlat*nlev,rnd)
  u = RESHAPE(rnd,(/nlon,nlat,nlev/)) * obs_err_u
  CALL com_randn(nlon*nlat*nlev,rnd)
  v = RESHAPE(rnd,(/nlon,nlat,nlev/)) * obs_err_v
  CALL com_randn(nlon*nlat*nlev,rnd)
  t = RESHAPE(rnd,(/nlon,nlat,nlev/)) * obs_err_t
  CALL com_randn(nlon*nlat*nlev,rnd)
  q = RESHAPE(rnd,(/nlon,nlat,nlev/)) * obs_err_q
  CALL com_randn(nlon*nlat,rnd2)
  ps = RESHAPE(rnd2,(/nlon,nlat/)) * obs_err_ps
  if(present(prec))then                                 ! LJJ
     CALL com_randn(nlon*nlat,rnd2)                      ! LJJ
     prec = RESHAPE(rnd2,(/nlon,nlat/)) * obs_err_prec      ! LJJ
  endif

  RETURN
END SUBROUTINE rand_grd
!-----------------------------------------------------------------------
! Smoothing using Lanczos filter
!-----------------------------------------------------------------------
SUBROUTINE smooth_grd(u,v,t,q,ps,prec)
  IMPLICIT NONE
  REAL(r_size),INTENT(INOUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: ps(nlon,nlat)
  REAL(r_size),INTENT(INOUT),optional :: prec(nlon,nlat)
  INTEGER :: i,j,k

  DO k=1,nlev
    DO j=1,nlat
      CALL com_filter_lanczos(nlon,pi/2.0d0,u(:,j,k))
    END DO
    DO i=1,nlon
      CALL com_filter_lanczos(nlat,pi/2.0d0,u(i,:,k))
    END DO
    DO j=1,nlat
      CALL com_filter_lanczos(nlon,pi/2.0d0,v(:,j,k))
    END DO
    DO i=1,nlon
      CALL com_filter_lanczos(nlat,pi/2.0d0,v(i,:,k))
    END DO
    DO j=1,nlat
      CALL com_filter_lanczos(nlon,pi/2.0d0,t(:,j,k))
    END DO
    DO i=1,nlon
      CALL com_filter_lanczos(nlat,pi/2.0d0,t(i,:,k))
    END DO
    DO j=1,nlat
      CALL com_filter_lanczos(nlon,pi/2.0d0,q(:,j,k))
    END DO
    DO i=1,nlon
      CALL com_filter_lanczos(nlat,pi/2.0d0,q(i,:,k))
    END DO
  END DO
  DO j=1,nlat
    CALL com_filter_lanczos(nlon,pi/2.0d0,ps(:,j))
  END DO
  DO i=1,nlon
    CALL com_filter_lanczos(nlat,pi/2.0d0,ps(i,:))
  END DO
if(present(prec))then
  DO j=1,nlat
    CALL com_filter_lanczos(nlon,pi/2.0d0,prec(:,j))
  END DO
  DO i=1,nlon
    CALL com_filter_lanczos(nlat,pi/2.0d0,prec(i,:))
  END DO
endif

  RETURN
END SUBROUTINE smooth_grd
!-----------------------------------------------------------------------
! Reshape
!-----------------------------------------------------------------------
SUBROUTINE reshape_grd(u,v,t,q,ps,gpv)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: gpv(ngpv)

  gpv(1       :ngrd  ) = RESHAPE(u,(/ngrd/))
  gpv(ngrd+1  :ngrd*2) = RESHAPE(v,(/ngrd/))
  gpv(ngrd*2+1:ngrd*3) = RESHAPE(t,(/ngrd/))
  gpv(ngrd*3+1:ngrd*4) = RESHAPE(q,(/ngrd/))
  gpv(ngrd*4+1:ngpv  ) = RESHAPE(ps,(/nlon*nlat/))

  RETURN
END SUBROUTINE reshape_grd

SUBROUTINE reshape_gpv(gpv,u,v,t,q,ps)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: gpv(ngpv)
  REAL(r_size),INTENT(OUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps(nlon,nlat)

  u = RESHAPE(gpv(1       :ngrd  ),(/nlon,nlat,nlev/))
  v = RESHAPE(gpv(ngrd+1  :ngrd*2),(/nlon,nlat,nlev/))
  t = RESHAPE(gpv(ngrd*2+1:ngrd*3),(/nlon,nlat,nlev/))
  q = RESHAPE(gpv(ngrd*3+1:ngrd*4),(/nlon,nlat,nlev/))
  ps = RESHAPE(gpv(ngrd*4+1:ngpv ),(/nlon,nlat/))

  RETURN
END SUBROUTINE reshape_gpv

SUBROUTINE idxcvt_gpv(n,i,j,k)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: n
  INTEGER,INTENT(OUT) :: i,j,k
  INTEGER :: nn

  nn = n

  DO
    IF( nn > ngrd ) THEN
      nn = nn - ngrd
    ELSE
      EXIT
    END IF
  END DO
  DO k=1,nlev
    IF( nn > nlon*nlat ) THEN
      nn = nn - nlon*nlat
    ELSE
      EXIT
    END IF
  END DO
  DO j=1,nlat
    IF( nn > nlon ) THEN
      nn = nn - nlon
    ELSE
      EXIT
    END IF
  END DO
  i = nn

  RETURN
END SUBROUTINE idxcvt_gpv
!-----------------------------------------------------------------------
! Observation operator (X -> Y)
!-----------------------------------------------------------------------
SUBROUTINE Trans_XtoY(u,v,t,q,ps,y)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: y(nobs)

  INTEGER :: i,j,k,iobs

  iobs = 1

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_u(i,j,k)==1 ) THEN
          y(iobs) = u(i,j,k)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_v(i,j,k)==1 ) THEN
          y(iobs) = v(i,j,k)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_t(i,j,k)==1 ) THEN
          y(iobs) = t(i,j,k)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_q(i,j,k)==1 ) THEN
          y(iobs) = q(i,j,k)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_ps(i,j)==1 ) THEN
          y(iobs) = ps(i,j)
          iobs = iobs + 1
        END IF
      END DO
    END DO

  RETURN
END SUBROUTINE Trans_XtoY

SUBROUTINE AD_Trans_XtoY(u,v,t,q,ps,y)
  IMPLICIT NONE
  REAL(r_size),INTENT(INOUT) :: u(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: v(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: q(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: ps(nlon,nlat)
  REAL(r_size),INTENT(IN) :: y(nobs)

  INTEGER :: i,j,k,iobs

  iobs = 1

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_u(i,j,k)==1 ) THEN
          u(i,j,k) = u(i,j,k) + y(iobs)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_v(i,j,k)==1 ) THEN
          v(i,j,k) = v(i,j,k) + y(iobs)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_t(i,j,k)==1 ) THEN
          t(i,j,k) = t(i,j,k) + y(iobs)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

  DO k=1,nlev
    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_q(i,j,k)==1 ) THEN
          q(i,j,k) = q(i,j,k) + y(iobs)
          iobs = iobs + 1
        END IF
      END DO
    END DO
  END DO

    DO j=1,nlat
      DO i=1,nlon
        IF( ex_obs_ps(i,j)==1 ) THEN
          ps(i,j) = ps(i,j) + y(iobs)
          iobs = iobs + 1
        END IF
      END DO
    END DO

  RETURN
END SUBROUTINE AD_Trans_XtoY

END MODULE common_speedy
