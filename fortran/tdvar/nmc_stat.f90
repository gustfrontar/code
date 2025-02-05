PROGRAM nmc_stat
  USE common
  USE common_speedy
  USE tdvar_tools

  IMPLICIT NONE

  INTEGER,PARAMETER :: ncorr=20

  REAL(r_size) :: u1(nlon,nlat,nlev)
  REAL(r_size) :: v1(nlon,nlat,nlev)
  REAL(r_size) :: t1(nlon,nlat,nlev)
  REAL(r_size) :: q1(nlon,nlat,nlev)
  REAL(r_size) :: ps1(nlon,nlat)
  REAL(r_size) :: u2(nlon,nlat,nlev)
  REAL(r_size) :: v2(nlon,nlat,nlev)
  REAL(r_size) :: t2(nlon,nlat,nlev)
  REAL(r_size) :: q2(nlon,nlat,nlev)
  REAL(r_size) :: ps2(nlon,nlat)
  REAL(r_size) :: std_u(nlon,nlat,nlev) = 0.0d0
  REAL(r_size) :: std_v(nlon,nlat,nlev) = 0.0d0
  REAL(r_size) :: std_t(nlon,nlat,nlev) = 0.0d0
  REAL(r_size) :: std_q(nlon,nlat,nlev) = 0.0d0
  REAL(r_size) :: std_ps(nlon,nlat) = 0.0d0
  REAL(r_size) :: xcorr_u(0:ncorr,nlat,nlev) = 0.0d0
  REAL(r_size) :: xcorr_v(0:ncorr,nlat,nlev) = 0.0d0
  REAL(r_size) :: xcorr_t(0:ncorr,nlat,nlev) = 0.0d0
  REAL(r_size) :: xcorr_q(0:ncorr,nlat,nlev) = 0.0d0
  REAL(r_size) :: xcorr_ps(0:ncorr,nlat) = 0.0d0
  REAL(r_size) :: ycorr_u(0:ncorr,nlev) = 0.0d0
  REAL(r_size) :: ycorr_v(0:ncorr,nlev) = 0.0d0
  REAL(r_size) :: ycorr_t(0:ncorr,nlev) = 0.0d0
  REAL(r_size) :: ycorr_q(0:ncorr,nlev) = 0.0d0
  REAL(r_size) :: ycorr_ps(0:ncorr) = 0.0d0
  REAL(r_size) :: xi_u(nlat,nlev) = 0.0d0
  REAL(r_size) :: xi_v(nlat,nlev) = 0.0d0
  REAL(r_size) :: xi_t(nlat,nlev) = 0.0d0
  REAL(r_size) :: xi_q(nlat,nlev) = 0.0d0
  REAL(r_size) :: xi_ps(nlat) = 0.0d0
  REAL(r_size) :: eta_u(nlev) = 0.0d0
  REAL(r_size) :: eta_v(nlev) = 0.0d0
  REAL(r_size) :: eta_t(nlev) = 0.0d0
  REAL(r_size) :: eta_q(nlev) = 0.0d0
  REAL(r_size) :: eta_ps = 0.0d0
  REAL(r_size) :: ug1(nlon,nlat,nlev)
  REAL(r_size) :: vg1(nlon,nlat,nlev)
  REAL(r_size) :: ug2(nlon,nlat,nlev)
  REAL(r_size) :: vg2(nlon,nlat,nlev)
  REAL(r_size) :: reg_u(nlat,nlev) = 0.0d0
  REAL(r_size) :: reg_v(nlat,nlev) = 0.0d0
  REAL(r_size) :: varug(nlat,nlev) = 0.0d0
  REAL(r_size) :: varvg(nlat,nlev) = 0.0d0
  REAL(r_sngl) :: xcorr4(0:ncorr,nlat,nlev)
  REAL(r_sngl) :: ycorr4(0:ncorr,nlev)
  REAL(r_sngl) :: reg_4(nlat,nlev)
  INTEGER :: i,j,k,n,loop,irec

  INTEGER :: iy,im,id,ih
  INTEGER :: iye,ime,ide,ihe
  CHARACTER(23) :: fcst1name='nmc_18hr/yyyymmddhh.grd'
  CHARACTER(23) :: fcst2name='nmc_24hr/yyyymmddhh.grd'
!                             123456789012345678901234567890

  iy = 1982
  im = 1
  id = 10 
  ih = 00
  iye = 1982
  ime = 1
  ide = 31
  ihe = 18

  CALL set_common_speedy

  loop = 0
  DO
    IF( iy==iye .AND. im==ime .AND. id==ide .AND. ih==ihe ) EXIT

    WRITE(fcst1name(10:13),'(I4.4)') iy
    WRITE(fcst1name(14:15),'(I2.2)') im
    WRITE(fcst1name(16:17),'(I2.2)') id
    WRITE(fcst1name(18:19),'(I2.2)') ih

    WRITE(fcst2name(10:13),'(I4.4)') iy
    WRITE(fcst2name(14:15),'(I2.2)') im
    WRITE(fcst2name(16:17),'(I2.2)') id
    WRITE(fcst2name(18:19),'(I2.2)') ih

    CALL open_grd(11,fcst1name)
    CALL open_grd(12,fcst2name)
    CALL read_grd(11,u1,v1,t1,q1,ps1)
    CALL read_grd(12,u2,v2,t2,q2,ps2)
    CALL close_grd(11)
    CALL close_grd(12)

    CALL geostrophy(t1,ps1,ug1,vg1)
    CALL geostrophy(t2,ps2,ug2,vg2)

    std_u = std_u + (u1-u2)*(u1-u2)
    std_v = std_v + (v1-v2)*(v1-v2)
    std_t = std_t + (t1-t2)*(t1-t2)
    std_q = std_q + (q1-q2)*(q1-q2)
    std_ps = std_ps + (ps1-ps2)*(ps1-ps2)

    DO n=0,ncorr
      DO i=n+1,nlon
        xcorr_u(n,:,:) = xcorr_u(n,:,:) &
          & + (u1(i,:,:)-u2(i,:,:))*(u1(i-n,:,:)-u2(i-n,:,:))
        xcorr_v(n,:,:) = xcorr_v(n,:,:) &
          & + (v1(i,:,:)-v2(i,:,:))*(v1(i-n,:,:)-v2(i-n,:,:))
        xcorr_t(n,:,:) = xcorr_t(n,:,:) &
          & + (t1(i,:,:)-t2(i,:,:))*(t1(i-n,:,:)-t2(i-n,:,:))
        xcorr_q(n,:,:) = xcorr_q(n,:,:) &
          & + (q1(i,:,:)-q2(i,:,:))*(q1(i-n,:,:)-q2(i-n,:,:))
        xcorr_ps(n,:) = xcorr_ps(n,:) &
          & + (ps1(i,:)-ps2(i,:))*(ps1(i-n,:)-ps2(i-n,:))
      END DO
      DO j=n+1,nlat
        DO k=1,nlev
          ycorr_u(n,k) = ycorr_u(n,k) &
            & + SUM((u1(:,j,k)-u2(:,j,k))*(u1(:,j-n,k)-u2(:,j-n,k))) &
            & / REAL(nlon,r_size)
          ycorr_v(n,k) = ycorr_v(n,k) &
            & + SUM((v1(:,j,k)-v2(:,j,k))*(v1(:,j-n,k)-v2(:,j-n,k))) &
            & / REAL(nlon,r_size)
          ycorr_t(n,k) = ycorr_t(n,k) &
            & + SUM((t1(:,j,k)-t2(:,j,k))*(t1(:,j-n,k)-t2(:,j-n,k))) &
            & / REAL(nlon,r_size)
          ycorr_q(n,k) = ycorr_q(n,k) &
            & + SUM((q1(:,j,k)-q2(:,j,k))*(q1(:,j-n,k)-q2(:,j-n,k))) &
            & / REAL(nlon,r_size)
        END DO
        ycorr_ps(n) = ycorr_ps(n) &
          & + SUM((ps1(:,j)-ps2(:,j))*(ps1(:,j-n)-ps2(:,j-n))) &
          & / REAL(nlon,r_size)
      END DO
    END DO

    DO k=1,nlev
      DO j=1,nlat
        reg_u(j,k) = reg_u(j,k) &
          & + SUM((u1(:,j,k)-u2(:,j,k))*(ug1(:,j,k)-ug2(:,j,k))) &
          & / REAL(nlon,r_size)
        varug(j,k) = varug(j,k) + SUM((ug1(:,j,k)-ug2(:,j,k))**2) &
          & / REAL(nlon,r_size)
        reg_v(j,k) = reg_v(j,k) &
          & + SUM((v1(:,j,k)-v2(:,j,k))*(vg1(:,j,k)-vg2(:,j,k))) &
          & / REAL(nlon,r_size)
        varvg(j,k) = varvg(j,k) + SUM((vg1(:,j,k)-vg2(:,j,k))**2) &
          & / REAL(nlon,r_size)
      END DO
    END DO

    loop = loop + 1
    CALL com_timeinc_hr(iy,im,id,ih,6)

  END DO

  std_u = SQRT( std_u / REAL(loop,r_size))
  std_v = SQRT( std_v / REAL(loop,r_size))
  std_t = SQRT( std_t / REAL(loop,r_size))
  std_q = SQRT( std_q / REAL(loop,r_size))
  std_ps = SQRT( std_ps / REAL(loop,r_size))

!  DO n=0,ncorr
!    xcorr_u(n,:,:) = xcorr_u(n,:,:) / REAL(loop,r_size) / REAL(nlon-n,r_size)
!    xcorr_v(n,:,:) = xcorr_v(n,:,:) / REAL(loop,r_size) / REAL(nlon-n,r_size)
!    xcorr_t(n,:,:) = xcorr_t(n,:,:) / REAL(loop,r_size) / REAL(nlon-n,r_size)
!    xcorr_q(n,:,:) = xcorr_q(n,:,:) / REAL(loop,r_size) / REAL(nlon-n,r_size)
!    xcorr_ps(n,:) = xcorr_ps(n,:) / REAL(loop,r_size) / REAL(nlon-n,r_size)
!    ycorr_u(n,:) = ycorr_u(n,:) / REAL(loop,r_size) / REAL(nlat-n,r_size)
!    ycorr_v(n,:) = ycorr_v(n,:) / REAL(loop,r_size) / REAL(nlat-n,r_size)
!    ycorr_t(n,:) = ycorr_t(n,:) / REAL(loop,r_size) / REAL(nlat-n,r_size)
!    ycorr_q(n,:) = ycorr_q(n,:) / REAL(loop,r_size) / REAL(nlat-n,r_size)
!    ycorr_ps(n) = ycorr_ps(n) / REAL(loop,r_size) / REAL(nlat-n,r_size)
!  END DO

  DO k=1,nlev
    DO j=1,nlat
      xcorr_u(0:ncorr,j,k) = xcorr_u(0:ncorr,j,k) / xcorr_u(0,j,k)
      xcorr_v(0:ncorr,j,k) = xcorr_v(0:ncorr,j,k) / xcorr_v(0,j,k)
      xcorr_t(0:ncorr,j,k) = xcorr_t(0:ncorr,j,k) / xcorr_t(0,j,k)
      xcorr_q(0:ncorr,j,k) = xcorr_q(0:ncorr,j,k) / xcorr_q(0,j,k)
    END DO
    ycorr_u(0:ncorr,k) = ycorr_u(0:ncorr,k) / ycorr_u(0,k)
    ycorr_v(0:ncorr,k) = ycorr_v(0:ncorr,k) / ycorr_v(0,k)
    ycorr_t(0:ncorr,k) = ycorr_t(0:ncorr,k) / ycorr_t(0,k)
    ycorr_q(0:ncorr,k) = ycorr_q(0:ncorr,k) / ycorr_q(0,k)
  END DO
  DO j=1,nlat
    xcorr_ps(0:ncorr,j) = xcorr_ps(0:ncorr,j) / xcorr_ps(0,j)
  END DO
  ycorr_ps(0:ncorr) = ycorr_ps(0:ncorr) / ycorr_ps(0)

  reg_u = reg_u / varug
  reg_v = reg_v / varvg
  !
  ! GAUSSIAN FITTING TO LOOK FOR XI and ETA (LENGTH SCALE)
  !
  DO k=1,nlev
    DO j=1,nlat
      CALL gauss_fit(xcorr_u(0:ncorr,j,k),xi_u(j,k))
      CALL gauss_fit(xcorr_v(0:ncorr,j,k),xi_v(j,k))
      CALL gauss_fit(xcorr_t(0:ncorr,j,k),xi_t(j,k))
      CALL gauss_fit(xcorr_q(0:ncorr,j,k),xi_q(j,k))
    END DO
    CALL gauss_fit(ycorr_u(0:ncorr,k),eta_u(k))
    CALL gauss_fit(ycorr_v(0:ncorr,k),eta_v(k))
    CALL gauss_fit(ycorr_t(0:ncorr,k),eta_t(k))
    CALL gauss_fit(ycorr_q(0:ncorr,k),eta_q(k))
  END DO
  DO j=1,nlat
    CALL gauss_fit(xcorr_ps(0:ncorr,j),xi_ps(j))
  END DO
  CALL gauss_fit(ycorr_ps(0:ncorr),eta_ps)
  !
  ! Output
  !
  !
  ! STDEV
  !
  CALL open_grd(90,'dat_stat/stdev.grd')
  CALL write_grd(90,std_u,std_v,std_t,std_q,std_ps)
  CALL close_grd(90)
  !
  ! XCORR
  !
  irec=1
  OPEN(90,file='dat_stat/xcorr.grd',access='direct',recl=4*(ncorr+1)*nlat)
  DO k=1,nlev
    xcorr4 = xcorr_u
    WRITE(90,rec=irec) ((xcorr4(n,j,k),n=0,ncorr),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4 = xcorr_v
    WRITE(90,rec=irec) ((xcorr4(n,j,k),n=0,ncorr),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4 = xcorr_t
    WRITE(90,rec=irec) ((xcorr4(n,j,k),n=0,ncorr),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4 = xcorr_q
    WRITE(90,rec=irec) ((xcorr4(n,j,k),n=0,ncorr),j=1,nlat)
    irec = irec + 1
  END DO
  xcorr4(:,:,1) = xcorr_ps
  WRITE(90,rec=irec) ((xcorr4(n,j,1),n=0,ncorr),j=1,nlat)
  irec = irec + 1
  CLOSE(90)
  !
  ! XI
  !
  irec=1
  OPEN(90,file='dat_stat/xi.grd',access='direct',recl=4*nlat)
  DO k=1,nlev
    xcorr4(1,:,:) = xi_u(:,:)
    WRITE(90,rec=irec) (xcorr4(1,j,k),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4(1,:,:) = xi_v(:,:)
    WRITE(90,rec=irec) (xcorr4(1,j,k),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4(1,:,:) = xi_t(:,:)
    WRITE(90,rec=irec) (xcorr4(1,j,k),j=1,nlat)
    irec = irec + 1
  END DO
  DO k=1,nlev
    xcorr4(1,:,:) = xi_q(:,:)
    WRITE(90,rec=irec) (xcorr4(1,j,k),j=1,nlat)
    irec = irec + 1
  END DO
  xcorr4(1,:,1) = xi_ps(:)
  WRITE(90,rec=irec) (xcorr4(1,j,1),j=1,nlat)
  irec = irec + 1
  CLOSE(90)
  !
  ! YCORR
  !
  irec=1
  OPEN(90,file='dat_stat/ycorr.grd',access='direct',recl=4*(ncorr+1))
  DO k=1,nlev
    ycorr4 = ycorr_u
    WRITE(90,rec=irec) (ycorr4(n,k),n=0,ncorr)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4 = ycorr_v
    WRITE(90,rec=irec) (ycorr4(n,k),n=0,ncorr)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4 = ycorr_t
    WRITE(90,rec=irec) (ycorr4(n,k),n=0,ncorr)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4 = ycorr_q
    WRITE(90,rec=irec) (ycorr4(n,k),n=0,ncorr)
    irec = irec + 1
  END DO
  ycorr4(:,1) = ycorr_ps
  WRITE(90,rec=irec) (ycorr4(n,1),n=0,ncorr)
  irec = irec + 1
  CLOSE(90)
  !
  ! ETA
  !
  irec=1
  OPEN(90,file='dat_stat/eta.grd',access='direct',recl=4)
  DO k=1,nlev
    ycorr4(1,:) = eta_u(:)
    WRITE(90,rec=irec) ycorr4(1,k)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4(1,:) = eta_v(:)
    WRITE(90,rec=irec) ycorr4(1,k)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4(1,:) = eta_t(:)
    WRITE(90,rec=irec) ycorr4(1,k)
    irec = irec + 1
  END DO
  DO k=1,nlev
    ycorr4(1,:) = eta_q(:)
    WRITE(90,rec=irec) ycorr4(1,k)
    irec = irec + 1
  END DO
  ycorr4(1,1) = eta_ps
  WRITE(90,rec=irec) ycorr4(1,1)
  irec = irec + 1
  CLOSE(90)
  !
  ! REG
  !
  OPEN(90,file='dat_stat/reg.grd',access='direct',recl=4*nlat*nlev)
  reg_4 = reg_u
  WRITE(90,rec=1) ((reg_4(j,k),j=1,nlat),k=1,nlev)
  reg_4 = reg_v
  WRITE(90,rec=2) ((reg_4(j,k),j=1,nlat),k=1,nlev)
  CLOSE(90)

CONTAINS
SUBROUTINE gauss_fit(func,sigma)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: func(0:ncorr)
  REAL(r_size),INTENT(OUT) :: sigma
  REAL(r_size) :: gauss(0:ncorr)
  REAL(r_size) :: cost(100),costmin,costa,costb

  INTEGER :: i,n

  sigma = 0.0d0
  gauss = 0.0d0
  gauss(0) = 1.0d0

  costa = SUM((func(:)-gauss(:))**2)

  DO
    sigma = sigma + 0.5d0

    DO i=1,ncorr
      gauss(i) = exp(-0.5d0*REAL(i,r_size)/sigma**2)
    END DO

    costb = SUM((func(:)-gauss(:))**2)

    IF( costb > costa ) THEN
      sigma = sigma - 0.5d0
      EXIT
    END IF

    costa = costb
  END DO

  sigma = sigma - 0.5d0

  DO n=1,100
    DO i=1,ncorr
      gauss(i) = exp(-0.5d0*REAL(i,r_size)/sigma**2)
    END DO

    cost(n) = SUM((func(:)-gauss(:))**2)

    sigma = sigma + 0.01d0
  END DO

  sigma = sigma - 1.0d0

  costmin = MINVAL(cost)
  DO n=1,100
    IF(costmin==cost(n)) EXIT
  END DO

  sigma = sigma + 0.01 * REAL(n-1,r_size)


  RETURN
END SUBROUTINE gauss_fit

END PROGRAM nmc_stat
