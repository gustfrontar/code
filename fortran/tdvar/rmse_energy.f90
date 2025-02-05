PROGRAM rmse_energy
  USE common
  USE common_speedy

  IMPLICIT NONE

  INTEGER(8),PARAMETER :: itimes = 1982010100
  INTEGER(8),PARAMETER :: itimee = 1982022818
  REAL(r_size),PARAMETER :: tref = 270.0d0
  REAL(r_size),PARAMETER :: pref = 1000.0d2
  REAL(r_size) :: uf(nlon,nlat,nlev)
  REAL(r_size) :: vf(nlon,nlat,nlev)
  REAL(r_size) :: tf(nlon,nlat,nlev)
  REAL(r_size) :: qf(nlon,nlat,nlev)
  REAL(r_size) :: psf(nlon,nlat)
  REAL(r_size) :: ut(nlon,nlat,nlev)
  REAL(r_size) :: vt(nlon,nlat,nlev)
  REAL(r_size) :: tt(nlon,nlat,nlev)
  REAL(r_size) :: qt(nlon,nlat,nlev)
  REAL(r_size) :: pst(nlon,nlat)
!                             1234567890123456789012345678901
  CHARACTER(22) :: analfname='analf/yyyymmddhh_p.grd'
  CHARACTER(22) :: truefname='truef/yyyymmddhh_p.grd'
  INTEGER :: iy,im,id,ih
  INTEGER :: it,irec
  INTEGER :: i,j,k
  INTEGER(8) :: itime
  LOGICAL :: ex

  REAL(r_size),ALLOCATABLE :: rmse_wind(:)
  REAL(r_size),ALLOCATABLE :: rmse_t(:)
  REAL(r_size),ALLOCATABLE :: rmse_ps(:)
  REAL(r_sngl) :: rmse4

  CALL com_time2ymdh(itimes,iy,im,id,ih)
  print *,iy,im,id,ih
  it=0
  DO
    CALL com_ymdh2time(iy,im,id,ih,itime)
    IF(itime>itimee) EXIT
    it = it+1
    CALL com_timeinc_hr(iy,im,id,ih,6)
  END DO

  ALLOCATE( rmse_wind(it) )
  ALLOCATE( rmse_t(it) )
  ALLOCATE( rmse_ps(it) )

  rmse_wind = 0.0d0
  rmse_t = 0.0d0
  rmse_ps = 0.0d0

  CALL com_time2ymdh(itimes,iy,im,id,ih)
  it=0
  DO
    CALL com_ymdh2time(iy,im,id,ih,itime)
    IF(itime>itimee) EXIT
    it = it+1
    WRITE(analfname(7:10),'(I4.4)') iy
    WRITE(analfname(11:12),'(I2.2)') im
    WRITE(analfname(13:14),'(I2.2)') id
    WRITE(analfname(15:16),'(I2.2)') ih
    WRITE(truefname(7:10),'(I4.4)') iy
    WRITE(truefname(11:12),'(I2.2)') im
    WRITE(truefname(13:14),'(I2.2)') id
    WRITE(truefname(15:16),'(I2.2)') ih
    INQUIRE(FILE=analfname,EXIST=ex)
    IF(ex) THEN
      CALL open_grd(2,analfname)
      CALL read_grd(2,uf,vf,tf,qf,psf)
      CALL close_grd(2)
      CALL open_grd(2,truefname)
      CALL read_grd(2,ut,vt,tt,qt,pst)
      CALL close_grd(2)
      rmse_wind(it) = 0.5d0 * SUM((ut-uf)**2)/REAL(nlon*nlat*nlev,r_size) &
        & + 0.5d0 * SUM((vt-vf)**2)/REAL(nlon*nlat*nlev,r_size)
      rmse_t(it) = 0.5d0 * cp/tref*SUM((tt-tf)**2)/REAL(nlon*nlat*nlev,r_size)
      rmse_ps(it)= 0.5d0 * rd*tref/pref**2*SUM((pst-psf)**2)/REAL(nlon*nlat,r_size)
    ELSE
      PRINT *,'no such file:',analfname
      rmse_wind(it) = 0.0d0
      rmse_t(it) = 0.0d0
      rmse_ps(it) = 0.0d0
    END IF
    CALL com_timeinc_hr(iy,im,id,ih,6)
  END DO

  OPEN(90,file='rmse_energy.grd',form='unformatted',access='direct',recl=4)
  irec = 1
  DO i=1,it
    rmse4 = rmse_wind(i) + rmse_t(i) + rmse_ps(i) ! total energy
    WRITE(90,rec=irec) rmse4
    irec = irec + 1

    rmse4 = rmse_wind(i)
    WRITE(90,rec=irec) rmse4
    irec = irec + 1

    rmse4 = rmse_t(i)
    WRITE(90,rec=irec) rmse4
    irec = irec + 1

    rmse4 = rmse_ps(i)
    WRITE(90,rec=irec) rmse4
    irec = irec + 1
  END DO
  CLOSE(90)

  STOP
END PROGRAM rmse_energy
