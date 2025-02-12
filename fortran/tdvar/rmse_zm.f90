PROGRAM rmse_zm
  USE common
  USE common_speedy

  IMPLICIT NONE

  INTEGER(8),PARAMETER :: itimes = 1982010100
  INTEGER(8),PARAMETER :: itimee = 1982022818
  REAL(r_size) :: uf(nlon,nlat,nlev)
  REAL(r_size) :: vf(nlon,nlat,nlev)
  REAL(r_size) :: tf(nlon,nlat,nlev)
  REAL(r_size) :: qf(nlon,nlat,nlev)
  REAL(r_size) :: zf(nlon,nlat,nlev)
  REAL(r_size) :: psf(nlon,nlat)
  REAL(r_size) :: ut(nlon,nlat,nlev)
  REAL(r_size) :: vt(nlon,nlat,nlev)
  REAL(r_size) :: tt(nlon,nlat,nlev)
  REAL(r_size) :: qt(nlon,nlat,nlev)
  REAL(r_size) :: zt(nlon,nlat,nlev)
  REAL(r_size) :: pst(nlon,nlat)
!                             1234567890123456789012345678901
  CHARACTER(22) :: analfname='analf/yyyymmddhh_p.grd'
  CHARACTER(22) :: truefname='truef/yyyymmddhh_p.grd'
  INTEGER :: iy,im,id,ih
  INTEGER :: it,irec
  INTEGER :: i,j,k
  INTEGER(8) :: itime
  LOGICAL :: ex

  REAL(r_size),ALLOCATABLE :: rmse_u(:,:,:)
  REAL(r_size),ALLOCATABLE :: rmse_v(:,:,:)
  REAL(r_size),ALLOCATABLE :: rmse_t(:,:,:)
  REAL(r_size),ALLOCATABLE :: rmse_q(:,:,:)
  REAL(r_size),ALLOCATABLE :: rmse_z(:,:,:)
  REAL(r_size),ALLOCATABLE :: rmse_ps(:,:)
  REAL(r_sngl) :: rmse4(nlat)

  CALL set_common_speedy


  CALL com_time2ymdh(itimes,iy,im,id,ih)
  print *,iy,im,id,ih
  it=0
  DO
    CALL com_ymdh2time(iy,im,id,ih,itime)
    IF(itime>itimee) EXIT
    it = it+1
    CALL com_timeinc_hr(iy,im,id,ih,6)
  END DO

  ALLOCATE( rmse_u(nlat,nlev,it) )
  ALLOCATE( rmse_v(nlat,nlev,it) )
  ALLOCATE( rmse_t(nlat,nlev,it) )
  ALLOCATE( rmse_q(nlat,nlev,it) )
  ALLOCATE( rmse_z(nlat,nlev,it) )
  ALLOCATE( rmse_ps(nlat,it) )

  rmse_u = 0.0d0
  rmse_v = 0.0d0
  rmse_t = 0.0d0
  rmse_q = 0.0d0
  rmse_z = 0.0d0
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
      CALL read_grd_p(2,uf,vf,tf,qf,zf,psf)
      CALL close_grd(2)
      CALL open_grd(2,truefname)
      CALL read_grd_p(2,ut,vt,tt,qt,zt,pst)
      CALL close_grd(2)
      DO j=1,nlat
        DO k=1,nlev
          rmse_u(j,k,it)=SQRT(SUM((ut(:,j,k)-uf(:,j,k))**2)/REAL(nlon,r_size))
          rmse_v(j,k,it)=SQRT(SUM((vt(:,j,k)-vf(:,j,k))**2)/REAL(nlon,r_size))
          rmse_t(j,k,it)=SQRT(SUM((tt(:,j,k)-tf(:,j,k))**2)/REAL(nlon,r_size))
          rmse_q(j,k,it)=SQRT(SUM((qt(:,j,k)-qf(:,j,k))**2)/REAL(nlon,r_size))
          rmse_z(j,k,it)=SQRT(SUM((zt(:,j,k)-zf(:,j,k))**2)/REAL(nlon,r_size))
        END DO
        rmse_ps(j,it)=SQRT(SUM((pst(:,j)-psf(:,j))**2)/REAL(nlon,r_size))
      END DO
    ELSE
      PRINT *,'no such file:',analfname
      rmse_u(:,:,it) = 0.0d0
      rmse_v(:,:,it) = 0.0d0
      rmse_t(:,:,it) = 0.0d0
      rmse_q(:,:,it) = 0.0d0
      rmse_z(:,:,it) = 0.0d0
      rmse_ps(:,it) = 0.0d0
    END IF
    CALL com_timeinc_hr(iy,im,id,ih,6)
  END DO

  OPEN(90,file='rmse_zm.grd',form='unformatted',access='direct',recl=4*nlat)
  irec = 1
  DO i=1,it
    DO k=1,nlev
      rmse4 = rmse_u(:,k,i)
      WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
      irec = irec + 1
    END DO
    DO k=1,nlev
      rmse4 = rmse_v(:,k,i)
      WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
      irec = irec + 1
    END DO
    DO k=1,nlev
      rmse4 = rmse_t(:,k,i)
      WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
      irec = irec + 1
    END DO
    DO k=1,nlev
      rmse4 = rmse_q(:,k,i)
      WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
      irec = irec + 1
    END DO
    DO k=1,nlev
      rmse4 = rmse_z(:,k,i)
      WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
      irec = irec + 1
    END DO
    rmse4 = rmse_ps(:,i)
    WRITE(90,rec=irec) (rmse4(j),j=1,nlat)
    irec = irec + 1
  END DO
  CLOSE(90)

  STOP
END PROGRAM rmse_zm
