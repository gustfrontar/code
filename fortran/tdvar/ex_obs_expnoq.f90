PROGRAM ex_obs
  IMPLICIT NONE

  LOGICAL,PARAMETER :: msw_test=.FALSE.
  LOGICAL,PARAMETER :: msw_real=.TRUE.
  LOGICAL,PARAMETER :: msw_dnsobs=.FALSE.
  INTEGER,PARAMETER :: nlon=96
  INTEGER,PARAMETER :: nlat=48
  INTEGER,PARAMETER :: nlev=7

  INTEGER :: ilon,ilat,ios
  CHARACTER(10) :: ctmp1,ctmp2
  INTEGER :: i,j,k

  INTEGER :: ex_u(nlon,nlat,nlev) = 0
  INTEGER :: ex_v(nlon,nlat,nlev) = 0
  INTEGER :: ex_t(nlon,nlat,nlev) = 0
  INTEGER :: ex_q(nlon,nlat,nlev) = 0
  INTEGER :: ex_ps(nlon,nlat) = 0
  INTEGER :: ex_prec(nlon,nlat) = 0

  IF (msw_test) THEN
    ex_u(76,35,4) = 1
  ELSE IF (msw_real) THEN
    OPEN(10,file='obsmark.gs')
    READ(10,'(A)')
    READ(10,'(A)')
    READ(10,'(A)')
    READ(10,'(A)')
    READ(10,'(A)')
    READ(10,'(A)')
    DO
      READ(10,'(A10,I2,A1,I2)',IOSTAT=ios) ctmp1,ilon,ctmp2,ilat
      IF( ios /= 0 ) EXIT
!      PRINT *,ilon,ilat,ios
      ex_u(ilon,ilat,1:nlev) = 1
      ex_v(ilon,ilat,1:nlev) = 1
      ex_t(ilon,ilat,1:nlev) = 1
!      ex_q(ilon,ilat,1:nlev) = 1
      ex_ps(ilon,ilat) = 1
    END DO
    CLOSE(10)
  ELSE IF (msw_dnsobs) THEN
    DO k=1,nlev
      ex_u(1:96:2,1:48:2,k) = 1
      ex_v(1:96:2,1:48:2,k) = 1
      ex_t(1:96:2,1:48:2,k) = 1
      ex_q(1:96:2,1:48:2,k) = 1
    END DO
    ex_ps(1:96:2,1:48:2) = 1
  ELSE
    DO k=1,nlev
      ex_u(1:96:4,4:48:4,k) = 1
      ex_v(1:96:4,4:48:4,k) = 1
      ex_t(1:96:4,4:48:4,k) = 1
      ex_q(1:96:4,4:48:4,k) = 1
    END DO
    ex_ps(1:96:4,4:48:4) = 1
  END IF

  PRINT '(I5,A,F5.1,A)',SUM(ex_u(:,:,3)),' STATIONS (',&
    & 100.*REAL(SUM(ex_u(:,:,3)))/REAL(nlon*nlat),'%)'

  WRITE(22) ex_u
  WRITE(22) ex_v
  WRITE(22) ex_t
  WRITE(22) ex_q
  WRITE(22) ex_ps
  WRITE(22) ex_prec

  STOP
END PROGRAM ex_obs
