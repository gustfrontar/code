MODULE tdvar_tools
  USE common
  USE common_speedy


  PUBLIC

  LOGICAL,PARAMETER :: msw_statread=.TRUE.      ! Read statistics from files
  LOGICAL,PARAMETER :: msw_stdhmean=.FALSE.     ! when this is true, it is better which is to average to get the reasonable
                                                ! statistics
  REAL(r_size),PARAMETER :: stdfact=1.4d0
  CHARACTER(18),PARAMETER :: stat_stdev='dat_stat/stdev.grd'
  CHARACTER(15),PARAMETER :: stat_xi='dat_stat/xi.grd'
  CHARACTER(16),PARAMETER :: stat_eta='dat_stat/eta.grd'
  CHARACTER(16),PARAMETER :: stat_reg='dat_stat/reg.grd'

  REAL(r_sngl),SAVE :: std4(nlon,nlat,nlev),xi4(nlat,nlev),eta4(nlev)
  REAL(r_sngl),SAVE :: reg4(nlat,nlev)
  REAL(r_size),SAVE :: stdu(nlon,nlat,nlev),xiu(nlat,nlev),etau(nlev)
  REAL(r_size),SAVE :: stdv(nlon,nlat,nlev),xiv(nlat,nlev),etav(nlev)
  REAL(r_size),SAVE :: stdt(nlon,nlat,nlev),xit(nlat,nlev),etat(nlev)
  REAL(r_size),SAVE :: stdq(nlon,nlat,nlev),xiq(nlat,nlev),etaq(nlev)
  REAL(r_size),SAVE :: stdps(nlon,nlat),xips(nlat),etaps
  REAL(r_size),SAVE :: regu(nlat,nlev)
  REAL(r_size),SAVE :: regv(nlat,nlev)
  REAL(r_size),SAVE :: alphax_u(4,nlat,nlev),alphay_u(4,nlev)
  REAL(r_size),SAVE :: alphax_v(4,nlat,nlev),alphay_v(4,nlev)
  REAL(r_size),SAVE :: alphax_t(4,nlat,nlev),alphay_t(4,nlev)
  REAL(r_size),SAVE :: alphax_q(4,nlat,nlev),alphay_q(4,nlev)
  REAL(r_size),SAVE :: alphax_ps(4,nlat),alphay_ps(4)
  REAL(r_size),SAVE :: sfactx_u(nlat,nlev),sfacty_u(nlev)
  REAL(r_size),SAVE :: sfactx_v(nlat,nlev),sfacty_v(nlev)
  REAL(r_size),SAVE :: sfactx_t(nlat,nlev),sfacty_t(nlev)
  REAL(r_size),SAVE :: sfactx_q(nlat,nlev),sfacty_q(nlev)
  REAL(r_size),SAVE :: sfactx_ps(nlat),sfacty_ps
  LOGICAL,SAVE :: msw_reg=.FALSE.

CONTAINS
!-----------------------------------------------------------------------
! [1] Set constants for 3DVAR
!-----------------------------------------------------------------------
SUBROUTINE set_tdvar_tools
  IMPLICIT NONE
  REAL(r_size) :: dymean
  INTEGER :: i,j,k,irec
  !
  ! Error standard deviation
  !
  IF(.NOT.msw_statread) THEN
    stdu = 2.0d0
    stdv = 2.0d0
    stdt = 2.0d0
    stdq = 2.0d-4
    stdps = 2.0d2
  ELSE
    irec = 1
    OPEN(10,file=stat_stdev,form='unformatted',access='direct',recl=4*nlon*nlat)
    DO k=1,nlev
      READ(10,rec=irec) ((std4(i,j,k),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
    stdu = REAL(std4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) ((std4(i,j,k),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
    stdv = REAL(std4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) ((std4(i,j,k),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
    stdt = REAL(std4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) ((std4(i,j,k),i=1,nlon),j=1,nlat)
      irec = irec + 1
    END DO
    stdq = REAL(std4,r_size)
    READ(10,rec=irec) ((std4(i,j,1),i=1,nlon),j=1,nlat)
    stdps = REAL(std4(:,:,1),r_size)
    CLOSE(10)
  END IF
  stdu = stdu * stdfact
  stdv = stdv * stdfact
  stdt = stdt * stdfact
  stdq = stdq * stdfact
  stdps = stdps * stdfact
  IF(msw_stdhmean) THEN
    DO k=1,nlev
      stdu(:,:,k) = SUM(stdu(:,:,k)) / REAL(nlon*nlat,r_size)
      stdv(:,:,k) = SUM(stdv(:,:,k)) / REAL(nlon*nlat,r_size)
      stdt(:,:,k) = SUM(stdt(:,:,k)) / REAL(nlon*nlat,r_size)
      stdq(:,:,k) = SUM(stdq(:,:,k)) / REAL(nlon*nlat,r_size)
    END DO
    stdps(:,:) = SUM(stdps(:,:)) / REAL(nlon*nlat,r_size)
  END IF
  !
  ! Horizontal Correlation length
  !
  IF(.NOT.msw_statread) THEN
    DO j=1,nlat
      DO k=1,nlev
        xiu(j,k) = 500.0d3 / dx(j)
        xiv(j,k) = 500.0d3 / dx(j)
        xit(j,k) = 500.0d3 / dx(j)
        xiq(j,k) = 500.0d3 / dx(j)
      END DO
      xips(j) = 500.0d3 / dx(j)
    END DO
    dymean = SUM(dy) / REAL(nlat,r_size)
    DO k=1,nlev
      etau(k) = 500.0d3 / dymean
      etav(k) = 500.0d3 / dymean
      etat(k) = 500.0d3 / dymean
      etaq(k) = 500.0d3 / dymean
    END DO
    etaps = 500.0d3 / dymean
  ELSE
    irec = 1
    OPEN(10,file=stat_xi,form='unformatted',access='direct',recl=4*nlat)
    DO k=1,nlev
      READ(10,rec=irec) (xi4(j,k),j=1,nlat)
      irec = irec + 1
    END DO
    xiu = REAL(xi4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) (xi4(j,k),j=1,nlat)
      irec = irec + 1
    END DO
    xiv = REAL(xi4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) (xi4(j,k),j=1,nlat)
      irec = irec + 1
    END DO
    xit = REAL(xi4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) (xi4(j,k),j=1,nlat)
      irec = irec + 1
    END DO
    xiq = REAL(xi4,r_size)
    READ(10,rec=irec) (xi4(j,1),j=1,nlat)
    xips = REAL(xi4(:,1),r_size)
    CLOSE(10)
    irec = 1
    OPEN(10,file=stat_eta,form='unformatted',access='direct',recl=4)
    DO k=1,nlev
      READ(10,rec=irec) eta4(k)
      irec = irec + 1
    END DO
    etau = REAL(eta4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) eta4(k)
      irec = irec + 1
    END DO
    etav = REAL(eta4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) eta4(k)
      irec = irec + 1
    END DO
    etat = REAL(eta4,r_size)
    DO k=1,nlev
      READ(10,rec=irec) eta4(k)
      irec = irec + 1
    END DO
    etaq = REAL(eta4,r_size)
    READ(10,rec=irec) eta4(1)
    irec = irec + 1
    etaps = REAL(eta4(1),r_size)
    CLOSE(10)
  END IF
  !
  ! Regression coefficient (Uu = U - reg * Ug)
  !
  IF(.NOT.msw_statread) THEN
    regu = 0.0d0
    regv = 0.0d0
  ELSE
    irec = 1
    OPEN(10,file=stat_reg,form='unformatted',access='direct',recl=4*nlat*nlev)
    READ(10,rec=1) ((reg4(j,k),j=1,nlat),k=1,nlev)
    regu = REAL(reg4,r_size)
    READ(10,rec=2) ((reg4(j,k),j=1,nlat),k=1,nlev)
    regv = REAL(reg4,r_size)
    CLOSE(10)
  END IF
  msw_reg=.TRUE.
  IF( MAX(MAXVAL(ABS(regu)),MAXVAL(ABS(regv)))==0.0d0 ) msw_reg=.FALSE.
  !
  ! Set Recursive Filter coefficients
  !
  DO j=1,nlat
    DO k=1,nlev
      CALL set_rfilter(xiu(j,k),alphax_u(:,j,k),sfactx_u(j,k))
      CALL set_rfilter(xiv(j,k),alphax_v(:,j,k),sfactx_v(j,k))
      CALL set_rfilter(xit(j,k),alphax_t(:,j,k),sfactx_t(j,k))
      CALL set_rfilter(xiq(j,k),alphax_q(:,j,k),sfactx_q(j,k))
    END DO
    CALL set_rfilter(xips(j),alphax_ps(:,j),sfactx_ps(j))
  END DO


  DO k=1,nlev
    CALL set_rfilter(etau(k),alphay_u(:,k),sfacty_u(k))
    CALL set_rfilter(etav(k),alphay_v(:,k),sfacty_v(k))
    CALL set_rfilter(etat(k),alphay_t(:,k),sfacty_t(k))
    CALL set_rfilter(etaq(k),alphay_q(:,k),sfacty_q(k))
  END DO
  CALL set_rfilter(etaps,alphay_ps(:),sfacty_ps)

  RETURN
END SUBROUTINE set_tdvar_tools
!-----------------------------------------------------------------------
! [2] Cost function
!-----------------------------------------------------------------------
SUBROUTINE cost_func(gues,ctrl,dep,costb,costy,gradcost)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: gues(ngpv)
  REAL(r_size),INTENT(IN) :: ctrl(ngpv)
  REAL(r_size),INTENT(IN) :: dep(nobs)
  REAL(r_size),INTENT(OUT) :: costb,costy
  REAL(r_size),INTENT(OUT) :: gradcost(ngpv)

  REAL(r_size) :: u(nlon,nlat,nlev)
  REAL(r_size) :: v(nlon,nlat,nlev)
  REAL(r_size) :: t(nlon,nlat,nlev)
  REAL(r_size) :: q(nlon,nlat,nlev)
  REAL(r_size) :: ps(nlon,nlat)
  REAL(r_size) :: u1(nlon,nlat,nlev)
  REAL(r_size) :: v1(nlon,nlat,nlev)
  REAL(r_size) :: t1(nlon,nlat,nlev)
  REAL(r_size) :: q1(nlon,nlat,nlev)
  REAL(r_size) :: ps1(nlon,nlat)
  REAL(r_size) :: u9(nlon,nlat,nlev)
  REAL(r_size) :: v9(nlon,nlat,nlev)
  REAL(r_size) :: t9(nlon,nlat,nlev)
  REAL(r_size) :: q9(nlon,nlat,nlev)
  REAL(r_size) :: ps9(nlon,nlat)
  REAL(r_size) :: yctrl(nobs)
  REAL(r_size) :: ctrl1(ngpv)



  CALL reshape_gpv(gues,u9,v9,t9,q9,ps9)
  CALL reshape_gpv(ctrl,u,v,t,q,ps)

  CALL Trans_VtoX(u9,v9,t9,q9,ps9,u,v,t,q,ps,u1,v1,t1,q1,ps1)
  CALL Trans_XtoY(u1,v1,t1,q1,ps1,yctrl)

  yctrl = ( yctrl - dep ) / obs_err

  costb = 0.5d0 * dot_product(ctrl,ctrl)
  costy = 0.5d0 * dot_product(yctrl,yctrl)

  yctrl = yctrl / obs_err

  u1=0.0d0
  v1=0.0d0
  t1=0.0d0
  q1=0.0d0
  ps1=0.0d0
  u=0.0d0
  v=0.0d0
  t=0.0d0
  q=0.0d0
  ps=0.0d0

  CALL AD_Trans_XtoY(u1,v1,t1,q1,ps1,yctrl)
  CALL AD_Trans_VtoX(u9,v9,t9,q9,ps9,u,v,t,q,ps,u1,v1,t1,q1,ps1)

  CALL reshape_grd(u,v,t,q,ps,ctrl1)

  gradcost = ctrl + ctrl1

  RETURN
END SUBROUTINE cost_func
!-----------------------------------------------------------------------
! [3] Control Variable Transformation (V -> X)
!-----------------------------------------------------------------------
SUBROUTINE Trans_VtoX_gpv(gues,ctrl1,ctrl5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: gues(ngpv)
  REAL(r_size),INTENT(IN) :: ctrl1(ngpv)
  REAL(r_size),INTENT(OUT) :: ctrl5(ngpv)

  REAL(r_size) :: u9(nlon,nlat,nlev)
  REAL(r_size) :: v9(nlon,nlat,nlev)
  REAL(r_size) :: t9(nlon,nlat,nlev)
  REAL(r_size) :: q9(nlon,nlat,nlev)
  REAL(r_size) :: ps9(nlon,nlat)
  REAL(r_size) :: u1(nlon,nlat,nlev)
  REAL(r_size) :: v1(nlon,nlat,nlev)
  REAL(r_size) :: t1(nlon,nlat,nlev)
  REAL(r_size) :: q1(nlon,nlat,nlev)
  REAL(r_size) :: ps1(nlon,nlat)
  REAL(r_size) :: u5(nlon,nlat,nlev)
  REAL(r_size) :: v5(nlon,nlat,nlev)
  REAL(r_size) :: t5(nlon,nlat,nlev)
  REAL(r_size) :: q5(nlon,nlat,nlev)
  REAL(r_size) :: ps5(nlon,nlat)

  CALL reshape_gpv(gues,u9,v9,t9,q9,ps9)
  CALL reshape_gpv(ctrl1,u1,v1,t1,q1,ps1)

  CALL Trans_VtoX(u9,v9,t9,q9,ps9,u1,v1,t1,q1,ps1,u5,v5,t5,q5,ps5)

  CALL reshape_grd(u5,v5,t5,q5,ps5,ctrl5)

  RETURN
END SUBROUTINE Trans_VtoX_gpv

SUBROUTINE Trans_VtoX(u9,v9,t9,q9,ps9,u1,v1,t1,q1,ps1,u5,v5,t5,q5,ps5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps9(nlon,nlat)
  REAL(r_size),INTENT(IN) :: u1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps1(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: u5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps5(nlon,nlat)

  REAL(r_size) :: u2(nlon,nlat,nlev)
  REAL(r_size) :: v2(nlon,nlat,nlev)
  REAL(r_size) :: t2(nlon,nlat,nlev)
  REAL(r_size) :: q2(nlon,nlat,nlev)
  REAL(r_size) :: ps2(nlon,nlat)
  REAL(r_size) :: ug(nlon,nlat,nlev)
  REAL(r_size) :: vg(nlon,nlat,nlev)
  REAL(r_size) :: ug9(nlon,nlat,nlev)
  REAL(r_size) :: vg9(nlon,nlat,nlev)
  INTEGER :: j,k
  !
  ! Multiply error standard deviation
  !
  u2(:,:,:) = u1(:,:,:) * stdu(:,:,:)
  v2(:,:,:) = v1(:,:,:) * stdv(:,:,:)
  t2(:,:,:) = t1(:,:,:) * stdt(:,:,:)
  q2(:,:,:) = q1(:,:,:) * stdq(:,:,:)
  ps2(:,:) = ps1(:,:) * stdps(:,:)
  !
  ! Horizontal Correlation Transformation
  !
  DO k=1,nlev
    CALL rfilter_fwd(u2(:,:,k),alphax_u(:,:,k),alphay_u(:,k), &
      & sfactx_u(:,k),sfacty_u(k),u5(:,:,k))
    CALL rfilter_fwd(v2(:,:,k),alphax_v(:,:,k),alphay_v(:,k), &
      & sfactx_v(:,k),sfacty_v(k),v5(:,:,k))
    CALL rfilter_fwd(t2(:,:,k),alphax_t(:,:,k),alphay_t(:,k), &
      & sfactx_t(:,k),sfacty_t(k),t5(:,:,k))
    CALL rfilter_fwd(q2(:,:,k),alphax_q(:,:,k),alphay_q(:,k), &
      & sfactx_q(:,k),sfacty_q(k),q5(:,:,k))
  END DO
  CALL rfilter_fwd(ps2(:,:),alphax_ps(:,:),alphay_ps(:), &
    & sfactx_ps(:),sfacty_ps,ps5(:,:))
  !
  ! Inter-variable correlation
  !
  IF( msw_reg ) THEN
    CALL geostrophy(t9+t5,ps9+ps5,ug,vg)
    CALL geostrophy(t9,ps9,ug9,vg9)
    DO k=1,nlev
      DO j=1,nlat
        u5(:,j,k) = u5(:,j,k) + regu(j,k) * (ug(:,j,k)-ug9(:,j,k))
        v5(:,j,k) = v5(:,j,k) + regv(j,k) * (vg(:,j,k)-vg9(:,j,k))
      END DO
    END DO
  END IF

  RETURN
END SUBROUTINE Trans_VtoX

SUBROUTINE TL_Trans_VtoX(u9,v9,t9,q9,ps9,u1,v1,t1,q1,ps1,u5,v5,t5,q5,ps5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps9(nlon,nlat)
  REAL(r_size),INTENT(IN) :: u1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q1(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps1(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: u5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: v5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: t5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: q5(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: ps5(nlon,nlat)

  REAL(r_size) :: u2(nlon,nlat,nlev)
  REAL(r_size) :: v2(nlon,nlat,nlev)
  REAL(r_size) :: t2(nlon,nlat,nlev)
  REAL(r_size) :: q2(nlon,nlat,nlev)
  REAL(r_size) :: ps2(nlon,nlat)
  REAL(r_size) :: ug(nlon,nlat,nlev)
  REAL(r_size) :: vg(nlon,nlat,nlev)
  INTEGER :: j,k
  !
  ! Multiply error standard deviation
  !
  u2(:,:,:) = u1(:,:,:) * stdu(:,:,:)
  v2(:,:,:) = v1(:,:,:) * stdv(:,:,:)
  t2(:,:,:) = t1(:,:,:) * stdt(:,:,:)
  q2(:,:,:) = q1(:,:,:) * stdq(:,:,:)
  ps2(:,:) = ps1(:,:) * stdps(:,:)
  !
  ! Horizontal Correlation Transformation
  !
  DO k=1,nlev
    CALL rfilter_fwd(u2(:,:,k),alphax_u(:,:,k),alphay_u(:,k), &
      & sfactx_u(:,k),sfacty_u(k),u5(:,:,k))
    CALL rfilter_fwd(v2(:,:,k),alphax_v(:,:,k),alphay_v(:,k), &
      & sfactx_v(:,k),sfacty_v(k),v5(:,:,k))
    CALL rfilter_fwd(t2(:,:,k),alphax_t(:,:,k),alphay_t(:,k), &
      & sfactx_t(:,k),sfacty_t(k),t5(:,:,k))
    CALL rfilter_fwd(q2(:,:,k),alphax_q(:,:,k),alphay_q(:,k), &
      & sfactx_q(:,k),sfacty_q(k),q5(:,:,k))
  END DO
  CALL rfilter_fwd(ps2(:,:),alphax_ps(:,:),alphay_ps(:), &
    & sfactx_ps(:),sfacty_ps,ps5(:,:))
  !
  ! Inter-variable correlation
  !
  IF( msw_reg ) THEN
    CALL TL_geostrophy(t9,ps9,t5,ps5,ug,vg)
    DO k=1,nlev
      DO j=1,nlat
        u5(:,j,k) = u5(:,j,k) + regu(j,k) * ug(:,j,k)
        v5(:,j,k) = v5(:,j,k) + regv(j,k) * vg(:,j,k)
      END DO
    END DO
  END IF

  RETURN
END SUBROUTINE TL_Trans_VtoX

SUBROUTINE AD_Trans_VtoX(u9,v9,t9,q9,ps9,u1,v1,t1,q1,ps1,u5,v5,t5,q5,ps5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: u9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps9(nlon,nlat)
  REAL(r_size),INTENT(INOUT) :: u1(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: v1(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: t1(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: q1(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: ps1(nlon,nlat)
  REAL(r_size),INTENT(IN) :: u5(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: v5(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: t5(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: q5(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps5(nlon,nlat)

  REAL(r_size) :: u2(nlon,nlat,nlev)
  REAL(r_size) :: v2(nlon,nlat,nlev)
  REAL(r_size) :: t2(nlon,nlat,nlev)
  REAL(r_size) :: q2(nlon,nlat,nlev)
  REAL(r_size) :: ps2(nlon,nlat)
  REAL(r_size) :: t3(nlon,nlat,nlev)
  REAL(r_size) :: ps3(nlon,nlat)
  REAL(r_size) :: ug(nlon,nlat,nlev)
  REAL(r_size) :: vg(nlon,nlat,nlev)
  INTEGER :: j,k
  !
  ! Inter-variable correlation
  !
  t3 = t5
  ps3 = ps5
  IF( msw_reg ) THEN
    DO k=1,nlev
      DO j=1,nlat
!        v5(:,j,k) = v5(:,j,k) + regv(j,k) * vg(:,j,k)
        vg(:,j,k) = regv(j,k) * v5(:,j,k)
!        u5(:,j,k) = u5(:,j,k) + regu(j,k) * ug(:,j,k)
        ug(:,j,k) = regu(j,k) * u5(:,j,k)
      END DO
    END DO
!    CALL TL_geostrophy(t9,ps9,t5,ps5,ug,vg)
    CALL AD_geostrophy(t9,ps9,t3,ps3,ug,vg)
  END IF
  !
  ! Horizontal Correlation Transformation
  !
  DO k=1,nlev
    CALL rfilter_bwd(u5(:,:,k),alphax_u(:,:,k),alphay_u(:,k), &
      & sfactx_u(:,k),sfacty_u(k),u2(:,:,k))
    CALL rfilter_bwd(v5(:,:,k),alphax_v(:,:,k),alphay_v(:,k), &
      & sfactx_v(:,k),sfacty_v(k),v2(:,:,k))
    CALL rfilter_bwd(t3(:,:,k),alphax_t(:,:,k),alphay_t(:,k), &
      & sfactx_t(:,k),sfacty_t(k),t2(:,:,k))
    CALL rfilter_bwd(q5(:,:,k),alphax_q(:,:,k),alphay_q(:,k), &
      & sfactx_q(:,k),sfacty_q(k),q2(:,:,k))
  END DO
  CALL rfilter_bwd(ps3(:,:),alphax_ps(:,:),alphay_ps(:), &
    & sfactx_ps(:),sfacty_ps,ps2(:,:))
  !
  ! Multiply error standard deviation
  !
!  ps2(:,:) = ps1(:,:) * stdps(:,:)
  ps1(:,:) = ps1(:,:) + ps2(:,:) * stdps(:,:)
!  q2(:,:,:) = q1(:,:,:) * stdq(:,:,:)
  q1(:,:,:) = q1(:,:,:) + q2(:,:,:) * stdq(:,:,:)
!  t2(:,:,:) = t1(:,:,:) * stdt(:,:,:)
  t1(:,:,:) = t1(:,:,:) + t2(:,:,:) * stdt(:,:,:)
!  v2(:,:,:) = v1(:,:,:) * stdv(:,:,:)
  v1(:,:,:) = v1(:,:,:) + v2(:,:,:) * stdv(:,:,:)
!  u2(:,:,:) = u1(:,:,:) * stdu(:,:,:)
  u1(:,:,:) = u1(:,:,:) + u2(:,:,:) * stdu(:,:,:)

  RETURN
END SUBROUTINE AD_Trans_VtoX
!-----------------------------------------------------------------------
! [4] Compute geostrophic winds on sigma levels (T,Ps) -> (Ug,Vg)
!-----------------------------------------------------------------------
SUBROUTINE geostrophy(t,ps,ug,vg)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: ug(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: vg(nlon,nlat,nlev)
  REAL(r_size) :: phi(nlon,nlat,nlev)
  REAL(r_size) :: lps(nlon,nlat)
  REAL(r_size) :: dlpsdx(nlon,nlat)
  REAL(r_size) :: dlpsdy(nlon,nlat)
  REAL(r_size) :: dphidx(nlon,nlat)
  REAL(r_size) :: dphidy(nlon,nlat)
  INTEGER :: j,k
  !
  ! Geopotential height
  !
  phi(:,:,1) = phi0(:,:) - rd * t(:,:,1) * log(sig(1))
  DO k=2,nlev
    phi(:,:,k) = phi(:,:,k-1) - rd * (t(:,:,k) + t(:,:,k-1)) * 0.5d0 &
      & * log(sig(k)/sig(k-1))
  END DO
  !
  ! Geostrophic wind
  !
  lps = log(ps)
  CALL ddx(lps,dlpsdx)
  CALL ddy(lps,dlpsdy)

  ug = 0.0d0
  vg = 0.0d0

  DO k=1,nlev
    CALL ddx(phi(:,:,k),dphidx(:,:))
    CALL ddy(phi(:,:,k),dphidy(:,:))
    DO j=1,nlat
!      IF( fcori(j) > 0.5d-4) THEN
        ug(:,j,k) = - (rd * t(:,j,k) * dlpsdy(:,j) + dphidy(:,j)) / fcori(j)
        vg(:,j,k) =   (rd * t(:,j,k) * dlpsdx(:,j) + dphidx(:,j)) / fcori(j)
!      END IF
    END DO
  END DO

  RETURN
END SUBROUTINE geostrophy

SUBROUTINE TL_geostrophy(t9,ps9,t,ps,ug,vg)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: t9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps9(nlon,nlat)
  REAL(r_size),INTENT(IN) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: ug(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: vg(nlon,nlat,nlev)
  REAL(r_size) :: phi(nlon,nlat,nlev)
  REAL(r_size) :: lps(nlon,nlat)
  REAL(r_size) :: lps9(nlon,nlat)
  REAL(r_size) :: dlpsdx(nlon,nlat)
  REAL(r_size) :: dlpsdy(nlon,nlat)
  REAL(r_size) :: dlpsdx9(nlon,nlat)
  REAL(r_size) :: dlpsdy9(nlon,nlat)
  REAL(r_size) :: dphidx(nlon,nlat)
  REAL(r_size) :: dphidy(nlon,nlat)
  INTEGER :: j,k
  !
  ! Geopotential height
  !
!  phi(:,:,1) = phi0(:,:) - rd * t(:,:,1) * log(sig(1))
  phi(:,:,1) = - rd * t(:,:,1) * log(sig(1))
  DO k=2,nlev
    phi(:,:,k) = phi(:,:,k-1) - rd * (t(:,:,k) + t(:,:,k-1)) * 0.5d0 &
      & * log(sig(k)/sig(k-1))
  END DO
  !
  ! Geostrophic wind
  !
!  lps = log(ps)
  lps = ps / ps9
  CALL ddx(lps,dlpsdx)
  CALL ddy(lps,dlpsdy)

  lps9 = log(ps9)
  CALL ddx(lps9,dlpsdx9)
  CALL ddy(lps9,dlpsdy9)

  ug = 0.0d0
  vg = 0.0d0

  DO k=1,nlev
    CALL ddx(phi(:,:,k),dphidx(:,:))
    CALL ddy(phi(:,:,k),dphidy(:,:))
    DO j=1,nlat
!      IF( fcori(j) > 0.5d-4) THEN
!        ug(:,j,k) = - (rd * t(:,j,k) * dlpsdy(:,j) + dphidy(:,j)) / fcori(j)
        ug(:,j,k) = - (rd * (t9(:,j,k) * dlpsdy(:,j) + t(:,j,k) * dlpsdy9(:,j))&
          & + dphidy(:,j)) / fcori(j)
!        vg(:,j,k) =   (rd * t(:,j,k) * dlpsdx(:,j) + dphidx(:,j)) / fcori(j)
        vg(:,j,k) =   (rd * (t9(:,j,k) * dlpsdx(:,j) + t(:,j,k) * dlpsdx9(:,j))&
          & + dphidx(:,j)) / fcori(j)
!      END IF
    END DO
  END DO

  RETURN
END SUBROUTINE TL_geostrophy

SUBROUTINE AD_geostrophy(t9,ps9,t,ps,ug,vg)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: t9(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ps9(nlon,nlat)
  REAL(r_size),INTENT(INOUT) :: t(nlon,nlat,nlev)
  REAL(r_size),INTENT(INOUT) :: ps(nlon,nlat)
  REAL(r_size),INTENT(IN) :: ug(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: vg(nlon,nlat,nlev)
  REAL(r_size) :: phi(nlon,nlat,nlev)
  REAL(r_size) :: lps(nlon,nlat)
  REAL(r_size) :: lps9(nlon,nlat)
  REAL(r_size) :: dlpsdx(nlon,nlat)
  REAL(r_size) :: dlpsdy(nlon,nlat)
  REAL(r_size) :: dlpsdx9(nlon,nlat)
  REAL(r_size) :: dlpsdy9(nlon,nlat)
  REAL(r_size) :: dphidx(nlon,nlat)
  REAL(r_size) :: dphidy(nlon,nlat)
  INTEGER :: j,k

  lps9 = log(ps9)
  CALL ddx(lps9,dlpsdx9)
  CALL ddy(lps9,dlpsdy9)

  dlpsdx = 0.0d0
  dlpsdy = 0.0d0
  phi = 0.0d0
  lps = 0.0d0

!  DO k=1,nlev
  DO k=nlev,1,-1
    dphidx = 0.0d0
    dphidy = 0.0d0
!    DO j=1,nlat
    DO j=nlat,1,-1
!      IF( fcori(j) > 0.5d-4) THEN
!!        vg(:,j,k) =   (rd * t(:,j,k) * dlpsdx(:,j) + dphidx(:,j)) / fcori(j)
!        vg(:,j,k) =   (rd * (t9(:,j,k)*dlpsdx(:,j) + t(:,j,k)*dlpsdx9(:,j)) &
!          & + dphidx(:,j)) / fcori(j)
        dlpsdx(:,j) = dlpsdx(:,j) + rd * t9(:,j,k) * vg(:,j,k) / fcori(j)
        t(:,j,k) = t(:,j,k) + rd * dlpsdx9(:,j) * vg(:,j,k) / fcori(j)
        dphidx(:,j) = dphidx(:,j) + vg(:,j,k) / fcori(j)
!!        ug(:,j,k) = - (rd * t(:,j,k) * dlpsdy(:,j) + dphidy(:,j)) / fcori(j)
!        ug(:,j,k) = - (rd * (t9(:,j,k)*dlpsdy(:,j) + t(:,j,k)*dlpsdy9(:,j)) &
!          & + dphidy(:,j)) / fcori(j)
        dlpsdy(:,j) = dlpsdy(:,j) - rd * t9(:,j,k) * ug(:,j,k) / fcori(j)
        t(:,j,k) = t(:,j,k) - rd * dlpsdy9(:,j) * ug(:,j,k) / fcori(j)
        dphidy(:,j) = dphidy(:,j) - ug(:,j,k) / fcori(j)
!      END IF
    END DO
!    CALL ddy(phi(:,:,k),dphidy(:,:))
    CALL AD_ddy(phi(:,:,k),dphidy(:,:))
!    CALL ddx(phi(:,:,k),dphidx(:,:))
    CALL AD_ddx(phi(:,:,k),dphidx(:,:))
  END DO
  !
  ! Geostrophic wind
  !
!  CALL ddy(lps,dlpsdy)
  CALL AD_ddy(lps,dlpsdy)
!  CALL ddx(lps,dlpsdx)
  CALL AD_ddx(lps,dlpsdx)
!!  lps = log(ps)
!  lps = ps / ps9
  ps = ps + lps / ps9
  !
  ! Geopotential height
  !
!  DO k=2,nlev
  DO k=nlev,2,-1
!    phi(:,:,k) = phi(:,:,k-1) - rd * (t(:,:,k) + t(:,:,k-1)) * 0.5d0 &
!      & * log(sig(k)/sig(k-1))
    phi(:,:,k-1) = phi(:,:,k-1) + phi(:,:,k)
    t(:,:,k) = t(:,:,k) - rd * phi(:,:,k) * 0.5d0 * log(sig(k)/sig(k-1))
    t(:,:,k-1) = t(:,:,k-1) - rd * phi(:,:,k) * 0.5d0 * log(sig(k)/sig(k-1))
  END DO
!!  phi(:,:,1) = phi0(:,:) - rd * t(:,:,1) * log(sig(1))
!  phi(:,:,1) = - rd * t(:,:,1) * log(sig(1))
  t(:,:,1) = t(:,:,1) - rd * phi(:,:,1) * log(sig(1))

  RETURN
END SUBROUTINE AD_geostrophy
!-----------------------------------------------------------------------
! [5] Space differential
!-----------------------------------------------------------------------
!--[5.1] x-direction ---------------------------------------------------
SUBROUTINE ddx(var1,var5)
  REAL(r_size),INTENT(IN) :: var1(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: var5(nlon,nlat)

  DO j=1,nlat
    DO i=2,nlon-1
      var5(i,j) = ( var1(i+1,j) - var1(i-1,j) ) * 0.5d0 / dx(j)
    END DO
    var5(1,j) = ( var1(2,j) - var1(nlon,j) ) * 0.5d0 / dx(j)
    var5(nlon,j) = ( var1(1,j) - var1(nlon-1,j) ) * 0.5d0 / dx(j)
  END DO

  RETURN
END SUBROUTINE ddx

SUBROUTINE AD_ddx(var1,var5)
  REAL(r_size),INTENT(INOUT) :: var1(nlon,nlat)
  REAL(r_size),INTENT(IN) :: var5(nlon,nlat)

!  DO j=1,nlat
  DO j=nlat,1,-1
!    var5(nlon,j) = ( var1(1,j) - var1(nlon-1,j) ) * 0.5d0 / dx(j)
    var1(1,j) = var1(1,j) + var5(nlon,j) * 0.5d0 / dx(j)
    var1(nlon-1,j) = var1(nlon-1,j) - var5(nlon,j) * 0.5d0 / dx(j)
!    var5(1,j) = ( var1(2,j) - var1(nlon,j) ) * 0.5d0 / dx(j)
    var1(2,j) = var1(2,j) + var5(1,j) * 0.5d0 / dx(j)
    var1(nlon,j) = var1(nlon,j) - var5(1,j) * 0.5d0 / dx(j)
!    DO i=2,nlon-1
    DO i=nlon-1,2,-1
!      var5(i,j) = ( var1(i+1,j) - var1(i-1,j) ) * 0.5d0 / dx(j)
      var1(i+1,j) = var1(i+1,j) + var5(i,j) * 0.5d0 / dx(j)
      var1(i-1,j) = var1(i-1,j) - var5(i,j) * 0.5d0 / dx(j)
    END DO
  END DO

  RETURN
END SUBROUTINE AD_ddx
!--[5.2] y-direction ---------------------------------------------------
SUBROUTINE ddy(var1,var5)
  REAL(r_size),INTENT(IN) :: var1(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: var5(nlon,nlat)

  DO i=1,nlon
    DO j=2,nlat-1
      var5(i,j) = ( (var1(i,j) - var1(i,j-1)) * dy(j) / dy(j-1) &
        & + (var1(i,j+1) - var1(i,j)) * dy(j-1) / dy(j) ) &
        & / (dy(j) + dy(j-1))
    END DO
    IF (i > nlon/2) THEN
      var5(i,1) = ( (var1(i,1) - var1(i-nlon/2,1)) * dy(1) / dy(nlat) &
        & + (var1(i,2) - var1(i,1)) * dy(nlat) / dy(1) ) &
        & / (dy(1) + dy(nlat))
      var5(i,nlat) = ( (var1(i,nlat) - var1(i,nlat-1)) * dy(nlat) / dy(nlat-1) &
        & + (var1(i-nlon/2,nlat) - var1(i,nlat)) * dy(nlat-1) / dy(nlat) ) &
        & / (dy(nlat) + dy(nlat-1))
    ELSE
      var5(i,1) = ( (var1(i,1) - var1(i+nlon/2,1)) * dy(1) / dy(nlat) &
        & + (var1(i,2) - var1(i,1)) * dy(nlat) / dy(1) ) &
        & / (dy(1) + dy(nlat))
      var5(i,nlat) = ( (var1(i,nlat) - var1(i,nlat-1)) * dy(nlat) / dy(nlat-1) &
        & + (var1(i+nlon/2,nlat) - var1(i,nlat)) * dy(nlat-1) / dy(nlat) ) &
        & / (dy(nlat) + dy(nlat-1))
    END IF

  END DO

  RETURN
END SUBROUTINE ddy

SUBROUTINE AD_ddy(var1,var5)
  REAL(r_size),INTENT(INOUT) :: var1(nlon,nlat)
  REAL(r_size),INTENT(IN) :: var5(nlon,nlat)

!  DO i=1,nlon
  DO i=nlon,1,-1
    IF (i > nlon/2) THEN
!      var5(i,1) = ( (var1(i,1) - var1(i-nlon/2,1)) * dy(1) / dy(nlat) &
!        & + (var1(i,2) - var1(i,1)) * dy(nlat) / dy(1) ) &
!        & / (dy(1) + dy(nlat))
      var1(i,1) = var1(i,1) + var5(i,1) * dy(1) / dy(nlat) &
        & / (dy(1) + dy(nlat))
      var1(i-nlon/2,1) = var1(i-nlon/2,1) - var5(i,1) * dy(1) / dy(nlat) &
        & / (dy(1) + dy(nlat))
      var1(i,2) = var1(i,2) + var5(i,1) * dy(nlat) / dy(1) &
        & / (dy(1) + dy(nlat))
      var1(i,1) = var1(i,1) - var5(i,1) * dy(nlat) / dy(1) &
        & / (dy(1) + dy(nlat))
!      var5(i,nlat) = ( (var1(i,nlat) - var1(i,nlat-1)) * dy(nlat) / dy(nlat-1)&
!        & + (var1(i-nlon/2,nlat) - var1(i,nlat)) * dy(nlat-1) / dy(nlat) ) &
!        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat) = var1(i,nlat) + var5(i,nlat) * dy(nlat) / dy(nlat-1) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat-1) = var1(i,nlat-1) - var5(i,nlat) * dy(nlat) / dy(nlat-1) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i-nlon/2,nlat) = var1(i-nlon/2,nlat) + var5(i,nlat) * dy(nlat-1) / dy(nlat) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat) = var1(i,nlat) - var5(i,nlat) * dy(nlat-1) / dy(nlat) &
        & / (dy(nlat) + dy(nlat-1))
    ELSE
!      var5(i,1) = ( (var1(i,1) - var1(i+nlon/2,1)) * dy(1) / dy(nlat) &
!        & + (var1(i,2) - var1(i,1)) * dy(nlat) / dy(1) ) &
!        & / (dy(1) + dy(nlat))
      var1(i,1) = var1(i,1) + var5(i,1) * dy(1) / dy(nlat) &
        & / (dy(1) + dy(nlat))
      var1(i+nlon/2,1) = var1(i+nlon/2,1) - var5(i,1) * dy(1) / dy(nlat) &
        & / (dy(1) + dy(nlat))
      var1(i,2) = var1(i,2) + var5(i,1) * dy(nlat) / dy(1) &
        & / (dy(1) + dy(nlat))
      var1(i,1) = var1(i,1) - var5(i,1) * dy(nlat) / dy(1) &
        & / (dy(1) + dy(nlat))
!      var5(i,nlat) = ( (var1(i,nlat) - var1(i,nlat-1)) * dy(nlat) / dy(nlat-1)&
!        & + (var1(i+nlon/2,nlat) - var1(i,nlat)) * dy(nlat-1) / dy(nlat) ) &
!        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat) = var1(i,nlat) + var5(i,nlat) * dy(nlat) / dy(nlat-1) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat-1) = var1(i,nlat-1) - var5(i,nlat) * dy(nlat) / dy(nlat-1) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i+nlon/2,nlat) = var1(i+nlon/2,nlat) + var5(i,nlat) * dy(nlat-1) / dy(nlat) &
        & / (dy(nlat) + dy(nlat-1))
      var1(i,nlat) = var1(i,nlat) - var5(i,nlat) * dy(nlat-1) / dy(nlat) &
        & / (dy(nlat) + dy(nlat-1))
    END IF
!    DO j=2,nlat-1
    DO j=nlat-1,2,-1
!      var5(i,j) = ( (var1(i,j) - var1(i,j-1)) * dy(j) / dy(j-1) &
!        & + (var1(i,j+1) - var1(i,j)) * dy(j-1) / dy(j) ) &
!        & / (dy(j) + dy(j-1))
      var1(i,j) = var1(i,j) + var5(i,j) * dy(j) / dy(j-1) &
        & / (dy(j) + dy(j-1))
      var1(i,j-1) = var1(i,j-1) - var5(i,j) * dy(j) / dy(j-1) &
        & / (dy(j) + dy(j-1))
      var1(i,j+1) = var1(i,j+1) + var5(i,j) * dy(j-1) / dy(j) &
        & / (dy(j) + dy(j-1))
      var1(i,j) = var1(i,j) - var5(i,j) * dy(j-1) / dy(j) &
        & / (dy(j) + dy(j-1))
    END DO

  END DO

  RETURN
END SUBROUTINE AD_ddy
!-----------------------------------------------------------------------
! [6] Recursive Filter
!-----------------------------------------------------------------------
SUBROUTINE rfilter_fwd(var,alphax,alphay,sfactx,sfacty,var5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: var(nlon,nlat)
  REAL(r_size),INTENT(IN) :: alphax(4,nlat)
  REAL(r_size),INTENT(IN) :: alphay(4)
  REAL(r_size),INTENT(IN) :: sfactx(nlat)
  REAL(r_size),INTENT(IN) :: sfacty
  REAL(r_size),INTENT(OUT) :: var5(nlon,nlat)
  REAL(r_size) :: vartmp(nlon,nlat)
  INTEGER :: j

  CALL rfilter_x_fwd(alphax,var,vartmp)
  DO j=1,nlat
    vartmp(:,j) = vartmp(:,j) * sqrt(sfactx(j))
  END DO
  CALL rfilter_y_fwd(alphay,vartmp,var5)
  var5 = var5 * sqrt(sfacty)

  RETURN
END SUBROUTINE rfilter_fwd

SUBROUTINE rfilter_bwd(var,alphax,alphay,sfactx,sfacty,var5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: var(nlon,nlat)
  REAL(r_size),INTENT(IN) :: alphax(4,nlat)
  REAL(r_size),INTENT(IN) :: alphay(4)
  REAL(r_size),INTENT(IN) :: sfactx(nlat)
  REAL(r_size),INTENT(IN) :: sfacty
  REAL(r_size),INTENT(OUT) :: var5(nlon,nlat)
  REAL(r_size) :: vartmp(nlon,nlat)
  INTEGER :: j

  vartmp = var * sqrt(sfacty)
  CALL rfilter_y_bwd(alphay,vartmp,var5)
  DO j=1,nlat
    vartmp(:,j) = var5(:,j) * sqrt(sfactx(j))
  END DO
  CALL rfilter_x_bwd(alphax,vartmp,var5)

  RETURN
END SUBROUTINE rfilter_bwd
!-----------------------------------------------------------------------
! [7] Core part of Recursive Filter
!-----------------------------------------------------------------------
!--[7.0] Set coefficient -----------------------------------------------
SUBROUTINE set_rfilter(xi,alpha,sfact)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: xi
  REAL(r_size),INTENT(OUT) :: alpha(4)
  REAL(r_size),INTENT(OUT) :: sfact
  REAL(r_size) :: b,c,d,e,p,q,r,b2,c2,d2,p2,q2
  COMPLEX(r_size) :: t1,t2,t3,u,v,w
  COMPLEX(r_size) :: k(4),z(4),zp(4)

  COMPLEX(r_size) :: cons_one
  COMPLEX(r_size) :: kk(4),pp2, qq2
  INTEGER :: i

  cons_one=-1.0d0

  sfact = sqrt(2.0d0*pi) * xi * .8d0

  b = xi**2/2.0d0
  c = xi**2/24.0d0 + xi**4/8.0d0
  d = xi**2/180.0d0 + xi**4/48.0d0 + xi**6/48.0d0
  e = xi**2/1120.0d0 + xi**4*7/1920.0d0 + xi**6/192.0d0 + xi**8/384.0d0

  p = c - b**2*3.0d0/8.0d0
  q = d - b*c/2.0d0 + b**3/8.0d0
  r = e - b*d/4.0d0 + b**2*c/16.0d0 - b**4*3.0d0/256.0d0

  b2 = p/2.0d0
  c2 = p**2/16.0d0 - r/4.0d0
  d2 = -q**2/64.0d0

  p2 = c2 - b2**2/3.0d0
  q2 = d2 - b2*c2/3.0d0 + b2**3*2.0d0/27.0d0
  !t1 = 0.5d0 * ( -q2 + sqrt(q2**2 + 4.0d0*p2**3/27.0d0) )
  !t2 = 0.5d0 * ( -q2 - sqrt(q2**2 + 4.0d0*p2**3/27.0d0) )

 ! jjliu
  pp2 = q2**2 +4.0d0*p2**3/27.0d0
  t1 = 0.5d0 * ( -q2 + sqrt(pp2) )
  t2 = 0.5d0 * ( -q2 - sqrt(pp2) )

  u = t1**(1.0d0/3.0d0)
  v = t2**(1.0d0/3.0d0)
  w = exp(2.0d0*pi*sqrt(cons_one)/3.0d0)

  t1 = u + v - b2/3.0d0
  t2 = u*w + v*w**2 - b2/3.0d0
  t3 = u*w**2 + v*w - b2/3.0d0

  u = sqrt(t1)
  v = sqrt(t2)
  w = sqrt(t3)

  w = w * real(-u*v*w)*8/q

  k(1) =   u + v + w - b/4.0d0
  k(2) =   u - v - w - b/4.0d0
  k(3) = - u + v - w - b/4.0d0
  k(4) = - u - v + w - b/4.0d0

  k(:) = 1.0d0/k(:)
  !z(:) = 1.0d0 - k(:)/2.0d0 - sqrt((1.0d0-k(:)/2.0d0)**2 - 1.0d0)
  !zp(:) = 1.0d0 - k(:)/2.0d0 + sqrt((1.0d0-k(:)/2.0d0)**2 - 1.0d0)
 !jjliu
  kk(:) = (1.0d0-k(:)/2.0d0)**2-1.0d0
  z(:) = 1.0d0 - k(:)/2.0d0 - sqrt(kk(:))
  zp(:) = 1.0d0 - k(:)/2.0d0 + sqrt(kk(:))

  DO i=1,4
    IF( abs(z(i)) > abs(zp(i)) ) z(i)=zp(i)
  END DO

  alpha(1) = REAL(z(1)+z(2)+z(3)+z(4), r_size)
  alpha(2) = -REAL(z(1)*z(2)+z(1)*z(3)+z(1)*z(4)+z(2)*z(3)+z(2)*z(4)+z(3)*z(4), r_size)
  alpha(3) = REAL(z(1)*z(2)*z(3)+z(2)*z(3)*z(4)+z(3)*z(4)*z(1)+z(4)*z(1)*z(2), r_size)
  alpha(4) = -REAL(z(1)*z(2)*z(3)*z(4), r_size)

  RETURN
END SUBROUTINE set_rfilter
!--[7.1] cyclic boundary -----------------------------------------------
SUBROUTINE rfilter_1d_cyc_fwd(ndim,a,za,zb)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: a(4)
  REAL(r_size),INTENT(IN) :: za(ndim)
  REAL(r_size),INTENT(OUT) :: zb(ndim)
  REAL(r_size) :: b
  INTEGER :: i

  b = 1.0d0 - SUM(a)

  zb(:) = 0.0d0
  DO i=5,ndim
    zb(i) = b*za(i)+a(1)*zb(i-1)+a(2)*zb(i-2)+a(3)*zb(i-3)+a(4)*zb(i-4)
  END DO
  zb(1) = b*za(1)+a(1)*zb(ndim)+a(2)*zb(ndim-1)+a(3)*zb(ndim-2)+a(4)*zb(ndim-3)
  zb(2) = b*za(2)+a(1)*zb(1)+a(2)*zb(ndim)+a(3)*zb(ndim-1)+a(4)*zb(ndim-2)
  zb(3) = b*za(3)+a(1)*zb(2)+a(2)*zb(1)+a(3)*zb(ndim)+a(4)*zb(ndim-1)
  zb(4) = b*za(4)+a(1)*zb(3)+a(2)*zb(2)+a(3)*zb(1)+a(4)*zb(ndim)
  DO i=5,ndim
    zb(i) = b*za(i)+a(1)*zb(i-1)+a(2)*zb(i-2)+a(3)*zb(i-3)+a(4)*zb(i-4)
  END DO

  RETURN
END SUBROUTINE rfilter_1d_cyc_fwd

SUBROUTINE rfilter_1d_cyc_bwd(ndim,a,zb,zc)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: a(4)
  REAL(r_size),INTENT(IN) :: zb(ndim)
  REAL(r_size),INTENT(OUT) :: zc(ndim)
  REAL(r_size) :: b
  INTEGER :: i

  b = 1.0d0 - SUM(a)

  zc(:) = 0.0d0
  DO i=ndim-4,1,-1
    zc(i)=b*zb(i)+a(1)*zc(i+1)+a(2)*zc(i+2)+a(3)*zc(i+3)+a(4)*zc(i+4)
  END DO
  zc(ndim) = b*zb(ndim)+a(1)*zc(1)+a(2)*zc(2)+a(3)*zc(3)+a(4)*zc(4)
  zc(ndim-1) = b*zb(ndim-1)+a(1)*zc(ndim)+a(2)*zc(1)+a(3)*zc(2)+a(4)*zc(3)
  zc(ndim-2) = b*zb(ndim-2)+a(1)*zc(ndim-1)+a(2)*zc(ndim)+a(3)*zc(1)+a(4)*zc(2)
  zc(ndim-3) = b*zb(ndim-3)+a(1)*zc(ndim-2)+a(2)*zc(ndim-1)+a(3)*zc(ndim)+a(4)*zc(1)
  DO i=ndim-4,1,-1
    zc(i)=b*zb(i)+a(1)*zc(i+1)+a(2)*zc(i+2)+a(3)*zc(i+3)+a(4)*zc(i+4)
  END DO

  RETURN
END SUBROUTINE rfilter_1d_cyc_bwd
!--[7.2] rigid boundary ------------------------------------------------
SUBROUTINE rfilter_1d_rgd_fwd(ndim,a,za,zb)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: a(4)
  REAL(r_size),INTENT(IN) :: za(ndim)
  REAL(r_size),INTENT(OUT) :: zb(ndim)
  REAL(r_size) :: b
  INTEGER :: i

  b = 1.0d0 - SUM(a)

  zb(:) = 0.0d0
  DO i=5,ndim
    zb(i) = b*za(i)+a(1)*zb(i-1)+a(2)*zb(i-2)+a(3)*zb(i-3)+a(4)*zb(i-4)
  END DO
  zb(1) = b*za(1)!+a(1)*zbm(1)+a(2)*zbm(2)+a(3)*zbm(3)+a(4)*zbm(4)
  zb(2) = b*za(2)+a(1)*zb(1)!+a(2)*zbm(1)+a(3)*zbm(2)+a(4)*zbm(3)
  zb(3) = b*za(3)+a(1)*zb(2)+a(2)*zb(1)!+a(3)*zbm(1)+a(4)*zbm(2)
  zb(4) = b*za(4)+a(1)*zb(3)+a(2)*zb(2)+a(3)*zb(1)!+a(4)*zbm(1)
  DO i=5,ndim
    zb(i) = b*za(i)+a(1)*zb(i-1)+a(2)*zb(i-2)+a(3)*zb(i-3)+a(4)*zb(i-4)
  END DO

  RETURN
END SUBROUTINE rfilter_1d_rgd_fwd

SUBROUTINE rfilter_1d_rgd_bwd(ndim,a,zb,zc)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndim
  REAL(r_size),INTENT(IN) :: a(4)
  REAL(r_size),INTENT(IN) :: zb(ndim)
  REAL(r_size),INTENT(OUT) :: zc(ndim)
  REAL(r_size) :: b
  INTEGER :: i

  b = 1.0d0 - SUM(a)

  zc(:) = 0.0d0
  DO i=ndim-4,1,-1
    zc(i)=b*zb(i)+a(1)*zc(i+1)+a(2)*zc(i+2)+a(3)*zc(i+3)+a(4)*zc(i+4)
  END DO
  zc(ndim) = b*zb(ndim)!+a(1)*zc(1)+a(2)*zc(2)+a(3)*zc(3)+a(4)*zc(4)
  zc(ndim-1) = b*zb(ndim-1)+a(1)*zc(ndim)!+a(2)*zc(1)+a(3)*zc(2)+a(4)*zc(3)
  zc(ndim-2) = b*zb(ndim-2)+a(1)*zc(ndim-1)+a(2)*zc(ndim)!+a(3)*zc(1)+a(4)*zc(2)
  zc(ndim-3) = b*zb(ndim-3)+a(1)*zc(ndim-2)+a(2)*zc(ndim-1)+a(3)*zc(ndim)!+a(4)*zc(1)
  DO i=ndim-4,1,-1
    zc(i)=b*zb(i)+a(1)*zc(i+1)+a(2)*zc(i+2)+a(3)*zc(i+3)+a(4)*zc(i+4)
  END DO

  RETURN
END SUBROUTINE rfilter_1d_rgd_bwd
!--[7.3] x-direction ---------------------------------------------------
SUBROUTINE rfilter_x_fwd(alpha,za,zb)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: alpha(4,nlat)
  REAL(r_size),INTENT(IN) :: za(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: zb(nlon,nlat)
  INTEGER :: j

  DO j=1,nlat
    CALL rfilter_1d_cyc_fwd(nlon,alpha(:,j),za(:,j),zb(:,j))
  END DO

  RETURN
END SUBROUTINE rfilter_x_fwd

SUBROUTINE rfilter_x_bwd(alpha,zb,zc)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: alpha(4,nlat)
  REAL(r_size),INTENT(INOUT) :: zb(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: zc(nlon,nlat)
  INTEGER :: j

  DO j=1,nlat
    CALL rfilter_1d_cyc_bwd(nlon,alpha(:,j),zb(:,j),zc(:,j))
  END DO

  RETURN
END SUBROUTINE rfilter_x_bwd
!--[7.4] y-direction ---------------------------------------------------
SUBROUTINE rfilter_y_fwd(alpha,za,zb)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: alpha(4)
  REAL(r_size),INTENT(IN) :: za(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: zb(nlon,nlat)
  INTEGER :: i

  DO i=1,nlon
    CALL rfilter_1d_rgd_fwd(nlat,alpha,za(i,:),zb(i,:))
  END DO

  RETURN
END SUBROUTINE rfilter_y_fwd

SUBROUTINE rfilter_y_bwd(alpha,zb,zc)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: alpha(4)
  REAL(r_size),INTENT(IN) :: zb(nlon,nlat)
  REAL(r_size),INTENT(OUT) :: zc(nlon,nlat)
  INTEGER :: i

  DO i=1,nlon
    CALL rfilter_1d_rgd_bwd(nlat,alpha,zb(i,:),zc(i,:))
  END DO

  RETURN
END SUBROUTINE rfilter_y_bwd

END MODULE tdvar_tools
