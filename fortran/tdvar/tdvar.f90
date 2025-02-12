PROGRAM tdvar
!=======================================================================
!
! [PURPOSE:] Main program of 3DVAR for the SPEEDY model
!
! [HISTORY:]
!   10/21/2004 Takemasa Miyoshi  created
!
!=======================================================================
  USE common
  USE common_speedy
  USE tdvar_tools
  USE minimizelib

  IMPLICIT NONE
  LOGICAL,PARAMETER :: msw_test=.TRUE.
  INTEGER,PARAMETER :: maxiter = 100
  REAL(r_size) :: u(nlon,nlat,nlev)
  REAL(r_size) :: v(nlon,nlat,nlev)
  REAL(r_size) :: t(nlon,nlat,nlev)
  REAL(r_size) :: q(nlon,nlat,nlev)
  REAL(r_size) :: ps(nlon,nlat)
  REAL(r_size) :: ue(nlon,nlat,nlev)
  REAL(r_size) :: ve(nlon,nlat,nlev)
  REAL(r_size) :: te(nlon,nlat,nlev)
  REAL(r_size) :: qe(nlon,nlat,nlev)
  REAL(r_size) :: pse(nlon,nlat)
  REAL(r_size) :: ctrl(ngpv)
  REAL(r_size) :: gues(ngpv)
  REAL(r_size) :: costb,costy,cost
  REAL(r_size),ALLOCATABLE :: gradcost(:)
  REAL(r_size),ALLOCATABLE :: obs(:)
  REAL(r_size),ALLOCATABLE :: dep(:)
  INTEGER :: n,iflag

  IF (msw_test) PRINT *,'TEST MODE (tdvar)'

  CALL set_common_speedy

  CALL set_tdvar_tools

  ALLOCATE( obs(ngpv) )

  CALL open_grd(2,'obs.grd')
  CALL read_grd(2,u,v,t,q,ps)
  !Esta funcion transforma los diferentes campos en un vector.
  CALL reshape_grd(u,v,t,q,ps,obs)
  CALL close_grd(2)

  CALL open_grd(2,'gues.grd')
  CALL read_grd(2,u,v,t,q,ps)
  !Esta funcion transforma los diferentes campos en un vector.
  CALL reshape_grd(u,v,t,q,ps,gues)
  CALL close_grd(2)


!!! For test
  IF (msw_test) THEN
    u = 1.0d0 
    v = 1.0d0 
    t = 1.0d0 
    q = 1.0d-4 
    ps = 1.0d2 
  ELSE
    obs = obs - gues
    CALL reshape_gpv(obs,u,v,t,q,ps)

  END IF
 DEALLOCATE( obs )


  ALLOCATE( dep(nobs) )
!Esta subrutina es el operador H que transforma el Xb en el Yb
!dep es el valor de las variables observadas en los puntos observados a partir de la 
!estimacion del modelo. u,v,t,q,ps es el vector estado del modelo para este caso.

  CALL Trans_XtoY(u,v,t,q,ps,dep)

  ALLOCATE( gradcost(ngpv) )

  ctrl = 0.0d0

  CALL Initialize_Minimizer(ngpv)
  PRINT '(A)',&
&'========================================================================='
  PRINT '(A)',&
&'|  N  | BG COST |OBS COST |  COST   | MAX(|GRAD|) | MAX(|CTRL|) | IFLAG |'
  PRINT '(A)',&
&'-------------------------------------------------------------------------'
!Este do es la iteracion para minimizar la funcion de costo.
!gues es el vector estado del modelo Xb
!ctrl es la solucion al problema de minimizacion que se inicializa en 0.
!dep  es el Xb en el espacio de las observaciones.
!costb es la componente de la funcion costo que tiene que ver con Xb
!costy es la componente de la funcion costo que tiene que ver con las observaciones
!gradcost es el gradiente de la funcion de costo.
  DO n=1,2*maxiter

    CALL cost_func(gues,ctrl,dep,costb,costy,gradcost)

    cost = costb + costy

    CALL Minimize(ngpv,ctrl,cost,gradcost,maxiter,iflag)

    PRINT '(I6,3ES10.2,2ES14.2,I6)',n,costb,costy,cost,MAXVAL(ABS(gradcost)),MAXVAL(ABS(ctrl)),iflag

    IF( iflag <= 0 ) EXIT

  END DO

  PRINT '(A)',&
&'========================================================================='

  CALL Trans_VtoX_gpv(gues,ctrl,ctrl)

!!! For test
!  IF (msw_test) THEN
!    CALL reshape_gpv(ctrl,u,v,t,q,ps)
!    print *,'U  ',maxval(u)
!    print *,'V  ',maxval(v)
!    print *,'T  ',maxval(t)
!    print *,'Q  ',maxval(q)
!    print *,'PS ',maxval(ps)
!  END IF

  ctrl = gues + ctrl

  CALL reshape_gpv(ctrl,u,v,t,q,ps)

  CALL open_grd(2,'anal.grd')
  CALL write_grd(2,u,v,t,q,ps)
  CALL close_grd(2)

  STOP
END PROGRAM tdvar
