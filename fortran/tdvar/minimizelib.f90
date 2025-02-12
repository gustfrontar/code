! =============================================================================
!
!  MINIMIZE: Minimizer for JNoVA
!            module to use lbfgs.f (Original is includeed in MSM4DVAR)
!
! =============================================================================
module minimizelib
!
! -----------------------------------------------------------------------------
!
!  HISTORY:
!  2002. 7.31 Y.Honda / Modify for JNoVA
!
! -----------------------------------------------------------------------------
!
!  COMMENT: Require 'lbfgs.f'
!
! -----------------------------------------------------------------------------
!   Declaration of Variables
! -----------------------------------------------------------------------------
!
!  USE vardef
  USE common
  implicit none

  ! ====================================================================
  !   >>> Variables required lbfgs.f(LBFGS Minimization)
  ! ====================================================================
  integer(4),          parameter :: nsave = 5
  real(kind = r_dble), parameter :: epsln = 1.0d-4

  integer(4),          save, private              :: iter
  integer(4),          save, private              :: iprint(2)
  integer(4),          save, private              :: point
  logical,             save, private              :: diagco
  real(kind = r_dble), save, private, allocatable :: zdiag(:)
  real(kind = r_dble), save, private, allocatable :: zs(:), zy(:), zw(:)

contains
! =============================================================================
! 
!   INITIALIZE_MINIMIZER: Initialize Minimizer parameters
!
! =============================================================================
  subroutine Initialize_Minimizer(vsiz)
!
! -----------------------------------------------------------------------------
!
!   HISTORY: 2002. 7.31 Y.Honda - First Code
!
! -----------------------------------------------------------------------------
    implicit none

    integer(4), intent(in) :: vsiz

    iter = 0

    iprint(1) = 1; iprint(2) = 0
    diagco = .false.

    allocate( zdiag(1:vsiz) )
    allocate( zs(1:vsiz * nsave) )
    allocate( zy(1:vsiz * nsave) )
    !    allocate( zw(1:vsiz * nsave) )
    allocate( zw(1:vsiz + 2 * nsave) )

    zdiag = 0.0d0
    zs    = 0.0d0
    zy    = 0.0d0
    zw    = 0.0d0

    return
  end subroutine Initialize_Minimizer

! =============================================================================
! 
!   MINIMIZE: Minimize by VA15AD (LBFGS)
!
! =============================================================================
  subroutine Minimize(vsiz, xctl, costf, costg, maxiter, iflag)
!
! -----------------------------------------------------------------------------
!
!   HISTORY: 2002. 7.31 Y.Honda - First Code
!
! -----------------------------------------------------------------------------
!
!   COMMENTS:
!      iflag >= 0 : normal termination
!            == - maxiter : reach maximum iteration
!            <  0 : abnormal termination except number of iteration reaches 
!                   maximum number
!
! -----------------------------------------------------------------------------
!

    implicit none
    integer(4),          intent(in)    :: vsiz
    integer(4),          intent(in)    :: maxiter
    integer(4),          intent(out)   :: iflag
    real(kind = r_size), intent(inout) :: xctl(vsiz)
    real(kind = r_size), intent(in)    :: costf
    real(kind = r_size), intent(in)    :: costg(vsiz)

    call va15ad(vsiz, nsave, xctl, costf, costg, diagco, zdiag, iprint,        &
      &         epsln, zs, zy, point, zw, iflag, iter)

    if(iter > maxiter) then
      iflag = - maxiter
    end if

    return
  end subroutine Minimize

! =============================================================================
! 
!   TERMINATE_MINIMIZER: Terminate Minimizer
!
! =============================================================================
  subroutine Terminate_Minimizer
!
! -----------------------------------------------------------------------------
!
!   HISTORY: 2002. 8. 1 Y.Honda - First Code
!
! -----------------------------------------------------------------------------
    implicit none

    deallocate( zdiag )
    deallocate( zs )
    deallocate( zy )
    deallocate( zw )

    return
  end subroutine Terminate_Minimizer

end module minimizelib
