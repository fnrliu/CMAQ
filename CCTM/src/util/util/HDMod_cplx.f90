!*************************************************************************
!* Hyperdual Number Module (HDMod_cplx) of Fortran Codes 
!*----------------------------------------------------------------

Module HDMod_cplx

use HDMod
implicit none

public

    integer, parameter                   :: SP_cplx = KIND(1.0)      ! Single Precision
    integer, parameter                   :: DP_cplx = KIND(1.d0)     ! Double Precision
    integer, parameter                   :: PR_cplx = DP_cplx             ! Set the precision
    real(PR_cplx), parameter, private    :: pi = ACOS(-1.0_PR)  
    complex(PR_cplx), parameter, private :: pi_c = (pi, 0.0_PR)

    !--- DEFINITION OF HYPERDUAL TYPE
    TYPE hyperdual_cplx
        complex(PR) :: x
        complex(PR) :: dx1
        complex(PR) :: dx2
        complex(PR) :: dx1x2
    END TYPE

    !================================================================!
    !                Overloading hdual functions                     !
    !================================================================!
      
    !----- Constructor
    interface hdual_cplx
      module procedure hdual_cplx_from_dble
    end interface

    interface assignment(=) 
      module procedure hdual_cplx_assign_hdual_cplx
    end interface

    interface CMPLX
      module procedure hdual_cplx_from_hdual, hdual_cplx_tens_from_hdual_tens
    end interface


    CONTAINS


    function hdual_cplx_from_dble(x11,x12,x21,x22) result(q)
          
      implicit none
      complex(PR_cplx), intent(in)  :: x11, x12, x21, x22
      TYPE(hyperdual_cplx)       :: q
      
      q%x = x11
      q%dx1 = x12
      q%dx2 = x21
      q%dx1x2 = x22
            
    end function hdual_cplx_from_dble


    subroutine hdual_cplx_assign_hdual_cplx(qcplx_left, qcplx_right)

      implicit none
      TYPE(hyperdual_cplx), intent(out)  :: qcplx_left
      TYPE(hyperdual_cplx), intent(in)    :: qcplx_right

      qcplx_left%x = qcplx_right%x
      qcplx_left%dx1 = qcplx_right%dx1
      qcplx_left%dx2 = qcplx_right%dx2
      qcplx_left%dx1x2 = qcplx_right%dx1x2

    end subroutine hdual_cplx_assign_hdual_cplx
		

    function hdual_cplx_from_hdual(qreal, qimag) result(qcplx)

      ! Complexify hyperdual numbers using CMPLX

      implicit none
      TYPE(hyperdual), intent(in) :: qreal
      TYPE(hyperdual), intent(in) :: qimag
      TYPE(hyperdual_cplx)        :: qcplx

      qcplx%x = CMPLX(qreal%x, qimag%x, KIND=PR_cplx) 
      qcplx%dx1 = CMPLX(qreal%dx1, qimag%dx1, KIND=PR_cplx)
      qcplx%dx2 = CMPLX(qreal%dx2, qimag%dx2, KIND=PR_cplx)
      qcplx%dx1x2 = CMPLX(qreal%dx1x2, qimag%dx1x2, KIND=PR_cplx)

    end function hdual_cplx_from_hdual


    function hdual_cplx_tens_from_hdual_tens(qreal, qimag) result(qcplx)

      ! Complexify hyperdual numbers using CMPLX

      implicit none
      TYPE(hyperdual), dimension(:,:,:), intent(in) :: qreal
      TYPE(hyperdual), dimension(:,:,:), intent(in) :: qimag
      TYPE(hyperdual_cplx), dimension(size(qreal, 1), size(qreal, 2), size(qreal, 3)) :: qcplx

      qcplx%x = CMPLX(qreal%x, qimag%x, KIND=PR_cplx) 
      qcplx%dx1 = CMPLX(qreal%dx1, qimag%dx1, KIND=PR_cplx)
      qcplx%dx2 = CMPLX(qreal%dx2, qimag%dx2, KIND=PR_cplx)
      qcplx%dx1x2 = CMPLX(qreal%dx1x2, qimag%dx1x2, KIND=PR_cplx)

    end function hdual_cplx_tens_from_hdual_tens

end module HDMod_cplx