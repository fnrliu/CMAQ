!*************************************************************************
!* Hyperdual Number Module (HDMod) of Fortran Codes 
!*----------------------------------------------------------------

Module HDMod

implicit none

public

    integer, parameter              :: SP = KIND(1.0)      ! Single Precision
    integer, parameter              :: DP = KIND(1.d0)     ! Double Precision
    ! integer, parameter              :: QP = KIND(1.q0)     ! Quadruple Precision
    integer, parameter              :: PR = DP             ! Set the precision
    real(PR), parameter, private    :: pi = ACOS(-1.0_PR)  
    complex(PR), parameter, private :: pi_c = (pi, 0.0_PR)

    !--- DEFINITION OF HYPERDUAL TYPE
    TYPE hyperdual
        real(PR) :: x
        real(PR) :: dx1
        real(PR) :: dx2
        real(PR) :: dx1x2
    END TYPE

    !================================================================!
    !                Overloading hdual functions                     !
    !================================================================!
      
    !----- Constructor
    interface hdual
      module procedure hdual_from_dble, hdual_from_one_dble, hdual_from_one_sngl
    end interface

    !----- Assignment
    interface assignment(=)
      module procedure hdual_assign_hdual, hdual_assign_dble, hdual_assign_int, & 
        hdual_array_assign_hdual, hdual_array_assign_hdual_array, hdual_array_assign_dble, & 
        hdual_array_assign_dble_array, hdual_array_assign_int, hdual_array_assign_int_array, &
        hdual_matrix_assign_hdual, hdual_matrix_assign_hdual_matrix, hdual_matrix_assign_dble, &
        hdual_matrix_assign_dble_matrix, hdual_matrix_assign_int, hdual_matrix_assign_int_matrix, & 
        hdual_tens_assign_hdual, hdual_tens_assign_hdual_tens, hdual_tens_assign_dble, & 
        hdual_tens_assign_dble_tens, hdual_tens_assign_int, hdual_tens_assign_int_tens
    end interface 
  
    !----- Comparison operators      
    interface operator(==)
      module procedure hdual_eq_hdual, hdual_eq_dble, dble_eq_hdual
    end interface

    interface operator(/=)
      module procedure hdual_ne_hdual, hdual_ne_dble, dble_ne_hdual
    end interface

    interface operator(>)
      module procedure hdual_gt_hdual, hdual_gt_dble, dble_gt_hdual
    end interface
  
    interface operator(>=)
      module procedure hdual_ge_hdual, hdual_ge_dble, dble_ge_hdual
    end interface
  
    interface operator(<)
      module procedure hdual_lt_hdual, hdual_lt_dble, dble_lt_hdual
    end interface
  
    interface operator(<=)
      module procedure hdual_le_hdual, hdual_le_dble, dble_le_hdual
    end interface


    interface operator (+)
        module procedure hdual_plus_hdual, hdual_plus_hdual_array, hdual_plus_hdual_matrix, &
              hdual_plus_hdual_tens, hdual_plus_dble, hdual_plus_dble_array, hdual_plus_dble_matrix, &
              hdual_plus_dble_tens, hdual_plus_int, hdual_plus_int_array, &
              hdual_plus_int_matrix, hdual_plus_int_tens, &
            hdual_array_plus_hdual, hdual_array_plus_hdual_array, hdual_array_plus_dble, &
              hdual_array_plus_dble_array, hdual_array_plus_int, &
              hdual_array_plus_int_array, &
            hdual_matrix_plus_hdual, hdual_matrix_plus_hdual_matrix, hdual_matrix_plus_dble, &
              hdual_matrix_plus_dble_matrix, hdual_matrix_plus_int, &
              hdual_matrix_plus_int_matrix, &
            hdual_tens_plus_hdual, hdual_tens_plus_hdual_tens, hdual_tens_plus_dble, &
              hdual_tens_plus_dble_tens, hdual_tens_plus_int, &
              hdual_tens_plus_int_tens, &
            dble_plus_hdual, dble_plus_hdual_array, dble_plus_hdual_matrix, dble_plus_hdual_tens, &
            dble_array_plus_hdual, dble_array_plus_hdual_array, &
            dble_matrix_plus_hdual, dble_matrix_plus_hdual_matrix, &
            dble_tens_plus_hdual, dble_tens_plus_hdual_tens, &
            int_plus_hdual, int_plus_hdual_array, int_plus_hdual_matrix, &
              int_plus_hdual_tens, &
            int_array_plus_hdual, int_array_plus_hdual_array, &
            int_matrix_plus_hdual, int_matrix_plus_hdual_matrix, &
            int_tens_plus_hdual, int_tens_plus_hdual_tens
      end interface

    interface operator(-)
      module procedure hdual_minus_hdual, hdual_minus_hdual_array, hdual_minus_hdual_matrix, &
          hdual_minus_hdual_tens, hdual_minus_dble, hdual_minus_dble_array, hdual_minus_dble_matrix,&
          hdual_minus_dble_tens, hdual_minus_int, hdual_minus_int_array, hdual_minus_int_matrix, &
          hdual_minus_int_tens, &
         hdual_array_minus_hdual, hdual_array_minus_hdual_array, &
           hdual_array_minus_dble, hdual_array_minus_dble_array, &
           hdual_array_minus_int, hdual_array_minus_int_array, &
         hdual_matrix_minus_hdual, hdual_matrix_minus_hdual_matrix, &
           hdual_matrix_minus_dble, hdual_matrix_minus_dble_matrix, &
           hdual_matrix_minus_int, hdual_matrix_minus_int_matrix, &
         hdual_tens_minus_hdual, hdual_tens_minus_hdual_tens, &
           hdual_tens_minus_dble, hdual_tens_minus_dble_tens, &
           hdual_tens_minus_int, hdual_tens_minus_int_tens, &
         dble_minus_hdual, dble_minus_hdual_array, &
           dble_minus_hdual_matrix, dble_minus_hdual_tens, &
         dble_array_minus_hdual, dble_array_minus_hdual_array, &
         dble_matrix_minus_hdual, dble_matrix_minus_hdual_matrix, &
         dble_tens_minus_hdual, dble_tens_minus_hdual_tens, &
         int_minus_hdual, int_minus_hdual_array, &
           int_minus_hdual_matrix, int_minus_hdual_tens, &
         int_array_minus_hdual, int_array_minus_hdual_array, &
         int_matrix_minus_hdual, int_matrix_minus_hdual_matrix, &
         int_tens_minus_hdual, int_tens_minus_hdual_tens, minus_hdual, &
         minus_hdual_array, minus_hdual_matrix, minus_hdual_tens
     end interface


    
    interface operator(*)
        module procedure hdual_mul_hdual, hdual_mul_hdual_array, hdual_mul_hdual_matrix, &
              hdual_mul_hdual_tens, hdual_mul_dble, hdual_mul_dble_array, hdual_mul_dble_matrix, &
              hdual_mul_dble_tens, hdual_mul_int, hdual_mul_int_array, hdual_mul_int_matrix, &
              hdual_mul_int_tens, & 
            hdual_array_mul_hdual, hdual_array_mul_dble, hdual_array_mul_int, &
            hdual_matrix_mul_hdual, hdual_matrix_mul_dble, hdual_matrix_mul_int, &
            hdual_tens_mul_hdual, hdual_tens_mul_dble, hdual_tens_mul_int, &
            dble_mul_hdual, dble_mul_hdual_array, dble_mul_hdual_matrix, &
              dble_mul_hdual_tens, &
            dble_array_mul_hdual, &
            dble_matrix_mul_hdual, &
            dble_tens_mul_hdual, &
            int_mul_hdual, int_mul_hdual_array, int_mul_hdual_matrix, &
              int_mul_hdual_tens, &
            int_array_mul_hdual, &
            int_matrix_mul_hdual, &
            int_tens_mul_hdual
      end interface


      interface operator(/)
        module procedure hdual_div_hdual, hdual_div_dble, hdual_div_int, &
            hdual_array_div_hdual, hdual_array_div_dble, hdual_array_div_int, &
            hdual_matrix_div_hdual, hdual_matrix_div_dble, hdual_matrix_div_int, &
            hdual_tens_div_hdual, hdual_tens_div_dble, hdual_tens_div_int, &
            dble_div_hdual, int_div_hdual, &
            dble_array_div_hdual, int_array_div_hdual, &
            dble_matrix_div_hdual, int_matrix_div_hdual, &
            dble_tens_div_hdual, int_tens_div_hdual
      end interface

      interface operator(**)
        module procedure hdual_pow_hdual, hdual_pow_int, hdual_pow_dble
      end interface



      !----- Intrinsic functions
      interface dble
        module procedure dble_hdual, dble_hdual_array, dble_hdual_matrix
      end interface
      
      interface abs
        module procedure abs_hdual, abs_hdual_array, abs_hdual_matrix
      end interface
      
      interface sign
        module procedure sign_hdual_hdual
      end interface

      interface max
        module procedure max_hdual_hdual, max_hdual_dble, max_dble_hdual
      end interface

      interface min
        module procedure min_hdual_hdual, min_hdual_dble, min_dble_hdual
      end interface

      interface maxval
        module procedure maxval_hdual_array
      end interface

      interface dot_product
        module procedure &
          dot_product_hdual_array_hdual_array, dot_product_hdual_array_dble_array, &
          dot_product_dble_array_hdual_array
      end interface

      interface sqrt
        module procedure hdsqrt
      end interface

      interface exp
        module procedure hdexp
      end interface

      interface log
        module procedure hdlog
      end interface
      
      interface cos
        module procedure hdcos
      end interface
      
      interface sin
        module procedure hdsin
      end interface

      interface tan
        module procedure hdtan
      end interface
    
      interface cosh
        module procedure hdcosh
      end interface
      
      interface sinh
        module procedure hdsinh
      end interface

      interface tanh
        module procedure hdtanh
      end interface

      interface acos
        module procedure hdacos
      end interface

      interface asin
        module procedure hdasin
      end interface

      interface atan
        module procedure hdatan
      end interface


    CONTAINS

    !================================================================!
    !                Overloading hdual functions                     !
    !================================================================!

    !----------------------------------------------------------------!
    !                         CONSTRUCTOR                            !
    !----------------------------------------------------------------!


      function hdual_from_dble(x11,x12,x21,x22) result(q)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_from_dble" :: hdual_from_dble
          
        implicit none
        real(PR), intent(in)  :: x11, x12, x21, x22
        TYPE(hyperdual)       :: q
        
        q%x = x11
        q%dx1 = x12
        q%dx2 = x21
        q%dx1x2 = x22
              
      end function hdual_from_dble


      function hdual_from_one_dble(x1) result(q)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_from_one_dble" :: hdual_from_one_dble
          
        implicit none
        real(PR), intent(in)  :: x1
        TYPE(hyperdual)       :: q
        
        q%x = x1
        q%dx1 = 0
        q%dx2 = 0
        q%dx1x2 = 0
              
      end function hdual_from_one_dble


      function hdual_from_one_sngl(x1) result(q)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_from_one_sngl" :: hdual_from_one_sngl
          
        implicit none
        real(PR), intent(in)  :: x1
        TYPE(hyperdual)       :: q
        
        q%x = SNGL(x1)
        q%dx1 = 0
        q%dx2 = 0
        q%dx1x2 = 0
              
      end function hdual_from_one_sngl

      
      
      
    !----------------------------------------------------------------!
    !                          ASSIGNMENT                            !
    !----------------------------------------------------------------!


      subroutine hdual_assign_hdual(qleft, qright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_assign_hdual" :: hdual_assign_hdual
        implicit none
        TYPE(hyperdual), intent(out) :: qleft
        TYPE(hyperdual), intent(in)  :: qright

        qleft%x     = qright%x
        qleft%dx1   = qright%dx1
        qleft%dx2   = qright%dx2
        qleft%dx1x2 = qright%dx1x2

      end subroutine hdual_assign_hdual
      

      subroutine hdual_assign_dble(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_assign_dble" :: hdual_assign_dble
          
        implicit none
        TYPE(hyperdual), intent(out)  :: qleft
        real(PR), intent(in)          :: iright

        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
  
      end subroutine hdual_assign_dble


      subroutine hdual_assign_int(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_assign_int" :: hdual_assign_int
        
        implicit none
        TYPE(hyperdual), intent(out)  :: qleft
        integer, intent(in)           :: iright
      
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
        
      end subroutine hdual_assign_int


      subroutine hdual_array_assign_hdual(qleft, qright)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_hdual" :: hdual_array_assign_hdual

        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        TYPE(hyperdual), intent(in)                 :: qright

        qleft%x = qright%x
        qleft%dx1 = qright%dx1
        qleft%dx2 = qright%dx2
        qleft%dx1x2 = qright%dx1x2
  
      end subroutine hdual_array_assign_hdual


      subroutine hdual_array_assign_hdual_array(qleft, qright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_hdual_array" :: hdual_array_assign_hdual_array
        
        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        TYPE(hyperdual), dimension(:), intent(in)   :: qright
                
        qleft%x = qright%x
        qleft%dx1 = qright%dx1
        qleft%dx2 = qright%dx2
        qleft%dx1x2 = qright%dx1x2
                  
        end subroutine hdual_array_assign_hdual_array
        

      subroutine hdual_array_assign_dble(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_dble" :: hdual_array_assign_dble
                
        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        real(PR), intent(in)                        :: iright
              
        qleft%x = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
                
      end subroutine hdual_array_assign_dble


      subroutine hdual_array_assign_dble_array(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_dble_array" :: hdual_array_assign_dble_array
          
        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        real(PR), dimension(:), intent(in)          :: iright
              
        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
                  
      end subroutine hdual_array_assign_dble_array
  

      subroutine hdual_array_assign_int(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_int" :: hdual_array_assign_int
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        integer, intent(in)                         :: iright
                
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
          
      end subroutine hdual_array_assign_int


      subroutine hdual_array_assign_int_array(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_assign_int_array" :: hdual_array_assign_int_array
                
        implicit none
        TYPE(hyperdual), dimension(:), intent(out)  :: qleft
        integer, dimension(:), intent(in)           :: iright
                
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
            
      end subroutine hdual_array_assign_int_array


      subroutine hdual_matrix_assign_hdual(qleft, qright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_hdual" :: hdual_matrix_assign_hdual
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        TYPE(hyperdual), intent(in)                   :: qright
      
        qleft%x     = qright%x
        qleft%dx1   = qright%dx1
        qleft%dx2   = qright%dx2
        qleft%dx1x2 = qright%dx1x2
      
      end subroutine hdual_matrix_assign_hdual


      subroutine hdual_matrix_assign_hdual_matrix(qleft, qright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_hdual_matrix" :: hdual_matrix_assign_hdual_matrix
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in)   :: qright
      
        qleft%x     = qright%x
        qleft%dx1   = qright%dx1
        qleft%dx2   = qright%dx2
        qleft%dx1x2 = qright%dx1x2
      
      end subroutine hdual_matrix_assign_hdual_matrix


      subroutine hdual_matrix_assign_dble(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_dble" :: hdual_matrix_assign_dble
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        real(PR), intent(in)                          :: iright
      
        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_matrix_assign_dble


      subroutine hdual_matrix_assign_dble_matrix(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_dble_matrix" :: hdual_matrix_assign_dble_matrix
              
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        real(PR), dimension(:,:), intent(in)          :: iright
              
        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
            
      end subroutine hdual_matrix_assign_dble_matrix


      subroutine hdual_matrix_assign_int(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_int" :: hdual_matrix_assign_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        integer, intent(in)                           :: iright
      
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_matrix_assign_int


      subroutine hdual_matrix_assign_int_matrix(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_assign_int_matrix" :: hdual_matrix_assign_int_matrix
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
        integer, dimension(:,:), intent(in)           :: iright
      
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_matrix_assign_int_matrix


      subroutine hdual_tens_assign_hdual(qleft, qright) 
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_hdual" :: hdual_tens_assign_hdual
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
        TYPE(hyperdual), intent(in)                     :: qright
      
        qleft%x     = qright%x
        qleft%dx1   = qright%dx1
        qleft%dx2   = qright%dx2
        qleft%dx1x2 = qright%dx1x2
      
      end subroutine hdual_tens_assign_hdual


      subroutine hdual_tens_assign_hdual_tens(qleft, qright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_hdual_tens" :: hdual_tens_assign_hdual_tens
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qright
      
        qleft%x     = qright%x
        qleft%dx1   = qright%dx1
        qleft%dx2   = qright%dx2
        qleft%dx1x2 = qright%dx1x2
      
      end subroutine hdual_tens_assign_hdual_tens


      subroutine hdual_tens_assign_dble(qleft, iright) 
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_dble" :: hdual_tens_assign_dble
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out) :: qleft
        real(PR), intent(in) :: iright
      
        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_tens_assign_dble


      subroutine hdual_tens_assign_dble_tens(qleft, iright) 
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_dble_tens" :: hdual_tens_assign_dble_tens
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
        real(PR), dimension(:,:,:), intent(in)          :: iright
      
        qleft%x     = iright
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_tens_assign_dble_tens


      subroutine hdual_tens_assign_int(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_int" :: hdual_tens_assign_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
        integer, intent(in)                             :: iright
      
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_tens_assign_int


      subroutine hdual_tens_assign_int_tens(qleft, iright) 
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_assign_int_tens" :: hdual_tens_assign_int_tens
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
        integer, dimension(:,:,:), intent(in)           :: iright
      
        qleft%x     = REAL(iright, PR)
        qleft%dx1   = 0.0_PR
        qleft%dx2   = 0.0_PR
        qleft%dx1x2 = 0.0_PR
      
      end subroutine hdual_tens_assign_int_tens


    !----------------------------------------------------------------!
    !                     COMPARISON OPERATORS                       !
    !----------------------------------------------------------------!

    !----- EQ operator (==)
      function hdual_eq_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_eq_hdual" :: hdual_eq_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR).EQ.REAL(qright%x,PR))
          
      end function hdual_eq_hdual


      function hdual_eq_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_eq_dble" :: hdual_eq_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR).EQ.iright)
        
      end function hdual_eq_dble
      

      function dble_eq_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_eq_hdual" :: dble_eq_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR).EQ.xleft)
        
      end function dble_eq_hdual

    
    !----- NE operator (/=)
      function hdual_ne_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_ne_hdual" :: hdual_ne_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR).NE.REAL(qright%x,PR))
          
      end function hdual_ne_hdual


      function hdual_ne_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_ne_dble" :: hdual_ne_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR).NE.iright)
        
      end function hdual_ne_dble
      

      function dble_ne_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_ne_hdual" :: dble_ne_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR).NE.xleft)
        
      end function dble_ne_hdual

      
      !----- GT operator (>)
      function hdual_gt_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_gt_hdual" :: hdual_gt_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) > REAL(qright%x,PR))
          
      end function hdual_gt_hdual


      function hdual_gt_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_gt_dble" :: hdual_gt_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) > iright)
        
      end function hdual_gt_dble
      

      function dble_gt_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_gt_hdual" :: dble_gt_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR) > xleft)
        
      end function dble_gt_hdual

      
      !----- GE operator (>=)
      function hdual_ge_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_ge_hdual" :: hdual_ge_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) >= REAL(qright%x,PR))
          
      end function hdual_ge_hdual


      function hdual_ge_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_ge_dble" :: hdual_ge_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) >= iright)
        
      end function hdual_ge_dble
      

      function dble_ge_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_ge_hdual" :: dble_ge_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR) >= xleft)
        
      end function dble_ge_hdual

      
      !----- LT operator (<)
      function hdual_lt_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_lt_hdual" :: hdual_lt_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) < REAL(qright%x,PR))
          
      end function hdual_lt_hdual


      function hdual_lt_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_lt_dble" :: hdual_lt_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) < iright)
        
      end function hdual_lt_dble
      

      function dble_lt_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_lt_hdual" :: dble_lt_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR) < xleft)
        
      end function dble_lt_hdual

      
      !----- LE operator (<)
      function hdual_le_hdual(qleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_le_hdual" :: hdual_le_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) <= REAL(qright%x,PR))
          
      end function hdual_le_hdual


      function hdual_le_dble(qleft, iright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_le_dble" :: hdual_le_dble

        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        REAL(PR), intent(in)        :: iright
        logical                     :: bool
        
        bool = (REAL(qleft%x,PR) <= iright)
        
      end function hdual_le_dble
      

      function dble_le_hdual(xleft, qright) result(bool)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_le_hdual" :: dble_le_hdual

        implicit none
        TYPE(hyperdual), intent(in) :: qright
        REAL(PR), intent(in)        :: xleft
        logical                     :: bool
        
        bool = (REAL(qright%x,PR) <= xleft)
        
      end function dble_le_hdual


      !----------------------------------------------------------------!
      !                     ARITHMETIC OPERATORS                       !
      !----------------------------------------------------------------!

      !----- Addition operator (+)
      function hdual_plus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_hdual" :: hdual_plus_hdual
        
        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        TYPE(hyperdual) :: res
              
        res%x = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                  
      end function hdual_plus_hdual


      function hdual_plus_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_hdual_array" :: hdual_plus_hdual_array
        
        implicit none
        TYPE(hyperdual), intent(in)               :: qleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                
      end function hdual_plus_hdual_array
        
      
      function hdual_plus_hdual_matrix(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_hdual_matrix" :: hdual_plus_hdual_matrix
      
        implicit none
        TYPE(hyperdual), intent(in)                 :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
              
        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                
      end function hdual_plus_hdual_matrix
                
      function hdual_plus_hdual_tens(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_hdual_tens" :: hdual_plus_hdual_tens
      
        implicit none
        TYPE(hyperdual), intent(in)                   :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
              
        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                
      end function hdual_plus_hdual_tens
         
      
      function hdual_plus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_dble" :: hdual_plus_dble
              
        implicit none      
        TYPE(hyperdual), intent(in) :: qleft
        real(PR), intent(in)        :: iright
        TYPE(hyperdual)             :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
                  
      end function hdual_plus_dble
           
      
      function hdual_plus_dble_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_dble_array" :: hdual_plus_dble_array
              
        implicit none      
        TYPE(hyperdual), intent(in)         :: qleft
        real(PR), dimension(:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_dble_array
              

      function hdual_plus_dble_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_dble_matrix" :: hdual_plus_dble_matrix
              
        implicit none      
        TYPE(hyperdual), intent(in)           :: qleft
        real(PR), dimension(:,:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_dble_matrix
           
      
      function hdual_plus_dble_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_dble_tens" :: hdual_plus_dble_tens
              
        implicit none      
        TYPE(hyperdual), intent(in)             :: qleft
        real(PR), dimension(:,:,:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
              
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_dble_tens
              

      function hdual_plus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_int" :: hdual_plus_int
              
        implicit none      
        TYPE(hyperdual), intent(in) :: qleft
        integer, intent(in)         :: iright
        TYPE(hyperdual)             :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_int
            
      
      function hdual_plus_int_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_int_array" :: hdual_plus_int_array
              
        implicit none      
        TYPE(hyperdual), intent(in)       :: qleft
        integer, dimension(:), intent(in) :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_int_array
      
      
      function hdual_plus_int_matrix(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_int_matrix" :: hdual_plus_int_matrix
              
        implicit none      
        TYPE(hyperdual), intent(in)         :: qleft
        integer, dimension(:,:), intent(in) :: iright
        TYPE(hyperdual), dimension(size(iright,1), size(iright,2))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_int_matrix
         
      
      function hdual_plus_int_tens(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_plus_int_tens" :: hdual_plus_int_tens
              
        implicit none      
        TYPE(hyperdual), intent(in)           :: qleft
        integer, dimension(:,:,:), intent(in) :: iright
        TYPE(hyperdual), &
          dimension(size(iright,1), size(iright,2), size(iright,3))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_plus_int_tens
        
      
      function hdual_array_plus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_hdual" :: hdual_array_plus_hdual
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                  
      end function hdual_array_plus_hdual
            
      
      function hdual_array_plus_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_hdual_array" :: hdual_array_plus_hdual_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft, qright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                  
      end function hdual_array_plus_hdual_array
           
      
      function hdual_array_plus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_dble" :: hdual_array_plus_dble
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), intent(in)                      :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_array_plus_dble
      
      
      function hdual_array_plus_dble_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_dble_array" :: hdual_array_plus_dble_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), dimension(:), intent(in)        :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_array_plus_dble_array
        
      
      function hdual_array_plus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_int" :: hdual_array_plus_int
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, intent(in)                       :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_array_plus_int
             
      
      function hdual_array_plus_int_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_plus_int_array" :: hdual_array_plus_int_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, dimension(:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_array_plus_int_array
             
      
      function hdual_matrix_plus_hdual(qleft, qright) result(res)
       !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_hdual" :: hdual_matrix_plus_hdual

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                 :: qright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2

      end function hdual_matrix_plus_hdual


      function hdual_matrix_plus_hdual_matrix(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_hdual_matrix" :: hdual_matrix_plus_hdual_matrix

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res

        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2

      end function hdual_matrix_plus_hdual_matrix


      function hdual_matrix_plus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_dble" :: hdual_matrix_plus_dble

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), intent(in)                        :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_matrix_plus_dble


      function hdual_matrix_plus_dble_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_dble_matrix" :: hdual_matrix_plus_dble_matrix

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), dimension(:,:), intent(in)        :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_matrix_plus_dble_matrix
        
      
      function hdual_matrix_plus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_int" :: hdual_matrix_plus_int
              
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, intent(in)                         :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_matrix_plus_int
         
      
      function hdual_matrix_plus_int_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_plus_int_matrix" :: hdual_matrix_plus_int_matrix
              
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, dimension(:,:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_matrix_plus_int_matrix


      function hdual_tens_plus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_hdual" :: hdual_tens_plus_hdual

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                   :: qright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2 
      
      end function

      function hdual_tens_plus_hdual_tens(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_hdual_tens" :: hdual_tens_plus_hdual_tens

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3))  :: res

        res%x  = qleft%x + qright%x
        res%dx1 = qleft%dx1 + qright%dx1
        res%dx2 = qleft%dx2 + qright%dx2
        res%dx1x2 = qleft%dx1x2 + qright%dx1x2
      
      end function hdual_tens_plus_hdual_tens

      function hdual_tens_plus_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_dble" :: hdual_tens_plus_dble

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), intent(in)                          :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_tens_plus_dble


      function hdual_tens_plus_dble_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_dble_tens" :: hdual_tens_plus_dble_tens

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), dimension(:,:,:), intent(in)        :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_tens_plus_dble_tens
       
      
      function hdual_tens_plus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_int" :: hdual_tens_plus_int
              
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, intent(in)                           :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1), size(qleft,2), size(qleft,3))  :: res
              
        res%x  = qleft%x + iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_tens_plus_int
        
      
      function hdual_tens_plus_int_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_plus_int_tens" :: hdual_tens_plus_int_tens
              
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, dimension(:,:,:), intent(in)         :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1), size(qleft,2), size(qleft,3))  :: res
              
        res%x = qleft%x + REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_tens_plus_int_tens


      function dble_plus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_plus_hdual" :: dble_plus_hdual
              
        implicit none
        real(PR), intent(in)        :: xleft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function dble_plus_hdual
         
      
      function dble_plus_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_plus_hdual_array" :: dble_plus_hdual_array
              
        implicit none
        real(PR), intent(in)                      :: xleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_plus_hdual_array


      function dble_plus_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_plus_hdual_matrix" :: dble_plus_hdual_matrix

        implicit none
        real(PR), intent(in)                        :: xleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res

        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
      
      end function dble_plus_hdual_matrix


      function dble_plus_hdual_tens(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_plus_hdual_tens" :: dble_plus_hdual_tens
              
        implicit none
        real(PR), intent(in)                          :: xleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_plus_hdual_tens


      function dble_array_plus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_plus_hdual" :: dble_array_plus_hdual
              
        implicit none
        real(PR), dimension(:), intent(in)        :: xleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(xleft))   :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_array_plus_hdual


      function dble_array_plus_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_plus_hdual_array" :: dble_array_plus_hdual_array
              
        implicit none
        real(PR), dimension(:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
              
      end function dble_array_plus_hdual_array


      function dble_matrix_plus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_plus_hdual" :: dble_matrix_plus_hdual
              
        implicit none
        real(PR), dimension(:,:), intent(in)      :: xleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_matrix_plus_hdual


      function dble_matrix_plus_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_plus_hdual_matrix" :: dble_matrix_plus_hdual_matrix
              
        implicit none
        real(PR), dimension(:,:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_matrix_plus_hdual_matrix


      function dble_tens_plus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_plus_hdual" :: dble_tens_plus_hdual
              
        implicit none
        real(PR), dimension(:,:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_tens_plus_hdual


      function dble_tens_plus_hdual_tens(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_plus_hdual_tens" :: dble_tens_plus_hdual_tens
              
        implicit none
        real(PR), dimension(:,:,:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
              
        res%x = qright%x + xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_tens_plus_hdual_tens


      function int_plus_hdual(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_plus_hdual" :: int_plus_hdual
              
        implicit none
        integer, intent(in)         :: ileft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_plus_hdual


      function int_plus_hdual_array(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_plus_hdual_array" :: int_plus_hdual_array
              
        implicit none
        integer, intent(in)                       :: ileft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_plus_hdual_array


      function int_plus_hdual_matrix(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_plus_hdual_matrix" :: int_plus_hdual_matrix
              
        implicit none
        integer, intent(in)                         :: ileft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_plus_hdual_matrix


      function int_plus_hdual_tens(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_plus_hdual_tens" :: int_plus_hdual_tens
              
        implicit none
        integer, intent(in)                           :: ileft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1), size(qright,2), size(qright,3)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_plus_hdual_tens


      function int_array_plus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_plus_hdual" :: int_array_plus_hdual
              
        implicit none
        integer, dimension(:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)       :: qright
        TYPE(hyperdual), dimension(size(ileft)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_array_plus_hdual

      function int_array_plus_hdual_array(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_plus_hdual_array" :: int_array_plus_hdual_array
              
        implicit none
        integer, dimension(:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(ileft))   :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_array_plus_hdual_array

      function int_matrix_plus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_plus_hdual" :: int_matrix_plus_hdual
              
        implicit none
        integer, dimension(:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)         :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_matrix_plus_hdual

      function int_matrix_plus_hdual_matrix(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_plus_hdual_matrix" :: int_matrix_plus_hdual_matrix
              
        implicit none
        integer, dimension(:,:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_matrix_plus_hdual_matrix

      function int_tens_plus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_plus_hdual" :: int_tens_plus_hdual
              
        implicit none
        integer, dimension(:,:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)           :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_tens_plus_hdual

      function int_tens_plus_hdual_tens(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_plus_hdual_tens" :: int_tens_plus_hdual_tens
              
        implicit none
        integer, dimension(:,:,:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
              
        res%x = qright%x + REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_tens_plus_hdual_tens


!----- Subtraction operator (-)
      function hdual_minus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_hdual" :: hdual_minus_hdual
        
        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        TYPE(hyperdual) :: res
              
        res%x = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                  
      end function hdual_minus_hdual


      function hdual_minus_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_hdual_array" :: hdual_minus_hdual_array
        
        implicit none
        TYPE(hyperdual), intent(in)               :: qleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                
      end function hdual_minus_hdual_array
        
      
      function hdual_minus_hdual_matrix(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_hdual_matrix" :: hdual_minus_hdual_matrix
      
        implicit none
        TYPE(hyperdual), intent(in)                 :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
              
        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                
      end function hdual_minus_hdual_matrix
                
      function hdual_minus_hdual_tens(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_hdual_tens" :: hdual_minus_hdual_tens
      
        implicit none
        TYPE(hyperdual), intent(in)                   :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
              
        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                
      end function hdual_minus_hdual_tens
         
      
      function hdual_minus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_dble" :: hdual_minus_dble
              
        implicit none      
        TYPE(hyperdual), intent(in) :: qleft
        real(PR), intent(in)        :: iright
        TYPE(hyperdual)             :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
                  
      end function hdual_minus_dble
           
      
      function hdual_minus_dble_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_dble_array" :: hdual_minus_dble_array
              
        implicit none      
        TYPE(hyperdual), intent(in)         :: qleft
        real(PR), dimension(:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_dble_array
              

      function hdual_minus_dble_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_dble_matrix" :: hdual_minus_dble_matrix
              
        implicit none      
        TYPE(hyperdual), intent(in)           :: qleft
        real(PR), dimension(:,:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_dble_matrix
           
      
      function hdual_minus_dble_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_dble_tens" :: hdual_minus_dble_tens
              
        implicit none      
        TYPE(hyperdual), intent(in)             :: qleft
        real(PR), dimension(:,:,:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
              
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_dble_tens
              

      function hdual_minus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_int" :: hdual_minus_int
              
        implicit none      
        TYPE(hyperdual), intent(in) :: qleft
        integer, intent(in)         :: iright
        TYPE(hyperdual)             :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_int
            
      
      function hdual_minus_int_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_int_array" :: hdual_minus_int_array
              
        implicit none      
        TYPE(hyperdual), intent(in)       :: qleft
        integer, dimension(:), intent(in) :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_int_array
      
      
      function hdual_minus_int_matrix(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_int_matrix" :: hdual_minus_int_matrix
              
        implicit none      
        TYPE(hyperdual), intent(in)         :: qleft
        integer, dimension(:,:), intent(in) :: iright
        TYPE(hyperdual), dimension(size(iright,1), size(iright,2))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_int_matrix
         
      
      function hdual_minus_int_tens(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_minus_int_tens" :: hdual_minus_int_tens
              
        implicit none      
        TYPE(hyperdual), intent(in)           :: qleft
        integer, dimension(:,:,:), intent(in) :: iright
        TYPE(hyperdual), &
          dimension(size(iright,1), size(iright,2), size(iright,3))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
                  
      end function hdual_minus_int_tens
        
      
      function hdual_array_minus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_hdual" :: hdual_array_minus_hdual
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                  
      end function hdual_array_minus_hdual
            
      
      function hdual_array_minus_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_hdual_array" :: hdual_array_minus_hdual_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft, qright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                  
      end function hdual_array_minus_hdual_array
           
      
      function hdual_array_minus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_dble" :: hdual_array_minus_dble
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), intent(in)                      :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_array_minus_dble
      
      
      function hdual_array_minus_dble_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_dble_array" :: hdual_array_minus_dble_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), dimension(:), intent(in)        :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_array_minus_dble_array
        
      
      function hdual_array_minus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_int" :: hdual_array_minus_int
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, intent(in)                       :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_array_minus_int
             
      
      function hdual_array_minus_int_array(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_minus_int_array" :: hdual_array_minus_int_array
              
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, dimension(:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_array_minus_int_array
             
      
      function hdual_matrix_minus_hdual(qleft, qright) result(res)
       !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_hdual" :: hdual_matrix_minus_hdual

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                 :: qright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2

      end function hdual_matrix_minus_hdual


      function hdual_matrix_minus_hdual_matrix(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_hdual_matrix" :: hdual_matrix_minus_hdual_matrix

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res

        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2

      end function hdual_matrix_minus_hdual_matrix


      function hdual_matrix_minus_dble(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_dble" :: hdual_matrix_minus_dble

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), intent(in)                        :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_matrix_minus_dble


      function hdual_matrix_minus_dble_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_dble_matrix" :: hdual_matrix_minus_dble_matrix

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), dimension(:,:), intent(in)        :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_matrix_minus_dble_matrix
        
      
      function hdual_matrix_minus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_int" :: hdual_matrix_minus_int
              
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, intent(in)                         :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_matrix_minus_int
         
      
      function hdual_matrix_minus_int_matrix(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_minus_int_matrix" :: hdual_matrix_minus_int_matrix
              
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, dimension(:,:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_matrix_minus_int_matrix


      function hdual_tens_minus_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_hdual" :: hdual_tens_minus_hdual

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                   :: qright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2 
      
      end function

      function hdual_tens_minus_hdual_tens(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_hdual_tens" :: hdual_tens_minus_hdual_tens

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3))  :: res

        res%x  = qleft%x - qright%x
        res%dx1 = qleft%dx1 - qright%dx1
        res%dx2 = qleft%dx2 - qright%dx2
        res%dx1x2 = qleft%dx1x2 - qright%dx1x2
      
      end function hdual_tens_minus_hdual_tens

      function hdual_tens_minus_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_dble" :: hdual_tens_minus_dble

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), intent(in)                          :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_tens_minus_dble


      function hdual_tens_minus_dble_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_dble_tens" :: hdual_tens_minus_dble_tens

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), dimension(:,:,:), intent(in)        :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res

        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
      
      end function hdual_tens_minus_dble_tens
       
      
      function hdual_tens_minus_int(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_int" :: hdual_tens_minus_int
              
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, intent(in)                           :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1), size(qleft,2), size(qleft,3))  :: res
              
        res%x  = qleft%x - iright
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2 
        res%dx1x2 = qleft%dx1x2
              
      end function hdual_tens_minus_int
        
      
      function hdual_tens_minus_int_tens(qleft, iright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_minus_int_tens" :: hdual_tens_minus_int_tens
              
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, dimension(:,:,:), intent(in)         :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1), size(qleft,2), size(qleft,3))  :: res
              
        res%x = qleft%x - REAL(iright,PR)
        res%dx1 = qleft%dx1 
        res%dx2 = qleft%dx2
        res%dx1x2 = qleft%dx1x2 
              
      end function hdual_tens_minus_int_tens


      function dble_minus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_minus_hdual" :: dble_minus_hdual
              
        implicit none
        real(PR), intent(in)        :: xleft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function dble_minus_hdual
         
      
      function dble_minus_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_minus_hdual_array" :: dble_minus_hdual_array
              
        implicit none
        real(PR), intent(in)                      :: xleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_minus_hdual_array


      function dble_minus_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_minus_hdual_matrix" :: dble_minus_hdual_matrix

        implicit none
        real(PR), intent(in)                        :: xleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res

        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
      
      end function dble_minus_hdual_matrix


      function dble_minus_hdual_tens(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_minus_hdual_tens" :: dble_minus_hdual_tens
              
        implicit none
        real(PR), intent(in)                          :: xleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_minus_hdual_tens


      function dble_array_minus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_minus_hdual" :: dble_array_minus_hdual
              
        implicit none
        real(PR), dimension(:), intent(in)        :: xleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(xleft))   :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_array_minus_hdual


      function dble_array_minus_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_minus_hdual_array" :: dble_array_minus_hdual_array
              
        implicit none
        real(PR), dimension(:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
              
      end function dble_array_minus_hdual_array


      function dble_matrix_minus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_minus_hdual" :: dble_matrix_minus_hdual
              
        implicit none
        real(PR), dimension(:,:), intent(in)      :: xleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_matrix_minus_hdual


      function dble_matrix_minus_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_minus_hdual_matrix" :: dble_matrix_minus_hdual_matrix
              
        implicit none
        real(PR), dimension(:,:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_matrix_minus_hdual_matrix


      function dble_tens_minus_hdual(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_minus_hdual" :: dble_tens_minus_hdual
              
        implicit none
        real(PR), dimension(:,:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_tens_minus_hdual


      function dble_tens_minus_hdual_tens(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_minus_hdual_tens" :: dble_tens_minus_hdual_tens
              
        implicit none
        real(PR), dimension(:,:,:), intent(in)        :: xleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
              
        res%x = qright%x - xleft
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
              
      end function dble_tens_minus_hdual_tens


      function int_minus_hdual(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_minus_hdual" :: int_minus_hdual
              
        implicit none
        integer, intent(in)         :: ileft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_minus_hdual


      function int_minus_hdual_array(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_minus_hdual_array" :: int_minus_hdual_array
              
        implicit none
        integer, intent(in)                       :: ileft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_minus_hdual_array


      function int_minus_hdual_matrix(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_minus_hdual_matrix" :: int_minus_hdual_matrix
              
        implicit none
        integer, intent(in)                         :: ileft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_minus_hdual_matrix


      function int_minus_hdual_tens(ileft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_minus_hdual_tens" :: int_minus_hdual_tens
              
        implicit none
        integer, intent(in)                           :: ileft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1), size(qright,2), size(qright,3)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_minus_hdual_tens


      function int_array_minus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_minus_hdual" :: int_array_minus_hdual
              
        implicit none
        integer, dimension(:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)       :: qright
        TYPE(hyperdual), dimension(size(ileft)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_array_minus_hdual

      function int_array_minus_hdual_array(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_minus_hdual_array" :: int_array_minus_hdual_array
              
        implicit none
        integer, dimension(:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(ileft))   :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_array_minus_hdual_array

      function int_matrix_minus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_minus_hdual" :: int_matrix_minus_hdual
              
        implicit none
        integer, dimension(:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)         :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_matrix_minus_hdual

      function int_matrix_minus_hdual_matrix(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_minus_hdual_matrix" :: int_matrix_minus_hdual_matrix
              
        implicit none
        integer, dimension(:,:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2 
                  
      end function int_matrix_minus_hdual_matrix

      function int_tens_minus_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_minus_hdual" :: int_tens_minus_hdual
              
        implicit none
        integer, dimension(:,:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)           :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_tens_minus_hdual

      function int_tens_minus_hdual_tens(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_minus_hdual_tens" :: int_tens_minus_hdual_tens
              
        implicit none
        integer, dimension(:,:,:), intent(in)         :: ileft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
              
        res%x = qright%x - REAL(ileft,PR)
        res%dx1 = qright%dx1 
        res%dx2 = qright%dx2 
        res%dx1x2 = qright%dx1x2
                  
      end function int_tens_minus_hdual_tens

      function minus_hdual(qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "minus_hdual" :: minus_hdual
        
        implicit none
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
      
        res%x     = - qright%x 
        res%dx1   = qright%dx1
        res%dx2   = qright%dx2
        res%dx1x2 = qright%dx1x2
      
      end function minus_hdual

      function minus_hdual_array(qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "minus_hdual_array" :: minus_hdual_array
        
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
      
        res%x     = - qright%x 
        res%dx1   = qright%dx1
        res%dx2   = qright%dx2
        res%dx1x2 = qright%dx1x2
      
      end function minus_hdual_array

      function minus_hdual_matrix(qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "minus_hdual_matrix" :: minus_hdual_matrix
        
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
      
        res%x     = - qright%x 
        res%dx1   = qright%dx1
        res%dx2   = qright%dx2
        res%dx1x2 = qright%dx1x2
      
      end function minus_hdual_matrix

      function minus_hdual_tens(qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "minus_hdual_tens" :: minus_hdual_tens
        
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1), size(qright,2),size(qright,3))  :: res
      
        res%x     = - qright%x 
        res%dx1   = qright%dx1
        res%dx2   = qright%dx2
        res%dx1x2 = qright%dx1x2
      
      end function minus_hdual_tens


      !----- Multiplication operator (*)
      function hdual_mul_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_hdual" :: hdual_mul_hdual
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        TYPE(hyperdual)             :: res
      
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
      
      end function hdual_mul_hdual
      
      function hdual_mul_hdual_array(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_hdual_array" :: hdual_mul_hdual_array
      
        implicit none
        TYPE(hyperdual), intent(in)               :: qleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
      
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
      
      end function hdual_mul_hdual_array
      
      function hdual_mul_hdual_matrix(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_hdual_matrix" :: hdual_mul_hdual_matrix
      
        implicit none
        TYPE(hyperdual), intent(in)                 :: qleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
      
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
      
      end function hdual_mul_hdual_matrix
      
      function hdual_mul_hdual_tens(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_hdual_tens" :: hdual_mul_hdual_tens
      
        implicit none
        TYPE(hyperdual), intent(in)                   :: qleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
      
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
      
      end function hdual_mul_hdual_tens

      function hdual_mul_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_dble" :: hdual_mul_dble
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        real(PR), intent(in)        :: iright
        TYPE(hyperdual)             :: res
      
        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
      
      end function hdual_mul_dble
      
      function hdual_mul_dble_array(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_dble_array" :: hdual_mul_dble_array
      
        implicit none
        TYPE(hyperdual), intent(in)         :: qleft
        real(PR), dimension(:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
      
        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
      
      end function hdual_mul_dble_array

      function hdual_mul_dble_matrix(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_dble_matrix" :: hdual_mul_dble_matrix
      
        implicit none
        TYPE(hyperdual), intent(in)           :: qleft
        real(PR), dimension(:,:), intent(in)  :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
      
        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
      
      end function hdual_mul_dble_matrix

      function hdual_mul_dble_tens(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_dble_tens" :: hdual_mul_dble_tens
      
        implicit none
        TYPE(hyperdual), intent(in)             :: qleft
        real(PR), dimension(:,:,:), intent(in)  :: iright
        TYPE(hyperdual), &
          dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
      
        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
      
      end function hdual_mul_dble_tens

      function hdual_mul_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_int" :: hdual_mul_int
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        integer, intent(in)         :: iright
        TYPE(hyperdual)             :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_mul_int

      function hdual_mul_int_array(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_int_array" :: hdual_mul_int_array
      
        implicit none
        TYPE(hyperdual), intent(in)               :: qleft
        integer, dimension(:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(iright))  :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_mul_int_array

      function hdual_mul_int_matrix(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_int_matrix" :: hdual_mul_int_matrix
      
        implicit none
        TYPE(hyperdual), intent(in)                 :: qleft
        integer, dimension(:,:), intent(in)         :: iright
        TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_mul_int_matrix

      function hdual_mul_int_tens(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_mul_int_tens" :: hdual_mul_int_tens
      
        implicit none
        TYPE(hyperdual), intent(in)                   :: qleft
        integer, dimension(:,:,:), intent(in)         :: iright
        TYPE(hyperdual), &
          dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_mul_int_tens

      function hdual_array_mul_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_mul_hdual" :: hdual_array_mul_hdual

        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(qleft))   :: res

        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
        
      end function hdual_array_mul_hdual

      function hdual_array_mul_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_mul_dble" :: hdual_array_mul_dble

        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), intent(in)                      :: iright
        TYPE(hyperdual), dimension(size(qleft)) :: res

        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
        
      end function hdual_array_mul_dble

      function hdual_array_mul_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_mul_int" :: hdual_array_mul_int
      
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, intent(in)                       :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_array_mul_int

      function hdual_matrix_mul_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_mul_hdual" :: hdual_matrix_mul_hdual
        
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                 :: qright
        TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
        
        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x

      end function hdual_matrix_mul_hdual

      function hdual_matrix_mul_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_mul_dble" :: hdual_matrix_mul_dble

        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), intent(in)                        :: iright
        TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res

        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
        
      end function hdual_matrix_mul_dble

      function hdual_matrix_mul_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_mul_int" :: hdual_matrix_mul_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, intent(in)                         :: iright
        TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_matrix_mul_int

      function hdual_tens_mul_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_mul_hdual" :: hdual_tens_mul_hdual

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                   :: qright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res

        res%x = qleft%x * qright%x
        res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
        res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
        res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
        
      end function

      function hdual_tens_mul_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_mul_dble" :: hdual_tens_mul_dble

        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), intent(in)                          :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res

        res%x = qleft%x * iright
        res%dx1 = qleft%dx1 * iright
        res%dx2 = qleft%dx2 * iright
        res%dx1x2 = qleft%dx1x2 * iright
        
      end function

      function hdual_tens_mul_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_mul_int" :: hdual_tens_mul_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, intent(in)                           :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res
      
        res%x = qleft%x * REAL(iright,PR)
        res%dx1 = qleft%dx1 * REAL(iright,PR)
        res%dx2 = qleft%dx2 * REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
      
      end function hdual_tens_mul_int

      function dble_mul_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_mul_hdual" :: dble_mul_hdual
      
        implicit none
        real(PR), intent(in)        :: xleft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_mul_hdual
      
      function dble_mul_hdual_array(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_mul_hdual_array" :: dble_mul_hdual_array
      
        implicit none
        real(PR), intent(in)                      :: xleft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_mul_hdual_array
      
      function dble_mul_hdual_matrix(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_mul_hdual_matrix" :: dble_mul_hdual_matrix
      
        implicit none
        real(PR), intent(in)                        :: xleft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_mul_hdual_matrix
      
      function dble_mul_hdual_tens(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_mul_hdual_tens" :: dble_mul_hdual_tens
      
        implicit none
        real(PR), intent(in)                          :: xleft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_mul_hdual_tens
      
      function dble_array_mul_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_mul_hdual" :: dble_array_mul_hdual
      
        implicit none
        real(PR), dimension(:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)         :: qright
        TYPE(hyperdual), dimension(size(xleft)) :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_array_mul_hdual
      
      function dble_matrix_mul_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_mul_hdual" :: dble_matrix_mul_hdual
      
        implicit none
        real(PR), dimension(:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)           :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_matrix_mul_hdual
      
      function dble_tens_mul_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_mul_hdual" :: dble_tens_mul_hdual
      
        implicit none
        real(PR), dimension(:,:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
      
        res%x = qright%x * xleft
        res%dx1 = qright%dx1 * xleft
        res%dx2 = qright%dx2 * xleft
        res%dx1x2 = qright%dx1x2 * xleft
      
      end function dble_tens_mul_hdual
      
      function int_mul_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_mul_hdual" :: int_mul_hdual
      
        implicit none
        integer, intent(in)         :: ileft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_mul_hdual

      function int_mul_hdual_array(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_mul_hdual_array" :: int_mul_hdual_array
      
        implicit none
        integer, intent(in)                       :: ileft
        TYPE(hyperdual), dimension(:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright))  :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_mul_hdual_array

      function int_mul_hdual_matrix(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_mul_hdual_matrix" :: int_mul_hdual_matrix
      
        implicit none
        integer, intent(in)                         :: ileft
        TYPE(hyperdual), dimension(:,:), intent(in) :: qright
        TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_mul_hdual_matrix

      function int_mul_hdual_tens(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_mul_hdual_tens" :: int_mul_hdual_tens
      
        implicit none
        integer, intent(in)                           :: ileft
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
        TYPE(hyperdual), &
          dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_mul_hdual_tens

      function int_array_mul_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_mul_hdual" :: int_array_mul_hdual
      
        implicit none
        integer, dimension(:), intent(in)       :: ileft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), dimension(size(ileft)) :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_array_mul_hdual

      function int_matrix_mul_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_mul_hdual" :: int_matrix_mul_hdual
      
        implicit none
        integer, dimension(:,:), intent(in)       :: ileft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_matrix_mul_hdual

      function int_tens_mul_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_mul_hdual" :: int_tens_mul_hdual
      
        implicit none
        integer, dimension(:,:,:), intent(in)       :: ileft
        TYPE(hyperdual), intent(in)                 :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
      
        res%x = qright%x * REAL(ileft,PR)
        res%dx1 = qright%dx1 * REAL(ileft,PR)
        res%dx2 = qright%dx2 * REAL(ileft,PR)
        res%dx1x2 = qright%dx1x2 * REAL(ileft,PR)
      
      end function int_tens_mul_hdual



      !----- Division operator (/)
      function hdual_div_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_div_hdual" :: hdual_div_hdual
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft, qright
        TYPE(hyperdual)             :: inv
        TYPE(hyperdual)             :: res
        
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = qleft * inv

      end function hdual_div_hdual
      
      function hdual_div_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_div_dble" :: hdual_div_dble
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        real(PR), intent(in)        :: iright
        TYPE(hyperdual)             :: res
      
        res%x = qleft%x / iright
        res%dx1 = qleft%dx1 / iright
        res%dx2 = qleft%dx2 / iright
        res%dx1x2 = qleft%dx1x2 / iright
          
      end function hdual_div_dble
      
      function hdual_div_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_div_int" :: hdual_div_int
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        integer, intent(in)         :: iright
        TYPE(hyperdual)             :: res
      
        res%x = qleft%x / REAL(iright,PR)
        res%dx1 = qleft%dx1 / REAL(iright,PR)
        res%dx2 = qleft%dx2 / REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
          
      end function hdual_div_int
      
      function hdual_array_div_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_div_hdual" :: hdual_array_div_hdual
      
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(qleft))   :: res
        TYPE(hyperdual)                           :: inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = qleft * inv
        
      end function hdual_array_div_hdual
      
      function hdual_array_div_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_div_dble" :: hdual_array_div_dble
      
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        real(PR), intent(in)                      :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
      
        res%x = qleft%x / iright
        res%dx1 = qleft%dx1 / iright
        res%dx2 = qleft%dx2 / iright
        res%dx1x2 = qleft%dx1x2 / iright
      
      end function hdual_array_div_dble
      
      function hdual_array_div_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_array_div_int" :: hdual_array_div_int
      
        implicit none
        TYPE(hyperdual), dimension(:), intent(in) :: qleft
        integer, intent(in)                       :: iright
        TYPE(hyperdual), dimension(size(qleft))   :: res
      
        res%x = qleft%x / REAL(iright,PR)
        res%dx1 = qleft%dx1 / REAL(iright,PR)
        res%dx2 = qleft%dx2 / REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
          
      end function hdual_array_div_int
      
      function hdual_matrix_div_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_div_hdual" :: hdual_matrix_div_hdual
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                 :: qright
        TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
        TYPE(hyperdual)                               ::  inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = qleft * inv
      
      end function hdual_matrix_div_hdual
      
      function hdual_matrix_div_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_div_dble" :: hdual_matrix_div_dble
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        real(PR), intent(in)                        :: iright
        TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
      
        res%x = qleft%x / iright
        res%dx1 = qleft%dx1 / iright
        res%dx2 = qleft%dx2 / iright
        res%dx1x2 = qleft%dx1x2 / iright
      
      end function hdual_matrix_div_dble
      
      function hdual_matrix_div_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_matrix_div_int" :: hdual_matrix_div_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
        integer, intent(in)                         :: iright
        TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
      
        res%x = qleft%x / REAL(iright,PR)
        res%dx1 = qleft%dx1 / REAL(iright,PR)
        res%dx2 = qleft%dx2 / REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
          
      end function hdual_matrix_div_int
      
      function hdual_tens_div_hdual(qleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_div_hdual" :: hdual_tens_div_hdual
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        TYPE(hyperdual), intent(in)                   :: qright
        TYPE(hyperdual)                               ::  inv
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
      
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = qleft * inv
      
      end function hdual_tens_div_hdual
      
      function hdual_tens_div_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_div_dble" :: hdual_tens_div_dble
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        real(PR), intent(in)                          :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
      
        res%x = qleft%x / iright
        res%dx1 = qleft%dx1 / iright
        res%dx2 = qleft%dx2 / iright
        res%dx1x2 = qleft%dx1x2 / iright
      
      end function hdual_tens_div_dble
      
      function hdual_tens_div_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_tens_div_int" :: hdual_tens_div_int
      
        implicit none
        TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
        integer, intent(in)                           :: iright
        TYPE(hyperdual), &
          dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res
      
        res%x = qleft%x / REAL(iright,PR)
        res%dx1 = qleft%dx1 / REAL(iright,PR)
        res%dx2 = qleft%dx2 / REAL(iright,PR)
        res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
          
      end function hdual_tens_div_int
      
      function dble_div_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_div_hdual" :: dble_div_hdual
      
        implicit none
        real(PR), intent(in)        :: xleft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
        TYPE(hyperdual)             :: inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = xleft * inv
      
      end function dble_div_hdual
      
      function int_div_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_div_hdual" :: int_div_hdual
      
        implicit none
        integer, intent(in)         :: ileft
        TYPE(hyperdual), intent(in) :: qright
        TYPE(hyperdual)             :: res
        TYPE(hyperdual)             :: inv
        
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = ileft * inv
      
      end function int_div_hdual    

      function dble_array_div_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_array_div_hdual" :: dble_array_div_hdual
      
        implicit none
        real(PR), dimension(:), intent(in)      :: xleft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), dimension(size(xleft)) :: res
        TYPE(hyperdual)                         :: inv

        inv = hdual_pow_dble(qright, -1.0_PR)
        res = xleft * inv
      
      end function dble_array_div_hdual
      
      function int_array_div_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_array_div_hdual" :: int_array_div_hdual
      
        implicit none
        integer, dimension(:), intent(in)         :: ileft
        TYPE(hyperdual), intent(in)               :: qright
        TYPE(hyperdual), dimension(size(ileft,1)) :: res
        TYPE(hyperdual)                           :: inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = ileft * inv
      
      end function int_array_div_hdual    

      function dble_matrix_div_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_matrix_div_hdual" :: dble_matrix_div_hdual
      
        implicit none
        real(PR), dimension(:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)           :: qright
        TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
        TYPE(hyperdual)                              :: inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = xleft * inv
      
      end function dble_matrix_div_hdual
      
      function int_matrix_div_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_matrix_div_hdual" :: int_matrix_div_hdual
      
        implicit none
        integer, dimension(:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)         :: qright
        TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
        TYPE(hyperdual)                              :: inv
      
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = ileft * inv
      
      end function int_matrix_div_hdual       

      function dble_tens_div_hdual(xleft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_tens_div_hdual" :: dble_tens_div_hdual
      
        implicit none
        real(PR), dimension(:,:,:), intent(in)  :: xleft
        TYPE(hyperdual), intent(in)             :: qright
        TYPE(hyperdual), &
          dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
        TYPE(hyperdual)                              :: inv
    
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = xleft * inv
      
      end function dble_tens_div_hdual
        
      function int_tens_div_hdual(ileft, qright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "int_tens_div_hdual" :: int_tens_div_hdual
      
        implicit none
        integer, dimension(:,:,:), intent(in) :: ileft
        TYPE(hyperdual), intent(in)           :: qright
        TYPE(hyperdual), &
          dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
        TYPE(hyperdual)                              :: inv
    
        inv = hdual_pow_dble(qright, -1.0_PR)
        res = ileft * inv
      
      end function int_tens_div_hdual


      !----- POW operator (**)
      function hdual_pow_hdual(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_pow_hdual" :: hdual_pow_hdual
          
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res

          res = hdexp(qright*hdlog(qleft))

        end function

      function hdual_pow_dble(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_pow_dble" :: hdual_pow_dble
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        real(PR), intent(in)        :: iright
        real(PR)                    :: xval
        real(PR)                    :: tol
        real(PR)                    :: deriv
        TYPE(hyperdual)             :: res

        xval = qleft%x 
        tol = 1.0e-15
        if (abs(xval) < tol) then
          
          if (xval >= 0) then 
            xval = tol
          endif 

          if (xval < 0) then
            xval = -tol
          endif

        endif 
        deriv = iright * xval**(iright - 1.0_PR)
        
        res%x = qleft%x**iright
        res%dx1 = qleft%dx1 * deriv
        res%dx2 = qleft%dx2 * deriv
        res%dx1x2 = qleft%dx1x2 * deriv + iright * (iright - 1.0_PR) * qleft%dx1 * qleft%dx2 * xval**(iright - 2.0_PR)
      
      end function hdual_pow_dble
      

      function hdual_pow_int(qleft, iright) result(res)
      !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_pow_int" :: hdual_pow_int
      
        implicit none
        TYPE(hyperdual), intent(in) :: qleft
        integer, intent(in)         :: iright
        real(PR)                    :: xval
        real(PR)                    :: tol
        real(PR)                    :: deriv
        TYPE(hyperdual)             :: res

        xval = qleft%x 
        tol = 1.0e-15
        if (abs(xval) < tol) then
          
          if (xval >= 0) then 
            xval = tol
          endif 

          if (xval < 0) then
            xval = -tol
          endif

        endif 
        deriv = iright * xval**(iright - 1.0_PR)
        
        res%x = qleft%x**iright
        res%dx1 = qleft%dx1 * deriv
        res%dx2 = qleft%dx2 * deriv
        res%dx1x2 = qleft%dx1x2 * deriv + iright * (iright - 1.0_PR) * qleft%dx1 * qleft%dx2 * xval**(iright - 2.0_PR)
      end function hdual_pow_int


    !----------------------------------------------------------------!
    !                     INTRINSIC FUNCTIONS                        !
    !----------------------------------------------------------------!

    !----- DBLE (conversion to double)
      function dble_hdual(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_hdual" :: dble_hdual
          
          implicit none
          TYPE(hyperdual), intent(in)   :: X_in
          real(PR)                      :: X_out

          X_out = DBLE(X_in%x) 
        
        end function dble_hdual

        function dble_hdual_array(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_hdual_array" :: dble_hdual_array
          
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: X_in
          real(PR), dimension(size(X_in))           :: X_out
          integer :: i

          do i = 1, size(X_in)
            X_out(i) = DBLE(X_in(i)%x)
          enddo

        end function dble_hdual_array

        function dble_hdual_matrix(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dble_hdual_matrix" :: dble_hdual_matrix
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)     :: X_in
          real(PR), dimension(size(X_in,1), size(X_in,2)) :: X_out
          integer :: i, j

          do i = 1, size(X_in,1)
            do j = 1, size(X_in,2)
              X_out(i,j) = DBLE(X_in(i,j)%x)
            enddo
          enddo

        end function dble_hdual_matrix


      !----- ABS (absolute value)
        function abs_hdual(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "abs_hdual" :: abs_hdual
          
          implicit none
          TYPE(hyperdual), intent(in)   :: X_in
          TYPE(hyperdual)               :: X_out

          X_out = sign(1.0_PR, REAL(X_in%x,PR)) * X_in 
        
        end function abs_hdual

        function abs_hdual_array(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "abs_hdual_array" :: abs_hdual_array
          
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: X_in
          TYPE(hyperdual), dimension(size(X_in))    :: X_out
          integer :: i

          do i = 1, size(X_in)
            X_out(i) = abs(X_in(i))
          enddo

        end function abs_hdual_array

        function abs_hdual_matrix(X_in) result(X_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "abs_hdual_matrix" :: abs_hdual_matrix
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)           :: X_in
          TYPE(hyperdual), dimension(size(X_in,1), size(X_in,2)):: X_out
          integer :: i, j

          do i = 1, size(X_in,1)
            do j = 1, size(X_in,2)
              X_out(i,j) = abs(X_in(i,j))
            enddo
          enddo

        end function abs_hdual_matrix


      !----- SIGN
        function sign_hdual_hdual(val_in, sign_in) result(val_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "sign_hdual_hdual" :: sign_hdual_hdual
          
          implicit none
          TYPE(hyperdual), intent(in)   :: val_in, sign_in
          TYPE(hyperdual)               :: val_out

          if (REAL(sign_in%x,PR).GE.(0.0_PR)) then
            val_out = abs(val_in)
          else
            val_out = abs(val_in) * (-1.0_PR)
          endif

        end function


      !----- MAX
        function max_hdual_hdual(q1, q2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "max_hdual_hdual" :: max_hdual_hdual

          implicit none
          TYPE(hyperdual), intent(in) :: q1
          TYPE(hyperdual), intent(in) :: q2
          TYPE(hyperdual)             :: q_out

          if (q1.GE.q2) then
            q_out = q1
          else
            q_out = q2
          endif

        end function

        function max_hdual_dble(q1, x2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "max_hdual_dble" :: max_hdual_dble

          implicit none
          TYPE(hyperdual), intent(in) :: q1
          real(PR), intent(in)        :: x2
          TYPE(hyperdual)             :: q_out

          if (q1.GE.x2) then
            q_out = q1
          else
            q_out = x2
          endif

        end function

        function max_dble_hdual(x1, q2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "max_dble_hdual" :: max_dble_hdual

          implicit none
          real(PR), intent(in)        :: x1
          TYPE(hyperdual), intent(in) :: q2
          TYPE(hyperdual)             :: q_out

          if (x1.GE.q2) then
            q_out = x1
          else
            q_out = q2
          endif

        end function


      !----- MIN
        function min_hdual_hdual(q1, q2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "min_hdual_hdual" :: min_hdual_hdual

          implicit none
          TYPE(hyperdual), intent(in) :: q1
          TYPE(hyperdual), intent(in) :: q2
          TYPE(hyperdual)             :: q_out

          if (q1.LE.q2) then
            q_out = q1
          else
            q_out = q2
          endif

        end function

        function min_hdual_dble(q1, x2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "min_hdual_dble" :: min_hdual_dble

          implicit none
          TYPE(hyperdual), intent(in) :: q1
          real(PR), intent(in)        :: x2
          TYPE(hyperdual)             :: q_out

          if (q1.LE.x2) then
            q_out = q1
          else
            q_out = x2
          endif

        end function

        function min_dble_hdual(x1, q2) result(q_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "min_dble_hdual" :: min_dble_hdual

          implicit none
          real(PR), intent(in)        :: x1
          TYPE(hyperdual), intent(in) :: q2
          TYPE(hyperdual)             :: q_out

          if (x1.LE.q2) then
            q_out = x1
          else
            q_out = q2
          endif

        end function
        

      !----- Maxval
        function maxval_hdual_array(X_in) result(val_out)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "maxval_hdual_array" :: maxval_hdual_array

          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: X_in
          TYPE(hyperdual)                           :: val_out
          integer :: i

          val_out = X_in(1)
          do i = 1, size(X_in)
            val_out = max(val_out, X_in(i))
          enddo

        end function


        !----- Matmul
      function matmul_hdual_array_hdual_matrix(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_array_hdual_matrix" :: matmul_hdual_array_hdual_matrix
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qleft))     :: res

          res%x     = matmul(qleft%x,qright%x)
          res%dx1   = matmul(qleft%x,qright%dx1) + matmul(qleft%dx1,qright%x)
          res%dx2   = matmul(qleft%x,qright%dx2) + matmul(qleft%dx2,qright%x)
          res%dx1x2 = matmul(qleft%x,qright%dx1x2) + matmul(qleft%dx1,qright%dx2) + matmul(qleft%dx2,qright%dx1) &
          + matmul(qleft%dx1x2,qright%x)

        end function matmul_hdual_array_hdual_matrix

        function matmul_hdual_array_dble_matrix(qleft, xright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_array_dble_matrix" :: matmul_hdual_array_dble_matrix
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), dimension(:,:), intent(in)      :: xright
          TYPE(hyperdual), dimension(size(qleft))   :: res

          res%x     = matmul(qleft%x,xright)
          res%dx1   = matmul(qleft%dx1,xright)
          res%dx2   = matmul(qleft%dx2,xright)
          res%dx1x2 = matmul(qleft%dx1x2,xright)

        end function matmul_hdual_array_dble_matrix

        function matmul_hdual_matrix_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_matrix_hdual_array" :: matmul_hdual_matrix_hdual_array
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          TYPE(hyperdual), dimension(:), intent(in)   :: qright
          TYPE(hyperdual), dimension(size(qleft,1))   :: res

          res%x     = matmul(qleft%x,qright%x)
          res%dx1   = matmul(qleft%x,qright%dx1) + matmul(qleft%dx1,qright%x)
          res%dx2   = matmul(qleft%x,qright%dx2) + matmul(qleft%dx2,qright%x)
          res%dx1x2 = matmul(qleft%x,qright%dx1x2) + matmul(qleft%dx1,qright%dx2) + matmul(qleft%dx2,qright%dx1) &
          + matmul(qleft%dx1x2,qright%x)

        end function matmul_hdual_matrix_hdual_array 

        function matmul_hdual_matrix_hdual_matrix(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_matrix_hdual_matrix" :: matmul_hdual_matrix_hdual_matrix
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qleft,1), size(qright,2)) :: res

          res%x     = matmul(qleft%x,qright%x)
          res%dx1   = matmul(qleft%x,qright%dx1) + matmul(qleft%dx1,qright%x)
          res%dx2   = matmul(qleft%x,qright%dx2) + matmul(qleft%dx2,qright%x)
          res%dx1x2 = matmul(qleft%x,qright%dx1x2) + matmul(qleft%dx1,qright%dx2) + matmul(qleft%dx2,qright%dx1) &
          + matmul(qleft%dx1x2,qright%x)

        end function

        function matmul_hdual_matrix_dble_array(qleft, xright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_matrix_dble_array" :: matmul_hdual_matrix_dble_array
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), dimension(:), intent(in)          :: xright
          TYPE(hyperdual), dimension(size(qleft,1))   :: res

          res%x     = matmul(qleft%x,xright)
          res%dx1   = matmul(qleft%dx1,xright)
          res%dx2   = matmul(qleft%dx2,xright)
          res%dx1x2 = matmul(qleft%dx1x2,xright)

        end function matmul_hdual_matrix_dble_array 

        function matmul_hdual_matrix_dble_matrix(qleft, xright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_hdual_matrix_dble_matrix" :: matmul_hdual_matrix_dble_matrix
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), dimension(:,:), intent(in)        :: xright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res

          res%x     = matmul(qleft%x,xright)
          res%dx1   = matmul(qleft%dx1,xright)
          res%dx2   = matmul(qleft%dx2,xright)
          res%dx1x2 = matmul(qleft%dx1x2,xright)

        end function matmul_hdual_matrix_dble_matrix 

        function matmul_dble_array_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_dble_array_hdual_matrix" :: matmul_dble_array_hdual_matrix
        
          implicit none
          real(PR), dimension(:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(xleft))     :: res

          res%x     = matmul(xleft,qright%x)
          res%dx1   = matmul(xleft,qright%dx1)
          res%dx2   = matmul(xleft,qright%dx2)
          res%dx1x2 = matmul(xleft,qright%dx1x2)

        end function matmul_dble_array_hdual_matrix 

        function matmul_dble_matrix_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_dble_matrix_hdual_array" :: matmul_dble_matrix_hdual_array
        
          implicit none
          real(PR), dimension(:,:), intent(in)      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res

          res%x     = matmul(xleft,qright%x)
          res%dx1   = matmul(xleft,qright%dx1)
          res%dx2   = matmul(xleft,qright%dx2)
          res%dx1x2 = matmul(xleft,qright%dx1x2)

        end function matmul_dble_matrix_hdual_array 

        function matmul_dble_matrix_hdual_matrix(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "matmul_dble_matrix_hdual_matrix" :: matmul_dble_matrix_hdual_matrix
        
          implicit none
          real(PR), dimension(:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2))  :: res

          res%x     = matmul(xleft,qright%x)
          res%dx1   = matmul(xleft,qright%dx1)
          res%dx2   = matmul(xleft,qright%dx2)
          res%dx1x2 = matmul(xleft,qright%dx1x2)

        end function matmul_dble_matrix_hdual_matrix 

        !----- Dot Product
      function dot_product_hdual_array_hdual_array(qleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dot_product_hdual_array_hdual_array" :: dot_product_hdual_array_hdual_array
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual)                           :: res

          res%x     = dot_product(qleft%x,qright%x)
          res%dx1   = dot_product(qleft%x,qright%dx1) + dot_product(qleft%dx1,qright%x)
          res%dx2   = dot_product(qleft%x,qright%dx2) + dot_product(qleft%dx2,qright%x)
          res%dx1x2 = dot_product(qleft%x,qright%dx1x2) + dot_product(qleft%dx1,qright%dx2) + dot_product(qleft%dx2,qright%dx1) &
          + dot_product(qleft%dx1x2,qright%x)

        end function dot_product_hdual_array_hdual_array

        function dot_product_hdual_array_dble_array(qleft, xright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dot_product_hdual_array_dble_array" :: dot_product_hdual_array_dble_array
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), dimension(:), intent(in)        :: xright
          TYPE(hyperdual)                           :: res

          res%x     = dot_product(qleft%x,xright)
          res%dx1   = dot_product(qleft%dx1,xright)
          res%dx2   = dot_product(qleft%dx2,xright)
          res%dx1x2 = dot_product(qleft%dx1x2,xright)

        end function dot_product_hdual_array_dble_array
        
        function dot_product_dble_array_hdual_array(xleft, qright) result(res)
        !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "dot_product_dble_array_hdual_array" :: dot_product_dble_array_hdual_array
        
          implicit none
          real(PR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual)                           :: res

          res%x     = dot_product(xleft,qright%x)
          res%dx1   = dot_product(xleft,qright%dx1)
          res%dx2   = dot_product(xleft,qright%dx2)
          res%dx1x2 = dot_product(xleft,qright%dx1x2)

        end function dot_product_dble_array_hdual_array

        ! Hyperdual Sqrt
        function hdsqrt(q) result(qsqrt)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdsqrt" :: hdsqrt
          
          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qsqrt
  
          qsqrt = hdual_pow_dble(q, 0.5_PR)

        end function hdsqrt

        ! Hyperdual exponential
        function hdexp(q) result(qexp)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdexp" :: hdexp
          
          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qexp
          real(PR)                    :: deriv
          
          deriv      = exp(q%x)
          qexp%x     = deriv
          qexp%dx1   = deriv * q%dx1
          qexp%dx2   = deriv * q%dx2
          qexp%dx1x2 = deriv * (q%dx1 * q%dx2 + q%dx1x2)

        end function hdexp

        function hdlog(q) result(qlog) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdlog" :: hdlog

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qlog
          real(PR)                    :: deriv1
          real(PR)                    :: deriv2

          deriv1     = q%dx1 / q%x
          deriv2     = q%dx2 / q%x
          qlog%x     = log(q%x)
          qlog%dx1   = deriv1
          qlog%dx2   = deriv2
          qlog%dx1x2 = q%dx1x2 / q%x - (deriv1 * deriv2)

        end function hdlog

        function hdcos(q) result(qcos)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcos" :: hdcos
          
          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qcos
          real(PR)                    :: deriv
          real(PR)                    :: funval

          deriv      = -sin(q%x)
          funval     = cos(q%x)
          qcos%x     = funval
          qcos%dx1   = deriv * q%dx1
          qcos%dx2   = deriv * q%dx2
          qcos%dx1x2 = -funval * q%dx1 * q%dx2 + deriv * q%dx1x2

        end function hdcos

        function hdsin(q) result(qsin)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdsin" :: hdsin

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qsin
          real(PR)                    :: deriv
          real(PR)                    :: funval

          deriv =  cos(q%x)
          funval = sin(q%x)
          qsin%x = funval
          qsin%dx1 = deriv * q%dx1
          qsin%dx2 = deriv * q%dx2
          qsin%dx1x2 = -funval * q%dx1 * q%dx2 + deriv * q%dx1x2
          
        end function hdsin

          function hdtan(q) result(qtan) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdtan" :: hdtan

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qtan
          real(PR)                    :: deriv
          real(PR)                    :: funval

          funval     = tan(q%x)
          deriv      = funval * funval + 1.0_PR
          qtan%x     = funval
          qtan%dx1   = deriv * q%dx1
          qtan%dx2   = deriv * q%dx2
          qtan%dx1x2 = (2.0_PR * funval * deriv) * q%dx1 * q%dx2 + deriv * q%dx1x2

          end function hdtan 
        
          function hdcosh(q) result(qcosh) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcosh" :: hdcosh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qcosh
          real(PR)                    :: deriv
          real(PR)                    :: funval

          funval      = cosh(q%x)
          deriv       = sinh(q%x)
          qcosh%x     = funval
          qcosh%dx1   = deriv * q%dx1
          qcosh%dx2   = deriv * q%dx2
          qcosh%dx1x2 = funval * q%dx1 * q%dx2 + deriv * q%dx1x2

          end function hdcosh

          function hdsinh(q) result(qsinh) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcosh" :: hdcosh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qsinh
          real(PR)                    :: deriv
          real(PR)                    :: funval

          funval      = sinh(q%x)
          deriv       = cosh(q%x)
          qsinh%x     = funval
          qsinh%dx1   = deriv * q%dx1
          qsinh%dx2   = deriv * q%dx2
          qsinh%dx1x2 = funval * q%dx1 * q%dx2 + deriv * q%dx1x2

          end function hdsinh

          function hdtanh(q) result(qtanh) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdtanh" :: hdtanh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qtanh
          real(PR)                    :: funval
          real(PR)                    :: deriv

          funval = tanh(q%x)
          deriv = 1.0_PR / cosh(q%x)**2
          qtanh%x     = funval
          qtanh%dx1   = deriv * q%dx1
          qtanh%dx2   = deriv * q%dx2
          qtanh%dx1x2 = -2.0_PR * funval * deriv * q%dx1 * q%dx2 + deriv * q%dx1x2

          end function hdtanh

          function hdacos(q) result(qacos) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcosh" :: hdcosh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qacos
          real(PR)                    :: deriv1
          real(PR)                    :: deriv2
          real(PR)                    :: funval

          funval      = acos(q%x)
          
          ! 1st and 2nd derivative of acos   -1 <= x <= 1
          deriv1      = -1.0_PR / sqrt(1.0_PR - q%x**2) 
          deriv2      = q%x / (1.0_PR - q%x**2)**(1.5_PR) 

          qacos%x     = funval
          qacos%dx1   = deriv1 * q%dx1
          qacos%dx2   = deriv1 * q%dx2
          qacos%dx1x2 = deriv2 * q%dx1 * q%dx2 + deriv1 * q%dx1x2

          end function hdacos 

          function hdasin(q) result(qasin) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcosh" :: hdcosh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qasin
          real(PR)                    :: deriv1
          real(PR)                    :: deriv2
          real(PR)                    :: funval

          funval      = asin(q%x)
          
          ! 1st and 2nd derivative of asin  -1 <= x <= 1
          deriv1      = 1.0_PR / sqrt(1.0_PR - q%x**2) 
          deriv2      = q%x / (1.0_PR - q%x**2)**(1.5_PR) 

          qasin%x     = funval
          qasin%dx1   = deriv1 * q%dx1
          qasin%dx2   = deriv1 * q%dx2
          qasin%dx1x2 = deriv2 * q%dx1 * q%dx2 + deriv1 * q%dx1x2

          end function hdasin

          function hdatan(q) result(qatan) 
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdcosh" :: hdcosh

          implicit none
          TYPE(hyperdual), intent(in) :: q
          TYPE(hyperdual)             :: qatan
          real(PR)                    :: deriv1
          real(PR)                    :: deriv2
          real(PR)                    :: funval

          funval      = atan(q%x)
          
          ! 1st and 2nd derivative of atan
          deriv1      = 1.0_PR / (1.0_PR + q%x**2) 
          deriv2      = -2.0_PR * q%x / (1.0_PR + q%x**2)**(2.0_PR) 

          qatan%x     = funval
          qatan%dx1   = deriv1 * q%dx1
          qatan%dx2   = deriv1 * q%dx2
          qatan%dx1x2 = deriv2 * q%dx1 * q%dx2 + deriv1 * q%dx1x2

          end function hdatan

        end module HDMod



            








          




      


      