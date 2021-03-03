!*************************************************************************
!* Hyperdual Number Module (HDMod) of Fortran Codes 
!*----------------------------------------------------------------

Module HDMod

  implicit none
  
  public
  
      integer, parameter              :: SPR = KIND(1.0)      ! Single Precision
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
        module procedure hdual_from_dble
      end interface
  
      !----- Assignment
      interface assignment(=)
        module procedure hdual_assign_hdual, hdual_assign_dble, hdual_assign_int, & 
          hdual_array_assign_hdual, hdual_array_assign_hdual_array, hdual_array_assign_dble, & 
          hdual_array_assign_dble_array, hdual_array_assign_int, hdual_array_assign_int_array, &
          hdual_matrix_assign_hdual, hdual_matrix_assign_hdual_matrix, hdual_matrix_assign_dble, &
          hdual_matrix_assign_dble_matrix, hdual_matrix_assign_int, hdual_matrix_assign_int_matrix, & 
          hdual_tens_assign_hdual, hdual_tens_assign_hdual_tens, hdual_tens_assign_dble, & 
          hdual_tens_assign_dble_tens, hdual_tens_assign_int, hdual_tens_assign_int_tens, hdual_4d_assign_hdual_4d, &
          hdual_4d_assign_dble, hdual_5d_assign_dble
      end interface 
    
      !----- Comparison operators      
      interface operator(==)
        module procedure hdual_eq_hdual, hdual_eq_dble, hdual_eq_dble_SPR, dble_eq_hdual, dble_eq_hdual_SPR
      end interface
  
      interface operator(/=)
        module procedure hdual_ne_hdual, hdual_ne_dble, hdual_ne_dble_SPR, dble_ne_hdual, dble_ne_hdual_SPR
      end interface
  
      interface operator(>)
        module procedure hdual_gt_hdual, hdual_gt_dble, hdual_gt_dble_SPR, dble_gt_hdual, dble_gt_hdual_SPR
      end interface
    
      interface operator(>=)
        module procedure hdual_ge_hdual, hdual_ge_dble, hdual_ge_dble_SPR, dble_ge_hdual, dble_ge_hdual_SPR
      end interface
    
      interface operator(<)
        module procedure hdual_lt_hdual, hdual_lt_dble, hdual_lt_dble_SPR, dble_lt_hdual, dble_lt_hdual_SPR
      end interface
    
      interface operator(<=)
        module procedure hdual_le_hdual, hdual_le_dble, hdual_le_dble_SPR, dble_le_hdual, dble_le_hdual_SPR
      end interface
  
  
      interface operator (+)
          module procedure hdual_plus_hdual, hdual_plus_hdual_array, hdual_plus_hdual_matrix, &
              hdual_plus_hdual_tens, hdual_plus_dble, hdual_plus_dble_SPR, hdual_plus_dble_array, &
              hdual_plus_dble_array_SPR, hdual_plus_dble_matrix, hdual_plus_dble_matrix_SPR, hdual_plus_dble_tens, &
              hdual_plus_dble_tens_SPR, hdual_plus_int, hdual_plus_int_array, hdual_plus_int_matrix, hdual_plus_int_tens, &
              hdual_array_plus_hdual, hdual_array_plus_hdual_array, hdual_array_plus_dble, &
              hdual_array_plus_dble_SPR, hdual_array_plus_dble_array, hdual_array_plus_dble_array_SPR, &
              hdual_array_plus_int, hdual_array_plus_int_array, &
              hdual_matrix_plus_hdual, hdual_matrix_plus_hdual_matrix, hdual_matrix_plus_dble, &
              hdual_matrix_plus_dble_SPR, hdual_matrix_plus_dble_matrix, hdual_matrix_plus_dble_matrix_SPR, &
              hdual_matrix_plus_int, hdual_matrix_plus_int_matrix, hdual_tens_plus_hdual, hdual_tens_plus_hdual_tens, &
              hdual_tens_plus_dble, hdual_tens_plus_dble_SPR, hdual_tens_plus_dble_tens, hdual_tens_plus_dble_tens_SPR, &
              hdual_tens_plus_int, hdual_tens_plus_int_tens, hdual_4d_plus_hdual, hdual_4d_plus_hdual_4d, &
              hdual_4d_plus_dble, hdual_4d_plus_dble_4d, hdual_4d_plus_dble_4d_SPR, hdual_4d_plus_int, hdual_4d_plus_int_4d, &
              dble_plus_hdual, dble_plus_hdual_SPR, dble_plus_hdual_array, &
              dble_plus_hdual_array_SPR, dble_plus_hdual_matrix, dble_plus_hdual_matrix_SPR, dble_plus_hdual_tens, &
              dble_plus_hdual_tens_SPR, dble_array_plus_hdual, dble_array_plus_hdual_SPR, dble_array_plus_hdual_array, &
              dble_array_plus_hdual_array_SPR, dble_matrix_plus_hdual, dble_matrix_plus_hdual_SPR, dble_matrix_plus_hdual_matrix, &
              dble_matrix_plus_hdual_matrix_SPR, dble_tens_plus_hdual, dble_tens_plus_hdual_SPR, dble_tens_plus_hdual_tens, &
              dble_tens_plus_hdual_tens_SPR, dble_4d_plus_hdual, dble_4d_plus_hdual_SPR, dble_4d_plus_hdual_4d, &
              dble_4d_plus_hdual_4d_SPR, int_plus_hdual, int_plus_hdual_array, int_plus_hdual_matrix, &
              int_plus_hdual_tens, int_array_plus_hdual, int_array_plus_hdual_array, &
              int_matrix_plus_hdual, int_matrix_plus_hdual_matrix, &
              int_tens_plus_hdual, int_tens_plus_hdual_tens
        end interface
  
      interface operator(-)
        module procedure hdual_minus_hdual, hdual_minus_hdual_array, hdual_minus_hdual_matrix, &
            hdual_minus_hdual_tens, hdual_minus_dble, hdual_minus_dble_SPR, hdual_minus_dble_array, &
            hdual_minus_dble_array_SPR, hdual_minus_dble_matrix, hdual_minus_dble_matrix_SPR, &
            hdual_minus_dble_tens, hdual_minus_dble_tens_SPR, hdual_minus_int, hdual_minus_int_array, &
            hdual_minus_int_matrix, hdual_minus_int_tens, hdual_array_minus_hdual, hdual_array_minus_hdual_array, &
            hdual_array_minus_dble, hdual_array_minus_dble_SPR, hdual_array_minus_dble_array, &
            hdual_array_minus_dble_array_SPR, hdual_array_minus_int, hdual_array_minus_int_array, &
            hdual_matrix_minus_hdual, hdual_matrix_minus_hdual_matrix, &
            hdual_matrix_minus_dble, hdual_matrix_minus_dble_SPR, hdual_matrix_minus_dble_matrix, &
            hdual_matrix_minus_dble_matrix_SPR, hdual_matrix_minus_int, hdual_matrix_minus_int_matrix, &
            hdual_tens_minus_hdual, hdual_tens_minus_hdual_tens, &
            hdual_tens_minus_dble, hdual_tens_minus_dble_SPR, hdual_tens_minus_dble_tens, &
            hdual_tens_minus_dble_tens_SPR, hdual_tens_minus_int, hdual_tens_minus_int_tens, &
            dble_minus_hdual, dble_minus_hdual_SPR, dble_minus_hdual_array, dble_minus_hdual_array_SPR, &
            dble_minus_hdual_matrix, dble_minus_hdual_matrix_SPR, dble_minus_hdual_tens, dble_minus_hdual_tens_SPR, &
            dble_array_minus_hdual, dble_array_minus_hdual_SPR, dble_array_minus_hdual_array, &
            dble_array_minus_hdual_array_SPR, dble_matrix_minus_hdual, dble_matrix_minus_hdual_SPR, &
            dble_matrix_minus_hdual_matrix, dble_matrix_minus_hdual_matrix_SPR, dble_tens_minus_hdual, &
            dble_tens_minus_hdual_SPR, dble_tens_minus_hdual_tens, dble_tens_minus_hdual_tens_SPR, &
            int_minus_hdual, int_minus_hdual_array, &
            int_minus_hdual_matrix, int_minus_hdual_tens, &
            int_array_minus_hdual, int_array_minus_hdual_array, &
            int_matrix_minus_hdual, int_matrix_minus_hdual_matrix, &
            int_tens_minus_hdual, int_tens_minus_hdual_tens, minus_hdual, &
            minus_hdual_array, minus_hdual_matrix, minus_hdual_tens
       end interface
  
  
      
      interface operator(*)
          module procedure hdual_mul_hdual, hdual_mul_hdual_array, hdual_mul_hdual_matrix, &
              hdual_mul_hdual_tens, hdual_mul_dble, hdual_mul_dble_SPR, hdual_mul_dble_array, &
              hdual_mul_dble_array_SPR, hdual_mul_dble_matrix, hdual_mul_dble_matrix_SPR, hdual_mul_dble_tens, &
              hdual_mul_dble_tens_SPR, hdual_mul_int, hdual_mul_int_array, hdual_mul_int_matrix, hdual_mul_int_tens, & 
              hdual_array_mul_hdual, hdual_array_mul_dble, hdual_array_mul_dble_SPR, hdual_array_mul_int, &
              hdual_matrix_mul_hdual, hdual_matrix_mul_dble, hdual_matrix_mul_dble_SPR, hdual_matrix_mul_int, &
              hdual_tens_mul_hdual, hdual_tens_mul_dble, hdual_tens_mul_dble_SPR, hdual_tens_mul_int, &
              dble_mul_hdual, dble_mul_hdual_SPR, dble_mul_hdual_array, dble_mul_hdual_array_SPR, dble_mul_hdual_matrix, &
              dble_mul_hdual_matrix_SPR, dble_mul_hdual_tens, dble_mul_hdual_tens_SPR, dble_array_mul_hdual, &
              dble_array_mul_hdual_SPR, dble_matrix_mul_hdual, dble_matrix_mul_hdual_SPR, &
              dble_tens_mul_hdual, dble_tens_mul_hdual_SPR, &
              int_mul_hdual, int_mul_hdual_array, int_mul_hdual_matrix, &
              int_mul_hdual_tens, int_array_mul_hdual, int_matrix_mul_hdual, &
              int_tens_mul_hdual, hdual_array_mul_hdual_array, &
              hdual_array_mul_dble_array ,dble_array_mul_hdual_array, &
              hdual_array_mul_dble_array_SPR ,dble_array_mul_hdual_array_SPR, hdual_matrix_mul_hdual_matrix, &
              hdual_matrix_mul_dble_matrix, hdual_matrix_mul_dble_matrix_SPR, dble_matrix_mul_hdual_matrix, &
              dble_matrix_mul_hdual_matrix_SPR, hdual_tens_mul_hdual_tens, &
              hdual_tens_mul_dble_tens, hdual_tens_mul_dble_tens_SPR, dble_tens_mul_hdual_tens, &
              dble_tens_mul_hdual_tens_SPR
        end interface
  
  
        interface operator(/)
          module procedure hdual_div_hdual, hdual_div_dble, hdual_div_dble_SPR, hdual_div_int, &
              hdual_array_div_hdual, hdual_array_div_dble, hdual_array_div_dble_SPR, hdual_array_div_int, &
              hdual_matrix_div_hdual, hdual_matrix_div_dble, hdual_matrix_div_dble_SPR, hdual_matrix_div_int, &
              hdual_tens_div_hdual, hdual_tens_div_dble, hdual_tens_div_dble_SPR, hdual_tens_div_int, &
              dble_div_hdual, dble_div_hdual_SPR, dble_div_hdual_tens_SPR, int_div_hdual, &
              dble_array_div_hdual, dble_array_div_hdual_SPR, int_array_div_hdual, &
              dble_matrix_div_hdual, dble_matrix_div_hdual_SPR, int_matrix_div_hdual, &
              dble_tens_div_hdual, dble_tens_div_hdual_SPR, int_tens_div_hdual, hdual_array_div_dble_array, &
              hdual_array_div_dble_array_SPR, hdual_array_div_hdual_array
        end interface
  
        ! Probably need more elemental divisions
  
        interface operator(**)
          module procedure hdual_pow_hdual, hdual_pow_int, hdual_pow_dble, hdual_pow_dble_SPR, dble_pow_hdual, &
            hdual_array_pow_dble, hdual_array_pow_dble_SPR
        end interface
  
  
  
        !----- Intrinsic functions
        interface dble
          module procedure dble_hdual, dble_hdual_array, dble_hdual_matrix
        end interface
        
        interface abs
          module procedure abs_hdual, abs_hdual_array, abs_hdual_matrix
        end interface
        
        interface sign
          module procedure sign_hdual_hdual, sign_hdual_dble, sign_dble_hdual
        end interface
  
        interface max
          module procedure max_hdual_hdual, max_hdual_dble, max_hdual_dble_SPR, max_dble_hdual, &
            max_hdual_array_dble, max_hdual_matrix_dble, max_hdual_matrix_dble_SPR, &
            max_hdual_tens_dble, max_hdual_tens_dble_SPR, &
            max_hdual_4d_dble, max_hdual_4d_dble_SPR, max_hdual_array_hdual_array
        end interface
  
        interface min
          module procedure min_hdual_hdual, min_hdual_dble, min_hdual_dble_SPR, min_dble_hdual, min_dble_hdual_SPR, &
            min_hdual_four, min_hdual_five
        end interface
  
        interface maxval
          module procedure maxval_hdual_array
        end interface

        interface minval
          module procedure minval_hdual_4d
        end interface
  
        interface dot_product
          module procedure &
            dot_product_hdual_array_hdual_array, dot_product_hdual_array_dble_array, &
            dot_product_hdual_array_dble_array_SPR, dot_product_dble_array_hdual_array, & 
            dot_product_dble_array_hdual_array_SPR
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
  
        interface log10
          module procedure hdlog10
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
  
        interface nint
          module procedure hdnint
        end interface 
  
        interface sum
          module procedure hdsum, hdsum_2d, hdsum_3d, hdsum_4d, hdsum_mask, hdsum_2d_dim, &
                           hdsum_3d_dim, hdsum_4d_dim, hdsum_5d_dim
        end interface 
  
        ! interface real
        !   module procedure real_hdual
        ! end interface
  
        interface erf
          module procedure hderf
        end interface
  
        interface erfc
          module procedure hderfc
        end interface
        
  
      CONTAINS
  
      !================================================================!
      !                Overloading hdual functions                     !
      !================================================================!
  
      !----------------------------------------------------------------!
      !                         CONSTRUCTOR                            !
      !----------------------------------------------------------------!
  
  
        function hdual_from_dble(x11,x12,x21,x22) result(q)
            
          implicit none
          real(PR), intent(in)  :: x11, x12, x21, x22
          TYPE(hyperdual)       :: q
          
          q%x = x11
          q%dx1 = x12
          q%dx2 = x21
          q%dx1x2 = x22
                
        end function hdual_from_dble
        
      !----------------------------------------------------------------!
      !                          ASSIGNMENT                            !
      !----------------------------------------------------------------!
  
  
        pure subroutine hdual_assign_hdual(qleft, qright) 
  
          implicit none
          TYPE(hyperdual), intent(out) :: qleft
          TYPE(hyperdual), intent(in)  :: qright
  
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
  
        end subroutine hdual_assign_hdual
        
  
        pure subroutine hdual_assign_dble(qleft, iright) 
            
          implicit none
          TYPE(hyperdual), intent(out)  :: qleft
          real(PR), intent(in)          :: iright
  
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
    
        end subroutine hdual_assign_dble
  
  
        subroutine hdual_assign_int(qleft, iright) 
          
          implicit none
          TYPE(hyperdual), intent(out)  :: qleft
          integer, intent(in)           :: iright
        
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
          
        end subroutine hdual_assign_int
  
  
        subroutine hdual_array_assign_hdual(qleft, qright)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          TYPE(hyperdual), intent(in)                 :: qright
  
          qleft%x = qright%x
          qleft%dx1 = qright%dx1
          qleft%dx2 = qright%dx2
          qleft%dx1x2 = qright%dx1x2
    
        end subroutine hdual_array_assign_hdual
  
  
        subroutine hdual_array_assign_hdual_array(qleft, qright) 
          
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          TYPE(hyperdual), dimension(:), intent(in)   :: qright
                  
          qleft%x = qright%x
          qleft%dx1 = qright%dx1
          qleft%dx2 = qright%dx2
          qleft%dx1x2 = qright%dx1x2
                    
          end subroutine hdual_array_assign_hdual_array
          
  
        subroutine hdual_array_assign_dble(qleft, iright) 
                  
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          real(PR), intent(in)                        :: iright
                
          qleft%x = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
                  
        end subroutine hdual_array_assign_dble
  
  
        subroutine hdual_array_assign_dble_array(qleft, iright) 
            
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          real(PR), dimension(:), intent(in)          :: iright
                
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
                    
        end subroutine hdual_array_assign_dble_array
    
  
        subroutine hdual_array_assign_int(qleft, iright) 
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          integer, intent(in)                         :: iright
                  
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
            
        end subroutine hdual_array_assign_int
  
  
        subroutine hdual_array_assign_int_array(qleft, iright) 
                  
          implicit none
          TYPE(hyperdual), dimension(:), intent(out)  :: qleft
          integer, dimension(:), intent(in)           :: iright
                  
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
              
        end subroutine hdual_array_assign_int_array
  
  
        subroutine hdual_matrix_assign_hdual(qleft, qright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          TYPE(hyperdual), intent(in)                   :: qright
        
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
        
        end subroutine hdual_matrix_assign_hdual
  
  
        pure subroutine hdual_matrix_assign_hdual_matrix(qleft, qright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qright
        
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
        
        end subroutine hdual_matrix_assign_hdual_matrix
  
  
        subroutine hdual_matrix_assign_dble(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          real(PR), intent(in)                          :: iright
        
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_matrix_assign_dble
  
  
        subroutine hdual_matrix_assign_dble_matrix(qleft, iright) 
                
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          real(PR), dimension(:,:), intent(in)          :: iright
                
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
              
        end subroutine hdual_matrix_assign_dble_matrix
  
  
        subroutine hdual_matrix_assign_int(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          integer, intent(in)                           :: iright
        
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_matrix_assign_int
  
  
        subroutine hdual_matrix_assign_int_matrix(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(out)  :: qleft
          integer, dimension(:,:), intent(in)           :: iright
        
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_matrix_assign_int_matrix
  
  
        subroutine hdual_tens_assign_hdual(qleft, qright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
          TYPE(hyperdual), intent(in)                     :: qright
        
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
        
        end subroutine hdual_tens_assign_hdual
  
  
        subroutine hdual_tens_assign_hdual_tens(qleft, qright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qright
        
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
        
        end subroutine hdual_tens_assign_hdual_tens
  
  
        subroutine hdual_tens_assign_dble(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out) :: qleft
          real(PR), intent(in) :: iright
        
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_tens_assign_dble
  
  
        subroutine hdual_tens_assign_dble_tens(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
          real(PR), dimension(:,:,:), intent(in)          :: iright
        
          qleft%x     = iright
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_tens_assign_dble_tens
  
  
        subroutine hdual_tens_assign_int(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
          integer, intent(in)                             :: iright
        
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_tens_assign_int
  
  
        subroutine hdual_tens_assign_int_tens(qleft, iright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(out)  :: qleft
          integer, dimension(:,:,:), intent(in)           :: iright
        
          qleft%x     = REAL(iright, PR)
          qleft%dx1   = 0.0_PR
          qleft%dx2   = 0.0_PR
          qleft%dx1x2 = 0.0_PR
        
        end subroutine hdual_tens_assign_int_tens
  

        subroutine hdual_4d_assign_hdual_4d(qleft, qright) 
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(out)  :: qleft
          TYPE(hyperdual), dimension(:,:,:,:), intent(in)   :: qright
        
          qleft%x     = qright%x
          qleft%dx1   = qright%dx1
          qleft%dx2   = qright%dx2
          qleft%dx1x2 = qright%dx1x2
        
        end subroutine hdual_4d_assign_hdual_4d

        
        subroutine hdual_4d_assign_dble(qleft, xright)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(out) :: qleft
          REAL(PR), intent(in)                             :: xright
          qleft%x = xright
          qleft%dx1 = 0.0_PR
          qleft%dx2 = 0.0_PR
          qleft%dx1x2 = 0.0_PR
  
        end subroutine hdual_4d_assign_dble
  
  
        subroutine hdual_5d_assign_dble(qleft, xright)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:,:), intent(out) :: qleft
          REAL(PR), intent(in)                               :: xright
          qleft%x = xright
          qleft%dx1 = 0.0_PR
          qleft%dx2 = 0.0_PR
          qleft%dx1x2 = 0.0_PR
  
        end subroutine hdual_5d_assign_dble
  
  
  
      !----------------------------------------------------------------!
      !                     COMPARISON OPERATORS                       !
      !----------------------------------------------------------------!
  
      !----- EQ operator (==)
        function hdual_eq_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).EQ.REAL(qright%x,PR))
            
        end function hdual_eq_hdual
  
  
        function hdual_eq_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).EQ.iright)
          
        end function hdual_eq_dble
  
  
        function hdual_eq_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).EQ.REAL(iright,PR))
          
        end function hdual_eq_dble_SPR
        
  
        function dble_eq_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR).EQ.xleft)
          
        end function dble_eq_hdual
  
  
        function dble_eq_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR).EQ.xleft)
          
        end function dble_eq_hdual_SPR
  
      
      !----- NE operator (/=)
        function hdual_ne_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).NE.REAL(qright%x,PR))
            
        end function hdual_ne_hdual
  
  
        function hdual_ne_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).NE.iright)
          
        end function hdual_ne_dble
  
  
        function hdual_ne_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR).NE.REAL(iright,PR))
          
        end function hdual_ne_dble_SPR
        
  
        function dble_ne_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR).NE.xleft)
          
        end function dble_ne_hdual
  
  
        function dble_ne_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR).NE.REAL(xleft,PR))
          
        end function dble_ne_hdual_SPR
  
        
        !----- GT operator (>)
        function hdual_gt_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) > REAL(qright%x,PR))
            
        end function hdual_gt_hdual
  
  
        function hdual_gt_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) > iright)
          
        end function hdual_gt_dble
  
  
        pure function hdual_gt_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) > REAL(iright,8))
          
        end function hdual_gt_dble_SPR
        
  
        function dble_gt_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) > xleft)
          
        end function dble_gt_hdual
  
  
        function dble_gt_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) > REAL(xleft,PR))
          
        end function dble_gt_hdual_SPR
  
  
  
        
        !----- GE operator (>=)
        function hdual_ge_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) >= REAL(qright%x,PR))
            
        end function hdual_ge_hdual
  
  
        function hdual_ge_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) >= iright)
          
        end function hdual_ge_dble
  
        function hdual_ge_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) >= iright)
          
        end function hdual_ge_dble_SPR
        
  
        function dble_ge_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) >= xleft)
          
        end function dble_ge_hdual
  
        function dble_ge_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) >= xleft)
          
        end function dble_ge_hdual_SPR
  
        
        !----- LT operator (<)
        function hdual_lt_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) < REAL(qright%x,PR))
            
        end function hdual_lt_hdual
  
  
        function hdual_lt_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) < iright)
          
        end function hdual_lt_dble
  
        function hdual_lt_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) < iright)
          
        end function hdual_lt_dble_SPR
        
  
        function dble_lt_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) < xleft)
          
        end function dble_lt_hdual
  
        function dble_lt_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) < xleft)
          
        end function dble_lt_hdual_SPR
  
        
        !----- LE operator (<)
        function hdual_le_hdual(qleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) <= REAL(qright%x,PR))
            
        end function hdual_le_hdual
  
  
        function hdual_le_dble(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(PR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) <= iright)
          
        end function hdual_le_dble
  
        pure function hdual_le_dble_SPR(qleft, iright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          REAL(SPR), intent(in)        :: iright
          logical                     :: bool
          
          bool = (REAL(qleft%x,PR) <= iright)
          
        end function hdual_le_dble_SPR
        
  
        function dble_le_hdual(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(PR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) <= xleft)
          
        end function dble_le_hdual
  
        function dble_le_hdual_SPR(xleft, qright) result(bool)
  
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          REAL(SPR), intent(in)        :: xleft
          logical                     :: bool
          
          bool = (REAL(qright%x,PR) <= xleft)
          
        end function dble_le_hdual_SPR
  
  
        
  
  
        !----------------------------------------------------------------!
        !                     ARITHMETIC OPERATORS                       !
        !----------------------------------------------------------------!
  
        !----- Addition operator (+)
        function hdual_plus_hdual(qleft, qright) result(res)
          
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          TYPE(hyperdual) :: res
                
          res%x = qleft%x + qright%x
          res%dx1 = qleft%dx1 + qright%dx1
          res%dx2 = qleft%dx2 + qright%dx2
          res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                    
        end function hdual_plus_hdual
  
  
        function hdual_plus_hdual_array(qleft, qright) result(res)
          
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
                
          implicit none      
          TYPE(hyperdual), intent(in) :: qleft
          real(PR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
        end function hdual_plus_dble
  
  
        function hdual_plus_dble_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in) :: qleft
          real(SPR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
                
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
        end function hdual_plus_dble_SPR
             
        
        function hdual_plus_dble_array(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)         :: qleft
          real(PR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_array
  
  
        function hdual_plus_dble_array_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)         :: qleft
          real(SPR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
                
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_array_SPR
                
  
        function hdual_plus_dble_matrix(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)           :: qleft
          real(PR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_matrix
  
  
        function hdual_plus_dble_matrix_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)           :: qleft
          real(SPR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
                
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_matrix_SPR
             
        
        function hdual_plus_dble_tens(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)             :: qleft
          real(PR), dimension(:,:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
                
            res%x  = qleft%x + iright
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_tens
  
  
        function hdual_plus_dble_tens_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)             :: qleft
          real(SPR), dimension(:,:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
                
            res%x  = qleft%x + REAL(iright,PR)
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
                    
        end function hdual_plus_dble_tens_SPR
                
  
        function hdual_plus_int(qleft, iright) result(res)
                
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
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft, qright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x + qright%x
          res%dx1 = qleft%dx1 + qright%dx1
          res%dx2 = qleft%dx2 + qright%dx2
          res%dx1x2 = qleft%dx1x2 + qright%dx1x2
                    
        end function hdual_array_plus_hdual_array
             
        
        function hdual_array_plus_dble(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_plus_dble
  
  
        function hdual_array_plus_dble_SPR(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_plus_dble_SPR
        
        
        function hdual_array_plus_dble_array(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), dimension(:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_plus_dble_array
  
  
        function hdual_array_plus_dble_array_SPR(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), dimension(:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_plus_dble_array_SPR
          
        
        function hdual_array_plus_int(qleft, iright) result(res)
                
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
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_plus_dble
  
  
        function hdual_matrix_plus_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_plus_dble_SPR
  
  
        function hdual_matrix_plus_dble_matrix(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), dimension(:,:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_plus_dble_matrix
  
  
        function hdual_matrix_plus_dble_matrix_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), dimension(:,:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_plus_dble_matrix_SPR
          
        
        function hdual_matrix_plus_int(qleft, iright) result(res)
                
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
  
  
        function hdual_tens_plus_dble_SPR(qleft, iright) result(res)
    
            implicit none
            TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
            real(SPR), intent(in)                          :: iright
            TYPE(hyperdual), &
              dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
    
            res%x  = qleft%x + REAL(iright, PR)
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
          
          end function hdual_tens_plus_dble_SPR
  
  
        function hdual_tens_plus_dble_tens(qleft, iright) result(res)
  
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
  
  
        function hdual_tens_plus_dble_tens_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(SPR), dimension(:,:,:), intent(in)        :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
  
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_tens_plus_dble_tens_SPR
         
        
        function hdual_tens_plus_int(qleft, iright) result(res)
                
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
  
  
        function hdual_4d_plus_hdual(qleft, qright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          TYPE(hyperdual), intent(in)                     :: qright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3),size(qleft,4))  :: res
  
          res%x  = qleft%x + qright%x
          res%dx1 = qleft%dx1 + qright%dx1
          res%dx2 = qleft%dx2 + qright%dx2
          res%dx1x2 = qleft%dx1x2 + qright%dx1x2 
        
        end function hdual_4d_plus_hdual
  
        function hdual_4d_plus_hdual_4d(qleft, qright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1),size(qright,2),size(qright,3),size(qright,4))  :: res
  
          res%x  = qleft%x + qright%x
          res%dx1 = qleft%dx1 + qright%dx1
          res%dx2 = qleft%dx2 + qright%dx2
          res%dx1x2 = qleft%dx1x2 + qright%dx1x2
        
        end function hdual_4d_plus_hdual_4d
  
  
        function hdual_4d_plus_dble(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          real(PR), intent(in)                            :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3),size(qleft,4))  :: res
  
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_4d_plus_dble
  
  
        function hdual_4d_plus_dble_SPR(qleft, iright) result(res)
    
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
            real(SPR), intent(in)                            :: iright
            TYPE(hyperdual), &
              dimension(size(qleft,1),size(qleft,2),size(qleft,3),size(qleft,4))  :: res
    
            res%x  = qleft%x + REAL(iright, PR)
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
          
          end function hdual_4d_plus_dble_SPR
  
  
        function hdual_4d_plus_dble_4d(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          real(PR), dimension(:,:,:,:), intent(in)        :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3),size(qleft,4))  :: res
  
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_4d_plus_dble_4d
  
  
        function hdual_4d_plus_dble_4d_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          real(SPR), dimension(:,:,:,:), intent(in)        :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3),size(qleft,4))  :: res
  
          res%x  = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_4d_plus_dble_4d_SPR
         
        
        function hdual_4d_plus_int(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          integer, intent(in)                             :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1), size(qleft,2), size(qleft,3),size(qleft,4))  :: res
                
          res%x  = qleft%x + iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                
        end function hdual_4d_plus_int
          
        
        function hdual_4d_plus_int_4d(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qleft
          integer, dimension(:,:,:,:), intent(in)         :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1), size(qleft,2), size(qleft,3),size(qleft,4))  :: res
                
          res%x = qleft%x + REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_4d_plus_int_4d
    
  
        function dble_plus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2 
                    
        end function dble_plus_hdual
  
  
        function dble_plus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2 
                    
        end function dble_plus_hdual_SPR
           
        
        function dble_plus_hdual_array(xleft, qright) result(res)
                
          implicit none
          real(PR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_plus_hdual_array
  
  
        function dble_plus_hdual_array_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_plus_hdual_array_SPR
  
  
        function dble_plus_hdual_matrix(xleft, qright) result(res)
  
          implicit none
          real(PR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
  
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
        
        end function dble_plus_hdual_matrix
  
  
        function dble_plus_hdual_matrix_SPR(xleft, qright) result(res)
  
          implicit none
          real(SPR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
  
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
        
        end function dble_plus_hdual_matrix_SPR
  
  
        function dble_plus_hdual_tens(xleft, qright) result(res)
                
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
  
  
        function dble_plus_hdual_tens_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)                          :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_plus_hdual_tens_SPR
  
  
        function dble_array_plus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft))   :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_array_plus_hdual
  
  
        function dble_array_plus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft))   :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_array_plus_hdual_SPR
  
  
        function dble_array_plus_hdual_array(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2 
                
        end function dble_array_plus_hdual_array
  
  
        function dble_array_plus_hdual_array_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2 
                
        end function dble_array_plus_hdual_array_SPR
  
  
        function dble_matrix_plus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_matrix_plus_hdual
  
  
        function dble_matrix_plus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_matrix_plus_hdual_SPR
  
  
        function dble_matrix_plus_hdual_matrix(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_matrix_plus_hdual_matrix
  
  
        function dble_matrix_plus_hdual_matrix_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_matrix_plus_hdual_matrix_SPR
  
  
        function dble_tens_plus_hdual(xleft, qright) result(res)
                
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
  
  
        function dble_tens_plus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = qright%x + REAL(xleft,SPR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_tens_plus_hdual_SPR
  
  
        function dble_tens_plus_hdual_tens(xleft, qright) result(res)
                
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
  
  
        function dble_tens_plus_hdual_tens_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_tens_plus_hdual_tens_SPR
  
  
        function dble_4d_plus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3),size(xleft,4)) :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_4d_plus_hdual
  
  
        function dble_4d_plus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3),size(xleft,4)) :: res
                
          res%x = qright%x + REAL(xleft,SPR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_4d_plus_hdual_SPR
  
  
        function dble_4d_plus_hdual_4d(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:,:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3),size(xleft,4)) :: res
                
          res%x = qright%x + xleft
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_4d_plus_hdual_4d
  
  
        function dble_4d_plus_hdual_4d_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3),size(xleft,4)) :: res
                
          res%x = qright%x + REAL(xleft,PR)
          res%dx1 = qright%dx1 
          res%dx2 = qright%dx2 
          res%dx1x2 = qright%dx1x2
                
        end function dble_4d_plus_hdual_4d_SPR
  
  
        function int_plus_hdual(ileft, qright) result(res)
                
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
          
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          TYPE(hyperdual) :: res
                
          res%x = qleft%x - qright%x
          res%dx1 = qleft%dx1 - qright%dx1
          res%dx2 = qleft%dx2 - qright%dx2
          res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                    
        end function hdual_minus_hdual
  
  
        function hdual_minus_hdual_array(qleft, qright) result(res)
          
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
                
          implicit none      
          TYPE(hyperdual), intent(in) :: qleft
          real(PR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
        end function hdual_minus_dble
  
  
        function hdual_minus_dble_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in) :: qleft
          real(SPR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
                
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                    
        end function hdual_minus_dble_SPR
             
        
        function hdual_minus_dble_array(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)         :: qleft
          real(PR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_array
  
  
        function hdual_minus_dble_array_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)         :: qleft
          real(SPR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
                
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_array_SPR
                
  
        function hdual_minus_dble_matrix(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)           :: qleft
          real(PR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_matrix
  
  
        function hdual_minus_dble_matrix_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)           :: qleft
          real(SPR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
                
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_matrix_SPR
             
        
        function hdual_minus_dble_tens(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)             :: qleft
          real(PR), dimension(:,:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
                
            res%x  = qleft%x - iright
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_tens
  
  
        function hdual_minus_dble_tens_SPR(qleft, iright) result(res)
                
          implicit none      
          TYPE(hyperdual), intent(in)             :: qleft
          real(SPR), dimension(:,:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
                
            res%x  = qleft%x - REAL(iright,PR)
            res%dx1 = qleft%dx1 
            res%dx2 = qleft%dx2 
            res%dx1x2 = qleft%dx1x2
                    
        end function hdual_minus_dble_tens_SPR
                
  
        function hdual_minus_int(qleft, iright) result(res)
                
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
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft, qright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x - qright%x
          res%dx1 = qleft%dx1 - qright%dx1
          res%dx2 = qleft%dx2 - qright%dx2
          res%dx1x2 = qleft%dx1x2 - qright%dx1x2
                    
        end function hdual_array_minus_hdual_array
             
        
        function hdual_array_minus_dble(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_minus_dble
  
  
        function hdual_array_minus_dble_SPR(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_minus_dble_SPR
        
        
        function hdual_array_minus_dble_array(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), dimension(:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_minus_dble_array
  
  
        function hdual_array_minus_dble_array_SPR(qleft, iright) result(res)
                
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), dimension(:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
                
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2 
                
        end function hdual_array_minus_dble_array_SPR
          
        
        function hdual_array_minus_int(qleft, iright) result(res)
                
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
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_minus_dble
  
  
        function hdual_matrix_minus_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_minus_dble_SPR
  
  
        function hdual_matrix_minus_dble_matrix(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), dimension(:,:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x - iright
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_minus_dble_matrix
  
  
        function hdual_matrix_minus_dble_matrix_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), dimension(:,:), intent(in)        :: iright
          TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_matrix_minus_dble_matrix_SPR
          
        
        function hdual_matrix_minus_int(qleft, iright) result(res)
                
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
  
  
        function hdual_tens_minus_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(SPR), intent(in)                          :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
  
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_tens_minus_dble_SPR
  
  
        function hdual_tens_minus_dble_tens(qleft, iright) result(res)
  
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
  
  
        function hdual_tens_minus_dble_tens_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(SPR), dimension(:,:,:), intent(in)        :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
  
          res%x  = qleft%x - REAL(iright,PR)
          res%dx1 = qleft%dx1 
          res%dx2 = qleft%dx2 
          res%dx1x2 = qleft%dx1x2
        
        end function hdual_tens_minus_dble_tens_SPR
         
        
        function hdual_tens_minus_int(qleft, iright) result(res)
                
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
                
          implicit none
          real(PR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function dble_minus_hdual
  
        
        function dble_minus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function dble_minus_hdual_SPR
           
        
        function dble_minus_hdual_array(xleft, qright) result(res)
                
          implicit none
          real(PR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_minus_hdual_array
  
  
        function dble_minus_hdual_array_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_minus_hdual_array_SPR
  
  
        function dble_minus_hdual_matrix(xleft, qright) result(res)
  
          implicit none
          real(PR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
  
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
        
        end function dble_minus_hdual_matrix
  
  
        function dble_minus_hdual_matrix_SPR(xleft, qright) result(res)
  
          implicit none
          real(SPR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
  
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
        
        end function dble_minus_hdual_matrix_SPR
  
  
        function dble_minus_hdual_tens(xleft, qright) result(res)
                
          implicit none
          real(PR), intent(in)                          :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_minus_hdual_tens
  
  
        function dble_minus_hdual_tens_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), intent(in)                          :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_minus_hdual_tens_SPR
  
  
        function dble_array_minus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft))   :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_array_minus_hdual
  
  
        function dble_array_minus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft))   :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_array_minus_hdual_SPR
  
  
        function dble_array_minus_hdual_array(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x =  xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                
        end function dble_array_minus_hdual_array
  
  
        function dble_array_minus_hdual_array_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x =  REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                
        end function dble_array_minus_hdual_array_SPR
  
  
        function dble_matrix_minus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_matrix_minus_hdual
  
  
        function dble_matrix_minus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_matrix_minus_hdual_SPR
  
  
        function dble_matrix_minus_hdual_matrix(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_matrix_minus_hdual_matrix
  
  
        function dble_matrix_minus_hdual_matrix_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_matrix_minus_hdual_matrix_SPR
  
  
        function dble_tens_minus_hdual(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_tens_minus_hdual
  
  
        function dble_tens_minus_hdual_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_tens_minus_hdual_SPR
  
  
        function dble_tens_minus_hdual_tens(xleft, qright) result(res)
                
          implicit none
          real(PR), dimension(:,:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = xleft - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_tens_minus_hdual_tens
  
  
        function dble_tens_minus_hdual_tens_SPR(xleft, qright) result(res)
                
          implicit none
          real(SPR), dimension(:,:,:), intent(in)        :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
                
          res%x = REAL(xleft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                
        end function dble_tens_minus_hdual_tens_SPR
  
  
        function int_minus_hdual(ileft, qright) result(res)
                
          implicit none
          integer, intent(in)         :: ileft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2
                    
        end function int_minus_hdual
  
  
        function int_minus_hdual_array(ileft, qright) result(res)
                
          implicit none
          integer, intent(in)                       :: ileft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_minus_hdual_array
  
  
        function int_minus_hdual_matrix(ileft, qright) result(res)
                
          implicit none
          integer, intent(in)                         :: ileft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_minus_hdual_matrix
  
  
        function int_minus_hdual_tens(ileft, qright) result(res)
                
          implicit none
          integer, intent(in)                           :: ileft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1), size(qright,2), size(qright,3)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_minus_hdual_tens
  
  
        function int_array_minus_hdual(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:), intent(in) :: ileft
          TYPE(hyperdual), intent(in)       :: qright
          TYPE(hyperdual), dimension(size(ileft)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2  
                    
        end function int_array_minus_hdual
  
  
        function int_array_minus_hdual_array(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:), intent(in)         :: ileft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(ileft))   :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_array_minus_hdual_array
  
        function int_matrix_minus_hdual(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:,:), intent(in) :: ileft
          TYPE(hyperdual), intent(in)         :: qright
          TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_matrix_minus_hdual
  
        function int_matrix_minus_hdual_matrix(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:,:), intent(in)         :: ileft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2  
                    
        end function int_matrix_minus_hdual_matrix
  
        function int_tens_minus_hdual(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:,:,:), intent(in) :: ileft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), &
            dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                    
        end function int_tens_minus_hdual
  
        function int_tens_minus_hdual_tens(ileft, qright) result(res)
                
          implicit none
          integer, dimension(:,:,:), intent(in)         :: ileft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
                
          res%x = REAL(ileft,PR) - qright%x
          res%dx1 = -qright%dx1 
          res%dx2 = -qright%dx2 
          res%dx1x2 = -qright%dx1x2 
                  
        end function int_tens_minus_hdual_tens
  
        function minus_hdual(qright) result(res)
          
          implicit none
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
        
          res%x     = - qright%x 
          res%dx1   = - qright%dx1
          res%dx2   = - qright%dx2
          res%dx1x2 = - qright%dx1x2
        
        end function minus_hdual
  
        function minus_hdual_array(qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
        
          res%x     = - qright%x 
          res%dx1   = - qright%dx1
          res%dx2   = - qright%dx2
          res%dx1x2 = - qright%dx1x2
        
        end function minus_hdual_array
  
        function minus_hdual_matrix(qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2))  :: res
        
          res%x     = - qright%x 
          res%dx1   = - qright%dx1
          res%dx2   = - qright%dx2
          res%dx1x2 = - qright%dx1x2
        
        end function minus_hdual_matrix
  
        function minus_hdual_tens(qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1), size(qright,2),size(qright,3))  :: res
        
          res%x     = - qright%x 
          res%dx1   = - qright%dx1
          res%dx2   = - qright%dx2
          res%dx1x2 = - qright%dx1x2
        
        end function minus_hdual_tens
  
  
        !----- Multiplication operator (*)
        pure function hdual_mul_hdual(qleft, qright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          TYPE(hyperdual)             :: res
        
          res%x = qleft%x * qright%x
          res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
          res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
          res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
        
        end function hdual_mul_hdual
        
        function hdual_mul_hdual_array(qleft, qright) result(res)
        
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
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          real(PR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
        
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
        
        end function hdual_mul_dble
  
  
        function hdual_mul_dble_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          real(SPR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
        
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
        
        end function hdual_mul_dble_SPR
  
        
        function hdual_mul_dble_array(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in)         :: qleft
          real(PR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
        
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
        
        end function hdual_mul_dble_array
  
  
        function hdual_mul_dble_array_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in)         :: qleft
          real(SPR), dimension(:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright))  :: res
        
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
        
        end function hdual_mul_dble_array_SPR
  
  
        function hdual_mul_dble_matrix(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in)           :: qleft
          real(PR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
        
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
        
        end function hdual_mul_dble_matrix
  
  
        function hdual_mul_dble_matrix_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in)           :: qleft
          real(SPR), dimension(:,:), intent(in)  :: iright
          TYPE(hyperdual), dimension(size(iright,1),size(iright,2)) :: res
        
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
        
        end function hdual_mul_dble_matrix_SPR
  
  
        function hdual_mul_dble_tens(qleft, iright) result(res)
        
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
  
  
        function hdual_mul_dble_tens_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in)             :: qleft
          real(SPR), dimension(:,:,:), intent(in)  :: iright
          TYPE(hyperdual), &
            dimension(size(iright,1),size(iright,2),size(iright,3)) :: res
        
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
        
        end function hdual_mul_dble_tens_SPR
  
  
        function hdual_mul_int(qleft, iright) result(res)
        
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
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft)) :: res
  
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
          
        end function hdual_array_mul_dble
  
  
        function hdual_array_mul_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft)) :: res
  
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
          
        end function hdual_array_mul_dble_SPR
  
  
        function hdual_array_mul_int(qleft, iright) result(res)
        
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
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
  
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
          
        end function hdual_matrix_mul_dble
  
  
        function hdual_matrix_mul_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
  
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
          
        end function hdual_matrix_mul_dble_SPR
  
  
        function hdual_matrix_mul_int(qleft, iright) result(res)
        
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
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          TYPE(hyperdual), intent(in)                   :: qright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res
  
          res%x = qleft%x * qright%x
          res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
          res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
          res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
          
        end function hdual_tens_mul_hdual
  
  
        function hdual_tens_mul_dble(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(PR), intent(in)                          :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res
  
          res%x = qleft%x * iright
          res%dx1 = qleft%dx1 * iright
          res%dx2 = qleft%dx2 * iright
          res%dx1x2 = qleft%dx1x2 * iright
          
        end function hdual_tens_mul_dble
  
  
        function hdual_tens_mul_dble_SPR(qleft, iright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(SPR), intent(in)                          :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3)) :: res
  
          res%x = qleft%x * REAL(iright,PR)
          res%dx1 = qleft%dx1 * REAL(iright,PR)
          res%dx2 = qleft%dx2 * REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(iright,PR)
          
        end function hdual_tens_mul_dble_SPR
  
  
        function hdual_tens_mul_int(qleft, iright) result(res)
        
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
        
          implicit none
          real(PR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
        
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
        
        end function dble_mul_hdual
  
  
        pure function dble_mul_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_mul_hdual_SPR
        
  
        function dble_mul_hdual_array(xleft, qright) result(res)
        
          implicit none
          real(PR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
        
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
        
        end function dble_mul_hdual_array
  
  
        function dble_mul_hdual_array_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), intent(in)                      :: xleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright))  :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_mul_hdual_array_SPR
        
  
        function dble_mul_hdual_matrix(xleft, qright) result(res)
        
          implicit none
          real(PR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
        
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
        
        end function dble_mul_hdual_matrix
  
  
        function dble_mul_hdual_matrix_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), intent(in)                        :: xleft
          TYPE(hyperdual), dimension(:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2)) :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_mul_hdual_matrix_SPR
        
  
        function dble_mul_hdual_tens(xleft, qright) result(res)
        
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
  
  
        function dble_mul_hdual_tens_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), intent(in)                          :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), &
            dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
      
        end function dble_mul_hdual_tens_SPR
        
  
        function dble_array_mul_hdual(xleft, qright) result(res)
        
          implicit none
          real(PR), dimension(:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)         :: qright
          TYPE(hyperdual), dimension(size(xleft)) :: res
        
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
        
        end function dble_array_mul_hdual
  
  
        function dble_array_mul_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)         :: qright
          TYPE(hyperdual), dimension(size(xleft)) :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_array_mul_hdual_SPR
        
  
        function dble_matrix_mul_hdual(xleft, qright) result(res)
        
          implicit none
          real(PR), dimension(:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
        
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
        
        end function dble_matrix_mul_hdual
        
  
        function dble_matrix_mul_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_matrix_mul_hdual_SPR
  
  
        function dble_tens_mul_hdual(xleft, qright) result(res)
        
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
  
  
        function dble_tens_mul_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
        
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
        
        end function dble_tens_mul_hdual_SPR
        
  
        function int_mul_hdual(ileft, qright) result(res)
        
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
  
  
        function hdual_array_mul_hdual_array(qleft, qright) result(res)
          ! Elemental multiplication, size(qleft) == size(qright)
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qleft
          TYPE(hyperdual), dimension(:), intent(in)   :: qright
          TYPE(hyperdual), dimension(size(qleft))     :: res
  
          res%x = qleft%x * qright%x
          res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
          res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
          res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
  
        end function hdual_array_mul_hdual_array
  
  
        function hdual_array_mul_dble_array(qleft, xright) result(res)
          ! Elemental multiplication, size(qleft) == size(qright)
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qleft
          Real(PR), dimension(:), intent(in)         :: xright
          TYPE(hyperdual), dimension(size(qleft))     :: res
  
          res%x = qleft%x * xright
          res%dx1 = qleft%dx1 * xright
          res%dx2 = qleft%dx2 * xright
          res%dx1x2 = qleft%dx1x2 * xright
  
        end function hdual_array_mul_dble_array
  
  
        function hdual_array_mul_dble_array_SPR(qleft, xright) result(res)
          ! Elemental multiplication, size(qleft) == size(qright)
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qleft
          Real(SPR), dimension(:), intent(in)         :: xright
          TYPE(hyperdual), dimension(size(qleft))     :: res
  
          res%x = qleft%x * REAL(xright,PR)
          res%dx1 = qleft%dx1 * REAL(xright,PR)
          res%dx2 = qleft%dx2 * REAL(xright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(xright,PR)
  
        end function hdual_array_mul_dble_array_SPR
  
  
        function dble_array_mul_hdual_array(xleft, qright) result(res)
          ! Elemental multiplication, size(qleft) == size(qright)
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qright
          Real(PR), dimension(:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(size(qright))     :: res
  
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
  
        end function dble_array_mul_hdual_array
  
  
        function dble_array_mul_hdual_array_SPR(xleft, qright) result(res)
          ! Elemental multiplication, size(qleft) == size(qright)
          implicit none
          TYPE(hyperdual), dimension(:), intent(in)   :: qright
          Real(SPR), dimension(:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(size(qright))     :: res
  
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
  
        end function dble_array_mul_hdual_array_SPR
  
  
  
        function hdual_matrix_mul_hdual_matrix(qleft, qright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qleft
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2))    :: res
  
          res%x = qleft%x * qright%x
          res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
          res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
          res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
  
        end function hdual_matrix_mul_hdual_matrix
  
  
        function hdual_matrix_mul_dble_matrix(qleft, xright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qleft
          real(PR), dimension(:,:), intent(in)          :: xright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2))    :: res
          
          res%x = qleft%x * xright
          res%dx1 = qleft%dx1 * xright
          res%dx2 = qleft%dx2 * xright
          res%dx1x2 = qleft%dx1x2 * xright
  
        end function hdual_matrix_mul_dble_matrix
  
  
        function hdual_matrix_mul_dble_matrix_SPR(qleft, xright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qleft
          real(SPR), dimension(:,:), intent(in)         :: xright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2))    :: res
          
          res%x = qleft%x * REAL(xright, 8)
          res%dx1 = qleft%dx1 * REAL(xright, 8)
          res%dx2 = qleft%dx2 * REAL(xright, 8)
          res%dx1x2 = qleft%dx1x2 * REAL(xright, 8) 
  
        end function hdual_matrix_mul_dble_matrix_SPR
  
  
        function dble_matrix_mul_hdual_matrix(xleft, qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qright
          real(PR), dimension(:,:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2))    :: res
          
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
          
        end function dble_matrix_mul_hdual_matrix
  
  
        function dble_matrix_mul_hdual_matrix_SPR(xleft, qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in)   :: qright
          real(SPR), dimension(:,:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2))    :: res
          
          res%x = qright%x * REAL(xleft, 8)
          res%dx1 = qright%dx1 * REAL(xleft, 8)
          res%dx2 = qright%dx2 * REAL(xleft, 8)
          res%dx1x2 = qright%dx1x2 * REAL(xleft, 8)
          
        end function dble_matrix_mul_hdual_matrix_SPR
  
  
  
  
        function hdual_tens_mul_hdual_tens(qleft, qright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qleft
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2),size(qleft,3))    :: res
  
          res%x = qleft%x * qright%x
          res%dx1 = qleft%x * qright%dx1 + qleft%dx1 * qright%x
          res%dx2 = qleft%x * qright%dx2 + qleft%dx2 * qright%x
          res%dx1x2 = qleft%x * qright%dx1x2 + qleft%dx1 * qright%dx2 + qleft%dx2 * qright%dx1 + qleft%dx1x2 * qright%x
  
        end function hdual_tens_mul_hdual_tens
  
  
        function hdual_tens_mul_dble_tens(qleft, xright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qleft
          real(PR), dimension(:,:,:), intent(in)          :: xright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2),size(qleft,3))    :: res
          
          res%x = qleft%x * xright
          res%dx1 = qleft%dx1 * xright
          res%dx2 = qleft%dx2 * xright
          res%dx1x2 = qleft%dx1x2 * xright
  
        end function hdual_tens_mul_dble_tens
  
  
        function hdual_tens_mul_dble_tens_SPR(qleft, xright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qleft
          real(SPR), dimension(:,:,:), intent(in)          :: xright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2),size(qleft,3))    :: res
          
          res%x = qleft%x * REAL(xright,PR)
          res%dx1 = qleft%dx1 * REAL(xright,PR)
          res%dx2 = qleft%dx2 * REAL(xright,PR)
          res%dx1x2 = qleft%dx1x2 * REAL(xright,PR)
  
        end function hdual_tens_mul_dble_tens_SPR
  
  
        function dble_tens_mul_hdual_tens(xleft, qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qright
          real(PR), dimension(:,:,:), intent(in)          :: xleft
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3))    :: res
          
          res%x = qright%x * xleft
          res%dx1 = qright%dx1 * xleft
          res%dx2 = qright%dx2 * xleft
          res%dx1x2 = qright%dx1x2 * xleft
          
        end function dble_tens_mul_hdual_tens
  
  
        function dble_tens_mul_hdual_tens_SPR(xleft, qright) result(res)
          
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in)   :: qright
          real(SPR), dimension(:,:,:), intent(in)         :: xleft
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3))    :: res
          
          res%x = qright%x * REAL(xleft,PR)
          res%dx1 = qright%dx1 * REAL(xleft,PR)
          res%dx2 = qright%dx2 * REAL(xleft,PR)
          res%dx1x2 = qright%dx1x2 * REAL(xleft,PR)
          
        end function dble_tens_mul_hdual_tens_SPR
  
  
        !----- Division operator (/)
        pure function hdual_div_hdual(qleft, qright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft, qright
          TYPE(hyperdual)             :: inv
          TYPE(hyperdual)             :: res
          
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = qleft * inv
  
        end function hdual_div_hdual
        
        function hdual_div_dble(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          real(PR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
        
          res%x = qleft%x / iright
          res%dx1 = qleft%dx1 / iright
          res%dx2 = qleft%dx2 / iright
          res%dx1x2 = qleft%dx1x2 / iright
            
        end function hdual_div_dble
  
  
        function hdual_div_dble_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), intent(in) :: qleft
          real(SPR), intent(in)        :: iright
          TYPE(hyperdual)             :: res
        
          res%x = qleft%x / REAL(iright,PR)
          res%dx1 = qleft%dx1 / REAL(iright,PR)
          res%dx2 = qleft%dx2 / REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
            
        end function hdual_div_dble_SPR
        
  
        function hdual_div_int(qleft, iright) result(res)
        
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
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(qleft))   :: res
          TYPE(hyperdual)                           :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = qleft * inv
          
        end function hdual_array_div_hdual
        
  
        function hdual_array_div_dble(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
        
          res%x = qleft%x / iright
          res%dx1 = qleft%dx1 / iright
          res%dx2 = qleft%dx2 / iright
          res%dx1x2 = qleft%dx1x2 / iright
        
        end function hdual_array_div_dble
  
  
        function hdual_array_div_dble_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), intent(in)                      :: iright
          TYPE(hyperdual), dimension(size(qleft))   :: res
        
          res%x = qleft%x / REAL(iright,PR)
          res%dx1 = qleft%dx1 / REAL(iright,PR)
          res%dx2 = qleft%dx2 / REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
        
        end function hdual_array_div_dble_SPR
  
        
        function hdual_array_div_int(qleft, iright) result(res)
        
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
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          TYPE(hyperdual), intent(in)                 :: qright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
          TYPE(hyperdual)                               ::  inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = qleft * inv
        
        end function hdual_matrix_div_hdual
        
        
        function hdual_matrix_div_dble(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(PR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
        
          res%x = qleft%x / iright
          res%dx1 = qleft%dx1 / iright
          res%dx2 = qleft%dx2 / iright
          res%dx1x2 = qleft%dx1x2 / iright
        
        end function hdual_matrix_div_dble
  
  
        function hdual_matrix_div_dble_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
          real(SPR), intent(in)                        :: iright
          TYPE(hyperdual), dimension(size(qleft,1),size(qleft,2)) :: res
        
          res%x = qleft%x / REAL(iright,PR)
          res%dx1 = qleft%dx1 / REAL(iright,PR)
          res%dx2 = qleft%dx2 / REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
        
        end function hdual_matrix_div_dble_SPR
  
        
        function hdual_matrix_div_int(qleft, iright) result(res)
        
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
  
  
        function hdual_tens_div_dble_SPR(qleft, iright) result(res)
        
          implicit none
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qleft
          real(SPR), intent(in)                          :: iright
          TYPE(hyperdual), &
            dimension(size(qleft,1),size(qleft,2),size(qleft,3))  :: res
        
          res%x = qleft%x / REAL(iright,PR)
          res%dx1 = qleft%dx1 / REAL(iright,PR)
          res%dx2 = qleft%dx2 / REAL(iright,PR)
          res%dx1x2 = qleft%dx1x2 / REAL(iright,PR)
        
        end function hdual_tens_div_dble_SPR
        
  
        function hdual_tens_div_int(qleft, iright) result(res)
        
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
        
          implicit none
          real(PR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
          TYPE(hyperdual)             :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = xleft * inv
        
        end function dble_div_hdual
  
  
        function dble_div_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), intent(in)        :: xleft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
          TYPE(hyperdual)             :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = REAL(xleft,PR) * inv
        
        end function dble_div_hdual_SPR
  
  
        function dble_div_hdual_tens_SPR(xleft, qright) result(res)
          
          implicit none
          real(SPR), intent(in)    :: xleft
          TYPE(hyperdual), dimension(:,:,:), intent(in) :: qright
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3)) :: res
          TYPE(hyperdual), dimension(size(qright,1),size(qright,2),size(qright,3)) :: inv 
          Integer                                       :: I,J,K
  
          do I = 1, size(qright,1)
            do J = 1, size(qright,2)
              do K = 1, size(qright,3)
                inv(I,J,K) = hdual_pow_dble(qright(I,J,K), -1.0_PR)
                res(I,J,K) = REAL(xleft, PR) * inv(I,J,K)
              enddo 
            enddo
          enddo
          
        end function dble_div_hdual_tens_SPR
        
        function int_div_hdual(ileft, qright) result(res)
        
          implicit none
          integer, intent(in)         :: ileft
          TYPE(hyperdual), intent(in) :: qright
          TYPE(hyperdual)             :: res
          TYPE(hyperdual)             :: inv
          
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = ileft * inv
        
        end function int_div_hdual    
  
  
        function dble_array_div_hdual(xleft, qright) result(res)
        
          implicit none
          real(PR), dimension(:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), dimension(size(xleft)) :: res
          TYPE(hyperdual)                         :: inv
  
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = xleft * inv
        
        end function dble_array_div_hdual
        
  
        function dble_array_div_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:), intent(in)      :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), dimension(size(xleft)) :: res
          TYPE(hyperdual)                         :: inv
  
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = REAL(xleft,PR) * inv
        
        end function dble_array_div_hdual_SPR
  
  
        function int_array_div_hdual(ileft, qright) result(res)
        
          implicit none
          integer, dimension(:), intent(in)         :: ileft
          TYPE(hyperdual), intent(in)               :: qright
          TYPE(hyperdual), dimension(size(ileft,1)) :: res
          TYPE(hyperdual)                           :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = ileft * inv
        
        end function int_array_div_hdual    
  
  
        function dble_matrix_div_hdual(xleft, qright) result(res)
        
          implicit none
          real(PR), dimension(:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
          TYPE(hyperdual)                              :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = xleft * inv
        
        end function dble_matrix_div_hdual
  
        
        function dble_matrix_div_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), dimension(size(xleft,1),size(xleft,2)) :: res
          TYPE(hyperdual)                              :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = REAL(xleft,PR) * inv
        
        end function dble_matrix_div_hdual_SPR
        
  
        function int_matrix_div_hdual(ileft, qright) result(res)
        
          implicit none
          integer, dimension(:,:), intent(in) :: ileft
          TYPE(hyperdual), intent(in)         :: qright
          TYPE(hyperdual), dimension(size(ileft,1),size(ileft,2)) :: res
          TYPE(hyperdual)                              :: inv
        
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = ileft * inv
        
        end function int_matrix_div_hdual       
  
  
        function dble_tens_div_hdual(xleft, qright) result(res)
        
          implicit none
          real(PR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
          TYPE(hyperdual)                              :: inv
      
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = xleft * inv
        
        end function dble_tens_div_hdual
  
  
        function dble_tens_div_hdual_SPR(xleft, qright) result(res)
        
          implicit none
          real(SPR), dimension(:,:,:), intent(in)  :: xleft
          TYPE(hyperdual), intent(in)             :: qright
          TYPE(hyperdual), &
            dimension(size(xleft,1),size(xleft,2),size(xleft,3)) :: res
          TYPE(hyperdual)                              :: inv
      
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = REAL(xleft,PR) * inv
        
        end function dble_tens_div_hdual_SPR
  
          
        function int_tens_div_hdual(ileft, qright) result(res)
        
          implicit none
          integer, dimension(:,:,:), intent(in) :: ileft
          TYPE(hyperdual), intent(in)           :: qright
          TYPE(hyperdual), &
            dimension(size(ileft,1),size(ileft,2),size(ileft,3)) :: res
          TYPE(hyperdual)                              :: inv
      
          inv = hdual_pow_dble(qright, -1.0_PR)
          res = ileft * inv
        
        end function int_tens_div_hdual
  
  
        function hdual_array_div_dble_array(qleft, xright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          Real(PR), dimension(:), intent(in)        :: xright
          TYPE(hyperdual), dimension(size(qleft))   :: res
  
          res%x = qleft%x / xright
          res%dx1 = qleft%dx1 / xright
          res%dx2 = qleft%dx2 / xright
          res%dx1x2 = qleft%dx1x2 / xright
  
        end function hdual_array_div_dble_array
  
  
        function hdual_array_div_dble_array_SPR(qleft, xright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          Real(SPR), dimension(:), intent(in)        :: xright
          TYPE(hyperdual), dimension(size(qleft))   :: res
  
          res%x = qleft%x / REAL(xright,PR)
          res%dx1 = qleft%dx1 / REAL(xright,PR)
          res%dx2 = qleft%dx2 / REAL(xright,PR)
          res%dx1x2 = qleft%dx1x2 / REAL(xright,PR)
  
        end function hdual_array_div_dble_array_SPR
        
  
        function hdual_array_div_hdual_array(qleft, qright) result(res)
  
          ! Elemental hdual array division
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          TYPE(hyperdual), dimension(:), intent(in) :: qright
          integer                                   :: i 
          TYPE(hyperdual), dimension(size(qright))  :: inv
          TYPE(hyperdual), dimension(size(qright))  :: res
  
          do i = 1, size(qright) 
            inv(i) = hdual_pow_dble(qright(i), -1.0_PR)
            res(i) = qleft(i) * inv(i)
          enddo
        
         end function hdual_array_div_hdual_array
  
        !----- POW operator (**)
        function hdual_pow_hdual(qleft, qright) result(res)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_pow_hdual" :: hdual_pow_hdual
            
            implicit none
            TYPE(hyperdual), intent(in) :: qleft
            TYPE(hyperdual), intent(in) :: qright
            TYPE(hyperdual)             :: res
  
            res = hdexp(qright*hdlog(qleft))
  
          end function hdual_pow_hdual
  
  
        pure function hdual_pow_dble(qleft, iright) result(res)
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
        
  
        function hdual_pow_dble_SPR(qleft, iright) result(res)
          !!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "hdual_pow_dble" :: hdual_pow_dble
          
            implicit none
            TYPE(hyperdual), intent(in) :: qleft
            real(SPR), intent(in)        :: iright
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
            deriv = REAL(iright,PR) * xval**(REAL(iright,PR) - 1.0_PR)
            
            res%x = qleft%x**REAL(iright,PR)
            res%dx1 = qleft%dx1 * deriv
            res%dx2 = qleft%dx2 * deriv
            res%dx1x2 = qleft%dx1x2 * deriv + REAL(iright,PR) * (REAL(iright,PR) - 1.0_PR) * &
                          qleft%dx1 * qleft%dx2 * xval**(REAL(iright,PR) - 2.0_PR)
          
          end function hdual_pow_dble_SPR
  
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
          
  
        function dble_pow_hdual(xleft, qright) result(res) 
            
            implicit none
            real(PR), intent(in)        :: xleft
            TYPE(hyperdual), intent(in) :: qright
            TYPE(hyperdual)             :: res
  
            res = hdexp(qright*log(xleft))
  
        end function dble_pow_hdual
  
  
        function hdual_array_pow_dble(qleft, xright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(PR), intent(in)                      :: xright
          TYPE(hyperdual), dimension(size(qleft))   :: res
          Integer                                   :: I
  
          do i = 1, size(qleft) 
            res(i) = hdual_pow_dble(qleft(i), xright)
          enddo 
  
        end function hdual_array_pow_dble
  
  
        function hdual_array_pow_dble_SPR(qleft, xright) result(res)
  
          implicit none
          TYPE(hyperdual), dimension(:), intent(in) :: qleft
          real(SPR), intent(in)                      :: xright
          TYPE(hyperdual), dimension(size(qleft))   :: res
          Integer                                   :: I
  
          do i = 1, size(qleft) 
            res(i) = hdual_pow_dble_SPR(qleft(i), xright)
          enddo 
  
        end function hdual_array_pow_dble_SPR
  
  
  
      !----------------------------------------------------------------!
      !                     INTRINSIC FUNCTIONS                        !
      !----------------------------------------------------------------!
  
      !----- DBLE (conversion to double)
        function dble_hdual(X_in) result(X_out)
            
            implicit none
            TYPE(hyperdual), intent(in)   :: X_in
            real(PR)                      :: X_out
  
            X_out = DBLE(X_in%x) 
          
          end function dble_hdual
  
          function dble_hdual_array(X_in) result(X_out)
            
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: X_in
            real(PR), dimension(size(X_in))           :: X_out
            integer :: i
  
            do i = 1, size(X_in)
              X_out(i) = DBLE(X_in(i)%x)
            enddo
  
          end function dble_hdual_array
  
          function dble_hdual_matrix(X_in) result(X_out)
            
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
            
            implicit none
            TYPE(hyperdual), intent(in)   :: X_in
            TYPE(hyperdual)               :: X_out
  
            X_out = sign(1.0_PR, REAL(X_in%x,PR)) * X_in 
          
          end function abs_hdual
  
          function abs_hdual_array(X_in) result(X_out)
            
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: X_in
            TYPE(hyperdual), dimension(size(X_in))    :: X_out
            integer :: i
  
            do i = 1, size(X_in)
              X_out(i) = abs(X_in(i))
            enddo
  
          end function abs_hdual_array
  
          function abs_hdual_matrix(X_in) result(X_out)
            
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
            
            implicit none
            TYPE(hyperdual), intent(in)   :: val_in, sign_in
            TYPE(hyperdual)               :: val_out
  
            if (REAL(sign_in%x,PR).GE.(0.0_PR)) then
              val_out = abs(val_in)
            else
              val_out = abs(val_in) * (-1.0_PR)
            endif
  
          end function sign_hdual_hdual
  
  
          function sign_hdual_dble(val_in, sign_in) result(val_out)
            
            implicit none
            TYPE(hyperdual), intent(in)   :: val_in
            real(PR),intent(in)           :: sign_in
            TYPE(hyperdual)               :: val_out
  
            if (sign_in .GE. (0.0_PR)) then
              val_out = abs(val_in)
            else
              val_out = abs(val_in) * (-1.0_PR)
            endif
  
          end function sign_hdual_dble
  
  
          function sign_dble_hdual(val_in, sign_in) result(val_out)
            
            implicit none
            TYPE(hyperdual), intent(in)   :: sign_in
            real(PR), intent(in)          :: val_in
            real(PR)                      :: val_out
  
            if (REAL(sign_in%x,PR).GE.(0.0_PR)) then
              val_out = abs(val_in)
            else
              val_out = abs(val_in) * (-1.0_PR)
            endif
  
          end function sign_dble_hdual
  
  
          
  
  
        !----- MAX
          function max_hdual_hdual(q1, q2) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q1
            TYPE(hyperdual), intent(in) :: q2
            TYPE(hyperdual)             :: q_out
  
            if (q1.GE.q2) then
              q_out = q1
            else
              q_out = q2
            endif
  
          end function max_hdual_hdual
  
  
          function max_hdual_dble(q1, x2) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q1
            real(PR), intent(in)        :: x2
            TYPE(hyperdual)             :: q_out
  
            if (q1.GE.x2) then
              q_out = q1
            else
              q_out = x2
            endif
  
          end function max_hdual_dble
  
  
          function max_hdual_dble_SPR(q1, x2) result(q_out)
  
              implicit none
              TYPE(hyperdual), intent(in) :: q1
              real(SPR), intent(in)        :: x2
              TYPE(hyperdual)             :: q_out
    
              if (q1.GE.REAL(x2,PR)) then
                q_out = q1
              else
                q_out = REAL(x2,PR)
              endif
    
            end function max_hdual_dble_SPR
  
  
          function max_dble_hdual(x1, q2) result(q_out)
  
            implicit none
            real(PR), intent(in)        :: x1
            TYPE(hyperdual), intent(in) :: q2
            TYPE(hyperdual)             :: q_out
  
            if (x1.GE.q2) then
              q_out = x1
            else
              q_out = q2
            endif
  
          end function max_dble_hdual
  
  
          ! function max_dble_hdual_SPR(x1, q2) result(q_out)
  
          !   implicit none
          !   real(SPR), intent(in)        :: x1
          !   TYPE(hyperdual), intent(in) :: q2
          !   TYPE(hyperdual)             :: q_out
  
          !   if (REAL(x1,PR).GE.q2) then
          !     q_out = REAL(x1,PR)
          !   else
          !     q_out = q2
          !   endif
  
          ! end function max_dble_hdual_SPR
          function max_hdual_array_dble(q1,x1) result(q_out)
            
            implicit none
            real(PR), intent(in)                               :: x1
            TYPE(hyperdual), dimension(:), intent(in)          :: q1
            TYPE(hyperdual), dimension(size(q1))               :: q_out
            Integer                                            :: I
  
            do I = 1, size(q1)
              if (q1(I) > x1) then
                q_out(I) = q1(I)
              else
                q_out(I) = x1
              endif
            enddo 
  
          end function max_hdual_array_dble
  
  
          function max_hdual_matrix_dble(q1,x1) result(q_out)
            
            implicit none
            real(PR), intent(in)                               :: x1
            TYPE(hyperdual), dimension(:,:), intent(in)        :: q1
            TYPE(hyperdual), dimension(size(q1,1),size(q1,2))  :: q_out
            Integer                                            :: I,J
  
            do I = 1, size(q1,1)
              do J = 1, size(q1, 2)
                if (q1(I,J) > x1) then
                  q_out(I,J) = q1(I,J)
                else
                  q_out(I,J) = x1
                endif
              enddo 
            enddo 
  
          end function max_hdual_matrix_dble
  
  
          function max_hdual_matrix_dble_SPR(q1,x1) result(q_out)
            
            implicit none
            real(SPR), intent(in)                                 :: x1
            TYPE(hyperdual), dimension(:,:), intent(in)          :: q1
            TYPE(hyperdual), dimension(size(q1,1),size(q1,2))    :: q_out
            Integer                                              :: I,J
  
            do I = 1, size(q1,1)
              do J = 1, size(q1, 2)
                if (q1(I,J) > REAL(x1,PR)) then
                  q_out(I,J) = q1(I,J)
                else
                  q_out(I,J) = REAL(x1,PR)
                endif
              enddo 
            enddo 
  
          end function max_hdual_matrix_dble_SPR
  
  
          ! function max_dble_hdual_matrix(x1,q1) result(q_out)
            
          !   implicit none
          !   real(PR), intent(in)                               :: x1
          !   TYPE(hyperdual), dimension(:,:), intent(in)        :: q1
          !   TYPE(hyperdual), dimension(size(q1,1),size(q1,2))  :: q_out
          !   Integer                                            :: I,J
  
          !   do I = 1, size(q1,1)
          !     do J = 1, size(q1, 2)
          !       if (q1(I,J) > x1) then
          !         q_out(I,J) = q1(I,J)
          !       else
          !         q_out(I,J) = x1
          !       endif
          !     enddo 
          !   enddo 
  
          ! end function max_dble_hdual_matrix
  
  
          ! function max_dble_hdual_matrix_SPR(x1,q1) result(q_out)
            
          !   implicit none
          !   real(SPR), intent(in)                                 :: x1
          !   TYPE(hyperdual), dimension(:,:), intent(in)          :: q1
          !   TYPE(hyperdual), dimension(size(q1,1),size(q1,2))    :: q_out
          !   Integer                                              :: I,J
  
          !   do I = 1, size(q1,1)
          !     do J = 1, size(q1, 2)
          !       if (q1(I,J) > REAL(x1,PR)) then
          !         q_out(I,J) = q1(I,J)
          !       else
          !         q_out(I,J) = REAL(x1,PR)
          !       endif
          !     enddo 
          !   enddo 
  
          ! end function max_dble_hdual_matrix_SPR
  
  
          function max_hdual_tens_dble(q1,x1) result(q_out)
            
            implicit none
            real(PR), intent(in)                                            :: x1
            TYPE(hyperdual), dimension(:,:,:), intent(in)                   :: q1
            TYPE(hyperdual), dimension(size(q1,1),size(q1,2),size(q1,3))    :: q_out
            Integer                                                         :: I,J,K
  
            do I = 1, size(q1,1)
              do J = 1, size(q1, 2)
                do K = 1, size(q1, 3)
                  if (q1(I,J,K) > x1) then
                    q_out(I,J,K) = q1(I,J,K)
                  else
                    q_out(I,J,K) = x1
                  endif
                enddo 
              enddo 
            enddo 
  
          end function max_hdual_tens_dble
  
  
          function max_hdual_tens_dble_SPR(q1,x1) result(q_out)
            
            implicit none
            real(SPR), intent(in)                                              :: x1
            TYPE(hyperdual), dimension(:,:,:), intent(in)                     :: q1
            TYPE(hyperdual), dimension(size(q1,1),size(q1,2),size(q1,3))      :: q_out
            Integer                                                           :: I,J,K
  
            do I = 1, size(q1,1)
              do J = 1, size(q1, 2)
                do K = 1, size(q1, 3)
                  if (q1(I,J,K) > REAL(x1,PR)) then
                    q_out(I,J,K) = q1(I,J,K)
                  else
                    q_out(I,J,K) = REAL(x1,PR)
                  endif
                enddo 
              enddo 
            enddo 
  
          end function max_hdual_tens_dble_SPR
  
  
          ! function max_dble_hdual_tens(x1,q1) result(q_out)
            
          !   implicit none
          !   real(PR), intent(in)                                            :: x1
          !   TYPE(hyperdual), dimension(:,:,:), intent(in)                   :: q1
          !   TYPE(hyperdual), dimension(size(q1,1),size(q1,2),size(q1,3))    :: q_out
          !   Integer                                                         :: I,J,K
  
          !   do I = 1, size(q1,1)
          !     do J = 1, size(q1, 2)
          !       do K = 1, size(q1, 3)
          !         if (q1(I,J,K) > x1) then
          !           q_out(I,J,K) = q1(I,J,K)
          !         else
          !           q_out(I,J,K) = REAL(x1,PR)
          !         endif
          !       enddo
          !     enddo 
          !   enddo 
  
          ! end function max_dble_hdual_tens
  
  
  
          ! function max_dble_hdual_tens_SPR(x1,q1) result(q_out)
            
          !   implicit none
          !   real(SPR), intent(in)                                            :: x1
          !   TYPE(hyperdual), dimension(:,:,:), intent(in)                     :: q1
          !   TYPE(hyperdual), dimension(size(q1,1),size(q1,2),size(q1,3))    :: q_out
          !   Integer                                                         :: I,J,K
  
          !   do I = 1, size(q1,1)
          !     do J = 1, size(q1, 2)
          !       do K = 1, size(q1, 3)
          !         if (q1(I,J,K) > REAL(x1,PR)) then
          !           q_out(I,J,K) = q1(I,J,K)
          !         else
          !           q_out(I,J,K) = REAL(x1,PR)
          !         endif
          !       enddo
          !     enddo 
          !   enddo 
  
          ! end function max_dble_hdual_tens_SPR
  
  
  
          function max_hdual_4d_dble(q1,x1) result(q_out)
  
            ! some function use max as maxval, for 4d hyperdual array
  
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q1
            Integer                                         :: I,J,K,L
            Real(PR)                                        :: x1
            TYPE(hyperdual), &
              dimension(size(q1,1),size(q1,2),size(q1,3),size(q1,4))   :: q_out
  
            do I = 1,size(q1, 1)
              do J = 1,size(q1, 2)
                do K = 1,size(q1, 3)
                  do L = 1,size(q1, 4)
                    if (q1(I,J,K,L) > x1) then
                      q_out(I,J,K,L) = q1(I,J,K,L)
                    else
                      q_out(I,J,K,L) = x1
                    endif
                  enddo
                enddo
              enddo
            enddo
  
          end function max_hdual_4d_dble
  
  
          ! function max_dble_hdual_4d(x1,q1) result(q_out)
  
    
          !   implicit none
          !   TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q1
          !   Integer                                         :: I,J,K,L
          !   Real(PR)                                        :: x1
          !   TYPE(hyperdual), &
          !     dimension(size(q1,1),size(q1,2),size(q1,3),size(q1,4))   :: q_out
  
          !   do I = 1,size(q1, 1)
          !     do J = 1,size(q1, 2)
          !       do K = 1,size(q1, 3)
          !         do L = 1,size(q1, 4)
          !           if (q1(I,J,K,L) > x1) then
          !             q_out(I,J,K,L) = q1(I,J,K,L)
          !           else
          !             q_out(I,J,K,L) = x1
          !           endif
          !         enddo
          !       enddo
          !     enddo
          !   enddo
  
          ! end function max_dble_hdual_4d
  
  
          function max_hdual_4d_dble_SPR(q1,x1) result(q_out)
  
            ! some function use max as maxval, for 4d hyperdual array
  
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q1
            Integer                                         :: I,J,K,L
            Real(SPR)                                        :: x1
            TYPE(hyperdual), &
              dimension(size(q1,1),size(q1,2),size(q1,3),size(q1,4))   :: q_out
  
            do I = 1,size(q1, 1)
              do J = 1,size(q1, 2)
                do K = 1,size(q1, 3)
                  do L = 1,size(q1, 4)
                    if (q1(I,J,K,L) > x1) then
                      q_out(I,J,K,L) = q1(I,J,K,L)
                    else
                      q_out(I,J,K,L) = REAL(x1,PR)
                    endif
                  enddo
                enddo
              enddo
            enddo
  
          end function max_hdual_4d_dble_SPR
  
  
  
          ! function max_dble_hdual_4d_SPR(x1,q1) result(q_out)
  
          !   ! some function use max as maxval, for 4d hyperdual array
  
          !   implicit none
          !   TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q1
          !   Integer                                         :: I,J,K,L
          !   Real(SPR)                                        :: x1
          !   TYPE(hyperdual), &
          !     dimension(size(q1,1),size(q1,2),size(q1,3),size(q1,4))   :: q_out
  
          !   do I = 1,size(q1, 1)
          !     do J = 1,size(q1, 2)
          !       do K = 1,size(q1, 3)
          !         do L = 1,size(q1, 4)
          !           if (q1(I,J,K,L) > x1) then
          !             q_out(I,J,K,L) = q1(I,J,K,L)
          !           else
          !             q_out(I,J,K,L) = REAL(x1,PR)
          !           endif
          !         enddo
          !       enddo
          !     enddo
          !   enddo
  
          ! end function max_dble_hdual_4d_SPR
  
  
  
          function max_hdual_array_hdual_array(q1, q2) result(q_out)
  
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: q1
            TYPE(hyperdual), dimension(:), intent(in) :: q2
            TYPE(hyperdual), dimension(size(q1))      :: q_out
            Integer                                   :: I
  
            do I = 1, size(q1)
              if (q1(I) > q2(I)) then
                q_out(I) = q1(I)
              else 
                q_out(I) = q2(I)
              endif
            enddo 
            
          end function max_hdual_array_hdual_array 
  
          
        !----- MIN
          function min_hdual_hdual(q1, q2) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q1
            TYPE(hyperdual), intent(in) :: q2
            TYPE(hyperdual)             :: q_out
  
            if (q1.LE.q2) then
              q_out = q1
            else
              q_out = q2
            endif
  
          end function min_hdual_hdual
  
  
          function min_hdual_dble(q1, x2) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q1
            real(PR), intent(in)        :: x2
            TYPE(hyperdual)             :: q_out
  
            if (q1.LE.x2) then
              q_out = q1
            else
              q_out = x2
            endif
  
          end function min_hdual_dble
  
  
          function min_hdual_dble_SPR(q1, x2) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q1
            real(SPR), intent(in)        :: x2
            TYPE(hyperdual)             :: q_out
  
            if (q1.LE.REAL(x2,PR)) then
              q_out = q1
            else
              q_out = REAL(x2,PR)
            endif
  
          end function min_hdual_dble_SPR
  
          
          function min_dble_hdual(x1, q2) result(q_out)
  
            implicit none
            real(PR), intent(in)        :: x1
            TYPE(hyperdual), intent(in) :: q2
            TYPE(hyperdual)             :: q_out
  
            if (x1.LE.q2) then
              q_out = x1
            else
              q_out = q2
            endif
  
          end function min_dble_hdual
  
  
          function min_dble_hdual_SPR(x1, q2) result(q_out)
  
            implicit none
            real(SPR), intent(in)        :: x1
            TYPE(hyperdual), intent(in) :: q2
            TYPE(hyperdual)             :: q_out
  
            if (REAL(x1,PR).LE.q2) then
              q_out = REAL(x1,PR)
            else
              q_out = q2
            endif
  
          end function min_dble_hdual_SPR
  
      
      function min_hdual_four(q1, q2, q3, q4) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in)   :: q1,q2,q3,q4
            integer                       :: I
            TYPE(hyperdual), dimension(3) :: qlst
            TYPE(hyperdual)               :: q_out
          
          q_out = q1
          qlst(1) = q2
          qlst(2) = q3
          qlst(3) = q4
          
          do I = 1, 3
            if (qlst(I) .LE. q_out) then
              q_out = qlst(I)
            endif
          enddo 
            
          
          end function min_hdual_four
  
  
          function min_hdual_five(q1, q2, q3, q4, q5) result(q_out)
  
            implicit none
            TYPE(hyperdual), intent(in)   :: q1,q2,q3,q4,q5
            integer                       :: I
            TYPE(hyperdual), dimension(4) :: qlst
            TYPE(hyperdual)               :: q_out
          
          q_out = q1
          qlst(1) = q2
          qlst(2) = q3
          qlst(3) = q4
          qlst(4) = q5
          
          do I = 1, 4
            if (qlst(I) .LE. q_out) then
              q_out = qlst(I)
            endif
          enddo 
            
          
          end function min_hdual_five
          
  
        !----- Maxval
          function maxval_hdual_array(X_in) result(val_out)
  
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: X_in
            TYPE(hyperdual)                           :: val_out
            integer :: i
  
            val_out = X_in(1)
            do i = 1, size(X_in)
              val_out = max(val_out, X_in(i))
            enddo
  
          end function

          !----- Minval
          function minval_hdual_4d(X_in) result(val_out)
  
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: X_in
            TYPE(hyperdual)                                 :: val_out
            integer :: i, j, k, l
  
            val_out = X_in(1,1,1,1)
            do i = 1, size(X_in, 1)
              do j = 1, size(X_in, 2)
                do k = 1, size(X_in, 3)
                  do l = 1,size(X_in, 4)
                    val_out = min(val_out, X_in(i,j,k,l))
                  enddo
                enddo 
              enddo 
            enddo 

          end function 
  
  
          !----- Matmul
          function matmul_hdual_array_hdual_matrix(qleft, qright) result(res)
          
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
          
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: qleft
            real(PR), dimension(:,:), intent(in)      :: xright
            TYPE(hyperdual), dimension(size(qleft))   :: res
  
            res%x     = matmul(qleft%x,xright)
            res%dx1   = matmul(qleft%dx1,xright)
            res%dx2   = matmul(qleft%dx2,xright)
            res%dx1x2 = matmul(qleft%dx1x2,xright)
  
          end function matmul_hdual_array_dble_matrix
  
  
          function matmul_hdual_array_dble_matrix_SPR(qleft, xright) result(res)
          
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: qleft
            real(SPR), dimension(:,:), intent(in)      :: xright
            TYPE(hyperdual), dimension(size(qleft))   :: res
  
            res%x     = matmul(qleft%x, REAL(xright,PR))
            res%dx1   = matmul(qleft%dx1, REAL(xright,PR))
            res%dx2   = matmul(qleft%dx2, REAL(xright,PR))
            res%dx1x2 = matmul(qleft%dx1x2, REAL(xright,PR))
  
          end function matmul_hdual_array_dble_matrix_SPR
  
  
          function matmul_hdual_matrix_hdual_array(qleft, qright) result(res)
          
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
          
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            TYPE(hyperdual), dimension(:,:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(qleft,1), size(qright,2)) :: res
  
            res%x     = matmul(qleft%x,qright%x)
            res%dx1   = matmul(qleft%x,qright%dx1) + matmul(qleft%dx1,qright%x)
            res%dx2   = matmul(qleft%x,qright%dx2) + matmul(qleft%dx2,qright%x)
            res%dx1x2 = matmul(qleft%x,qright%dx1x2) + matmul(qleft%dx1,qright%dx2) + matmul(qleft%dx2,qright%dx1) &
            + matmul(qleft%dx1x2,qright%x)
  
          end function matmul_hdual_matrix_hdual_matrix
  
  
          function matmul_hdual_matrix_dble_array(qleft, xright) result(res)
  
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            real(PR), dimension(:), intent(in)          :: xright
            TYPE(hyperdual), dimension(size(qleft,1))   :: res
  
            res%x     = matmul(qleft%x,xright)
            res%dx1   = matmul(qleft%dx1,xright)
            res%dx2   = matmul(qleft%dx2,xright)
            res%dx1x2 = matmul(qleft%dx1x2,xright)
  
          end function matmul_hdual_matrix_dble_array 
  
  
          function matmul_hdual_matrix_dble_array_SPR(qleft, xright) result(res)
  
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            real(SPR), dimension(:), intent(in)          :: xright
            TYPE(hyperdual), dimension(size(qleft,1))   :: res
  
            res%x     = matmul(qleft%x,REAL(xright,PR))
            res%dx1   = matmul(qleft%dx1,REAL(xright,PR))
            res%dx2   = matmul(qleft%dx2,REAL(xright,PR))
            res%dx1x2 = matmul(qleft%dx1x2,REAL(xright,PR))
  
          end function matmul_hdual_matrix_dble_array_SPR 
  
  
          function matmul_hdual_matrix_dble_matrix(qleft, xright) result(res)
          
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            real(PR), dimension(:,:), intent(in)        :: xright
            TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
            res%x     = matmul(qleft%x,xright)
            res%dx1   = matmul(qleft%dx1,xright)
            res%dx2   = matmul(qleft%dx2,xright)
            res%dx1x2 = matmul(qleft%dx1x2,xright)
  
          end function matmul_hdual_matrix_dble_matrix 
  
  
          function matmul_hdual_matrix_dble_matrix_SPR(qleft, xright) result(res)
          
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            real(SPR), dimension(:,:), intent(in)        :: xright
            TYPE(hyperdual), dimension(size(qleft,1), size(qleft,2))  :: res
  
            res%x     = matmul(qleft%x,REAL(xright,PR))
            res%dx1   = matmul(qleft%dx1,REAL(xright,PR))
            res%dx2   = matmul(qleft%dx2,REAL(xright,PR))
            res%dx1x2 = matmul(qleft%dx1x2,REAL(xright,PR))
  
          end function matmul_hdual_matrix_dble_matrix_SPR
  
  
          function matmul_dble_array_hdual_matrix(xleft, qright) result(res)
          
            implicit none
            real(PR), dimension(:), intent(in)          :: xleft
            TYPE(hyperdual), dimension(:,:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(xleft))     :: res
  
            res%x     = matmul(xleft,qright%x)
            res%dx1   = matmul(xleft,qright%dx1)
            res%dx2   = matmul(xleft,qright%dx2)
            res%dx1x2 = matmul(xleft,qright%dx1x2)
  
          end function matmul_dble_array_hdual_matrix 
  
  
          function matmul_dble_array_hdual_matrix_SPR(xleft, qright) result(res)
          
            implicit none
            real(SPR), dimension(:), intent(in)          :: xleft
            TYPE(hyperdual), dimension(:,:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(xleft))     :: res
  
            res%x     = matmul(REAL(xleft,PR),qright%x)
            res%dx1   = matmul(REAL(xleft,PR),qright%dx1)
            res%dx2   = matmul(REAL(xleft,PR),qright%dx2)
            res%dx1x2 = matmul(REAL(xleft,PR),qright%dx1x2)
  
          end function matmul_dble_array_hdual_matrix_SPR 
  
  
          function matmul_dble_matrix_hdual_array(xleft, qright) result(res)
          
            implicit none
            real(PR), dimension(:,:), intent(in)      :: xleft
            TYPE(hyperdual), dimension(:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(qright))  :: res
  
            res%x     = matmul(xleft,qright%x)
            res%dx1   = matmul(xleft,qright%dx1)
            res%dx2   = matmul(xleft,qright%dx2)
            res%dx1x2 = matmul(xleft,qright%dx1x2)
  
          end function matmul_dble_matrix_hdual_array 
  
  
          function matmul_dble_matrix_hdual_array_SPR(xleft, qright) result(res)
          
            implicit none
            real(SPR), dimension(:,:), intent(in)      :: xleft
            TYPE(hyperdual), dimension(:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(qright))  :: res
  
            res%x     = matmul(REAL(xleft,PR),qright%x)
            res%dx1   = matmul(REAL(xleft,PR),qright%dx1)
            res%dx2   = matmul(REAL(xleft,PR),qright%dx2)
            res%dx1x2 = matmul(REAL(xleft,PR),qright%dx1x2)
  
          end function matmul_dble_matrix_hdual_array_SPR
  
  
          function matmul_dble_matrix_hdual_matrix(xleft, qright) result(res)
          
            implicit none
            real(PR), dimension(:,:), intent(in)        :: xleft
            TYPE(hyperdual), dimension(:,:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(qright,1),size(qright,2))  :: res
  
            res%x     = matmul(xleft,qright%x)
            res%dx1   = matmul(xleft,qright%dx1)
            res%dx2   = matmul(xleft,qright%dx2)
            res%dx1x2 = matmul(xleft,qright%dx1x2)
  
          end function matmul_dble_matrix_hdual_matrix 
  
  
          function matmul_dble_matrix_hdual_matrix_SPR(xleft, qright) result(res)
          
            implicit none
            real(PR), dimension(:,:), intent(in)        :: xleft
            TYPE(hyperdual), dimension(:,:), intent(in) :: qright
            TYPE(hyperdual), dimension(size(qright,1),size(qright,2))  :: res
  
            res%x     = matmul(xleft,qright%x)
            res%dx1   = matmul(xleft,qright%dx1)
            res%dx2   = matmul(xleft,qright%dx2)
            res%dx1x2 = matmul(xleft,qright%dx1x2)
  
          end function matmul_dble_matrix_hdual_matrix_SPR
  
  
          !----- Dot Product
        function dot_product_hdual_array_hdual_array(qleft, qright) result(res)
          
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
          
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: qleft
            real(PR), dimension(:), intent(in)        :: xright
            TYPE(hyperdual)                           :: res
  
            res%x     = dot_product(qleft%x,xright)
            res%dx1   = dot_product(qleft%dx1,xright)
            res%dx2   = dot_product(qleft%dx2,xright)
            res%dx1x2 = dot_product(qleft%dx1x2,xright)
  
          end function dot_product_hdual_array_dble_array
  
  
          function dot_product_hdual_array_dble_array_SPR(qleft, xright) result(res)
          
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: qleft
            real(SPR), dimension(:), intent(in)        :: xright
            TYPE(hyperdual)                           :: res
  
            res%x     = dot_product(qleft%x,REAL(xright,PR))
            res%dx1   = dot_product(qleft%dx1,REAL(xright,PR))
            res%dx2   = dot_product(qleft%dx2,REAL(xright,PR))
            res%dx1x2 = dot_product(qleft%dx1x2,REAL(xright,PR))
  
          end function dot_product_hdual_array_dble_array_SPR
  
          
          function dot_product_dble_array_hdual_array(xleft, qright) result(res)
          
            implicit none
            real(PR), dimension(:), intent(in)        :: xleft
            TYPE(hyperdual), dimension(:), intent(in) :: qright
            TYPE(hyperdual)                           :: res
  
            res%x     = dot_product(xleft,qright%x)
            res%dx1   = dot_product(xleft,qright%dx1)
            res%dx2   = dot_product(xleft,qright%dx2)
            res%dx1x2 = dot_product(xleft,qright%dx1x2)
  
          end function dot_product_dble_array_hdual_array
  
  
          function dot_product_dble_array_hdual_array_SPR(xleft, qright) result(res)
          
            implicit none
            real(SPR), dimension(:), intent(in)        :: xleft
            TYPE(hyperdual), dimension(:), intent(in) :: qright
            TYPE(hyperdual)                           :: res
  
            res%x     = dot_product(REAL(xleft,PR),qright%x)
            res%dx1   = dot_product(REAL(xleft,PR),qright%dx1)
            res%dx2   = dot_product(REAL(xleft,PR),qright%dx2)
            res%dx1x2 = dot_product(REAL(xleft,PR),qright%dx1x2)
  
          end function dot_product_dble_array_hdual_array_SPR
  
  
          ! Hyperdual Sqrt
          function hdsqrt(q) result(qsqrt)
            
            implicit none
            TYPE(hyperdual), intent(in) :: q
            TYPE(hyperdual)             :: qsqrt
    
            qsqrt = hdual_pow_dble(q, 0.5_PR)
  
          end function hdsqrt
  
          
          ! Hyperdual exponential
          function hdexp(q) result(qexp)
            
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
  
  
          function hdlog10(q) result(qlog10)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q
            TYPE(hyperdual)             :: qlog10
  
            qlog10 = hdlog(q) / log(10.0_PR)
  
          end function hdlog10
  
  
          function hdcos(q) result(qcos)
            
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
  
  
          function hdnint(q) result (nearint)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q
            integer                     :: nearint
            real(PR)                    :: funval
  
            funval = q%x
            nearint = NINT(funval)
              
          end function hdnint
  
  
          function hdual_cast(x1) result(q)
            
            !! Convert real numbers to hyperdual numbers
            implicit none
            real(PR), intent(in)   :: x1
            TYPE(hyperdual)        :: q
  
            q%x = REAL(x1, PR)
            q%dx1 = 0
            q%dx2 = 0
            q%dx1x2 = 0
  
          end function hdual_cast


          function hdual_cast_array(x1) result(q)
            
            !! Convert real numbers to hyperdual numbers
            implicit none
            real(PR), dimension(:), intent(in)   :: x1
            TYPE(hyperdual), dimension(size(x1)) :: q
  
            q%x = REAL(x1, PR)
            q%dx1 = 0
            q%dx2 = 0
            q%dx1x2 = 0
  
          end function hdual_cast_array

          
          function hdual_cast_matrix(x1) result(q)
            
            !! Convert real numbers to hyperdual numbers
            implicit none
            real(PR), dimension(:,:), intent(in)   :: x1
            TYPE(hyperdual), dimension(size(x1, 1), size(x1, 2))   :: q
  
            q%x = REAL(x1, PR)
            q%dx1 = 0
            q%dx2 = 0
            q%dx1x2 = 0
  
          end function hdual_cast_matrix
  
  
          function hdsum(q) result(qsum)
            ! 1D array -> single number
            implicit none
            TYPE(hyperdual), dimension(:), intent(in) :: q
            TYPE(hyperdual)                           :: qsum
  
            qsum%x = sum(q%x)
            qsum%dx1 = sum(q%dx1)
            qsum%dx2 = sum(q%dx2)
            qsum%dx1x2 = sum(q%dx1x2)
  
          end function hdsum
  
          function hdsum_2d(q) result(qsum)
            ! 2D array -> single number
            implicit none
            TYPE(hyperdual), dimension(:, :), intent(in) :: q
            TYPE(hyperdual)                              :: qsum
  
            qsum%x = sum(q%x)
            qsum%dx1 = sum(q%dx1)
            qsum%dx2 = sum(q%dx2)
            qsum%dx1x2 = sum(q%dx1x2)
  
          end function hdsum_2d
  
  
          function hdsum_3d(q) result(qsum)
            ! 3D array -> single number
            implicit none
            TYPE(hyperdual), dimension(:,:,:), intent(in) :: q
            TYPE(hyperdual)                                 :: qsum
  
            qsum%x = sum(q%x)
            qsum%dx1 = sum(q%dx1)
            qsum%dx2 = sum(q%dx2)
            qsum%dx1x2 = sum(q%dx1x2)
  
          end function hdsum_3d
  
  
          function hdsum_4d(q) result(qsum)
            ! 3D array -> single number
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q
            TYPE(hyperdual)                                 :: qsum
  
            qsum%x = sum(q%x)
            qsum%dx1 = sum(q%dx1)
            qsum%dx2 = sum(q%dx2)
            qsum%dx1x2 = sum(q%dx1x2)
  
          end function hdsum_4d
        
  
      
          function hdsum_2d_dim(q, dim) result(qsum)
  
            ! 2d --> 1d, reduction along a dimension
            
            implicit none
            TYPE(hyperdual), dimension(:,:), intent(in) :: q
            Integer                                     :: dim
            Integer                                     :: c_dim
            TYPE(hyperdual), allocatable, dimension(:)  :: qsum
      
            c_dim = change_dim_2d(dim)
            allocate(qsum(size(q, c_dim)))
      
            qsum%x = sum(q%x, dim)
            qsum%dx1 = sum(q%dx1, dim)
            qsum%dx2 = sum(q%dx2, dim)
            qsum%dx1x2 = sum(q%dx1x2, dim)
  
          end function hdsum_2d_dim
  
  
          function hdsum_3d_dim(q, dim) result(qsum)
  
            ! 3d --> 2d, reduction along a dimension
            
            implicit none
            TYPE(hyperdual), dimension(:,:,:), intent(in) :: q
            Integer                                       :: dim
            Integer, dimension(2)                         :: c_dim
            TYPE(hyperdual), allocatable, dimension(:, :) :: qsum
            
            c_dim = change_dim_3d(dim)
            allocate(qsum(size(q, c_dim(1)), size(q, c_dim(2))))
  
            qsum%x = sum(q%x, dim)
            qsum%dx1 = sum(q%dx1, dim)
            qsum%dx2 = sum(q%dx2, dim)
            qsum%dx1x2 = sum(q%dx1x2, dim)
  
          end function hdsum_3d_dim
  
  
          function hdsum_4d_dim(q, dim) result(qsum)
  
            ! 4d --> 3d, reduction along a dimension
            
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:), intent(in) :: q
            Integer                                         :: dim
            Integer, dimension(3)                           :: c_dim
            TYPE(hyperdual), allocatable, dimension(:,:,:)  :: qsum
            
            c_dim = change_dim_4d(dim)
            
            allocate(qsum((size(q, c_dim(1))), size(q, c_dim(2)), size(q, c_dim(3))))
  
            qsum%x = sum(q%x, dim)
            qsum%dx1 = sum(q%dx1, dim)
            qsum%dx2 = sum(q%dx2, dim)
            qsum%dx1x2 = sum(q%dx1x2, dim)
  
          end function hdsum_4d_dim
  
          function hdsum_5d_dim(q, dim) result(qsum)
  
            ! 5d --> 4d, reduction along a dimension
            
            implicit none
            TYPE(hyperdual), dimension(:,:,:,:,:), intent(in) :: q
            Integer                                         :: dim
            Integer, dimension(4)                           :: c_dim
            TYPE(hyperdual), allocatable, dimension(:,:,:,:)  :: qsum
            
            c_dim = change_dim_5d(dim)
            
            allocate(qsum((size(q, c_dim(1))), size(q, c_dim(2)), size(q, c_dim(3)), size(q, c_dim(4))))
  
            qsum%x = sum(q%x, dim)
            qsum%dx1 = sum(q%dx1, dim)
            qsum%dx2 = sum(q%dx2, dim)
            qsum%dx1x2 = sum(q%dx1x2, dim)
  
          end function hdsum_5d_dim
  
  
          function hdsum_mask(q, mask) result(qsum) 
            ! take the sum of an hyperdual array --> one hyperdual
            implicit none
            TYPE(hyperdual), dimension(:), intent(in)     :: q
            Logical, dimension(size(q)), intent(in)       :: mask
            TYPE(hyperdual)                               :: qsum
            integer                                       :: i
  
            do i = 1, size(q) 
              if (mask(i) .eqv. .TRUE. ) then 
                qsum%x = qsum%x + q(i)%x
                qsum%dx1 = qsum%dx1 + q(i)%dx1
                qsum%dx2 = qsum%dx2 + q(i)%dx2
                qsum%dx1x2 = qsum%dx1x2 + q(i)%dx1x2
              endif 
            enddo 
          end function hdsum_mask 
  
            function hdsum_mask_scalar(q, mask) result(qsum) 
              ! take the sum of an hyperdual array --> one hyperdual
              implicit none
              TYPE(hyperdual), dimension(:), intent(in)     :: q
              Logical, intent(in)                           :: mask
              TYPE(hyperdual)                               :: qsum
              integer                                       :: i
  
              if (mask .eqv. .TRUE.) then 
                qsum = hdsum(q)
              else
                qsum = hyperdual(0.0d0, 0.0d0, 0.0d0, 0.0d0)
              endif  
          
            end function hdsum_mask_scalar
  
  
            ! function hdsum_2d_dim_mask(q, dim, mask) result(qsum) 
            !   ! take the sum of an hyperdual array --> one hyperdual
            !   implicit none
            !   TYPE(hyperdual), dimension(:,:), intent(in)     :: q
            !   Logical, dimension(size(q)), intent(in)         :: mask
            !   TYPE(hyperdual)                                 :: qsum
            !   integer                                         :: i
  
            !   do i = 1, size(q) 
            !     if (mask(i) .eqv. .TRUE. ) then 
            !       qsum%x = qsum%x + q(i)%x
            !       qsum%dx1 = qsum%dx1 + q(i)%dx1
            !       qsum%dx2 = qsum%dx2 + q(i)%dxÍ2
            !       qsum%dx1x2 = qsum%dx1x2 + q(i)%dx1x2
            !     endif 
            !   enddo 
            ! end function hdsum_2d_dim_mask 
  
          function change_dim_2d(dim) result(outdim) 
            implicit none
            Integer, INTENT(IN) :: dim
            Integer             :: outdim
  
            if (dim == 1) then
              outdim = 2
            else
              outdim = 1
            endif 
            
          end function change_dim_2d
  !
  
          function change_dim_3d(dim) result(outdim)
            implicit none
            Integer, INTENT(IN)   :: dim
            Integer, dimension(2) :: outdim
            if (dim == 1) then
              outdim = (/ 2,3 /)
            else if (dim == 2) then
              outdim = (/ 1,3 /)
            else
              outdim = (/ 1,2 /)
            endif 
              
          end function change_dim_3d
  
  
          function change_dim_4d(dim) result(outdim)
            implicit none
            Integer, INTENT(IN)   :: dim
            Integer, dimension(3) :: outdim
            if (dim == 1) then
              outdim = (/ 2,3,4 /)
            else if (dim == 2) then
              outdim = (/ 1,3,4 /)
            else if (dim == 3) then
              outdim = (/ 1,2,4 /)
            else
              outdim = (/ 1,2,3 /)
            endif 
              
          end function change_dim_4d
  
          function change_dim_5d(dim) result(outdim)
            implicit none
            Integer, INTENT(IN)   :: dim
            Integer, dimension(4) :: outdim
            if (dim == 1) then
              outdim = (/ 2,3,4,5 /)
            else if (dim == 2) then
              outdim = (/ 1,3,4,5 /)
            else if (dim == 3) then
              outdim = (/ 1,2,4,5 /)
            else if (dim == 4) then
              outdim = (/ 1,2,3,5 /)
            else
              outdim = (/ 1,2,3,4 /)
            endif 
              
          end function change_dim_5d
  
  
          function real_hdual(qleft) result(xright)
            ! convert hyperdual to real, throw away the sensitivity information
            ! need to be very careful when using
  
            TYPE(hyperdual), intent(in) :: qleft
            real(PR)                    :: xright
  
            xright = qleft%x
  
          end function real_hdual


          function real_hdual_array(qleft) result(xright)
            ! convert hyperdual to real, throw away the sensitivity information
            ! need to be very careful when using
  
            TYPE(hyperdual), dimension(:), intent(in) :: qleft
            real(PR), dimension(size(qleft))          :: xright
  
            xright = qleft%x
  
          end function real_hdual_array


          function real_hdual_matrix(qleft) result(xright)
            ! convert hyperdual to real, throw away the sensitivity information
            ! need to be very careful when using
  
            TYPE(hyperdual), dimension(:,:), intent(in) :: qleft
            real(PR), dimension(size(qleft,1), size(qleft,2))  :: xright
  
            xright = qleft%x
  
          end function real_hdual_matrix
  
  
          function int_hdual(qleft) result(iright)
            ! convert hyperdual to int, throw away the sensitivity information
            ! need to be very careful when using
            
            TYPE(hyperdual), intent(in) :: qleft
            integer                     :: iright
            
            iright = int(qleft%x)
            
          end function int_hdual
      
          
          function hderf(q) result(res)
  
            ! Error Function overloading
            implicit none
            TYPE(hyperdual), intent(in) :: q
            real(PR)             :: deriv1
            real(PR)             :: deriv2
            real(PR)             :: funval
            TYPE(hyperdual)      :: res
  
            ! First and second order derivative of erf function
            deriv1 = 2.0_PR / SQRT(pi) * exp( -q%x**2.0_PR)
            deriv2 = -4.0_PR / SQRT(pi) * q%x * exp( -q%x**2.0_PR)
            funval = erf(q%x)
  
            res%x = funval
            res%dx1 = deriv1 * q%dx1
            res%dx2 = deriv1 * q%dx2
            res%dx1x2 = deriv2 * q%dx1 * q%dx2 + deriv1 * q%dx1x2
  
  
          end function hderf
  
  
          function hderfc(q) result(res)
  
            implicit none
            TYPE(hyperdual), intent(in) :: q
            TYPE(hyperdual)             :: res
  
            res = 1 - erf(q)
  
          end function hderfc
      
      end module HDMod
  
  