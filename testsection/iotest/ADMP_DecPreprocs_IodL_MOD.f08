module ADMP_DecPreprocs
    use, non_intrinsic :: KIND_MOD, only : i1b,i2b
    use, intrinsic :: iso_fortran_env, only : real32,real64,real128
implicit none 
!PREPROCESSING DIRECTIVES START FOR DECLARATION OF COORD AND VEL ARRAY 
#ifdef PREC  
#if (PREC == 2) 
   real(kind=real64),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
   real(kind=real64),dimension(:),allocatable :: vel       ! MASS SCALED VELOCITY          
 #elif (PREC == 4) 
   real(kind=real128),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
   real(kind=real128),dimension(:),allocatable :: vel       ! MASS SCALED VELOCITY         
 #elif (PREC == 1)
   real(kind=real32),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
   real(kind=real32),dimension(:),allocatable :: vel        ! MASS SCALED VELOCITY    
#endif      
#ifndef PREC
   real(kind=real64),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
   real(kind=real64),dimension(:),allocatable :: vel        ! MASS SCALED VELOCITY   
#endif       
#endif
! G_LOG_CHECK SUBROUTINE ACTUAL ARGUMENTS 
logical ::  IO_CHECK_ERROR=.FALSE.,  &  ! FATAL ERROR FLAG FOR G_LOG_CHECK_SUBROUTINE 
            Normal_term=.FALSE.         ! FLAG FOR NORMAL TERMINATION GAUSSIAN FILE
character(len=3) :: Units               ! UNITS IDENTIFIER FOR DISTANCE/ANGLES IN Z-MATRIX

! G_LOG_INPUT SUB ACTUAL ARGUMENTS 
integer(kind=i1b) :: At_Num             ! NUMBER OF ATOMS
integer(kind=i2b) :: Step_Num           ! NUMBER OG F STEP
logical :: IO_INPUT_ERROR=.FALSE.       ! FATAL ERROR FLAG FOR G_LOG_INPUT_SUBROUTINE 
! G_BIN_INPUT ACTUAL ARGUMENTS 
character(len=132) :: Bin_filename
! BIN_GEN ACTUAL ARGUMENTS
logical :: bingen_flag=.FALSE.

end module ADMP_DecPreprocs