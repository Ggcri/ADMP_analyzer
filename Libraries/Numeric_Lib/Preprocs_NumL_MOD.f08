module Numeric_preprocessing
    use, intrinsic :: iso_fortran_env, only : real32,real64,real128
implicit none 
!PREPROCESSING DIRECTIVES START FOR DECLARATION OF COORD AND VEL ARRAY 
#ifdef PREC  
#if (PREC == 2) 
   real(kind=real64),dimension(:),allocatable :: rmsd_vec     ! ATOM COORDINATES 
#elif (PREC == 4) 
   real(kind=real128),dimension(:),allocatable :: rmsd_vec     ! ATOM COORDINATES 
#elif (PREC == 1)
   real(kind=real32),dimension(:),allocatable :: rmsd_vec     ! ATOM COORDINATES 
#endif       
#else
   real(kind=real64),dimension(:),allocatable :: rmsd_vec     ! ATOM COORDINATES 
#endif

end module Numeric_preprocessing
