program testing
    use, intrinsic :: iso_fortran_env, only : real64 
    implicit none 
! #ifdef _OPENMP
! #endif
! print *, 'parallel mode' 
! #ifdef PREC  
! #if PREC == 2
! print *, 'prec=2' 
!    real(kind=real64),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
!    real(kind=real64),dimension(:),allocatable :: vel       ! MASS SCALED VELOCITY          
! #elif PREC == 4 
! print *, 'prec=4' 
!    real(kind=real128),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
!    real(kind=real128),dimension(:),allocatable :: vel       ! MASS SCALED VELOCITY         
! #elif PREC == 1
! print *, 'prec=1' 
!    real(kind=real32),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
!    real(kind=real32),dimension(:),allocatable :: vel        ! MASS SCALED VELOCITY          
! #endif       
! #endif
! #ifndef PREC
! print *, 'prec=2' 
!    real(kind=real64),dimension(:),allocatable :: Coord     ! ATOM COORDINATES 
!    real(kind=real64),dimension(:),allocatable :: vel        ! MASS SCALED VELOCITY   
! #endif
    ! use, non_intrinsic :: etc_utilities_interface
    ! use, intrinsic :: iso_fortran_env, only : real64,compiler_version
    ! implicit none 
    ! real,dimension(10) :: random=1. 
    ! character(len=30) :: line1 
    ! integer :: vernum
    ! print *, 'starting parsing test' 
    ! call Parsing(Opt_vec,Gnu_opt, PARSING_FLAG,Atom_ids,Ref_f)
    ! if (PARSING_FLAG .eqv. .true.) then 
        !  STOP 'ERROR TERMINATION VIA PARSING SUBPROGRAM' 
    ! else 
        ! print *, 'atom id are',Atom_ids
        ! print *, 'no error ' 
    ! print *, Opt_vec(1)%label 
    ! print *, Opt_vec(2)%label
    ! print *, Opt_vec(3)%label
    ! print *, Opt_vec(4)%label
    ! print *, Opt_vec(5)%label
    ! print *, Opt_vec(6)%label
    ! print *, Opt_vec(7)%label
    ! end if
!    call Gnu_plot(random,Gnu_Flag,Opt_vec(5)%flag,Gnu_opt)
! 
character(len=100) :: filename 
filename= 'ciaone.log' 
select case  (filename( len_trim(filename)-3 : ) )

case ('.log')
print *, 'log' 

case ('.bin')

case default 
      print *, 'INVALID EXTENTION FOR FILE: ' , filename
      stop 
end select



end program testing
