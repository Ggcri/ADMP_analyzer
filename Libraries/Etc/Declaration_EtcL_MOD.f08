module Etc_utilities_declaration
    use iso_fortran_env, only : int8
    implicit none 
! PARSING ACTUAL ARGUMENTS 
    type opt_flags
        character(len=:),allocatable :: label
        logical :: flag=.false.
    end type opt_flags
    integer,parameter :: opt_num= 6
    type(opt_flags),dimension(opt_num) :: Opt_vec
    logical :: PARSING_FLAG
    integer :: Ref_f
    integer(kind=int8),dimension(:),allocatable :: Atom_ids
    character(len=132) :: Gnu_opt
! GNU_plot actual arguments
    logical :: Gnu_flag 
    logical :: Op_flag 

end module Etc_utilities_declaration
