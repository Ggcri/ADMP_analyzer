module Etc_utilities_declaration
    use iso_fortran_env, only : int8,int16
    implicit none 
! PARSING ACTUAL ARGUMENTS 
    type opt_flags
      ! DATA TYPE TO HANLDE THE KEYWORDS TO PARSE
        character(len=:),allocatable :: label ! LABEL NAME OF THE KEYWORD
        logical :: flag=.false.               ! KEYWORD PRESENCE FLAG
    end type opt_flags
    integer,parameter :: opt_num= 6           ! NUMBER OF MANDATORY ARGUMENTS
    type(opt_flags),dimension(opt_num) :: Opt_vec ! ARRAY OF DERIVED TYPE, TO CONTAIN THE MANDATORY KEY-VAL OBJECT
    logical :: PARSING_FLAG,RMSDErrFlag
    integer :: test
    integer(int16) :: Ref_f ! REFERENCE FRAME OF THE DYNAMIC 
    integer(kind=int8),dimension(:),allocatable :: Atom_ids ! ATOM GROUP IDENTIFIER
    character(len=132) :: Gnu_opt ! STRING CONTAINING USER DEFINED GNU OPTIONS 
! GNU_plot actual arguments
    logical :: Gnu_flag ! FLAG FOR EXCEPTION HANDLING
    logical :: Op_flag  ! FLAG FOR EXCEPTION HANDLING

end module Etc_utilities_declaration
