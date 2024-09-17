module  NUMERIC
  interface RMSD ! GENERIC INTERFACE TO HANDLE SINGLE,DOUPLE AND QUADRUPLE PRECISION MODE
        subroutine dp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
            use iso_fortran_env , only : real64,int8 
            ! DUMMY SECTION 
            real(kind=real64),intent(in), dimension(:),target :: coord 
            integer(kind=int8),intent(in) ::   Step_Num    ,&  ! MAXIMUM STEP NUMBER 
                                                Natoms        ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids 
            real(kind=real64),intent(out),dimension(:),allocatable :: rmsd_vec
            integer,intent(in),optional ::  REF_t           ! TIME STEP OF REFERENCE STRUCTURE
        end subroutine dp_RMSD

        subroutine sp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
            use iso_fortran_env , only : real32,int8 
            ! DUMMY SECTION 
            real(kind=real32),intent(in), dimension(:),target :: coord 
            integer(kind=int8),intent(in) ::   Step_Num    ,&  ! MAXIMUM STEP NUMBER 
                                                Natoms        ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids 
            real(kind=real32),intent(out),dimension(:),allocatable :: rmsd_vec
            integer,intent(in),optional ::  REF_t           ! TIME STEP OF REFERENCE STRUCTURE
        end subroutine sp_RMSD

        subroutine qp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
            use iso_fortran_env , only : real128,int8 
            ! DUMMY SECTION 
            real(kind=real128),intent(in), dimension(:),target :: coord 
            integer(kind=int8),intent(in) ::   Step_Num    ,&  ! MAXIMUM STEP NUMBER 
                                                Natoms        ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids 
            real(kind=real128),intent(out),dimension(:),allocatable :: rmsd_vec
            integer,intent(in),optional ::  REF_t           ! TIME STEP OF REFERENCE STRUCTURE
        end subroutine qp_RMSD        

    end interface RMSD



end module 
