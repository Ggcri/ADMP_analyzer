module  NUMERIC
  interface RMSD ! GENERIC INTERFACE TO HANDLE SINGLE,DOUPLE AND QUADRUPLE PRECISION MODE
        subroutine dp_RMSD (rmsd_vec,coord,Step_Num,Natoms,RMSDErrFlag,AT_ids,REF_t)
            use, intrinsic :: iso_fortran_env , only : real64,int8,int16,int8 !!! RICORDA DI CAMBIARE INT 8 A IB2 E USARE MODULO KIND
            ! DUMMY SECTION 
            real(kind=real64),intent(in), dimension(:) :: coord 
            integer(kind=int16),intent(inout) ::     Step_Num        ! MAXIMUM STEP NUMBER 
            integer(kind=int8),intent(inout) ::     Natoms          ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids ! out necessary since default value must be given
            integer(kind=int16),intent(in),optional ::  REF_t       ! TIME STEP OF REFERENCE STRUCTURE
            real(kind=real64),intent(out),dimension(:),allocatable :: rmsd_vec
            logical,intent(out) :: RMSDErrFlag
        end subroutine dp_RMSD

        subroutine sp_RMSD (rmsd_vec,coord,Step_Num,Natoms,RMSDErrFlag,AT_ids,REF_t)
            use, intrinsic :: iso_fortran_env , only : real32,int8,int16,int8 !!! RICORDA DI CAMBIARE INT 8 A IB2 E USARE MODULO KIND
            ! DUMMY SECTION 
            real(kind=real32),intent(in), dimension(:) :: coord 
            integer(kind=int16),intent(inout) ::     Step_Num        ! MAXIMUM STEP NUMBER 
            integer(kind=int8),intent(inout) ::     Natoms          ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids ! out necessary since default value must be given
            integer(kind=int16),intent(in),optional ::  REF_t       ! TIME STEP OF REFERENCE STRUCTURE
            real(kind=real32),intent(out),dimension(:),allocatable :: rmsd_vec
            logical,intent(out) :: RMSDErrFlag
        end subroutine sp_RMSD

        subroutine qp_RMSD (rmsd_vec,coord,Step_Num,Natoms,RMSDErrFlag,AT_ids,REF_t)
            use, intrinsic :: iso_fortran_env , only : real128,int8,int16,int8 !!! RICORDA DI CAMBIARE INT 8 A IB2 E USARE MODULO KIND
            ! DUMMY SECTION 
            real(kind=real128),intent(in), dimension(:) :: coord 
            integer(kind=int16),intent(inout) ::     Step_Num        ! MAXIMUM STEP NUMBER 
            integer(kind=int8),intent(inout) ::     Natoms          ! ATOM NUMBER
            integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids ! out necessary since default value must be given
            integer(kind=int16),intent(in),optional ::  REF_t       ! TIME STEP OF REFERENCE STRUCTURE
            real(kind=real128),intent(out),dimension(:),allocatable :: rmsd_vec
            logical,intent(out) :: RMSDErrFlag
        end subroutine qp_RMSD        

    end interface RMSD



end module 
