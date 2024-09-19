module ADMP_INPUT_MOD 
use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
use, intrinsic :: iso_fortran_env, only : real32,real64,real128,int8,int16
private dpG_Log_Input,spG_Log_Input,qpG_Log_Input, &  ! ACCESSIBLE PROCEDURES ONLY BY MEAN OF GENERIC INTERFACE
        spG_Bin_Input,dpG_Bin_Input,qpG_Bin_Input
    !INTERFACE TO SEPARATE MODULE SUBROUTINE
    interface
        module subroutine G_Log_Check(filename,IO_CHECK_Error,Normal_term,Units)
                !DUMMY SECTION 
                character(len=*), intent(in) :: filename
                logical, intent(out) :: IO_CHECK_Error,  &
                                        Normal_term
                character(len=7),intent(out), optional :: Units 

        end subroutine G_Log_check
    end interface
    
    interface G_Log_Input
        module subroutine dpG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
                integer(kind=int8),intent(out) ::    At_Num      ! ATOM NUMBER
                integer(kind=int16),intent(out) ::    Step_Num    ! STEP NUMBER
                real(kind=real64),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
                logical,intent(out) :: IO_INPUT_ERROR
                character(len=*), intent(in) :: filename    ! LOGFILENAME
                logical,intent(in) :: Normal_flag
        end subroutine dpG_Log_Input

        module subroutine spG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
            integer(kind=int8),intent(out) ::    At_Num      ! ATOM NUMBER
            integer(kind=int16),intent(out) ::    Step_Num    ! STEP NUMBER
            real(kind=real32),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
            logical,intent(out) :: IO_INPUT_ERROR
            character(len=*), intent(in) :: filename    ! LOGFILENAME
            logical,intent(in) :: Normal_flag
        end subroutine spG_Log_Input

        module subroutine qpG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
            integer(kind=int8),intent(out) ::    At_Num      ! ATOM NUMBER
            integer(kind=int16),intent(out) ::    Step_Num    ! STEP NUMBER
            real(kind=real128),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
            logical,intent(out) :: IO_INPUT_ERROR
            character(len=*), intent(in) :: filename    ! LOGFILENAME
            logical,intent(in) :: Normal_flag
        end subroutine qpG_Log_Input
    end interface G_Log_Input

    interface G_Bin_Input


    module subroutine spG_Bin_Input(Bin_filename,coord_vec,vel_vec,Bin_in_flag,Natoms,Nsteps)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real32),dimension(:),allocatable,intent(out) :: coord_vec,vel_vec
        logical,intent(out) :: Bin_in_flag
        integer(kind=int8),intent(out) :: Natoms
        integer(kind=int16),intent(out) :: Nsteps
    end subroutine spG_Bin_Input

! TO MODIFY AFTER 

    module subroutine dpG_Bin_Input(Bin_filename,coord_vec,vel_vec,Bin_in_flag,Natoms,Nsteps)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real64),dimension(:),allocatable,intent(out) :: coord_vec,vel_vec 
        logical,intent(out) :: Bin_in_flag
        integer(kind=int8),intent(out) :: Natoms
        integer(kind=int16),intent(out) :: Nsteps
    end subroutine dpG_Bin_Input


    module subroutine qpG_Bin_Input(Bin_filename,coord_vec,vel_vec,Bin_in_flag,Natoms,Nsteps)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real128),dimension(:),allocatable,intent(out) :: coord_vec,vel_vec
        logical,intent(out) :: Bin_in_flag
        integer(kind=int8),intent(out) :: Natoms
        integer(kind=int16),intent(out) :: Nsteps

    end subroutine qpG_Bin_Input

    end interface G_Bin_Input




end module ADMP_INPUT_MOD
