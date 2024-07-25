module ADMP_INPUT_MOD 
use, non_intrinsic :: KIND_MOD, only : i1b,i2b
use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
use, intrinsic :: iso_fortran_env, only : real32,real64,real128

    character(len=132) :: filename      ! .LOG GAUSSIN FILENAME 
    character(len=3) :: Units           ! UNITS IDENTIFIER FOR DISTANCE/ANGLES IN Z-MATRIX
    logical ::  IO_CHECK_ERROR ,&       ! FATAL ERROR FLAG FOR G_LOG_CHECK_SUBROUTINE 
                IO_INPUT_ERROR          ! FATAL ERROR FLAG FOR G_LOG_INPUT_SUBROUTINE

        ! G_LOG_INPUT ACTUAL ARGUMENTS 
    integer(kind=i1b) ::    At_Num      ! ATOM NUMBER
    integer(kind=i2b) ::    Step_Num    ! STEP NUMBER
    real(kind=real64),dimension(:),allocatable :: Coord     ! ATOM COORDINATES NUMBER
    private dpG_Log_Input,spG_Log_Input,qpG_Log_Input   ! ACCESSIBLE PROCEDURES ONLY BY MEAN OF GENERIC INTERFACE

    !INTERFACE TO SEPARATE MODULE SUBROUTINE
    interface
        module subroutine G_Log_Check(filename,IO_CHECK_Error,Normal_term,Units)
                !DUMMY SECTION 
                character(len=*), intent(in) :: filename
                logical, intent(out) :: IO_CHECK_ERROR,  &
                                        Normal_term
                character(len=7),intent(out), optional :: Units 

        end subroutine G_Log_check
    end interface
    
    interface G_Log_Input
        module subroutine dpG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
                integer(kind=i1b),intent(out) ::    At_Num      ! ATOM NUMBER
                integer(kind=i2b),intent(out) ::    Step_Num    ! STEP NUMBER
                real(kind=real64),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
                logical,intent(out) :: IO_INPUT_ERROR
                character(len=*), intent(in) :: filename    ! LOGFILENAME
                logical,intent(in) :: Normal_flag
        end subroutine dpG_Log_Input

        module subroutine spG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
            integer(kind=i1b),intent(out) ::    At_Num      ! ATOM NUMBER
            integer(kind=i2b),intent(out) ::    Step_Num    ! STEP NUMBER
            real(kind=real32),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
            logical,intent(out) :: IO_INPUT_ERROR
            character(len=*), intent(in) :: filename    ! LOGFILENAME
            logical,intent(in) :: Normal_flag
        end subroutine spG_Log_Input

        module subroutine qpG_Log_Input(filename,At_Num,Step_Num,Coord,vel,Normal_flag,IO_INPUT_ERROR)
            integer(kind=i1b),intent(out) ::    At_Num      ! ATOM NUMBER
            integer(kind=i2b),intent(out) ::    Step_Num    ! STEP NUMBER
            real(kind=real128),dimension(:),allocatable,intent(out) :: Coord,vel     ! ATOM COORDINATES NUMBER
            logical,intent(out) :: IO_INPUT_ERROR
            character(len=*), intent(in) :: filename    ! LOGFILENAME
            logical,intent(in) :: Normal_flag
        end subroutine qpG_Log_Input
    end interface G_Log_Input

    interface G_Bin_Input


    module subroutine spG_Bin_Input(Bin_filename,coord_vec,atom_ids)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real32),dimension(:),allocatable,intent(out) :: coord_vec 
        integer,dimension(:),intent(in),optional :: atom_ids

    end subroutine spG_Bin_Input


    module subroutine dpG_Bin_Input(Bin_filename,coord_vec,atom_ids)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real64),dimension(:),allocatable,intent(out) :: coord_vec 
        integer,dimension(:),intent(in),optional :: atom_ids

    end subroutine dpG_Bin_Input


    module subroutine qpG_Bin_Input(Bin_filename,coord_vec,atom_ids)
        character(len=*),intent(in) :: Bin_filename 
        real(kind=real128),dimension(:),allocatable,intent(out) :: coord_vec 
        integer,dimension(:),intent(in),optional :: atom_ids

    end subroutine qpG_Bin_Input

    end interface G_Bin_Input




end module ADMP_INPUT_MOD
