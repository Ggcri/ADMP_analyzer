module ADMP_OUTPUT
! THIS MODULE HANDLES THE OUTPUT SECTION OF DATA TRANSFER PART
! IT WILL CONTAIN THE INTERFACE FOR BIN_GEN SUBROUTINE 
! + FUTURE FEATURES 
private spBin_gen,dpBin_gen,qpBin_gen

    interface Bin_gen
        
        subroutine spBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
             use, intrinsic :: iso_fortran_env, only : real32,int8,int16
             use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
            real(kind=real32),dimension(:),intent(in) ::    coord, &    ! COORDINATES VECTOR 
                                                            vel         ! VELOCITY VECTOR
            character(len=*),intent(in) :: log_filename             ! LOG_FILENAME
            integer(kind=int8),intent(in) :: Natoms
            integer(kind=int16),intent(in) ::  Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag
            
        end subroutine spBin_gen
    
        subroutine dpBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
            use, intrinsic :: iso_fortran_env, only : real64,int8,int16
            use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
            real(kind=real64),dimension(:),intent(in) ::    coord, &       ! COORDINATES VECTOR 
                                                            vel         ! VELOCITY VECTOR
            character(len=*),intent(in) :: log_filename             ! LOG_FILENAME
            integer(kind=int8),intent(in) :: Natoms
            integer(kind=int16),intent(in) ::  Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag
        end subroutine dpBin_gen
    
        subroutine qpBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
            use, intrinsic :: iso_fortran_env, only : real128,int8,int16
            use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
            real(kind=real128),dimension(:),intent(in) ::   coord, &      ! COORDINATES VECTOR 
                                                            vel         ! VELOCITY VECTOR
            character(len=*),intent(in) :: log_filename             ! LOG_FILENAME
            integer(kind=int8),intent(in) :: Natoms
            integer(kind=int16),intent(in) ::  Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag
        end subroutine qpBin_gen

    end interface Bin_gen


end module ADMP_OUTPUT
