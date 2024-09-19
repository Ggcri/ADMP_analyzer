module Etc_utilities_interface
    ! IT WILL BE SUFFICIENT TO USE ETC_UTILITIES_INTERFACE
    use Etc_utilities_declaration
    ! INTERFACES  
    
    interface
    function Up_to_low(string)
        character(len=*),intent(in) :: string 
        character(len=len_trim(string)) :: Up_to_low ! RESULT ACTUALLY IS NOT PART OF INTERFACE BUT I LIKE IT
    end function Up_to_low
    end interface
    
    interface 
    subroutine Parsing(Opt_vec,Gnu_opt,PARSING_FLAG,Atom_ids,Ref_f)
        import :: opt_flags,int8,int16 ! INTERFACES HAS NO HOST AND RECURSIVE USE ASSOCIATION NOT PERMITTED
        implicit none
        integer(int16),intent(out) :: Ref_f 
        logical,intent(out) :: PARSING_FLAG
        integer(kind=int8),intent(out),dimension(:),allocatable :: Atom_ids
        character(len=77),intent(out) :: Gnu_opt
        type(opt_flags),dimension(:),intent(out) :: Opt_vec 
        end subroutine 
    end interface

    interface Gnu_plot
        subroutine sGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,space_dist,x_val)
            use, intrinsic :: iso_fortran_env, only : real32
            implicit none 
            real(kind=real32),intent(in),dimension(:) :: fx            ! VECTOR WITH F(X) VALUES
            logical,intent(out) :: Gnu_Flag               ! SIGNALING FLAG FOR EXCEPTIONS HANDLING 
            character(len=*),intent(in) ::  Gnu_opt       ! STRING CONTAINING USER DEFINED GNU OPTIONS 
            logical,intent(in) :: Op_flag                 ! OPERATIVE MODE FLAG :
                                                          ! IF TRUE THE USER GIVE IN INPUT SOME GNU OPTIONS
                                                          ! IF FALSE DEFAULT MODE SELECTED 
            real,intent(in),optional :: Start_x, &        ! STARTING VALUE OF X (1 IF NOT PRESENT) 
                                        space_dist        ! SPACING BETWEEN X VALUES (1 IF NOT PRESENT)
                                        
            real(kind=real32),intent(in),dimension(:),optional :: x_val   ! VECTOR WITH X VALUES 
        end subroutine sGnu_plot



        subroutine dGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,space_dist,x_val)
            use, intrinsic :: iso_fortran_env, only : real64
            implicit none 
            real(kind=real64),intent(in),dimension(:) :: fx            
            logical,intent(out) :: Gnu_Flag               
            character(len=*),intent(in) ::  Gnu_opt       
            logical,intent(in) :: Op_flag                 
                                                          
                                                          
            real,intent(in),optional :: Start_x, &        
                                        space_dist        
                                        
            real(kind=real64),intent(in),dimension(:),optional :: x_val   
        end subroutine dGnu_plot




        subroutine qGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,space_dist,x_val)
            use, intrinsic :: iso_fortran_env, only : real128
            implicit none 
            real(kind=real128),intent(in),dimension(:) :: fx            
            logical,intent(out) :: Gnu_Flag               
            character(len=*),intent(in) ::  Gnu_opt       
            logical,intent(in) :: Op_flag                 
                                                          
                                                          
            real,intent(in),optional :: Start_x, &        
                                        space_dist        
                                        
            real(kind=real128),intent(in),dimension(:),optional :: x_val   
        end subroutine qGnu_plot


        
    end interface Gnu_plot

end module Etc_utilities_interface
