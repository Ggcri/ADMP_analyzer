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

        subroutine Gnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,space_dist,x_val)
            implicit none 
            real,intent(in),dimension(:) :: fx            ! VECTOR WITH F(X) VALUES
            logical,intent(out) :: Gnu_Flag               ! SIGNALING FLAG FOR EXCEPTIONS HANDLING 
            character(len=*),intent(in) ::  Gnu_opt       ! STRING CONTAINING USER DEFINED GNU OPTIONS 
            logical,intent(in) :: Op_flag                 ! OPERATIVE MODE FLAG :
                                                          ! IF TRUE THE USER GIVE IN INPUT SOME GNU OPTIONS
                                                          ! IF FALSE DEFAULT MODE SELECTED 
            real,intent(in),optional :: Start_x, &        ! STARTING VALUE OF X (1 IF NOT PRESENT) 
                                        space_dist        ! SPACING BETWEEN X VALUES (1 IF NOT PRESENT)
                                        
            real,intent(in),dimension(:),optional :: x_val   ! VECTOR WITH X VALUES 
        end subroutine Gnu_plot
    end interface

end module Etc_utilities_interface
