function Pointing(coord,Nstep,Natoms,At_ids,Ref_T)
    use iso_fortran_env
    implicit none 
    real(kind=real64),dimension(:),pointer :: pointing 
    real(kind=real64),dimension(:),intent(in),target :: coord
    integer(kind=int8),dimension(:),intent(in),optional :: At_ids
    integer(kind=int8),intent(in),optional :: Ref_T
    integer,intent(in) :: Nstep,Natoms

    ! LOCAL SECTION
    integer,dimension(:),allocatable :: spread_index 
    integer :: Spread_id_size

        nullify(pointing)

        ! DEFAULT CASE 
        if ( (.not. present(At_ids) ) .and. (.not. present(REF_t) ) ) then 
            pointing => coord(:)
            
        else if ( (present(At_ids)) .and. (.not. present(REF_t) ) ) then 
            spread_id_size=(size(coord)/ (Natoms))*size(At_ids) ! THIS IS THE SIZE OF THE ATOMS OF INTERES
            
            spread_index=(At_ids*3)-2 
            spread_index = reshape ( spread(spread_index ,2, Nstep ) , (/size(coord)/) )
            

            pointing => coord(spread_index)
        else if ( (present(At_ids)) .and. (present(REF_t)) ) then 

            
        end if 
    


        

        

        


end function