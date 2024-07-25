! ------------------------------------------------------------------
! GOAL :    THE SUBROUTINE WRITE A FILE OF COORDINATES IN INTERNAL
!           REPRESENTATION 
! INPUT :   DATA TO WRITE, SO, THE VECTOR OF COORDINATES 
!           ATOM NUMBER, STEP NUMBER
! OUTPUT :  FILE IN INTERNAL REPRESENTATION 
!           DIRECT_COORD_END WHICH INDICATES THE POSITION OF NATOMS
!           AND NSTEPS IN INTERNAL REPRESENTATION FILE 
! ------------------------------------------------------------------ 
subroutine spBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
    use, intrinsic :: iso_fortran_env, only : real32
    use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
    ! DECLARATIN SECTION

        ! DUMMY
            real(kind=real32),dimension(:),intent(in) :: vel
            real(kind=real32),dimension(:),intent(in) :: coord  ! COORDINATES VECTOR 
            character(len=*),intent(in) :: log_filename ! LOG_FILENAME 
            integer,intent(in) :: Natoms, Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag 
            
            
        ! LOCAL      
            character(len=8) :: stat 
            integer ::  unit_id     ! UNIT IDENTIFIE TO BIN_FILENAME 
            logical ::  exists
            character(len=3) :: User_opt  
            integer :: length ! RECORD LENGTH IN TERM OF DATA FOR DIRECT ACCESS BINARY FILE
    
    ! END DECLARATION SECTION 

    ! TO GENERATE BIN FILE THE PRESENCE OF A LOG FILE IN INPUT IS MANDATORY
            
     ! START
        
        Bin_filename=log_filename(:len(log_filename)-3)//'.bin'
        inquire(file=Bin_filename,exist=exists)
        if (exists .eqv. .true.) then 
        1001 continue 
            print '(a,1x,a)', 'WARNING: THE BIN FILE',Bin_filename,'ALREADY EXISTS, DO YOU WANT TO UPDATE? (y/n) or q to quit'
            read *, User_opt
            User_opt = Up_to_low(User_opt) 
            select case (User_opt(1:1))
            case ('y')
                stat= 'replace '
            case ('n')
                print *, 'INSERT NEW FILENAME OR PRESS Q TO QUIT' 
                read *, Bin_filename 
                if (Up_to_low(Bin_filename) .eq. 'q') return
            case ('q')
                print *, ' QUIT OPTION ' 
                bingen_flag=.TRUE.
                return 
            case default 
                print *, 'NO VALID OPTION' 
                go to 1001
            end select 
        else if (.false.) then
            stat='new' 
        end if   
        inquire(iolength=length) coord
        open(newunit=unit_id,file=Bin_filename,form='unformatted',status=stat,recl=length,access='direct') 
        write(unit=unit_id,rec=1) Natoms,Nsteps        
        write(unit=unit_id,rec=2) coord
        write(unit=unit_id,rec=3) vel
        close(unit_id)


    ! END
end subroutine spBin_gen


subroutine dpBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
    use, intrinsic :: iso_fortran_env, only : real64
    use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
    ! DECLARATIN SECTION

        ! DUMMY
            real(kind=real64),dimension(:),intent(in) :: vel
            real(kind=real64),dimension(:),intent(in) :: coord  ! COORDINATES VECTOR 
            character(len=*),intent(in) :: log_filename ! LOG_FILENAME 
            integer,intent(in) :: Natoms, Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag 
            
        ! LOCAL      
            character(len=8) :: stat 
            integer ::  unit_id     ! UNIT IDENTIFIE TO BIN_FILENAME 
            logical ::  exists
            character(len=3) :: User_opt  
            integer :: length ! RECORD LENGTH IN TERM OF DATA FOR DIRECT ACCESS BINARY FILE
    
    ! END DECLARATION SECTION 

    ! START
        
        Bin_filename=log_filename(:-3)//'bin'
        inquire(file=Bin_filename,exist=exists)
        if (exists .eqv. .true.) then 
        1001 continue 
            print '(a,1x,a)', 'WARNING: THE BIN FILE',Bin_filename,'ALREADY EXISTS, DO YOU WANT TO UPDATE? (y/n) or q to quit'
            read *, User_opt
            User_opt = Up_to_low(User_opt) 
            select case (User_opt(1:1))
            case ('y')
                stat= 'replace '
            case ('n')
                print *, 'INSERT NEW FILENAME OR PRESS Q TO QUIT' 
                read *, Bin_filename 
                if (Up_to_low(Bin_filename) .eq. 'q') return
            case ('q')
                print *, ' QUIT OPTION ' 
                bingen_flag=.TRUE. 
                return 
            case default 
                print *, 'NO VALID OPTION' 
                go to 1001
            end select 
        else if (.false.) then
            stat='new' 
        end if   
        inquire(iolength=length) coord
        open(newunit=unit_id,file=Bin_filename,form='unformatted',status=stat,recl=length,access='direct') 
        write(unit=unit_id,rec=1) Natoms,Nsteps
        write(unit=unit_id,rec=2) coord
        write(unit=unit_id,rec=3) vel
        close(unit_id)


    ! END
end subroutine dpBin_gen


subroutine qpBin_gen(log_filename,coord,vel,Bin_filename,Natoms,Nsteps,bingen_flag)
    use, intrinsic :: iso_fortran_env, only : real128
    use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low
    ! DECLARATIN SECTION

        ! DUMMY
            real(kind=real128),dimension(:),intent(in) :: vel
            real(kind=real128),dimension(:),intent(in) :: coord  ! COORDINATES VECTOR 
            character(len=*),intent(in) :: log_filename ! LOG_FILENAME 
            integer,intent(in) :: Natoms, Nsteps 
            character(len=*),intent(out) :: Bin_filename
            logical,intent(out) :: bingen_flag 
            
        ! LOCAL      
            character(len=8) :: stat 
            integer ::  unit_id     ! UNIT IDENTIFIE TO BIN_FILENAME 
            logical ::  exists
            character(len=3) :: User_opt  
            integer :: length ! RECORD LENGTH IN TERM OF DATA FOR DIRECT ACCESS BINARY FILE
    
    ! END DECLARATION SECTION 

    ! START
        
        Bin_filename=log_filename(:-3)//'bin'
        inquire(file=Bin_filename,exist=exists)
        if (exists .eqv. .true.) then 
        1001 continue 
            print '(a,1x,a)', 'WARNING: THE BIN FILE',Bin_filename,'ALREADY EXISTS, DO YOU WANT TO UPDATE? (y/n) or q to quit'
            read *, User_opt
            User_opt = Up_to_low(User_opt) 
            select case (User_opt(1:1))
            case ('y')
                stat= 'replace '
            case ('n')
                print *, 'INSERT NEW FILENAME OR PRESS Q TO QUIT' 
                read *, Bin_filename 
                if (Up_to_low(Bin_filename) .eq. 'q') return
            case ('q')
                print *, ' QUIT OPTION ' 
                bingen_flag=.TRUE. 
                return 
            case default 
                print *, 'NO VALID OPTION' 
                go to 1001
            end select 
        else if (.false.) then
            stat='new' 
        end if   
        inquire(iolength=length) coord
        write(unit=unit_id,rec=1) Natoms,Nsteps
        open(newunit=unit_id,file=Bin_filename,form='unformatted',status=stat,recl=length,access='direct') 
        write(unit=unit_id,rec=2) coord
        write(unit=unit_id,rec=3) vel 
        close(unit_id)              
        

    ! END
end subroutine qpBin_gen


