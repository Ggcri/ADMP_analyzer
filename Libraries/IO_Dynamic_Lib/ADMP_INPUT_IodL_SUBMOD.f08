submodule (ADMP_INPUT_MOD) ADMP_INPUT_LOG_BIN
contains
module procedure G_Log_Check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! THE SCOPE OF G_LOG_
! CHECK IS TO PERFORM    
! A CHECK OF .LOG FILES AND IF REQUIRED     !
! READ THE UNITS USED FOR ANGLES AND        ! 
! DISTANCES IN Z-MATRIX                     !
!  HALTING PROBLEMS HANDLING FOR I/O        !
! OPERATION                                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INPUT:    FILENAME.LOG                    !
! OUTPUT :  FATAL ERROR FLAG IF SOMETHING   !
!           WENT WRONG                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! subroutine G_Log_check_sub BODY
!LOCAL SECTION 
    implicit none
        logical ::  Exist_b         ! FILE EXISTANCE BOOLEAN
        integer     ::  k, &        ! COUNTER
                        ios,&       ! Input output status index
                        F_Unit,&    ! FILE UNIT INDENTIFIER
                        Char_id     ! CHARACTER INDEX FOR SUBSTRING SEARCH IN STRING
        character(len=2)    ::      User_opt_CHECK    ! USER OPTION FOR FILE PROCESSING IF NO NORMAL TERMINATION IS CHECKED
        character(len=256)  ::      Err_msg, &  ! ERROR MESSAGE FOR IO OP
                                    Line        ! LINE FOR IO OPERATION
        character(len=17)   ::      Units_char  ! UNITS SUBSTRING 'UNITS=UNITSOPT'
        character(len=20)   ::      G_version='ES64L-GDVRevJ.02'
    IO_CHECK_ERROR=.FALSE.
    Normal_term=.TRUE. 
!START
    ! CONTROL SECTION
    ! EXISTANCE CHECK S
        inquire(file=filename,exist=Exist_b)
        if (.not. Exist_b) then 
            print '(2a,1x,a)', 'ERROR: ',trim(filename),'does not exist'
            IO_CHECK_ERROR=.TRUE.
            return 
        end if
    ! EXISTANCE CHECK E 
        open(newunit=F_Unit,file=filename,status='old',action='read',position='append',iostat=ios)
    ! OPEN ERROR AND EXTENTION CHECK S
        if (ios /= 0) then
            inquire(unit=F_unit,iomsg=Err_msg)
            print '(4a)', 'ERROR: Open of  ', trim(filename),' failed', trim(Err_msg) 
            IO_CHECK_ERROR=.TRUE.
            return
        else if ( verify(filename,'.log') /= 0) then
            print '(2a,1x,a)', 'ERROR in ',trim(filename),'extention: .log file is required' 
            IO_CHECK_ERROR=.TRUE.
            return
        endif
    ! OPEN ERROR AND EXTENTION CHECK E
    
    ! NORMAL TERMINATION CHECK S
        backspace(F_unit)
        read(unit=F_unit,fmt=*,iostat=ios,iomsg=Err_msg) Line 
        if (ios /= 0) then
            print '(4(a,1x))', 'ERROR: Read of', trim(filename),'failed', trim(Err_msg ) 
            IO_CHECK_ERROR=.TRUE.
            return
        else if (index(Line,'Normal') .eq. 0) then 
            print '(a)', 'WARNING: no Normal Terminaton detected, do you want to proceed? (Y/N)' 
            read(*,*) User_opt_CHECK
            1001    continue  
                select case (Up_to_low(User_opt_CHECK))
                    case  ('n','no')
                        IO_CHECK_ERROR=.TRUE. 
                        return 
                    case  ('y','yes')
                        Normal_term=.FALSE. 
                    case default 
                        print *, 'No valid option inserted, insert valid option'
                        goto 1001    
                end select
        end if
    ! NORMAL TERMINATION CHECK E 

    ! VERSION CHECK S
        rewind(F_unit)
        do 
            read(F_unit,'(a)') line 
            if (index(line,'Gaussian DV') .ne. 0 ) then 
                if (index(line,G_version) .eq. 0 ) then             
                    print *, 'WARNING: THE GAUSSIAN VERSION IS NOT COMPATIBLE WITH ',G_version
                end if
                exit 
            end if
        end do 
    
    ! VERSION CHECK E

if (present(Units)) then
    rewind F_unit
    ! CHECKING FOR UNITS OPTION IN ROUTE SECTION FOR DISTANCE AND ANGLES USED 
    search1 : do k=1,200
        read(unit=F_unit,fmt='(a)') Line 
        if (index(Line,'units=') .ne. 0) then
            ! INSERIRE FUNZIONE LOWER CASE QUI
             Char_id=index(Line,'units=')
                ! CHECK FOR PARENTHESIS FOR MULTPLE OPTION IN INPUT
                    if ( Line(Char_id+6:Char_id+6) == '('  ) then
                    ! IN THIS CASE THER WILL BE A COMMA SEPARATOR 
                        Units_char(1:)=Line(Char_id+6:index(Line,')')-1)
                    ! UNITS CHAR WILL BE NOW SOMETHING LIKE 'au,deg' or 'au,rad' and so on
                    else
                    ! IF NO PARENTHESES IS DETECTET A LD READ LED TO UNITS_CHAR : units=opt
                        read(Line(Char_id:),*) Units_char
                    ! EXTRACT THE CURRENT OPTION 
                        Units_char(1:)=trim(Units_char(6:))
                    end if
            exit search1
        end if
    end do search1 
    Units=Up_to_low(Units_char(1:6))
end if

end procedure G_Log_Check

module procedure dpG_Log_Input
    implicit none
    !LOCAL DECLARATION SECTION
    integer ::  F_unit,     &   ! FILE UNIT
                All_stat,   &   ! ALLOCATE STATUS
                ios,        &   ! IOSTATUS READ OPERATION 
                i,k=1,J=1       ! COUNTERS
    character(len=256) ::   Err_msg, &  ! ERROR MESSAGE 
                            Line        ! LINE FOR IO OPERATION
    character(len=22) ::    trig1,& ! TRIGGER LINE FOR READING  
                            trig2,& ! TRIGGER LINE FOR READING
                            trig3,& ! TRIGGER LINE FOR READING
                            trig4   ! TRIGGER LINE FOR READING
    logical,dimension(:),allocatable :: mask 
    real(kind=real64),allocatable,dimension(:) :: temp_vel,temp_coord


    ! DUMMY INIZIALIZATION
    At_Num=0_int8 ; Step_Num=0_int16
    IO_INPUT_ERROR=.FALSE. 
    ! Changed data since obsolescent
    ! data trig1,trig2,trig3,trig4/'Charge','Maximum','Cartesian coordinates:','NAtoms='/ 
    trig1 = 'Charge' ; trig2 = 'Maximum' ; trig3 = 'Cartesian coordinates:' ; trig4 = 'NAtoms='
    !START 
    inquire(file=filename,number=F_unit)
    ! INPUT OF STEP NUMBER AND ATOM NUMBER S
    do  
        read(F_unit,*) line
        if (index(line,trig2) /= 0) then
            read( line( index(line,'=')+1: ),* ) Step_Num
        else if (index(line,trig4) /= 0) then 
            read(line(len(trig4)+1:),*) At_Num
            exit
        end if
    end do 
    ! INPUT OF STEP NUMBER AND ATOM NUMBER E


    ! INPUT OF COORDINATES & VELOCITY S
    if (Normal_flag .eqv. .true.) then 
        allocate(real(kind=real64):: coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real64):: vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        do
        read(F_unit,'(a)',iostat=ios) line 
            if (index(line,'MW Cartesian velocity:') /= 0) then
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) vel(k:)
                    k=k+3
                end do
            else if (index(line,'Input orientation:') /= 0  ) then
                read(F_unit,'(3/)')
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(35:),*,iostat=ios) coord(J:)
                    J=J+3
                end do
            end if
        end do

    else
        allocate(real(kind=real64):: temp_coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real64):: temp_vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(logical:: mask(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of mask array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        else 
            mask=.false. 
        endif
        do
            read(F_unit,'(a)',iostat=ios) line 
                if (index(line,'MW Cartesian velocity:') /= 0) then
                    Step_Num=Step_Num+1_int16
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) temp_vel(k:)
                        k=k+3
                    end do
                else if (index(line,'Input orientation:') /= 0  ) then
                    read(F_unit,'(3/)')
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(35:),*,iostat=ios) temp_coord(J:)
                        J=J+3
                    end do
                end if
            end do
            mask(:Step_Num*3*At_Num)=.true. 
            coord=pack(temp_coord,mask)
            vel=pack(temp_vel,mask)
    end if
    ! INPUT OF COORDINATES & VELOCITY E
end procedure dpG_Log_Input

module procedure spG_Log_Input
    implicit none
    !LOCAL DECLARATION SECTION
    integer ::  F_unit,     &   ! FILE UNIT
                All_stat,   &   ! ALLOCATE STATUS
                ios,        &   ! IOSTATUS READ OPERATION 
                i,k=1,J=1       ! COUNTERS
    character(len=256) ::   Err_msg, &  ! ERROR MESSAGE 
                            Line        ! LINE FOR IO OPERATION
    character(len=22) ::    trig1,& ! TRIGGER LINE FOR READING  
                            trig2,& ! TRIGGER LINE FOR READING
                            trig3,& ! TRIGGER LINE FOR READING
                            trig4   ! TRIGGER LINE FOR READING
    logical,dimension(:),allocatable :: mask 
    real(kind=real32),allocatable,dimension(:) :: temp_vel,temp_coord


    ! DUMMY INIZIALIZATION
    At_Num=0_int8 ; Step_Num=0_int16
    IO_INPUT_ERROR=.FALSE. 
    ! data trig1,trig2,trig3,trig4/'Charge','Maximum','Cartesian coordinates:','NAtoms='/ 
    trig1 = 'Charge' ; trig2 = 'Maximum' ; trig3 = 'Cartesian coordinates:' ; trig4 = 'NAtoms='
    !START 
    inquire(file=filename,number=F_unit)
    do  
        read(F_unit,*) line
        if (index(line,trig2) /= 0) then
            read( line( index(line,'=')+1: ),* ) Step_Num
        else if (index(line,trig4) /= 0) then 
            read(line(len(trig4)+1:),*) At_Num
            exit
        end if
    end do 
    ! INPUT OF STEP NUMBER AND ATOM NUMBER E


    ! INPUT OF COORDINATES & VELOCITY S
    if (Normal_flag .eqv. .true.) then 
        allocate(real(kind=real32):: coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real32):: vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        do
        read(F_unit,'(a)',iostat=ios) line 
            if (index(line,'MW Cartesian velocity:') /= 0) then
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) vel(k:)
                    k=k+3
                end do
            else if (index(line,'Input orientation:') /= 0  ) then
                read(F_unit,'(3/)')
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(35:),*,iostat=ios) coord(J:)
                    J=J+3
                end do
            end if
        end do

    else
        allocate(real(kind=real32):: temp_coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real32):: temp_vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(logical:: mask(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of mask array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        else 
            mask=.false. 
        endif
        do
            read(F_unit,'(a)',iostat=ios) line 
                if (index(line,'MW Cartesian velocity:') /= 0) then
                    Step_Num=Step_Num+1_int16
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) temp_vel(k:)
                        k=k+3
                    end do
                else if (index(line,'Input orientation:') /= 0  ) then
                    read(F_unit,'(3/)')
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(35:),*,iostat=ios) temp_coord(J:)
                        J=J+3
                    end do
                end if
            end do
            mask(:Step_Num*3*At_Num)=.true. 
            coord=pack(temp_coord,mask)
            vel=pack(temp_vel,mask)
    end if
    ! INPUT OF COORDINATES & VELOCITY E
end procedure spG_Log_Input

module procedure qpG_Log_Input
    implicit none
    !LOCAL DECLARATION SECTION
    integer ::  F_unit,     &   ! FILE UNIT
                All_stat,   &   ! ALLOCATE STATUS
                ios,        &   ! IOSTATUS READ OPERATION 
                i,k=1,J=1       ! COUNTERS
    character(len=256) ::   Err_msg, &  ! ERROR MESSAGE 
                            Line        ! LINE FOR IO OPERATION
    character(len=22) ::    trig1,& ! TRIGGER LINE FOR READING  
                            trig2,& ! TRIGGER LINE FOR READING
                            trig3,& ! TRIGGER LINE FOR READING
                            trig4   ! TRIGGER LINE FOR READING
    logical,dimension(:),allocatable :: mask 
    real(kind=real128),allocatable,dimension(:) :: temp_vel,temp_coord


    ! DUMMY INIZIALIZATION
    At_Num=0_int8 ; Step_Num=0_int16
    IO_INPUT_ERROR=.FALSE. 
    ! data trig1,trig2,trig3,trig4/'Charge','Maximum','Cartesian coordinates:','NAtoms='/ 
    trig1 = 'Charge' ; trig2 = 'Maximum' ; trig3 = 'Cartesian coordinates:' ; trig4 = 'NAtoms='
    !START 
    inquire(file=filename,number=F_unit)
    do  
        read(F_unit,*) line
        if (index(line,trig2) /= 0) then
            read( line( index(line,'=')+1: ),* ) Step_Num
        else if (index(line,trig4) /= 0) then 
            read(line(len(trig4)+1:),*) At_Num
            exit
        end if
    end do 
    ! INPUT OF STEP NUMBER AND ATOM NUMBER E


    ! INPUT OF COORDINATES & VELOCITY S
    if (Normal_flag .eqv. .true.) then 
        allocate(real(kind=real128):: coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real128):: vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        do
        read(F_unit,'(a)',iostat=ios) line 
            if (index(line,'MW Cartesian velocity:') /= 0) then
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) vel(k:)
                    k=k+3
                end do
            else if (index(line,'Input orientation:') /= 0  ) then
                read(F_unit,'(3/)')
                do i=1,At_Num
                    read(F_unit,'(a)') line
                    read(line(35:),*,iostat=ios) coord(J:)
                    J=J+3
                end do
            end if
        end do

    else
        allocate(real(kind=real128):: temp_coord(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of coord array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(real(kind=real128):: temp_vel(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of velocity array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        endif
        allocate(logical:: mask(At_Num*Step_Num*3), errmsg=Err_msg,stat=All_stat)
        if (All_stat /= 0) then
            print ('(2a,/)'), 'ERROR: Allocation of mask array failed ', trim(Err_msg)
            IO_INPUT_ERROR=.TRUE.
            return
        else 
            mask=.false. 
        endif
        do
            read(F_unit,'(a)',iostat=ios) line 
                if (index(line,'MW Cartesian velocity:') /= 0) then
                    Step_Num=Step_Num+1_int16
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) temp_vel(k:)
                        k=k+3
                    end do
                else if (index(line,'Input orientation:') /= 0  ) then
                    read(F_unit,'(3/)')
                    do i=1,At_Num
                        read(F_unit,'(a)') line
                        read(line(35:),*,iostat=ios) temp_coord(J:)
                        J=J+3
                    end do
                end if
            end do
            mask(:Step_Num*3*At_Num)=.true. 
            coord=pack(temp_coord,mask)
            vel=pack(temp_vel,mask)
    end if
    ! INPUT OF COORDINATES & VELOCITY E
end procedure qpG_Log_Input


module procedure spG_bin_Input
    implicit none 
    integer :: unit_id  ,&
               iostatus ,&
               reclen
    
    ! INIZIALIZATION
    Bin_in_flag=.false.

    ! START 
    inquire(iolength=reclen) Natoms
    open(newunit=unit_id,file='atom_num_step.bin',status='old',action='read',access='direct',form='unformatted',recl=reclen)
    read(unit_id,rec=1) Natoms
    read(unit_id,rec=2) Nsteps
    close(unit_id,status='delete',iostat=iostatus) ! NO PERMISSION STATUS 
    if (iostatus .ne. 0) close(unit_id,iostat=iostatus)
    if (iostatus .ne. 0) then 
        bin_in_flag=.true. 
        print *, 'ERROR: error status in atom_num_step.bin file close process'
        return
    end if
    allocate(coord_vec(3*Natoms*Nsteps),vel_vec(3*Natoms*Nsteps))
    inquire(iolength=reclen) coord_vec
    open(newunit=unit_id,file=Bin_filename,status='old',action='read',form='unformatted',access='direct',&
         iostat=iostatus,recl=reclen)
         !$ omp parallel 
         !$ omp sections 
         !$ omp section 
         read(unit_id,rec=1) coord_vec
         !$ omp section
         read(unit_id,rec=2) vel_vec
         !$ omp end sections 

        !$ omp end parallel 
    close(unit_id)    
end procedure spG_Bin_Input

    
    
    
    
    
module procedure dpG_bin_Input
    implicit none 
    integer :: unit_id  ,&
               iostatus ,&
               reclen
    
    ! INIZIALIZATION
    Bin_in_flag=.false.

    ! START 
    inquire(iolength=reclen) Natoms
    open(newunit=unit_id,file='atom_num_step.bin',status='old',action='read',access='direct',form='unformatted',recl=reclen)
    read(unit_id,rec=1) Natoms
    read(unit_id,rec=2) Nsteps
    close(unit_id,status='delete',iostat=iostatus) ! NO PERMISSION STATUS 
    if (iostatus .ne. 0) close(unit_id,iostat=iostatus)
    if (iostatus .ne. 0) then 
        bin_in_flag=.true. 
        print *, 'ERROR: error status in atom_num_step.bin file close process'
        return
    end if
    allocate(coord_vec(3*Natoms*Nsteps),vel_vec(3*Natoms*Nsteps))
    inquire(iolength=reclen) coord_vec
    open(newunit=unit_id,file=Bin_filename,status='old',action='read',form='unformatted',access='direct',&
         iostat=iostatus,recl=reclen)
         !$ omp parallel 
         !$ omp sections 
         !$ omp section 
         read(unit_id,rec=1) coord_vec
         !$ omp section
         read(unit_id,rec=2) vel_vec
         !$ omp end sections 

        !$ omp end parallel 
    close(unit_id)    


end procedure dpG_Bin_Input

module procedure qpG_bin_Input
    implicit none 
    integer :: unit_id  ,&
               iostatus ,&
               reclen
    
    ! INIZIALIZATION
    Bin_in_flag=.false.

    ! START 
    inquire(iolength=reclen) Natoms
    open(newunit=unit_id,file='atom_num_step.bin',status='old',action='read',access='direct',form='unformatted',recl=reclen)
    read(unit_id,rec=1) Natoms
    read(unit_id,rec=2) Nsteps
    close(unit_id,status='delete',iostat=iostatus) ! NO PERMISSION STATUS 
    if (iostatus .ne. 0) close(unit_id,iostat=iostatus)
    if (iostatus .ne. 0) then 
        bin_in_flag=.true. 
        print *, 'ERROR: error status in atom_num_step.bin file close process'
        return
    end if
    allocate(coord_vec(3*Natoms*Nsteps),vel_vec(3*Natoms*Nsteps))
    inquire(iolength=reclen) coord_vec
    open(newunit=unit_id,file=Bin_filename,status='old',action='read',form='unformatted',access='direct',&
         iostat=iostatus,recl=reclen)
         !$ omp parallel 
         !$ omp sections 
         !$ omp section 
         read(unit_id,rec=1) coord_vec
         !$ omp section
         read(unit_id,rec=2) vel_vec
         !$ omp end sections 

        !$ omp end parallel 
    close(unit_id)    

    end procedure qpG_Bin_Input

end submodule ADMP_INPUT_LOG_BIN
