subroutine sGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,x_spacing,x_val)
    use, intrinsic :: iso_fortran_env, only : real32
    implicit none 

    real(kind=real32),intent(in),dimension(:) :: fx      ! VECTOR WITH F(X) VALUES
    logical,intent(out) :: Gnu_Flag         ! SIGNALING FLAG FOR SUBROUTINE EXCEPTIONS HANDLING 
    character(len=*),intent(in) ::  Gnu_opt ! STRING CONTAINING USER DEFINED GNU OPTIONS 
    logical,intent(in) :: Op_flag           ! OPERATIVE MODE FLAG :
                                            ! IF TRUE THE USER GIVE IN INPUT SOME GNU OPTIONS
                                            ! IF FALSE DEFAULT MODE SELECTED 
    real,intent(in),optional :: Start_x, &  ! STARTING VALUE OF X (1 IF NOT PRESENT) 
                                x_spacing     ! SPACING BETWEEN X VALUES (1 IF NOT PRESENT)
                                
    real(kind=real32),intent(in),dimension(:),optional :: x_val   ! VECTOR WITH X VALUES 
    
    ! LOCAL
    
    character(len=256) :: Err_msg='' ! ERROR MESSAGE FOR FILE CONNECTION EXCEPTION HANDLING  
    
    character(len=28) :: G_script_file=' '  ! .GP SCRIPT FILE NAME
    
    integer ::  gnu_stat,   &   ! CONTROL VARIABLE FOR EXECUTE COMMAND LINE CONNECTED TO GNUPLOT EXECUTION
                unit_id,    &   ! UNIT IDENTIFIER OF SCRIPT.gp FILE
                ios,        &   ! CONTROL VARIABLE FOR OPEN STATEMENT
                i               ! COUNTER 
                
    character(len=9) :: date
    character(len=5) :: time 
    real :: l_Start_x=1,l_spacing=1 
    
    real(kind=kind(x_val)),dimension(size(fx)) :: l_x_val

    character(len=20),dimension(6) :: label_vec=['x_un   ','y_un   ','x_lab  ','y_lab  ','title  ','datfile']
    ! PLOT SETTINGS  
    character(len=:),allocatable :: datfile,    &
                                    title,      &
                                    x_lab,      &
                                    y_lab,      &               
                                    x_un,       &
                                    y_un        
    integer :: lt=1 ! LINE COLOR TYPE       
    real    :: lw=2.5 ! LINE WIDTH 
    logical :: ExistStat=.false. 

    


    ! START
    Gnu_Flag=.FALSE. 
    ! COMMENTED SECTION
    ! call get_environment_variable('HOME',status=gnu_stat)
    ! if (gnu_stat .eq. 1) then
        ! print *, 'ERROR : gnuplot environment variable does not exist ' 
        ! Gnu_Flag=.TRUE.
        ! return
    ! end if
    call date_and_time(date,time)
    date=(date(1:4)//'_'//date(5:6)//'_'//date(7:8))
    ! print *, date//'iii'
    time=(time(1:2)//'_'//time(3:4))
    ! print *,time//'iii'
    G_script_file='Gpscript_'//date//'_'//time//'.plt' 
    open(newunit=unit_id,file=G_script_file,status='unknown',action='write',iostat=ios)
    if (ios .ne. 0) then 
        inquire(unit_id,exist=ExistStat)
        if (.not. ExistStat) then 
          write(Err_msg,'(a,1x,a,1x,a)') 'File', trim(G_script_file), 'does not exist' 
          write(*,'(a,/,a)') 'ERROR: ',Err_msg 
          Gnu_Flag=.TRUE. 
          return 
        end if 
        inquire(unit_id,iomsg=Err_msg)
        write(*,'(a,/,a)') 'ERROR: ',Err_msg 
        Gnu_Flag=.TRUE. 
        return 
    end if
    if (Op_flag .eqv. .true. ) then 
        ! USER OPTIONS 
        ! READING OF USER OPTIONS 
        do i=1,size(label_vec)
        ! OVERRIDING OF USER OPTIONS IN LABEL VECTOR 
                          ! INTERNAL FILE SPECIFICATION                                 LIST DIRECTED I/O
            read(Gnu_opt (  index(Gnu_opt,trim(label_vec(i)))  +len_trim(label_vec(i))+1:),    * ) label_vec(i)
        end do 
    else 
        ! DEFAULT MODE
        label_vec(1)='au'
        label_vec(2)='au'
        label_vec(3)='x'
        label_vec(4)='f(x)'
        label_vec(5)='Default_title'
        label_vec(6)='Sample.dat'
    end if 
    allocate(character(len=len_trim(  label_vec(1)  )) :: x_un    )
    allocate(character(len=len_trim(  label_vec(2)  )) :: y_un    )
    allocate(character(len=len_trim(  label_vec(3)  )) :: x_lab   )
    allocate(character(len=len_trim(  label_vec(4)  )) :: y_lab   )
    allocate(character(len=len_trim(  label_vec(6)  )) :: datfile )
    allocate(character(len=len_trim(  label_vec(5)  )) :: title   )
    x_un=trim(label_vec(1))
    y_un=trim(label_vec(2))
    x_lab=trim(label_vec(3))
    y_lab=trim(label_vec(4))
    title=trim(label_vec(5))
    datfile = trim(label_vec(6))

    ! OPEN .DAT FILE FOR DATA WRITING 
    open(unit=15,file=label_vec(6),action='write',status='unknown') 
    
    ! CHECK FOR PRESENCE OF OPTIONAL ARGUMENT 
     if (present(Start_x) .eqv. .true.)       l_Start_x=Start_x   ! SET LOCAL STARTING X VALUE
     if (present(x_spacing) .eqv. .true.)     l_spacing=x_spacing   ! SET LOCAL SPACING OF X VALUES

     ! IF PRESENT THE X VALUES VECTOR 
     if (present(x_val)   .eqv. .true.) then
        ! SET X INITIAL VALUE AND LOCAL ARRAY l_x_val 
        l_x_val=x_val
     else 
        l_x_val(1)= l_Start_x
        do i=2,size(fx)
            l_x_val(i)=l_x_val(i-1)+l_spacing
        end do
     end if 
     
     do i = 1, size(fx)
        write(15,'(f0.10,3x,f0.10)') l_x_val(i),fx(i)
     end do 
     close(15)
     
    ! SET FILENAME
    write(unit_id,'(a)') 'reset'
    write(unit_id,999) 'fname = ',datfile 

    ! SET OPTIONS FOR GNUPLOT
    write(unit_id,*) 'set term wxt persist font "Helvetica-Bold,20" size 1200,800 lw ',lw 
    write(unit_id,1001) 'output',title 
    write(unit_id,1001) 'title',title 
    write(unit_id,1000) 'ylabel '//'"'//y_lab//'('//y_un//')'//'"'
    write(unit_id,1000) 'xlabel '//'"'//x_lab//'('//x_un//')'//'"'
    write(unit_id,1000) 'key on right'
    write(unit_id,*) 'set border lw', lw
    write(unit_id,*) 'set ytics nomirror'
    write(unit_id,*) 'set xtics nomirror'

    ! PLOT 
    write(unit_id,998) trim(label_vec(6)),lt,lw,datfile
    close(unit_id)
    call execute_command_line('gnuplot '//trim(G_script_file),cmdstat=gnu_stat)

    if (gnu_stat==-1) then 
        print *, 'ERROR : COMMAND LINE EXECUTION NON SUPPPORTED'
        Gnu_Flag=.TRUE. 
        return 
    end if 
    close(15) 
    return
! SET OPTION FORMAT
1000 format('set',1x,a,1x)
1001 format('set',1x,a,1x,'"',a,'"')
 999 format(a,1x,'"',a,'"')
 998 format('plot',1x,'"',a,'"',1x,'with lines lt',1x,i0,1x,'lw',1x,f0.1,1x,'title',1x,'"',a,'"') ! plot format 
end subroutine sGnu_plot


subroutine dGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,x_spacing,x_val)
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none 
    real(kind=real64),intent(in),dimension(:) :: fx      ! VECTOR WITH F(X) VALUES
    logical,intent(out) :: Gnu_Flag         ! SIGNALING FLAG FOR SUBROUTINE EXCEPTIONS HANDLING 
    character(len=*),intent(in) ::  Gnu_opt ! STRING CONTAINING USER DEFINED GNU OPTIONS 
    logical,intent(in) :: Op_flag           ! OPERATIVE MODE FLAG :
                                            ! IF TRUE THE USER GIVE IN INPUT SOME GNU OPTIONS
                                            ! IF FALSE DEFAULT MODE SELECTED 
    real,intent(in),optional :: Start_x, &  ! STARTING VALUE OF X (1 IF NOT PRESENT) 
                                x_spacing     ! SPACING BETWEEN X VALUES (1 IF NOT PRESENT)
                                
    real(kind=real64),intent(in),dimension(:),optional :: x_val   ! VECTOR WITH X VALUES 
    
    ! LOCAL
    
    character(len=256) :: Err_msg='' ! ERROR MESSAGE FOR FILE CONNECTION EXCEPTION HANDLING  
    
    character(len=28) :: G_script_file=' '  ! .GP SCRIPT FILE NAME
    
    integer ::  gnu_stat,   &   ! CONTROL VARIABLE FOR EXECUTE COMMAND LINE CONNECTED TO GNUPLOT EXECUTION
                unit_id,    &   ! UNIT IDENTIFIER OF SCRIPT.gp FILE
                ios,        &   ! CONTROL VARIABLE FOR OPEN STATEMENT
                i               ! COUNTER 
                
    character(len=9) :: date
    character(len=5) :: time 
    real :: l_Start_x=1,l_spacing=1 
    
    real(kind=kind(x_val)),dimension(size(fx)) :: l_x_val

    character(len=20),dimension(6) :: label_vec=['x_un   ','y_un   ','x_lab  ','y_lab  ','title  ','datfile']
    ! PLOT SETTINGS  
    character(len=:),allocatable :: datfile,    &
                                    title,      &
                                    x_lab,      &
                                    y_lab,      &               
                                    x_un,       &
                                    y_un        
    integer :: lt=1 ! LINE COLOR TYPE       
    real    :: lw=2.5 ! LINE WIDTH 
    logical :: ExistStat=.false. 

    


    ! START
    Gnu_Flag=.FALSE. 
    ! COMMENTED SECTION
    ! call get_environment_variable('HOME',status=gnu_stat)
    ! if (gnu_stat .eq. 1) then
        ! print *, 'ERROR : gnuplot environment variable does not exist ' 
        ! Gnu_Flag=.TRUE.
        ! return
    ! end if
    call date_and_time(date,time)
    date=(date(1:4)//'_'//date(5:6)//'_'//date(7:8))
    ! print *, date//'iii'
    time=(time(1:2)//'_'//time(3:4))
    ! print *,time//'iii'
    G_script_file='Gpscript_'//date//'_'//time//'.plt' 
    open(newunit=unit_id,file=G_script_file,status='unknown',action='write',iostat=ios)
    if (ios .ne. 0) then 
        inquire(unit_id,exist=ExistStat)
        if (.not. ExistStat) then 
          write(Err_msg,'(a,1x,a,1x,a)') 'File', trim(G_script_file), 'does not exist' 
          write(*,'(a,/,a)') 'ERROR: ',Err_msg 
          Gnu_Flag=.TRUE. 
          return 
        end if 
        inquire(unit_id,iomsg=Err_msg)
        write(*,'(a,/,a)') 'ERROR: ',Err_msg 
        Gnu_Flag=.TRUE. 
        return 
    end if
    if (Op_flag .eqv. .true. ) then 
        ! USER OPTIONS 
        ! READING OF USER OPTIONS 
        do i=1,size(label_vec)
        ! OVERRIDING OF USER OPTIONS IN LABEL VECTOR 
                          ! INTERNAL FILE SPECIFICATION                                 LIST DIRECTED I/O
            read(Gnu_opt (  index(Gnu_opt,trim(label_vec(i)))  +len_trim(label_vec(i))+1:),    * ) label_vec(i)
        end do 
    else 
        ! DEFAULT MODE
        label_vec(1)='au'
        label_vec(2)='au'
        label_vec(3)='x'
        label_vec(4)='f(x)'
        label_vec(5)='Default_title'
        label_vec(6)='Sample.dat'
    end if 
    allocate(character(len=len_trim(  label_vec(1)  )) :: x_un    )
    allocate(character(len=len_trim(  label_vec(2)  )) :: y_un    )
    allocate(character(len=len_trim(  label_vec(3)  )) :: x_lab   )
    allocate(character(len=len_trim(  label_vec(4)  )) :: y_lab   )
    allocate(character(len=len_trim(  label_vec(6)  )) :: datfile )
    allocate(character(len=len_trim(  label_vec(5)  )) :: title   )
    x_un=trim(label_vec(1))
    y_un=trim(label_vec(2))
    x_lab=trim(label_vec(3))
    y_lab=trim(label_vec(4))
    title=trim(label_vec(5))
    datfile = trim(label_vec(6))

    ! OPEN .DAT FILE FOR DATA WRITING 
    open(unit=15,file=label_vec(6),action='write',status='unknown') 
    
    ! CHECK FOR PRESENCE OF OPTIONAL ARGUMENT 
     if (present(Start_x) .eqv. .true.)       l_Start_x=Start_x   ! SET LOCAL STARTING X VALUE
     if (present(x_spacing) .eqv. .true.)     l_spacing=x_spacing   ! SET LOCAL SPACING OF X VALUES

     ! IF PRESENT THE X VALUES VECTOR 
     if (present(x_val)   .eqv. .true.) then
        ! SET X INITIAL VALUE AND LOCAL ARRAY l_x_val 
        l_x_val=x_val
     else 
        l_x_val(1)= l_Start_x
        do i=2,size(fx)
            l_x_val(i)=l_x_val(i-1)+l_spacing
        end do
     end if 
     
     do i = 1, size(fx)
        write(15,'(f0.10,3x,f0.10)') l_x_val(i),fx(i)
     end do 
     close(15)
     
    ! SET FILENAME
    write(unit_id,'(a)') 'reset'
    write(unit_id,999) 'fname = ',datfile 

    ! SET OPTIONS FOR GNUPLOT
    write(unit_id,*) 'set term wxt persist font "Helvetica-Bold,20" size 1200,800 lw ',lw 
    write(unit_id,1001) 'output',title 
    write(unit_id,1001) 'title',title 
    write(unit_id,1000) 'ylabel '//'"'//y_lab//'('//y_un//')'//'"'
    write(unit_id,1000) 'xlabel '//'"'//x_lab//'('//x_un//')'//'"'
    write(unit_id,1000) 'key on right'
    write(unit_id,*) 'set border lw', lw
    write(unit_id,*) 'set ytics nomirror'
    write(unit_id,*) 'set xtics nomirror'

    ! PLOT 
    write(unit_id,998) trim(label_vec(6)),lt,lw,datfile
    close(unit_id)
    call execute_command_line('gnuplot '//trim(G_script_file),cmdstat=gnu_stat)

    if (gnu_stat==-1) then 
        print *, 'ERROR : COMMAND LINE EXECUTION NON SUPPPORTED'
        Gnu_Flag=.TRUE. 
        return 
    end if 
    close(15) 
    return
! SET OPTION FORMAT
1000 format('set',1x,a,1x)
1001 format('set',1x,a,1x,'"',a,'"')
 999 format(a,1x,'"',a,'"')
 998 format('plot',1x,'"',a,'"',1x,'with lines lt',1x,i0,1x,'lw',1x,f0.1,1x,'title',1x,'"',a,'"') ! plot format 
end subroutine dGnu_plot



subroutine qGnu_plot(fx,Gnu_Flag,Op_flag,Gnu_opt,Start_x,x_spacing,x_val)
    use, intrinsic :: iso_fortran_env, only : real128
    implicit none 
    real(kind=real128),intent(in),dimension(:) :: fx      ! VECTOR WITH F(X) VALUES
    logical,intent(out) :: Gnu_Flag         ! SIGNALING FLAG FOR SUBROUTINE EXCEPTIONS HANDLING 
    character(len=*),intent(in) ::  Gnu_opt ! STRING CONTAINING USER DEFINED GNU OPTIONS 
    logical,intent(in) :: Op_flag           ! OPERATIVE MODE FLAG :
                                            ! IF TRUE THE USER GIVE IN INPUT SOME GNU OPTIONS
                                            ! IF FALSE DEFAULT MODE SELECTED 
    real,intent(in),optional :: Start_x, &  ! STARTING VALUE OF X (1 IF NOT PRESENT) 
                                x_spacing     ! SPACING BETWEEN X VALUES (1 IF NOT PRESENT)
                                
    real(kind=real128),intent(in),dimension(:),optional :: x_val   ! VECTOR WITH X VALUES 
    
    ! LOCAL
    
    character(len=256) :: Err_msg='' ! ERROR MESSAGE FOR FILE CONNECTION EXCEPTION HANDLING  
    
    character(len=28) :: G_script_file=' '  ! .GP SCRIPT FILE NAME
    
    integer ::  gnu_stat,   &   ! CONTROL VARIABLE FOR EXECUTE COMMAND LINE CONNECTED TO GNUPLOT EXECUTION
                unit_id,    &   ! UNIT IDENTIFIER OF SCRIPT.gp FILE
                ios,        &   ! CONTROL VARIABLE FOR OPEN STATEMENT
                i               ! COUNTER 
                
    character(len=9) :: date
    character(len=5) :: time 
    real :: l_Start_x=1,l_spacing=1 
    
    real(kind=kind(x_val)),dimension(size(fx)) :: l_x_val

    character(len=20),dimension(6) :: label_vec=['x_un   ','y_un   ','x_lab  ','y_lab  ','title  ','datfile']
    ! PLOT SETTINGS  
    character(len=:),allocatable :: datfile,    &
                                    title,      &
                                    x_lab,      &
                                    y_lab,      &               
                                    x_un,       &
                                    y_un        
    integer :: lt=1 ! LINE COLOR TYPE       
    real    :: lw=2.5 ! LINE WIDTH 
    logical :: ExistStat=.false. 

    


    ! START
    Gnu_Flag=.FALSE. 
    call date_and_time(date,time)
    date=(date(1:4)//'_'//date(5:6)//'_'//date(7:8))
    ! print *, date//'iii'
    time=(time(1:2)//'_'//time(3:4))
    ! print *,time//'iii'
    G_script_file='Gpscript_'//date//'_'//time//'.plt' 
    open(newunit=unit_id,file=G_script_file,status='unknown',action='write',iostat=ios)
    if (ios .ne. 0) then 
        inquire(unit_id,exist=ExistStat)
        if (.not. ExistStat) then 
          write(Err_msg,'(a,1x,a,1x,a)') 'File', trim(G_script_file), 'does not exist' 
          write(*,'(a,/,a)') 'ERROR: ',Err_msg 
          Gnu_Flag=.TRUE. 
          return 
        end if 
        inquire(unit_id,iomsg=Err_msg)
        write(*,'(a,/,a)') 'ERROR: ',Err_msg 
        Gnu_Flag=.TRUE. 
        return 
    end if
    if (Op_flag .eqv. .true. ) then 
        ! USER OPTIONS 
        ! READING OF USER OPTIONS 
        do i=1,size(label_vec)
        ! OVERRIDING OF USER OPTIONS IN LABEL VECTOR 
                          ! INTERNAL FILE SPECIFICATION                                 LIST DIRECTED I/O
            read(Gnu_opt (  index(Gnu_opt,trim(label_vec(i)))  +len_trim(label_vec(i))+1:),    * ) label_vec(i)
        end do 
    else 
        ! DEFAULT MODE
        label_vec(1)='au'
        label_vec(2)='au'
        label_vec(3)='x'
        label_vec(4)='f(x)'
        label_vec(5)='Default_title'
        label_vec(6)='Sample.dat'
    end if 
    allocate(character(len=len_trim(  label_vec(1)  )) :: x_un    )
    allocate(character(len=len_trim(  label_vec(2)  )) :: y_un    )
    allocate(character(len=len_trim(  label_vec(3)  )) :: x_lab   )
    allocate(character(len=len_trim(  label_vec(4)  )) :: y_lab   )
    allocate(character(len=len_trim(  label_vec(6)  )) :: datfile )
    allocate(character(len=len_trim(  label_vec(5)  )) :: title   )
    x_un=trim(label_vec(1))
    y_un=trim(label_vec(2))
    x_lab=trim(label_vec(3))
    y_lab=trim(label_vec(4))
    title=trim(label_vec(5))
    datfile = trim(label_vec(6))

    ! OPEN .DAT FILE FOR DATA WRITING 
    open(unit=15,file=label_vec(6),action='write',status='unknown') 
    
    ! CHECK FOR PRESENCE OF OPTIONAL ARGUMENT 
     if (present(Start_x) .eqv. .true.)       l_Start_x=Start_x   ! SET LOCAL STARTING X VALUE
     if (present(x_spacing) .eqv. .true.)     l_spacing=x_spacing   ! SET LOCAL SPACING OF X VALUES

     ! IF PRESENT THE X VALUES VECTOR 
     if (present(x_val)   .eqv. .true.) then
        ! SET X INITIAL VALUE AND LOCAL ARRAY l_x_val 
        l_x_val=x_val
     else 
        l_x_val(1)= l_Start_x
        do i=2,size(fx)
            l_x_val(i)=l_x_val(i-1)+l_spacing
        end do
     end if 
     
     do i = 1, size(fx)
        write(15,'(f0.10,3x,f0.10)') l_x_val(i),fx(i)
     end do 
     close(15)
     
    ! SET FILENAME
    write(unit_id,'(a)') 'reset'
    write(unit_id,999) 'fname = ',datfile 

    ! SET OPTIONS FOR GNUPLOT
    write(unit_id,*) 'set term wxt persist font "Helvetica-Bold,20" size 1200,800 lw ',lw 
    write(unit_id,1001) 'output',title 
    write(unit_id,1001) 'title',title 
    write(unit_id,1000) 'ylabel '//'"'//y_lab//'('//y_un//')'//'"'
    write(unit_id,1000) 'xlabel '//'"'//x_lab//'('//x_un//')'//'"'
    write(unit_id,1000) 'key on right'
    write(unit_id,*) 'set border lw', lw
    write(unit_id,*) 'set ytics nomirror'
    write(unit_id,*) 'set xtics nomirror'

    ! PLOT 
    write(unit_id,998) trim(label_vec(6)),lt,lw,datfile
    close(unit_id)
    call execute_command_line('gnuplot '//trim(G_script_file),cmdstat=gnu_stat)

    if (gnu_stat==-1) then 
        print *, 'ERROR : COMMAND LINE EXECUTION NON SUPPPORTED'
        Gnu_Flag=.TRUE. 
        return 
    end if 
    close(15) 
    return
! SET OPTION FORMAT
1000 format('set',1x,a,1x)
1001 format('set',1x,a,1x,'"',a,'"')
 999 format(a,1x,'"',a,'"')
 998 format('plot',1x,'"',a,'"',1x,'with lines lt',1x,i0,1x,'lw',1x,f0.1,1x,'title',1x,'"',a,'"') ! plot format 
end subroutine qGnu_plot
