subroutine Parsing(Opt_vec,Gnu_opt, PARSING_FLAG,Atom_ids,Ref_f)
use Etc_utilities_interface, only : Up_to_low,opt_flags
use, intrinsic :: iso_fortran_env, only : compiler_version,int8
implicit none
integer,intent(out) :: Ref_f 
logical,intent(out) :: PARSING_FLAG
integer(kind=int8),intent(out),dimension(:),allocatable :: Atom_ids
character(len=*),intent(out) :: Gnu_opt
type(opt_flags),dimension(:),intent(out) :: Opt_vec 

! LOCAL SECTION 
integer ::  i=1,j=1,k,  &  ! COUNTER
            parstat,    &  ! STATUS VARIABLE FOR GET_COMMAND_ARGUMENT SUBROUTINE
            Arg_len,    &  ! ARGUMENT LENGHT
            ios,        &  ! IOSTATUS I/O OP
            N,          &  ! NUMBER OF SUBGROUP OF ATOMS IF ATOM IDS IS SPECIFIED
            vernum         ! COMPILER VERSION NUMBER 

character(len=132) :: paropt,line=' ',f_version
character(len=:),allocatable :: filename 
logical,dimension(5) :: Gnu_flag=.false.  
! START 
    ! INIZIALIZATION TO DEFAULT VALUES
    do k=3,size(Opt_vec)
        allocate(character( 8 ) ::  Opt_vec(k)%label )
        Opt_vec(k)=opt_flags(label='not_set',flag=.false.)
    end do 
    Ref_f=1
            !Opt_vec(3)   RMSD COMPUTATION FLAG      
            !Opt_vec(4)   ATOM IDS FLAG  
            !Opt_vec(5)   GNU PLOT FOR RMSD FLAG  
            !Opt_vec(6)   REFERENCE FRAME FLAG  
if (command_argument_count() == 0 ) then 
    print *, 'WARNING: NO OPTION GIVEN IN INPUT'
    write(*,'(a,1x)',advance='no') 'Insert filename to process or press q to quit:'
    read(*,'(a)') line 
    if (line=='q') then 
        PARSING_FLAG=.true.
        return 
    else 
        allocate(character(len_trim(line)) ::  Opt_vec(2)%label,Opt_vec(1)%label )
    end if
    ! DEFAULT MODE INIZIALIZATION
     Opt_vec(1)=opt_flags(label=line,flag=.true.)                                    ! FILENAME FLAG
     Opt_vec(2)=opt_flags(label=line(1:len_trim(line)-3)//'bin',flag=.true.)       ! GENERATION OF BIN FILE FLAG
    print *, 'DEFAULT OPERATING MODE '
    return 
end if
opt: do
    call get_command_argument(i,value=paropt,status=parstat)
    select case (Up_to_low(paropt))
        case ('-f','--file')
            call get_command_argument(i+1,length=Arg_len)
            allocate(character(len=Arg_len) :: filename)
            call get_command_argument(i+1,value=filename)
            if (filename(1:1) == '-' ) then 
                    print *, 'ERROR: invalid filename inserted '
                    PARSING_FLAG=.TRUE. 
                    return 
            end if
            Opt_vec(1)=opt_flags(label=filename,flag=.true.) 
            i=i+2    

        case ('-gb','--genbin')
            Opt_vec(2)=opt_flags(flag=.true.) 
            i=i+1             
            
        case ('-h','--help')
            i=i+1
            call documentation()
            stop 0 ! side-effect voluto
        case ('-rmsd','--rmsd')
            Opt_vec(3)=opt_flags(label=paropt,flag=.true.)            
            i=i+1
        case ('-at_ids','--atom_ids')
            Opt_vec(4)=opt_flags(label=paropt,flag=.true.)            
            call get_command_argument(i+1,value=paropt)
            read(paropt,*,iostat=ios,iomsg=line) N
                if (ios .gt. 0) then 
                    print *, 'ERROR: bad value inserted '
                    PARSING_FLAG=.TRUE. 
                    return 
                end if 
            allocate(Atom_ids(N))
            do i=i+2,i+1+N
                call get_command_argument(i,value=paropt)
                read(paropt,*,iostat=ios) Atom_ids(j)
                if (ios .gt. 0) then 
                    print *, 'ERROR: bad value inserted '
                    PARSING_FLAG=.TRUE. 
                    return
                ! GFORTRAN 9 NEEDED  
                    f_version= compiler_version()
                    if ( (f_version == 'GCC') )  then
                        read( f_version( (index(f_version,'version')+8): index(f_version,'version')+8),*) vernum  
                        if (vernum .ge. 9) then 
                            if ( findloc(Atom_ids,Atom_ids(j),1 ) .ne. 0   ) then
                                 print *, 'ERROR: redundancy in atom index inserted '
                                 PARSING_FLAG=.TRUE. 
                                 return 
                            end if
                        end if 
                    end if
                else if ( findloc(Atom_ids,Atom_ids(j),1 ) .ne. 0   ) then
                    print *, 'ERROR: redundancy in atom index inserted '
                    PARSING_FLAG=.TRUE. 
                    return 
                end if
                j=j+1
            end do
        !!!!!!!!!   GNUPLOT    !!!!!!!!! 

        ! READING OF GNU PLOT OPTIONS. 
        ! PLEASE NOTE THAT FOR THE GNU PLOT 
        ! SUBROUTINE SOME OPTION ARE MANDATORY SO THERE WILL BE A CONTROL
        ! SECTION ON THESE 
        case ('-gnu','--gnuplot')
            Opt_vec(5)=opt_flags(label=paropt,flag=.true.) 
            i=i+1
        gnu: do 
                call get_command_argument(i,value=paropt,status=parstat)
                select case (Up_to_low(paropt))
                    
                case('x_un')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(1)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case('y_un')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(2)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case('x_lab')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(3)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case('y_lab')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(4)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case('title')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(5)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case('datfile')
                    call get_command_argument(i+1,value=Gnu_opt)
                    Gnu_flag(5)=.TRUE.
                    line=trim(line)//' '//trim(Up_to_low(paropt))//' '//trim(Gnu_opt)
                    i=i+2
                case default 
                    if (parstat > 0) then
                        Gnu_opt=line
                        exit opt
                    else if (paropt(1:1) == '-' ) then
                        Gnu_opt=line 
                        exit  
                    else
                        print '(3a)', 'ERROR: OPTION (',trim(paropt), ') IS AN INVALID OPTION  '
                        PARSING_FLAG=.TRUE. 
                        return 
                    end if
                end select
            end do gnu
        ! READING OF GNU PLOT OPTIONS. E
            
        case ('-ref_f','--reference_frame')
            Opt_vec(6)=opt_flags(label=paropt,flag=.true.) 
            call get_command_argument(i+1,value=paropt)
            read(paropt,*,iostat=ios) Ref_f
            if (ios .gt. 0) then 
                print '(a,1x,i0,1x,":",1x,a)', 'ERROR: bad value inserted for reference frame in position',i+1,paropt
                PARSING_FLAG=.TRUE. 
                return 
            end if        
            i=i+2
            Opt_vec(5)=opt_flags(label=paropt,flag=.true.)            
        case default 
            if (parstat > 0) then
                exit 
            else
                print '(3a)', 'ERROR: OPTION (',trim(paropt), ') IS AN INVALID OPTION  '
                PARSING_FLAG=.TRUE. 
            end if 
        return
    end select 
end do opt

! PARSING CONTROL SECTION 
    if (Opt_vec(1)%flag .eqv. .false. ) then 
        write(*,'(a)') 'ERROR : NO FILE TO PROCESS GIVEN IN INPUT'
        PARSING_FLAG=.TRUE.
        return
    end if
return 

! END

! CONTAINS SUBROUTINE DOCUMENTATION
contains 
subroutine  documentation()
    implicit none 
    
    write(*,1001) 'COMPILER VERSION USED:',compiler_version()
    write(*,1000) 'INPUT SECTION:'
    write(*,1000) 'Type 1: DEFAULT MODE'
    
    write(*,'(*(a,/))') 'Dynamic_analyzer [-f,--filename STRING] ','ANALYZE A .LOG FILE &
    &AND GENERATE A .BIN FILE WITH', 'STEP NUMBER, ATOM NUMBER, COORDINATES AND MASS WEIGHTED VELOCITIES'

    write(*,1000) 'Type 2: NON-DEFAULT MODE' 
    write(*,'(a,/)') 'DYNAMIC_ANALYZER [-KEY/--KEY_EXTENDED_FORM KEYVALUE]'
    write(*,1002) '-h, --help', 'PRINT THE DOCUMENTATION'
    
    write(*,'(77("*"))')
    write(*,'(a)') 'Input/output control:'
    write(*,1002) '-f, --filename STRING', '.LOG OR .BIN FILENAME TO PROCESS'
    write(*,1002) '-bg, --bingen', 'GENERATES A .BIN FILE WITH STEP NUMBER,','ATOM NUMBER&
    &,COORDINATES (ANG) AND MASS WEIGHTED VELOCITIES'

    write(*,'(77("*"))')
    write(*,'(a)') 'Numerical control:' 
    write(*,1002) '-rmsd, --rmsd','SPECIFY TO COMPUTE THE RMSD TAKING THE FIRST FRAME', ' STRUCTURE AS THE REFEREMENT ONE'
    write(*,1002) '-at_ids, --atom_ids NUM NUM_LIST','SPECIFY THE ATOM IDENTIFIERS FOR',' THE REFERENCE STUCTURE IN RMSD&
    & COMPUTATION.', ' NUM SPECIFIES THE NUMBER OF ATOMS AND NUM_LIST THE ATOM IDS '
    write(*,1002) '-ref_f, --reference_frame NUM','','SPECIFY REFERENCE STRUCTURE FRAME IN RMSD COMPUTATION'
    
    write(*,'(77("*"))')
    write(*,'(a)') 'GNUPLOT control:'
    write(*,1002) '-gnu, --gnuplop OPTION_LIST','THE PROGRAM WILL PRODUCE RMSD PLOT IN OUTPUT'
    write(*,'(a,/)') 'Option list: '
    write(*,1002) 'x_un STRING','x axis unit'
    write(*,1002) 'y_un STRING','y axis unit'
    write(*,1002) 'x_lab STRING' ,'x axis label'
    write(*,1002) 'y_label STRING ','y axis label'
    write(*,1002) 'datfile STRING ','.dat file produced after the execution'
    write(*,1002) 'title STRING  ','plot title'
    
    
    1000 format ( /,77("-"),/,a,/,77("-") ) 
    1001 format ( /,77("-"),/,a,3x,a,/,77("-") ) 
    1002 format (("key: ",a,1x,'-->',1x,*(a,/,16x)  ) )

end subroutine documentation 




end subroutine Parsing

