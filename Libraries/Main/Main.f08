      program Main 
            use, non_intrinsic :: Etc_utilities_interface 
            use, non_intrinsic :: ADMP_DecPreprocs 
            use, non_intrinsic :: ADMP_INPUT_MOD 
            use, non_intrinsic :: ADMP_OUTPUT
            use, non_intrinsic :: Numeric_preprocessing
            use, non_intrinsic :: NUMERIC
            implicit none 
            ! START 
            ! USER PARSING SECTION 

            call Parsing(Opt_vec,Gnu_opt,PARSING_FLAG,Atom_ids,Ref_f)
            if (PARSING_FLAG .eqv. .TRUE. ) then 
                  stop 'ERROR TERMINATION VIA PARSING SUBPROGRAM'
            end if 
            ! INPUT SECTION START
            ! ASSOCIATION BLOCK, MORE READABLE CODE 
            associate(  logfilename => Opt_vec(1)%label     ,&
                        binflag     => Opt_vec(2)%flag      ,&                   
                        rmsdflag    => Opt_vec(3)%flag      ,&   
                        atomidfl    => Opt_vec(4)%flag      ,&   
                        gnupltfl    => Opt_vec(5)%flag      ,&   
                        reffrfla    => Opt_vec(6)%flag      )   


            ! CHECK FOR EXTENSION 
            select case  (logfilename( len_trim( logfilename )-3 : ) )
            case ('.log')
                  call G_Log_Check(logfilename,IO_CHECK_ERROR,Normal_term)

                  if (IO_CHECK_ERROR) stop 'ERROR TERMINATION VIA G_LOG_CHECK SUBPROGRAM'
                  
                   call G_LOG_INPUT(logfilename,At_Num,Step_Num,Coord,vel,Normal_term,IO_INPUT_ERROR)

                   if (binflag) then 
                  ! BIN GENERATION BRANCH
                        call Bin_gen(logfilename,coord,vel,Bin_filename,At_Num,Step_Num,bingen_flag)
                        if (bingen_flag) stop 'ERROR TERMINATION VIA G_LOG_CHECK SUBPROGRAM'
                  end if
            case ('.bin')
                  call G_Bin_Input(Bin_filename,coord,vel,Bin_in_flag,At_Num,Step_Num)
                  if (Bin_in_flag) stop 'ERROR TERMINATION VIA G_LOG_CHECK SUBPROGRAM'
            case default 
                  print *, 'INVALID EXTENTION FOR FILE: ' , logfilename
                  stop 
            end select

            ! INPUT SECTION END 
            ! NUMERIC SECTION 
            ! RMSD 
            if ( rmsdflag ) then 
                  if ( (atomidfl .eqv. .false.) .and. (reffrfla .eqv. .false.) ) then 
                        call RMSD(rmsd_vec,coord,Step_Num,At_Num,RMSDErrFlag)
                        print *, 'hello'
                  
                  else if ( (atomidfl .eqv. .true.) .and. (reffrfla .eqv. .false.) ) then
                        call RMSD(rmsd_vec,coord,Step_Num,At_Num,RMSDErrFlag,Atom_ids) 

                  else if ( (atomidfl .eqv. .false.) .and. (reffrfla .eqv. .true.) ) then
                        call RMSD(rmsd_vec,coord,Step_Num,At_Num,RMSDErrFlag,Ref_t=Ref_f) 

                  else if ( (atomidfl .eqv. .true.) .and. (reffrfla .eqv. .true.) ) then
                        call RMSD(rmsd_vec,coord,Step_Num,At_Num,RMSDErrFlag,Atom_ids,Ref_f) 
                  end if

                  if (gnupltfl) then 
                        ! call Gnu_plot(rmsd_vec,Gnu_Flag,gnupltfl,Gnu_opt)
                        ! GENERIC INTERFACES PER EVITARE TYPE MISMATCH
                        if (Gnu_Flag) stop 'ERROR TERMINATION VIA GNU_PLOT SUBPROGRAM'

                  end if 
            end if

            end associate 
      end program Main
