! GOAL :      GIVEN A REFERENCE STRUCTURE X(T1) (FOR INSTANCE THE COORDINATE VECTOR AT REFERENCE TIME T1)
            ! THE GOAL OF THIS PROCEDURE IS TO COMPUTE THE ROOT MEAN SQUARE DISTANCE OF THE SAME STRUCTURE
            ! AT DIFFERENT TIME RESPECT X(Ti) (WHERE Ti INDICATES THE TIME STEP) 
            ! THE USER SHOULD BE ABLE TO DEFINE THE GROUP OF ATOMS OF INTEREST (ALL MOLECULE BY DEFAULT) 
            ! AND THE REFERENCE STRUCTURE, FOR INSTANCE THE TIME STEP OF REFERENCE (FIRST STRUCTURE BY DEFAULT) 
            ! THE FORMULA USED IN THIS PROCEDURE IS SHOWED HERE:
            ! Schreiner W, Karch R, Knapp B, Ilieva N. Relaxation estimation of RMSD in molecular dynamics immunosimulations.
            ! Comput Math Methods Med. 2012;2012:173521. 
            ! doi:10.1155/2012/173521
! INPUT :     COORD -> COORDINATE VECTOR READ BY THE DYNAMIC. 
            ! REFERENCE_INDEX -> INTEGER,OPTIONAL,ALLOCATABLE ARRAY . 
            ! THE ATOM IDs INDIVIDUATING THE INTEREST STRUCTURE
            ! REFERENCE_TIME -> INTEGER  THE TIME STEP OF THE REFERENCE STRUCTURE 

! ------------------------------------------------------------------------------------------------------------------------------            
!  OUTPUT:    RMSD_VEC -> REAL(KIND=COORD),DIMENSION(SIZE(COORD)),INTENT(OUT) . RMSD VECTOR 
    
subroutine sp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
    use, intrinsic :: iso_fortran_env , only : real32,int8 !!! RICORDA DI CAMBIARE INT 8 A IB2 E USARE MODULO KIND
    implicit none
    ! DUMMY SECTION 
    real(kind=real32),intent(in), dimension(:) :: coord 
    integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids ! out necessary since default value must be given
    integer(kind=int8),intent(inout) ::     Step_Num    ,&  ! MAXIMUM STEP NUMBER 
                                            Natoms          ! ATOM NUMBER
    integer,intent(in),optional ::  REF_t       ! TIME STEP OF REFERENCE STRUCTURE
    real(kind=real32),intent(out),dimension(:),allocatable :: rmsd_vec

    ! LOCAL SECTION 
    ! IDEA : DEFINIRE PUNTATORE CHE VA A PRENDERE SOLO LE 
    ! COORDINATE DEGLI ATOMI DELLA STRUTTURA DI INTERESSE
    ! MAGARI A TUTTI I TIME STEP 
    ! L'UTILITA' DEL POINTER STA NEL FATTO CHE POSSO
    ! SFRUTTARE OP TRA ARRAY DI INTERESSE IN MANIERA OTTIMIZZATA
    ! PER ESEMIO ARR1 OP ARR2                            
    real(kind=real32),dimension(:),allocatable :: L2_vec
    real(kind=real32),dimension(:), allocatable :: Xr_vec , Xt_vec  ! XR_VEC WILL CONTAIN THE COORDINATES OF THE REFERENCE FRAME
                                                                    ! WHILE XT_VEC WILL CONTAIN THE OTHER COODINATES TO COMPARE WITH XR_VEC
    integer(kind=int8),dimension(:),allocatable :: spread_index 
    integer :: i,J=1 ! counter
    integer,dimension(:),allocatable :: slice_vec
    integer(kind=kind(At_ids)) :: I_one,I_two,I_three

    ! EQUALS GIVES PRIORITY TO INTEGER LITERAL CONSTANTS, I.E. INT FUNCTION REQUIRED TO PRESERVE KIND
      I_one   = int(1,kind(At_ids)) 
      I_two   = int(2,kind(At_ids))
      I_three = int(3,kind(At_ids))
! PART 1:   DEFINITION OF THE Xr_vec THAT CONTAINS THE COORDINATES OF THE REFERENCE STRUCTURE     
!           AND, IN CASE IT IS SPECIFIED,    
!           DEFINITION OF Xt_vec THAT CONTAINS THE COORDINATES 
!           OF THE REFERENCE STRUCTURE AT EACH TIME STEP        
!           FROM NOW ON THERE WILL BE DIFFERENT MODE TO OBTAIN THE REFERENCE FRAME AND THE
!           OTHER FRAMES BASED ON THE FACT THAT THE USER CAN:
!               1. Don't give anything, so the reference structure will be assumed to be the whole molecule at frame 1 (DEFAULT
!               MODE)
!               2. Don't give the reference frame but the reference structure is a subset of atoms 
!               3. Give a reference frame and specific subset of atoms 

    
    ! DEFAULT MODE  : NO ATOM IDS OR REFERENCE FRAME GIVEN IN INPUT. IN THIS CASE COORD=Xt_vec 
    !                 SINCE THE REFERENCE STRUCTURE WILL BE THE MOLECULE AS WHOLE AT THE FIRST
    !                 FRAME.
        if ( (.not. present(At_ids) ) .and. (.not. present(REF_t) ) ) then 
            ! THE REFERENCE VECTOR IS A REPETITION OVER THE WHOLE DIMENSION OF THE ARRAY COORD OF THE FIRST STRUCTURE
            Xr_vec= reshape (spread(coord(1:Natoms*3),2 ,Step_Num ),(/size(coord)/) ) 
            ! Xr_vec=coord(1:Natoms)
            Xt_vec=coord
    ! END DEFAULT MODE

    ! MODE 1    : THE REFERENCE STRUCTURE IS A (USER DEFINED) GROUP OF ATOMS IN THE FIRST FRAME
        else if ( (present(At_ids)) .and. (.not. present(REF_t) ) ) then 
            ! REMAPPING OF THE VECTOR OF INDEXES TO THE VECTOR THAT POINTS TO THE X VALUE OF EACH ATOM AT THE FIRST STEP
            At_ids=int((At_ids*3)-2,kind(At_ids))                             ! VECTOR TO POINT TO X POSITION OF EACH ATOM AT FIRST TIME STEP
            allocate(spread_index(size(At_ids)*3))                    ! SPREAD INDEX IS THE VECTOR NECESSARY IN SPREAD INTRINSIC FUNCTION 
                                                                      ! IT INDICATES THE INDEXES OF THE COORDINATES (THE COORD ELEMENTS) TO SPREAD
            ! SPREAD INDEX INIZIALIZATION
            do i=1,size(At_ids)
                spread_index(j)=At_ids(i)
                spread_index(j+1)=At_ids(i)+I_one
                spread_index(j+2)=At_ids(i)+I_two
                j=j+3
            end do
    
            Xr_vec= reshape (spread(coord(spread_index),2 ,Step_Num ),(/size(spread_index)*Step_Num/) ) ! VECTOR OF REFERENCE STRUCTURE REPEATED NSTEP TIME
            ! DEFINE A SLICE VECTOR THAT CONTAINS THE INDEXES OF THE REFERENCE STRUCTURE ATOMS AT EACH FRAME. 
            spread_index=int (spread_index-(Natoms*I_three*(Ref_t-I_one)),kind(At_ids))
            slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
            do i=1,Step_Num-1 
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*(i+1)))=&
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*(i+1)))+(Natoms*i*3 )
            end do        
            ! allocate(Xt_vec(size(slice_vec)))
            Xt_vec=coord(slice_vec)
            Natoms=int(size(At_ids),kind(At_ids))
        end if 
    ! END MODE 2

! END PART ONE

! PART 2: OBTAINING THE RMSD VECTOR 

        L2_vec=Xr_vec-Xt_vec
        allocate(rmsd_vec( Step_Num ))
    !$omp do 
        do i=1,Step_Num 
            rmsd_vec(i)=(norm2( L2_vec( (i-1)*Natoms*3 +1:i*Natoms*3) ))
        end do 
    !$omp end do 
    rmsd_vec=rmsd_vec/(Natoms**2._real32)
end subroutine sp_RMSD

subroutine dp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
    use, intrinsic :: iso_fortran_env , only : real64,int8 
    implicit none
    ! DUMMY SECTION 
    real(kind=real64),intent(in), dimension(:) :: coord 
    integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids 
    integer(kind=int8),intent(inout) ::    Step_Num    ,&  
                                Natoms          
    integer,intent(in),optional ::  REF_t       
    real(kind=real64),intent(out),dimension(:),allocatable :: rmsd_vec

    ! LOCAL SECTION 
    real(kind=real64),dimension(:),allocatable :: L2_vec
    real(kind=real64),dimension(:), allocatable :: Xr_vec , Xt_vec
    integer(kind=int8),dimension(:),allocatable :: spread_index 
    integer :: i,J=1 ! counter
    integer,dimension(:),allocatable :: slice_vec
    integer(kind=kind(At_ids)) :: I_one,I_two,I_three

      I_one   = int(1,kind(At_ids)) 
      I_two   = int(2,kind(At_ids))
      I_three = int(3,kind(At_ids))
    
        if ( (.not. present(At_ids) ) .and. (.not. present(REF_t) ) ) then 
            Xr_vec= reshape (spread(coord(1:Natoms*3),2 ,Step_Num ),(/size(coord)/) ) 
            Xt_vec=coord

        else if ( (present(At_ids)) .and. (.not. present(REF_t) ) ) then 
            At_ids=int((At_ids*3)-2,kind(At_ids))                           
            allocate(spread_index(size(At_ids)*3))        
                                                          
            do i=1,size(At_ids)
                spread_index(j)=At_ids(i)
                spread_index(j+1)=At_ids(i)+I_one
                spread_index(j+2)=At_ids(i)+I_two
                j=j+3
            end do
            Xr_vec= reshape (spread(coord(spread_index),2 ,Step_Num ),(/size(spread_index)*Step_Num/) ) 
            slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
            do i=1,Step_Num-1 
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*(i+1)))=&
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*(i+1)))+(Natoms*i*3 )
            end do 
            
            
            Xt_vec=coord(slice_vec)
            Natoms=int(size(At_ids),kind(At_ids))
    ! END MODE 1

    ! MODE 2    : THE REFERENCE STRUCTURE IS A SPECIFIED GROUP OF ATOMS OF A SPECIFIC FRAME
        else if ( (present(At_ids)) .and. (present(REF_t)) ) then 
            
            At_ids=int( ( (At_ids*3)-2)+( (Natoms*3)*(REF_T-1) ),kind(At_ids) )        
            allocate(spread_index(size(At_ids)*3))              
                                                                
            
            do i=1,size(At_ids)
                spread_index(j)=At_ids(i)
                spread_index(j+1)=At_ids(i)+I_one
                spread_index(j+2)=At_ids(i)+I_two
                j=j+3
            end do
    
            Xr_vec= reshape (spread(coord(spread_index),2 ,Step_Num ),(/size(spread_index)*Step_Num/) ) 
            
            spread_index=int(spread_index-(Natoms*I_three*(Ref_t-I_one)),kind(At_ids))
            slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
            do i=1,Step_Num-1 
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)=&
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)+(Natoms*i*3 )
            end do        
            
            Xt_vec=coord(slice_vec)
            Natoms=int(size(At_ids),kind(At_ids))
        end if 
    ! END MODE 2

! END PART ONE

! PART 2: OBTAINING THE RMSD VECTOR 

        L2_vec=Xr_vec-Xt_vec
        allocate(rmsd_vec( Step_Num ))
        !$omp do
        do i=1,Step_Num
            rmsd_vec(i)=(norm2( L2_vec( (i-1)*Natoms*3 +1:i*Natoms*3) ))
        end do 
        !$omp end do
        rmsd_vec=rmsd_vec/(Natoms**2._real64)
end subroutine dp_RMSD


subroutine qp_RMSD (rmsd_vec,coord,Step_Num,Natoms,AT_ids,REF_t)
    use, intrinsic :: iso_fortran_env , only : real128,int8 
    implicit none
    ! DUMMY SECTION 
    real(kind=real128),intent(in), dimension(:) :: coord 
    integer(kind=int8),intent(inout),dimension(Natoms),optional :: AT_ids 
    integer(kind=int8),intent(inout) ::    Step_Num    ,&  
                                Natoms          
    integer,intent(in),optional ::  REF_t       
    real(kind=real128),intent(out),dimension(:),allocatable :: rmsd_vec

    ! LOCAL SECTION 
    real(kind=real128),dimension(:),allocatable :: L2_vec
    real(kind=real128),dimension(:), allocatable :: Xr_vec , Xt_vec
    integer(kind=int8),dimension(:),allocatable :: spread_index 
    integer :: i,J=1 ! counter
    integer,dimension(:),allocatable :: slice_vec
    integer(kind=kind(At_ids)) :: I_one,I_two,I_three

        I_one   = int(1,kind(At_ids)) 
        I_two   = int(2,kind(At_ids))
        I_three = int(3,kind(At_ids))
    ! DEFAULT MODE  : NO ATOM IDS OR REFERENCE FRAME GIVEN IN INPUT. IN THIS CASE COORD=Xt_vec
        if ( (.not. present(At_ids) ) .and. (.not. present(REF_t) ) ) then 
            Xr_vec= reshape (spread(coord(1:Natoms*3),2 ,Step_Num ),(/size(coord)/) ) 
            ! Xr_vec=coord(1:Natoms)
            Xt_vec=coord
    ! END DEFAULT MODE

    ! MODE 1    : THE REFERENCE STRUCTURE IS A (USER DEFINED) GROUP OF ATOMS IN THE FIRST FRAME
        else if ( (present(At_ids)) .and. (.not. present(REF_t) ) ) then 
            At_ids=(At_ids*I_three)-I_two                   
            allocate(spread_index(size(At_ids)*3))          
                                                            
            
            do i=1,size(At_ids)
                spread_index(j)=At_ids(i)
                spread_index(j+1)=At_ids(i)+I_one
                spread_index(j+2)=At_ids(i)+I_two
                j=j+3
            end do
            
            Xr_vec= reshape (spread(coord(spread_index),2 ,Step_Num ),(/size(spread_index)*Step_Num/) ) 
            
            slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
            do i=1,Step_Num-1 
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)=&
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)+(Natoms*i*3 )
            end do 
            
            
            Xt_vec=coord(slice_vec)
            Natoms=int(size(At_ids),kind(At_ids))
    ! END MODE 1

    ! MODE 2    : THE REFERENCE STRUCTURE IS A SPECIFIED GROUP OF ATOMS OF A SPECIFIC FRAME
        else if ( (present(At_ids)) .and. (present(REF_t)) ) then 
            
            At_ids=int( ( (At_ids*3)-2)+( (Natoms*3)*(REF_T-1) ),kind(At_ids) )        
            allocate(spread_index(size(At_ids)*3))              
                                                                
            
            do i=1,size(At_ids)
                spread_index(j)=At_ids(i)
                spread_index(j+1)=At_ids(i)+I_one
                spread_index(j+2)=At_ids(i)+I_two
                j=j+3
            end do
    
            Xr_vec= reshape (spread(coord(spread_index),2 ,Step_Num ),(/size(spread_index)*Step_Num/) ) 
            
            spread_index=int(spread_index-(Natoms*I_three*(Ref_t-I_three)),kind(At_ids))
            slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
            do i=1,Step_Num-1 
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)=&
                slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)+(Natoms*i*3 )
            end do        
            
            Xt_vec=coord(slice_vec)
            Natoms=int(size(At_ids),kind(At_ids))
        end if 
    ! END MODE 2

! END PART ONE

! PART 2: OBTAINING THE RMSD VECTOR 

        L2_vec=Xr_vec-Xt_vec
        allocate(rmsd_vec( Step_Num ))
        !$omp do
        do i=1,Step_Num
            rmsd_vec(i)=(norm2( L2_vec( (i-1)*Natoms*3 +1:i*Natoms*3) ))
        end do
        !$omp end do
        rmsd_vec=rmsd_vec/(Natoms**2._real128)
end subroutine qp_RMSD


    

    


