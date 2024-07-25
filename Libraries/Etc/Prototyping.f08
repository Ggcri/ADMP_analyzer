program Dynamic_procs
use, non_intrinsic :: Etc_utilities_interface, only : Up_to_low,Parsing,opt_flags
use, intrinsic :: iso_fortran_env, only : real64,real128
implicit real (x,y,z) 
implicit integer (a-d,i,j,k)
implicit character(len=256) (s)
character(len=17) ::    Units_char  ! UNITS SUBSTRING 'UNITS=UNITSOPT'
character(len=256) :: line,G_version= 'ES64L-GDVRevJ.02'
character(len=2) :: TRY,dumchar
real(kind=real64),dimension(12*3*3) :: coord2=0.,coord_bin=0.,vel2=0
real,dimension(20),target :: arr1=1
real,dimension(:,:),allocatable :: arr2
real(kind=real64),dimension(:),allocatable :: arr3,vel3
real,pointer,dimension(:) :: p1 => null()
real,dimension(3) :: arr4=30
integer,dimension(3) :: slice=(/2,5,9/)
integer :: Step_Num,leng
! TEST SU FUNZIONI DI MANIPOLAZIONE ARRAY
logical,dimension(:),allocatable :: mask
integer, dimension(2) :: At_ids=(/1,4/)
integer, dimension(6) :: spread_index
real,allocatable,dimension(:) :: X_rvec
integer,dimension(:),allocatable :: slice_vec
integer :: Natoms,Ref_t,t=1,N,k,J=1,Nstep,L=1
real(kind=real128),dimension(25000*12*3) :: storage_try 


! TESTING DELL'EXTENTION CHECK
! line=' blabla.log'
! print *, line
! print *, line( len_trim(line)-3:)


 open(unit=10,file='Prototype.txt',action='read')

!  TESTING VERSION CHECH 
!  do 
!   read(10,'(a)') line 
!   if (index(line,'Gaussian DV') .ne. 0 ) then 

!       if (index( line,G_version) .ne. 0 ) then 
!           print *, ' VERSION NOT COMPATIBLE'
!           ! print *, 'index is', index(line(len('Gaussian_DV'):),G_version)
!       else 
!           print *, 'VERSION COMPATIBLE'
!         ! print *, 'index is', index(line(len('Gaussian_DV'):),G_version)

!       end if
!       exit 
!   end if
!  end do 
! rewind(10)

! TESTING UNIT CHECK AND READ 
!do  
!        read(10,'(a)') line
!        if (scan(line,'#') .ne. 0) then
!        print *, line
!        exit
!        end if
!end do
! try='Z'
! s_1='abcdefgh units=(Au,Ang)  jdj'
! s_2='abcdefgh units=Au  jdj'
! a_index=index(s_1,'units=')
!print *, s_1(a_index+6:)
! read(s_1(a_index:),'(a)') Units_char



!print *, Units_char
! Se c'è una parentesi c'è una virgola
! if (index(Units_char,')') .ne. 0 ) Units_char=Units_char(8:index(Units_char,')')-1)
  ! allocate(mask(12*3*3)) 
! mask=.false.
! mask(:12*3*2)=.true.

! TESTING VELOCITY AND COORD READ
 k=1 
 L=1

outer : do 
    read(10,'(a)',iostat=j,end=1000) line  
    if (index(line,'MW Cartesian velocity:') /= 0) then
        do i=1,12
            read(10,'(a)') line 
             !print *, line 
            read(line(11:),'(2x,d20.12,2(4x,d20.12))',iostat=ios) vel2(k:) 
             !write(*,'(3(d20.12,3x))') vel2(k:k+2) 
            !print *, 'k is',k 
            k=k+3 
        end do 
      else if (index(line,'Input orientation:') /= 0  ) then
        !print *,' trigger works'
        read(10,'(3/)')
        do i=1,12
            read(10,'(a)') line
            ! print *, line
            ! print *, line(35:)
            read(line(35:),*,iostat=ios) coord2(L:)
            L=L+3
        end do
    end if 
 end do outer 
 rewind(10) 
 1000 continue

! write(*,'(3(f10.7,3x))') coord2

!  vel3= pack(vel2,mask)
! 
!  write(*,'( 3(d20.11) )') vel3
!  stop
!  print *, size(vel2)




 
! TEST RMSD MODE 2
    ! IN QUESTA MODALILTA' L'UTENTE HA FORNITO SOLO UN ATOM ID 
    ! QUINDI LA STRUTTURA DI RIFERIMENTO VERRA' PRESA DAL PRIMO STEP 
    ! DATA  
      ! Natoms=12
      ! Ref_t=3
      ! Step_Num=3
      ! At_ids=(/ 3,7 /)

    ! START 
      ! At_ids= ( (At_ids*3)-2 )     
      ! j=1
      !  do i=1,size(At_ids)
        !  spread_index(j)=At_ids(i)
        !  spread_index(j+1)=At_ids(i)+1
        !  spread_index(j+2)=At_ids(i)+2
        !  j=j+3
      !  end do
      ! print *, 'SPREAD INDEX'
      ! print '(*(i0,3x))' , spread_index
      ! Assegnazione struttura di riferimento TODO
      ! slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) ! NOTA N STEP = 3 IN SPREAD FUNCTION QUESTO CORRISPONDE AL NUMERO DI COPIE DA FARE
      ! 
      ! print '(*(i0,/))',  slice_vec
      ! SLICE VEC QUI è A UNO STEP INTERMEDIO DOVE RIPORTA SOLO GLI INDICATORI DI POSIZIOINE DELLA COORDINATE
      ! IN SEQUENZA DEGLI ATOMI DI INTERESSE RIPETURE PER UN NUMERO DI VOLTE PARI AL NUMERO DI STEP 
      ! print '(100("-"))' 
      ! print *, 'SIZE SPREAD INDEX IS', size(spread_index)
      ! do i=1,Step_Num-1 !LIMIOTE SUPERIORE NSTEP-1
          ! slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)=&
          ! slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)+(Natoms*i*3 )
      ! 
      ! 
      ! end do
      ! print '(*(i0,/))',  slice_vec
      ! SLICE VEC VA A PRENDERE GLI INDICATORI DI POSIZIONE DELLE COORDINATE (XYZ) 
      ! DI OGNI ATOMO SPECIFICATO DALL'UTENTE IN ATOM ID AD OGNI TIME STEP A PARTIRE DAL PRIMO
      
    

! END TEST MODE 2


! TEST MODE 3:
  ! IN QUESTA MODALILTA' L'UTENTE NON HA FORNITO SOLO UN ATOM ID 
  ! MA ANCHE UNO STEP A CUI FAR RIFERIMENTO 
  
    ! LA PROCEDURA PER PRENDERE LA STRUTTURA AI DIVERSI TIME STEP NON DOVREBBE CAMBIARE
    ! QUELLO CHE CAMBIA E' COME SI PRENDE LA STRUTTURA DI RIFERIMENTO 
    ! Natoms=12
    ! Ref_t=3
    ! Step_Num=3
    ! At_ids=(/ 3,7 /)
    ! At_ids= ( (At_ids*3)-2 )+ (Natoms*3)*(Ref_t-1) ! VERIFIED OK
    ! print '(5("-"),/,i0)' , At_ids
  ! 
  ! j=1
  ! do i=1,size(At_ids)
    ! spread_index(j)=At_ids(i)
    ! spread_index(j+1)=At_ids(i)+1
    ! spread_index(j+2)=At_ids(i)+2
    ! j=j+3
  ! end do
  ! print *, 'SPREAD INDEX INDICATING THE COORDINATES OF REFEREMENT STRUCTURE' 
  ! print ' (i0) ' ,spread_index
  ! print *, ' THE COORDINATES OF THE REFERENCE STRUCTURE ARE' 
  ! print '(3(1x,f0.10),/)', coord2(spread_index)
  ! open(unit=122,status='replace',action='write',position='append',file='spreading_vector.txt')
  ! 
  ! write(122,'(f0.13)') reshape( spread( coord2(spread_index),2,Step_Num   ),[Step_Num*size(spread_index)]  ) 
  ! THE OBTAINEMENT OF THE REFERENCE STUTURE AT A GIVEN TIME STEP IS VERIFIED 
  ! NOW LETS VERIFY IF THE SAME ALGO FOR THE STRUCTURE AT DIFFERENT TIME STEP WORKS WELL 
  ! spread_index=spread_index-(Natoms*3*(Ref_t-1))
  ! slice_vec= reshape (   (spread(spread_index,2,Step_Num)),(/size(spread_index)*Step_Num/)) 
  ! do i=1,Step_Num-1 !LIMIOTE SUPERIORE NSTEP-1
    ! slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)=&
    ! slice_vec((size(spread_index)*i)+1:(size(spread_index)*i)+1+size(spread_index)-1)+(Natoms*i*3 )
  ! end do
  ! print '(i0)', slice_vec
! END TEST MODE 3
      ! DIRECT ACCESS TEST START

      line=' '
      inquire(iolength=leng) coord2
      open(unit=133,file='Binary.bin',form='unformatted',status='replace',recl=leng,access='direct')

      ! do j=1,size(coord2)/3
        ! write(133,rec=j+1) coord2( j*3-2:j*3 )
        ! INTRODURRE VARIABILE PER L'INDICE DELLE COORDINATE
        ! LOOP FACILE DA PARALLELIZZARE 
        ! LOOP LEVEL PARALLELIZATION
      ! end do
      write(133,rec=1) 12,3
      write(133,rec=2) coord2
  ! TRY TO RETRIVE DATA WITH DIRECT DATA TRANSFER 
      read(133,rec=2) coord_bin 
       write(*, '(///,3(f10.7,3x))') coord_bin
      close(133)
      
      
      ! open(134,file='Binary.bin',form='unformatted',status='old',action='read',access='direct',recl=leng) 
      
      ! read(134,rec=1) Natoms,Nstep
      ! print *, Natoms,Nstep

      ! At_ids=(/1,4/)
      ! do j=1,Nstep
      !   do k=1,size(At_ids)
      !       N=At_ids(k)+(Natoms * (j-1) )+1
      !       ! print *, 'At_ids(k) is', At_ids(k)
      !       ! print *, 'N is' , N 
      !       read(134,rec=N) coord2(t:t+2)
      !        print *, coord2(t:t+2)
      !       t=t+3
      !   end do 
      ! end do 

! DIRECT ACCESS TEST END 


! PARSING SUBROUTINE TEST S 

end program Dynamic_procs