function Up_to_low(string)
    implicit none
    character(len=*),intent(in) :: string 
    character(len=len_trim(string)) :: Up_to_low
    integer ::  i             ! COUNTER 
    intrinsic :: achar

    Up_to_low=trim(string)
    do i=1,len_trim(string)      
        select case (Up_to_low(i:i))
            case ('A':'Z')
                Up_to_low(i:i)=achar(  (ichar(string(i:i) ))+32  )
            case default
                cycle
        end select
    end do

end function Up_to_low