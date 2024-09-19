module KIND_MOD
implicit none
!FLOATING POINT KIND
integer,parameter ::  &
                 sp=selected_real_kind(6,37), &
                 dp=selected_real_kind(2*precision(1.0_sp)), &
                 qp=selected_real_kind(4*precision(1.0_sp)) 
!INTEGER KIND
integer,parameter ::  &
                 i4b=selected_int_kind(9), &
                 i2b=selected_int_kind(4), &
                 i1b=selected_int_kind(2)


integer, parameter :: ShortInt = i2b 
integer, parameter :: LongInt = i4b 

end module KIND_MOD
