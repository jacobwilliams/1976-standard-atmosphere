program test

use COESA_module
use iso_fortran_env, wp => real64

implicit none

real(wp) :: d
integer :: i
real(wp),dimension(3),parameter :: alts = [0.0_wp, 84852.0_wp, 86000.0_wp]

write(*,*) 'test'

do i = 1, size(alts)

    d = COESA_density(alts(i))
    write(*,*) alts(i),d

end do 
! write(*,*) 84852, COESA_density(84852.0_wp)
! write(*,*) 86000, COESA_density(86000.0_wp)

end program test