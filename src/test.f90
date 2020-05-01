program test

use COESA_module
use iso_fortran_env, wp => real64

implicit none

write(*,*) 0,     COESA_density(0.0_wp)
!write(*,*) 84852, COESA_density(84852.0_wp)
write(*,*) 86000, COESA_density(86000.0_wp)

end program test