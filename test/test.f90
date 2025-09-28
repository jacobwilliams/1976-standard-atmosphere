program test

use COESA_module, wp => coesa_wp

implicit none

real(wp) :: d
integer :: i
real(wp),dimension(*),parameter :: alts = [ 0.0_wp,  &
                                            5.0_wp, &
                                            10.0_wp, &
                                            80000.0_wp, &
                                            80000.1_wp, &
                                            80000.5_wp, &
                                            80000.6_wp, &
                                            81000.0_wp, &
                                            81000.1_wp, &
                                            81000.5_wp, &
                                            81000.6_wp, &
                                            82000.0_wp, &
                                            82000.1_wp, &
                                            82000.5_wp, &
                                            82000.6_wp, &
                                            83000.0_wp, &
                                            83000.1_wp, &
                                            83000.5_wp, &
                                            83000.6_wp, &
                                            84000.0_wp, &
                                            84000.1_wp, &
                                            84000.5_wp, &
                                            84000.6_wp, &
                                            84852.0_wp, &
                                            84852.1_wp, &
                                            85000.0_wp, &
                                            85000.1_wp, &
                                            85000.5_wp, &
                                            85000.6_wp, &
                                            86000.0_wp, &
                                            86000.1_wp, &
                                            91000.0_wp, &
                                            91000.1_wp, &
                                            110000.0_wp, &
                                            110000.1_wp, &
                                            120000.0_wp, &
                                            120000.1_wp, &
                                            200000.0_wp, &
                                            300000.0_wp, &
                                            400000.0_wp, &
                                            500000.0_wp, &
                                            1000000.0_wp ]

write(*,*) ''
call coesa_initialize()
write(*,*) ''
write(*,*) '-------'

write(*,*) ''
do i = 1, size(alts)
    d = COESA_density(alts(i))
    write(*,*) alts(i),d
end do
write(*,*) ''

end program test