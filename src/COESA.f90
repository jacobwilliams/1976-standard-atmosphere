!************************************************************************************
!>
!  1976 Standard Atmosphere Model
!
!### History
!  * Original Julia code from: https://github.com/danielmatz/COESA.jl [MIT License]
!  * Jacob Williams, converted to Fortran 4/30/2020

    module COESA_module

    use iso_fortran_env, wp => real64

    implicit none

    private

    real(wp),parameter :: r0 = 6356766.0_wp ! (m), effective Earth radius at 45 deg N latitude
    real(wp),parameter :: g0 = 9.80665_wp ! (m / s^2) or (m^2 / s^2 m')
    real(wp),parameter :: M0 = 28.9644_wp ! (kg / kmol)
    real(wp),parameter :: Rstar = 8.31432e3_wp ! (N m / kmol K)
    real(wp),parameter :: gamma = 1.4_wp
    real(wp),dimension(*),parameter :: Hb = [0, 11, 20, 32, 47, 51, 71] * 1000.0_wp ! (m')
    real(wp),dimension(*),parameter :: Lmb = [-6.5_wp, 0.0_wp, 1.0_wp, 2.8_wp, 0.0_wp, -2.8_wp, -2.0_wp] / 1000.0_wp ! (K / m')
    real(wp),dimension(*),parameter :: Ztable = [80.0_wp, 80.5_wp, 81.0_wp, 81.5_wp, 82.0_wp, 82.5_wp, 83.0_wp, &
                                                83.5_wp, 84.0_wp, 84.5_wp, 85.0_wp, 85.5_wp, 86.0_wp] * 1000 ! (m)
    real(wp),dimension(*),parameter :: Mratiotable = [1.0_wp, 0.999996_wp, 0.999989_wp, 0.999971_wp, 0.999941_wp, &
                                                    0.999909_wp, 0.999870_wp, 0.999829_wp, 0.999786_wp, 0.999741_wp, &
                                                    0.999694_wp, 0.999641_wp, 0.999579_wp]

    ! based on David's code, which was based on Regan
    ! M and log(P) are interpolated quadratically
    real(wp),dimension(*),parameter :: Ztableupper = [&
        86000.0_wp,  87000.0_wp,  88000.0_wp,  89000.0_wp,  90000.0_wp,  91000.0_wp, 93000.0_wp,  &
        95000.0_wp,  97000.0_wp,  99000.0_wp, 101000.0_wp, 103000.0_wp, 105000.0_wp, 107000.0_wp, &
        109000.0_wp, 110000.0_wp, 111000.0_wp, 112000.0_wp, 113000.0_wp, 114000.0_wp, 115000.0_wp, &
        116000.0_wp, 117000.0_wp, 118000.0_wp, 119000.0_wp, 120000.0_wp, 125000.0_wp, 130000.0_wp, &
        135000.0_wp, 140000.0_wp, 145000.0_wp, 150000.0_wp, 160000.0_wp, 170000.0_wp, 180000.0_wp, &
        190000.0_wp, 200000.0_wp, 210000.0_wp, 220000.0_wp, 230000.0_wp, 240000.0_wp, 250000.0_wp, &
        260000.0_wp, 270000.0_wp, 280000.0_wp, 290000.0_wp, 300000.0_wp, 310000.0_wp, 320000.0_wp, &
        330000.0_wp, 340000.0_wp, 350000.0_wp, 360000.0_wp, 370000.0_wp, 380000.0_wp, 390000.0_wp, &
        400000.0_wp, 410000.0_wp, 420000.0_wp, 430000.0_wp, 440000.0_wp, 450000.0_wp, 460000.0_wp, &
        470000.0_wp, 480000.0_wp, 490000.0_wp, 500000.0_wp, 525000.0_wp, 550000.0_wp, 575000.0_wp, &
        600000.0_wp, 625000.0_wp, 650000.0_wp, 675000.0_wp, 700000.0_wp, 725000.0_wp, 750000.0_wp, &
        775000.0_wp, 800000.0_wp, 825000.0_wp, 850000.0_wp, 875000.0_wp, 900000.0_wp, 925000.0_wp, &
        950000.0_wp, 975000.0_wp, 1000000.0_wp] ! (m)
    real(wp),dimension(*),parameter :: Ptableupper = [&
        3.7338E-1_wp, 3.1259E-1_wp, 2.6173E-1_wp, 2.1919E-1_wp, 1.8359E-1_wp, &
        1.5381E-1_wp, 1.0801E-1_wp, 7.5966E-2_wp, 5.3571E-2_wp, 3.7948E-2_wp, 2.7192E-2_wp, &
        1.9742E-2_wp, 1.4477E-2_wp, 1.0751E-2_wp, 8.1142E-3_wp, 7.1042E-3_wp, 6.2614E-3_wp, &
        5.5547E-3_wp, 4.9570E-3_wp, 4.4473E-3_wp, 4.0096E-3_wp, 3.6312E-3_wp, 3.3022E-3_wp, &
        3.0144E-3_wp, 2.7615E-3_wp, 2.5382E-3_wp, 1.7354E-3_wp, 1.2505E-3_wp, 9.3568E-4_wp, &
        7.2028E-4_wp, 5.6691E-4_wp, 4.5422E-4_wp, 3.0395E-4_wp, 2.1210E-4_wp, 1.5271E-4_wp, &
        1.1266E-4_wp, 8.4736E-5_wp, 6.4756E-5_wp, 5.0149E-5_wp, 3.9276E-5_wp, 3.1059E-5_wp, &
        2.4767E-5_wp, 1.9894E-5_wp, 1.6083E-5_wp, 1.3076E-5_wp, 1.0683E-5_wp, 8.7704E-6_wp, &
        7.2285E-6_wp, 5.9796E-6_wp, 4.9630E-6_wp, 4.1320E-6_wp, 3.4498E-6_wp, 2.8878E-6_wp, &
        2.4234E-6_wp, 2.0384E-6_wp, 1.7184E-6_wp, 1.4518E-6_wp, 1.2291E-6_wp, 1.0427E-6_wp, &
        8.8645E-7_wp, 7.5517E-7_wp, 6.4468E-7_wp, 5.5155E-7_wp, 4.7292E-7_wp, 4.0642E-7_wp, &
        3.5011E-7_wp, 3.0236E-7_wp, 2.1200E-7_wp, 1.5137E-7_wp, 1.1028E-7_wp, 8.2130E-8_wp, &
        6.2601E-8_wp, 4.8865E-8_wp, 3.9048E-8_wp, 3.1908E-8_wp, 2.6611E-8_wp, 2.2599E-8_wp, &
        1.9493E-8_wp, 1.7036E-8_wp, 1.5051E-8_wp, 1.3415E-8_wp, 1.2043E-8_wp, 1.0873E-8_wp, &
        9.8635E-9_wp, 8.9816E-9_wp, 8.2043E-9_wp, 7.5138E-9_wp] ! (Pa)
    real(wp),dimension(*),parameter :: logPtableupper = log(Ptableupper)
    real(wp),dimension(*),parameter :: Mtableupper = [&
        28.95_wp, 28.95_wp, 28.94_wp, 28.93_wp, 28.91_wp, 28.89_wp, 28.82_wp, 28.73_wp, &
        28.62_wp, 28.48_wp, 28.30_wp, 28.10_wp, 27.88_wp, 27.64_wp, 27.39_wp, 27.27_wp, 27.14_wp, 27.02_wp, &
        26.90_wp, 26.79_wp, 26.68_wp, 26.58_wp, 26.48_wp, 26.38_wp, 26.29_wp, 26.20_wp, 25.80_wp, 25.44_wp, &
        25.09_wp, 24.75_wp, 24.42_wp, 24.10_wp, 23.49_wp, 22.90_wp, 22.34_wp, 21.81_wp, 21.30_wp, 20.83_wp, &
        20.37_wp, 19.95_wp, 19.56_wp, 19.19_wp, 18.85_wp, 18.53_wp, 18.24_wp, 17.97_wp, 17.73_wp, 17.50_wp, &
        17.29_wp, 17.09_wp, 16.91_wp, 16.74_wp, 16.57_wp, 16.42_wp, 16.27_wp, 16.13_wp, 15.98_wp, 15.84_wp, &
        15.70_wp, 15.55_wp, 15.40_wp, 15.25_wp, 15.08_wp, 14.91_wp, 14.73_wp, 14.54_wp, 14.33_wp, 13.76_wp, &
        13.09_wp, 12.34_wp, 11.51_wp, 10.62_wp,  9.72_wp,  8.83_wp,  8.00_wp,  7.24_wp,  6.58_wp,  6.01_wp, &
        5.54_wp,   5.16_wp,  4.85_wp,  4.60_wp,  4.40_wp,  4.25_wp,  4.12_wp,  4.02_wp,  3.94_wp] ! (kg / kmol)

    ! subroutine initialize()
    !     integer :: i
    !     Tmb = [288.15] ! (K)
    !     do i = 1, (size(Hb) - 1)
    !         Tmb = [Tmb, Tmb(i) + Lmb(i) * (Hb(i + 1) - Hb(i))]
    !     end do
    !     Pb = [101325.0]
    !     do i = 1, (size(Hb) - 1)
    !         if (Lmb(i) == 0) then
    !             Pb = [Pb, Pb(i) * exp(-g0 * M0 * (Hb(i + 1) - Hb(i)) / (Rstar * Tmb(i)))]
    !         else
    !             Pb = [Pb, Pb(i) * (Tmb(i) / (Tmb(i) + Lmb(i) * (Hb(i + 1) - Hb(i)))) ** (g0 * M0 / (Rstar * Lmb(i)))]
    !         end if
    !     end do
    !     write(*,*) ''
    !     write(*,'(A,*(E30.18E3,1x))') 'Tmb = ', Tmb
    !     write(*,*) ''
    !     write(*,'(A,*(E30.18E3,1x))') 'Pb = ', Pb
    !     write(*,*) ''
    ! end subroutine initialize

    real(wp),dimension(*),parameter :: Tmb = [0.288149993896484375E+003_wp, &
                                              0.216649993896484375E+003_wp, &
                                              0.216649993896484375E+003_wp, &
                                              0.228649993896484375E+003_wp, &
                                              0.270649993896484375E+003_wp, &
                                              0.270649993896484375E+003_wp, &
                                              0.214649993896484375E+003_wp]
    real(wp),dimension(*),parameter :: Pb = [0.101325000000000000E+006_wp, &
                                             0.226320631419326419E+005_wp, &
                                             0.547488824962697163E+004_wp, &
                                             0.868018574313198656E+003_wp, &
                                             0.110906285838442443E+003_wp, &
                                             0.669388604563498717E+002_wp, &
                                             0.395641939562468359E+001_wp]

    type,public :: State
        real(wp) :: mean_molecular_weight = 0.0_wp
        real(wp) :: temperature = 0.0_wp
        real(wp) :: pressure = 0.0_wp
        real(wp) :: speed_of_sound = 0.0_wp
        contains
        procedure :: density
    end type State

    public :: COESA_atmosphere
    public :: COESA_density

    contains
!************************************************************************************

pure function find(x, xvec) result(i)
    real(wp),intent(in) :: x
    real(wp),dimension(:),intent(in) :: xvec
    integer :: i
    i = 1
    do
        if (i >= size(xvec)) exit
        if (x <= xvec(i + 1)) exit
        i = i + 1
    end do
end function find

pure real(wp) function density(s)
    class(State),intent(in) :: s
    density = s%pressure * s%mean_molecular_weight / (Rstar * s%temperature)
end function density

pure real(wp) function geopotential_altitude(Z)
    real(wp),intent(in) :: Z
    geopotential_altitude = r0 * Z / (r0 + Z)
end function geopotential_altitude

pure function findb(H) result(b)
    real(wp),intent(in) :: H
    integer :: b
    b = find(H, Hb) - 1
end function findb

pure real(wp) function Tm(H)
    real(wp),intent(in) :: H
    integer :: i, b
    b = findb(H)
    i = b + 1
    Tm = Tmb(i) + Lmb(i) * (H - Hb(i))
end function Tm

pure real(wp) function temperature_lower(H, M)
    real(wp),intent(in) :: H
    real(wp),intent(in) :: M
    temperature_lower = Tm(H) / M0 * M
end function temperature_lower

pure real(wp) function pressure_lower(H)
    real(wp),intent(in) :: H
    integer :: i, b
    b = findb(H)
    i = b + 1
    if (Lmb(i) == 0) then
        pressure_lower = Pb(i) * exp(-g0 * M0 * (H - Hb(i)) / (Rstar * Tmb(i)))
    else
        pressure_lower = Pb(i) * (Tmb(i) / (Tmb(i) + Lmb(i) * (H - Hb(i)))) ** (g0 * M0 / (Rstar * Lmb(i)))
    end if
end function pressure_lower

pure real(wp) function speed_of_sound_lower(T, M)
    real(wp),intent(in) :: T, M
    speed_of_sound_lower = sqrt(gamma * Rstar * T / M)
end function speed_of_sound_lower

pure real(wp) function mean_molecular_weight_ratio_lower(Z)
    real(wp),intent(in) :: Z
    integer :: i
    if (Z < Ztable(1)) then
        mean_molecular_weight_ratio_lower = 1.0_wp
    else if (Z > Ztable(size(Ztable))) then
        error stop "altitude above maximum value in table"
    else
        i = find(Z, Ztable)
        mean_molecular_weight_ratio_lower = &
            Mratiotable(i) + (Mratiotable(i + 1) - Mratiotable(i)) / &
            (Ztable(i + 1) - Ztable(i)) * (Z - Ztable(i))
    end if
end function mean_molecular_weight_ratio_lower

pure real(wp) function mean_molecular_weight_lower(Z)
    real(wp),intent(in) :: Z
    mean_molecular_weight_lower = M0 * mean_molecular_weight_ratio_lower(Z)
end function mean_molecular_weight_lower

pure real(wp) function temperature_upper(Z)
    real(wp),intent(in) :: Z
    real(wp) :: Tc,A,aa,T9,LK9,Z9,T10,Z10,Tinf,lambda,xi
    if (Z <= 91000.0_wp) then
        temperature_upper = 186.8673_wp ! (K)
    elseif (Z <= 110000.0_wp) then
        Tc = 263.1905_wp ! (K)
        A = -76.3232_wp ! (K)
        aa = -19.9429_wp * 1000.0_wp ! (m)
        temperature_upper = Tc + A * sqrt(1.0_wp - ((Z - 91000.0_wp) / aa) ** 2)
    elseif (Z <= 120000.0_wp) then
        T9 = 240.0_wp ! (K)
        LK9 = 12.0_wp / 1000.0_wp ! (K / m)
        Z9 = 110000.0_wp ! (m)
        temperature_upper = T9 + LK9 * (Z - Z9)
    elseif (Z <= 1000000.0_wp) then
        T10 = 360.0_wp ! (K)
        Z10 = 120000.0_wp ! (m)
        Tinf = 1000.0_wp ! (K)
        lambda = 0.01875_wp / 1000.0_wp ! (1 / m)
        xi = (Z - Z10) * (r0 + Z10) / (r0 + Z)
        temperature_upper = Tinf - (Tinf - T10) * exp(-lambda * xi)
    end if
end function temperature_upper

pure integer function interpolation_index(Z)
    real(wp),intent(in) :: Z
    integer :: i
    ! Find the index for the lower side of the altitude interval
    i = find(Z, Ztableupper)
    ! We are going to reference all elements from i - 1 to i + 1, so we need to
    ! adjust the index away from the boundaries
    if (i==1) i = 2
    interpolation_index = i
end function interpolation_index

pure subroutine interpolation_scale_factors(i, Z, scale0, scale1, scale2)
    integer,intent(in) :: i
    real(wp),intent(in) :: Z
    real(wp),intent(out) :: scale0, scale1, scale2
    real(wp) :: Z0,Z1,Z2
    Z0 = Ztableupper(i - 1)
    Z1 = Ztableupper(i)
    Z2 = Ztableupper(i + 1)
    scale0 = (Z - Z1) * (Z - Z2) / ((Z0 - Z1) * (Z0 - Z2))
    scale1 = (Z - Z0) * (Z - Z2) / ((Z1 - Z0) * (Z1 - Z2))
    scale2 = (Z - Z0) * (Z - Z1) / ((Z2 - Z0) * (Z2 - Z1))
end subroutine interpolation_scale_factors

pure real(wp) function pressure_upper(Z)
    real(wp),intent(in) :: Z
    integer :: i
    real(wp) :: logP0,logP1,logP2,logP,scale0,scale1,scale2
    i = interpolation_index(Z)
    call interpolation_scale_factors(i, Z, scale0, scale1, scale2)
    logP0 = logPtableupper(i - 1)
    logP1 = logPtableupper(i)
    logP2 = logPtableupper(i + 1)
    logP = logP0 * scale0 + logP1 * scale1 + logP2 * scale2
    pressure_upper = exp(logP)
end function pressure_upper

pure real(wp) function mean_molecular_weight_upper(Z)
    real(wp),intent(in) :: Z
    integer :: i
    real(wp) :: M0,M1,M2,scale0,scale1,scale2
    i = interpolation_index(Z)
    call interpolation_scale_factors(i, Z, scale0, scale1, scale2)
    M0 = Mtableupper(i - 1)
    M1 = Mtableupper(i)
    M2 = Mtableupper(i + 1)
    mean_molecular_weight_upper = M0 * scale0 + M1 * scale1 + M2 * scale2
end function mean_molecular_weight_upper

pure real(wp) function speed_of_sound_86km()
    real(wp),parameter :: Z = 86000.0_wp ! (m)
    real(wp) :: H,M,T
    H = geopotential_altitude(Z)
    M = mean_molecular_weight_lower(Z)
    T = temperature_lower(H, M)
    speed_of_sound_86km = speed_of_sound_lower(T, M)
end function speed_of_sound_86km

function COESA_atmosphere(Z) result(s)
    real(wp),intent(in) :: Z ! altitude in meters
    type(state) :: s
    real(wp) :: H,M,T,P,c
    if (Z < -5000.0_wp) then
        error stop "altitude below lower bound of -5000 m"
    else if (Z > 1000000.0_wp) then
        error stop "altitude above upper bound of 1000000 m"
    else if (Z < 86000.0_wp) then
        H = geopotential_altitude(Z)
        M = mean_molecular_weight_lower(Z)
        T = temperature_lower(H, M)
        P = pressure_lower(H)
        c = speed_of_sound_lower(T, M)
    else
        T = temperature_upper(Z)
        P = pressure_upper(Z)
        M = mean_molecular_weight_upper(Z)
        c = speed_of_sound_86km()
    end if
    s = State(M, T, P, c)
end function COESA_atmosphere

function COESA_density(Z) result(density)
    !! a simple version that just returns density
    real(wp),intent(in) :: Z !! altitude in meters
    real(wp) :: density !! kg/m^3
    real(wp) :: H,M,T,P
    if (Z < -5000.0_wp) then
        error stop "altitude below lower bound of -5000 m"
    else if (Z > 1000000.0_wp) then
        error stop "altitude above upper bound of 1000000 m"
    else if (Z < 86000.0_wp) then
        H = geopotential_altitude(Z)
        M = mean_molecular_weight_lower(Z)
        T = temperature_lower(H, M)
        P = pressure_lower(H)
    else
        T = temperature_upper(Z)
        P = pressure_upper(Z)
        M = mean_molecular_weight_upper(Z)
    end if
    density = P * M / (Rstar * T)
end function COESA_density

!************************************************************************************
    end module COESA_module
!************************************************************************************