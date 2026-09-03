!======================================================================!
subroutine INIT(params_in, n_params, n_days)
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
integer, intent(in) :: n_params, n_days
real(8), intent(in), dimension(n_params) :: params_in
!----------------------------------------------------------------------!
if (.not. allocated(T_soil)) allocate(T_soil(nlayers))

open (10, file = 'home_dir.txt', status = 'old')
read (10,*) home_dir
close (10)
!----------------------------------------------------------------------!
open (11, file = 'driver.txt', status = 'old')
read (11,*) syr
read (11,*) eyr
read (11,*) nyr_co2
read (11,*) dz (1)
read (11,*) dz (2)
read (11,*) theta (1)
read (11,*) theta (2)
read (11,*) snowpack
read (11,*) Wcan
read (11,*) LAI
read (11,*) height
read (11,*) biomass
read (11,*) froot_top
do ip = 1, n_pools
  read (11,*) pool_initial (ip,1)
end do
read (11,*) fiSOM
close (11)

wfps_threshold = params_in(1)
T_ref          = params_in(2)
Vcmax_top      = params_in(3)
theta_sat      = params_in(4)
q10            = params_in(5)
dz(2)          = params_in(6)
Topt_J         = params_in(7)
moisture_dry_width = params_in(8)
pool_initial(7,1)  = params_in(9)
pool_initial(7,2)  = params_in(10)
omega_J            = params_in(11)
Kx                 = params_in(12)
lwp_crit           = params_in(13)
b_perc             = params_in(14)
LAI                = params_in(15)
pool_initial(3,1)  = params_in(16)
pool_initial(3,2)  = params_in(17)
asw                = params_in(18)
perc_max           = params_in(19)
dz(1)              = params_in(20)
KPAR               = params_in(21)

Jmax_top = 2.1 * Vcmax_top

!----------------------------------------------------------------------!
! Assume SM_MAX is saturated water content (porosity) and all soil is
! peat. Using Eqn. 7.90 of oleson and theta_sat_om = 0.9. But obs
! suggest 0.7, so calibrate down to that.
!----------------------------------------------------------------------!
SM_MAX (:) = theta_sat * dz (:)
SM_MIN (:) = SM_MAX / saturation_to_minimum
!----------------------------------------------------------------------!
sm (1) = theta (1) * dz (1)
sm (2) = SM_MAX (2) 
depth_layer1 = dz (1) / 1000 ! Thickness of layer 1 (m)
depth_layer2 = dz (2) / 1000 ! Thickness of layer 2 (m)
n_layer1_nodes = 1 + nint(depth_layer1 / dz_soil)
n_layer2_nodes = 1 + nint((depth_layer1 + depth_layer2) / dz_soil)

T_profile (:) = mean_air_temp
T_soil_daily_mean (:) = mean_air_temp
T_soil (:) = mean_air_temp
!----------------------------------------------------------------------!
! Split SOM pools over layers in proportion to thicknesse.
!----------------------------------------------------------------------!
do kl = 1, nlayers
  pool_initial (:,kl) = fiSOM * pool_initial (:,1) * dz (kl) / &
                        (dz (1) + dz (2))
end do

!----------------------------------------------------------------------!
! Total number of years in simuation (yr)
!----------------------------------------------------------------------!
nyr_sim = eyr - syr + 1
!----------------------------------------------------------------------!
if (.not. allocated(co2_ppm)) allocate(co2_ppm(nyr_co2))
   if (.not. allocated(tmp))     allocate(tmp(nyr_sim, ntimes))
   if (.not. allocated(pre))     allocate(pre(nyr_sim, ntimes))
   if (.not. allocated(tswrf))   allocate(tswrf(nyr_sim, ntimes))
   if (.not. allocated(dlwrf))   allocate(dlwrf(nyr_sim, ntimes))
   if (.not. allocated(spfh))    allocate(spfh(nyr_sim, ntimes))
   if (.not. allocated(pres))    allocate(pres(nyr_sim, ntimes))
   if (.not. allocated(ugrd))    allocate(ugrd(nyr_sim, ntimes))
   if (.not. allocated(vgrd))    allocate(vgrd(nyr_sim, ntimes))
!----------------------------------------------------------------------!
! Read all CO2 and climate forcings.
!----------------------------------------------------------------------!
call READ_HYBRID15_CLR_FORCING
!----------------------------------------------------------------------!
do kl = 1, nlayers
c_state (:,kl) = pool_initial (:,kl)
end do
!----------------------------------------------------------------------!
end subroutine INIT
!======================================================================!
