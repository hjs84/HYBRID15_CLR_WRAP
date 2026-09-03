! file: src/HYBRID15_CLR.F90
module HYBRID15_CLR
  use PARS_MOD
  use VARS_MOD
  implicit none

contains

  subroutine run_model (params_in, n_params, nee_out, gpp_out,&
      reco_out, le_out, sm_out, n_days)
  
    implicit none

    integer, intent(in) :: n_params, n_days
    real(8), intent(in), dimension(n_params) :: params_in

    real(8), intent(out), dimension(n_days) :: nee_out
    real(8), intent(out), dimension(n_days) :: gpp_out
    real(8), intent(out), dimension(n_days) :: reco_out
    real(8), intent(out), dimension(n_days) :: le_out
    real(8), intent(out), dimension(n_days) :: sm_out

    !f2py intent(in) :: params_in
    !f2py intent(out) :: nee_out, gpp_out, reco_out, le_out, sm_out
    !f2py integer intent(hide), depend(params_in) :: n_params = len(params_in)
    !f2py integer intent(hide), depend(nee_out) :: n_days = len(nee_out)

    !!!Map Python parameters here!!!
    !wfps_threshold = params_in(1)
    !T_ref          = params_in(2)
    !Vcmax_top      = params_in(3)
    !theta_sat      = params_in(4)
    !q10            = params_in(5)
    !dz(2)          = params_in(6)
    !Topt_J         = params_in(7)
    !moisture_dry_width = params_in(8)
    !pool_initial(7,1)  = params_in(9)
    !pool_initial(7,2)  = params_in(10)
    !omega_J            = params_in(11)
    !Kx                 = params_in(12)
    !lwp_crit           = params_in(13)
    !b_perc             = params_in(14)
    !LAI                = params_in(15)
    !pool_initial(3,1)  = params_in(16)
    !pool_initial(3,2)  = params_in(17)
    !asw                = params_in(18)
    !perc_max           = params_in(19)
    !dz(1)              = params_in(20)
    !KPAR               = params_in(21)

    integer :: day_idx
    day_idx = 0

    call INIT(params_in, n_params, n_days)

    kyr_ce = syr
    do ikyr = 1, nyr_sim
      C0 = CDM * biomass + sum (c_state)
      W0 = sm (1) + sm (2) + Wcan
      ca_fmol = co2_ppm (kyr_ce) / 1.0e6 ! mol[CO2] mol[air]-1
      Raut_ann = zero ! Annual autotrophic respiration       (g[C] m-2 yr-1)
      GPP_ann  = zero ! Annual gross primary production      (g[C] m-2 yr-1)
      Rhet_ann = zero ! Annual heterotrophic respiration     (g[C] m-2 yr-1)
      L_ann    = zero ! Annual litter flux (g[C] m-2 yr-1)
      PPT_ann  = zero ! Annual precipitation                         (mm/yr)
      RO_ann   = zero
      ET_ann   = zero
      NEE_ann  = zero
      it = 0
    
      do kday = 1, ndays
        hr = 0.0 - dt_hr
        TC_day   = zero
        LE_day   = zero
        sm_day   = zero
        PPT_day  = zero
        GPP_day  = zero
        Raut_day = zero
        total_litter_day = zero
        leaching_water_day = zero
        G_day = zero
        T_soil_daily_sum = zero
    
        do kt = 1, nt
          it = it + 1
          hr = hr + dt_hr
          ! Set local climate variables for this timepoint.

          tmp_l   = tmp   (ikyr,it) ! Air temperature                    (K)
          TC      = tmp_l - tf      ! Air temperature                   (oC)
          pre_l   = pre   (ikyr,it) ! Precipitation                   (mm/s)
          tswrf_l = tswrf (ikyr,it) ! Tot dnwd SW flx, sfc, time mean (W/m2)
          dlwrf_l = dlwrf (ikyr,it) ! Dnwd LW rad flx                 (W/m2)
          spfh_l  = spfh  (ikyr,it) ! Specific humidity              (kg/kg)
          pres_l  = pres  (ikyr,it) ! Pressure                          (Pa)
          ugrd_l  = ugrd  (ikyr,it) ! Zonal component of wnd speed     (m/s)
          vgrd_l  = vgrd  (ikyr,it) ! Merdional component of wnd speed (m/s)
          TC_day = TC_day + TC

          call CROWN
          call HYDRO
          call SOILTEMP
          call GROW

          total_litter_day (1) = total_litter_day (1) + dt_s * litter
          leaching_water_day = leaching_water_day + leaching_water_cm

          do kl = 1, nlayers
            sm_day (kl) = sm_day (kl) + sm (kl)
            T_soil_daily_sum (kl) = T_soil_daily_sum (kl) + T_soil (kl)
          end do

          LE_day   = LE_day   + LE
          PPT_day  = PPT_day  + dt_s * pre_l
          GPP_day  = GPP_day  + dt_s * gpp
          Raut_day = Raut_day + dt_s * Raut

          PPT_ann  = PPT_ann  + dt_s * pre_l
          RO_ann   = RO_ann   + dt_s * sm_q
          ET_ann   = ET_ann   + dt_s * aet
          GPP_ann  = GPP_ann  + dt_s * gpp
          Raut_ann = Raut_ann + dt_s * Raut
          NEE_ann  = NEE_ann  + dt_s * (Raut - gpp)
        end do ! kt
  
        total_input = CDM * total_litter_day

        TC_day = TC_day / float (nt)
        T_soil_daily_mean (1) = T_soil_daily_sum (1) / float (nt)
        T_soil_daily_mean (2) = T_soil_daily_sum (2) / float (nt)
        LE_day = LE_day / float (nt)
        sm_day (:) = sm_day (:) / float (nt)
        leaching_water_day = leaching_cm_day ! I think!
      
        call DECOMP
      
        L_ann    = L_ann    + sum (total_litter_day (:))
        Rhet_ann = Rhet_ann + day_s * Rhet
        NEE_ann  = NEE_ann  + day_s * Rhet

        Rhet_day = day_s * Rhet
        NEE_day = Raut_day + Rhet_day - GPP_day
     
        day_idx = day_idx + 1
        nee_out (day_idx)  = real (NEE_day, 8)
        gpp_out (day_idx)  = real (GPP_day, 8)
        reco_out (day_idx) = real (Raut_day + Rhet_day, 8)
        le_out (day_idx)   = real (LE_day, 8)
        sm_out (day_idx)   = real (sm_day (1), 8)

      end do ! kday

      C1 = CDM * biomass + sum (c_state) ! final C
      Cbal = C1 & ! final C
                           - C0 & ! initial C
                           - (GPP_ann - Raut_ann - Rhet_ann) ! gains - losses
      W1 = sm (1) + sm (2) + Wcan
      Wbal = (W1-W0)-(PPT_ann-RO_ann-ET_ann)
      kyr_ce = kyr_ce + 1

    end do ! kyr

  end subroutine run_model

end module HYBRID15_CLR
