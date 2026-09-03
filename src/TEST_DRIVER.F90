! file: src/TEST_DRIVER.F90
program test_driver
  use PARS_MOD
  use VARS_MOD
  use HYBRID15_CLR
  implicit none

  integer, parameter :: n_params = 21
  integer, parameter :: n_days = 731
  
  real(8), dimension(n_params) :: test_params
  real(8), dimension(n_days)   :: nee_out, gpp_out, reco_out, le_out, sm_out
  integer :: d

  write(*,*) "=== Starting Standalone Fortran Wrapper Test ==="

  ! 1. Set dummy test parameters
  test_params = (/ &
      60.0_8,    & ! 1:  wfps_threshold
      25.0_8,    & ! 2:  T_ref
      30.0e-6_8, & ! 3:  Vcmax_top
      0.6_8,     & ! 4:  theta_sat
      2.0_8,     & ! 5:  q10
      750.0_8,   & ! 6:  dz(2)
      31.0_8,    & ! 7:  Topt_J
      800.0_8,   & ! 8:  moisture_dry_width
      4420.0_8,  & ! 9:  pool_initial(7,1)
      3825.0_8,  & ! 10: pool_initial(7,2)
      18.0_8,    & ! 11: omega_J
      0.01_8,    & ! 12: Kx
      -2.0_8,    & ! 13: lwp_crit
      2.55_8,    & ! 14: b_perc
      4.0_8,     & ! 15: LAI
      255.0_8,   & ! 16: pool_initial(3,1)
      255.0_8,   & ! 17: pool_initial(3,2)
      0.12_8,    & ! 18: asw
      5.0_8,     & ! 19: perc_max
      250.0_8,   & ! 20: dz(1)
      0.65_8     & ! 21: KPAR
  /)
  ! 2. Call the wrapper subroutine directly
  call run_model( &
      test_params, n_params, &
      nee_out, gpp_out, reco_out, le_out, sm_out, n_days)

  ! 3. Print sample outputs to terminal to verify execution
  write(*,*) "Simulation completed successfully!"
  write(*,*) "Day 1 GPP  :", gpp_out(1)
  write(*,*) "Day 1 RECO :", reco_out(1)
  write(*,*) "Day 1 NEE  :", nee_out(1)
  write(*,*) "Day 1 LE   :", le_out(1)
  write(*,*) "Day 1 SM   :", sm_out(1)
  
  print *, "=== ARRAY INTEGRITY CHECK ==="
  print *, "GPP  - Min:", minval(gpp_out),  " Max:", maxval(gpp_out),  " Mean:", sum(gpp_out)/n_days
  print *, "NEE  - Min:", minval(nee_out),  " Max:", maxval(nee_out),  " Mean:", sum(nee_out)/n_days
  print *, "RECO - Min:", minval(reco_out), " Max:", maxval(reco_out), " Mean:", sum(reco_out)/n_days
  print *, "LE   - Min:", minval(le_out),   " Max:", maxval(le_out),   " Mean:", sum(le_out)/n_days
  print *, "SM   - Min:", minval(sm_out),   " Max:", maxval(sm_out),   " Mean:", sum(sm_out)/n_days

  write(*,*) "Mean Annual GPP :", sum(gpp_out) / real(n_days, 8)

end program test_driver
