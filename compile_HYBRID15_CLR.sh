#!/bin/bash
set -euo pipefail

# ============================================================
# Compile and run HYBRID15_CLR
# ============================================================

# Choose compiler.
# Default is gfortran.
# it will also run:
#   ./compile_eight_pool.sh ifort
#   ./compile_eight_pool.sh ifx
#   ./compile_eight_pool.sh ftn
# Or set FC before running:
#   FC=ifx ./compile_eight_pool.sh
# FCOMP="${1:-${FC:-gfortran}}"
FCOMP="${1:-${FC:-mpif90}}"

echo "Using Fortran compiler: ${FCOMP}"
echo "'" > home_dir.txt
echo $HOME >> home_dir.txt
echo "'" >> home_dir.txt

# Create required directories.
mkdir -p build results slurm_logs

# Clean, compile, and run the model using the default eightpool.nml file.
make clean FC="${FCOMP}"
#make all FC="${FCOMP}"
#make run FC="${FCOMP}"
make test FC="${FCOMP}"

