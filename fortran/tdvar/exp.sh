#!/bin/sh

#Full experiment Ps T U V Q
cp ex_obs_expfull.f90 ex_obs.f90 
./tdvar.sh EXP_FULL

./rmse_energy.sh EXP_FULL
./rmse_zm.sh EXP_FULL

#NO Q experiment Ps T U V
cp ex_obs_expnoq.f90 ex_obs.f90 
./tdvar.sh EXP_NOQ

./rmse_energy.sh EXP_NOQ
./rmse_zm.sh EXP_NOQ

#NO UV experiment Ps T
cp ex_obs_expnouv.f90 ex_obs.f90 
./tdvar.sh EXP_NOUV

./rmse_energy.sh EXP_NOUV
./rmse_zm.sh EXP_NOUV

#NO T experiment Ps U V
cp ex_obs_expnot.f90 ex_obs.f90 
./tdvar.sh EXP_NOT

./rmse_energy.sh EXP_NOT
./rmse_zm.sh EXP_NOT

