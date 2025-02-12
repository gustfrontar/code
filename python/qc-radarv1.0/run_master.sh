#!/bin/bash
source /home/desa/.bashrc
source /home/opt/intel/intelpython3/bin/activate qc-radar2
export PYART_QUIET=""
cd /home/desa/qc-radarV1.0
python master.py
