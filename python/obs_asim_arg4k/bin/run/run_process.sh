#!/bin/bash

### SET TOOL DIRECTORIES
export TOOLSDIR=$(realpath $(pwd)/../../modules/obs-tools)
for f in $TOOLSDIR/utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done

### RUN FROM TOOL
export PROC=$(job_get_proc_name "$0")
job_load_config "../config"
job_run_from_tool $TOOLSDIR $PROC

