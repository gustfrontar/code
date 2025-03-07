#!/bin/bash

get_timeinc_from_wait() {

   last_date=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC $DATEFILE_fmt)
   wait_date=$(date -u -d "- $WAIT_TIME seconds" +"$DATEFILE_fmt")
   delta_dates=$(get_date_diff $wait_date $last_date)
   steps=$(($delta_dates / $WLENGTH))
   mod=$(($delta_dates % $WLENGTH))
   thr_time=$(echo "($WLENGTH * $BUFFER_TIME)/1" | bc)
   [[ $mod -gt $thr_time ]] && steps=$(($steps + 1))
   TIMEINC=$(($WLENGTH * $steps))

   echo $TIMEINC
}
