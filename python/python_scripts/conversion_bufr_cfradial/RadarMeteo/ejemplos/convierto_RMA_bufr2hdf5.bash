#!/bin/bash

for i in `ls -1 *.BUFR`;
do
~/tmp/bbufr/tests/bufr2hdf5 -d ~/tmp/bbufr/tables/ $i `echo $i | cut -f 1 -d '.'`.H5
done
