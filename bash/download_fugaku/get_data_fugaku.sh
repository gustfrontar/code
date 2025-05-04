SOURCEDIR=$1
DESTDIR=$2

nohup rsync -avzh a04037@login.fugaku.r-ccs.riken.jp:${SOURCEDIR} ${DESTDIR} &



