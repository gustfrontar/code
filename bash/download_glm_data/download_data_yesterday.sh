

cd /home/jruiz/share/test_lucho/

fecha_ayer=$(date --date="yesterday" "+%Y%m%d")

wget -e robots=off -r -np --page-requisites --convert-links 'https://atm.ucar.edu/native/satellite/GOES/GRB16/Products/GeostationaryLightningMapper/FullDisk/'${fecha_ayer}'/'


