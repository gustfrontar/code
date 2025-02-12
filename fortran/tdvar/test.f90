       program test
       use common
       use common_speedy
       real(kind=8):: u(nlon, nlat, nlev)
       real(kind=8):: v(nlon, nlat, nlev)
       real(kind=8):: t(nlon, nlat, nlev)
       real(kind=8):: q(nlon, nlat, nlev)
       real(kind=8):: ps(nlon, nlat)

       open(2, file='/data/system4/jjliu/DAS_result/3dvar/response/gues/1982010106.grd',access='direct', &
                recl=nlon*nlat*4)
       call read_grd(2, u, v, t, q, ps)
       print *, 'zonal wind', u(1, 1, 1)
       end program test
