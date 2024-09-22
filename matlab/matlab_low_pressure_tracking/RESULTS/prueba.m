load ./ecmf/GROUP_2/GROUP_2007032512_L7.mat
group_ecmf=group;

load ./egrr/GROUP_2/GROUP_2007032512_L7.mat
group_egrr=group;

load ./kwbc/GROUP_2/GROUP_2007032512_L7.mat
group_kwbc=group;

figure
hold on
plot(group_ecmf.minlonanalysis(:,21),group_ecmf.minlatanalysis(:,21),'bo-')
plot(group_egrr.minlonanalysis(:,15),group_egrr.minlatanalysis(:,15),'ro-')
plot(group_kwbc.minlonanalysis(:,10),group_kwbc.minlatanalysis(:,10),'go-')

figure
hold on
plot(group_ecmf.trajidanalysis(:,21),'bo-')
plot(group_egrr.trajidanalysis(:,15),'ro-')
plot(group_kwbc.trajidanalysis(:,10),'go-')