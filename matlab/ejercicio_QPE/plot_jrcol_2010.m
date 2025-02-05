function [colores] = plot_jrcol_2009(v,vcol)
%**************************************************************************
%       ESTA FUNCION CONSTRUYE UN MAPA DE COLORES PARA APLICAR A UNA FIGURA
%**************************************************************************
% USO: PLOT_JRCOL(V,VCOL) DONDE V ES UN VECTOR QUE CONTIENE LOS LIMITES DE
% LOS INTERVALOS QUE QUEREMOS PLOTEAR. Y VCOL SON LOS NUMEROS DE LOS
% COLORES QUE QUEREMOS ASIGNAR A CADA INTERVALO. LOS CODIGOS DE COLORES
% UTILIZADOS EN VCOL SON LOS MISMOS QUE LOS DEFINIDOS EN EL SCRIPT DE GRADS
% JAECOL.
% COLORES=PLOT_JRCOL(V,VCOL) DEVUELVE EL MAPA DE COLORES QUE ESTA DE
% ACUERDO CON LOS INTERVALOS Y LOS CODIGOS DE COLORES SELECCIONADOS.
% LA CANTIDAD DE ELEMENTOS DE COLORES, DEBE SER UNA MENOS QUE LA DE V, Y
% LOS COLORES SE ASIGNAN DE LA SIGUIENTE MANERA: SI V ES (0 0.1 0.2 ...0.9
% 1) EL PRIMER CODIGO DE COLOR EN VCOL SE ASIGNA AL INTERVALO 0 0.1 Y ASI
% CONSECUTIVAMENTE HASTA EL INTERVALO 0.9 1). SI V NO ESTA ORDENADO DE
% MENOR A MAYOR EL SCRIPT LO ORDENA PERO EL ORDEN DE LOS COLORES SE VERA
% ALTERADO.
% LOS LIMITES DE LA ESCALA SE FIJAN ENTRE EL MINIMO DE V Y EL MAXIMO DE V.

jrcol(1,:)= [ 0    0    0];
jrcol(2,:)= [ 255 255 255];
jrcol(21,:)=[ 255 250 170];
jrcol(22,:)=[ 255 232 120];
jrcol(23,:)=[ 255 192  60];
jrcol(24,:)=[ 255 160   0];
jrcol(25,:)=[ 255  96   0];
jrcol(26,:)=[ 255  50   0];
jrcol(27,:)=[ 225  20   0];
jrcol(28,:)=[ 192   0   0];
jrcol(29,:)=[ 165   0   0];
jrcol(31,:)=[ 230 255 225];
jrcol(32,:)=[ 200 255 190];
jrcol(33,:)=[ 180 250 170];
jrcol(34,:)=[ 150 245 140];
jrcol(35,:)=[ 120 245 115];
jrcol(36,:)=[  80 240  80];
jrcol(37,:)=[  55 210  60];
jrcol(38,:)=[  30 180  30];
jrcol(39,:)=[  15 160  15];
jrcol(41,:)=[ 200 255 255];
jrcol(42,:)=[ 175 240 255];
jrcol(43,:)=[ 130 210 255];
jrcol(44,:)=[  95 190 250];
jrcol(45,:)=[  75 180 240];
jrcol(46,:)=[  60 170 230];
jrcol(47,:)=[  40 150 210];
jrcol(48,:)=[  30 140 200];
jrcol(49,:)=[  20 130 190];
jrcol(51,:)=[ 220 220 255];
jrcol(52,:)=[ 192 180 255];
jrcol(53,:)=[ 160 140 255];
jrcol(54,:)=[ 128 112 235];
jrcol(55,:)=[ 112  96 220];
jrcol(56,:)=[  72  60 200];
jrcol(57,:)=[  60  40 180];
jrcol(58,:)=[  45  30 165];
jrcol(59,:)=[  40   0 160];
jrcol(61,:)=[ 255 230 230];
jrcol(62,:)=[ 255 200 200];
jrcol(63,:)=[ 248 160 160];
jrcol(64,:)=[ 230 140 140];
jrcol(65,:)=[ 230 112 112];
jrcol(66,:)=[ 230  80  80];
jrcol(67,:)=[ 200  60  60];
jrcol(68,:)=[ 180  40  40];
jrcol(69,:)=[ 164  32  32];
jrcol(71,:)=[ 250 250 250];
jrcol(72,:)=[ 225 225 225];
jrcol(73,:)=[ 200 200 200];
jrcol(74,:)=[ 180 180 180];
jrcol(75,:)=[ 160 160 160];
jrcol(76,:)=[ 150 150 150];
jrcol(77,:)=[ 140 140 140];
jrcol(78,:)=[ 124 124 124];
jrcol(79,:)=[ 112 112 112];
jrcol(80,:)=[  92  92  92];
jrcol(81,:)=[  80  80  80];
jrcol(82,:)=[  70  70  70];
jrcol(83,:)=[  60  60  60];
jrcol(84,:)=[  50  50  50];
jrcol(85,:)=[  40  40  40];
jrcol(86,:)=[  36  36  36];
jrcol(87,:)=[  32  32  32];


%La siguiente es la escala de colores que aparece en los mapas de pp del
%modelo MRF-GFS

jrcol(88,:)=[  0  236  236];
jrcol(89,:)=[  1  160  246];
jrcol(90,:)=[  0    0  255];
jrcol(91,:)=[  0  255    0];
jrcol(92,:)=[  0  200   0];
jrcol(93,:)=[  0  144    0];
jrcol(94,:)=[255  255    0];
jrcol(95,:)=[255  144    0];
jrcol(96,:)=[255    0    0];
jrcol(97,:)=[192    0    0];
jrcol(98,:)=[255    0  255];
jrcol(99,:)=[153   85  201];


% Channel 4 IR Cloud Top Colour

jrcol(100,:)=[  0    0  255]; % -39C a -45C
jrcol(101,:)=[179  138   70]; % -45C a -51C
jrcol(102,:)=[  0  255    0]; % -51C a -57C
jrcol(103,:)=[255  255    0]; % -57C a -63C
jrcol(104,:)=[255    0    0]; % -63C a -69C
jrcol(105,:)=[255    0  255]; % -69C a -75C
jrcol(106,:)=[102  102  102]; % < -75C


% Escala radar SMN

jrcol(107,:)=[  0  240  240]; %  2 a  6 dBZ
jrcol(108,:)=[  0  180  230]; %  6 a 10 dBZ
jrcol(109,:)=[  0  120  220]; % 10 a 14 dBZ
jrcol(110,:)=[  0    0  210]; % 14 a 18 dBZ
jrcol(111,:)=[  0  240    0]; % 18 a 22 dBZ
jrcol(112,:)=[  0  180    0]; % 22 a 26 dBZ
jrcol(113,:)=[  0  120    0]; % 26 a 30 dBZ
jrcol(114,:)=[240  240    0]; % 30 a 34 dBZ
jrcol(115,:)=[200  200    0]; % 34 a 38 dBZ
jrcol(116,:)=[200  140    0]; % 38 a 42 dBZ
jrcol(117,:)=[240   40    0]; % 42 a 46 dBZ
jrcol(118,:)=[160    0    0]; % 46 a 50 dBZ
jrcol(119,:)=[240    0  240]; % 50 a 54 dBZ
jrcol(120,:)=[160    0  160]; % 54 a 58 dBZ
jrcol(121,:)=[252  252  252]; % 58 a 62 dBZ


% Escala radares meteorologia Cuba

jrcol(122,:)=[  0  255  255]; %  10 dBZ
jrcol(123,:)=[  0  204  255]; %  15 dBZ
jrcol(124,:)=[  0  152  255]; %  20 dBZ
jrcol(125,:)=[  0  255   66]; %  25 dBZ
jrcol(126,:)=[  0  204   51]; %  30 dBZ
jrcol(127,:)=[ 51  153  102]; %  35 dBZ
jrcol(128,:)=[255  255    0]; %  40 dBZ
jrcol(129,:)=[255  204    0]; %  45 dBZ
jrcol(130,:)=[255  153    0]; %  50 dBZ
jrcol(131,:)=[255    0    0]; %  55 dBZ
jrcol(132,:)=[204    0    0]; %  60 dBZ
jrcol(133,:)=[153    0    0]; %  65 dBZ
jrcol(134,:)=[204    0  255]; %  70 dBZ
jrcol(135,:)=[153    0  255]; %  75 dBZ
jrcol(136,:)=[127   31  170]; %  80 dBZ


% Escala radar INTA Pergamino

jrcol(137,:)=[149  234  255]; %  2 a  6 dBZ
jrcol(138,:)=[106  191  255]; %  6 a 10 dBZ
jrcol(139,:)=[ 64  149  255]; % 10 a 14 dBZ
jrcol(140,:)=[ 21  106  255]; % 14 a 18 dBZ
jrcol(141,:)=[ 84  252    0]; % 18 a 22 dBZ
jrcol(142,:)=[ 82  192    0]; % 22 a 26 dBZ
jrcol(143,:)=[ 79  132    0]; % 26 a 30 dBZ
jrcol(144,:)=[252  252    0]; % 30 a 34 dBZ
jrcol(145,:)=[212  212    0]; % 34 a 38 dBZ
jrcol(146,:)=[171  171    0]; % 38 a 42 dBZ
jrcol(147,:)=[255    0    0]; % 42 a 46 dBZ
jrcol(148,:)=[170    0    0]; % 46 a 50 dBZ
jrcol(149,:)=[255    0  255]; % 50 a 54 dBZ
jrcol(150,:)=[171    0  171]; % 54 a 58 dBZ
jrcol(151,:)=[255  255  255]; % 58 a 62 dBZ

% Escala radar NWS USA

jrcol(152,:)=[  4  233  231]; %  5 a 10 dBZ
jrcol(153,:)=[  1  159  244]; % 10 a 15 dBZ
jrcol(154,:)=[  3    0  244]; % 15 a 20 dBZ
jrcol(155,:)=[  2  253    2]; % 20 a 25 dBZ
jrcol(156,:)=[  1  197    1]; % 25 a 30 dBZ
jrcol(157,:)=[  0  142    0]; % 30 a 35 dBZ
jrcol(158,:)=[253  248    2]; % 35 a 40 dBZ
jrcol(159,:)=[229  188    0]; % 40 a 45 dBZ
jrcol(160,:)=[253  149    0]; % 45 a 50 dBZ
jrcol(161,:)=[253    0    0]; % 50 a 55 dBZ
jrcol(162,:)=[212    0    0]; % 55 a 60 dBZ
jrcol(163,:)=[188    0    0]; % 60 a 65 dBZ
jrcol(164,:)=[248    0  253]; % 65 a 70 dBZ
jrcol(165,:)=[152   84  198]; % 70 a 75 dBZ
jrcol(166,:)=[252  252  252]; % > 75 dBZ

% Escala radar INTA (Romina)

jrcol(167,:)=[ 78 235 235]; %  5 a 10 dBZ
jrcol(168,:)=[ 30 166 239]; % 10 a 15 dBZ
jrcol(169,:)=[  7  76 225]; % 15 a 20 dBZ
jrcol(170,:)=[107 223  18]; % 20 a 25 dBZ
jrcol(171,:)=[ 87 172  59]; % 25 a 30 dBZ
jrcol(172,:)=[ 29 139   2]; % 30 a 35 dBZ
jrcol(173,:)=[236 236   0]; % 35 a 40 dBZ
jrcol(174,:)=[244 163   0]; % 40 a 45 dBZ
jrcol(175,:)=[244 110  14]; % 45 a 50 dBZ
jrcol(176,:)=[226   0   0]; % 50 a 55 dBZ
jrcol(177,:)=[166  16  29]; % 55 a 60 dBZ
jrcol(178,:)=[ 98   1  14]; % 60 a 65 dBZ
jrcol(179,:)=[255 255 255]; % >65 dBZ (Blanco)

% Escala radares INTA

jrcol(180,:)=[  0 255 255]; % 15.0 a 18.3 dBZ
jrcol(181,:)=[  0 153 255]; % 18.3 a 22.1 dBZ
jrcol(182,:)=[  0   0 255]; % 22.1 a 25.7 dBZ
jrcol(183,:)=[  0 255   0]; % 25.7 a 29.3 dBZ
jrcol(184,:)=[  0 204   0]; % 29.3 a 32.9 dBZ
jrcol(185,:)=[  0 153   0]; % 32.9 a 36.4 dBZ
jrcol(186,:)=[255 255   0]; % 36.4 a 40.0 dBZ
jrcol(187,:)=[255 204   0]; % 40.0 a 43.6 dBZ
jrcol(188,:)=[255 102   0]; % 43.6 a 47.1 dBZ
jrcol(189,:)=[255   0   0]; % 47.1 a 50.7 dBZ
jrcol(190,:)=[204   0   0]; % 50.7 a 54.3 dBZ
jrcol(191,:)=[153   0   0]; % 54.3 a 57.9 dBZ
jrcol(192,:)=[255   0 255]; % 57.9 a 61.4 dBZ
jrcol(193,:)=[153   0 255]; % 61.4 a 65.0 dBZ
jrcol(194,:)=[255 255 255]; % >65 dBZ (Blanco)
jrcol(195,:)=[183 183 183]; %Color gris para la zona con ecos menos a 15 dBZ

% Escala mapa precip verif cyn (SMN)

jrcol(196,:)=[255 250 170]; %   0 a   1 mm
jrcol(197,:)=[255 192  60]; %   1 a   2 mm
jrcol(198,:)=[248 160 160]; %   2 a   5 mm
jrcol(199,:)=[225  20   0]; %   5 a  10 mm
jrcol(200,:)=[200 255 190]; %  10 a  20 mm
jrcol(201,:)=[120 245 115]; %  20 a  30 mm
jrcol(202,:)=[ 55 210  60]; %  30 a  50 mm
jrcol(203,:)=[200 255 255]; %  50 a  70 mm
jrcol(204,:)=[130 210 255]; %  70 a  80 mm
jrcol(205,:)=[ 60 170 230]; %  80 a 100 mm
jrcol(206,:)=[192 180 255]; % 100 a 120 mm
jrcol(207,:)=[128 112 235]; % 120 a 150 mm
jrcol(208,:)=[ 72  60 200]; % >150 mm

% Otra escala de colores para mapa de precip 24hs

jrcol(209,:)=[  4 236 236]; % 0.25 a 2.5 mm
jrcol(210,:)=[  4 136 244]; % 2.5 a 6.4 mm
jrcol(211,:)=[  2   2 248]; % 6.4 a  13 mm
jrcol(212,:)=[  4 252   5]; %  13 a  19 mm
jrcol(213,:)=[  3 204   4]; %  19 a  25 mm
jrcol(214,:)=[  5 143   4]; %  25 a  38 mm
jrcol(215,:)=[252 252   4]; %  38 a  51 mm
jrcol(216,:)=[229 196   3]; %  51 a  64 mm
jrcol(217,:)=[253 148   4]; %  64 a  76 mm
jrcol(218,:)=[252   4   4]; %  76 a 102 mm
jrcol(219,:)=[220   4   5]; % 102 a 127 mm
jrcol(220,:)=[196   4   3]; % 127 a 152 mm
jrcol(221,:)=[250   4 251]; % 152 a 203 mm
jrcol(222,:)=[155  84 204]; % 203 a 254 mm
jrcol(223,:)=[252 252 252]; % >254 mm

% Escala de PrecipAccum (Caro-Hidro)

jrcol(224,:)=[233 251 252]; %  5 a  10 mm
jrcol(225,:)=[211 246 249]; % 10 a  25 mm
jrcol(226,:)=[126 209 228]; % 25 a  50 mm
jrcol(227,:)=[ 98 163 200]; % 50 a  75 mm
jrcol(228,:)=[ 70 116 172]; % 75 a 100 mm
jrcol(229,:)=[ 28  47 130]; % >100 mm

% Escala de PrecipAccum 24H (JSMeteoView)

jrcol(230,:)=[  0   0   0]; %  0.0 a   0.1 mm
jrcol(231,:)=[ 56   0 112]; %  0.1 a   0.3 mm
jrcol(232,:)=[ 48   0 168]; %  0.3 a   0.6 mm
jrcol(233,:)=[  0   0 252]; %  0.6 a   1.0 mm
jrcol(234,:)=[  0 108 192]; %  1.0 a   2.0 mm
jrcol(235,:)=[  0 160   0]; %  2.0 a   4.0 mm
jrcol(236,:)=[  0 188   0]; %  4.0 a   6.0 mm
jrcol(237,:)=[ 52 216   0]; %  6.0 a  10.0 mm
jrcol(238,:)=[156 220   0]; % 10.0 a  15.0 mm
jrcol(239,:)=[224 220   0]; % 15.0 a  20.0 mm
jrcol(240,:)=[252 176   0]; % 20.0 a  30.0 mm
jrcol(241,:)=[252 132   0]; % 30.0 a  40.0 mm
jrcol(242,:)=[252  88   0]; % 40.0 a  60.0 mm
jrcol(243,:)=[252   0   0]; % 60.0 a  80.0 mm
jrcol(244,:)=[160   0   0]; % 80.0 a 100.0 mm
jrcol(245,:)=[252 252 252]; % > 100.0 mm


jrcol=jrcol/255;

colores_size=1500;
v=sort(v);
colores=zeros(colores_size,3);
ncolores=length(v)-1;  %Esto debería conicidir con length de colores.

minv=min(v);
maxv=max(v);

indices=round((colores_size-1)*(v(:)-minv)/(maxv-minv)+1);

for icol=1:ncolores
    for ii=indices(icol):indices(icol+1)
    colores(ii,:)=jrcol(vcol(icol),:);
    end
end

colormap(colores)
caxis([minv maxv]);
%colorbar('SouthOutside');
colorbar('YTick',v);

