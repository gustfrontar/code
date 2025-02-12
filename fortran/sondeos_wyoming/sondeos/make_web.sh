#!/bin/bash
##############################################################################################
# Este script genera la página web para mostrar los diferentes gráficos asociados a un perfil
# Juan Ruiz - 2006
##############################################################################################

#Parametros 
##############################################################################################
htmpath=$1             #Path donde se generan los htm :/WRFV2/wrfsi/domains/operativo/www
anio=$2                #Año 4digitos
mes=$3                 #Mes en minuscula
dia=$4                 #Dia dos digitos
hora=$5                #Hora dos digitos
estacion=$6            #Nombre de la estacion comenzando en mayuscula
nestacion=$7           #Numero de la estacion 
##############################################################################################

web=${htmpath}${nestacion}.htm

echo "<html>"> $web
echo "<head>">> $web
echo "  <meta http-equiv='content-type'">> $web
echo " content='text/html; charset=ISO-8859-1'> ">> $web
echo "  <title>Sondeos CIMA</title>">> $web
echo "</head>">> $web
echo "<body>">> $web
echo "<script language="JavaScript">">> $web
echo "var win = null;">> $web
echo "function newWindow(mypage,myname,w,h,features) {">> $web
echo "  var winl = (screen.width-w)/2;">> $web
echo "  var wint = (screen.height-h)/2;">> $web
echo "  if (winl < 0) winl = 0;">> $web
echo "  if (wint < 0) wint = 0;">> $web
echo "  var settings = 'height=' + h + ',';">> $web
echo "  settings += 'width=' + w + ',';">> $web
echo "  settings += 'top=' + wint + ',';">> $web
echo "  settings += 'left=' + winl + ',';">> $web
echo "  settings += features;">> $web
echo "  win = window.open(mypage,myname,settings);">> $web
echo "  win.window.focus();">> $web
echo "}">> $web
echo "</script>">> $web
echo "<table cellpadding='2' cellspacing='2' border='1'">> $web
echo " style='text-align: left; width: 100px; margin-left: auto; margin-right: auto;'>">> $web
echo "  <tbody>">> $web
echo "    <tr align='center'>">> $web
echo "      <td">> $web
echo " style='background-color: rgb(0, 0, 153); text-align: center; vertical-align: middle;'><big><span">> $web
echo " style='color: rgb(255, 255, 255); font-weight: bold;'>An&aacute;lisis">> $web
echo "termodin&aacute;mico del sondeo realizado en la estaci&oacute;n ${estacion}">> $web
echo "a las ${hora} UTC del dia ${dia} de ${mes} de ${anio}</span></big><br>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig7.png' title='' ">> $web
echo " alt='Figura 1' ><br>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br>">> $web
echo "      <div">> $web
echo " style='text-align: center; font-weight: bold; color: rgb(255, 255, 255);'><big>">> $web
echo "      <a href='#null' ">> $web
echo " onclick=\"newWindow('desc1_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca">> $web
echo "de este grafico</a></big></div>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig8.png' title='' ">> $web
echo " alt='Figura 1' ><br>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br>">> $web
echo "      <div">> $web
echo " style='text-align: center; font-weight: bold; color: rgb(255, 255, 255);'><big>">> $web
echo "      <a href='#null' ">> $web
echo " onclick=\"newWindow('deschodo_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca">> $web
echo "de este grafico</a></big></div>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig2.png' title='' ">> $web
echo " alt='Figura 3' ><br> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td ">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br> ">> $web
echo "      <div style='text-align: center; font-weight: bold;'><big><a ">> $web
echo " href='#null' ">> $web
echo " onclick=\"newWindow('desc3_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca ">> $web
echo "de este grafico</a></big></div> ">> $web
echo "      </td> ">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig3.png' title='' ">> $web
echo " alt='Figura 4' ><br>">> $web
echo "      </td>">> $web
echo "    </tr>">> $web
echo "    <tr>">> $web
echo "      <td ">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br> ">> $web
echo "      <div style='text-align: center; font-weight: bold;'><big><a ">> $web
echo " href='#null' ">> $web
echo " onclick=\"newWindow('desc4_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca ">> $web
echo "de este grafico</a></big></div> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig4.png' title='' ">> $web
echo " alt='Figura 5' ><br> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td ">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br> ">> $web
echo "      <div style='text-align: center; font-weight: bold;'><big><a ">> $web
echo " href='#null' ">> $web
echo " onclick=\"newWindow('desc5_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca ">> $web
echo "de este grafico</a></big></div> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig5.png' title='' ">> $web
echo " alt='Figura 6' ><br> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td ">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br> ">> $web
echo "      <div style='text-align: center; font-weight: bold;'><big><a ">> $web
echo " href='#null' ">> $web
echo " onclick=\"newWindow('desc6_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca ">> $web
echo "de este grafico</a></big></div> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td style='vertical-align: top;'><img src='./gif/${nestacion}fig6.png' title='' ">> $web
echo " alt='Figura 6' ><br> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web
echo "    <tr> ">> $web
echo "      <td ">> $web
echo " style='vertical-align: top; background-color: rgb(255, 204, 0);'><br> ">> $web
echo "      <div style='text-align: center; font-weight: bold;'><big><a ">> $web
echo " href='#null' ">> $web
echo " onclick=\"newWindow('desc7_sondeo.htm','','650','250','resizable,scrollbars,status')\">Acerca ">> $web
echo "de este grafico</a></big></div> ">> $web
echo "      </td> ">> $web
echo "    </tr> ">> $web

echo "  </tbody> ">> $web
echo "</table> ">> $web
echo "<br> ">> $web
echo "</body> ">> $web
echo "</html> ">> $web
