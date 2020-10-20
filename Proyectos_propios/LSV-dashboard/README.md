# Leer primero!!

### LSV-Dashboard

Se me pidio que se armara unas aplicaciones donde se pudiese ver las ventas y otras estadisticas importantes del negocio. Los interesados tendrian entonces la posibilidad de estudiar posibles oportunidades de negocio, observar debilidades, y en general tener una idea del monto de dinero que mueve dia a dia.

No habia previamente instalado un punto de venta (POS) en su negocio, por lo cual nunca se supo exactamente cuanto se ingresaba de dinero, ni cuanto se vendia de cada producto.

Luego de haber instalado un sistema POS en el negocio, con los datos provenientes de dicho POS, pude crear 2 Shiny web Apps, donde podra observar las 24hs la informacion que el buscaba.

#### En practica, solo es necesario abrir el script Madre(Mother) porque en este script corren todos los otros scripts, incluso envia el orden de subir la aplicacion a la nube como ultimo orden. El script Madre es el "All-in-1 Solution".
  El orden cronologico entonces seria:
  1) Abrir el script [Madre](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/Mother_v2.R)
  2) Al correr el Madre, enviara el orden de primero ejecutar el script [Automatizacion](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/Automatizacion_v3.R)
  3) Al finalizar la ejecucion de Automatizaciones, ejecuta el script [Clusterizacion](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/Clusterizacion_v2.1.R)
  4) Luego de finalizar la clusterizacion, se sube la aplicacion al servidor Shiny, listo para ser visualizado!
      * Las aplicaciones son, el [Dashboard](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/app_dashboard.R) y la [App_Almacen](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/app_almacen.R)

  
#### Explicacion de los scripts

1) Automatizacion: 
  * Carga el [raw data(link a un ejemplo)](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/ejemplos/ene_2.xlsx) del POS
  * Transforma los inputs en nuevas planillas con info relevante a las ventas, asi brindando informacion como venta total por dia,semana,mes, u otro rango deseado,venta de cada producto individual,venta por categoria, caida o alza de ventas de los productos, y otras funciones mas.
  * [Enlace](https://github.com/Damian324/Data-Science-Portfolio/tree/master/Proyectos_propios/LSV-dashboard/ejemplos) a outputs ejemplares del mes enero.
  * Outputs generados:
<img alt= "Outputs generados por script automatizacion" src="https://raw.githubusercontent.com/Damian324/Data-Science-Portfolio/master/Proyectos_propios/LSV-dashboard/imagenes/outputs.jpg">
  Estas planillas son todas guardadas segun el mes de los datos, para luego ser cargadas y unidas en el script siguiente
  
  
2) Clusterizacion:
  * El objetivo de el script presente es cargar todos los archivos que se crearon previamente, juntarlos y consolidarlos en tablas que luego seran las utilizadas para generar las aplicaciones
<img alt="Outputs del script clusterizacion" src="https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/imagenes/clusterizacion.jpg">  

3) Apps
  * Por cuestiones de privacidad del negocio, los links a los shiny apps no estaran presentes en este documento. Sin embargo, los scripts si, ademas me han permitido a incluir fotos para mostrar aunque sea un pantallazo de lo que realmente son las apps.
  * El objetivo del [DASHBOARD](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/app_dashboard.R) es visualizar lo esencial del negocio. La informacion esta consolidada en un dashboard facil de enteder y visualizar. Dejo printscreen con explicaciones:
 <img src="https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/imagenes/app_dashboard_1.jpg">
 
  * Tab que se abre al hacer click sobre un dia en particular:
 <img src="https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/imagenes/app_dashboard_dia.jpg">
 
  * Tab que se abre al hacer click sobre una categoria en particular:
 <img src="https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/imagenes/app_dashboard_cat.jpg">
 
  * La segunda [APLICACION](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/LSV-dashboard/app_almacen.R) entra en mas detalle, dependiendo de lo que se quiera estudiar. Se divide en 6 "bloques" e incluye informacion como por ejemplo, de que productos aumentaron o bajaron en ventas, cual es el share de cada producto, seleccion de periodos especificos para analizar fechas flexibles, y otras funciones.
  * Dejo abajo 2 bloques ejemplares:
  <img src="https://raw.githubusercontent.com/Damian324/Data-Science-Portfolio/master/Proyectos_propios/LSV-dashboard/imagenes/app_almacen_cat.jpg">
  <img src="https://raw.githubusercontent.com/Damian324/Data-Science-Portfolio/master/Proyectos_propios/LSV-dashboard/imagenes/app_almacen_dif.jpg">
 
 
 
 
 
 
 
 
 
 
 
 
