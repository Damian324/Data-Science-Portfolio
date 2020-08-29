### Leer primero

Este proyecto se utiliza en un negocio de caracter venta al por menor, en el cual requerian saber cuales productos se venden con cual otro, es decir, que productos resultan ser complementarios. IBM define el analisis de canasta(cesta) de la compra de la siguiente manera: "En el análisis de cesta de la compra, algoritmos inteligentes analizan enormes cantidades de datos transaccionales para poner de relieve “asociaciones” – patrones que muestran los vínculos entre productos que habitualmente se compran juntos."

El analisis se compone de 2 partes:

1) [Importacion,transformacion, y estructuracion de los datos](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/analisis%20de%20canasta%20de%20la%20compra/canasta_github.md)
2) [Analisis de canasta de la compra](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/analisis%20de%20canasta%20de%20la%20compra/ejecucion_analisis_canasta.ipynb)

_Aclaracion: Para este analisis, dado al respeto de la privacidad los datos de mi cliente, y con su autorizacion,solo se uso una muestra pequeña de dos dias de trabajo, de solo una caja._

#### Primer Parte

Esta [primer](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/analisis%20de%20canasta%20de%20la%20compra/canasta_github.md) parte del analisis se realizo con el software libre [R](https://www.r-project.org/). Esta herramienta de programacion es sumamente util por distintias razones, incluyendo dandole estructura a datos no estructurados.

Los datos que utilizaremos provienen de el sistema de punto de venta(POS) del cliente. Una barrera a superar con la informacion exportada de este POS, es que los reportes de ventas que emite son acumulados, es decir, no identifica a cada venta en particular (no hay Ticket ID), ni discrimia por timestamp (fecha, y hora). 

Sin embargo existe una manera para ver la informacion de cada transaccion realizada. El tema es que esta informacion vienen en forma de PDF, y es el ticket particular emitido en cada transaccion. Dejo imagen ejemplar:

<img src="https://raw.githubusercontent.com/Damian324/Data-Science-Portfolio/master/Proyectos_propios/analisis%20de%20canasta%20de%20la%20compra/imagenes/ticket.jpg">

Entonces tuve que desarrollar un script que logre extraer esta informacin de los tickets, y juntarlo todo en una tabla para realizar este analisis, ademas de muchos otros estudios posibles. 

Mas detalles en este [link](https://github.com/Damian324/Data-Science-Portfolio/blob/master/Proyectos_propios/analisis%20de%20canasta%20de%20la%20compra/canasta_github.md) a la primer parte de este proyecto.
