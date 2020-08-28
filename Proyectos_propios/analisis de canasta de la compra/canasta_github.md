Lectura de archivos PDF, transformacion de datos, creacion de tabla con informacion transaccionaria (tickets)
================

#### Cargar las librerias necesarias

#### Tabulizer es el paquete que me permite leer archivos PDF y extraer su informacion. Como esta hecho con Java, debo tambien cargar su dependencia rJava.

``` r
library(tabulizer)
library(rJava)
library(stringr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(chron)
```

    ## 
    ## Attaching package: 'chron'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     days, hours, minutes, seconds, years

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(openxlsx)
options(scipen=999) 
```

#### Los PDF se encuentran todos archivados en la misma carpeta, el siguiente chunck de codigo cargara, y modificara el nombre todos los pdf que se encuentren en la carpeta seleccionada. Los PDF seran almacenados en la lista tickets\_load.

``` r
pdf_path <- "C:/Users/Usuario/Desktop/Ventas/tickets/rojo/"

setwd(pdf_path)

tickets_rojo <-  list.files(pattern="*.pdf")

tickets_load <- list()

for(i in 1:length(tickets_rojo)){
  
  pdf_load <- tickets_rojo[i]
  
  tickets_load[[i]] <- extract_text(paste(pdf_path,pdf_load,sep =''))
  names(tickets_load)[i] <- gsub('.pdf$','',tickets_rojo[i])
  
}

tickets_final <- list()
```

#### Veamos un ejemplo de como se cargan los archivos pdf.

#### De este texto tenemos que rescatar la informacion relevante (Cantidad, Descripcion, Importe), y darle forma de un ticket.

``` r
print(tickets_load[1])
```

    ## $r1
    ## [1] "         DAMIAN\r\n     DIRECCION 123 COL. COLONIA\r\n           (555) 123 4567\r\n           RFC0031282AB1\r\n                 02/01/2020 09:48 PM\r\nCAJERO:                     LODEPATO\r\nFOLIO:                          2095\r\nCANT. DESCRIPCION           IMPORTE\r\n===================================\r\n1     CARAMELOS               $2.00\r\n1     JABON REXONA           $28.00\r\n        NO. DE ARTICULOS: 2\r\n           TOTAL: $30.00\r\n        PAGO CON: $30.00\r\n        SU CAMBIO: $0.00\r\n       GRACIAS POR SU COMPRA\r\n   WWW.ABARROTESPUNTODEVENTA.COM\r\n                                   .\r\n"

#### Para volver a darle forma de un ticket al texto extraido de los PDF, se les hara las modificaciones programadas abajo, con herramientas regex, como tambien aprovechando del formato estandar de los tickets PDF. Luego de de transformar el texto, se almacenan todos los tickets en otra lista.

``` r
for(i in 1:length(tickets_load)){

object_length <- '         DAMIAN\r\n     DIRECCION 123 COL. COLONIA\r\n           (555) 123 4567\r\n           RFC0031282AB1\r\n                 '

object_length_ending <- '          TOTAL: $193.00\r\n       PAGO CON: $200.00\r\n        SU CAMBIO: $7.00\r\n       GRACIAS POR SU COMPRA\r\n   WWW.ABARROTESPUNTODEVENTA.COM\r\n                                   .\r\n'

ticket <- substr(tickets_load[i],nchar(object_length),nchar(tickets_load[i]))

ticket <- substr(ticket,0,(nchar(ticket) - nchar(object_length_ending)))

ticket <- gsub('IMPORTE\r\n===================================\r\n','',ticket)

ticket <- gsub('\r\nCAJERO:                     LODEPATO\r\nFOLIO:                          ','',ticket)

ticket <- gsub('(PM|AM).*\r\nCANT. DESCRIPCION           ','\\1  ',ticket)

ticket <- gsub('\r\n','   ',ticket)

ticket <- trimws(ticket)

ticket <- str_squish(ticket)

date <- substr(ticket,0,10)

time <- substr(ticket,12,19)

articulos <- str_squish(substr(ticket,nchar(ticket) - 1,nchar(ticket)))

ticket <- substr(ticket,21,nchar(ticket))

ticket <- substr(ticket,0,(nchar(ticket) - 20))

ticket <- str_squish(ticket)

ticket <- gsub('\\.KG','',ticket)

ticket <- gsub('(^\\d+).\\d+KG','\\1',ticket)

ticket <- gsub('-','',ticket)

ticket_split <- gsub('(\\$\\d+\\.\\d+\\s)','\\1-',ticket)

ticket_split <- str_split(ticket_split,'-')

ticket_split <- unlist(ticket_split)

ticket_split <- gsub('(^\\d+)KG','\\1',ticket_split)

ticket_split <- str_squish(ticket_split)

ticket_split <- gsub('(^\\d+)\\.\\d+','\\1',ticket_split)

ticket_split <- gsub('(^\\d+\\s)','\\1-',ticket_split)

ticket_split <- gsub('(\\$)','-\\1',ticket_split)

DF <- data.frame(do.call(rbind, strsplit(ticket_split,"-", fixed=TRUE)))

names(DF) <- c('Cant','Descripcion','Precio.Total')

DF$Descripcion<- as.character(DF$Descripcion)
DF$Fecha <- dmy(date)
DF$Hora <- as.character(chron(times=substr(parse_date_time(time, '%I:%M %p'),12,19)))
DF$Articulos <- as.numeric(articulos)
DF$Precio.Total <- as.numeric(gsub('\\$','',DF$Precio.Total))
DF$Cant <- as.numeric(as.character(DF$Cant))
DF$Precio.Uni <- round(DF$Precio.Total/DF$Cant,2)
DF$Suma <- sum(DF$Precio.Total)
DF$Transaccion <- names(tickets_load)[i]

tickets_final[[i]] <- DF
names(tickets_final)[i] <- names(tickets_load)[i]


}

df_test <- bind_rows(tickets_final)

df_test$Hora <- chron(times=df_test$Hora)
```

#### Veamos un ejemplo de como van tomando forma los tickets.

#### Pasamos entonces de extraer y limpiar textos no estructurados, a tener el siguiente formato para cada ticket. Ademas, utilizando la informacion original de los tickets, agregamos nuevas columnas para brindar mejor informacion sobre cada operacion.

``` r
tickets_final[[1]]
```

    ##   Cant   Descripcion Precio.Total      Fecha     Hora Articulos Precio.Uni
    ## 1    1    CARAMELOS             2 2020-01-02 21:48:00         2          2
    ## 2    1 JABON REXONA            28 2020-01-02 21:48:00         2         28
    ##   Suma Transaccion
    ## 1   30          r1
    ## 2   30          r1

#### Los tickets individuales fueron todos almacenados en una lista, y luego se consolidaron todos los tickets en una tabla. Veamos las primeras 10 observaciones. La ultima columna 'Transaccion' es la llave que da identidad unica de cada ticket.

``` r
head(df_test,10)
```

    ##    Cant           Descripcion Precio.Total      Fecha     Hora Articulos
    ## 1     1            CARAMELOS             2 2020-01-02 21:48:00         2
    ## 2     1         JABON REXONA            28 2020-01-02 21:48:00         2
    ## 3   400 QUESO FRESCO ECONOMI           100 2020-01-02 21:20:00         2
    ## 4     1 MOLTO PURE DE TOMATE            30 2020-01-02 21:20:00         2
    ## 5     1 PUGLISI CABALLA EN A           129 2020-01-02 13:22:00         1
    ## 6     2  TANG MIX MULTIFRUTA            24 2020-01-02 13:19:00         2
    ## 7     1 GRANIX SKARCHITOS CE            57 2020-01-02 13:16:00         1
    ## 8   100 QUESO FRESCO PUNTA D            30 2020-01-02 13:15:00         3
    ## 9     1 MOLINOS ALA ARROZ LA            24 2020-01-02 13:15:00         3
    ## 10    1      SALCHICHA WELTY            30 2020-01-02 13:15:00         3
    ##    Precio.Uni Suma Transaccion
    ## 1        2.00   30          r1
    ## 2       28.00   30          r1
    ## 3        0.25  130         r10
    ## 4       30.00  130         r10
    ## 5      129.00  129        r100
    ## 6       12.00   24        r101
    ## 7       57.00   57        r102
    ## 8        0.30   84        r103
    ## 9       24.00   84        r103
    ## 10      30.00   84        r103

#### Los proximos dos pasos consisten en unir a las descripciones de los productos, con su codigo de barra que los identifica. Para hacer el analisis de canasta, es necesario que cada producto tenga un identificador unico, por ende la importancia del codigo de barra.

#### Dado a que las cajas en el negocio del cliente no estan unificadas(caja roja y caja azul), el mismo producto puede llegar a tener dos Descripciones distinitas, pero SIEMPRE EL MISMO CODIGO DE BARRA. El codigo de barra sera el identificador a tomar en cuenta por el algoritmo apriori del analisis de canasta.

#### Para afrontar este dilema, se les dara dara un formato uniforme a las descripciones en cada invetario, como tambien a nuestra tabla de tickets, luego se procedera a unir las descripciones de nuestra tabla con el codigo de barra de ambos inventarios, primero se prueba la union con el inventario de una caja, y luego con la otra.

``` r
#carga de inventarios
inventario_rojo <- read.xlsx('C:/Users/Usuario/Desktop/Ventas/tickets/inventario/inventario_rojo.xlsx')
inventario_azul <- read.xlsx('C:/Users/Usuario/Desktop/Ventas/tickets/inventario/inventario_azul.xlsx')


#Formato uniforme de descripciones
df_test$Descripcion <- str_squish(substr(df_test$Descripcion,0,19))

inventario_rojo$Descripcion <- str_squish(stringr::str_to_upper(substr(inventario_rojo$Descripcion,0,19)))

inventario_azul$Descripcion <- str_squish(stringr::str_to_upper(substr(inventario_azul$Descripcion,0,19)))
#19 characters ya que el ticket del punto de venta solo imprime los primeros 19 characters de cada Descripcion.
```

#### En el caso que se dupliquen las descripciones, eliminarlas.

#### Primero se une las descripciones de nuestra tabla con los codigos del primero inventario, se revisa cuales no lograron unirse, y las que no lograron unirse con un inventario, se unen con el otro inventario correspondiente.

``` r
#Elimina duplicaciones

dupli_df <- inventario_rojo[duplicated(inventario_rojo$Descripcion),]

inven_rojo_neto <- inventario_rojo[!duplicated(inventario_rojo$Descripcion),]

inven_azul_neto <- inventario_azul[!duplicated(inventario_azul$Descripcion),]

#Join con invetario rojo
left_test <- left_join(df_test,inven_rojo_neto[c(1,2)], by = 'Descripcion')

#Revisar cuales no unieron con rojo
diag_left <- left_test %>%
  filter(is.na(Codigo))

#Unir los NA que por descarte son de la otra caja

left_test[is.na(left_test$Codigo),'Codigo'] <- left_join(left_test[is.na(left_test$Codigo),],inven_azul_neto[c(1,2)],by = 'Descripcion')[11]
```

#### Veamos como va quedando la union de los codigos de barra.

#### Quedan valores no existentes (NA) en la ultima columna 'Codigo', como puede ser esto si ya probamos con los inventarios de ambas cajas del negocio?

``` r
head(left_test,10)
```

    ##    Cant         Descripcion Precio.Total      Fecha     Hora Articulos
    ## 1     1           CARAMELOS            2 2020-01-02 21:48:00         2
    ## 2     1        JABON REXONA           28 2020-01-02 21:48:00         2
    ## 3   400 QUESO FRESCO ECONOM          100 2020-01-02 21:20:00         2
    ## 4     1 MOLTO PURE DE TOMAT           30 2020-01-02 21:20:00         2
    ## 5     1  PUGLISI CABALLA EN          129 2020-01-02 13:22:00         1
    ## 6     2 TANG MIX MULTIFRUTA           24 2020-01-02 13:19:00         2
    ## 7     1 GRANIX SKARCHITOS C           57 2020-01-02 13:16:00         1
    ## 8   100  QUESO FRESCO PUNTA           30 2020-01-02 13:15:00         3
    ## 9     1 MOLINOS ALA ARROZ L           24 2020-01-02 13:15:00         3
    ## 10    1     SALCHICHA WELTY           30 2020-01-02 13:15:00         3
    ##    Precio.Uni Suma Transaccion        Codigo
    ## 1        2.00   30          r1           414
    ## 2       28.00   30          r1          <NA>
    ## 3        0.25  130         r10           100
    ## 4       30.00  130         r10 7791579001132
    ## 5      129.00  129        r100 7790748235064
    ## 6       12.00   24        r101 7622300868727
    ## 7       57.00   57        r102 7790045001607
    ## 8        0.30   84        r103           101
    ## 9       24.00   84        r103 7791120031557
    ## 10      30.00   84        r103          <NA>

#### Se da a las siguientes razones:

#### Existen ventas de articulos que por alguna razon particular del tendero, no dan de alta al producto y los venden como 'PRODUCTO COMUN'. Estos productos comunes no aparecen en el inventario pero si en nuestro tickets, entonces asignemelos un codigo de barra dummy a los productos comunes (lo representaremos con el numero 0).

#### Existe una sola otra excepcion que resulto ser los OREOs. Al parecer estaban dado de alta con un codigo dummy como tambien su codigo de barra en ambas cajas. Entonces, los unificare con su codigo de barra.

``` r
diag_left_2 <- left_test %>%
  filter(is.na(Codigo))

#Unir Oreos con un unico codigo
left_test[left_test$Descripcion == 'OREO X1' | left_test$Descripcion == 'OREO PAQ UNIDAD','Codigo'] <- as.numeric(7622300864958)

#Aisgnarles valor dummy de 0 a los productos comunes como codigo de barra.

left_test[is.na(left_test$Codigo),'Codigo'] <- 0

#Ver si aun quedan valores NA

sapply(left_test,function(x) sum(is.na(x)))
```

    ##         Cant  Descripcion Precio.Total        Fecha         Hora 
    ##            0            0            0            0            0 
    ##    Articulos   Precio.Uni         Suma  Transaccion       Codigo 
    ##            0            0            0            0            0

``` r
ifelse(sapply(left_test,function(x) sum(is.na(x))) > 0,'MISSING DATA','DATA OK')
```

    ##         Cant  Descripcion Precio.Total        Fecha         Hora 
    ##    "DATA OK"    "DATA OK"    "DATA OK"    "DATA OK"    "DATA OK" 
    ##    Articulos   Precio.Uni         Suma  Transaccion       Codigo 
    ##    "DATA OK"    "DATA OK"    "DATA OK"    "DATA OK"    "DATA OK"

#### Ultimos retoques:

#### Reordenar columnas para que la tabla sea mas facil de entender.

#### Crear,y asignar el directorio para almacenar nuestra tabla con informacion de los tickets.

#### Exportar nuestra tabla final para seguir con el analisis de canasta.

``` r
#rearrange columns:
#Fecha,Hora,Transaccion,Codigo,Descripcion,Cantidad,Precio.Uni,Precio.Total,Articulos,Suma

left_test <- left_test[c(4,5,9,10,2,1,7,3,6,8)]

##Proseguir con analisis:reglas de asociacion, aka, Apriori Algorithm
transacciones <- 'C:/Users/Usuario/Desktop/Ventas/tickets/rojo/transacciones/'

setwd(transacciones)
write.xlsx(left_test,paste(transacciones,'transacciones.xlsx',sep = ''))

head(left_test,10)
```

    ##         Fecha     Hora Transaccion        Codigo         Descripcion Cant
    ## 1  2020-01-02 21:48:00          r1           414           CARAMELOS    1
    ## 2  2020-01-02 21:48:00          r1             0        JABON REXONA    1
    ## 3  2020-01-02 21:20:00         r10           100 QUESO FRESCO ECONOM  400
    ## 4  2020-01-02 21:20:00         r10 7791579001132 MOLTO PURE DE TOMAT    1
    ## 5  2020-01-02 13:22:00        r100 7790748235064  PUGLISI CABALLA EN    1
    ## 6  2020-01-02 13:19:00        r101 7622300868727 TANG MIX MULTIFRUTA    2
    ## 7  2020-01-02 13:16:00        r102 7790045001607 GRANIX SKARCHITOS C    1
    ## 8  2020-01-02 13:15:00        r103           101  QUESO FRESCO PUNTA  100
    ## 9  2020-01-02 13:15:00        r103 7791120031557 MOLINOS ALA ARROZ L    1
    ## 10 2020-01-02 13:15:00        r103             0     SALCHICHA WELTY    1
    ##    Precio.Uni Precio.Total Articulos Suma
    ## 1        2.00            2         2   30
    ## 2       28.00           28         2   30
    ## 3        0.25          100         2  130
    ## 4       30.00           30         2  130
    ## 5      129.00          129         1  129
    ## 6       12.00           24         2   24
    ## 7       57.00           57         1   57
    ## 8        0.30           30         3   84
    ## 9       24.00           24         3   84
    ## 10      30.00           30         3   84

Listo! Ahora que hemos extraido, transformado, y estandarizado nuestra informacion de los tickets, podemos proceder con analizar nuestra canasta con ayuda de *Python* y ver cuales de nuestros productos son complementarios.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
