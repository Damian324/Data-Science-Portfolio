library(tabulizer)
library(rJava)
library(stringr)
library(lubridate)
library(chron)
library(dplyr)
library(openxlsx)
options(scipen=999) 


pdf_path <- "C:/Users/Usuario/Desktop/Ventas/tickets/rojo/"

setwd(pdf_path)

tickets_rojo <-  list.files(pattern="*.pdf")

tickets_rojo

tickets_load <- list()

for(i in 1:length(tickets_rojo)){
  
  pdf_load <- tickets_rojo[i]
  
  tickets_load[[i]] <- extract_text(paste(pdf_path,pdf_load,sep =''))
  names(tickets_load)[i] <- gsub('.pdf$','',tickets_rojo[i])
  
}

tickets_final <- list()

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

#net step is to join df_test description and inventory(ill have to join both red and blue because i have mixed tickets here)


inventario_rojo <- read.xlsx('C:/Users/Usuario/Desktop/Ventas/tickets/inventario/inventario_rojo.xlsx')
inventario_azul <- read.xlsx('C:/Users/Usuario/Desktop/Ventas/tickets/inventario/inventario_azul.xlsx')

#Make both descriptions same length in respects of nchar

df_test$Descripcion <- str_squish(substr(df_test$Descripcion,0,19))

inventario_rojo$Descripcion <- str_squish(stringr::str_to_upper(substr(inventario_rojo$Descripcion,0,19)))
inventario_azul$Descripcion <- str_squish(stringr::str_to_upper(substr(inventario_azul$Descripcion,0,19)))
#It is limited to 19 charactrs given the length of the description of item when ticket is printed (which is actually 20, but 19 ajusts for '.KG or KG' in quantities of Balanceados and Fiambreria)

#Check duplicated values (on Descripcion) and remove them

dupli_df <- inventario_rojo[duplicated(inventario_rojo$Descripcion),]

inven_rojo_neto <- inventario_rojo[!duplicated(inventario_rojo$Descripcion),]

inven_azul_neto <- inventario_azul[!duplicated(inventario_azul$Descripcion),]

#first check which descrptions match with the inventario rojo
left_test <- left_join(df_test,inven_rojo_neto[c(1,2)], by = 'Descripcion')

#See which ones didnt match
diag_left <- left_test %>%
  filter(is.na(Codigo))

#Match remaining NAs with inventario azul

left_test[is.na(left_test$Codigo),'Codigo'] <- left_join(left_test[is.na(left_test$Codigo),],inven_azul_neto[c(1,2)],by = 'Descripcion')[11]

diag_left_2 <- left_test %>%
  filter(is.na(Codigo))
#All of the NAs are attributed to 'Productos Comun', which are items without a barcode.
#The only exception is the OREO X1 item, which has been recently changed in the POS system.
#Oreo code 7622300864958

left_test[left_test$Descripcion == 'OREO X1' | left_test$Descripcion == 'OREO PAQ UNIDAD','Codigo'] <- as.numeric(7622300864958)

#Replace all of Productos Comun with dummy value of 0.

left_test[is.na(left_test$Codigo),'Codigo'] <- 0

#Test for missing data

sapply(left_test,function(x) sum(is.na(x)))

ifelse(sapply(left_test,function(x) sum(is.na(x))) > 0,'MISSING DATA','DATA OK')

#rearrange columns:
#Fecha,Hora,Transaccion,Codigo,Descripcion,Cantidad,Precio.Uni,Precio.Total,Articulos,Suma

left_test <- left_test[c(4,5,9,10,2,1,7,3,6,8)]

##Proseguir con analisis:reglas de asociacion, aka, Apriori Algorithm
transacciones <- 'C:/Users/Usuario/Desktop/Ventas/tickets/rojo/transacciones/'

setwd(transacciones)
write.xlsx(left_test,paste(transacciones,'transacciones.xlsx',sep = ''))
