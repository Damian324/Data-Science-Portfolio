rm(list = ls())


##Paths

resumen_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/resumen'

resumen_ventas_xart_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/resumen_ventas_xart'

share_x_cat_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/share_x_cat'

venta_x_dia_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/venta_x_dia'

productos_comun_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/productos_comun'

app_path <- 'C:/Users/Usuario/Desktop/Ventas/App/LSV'

####Inputs

#RESUMEN

setwd(resumen_path)

resumen_files <-  list.files(pattern="*.xlsx")

resumen_list <- list()

for(i in 1:length(resumen_files)){
  
  x_load <- resumen_files[i]
  resumen_list[[i]] <- read_xlsx(x_load)
  names(resumen_list)[i] <- gsub('.xlsx$','',resumen_files[i])
  
}

resumen <- bind_rows(resumen_list)

resumen <- resumen %>%
  mutate(Mes = months(Fecha), label_id = substr(resumen$Fecha,9,11))

setwd(app_path)

write.xlsx(resumen,'resumen.xlsx')

##MENSUAL
mensual <- resumen

mensual$mes <- month(mensual$Fecha)

mensual <- mensual %>%
  group_by(mes,Departamento,Descripcion)%>%
  filter(mes != 12)%>%
  summarize(ventas_mensual = sum(venta), cant_mensual = sum(Cantidad))%>%
  arrange(Descripcion,mes)


for (i in 1:nrow(mensual)){
  
  if(mensual[i,'mes'] == 1){
    
    x <- 0
    y <- 0
    
  }else if((abs((mensual[i,'mes'] - mensual[i - 1,'mes'])) > 1) & 
           (mensual[i,'Descripcion'] != mensual[i - 1,'Descripcion'])){
    
    x <- 0
    y <- 0
    
  }else{
    
    x <- ifelse(mensual$Descripcion[i] == mensual$Descripcion[i - 1],
                mensual$ventas_mensual[i] - mensual$ventas_mensual[i - 1],0)
    
    y <- ifelse(mensual$Descripcion[i] == mensual$Descripcion[i - 1],
                mensual$cant_mensual[i] - mensual$cant_mensual[i - 1],0)
    
  }
  
  mensual$dif_venta[i] <- x
  mensual$dif_cant[i] <- y
  
}

setwd(app_path)

write.xlsx(mensual,'mensual.xlsx')

#RESUMEN_VENTAS_XART

setwd(resumen_ventas_xart_path)

resumen_ventas_xart_files <-  list.files(pattern="*.xlsx")

resumen_ventas_xart_list <- list()

for(i in 1:length(resumen_ventas_xart_files)){
  
  x_load <- resumen_ventas_xart_files[i]
  resumen_ventas_xart_list[[i]] <- read_xlsx(x_load)
  names(resumen_ventas_xart_list)[i] <- gsub('.xlsx$','',resumen_ventas_xart_files[i])
  
}

resumen_ventas_xart <- bind_rows(resumen_ventas_xart_list)

setwd(app_path)

write.xlsx(resumen_ventas_xart,'resumen_ventas_xart.xlsx')

#SHARE_X_CAT

setwd(share_x_cat_path)

share_x_cat_files <-  list.files(pattern="*.xlsx")

share_x_cat_list <- list()

for(i in 1:length(share_x_cat_files)){
  
  x_load <- share_x_cat_files[i]
  share_x_cat_list[[i]] <- read_xlsx(x_load)
  names(share_x_cat_list)[i] <- gsub('.xlsx$','',share_x_cat_files[i])
  
}

share_x_cat <- bind_rows(share_x_cat_list)

setwd(app_path)

write.xlsx(share_x_cat,'share_x_cat.xlsx')


##VENTA_X_DIA

setwd(venta_x_dia_path)

venta_x_dia_files <-  list.files(pattern="*.xlsx")

venta_x_dia_list <- list()

for(i in 1:length(venta_x_dia_files)){
  
  x_load <- venta_x_dia_files[i]
  venta_x_dia_list[[i]] <- read_xlsx(x_load)
  names(venta_x_dia_list)[i] <- gsub('.xlsx$','',venta_x_dia_files[i])
  
}

venta_x_dia <- bind_rows(venta_x_dia_list)

venta_x_dia <- venta_x_dia %>%
  arrange(Fecha)

setwd(app_path)

write.xlsx(venta_x_dia,'venta_x_dia.xlsx')

##PRODUCTOS_COMUN

setwd(productos_comun_path)

productos_comun_files <-  list.files(pattern="*.xlsx")

productos_comun_list <- list()

for(i in 1:length(productos_comun_files)){
  
  x_load <- productos_comun_files[i]
  productos_comun_list[[i]] <- read_xlsx(x_load)
  names(productos_comun_list)[i] <- gsub('.xlsx$','',productos_comun_files[i])
  
}

productos_comun_de_evolucion <- bind_rows(productos_comun_list)

setwd(app_path)

write.xlsx(productos_comun_de_evolucion,'productos_comun_de_evolucion.xlsx')



rm(productos_comun_list,resumen_list,resumen_ventas_xart_list,share_x_cat_list,venta_x_dia_list)

beep(1)
