library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(beepr)
options(scipen=999) #removes scientific notation


#####-----------DATA PREP-------------------------

#loading files to to environment

resumen_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/resumen'

resumen_ventas_xart_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/resumen_ventas_xart'

share_x_cat_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/share_x_cat'

venta_x_dia_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/venta_x_dia'

productos_comun_path <- 'C:/Users/Usuario/Desktop/Ventas/Shiny_Outputs/productos_comun'




setwd(path_raw)

getwd()

ventas <-  list.files(pattern="*.xlsx")

df_load <- list()

for(i in 1:length(ventas)){
  
  x_load <- ventas[i]
  
  df_load[[i]] <- read_excel(x_load,col_types = c('text','text','numeric','text','text','text','guess'))
  names(df_load)[i] <- gsub('.xlsx$','',ventas[i])
  
}

list2env(df_load, envir = .GlobalEnv)



#DATA WRANGLING


finde <- c('sábado','domingo')

df_names <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

df_names

df_list <- list()

for(i in 1:length(df_names)){
  
  df_list[[i]]<- get(df_names[i])
  names(df_list)[i] <- df_names[i]
  df_list[[i]][[4]] <- as.numeric(gsub('\\$','',df_list[[i]][[4]]))
  
  names(df_list[[i]])[4] <- 'Precio_Unitario'
  df_list[[i]]$`Precio Costo` <- NULL
  df_list[[i]]$venta <- round((df_list[[i]]$Cantidad * df_list[[i]]$Precio_Unitario),2)
  df_list[[i]] <- df_list[[i]] %>%
    mutate(Codigo = as.numeric(ifelse(Descripcion == 'Combo Pancho Largo','222',Codigo)))%>%
    mutate(Codigo = as.numeric(ifelse(Departamento == '- Comunes -',0,Codigo)))
  
  df_list[[i]]$Fecha <- ymd(df_list[[i]]$Fecha)
  df_list[[i]]<- df_list[[i]] %>%
    mutate(Dia = weekdays(Fecha))
  df_list[[i]] <- df_list[[i]] %>%
    mutate(semana = ifelse(Dia %in% finde,'finde','semana'))
  
  
}

resumen <- bind_rows(df_list)

sort(unique(resumen$Fecha))



#LOAD LATEST INVENTORY LIST INTO ENVIRONMENT AND MATCH BY Codigo
setwd(path_inventario)

inventario <-  list.files(pattern="*.xlsx")

list2env(
  lapply(setNames(inventario, make.names(gsub("*.xlsx$", "", inventario))), 
         read_excel), envir = .GlobalEnv)

#Match descriptions between resumen and inventario

match_descripciones <- function(x,inventario_nombre){
  y <- x
  
  y <- left_join(y,inventario[c(1,2)],by = 'Codigo')
  
  y <- y %>%
    mutate(Descripcion.y = ifelse(Codigo == 0,'- Producto Comun -',Descripcion.y))
  
  y <- y[c('Codigo','Descripcion.y','Cantidad','Precio_Unitario','Departamento','venta','Fecha','Dia','semana','Descripcion.x')]
  
  y$Descripcion.x <- NULL
  
  names(y) <- c('Codigo','Descripcion','Cantidad','Precio_Unitario','Departamento','venta','Fecha','Dia','semana')
  
  z <- deparse(substitute(x))
  assign(z,y,envir = .GlobalEnv)
  
  
}

match_descripciones(resumen,inventario)


###Add columns splitting month into weeks
cat_not_consider <- c("Golosinas (Eliminado 04/01/2020)","- Sin Departamento -","Analgesicos",'Pastas (Eliminado 04/01/2020)')

categorias <- unique(resumen$Departamento)

categorias <- categorias[!(unique(resumen$Departamento) %in% cat_not_consider)]

resumen <- resumen %>%
  filter(Departamento %in% categorias)%>%
  arrange(Fecha)

resumen <- resumen %>% 
  mutate(segmento_int = cut.Date(Fecha, breaks = "7 days", labels = FALSE)) %>% 
  arrange(Fecha)

resumen <- resumen %>%
  group_by(segmento_int)%>%
  mutate(segmento = paste(substr(min(Fecha),start = 6, stop = 10),substr(max(Fecha),start = 6,stop = 10), sep = '/'))


year_month <- paste(gsub('\\.','',unique(format(as.Date(resumen$Fecha), "%Y-%b"))),'.xlsx',sep = '')

setwd(resumen_path)

write.xlsx(resumen,
           paste('resumen', year_month, sep = '-'))
###----------DATA PREP COMPLETE------------------


####------------ANALYTICS---------------------


####--------------VENTAS X DIA

venta_x_dia <- resumen%>%
  group_by(Fecha,Dia)%>%
  summarize(venta_total = sum(venta))%>%
  ungroup()%>%
  group_by(Dia)%>%
  mutate(promedio = mean(venta_total))

setwd(venta_x_dia_path)

write.xlsx(venta_x_dia,paste('venta_x_dia', year_month, sep = '-'))


##---------VENTAS X ARTICULOS, ACUMULADOS

#Monto total venta de cada articulo

resumen_ventas_xart <- resumen%>%
  group_by(Codigo,Descripcion,Fecha)%>%
  summarize(venta_total = sum(venta), cantidad_total = sum(Cantidad))%>%
  arrange(desc(venta_total))

setwd(resumen_ventas_xart_path)

write.xlsx(resumen_ventas_xart,paste('resumen_ventas_xart', year_month, sep = '-'))

#ventas en cantidad de cada articulo

####-------DE-EVOLUCION DE PRODUCTOS COMUNES

productos_comun_de_evolucion <- resumen %>%
  filter(Codigo == 0) %>%
  group_by(segmento) %>%
  summarize(venta_total = sum(venta))%>%
  arrange(segmento)

setwd(productos_comun_path)

write.xlsx(productos_comun_de_evolucion,paste('productos_comun_de_evolucion', year_month, sep = '-'))

###----------ANALYTICS POR CATEGORIA

sale_by_cat <- resumen %>%
  group_by(Departamento,Fecha)%>%
  summarize( venta_total = sum(venta))%>%
  arrange(desc(venta_total))

#share of categoria

share_x_cat <- sale_by_cat%>%
  mutate(share = round((venta_total/sum(venta_total)*100),2),graph = paste(Departamento,share,sep = ' '))

setwd(share_x_cat_path)

write.xlsx(share_x_cat,paste('share_x_cat', year_month, sep = '-'))

setwd(script_source)

beep(1)




