#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)
library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)


resumen <- read_xlsx('resumen.xlsx')
productos_comun_de_evolucion <- read_xlsx('productos_comun_de_evolucion.xlsx')
resumen_ventas_xart <- read_xlsx('resumen_ventas_xart.xlsx')
share_x_cat <- read_xlsx('share_x_cat.xlsx')
venta_x_dia <- read_xlsx('venta_x_dia.xlsx')
mensual <- read_xlsx('mensual.xlsx')

ui <- fluidPage(
  titlePanel("LSV - Analisis de Ventas"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('fecha','Seleccione fecha', start = tail(venta_x_dia$Fecha,7)[1], end = tail(venta_x_dia$Fecha,7)[7],
                     min = min(venta_x_dia$Fecha), max = max(venta_x_dia$Fecha)),
      pickerInput('dia_picker','Seleccione dia(s) deseado', choices = unique(venta_x_dia$Dia), multiple = TRUE,selected = unique(venta_x_dia$Dia))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Tabla',DT::DTOutput('ventas_x_dia_tabla')),
        tabPanel('Grafico',plotly::plotlyOutput('ventas_x_dia'))
        
        
      )
    )
  ),
  
  ####venta_x_cat
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('select_share_fecha_depto','Seleccione Fecha', start = tail(sort(unique(share_x_cat$Fecha)),7)[1], end = tail(sort(unique(share_x_cat$Fecha)),7)[7],
                     min = min(share_x_cat$Fecha), max = max(share_x_cat$Fecha)),
      sliderInput('select_share_x_cat_head','Seleccione cantidad de categorias',min = 0,max = length(unique(share_x_cat$Departamento)),value = 10, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Tabla',DT::DTOutput('share_x_categoria_tabla')),
        tabPanel('Ventas',plotly::plotlyOutput('ventas_x_categoria')),
        tabPanel('Share',plotly::plotlyOutput('share_x_categoria'))
        
      )
      
    )
    
  ),
  ###VENTA_X_CATEGORIA_PRODUCTO

  sidebarLayout(
    sidebarPanel(
      selectInput('select_departamento','Seleccione la categoria deseada', unique(resumen$Departamento)),
      dateRangeInput('select_departamento_fecha','Seleccione la(s) fecha(s) deseada',  start = tail(sort(unique(resumen$Fecha)),7)[1], end = tail(sort(unique(resumen$Fecha)),7)[7],
                     min = min(resumen$Fecha), max = max(resumen$Fecha)),
      sliderInput('select_departamento_artcant','Seleccione numero de items a observar en el grafico', min = 0,max = 50,value = 10)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Tabla',DT::DTOutput('ventas_x_categoriaprod_tabla')),
        tabPanel('Ventas',plotly::plotlyOutput('ventas_x_categoriaprod')),
        tabPanel('Share',plotly::plotlyOutput('ventas_x_categoriaprod_share'))
        
      )
      
      
    )
    
  ),
  
  ##MENSUAL
  
  sidebarLayout(
    sidebarPanel(
      selectInput('select_departamento_mes','Seleccione la categoria deseada', unique(mensual$Departamento))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel('Tabla',DT::DTOutput('departamento_mes'))
        
      )
      
      
    )
    
  ),
  
  
  ####ventas_x_art
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('select_venta_x_art_fecha','Seleccione la(s) fecha(s) deseada', start = tail(sort(unique(resumen_ventas_xart$Fecha)),7)[1], end = tail(sort(unique(resumen_ventas_xart$Fecha)),7)[7],
                     min = min(resumen_ventas_xart$Fecha), max = max(resumen_ventas_xart$Fecha)),
      sliderInput('select_venta_x_art_head_grafico','Seleccione numero de productos a observar en el grafico', min = 0,max = 50,value = 25)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Tabla',DT::DTOutput('venta_x_art_tabla')),
        tabPanel('Grafico',plotly::plotlyOutput('ventas_x_art'))
        
      )
      
    )
    
  ),
  
  ##Productos comun
  sidebarLayout(
    sidebarPanel(
      pickerInput('picker_producto_comun','Selecciones la(s) semana(s) deseadas', choices = sort(unique(productos_comun_de_evolucion$segmento)), multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('(De)Evolucion',plotly::plotlyOutput('productos_comun'))
      )
      
    )
    
  )
  
)















##!!!!!!!!!!!!!!!!!!!!!ADD MONTH TO RESUMEN_ESTADISTICAS IN OTHER SCRIPT
####----VENTA X DIA 
server <- function(input, output, session){
  output$ventas_x_dia_tabla <- DT::renderDT({
    
    DT::datatable(venta_x_dia %>%
                    filter(Fecha >= input$fecha[1], Fecha <= input$fecha[2],Dia %in% input$dia_picker)%>%
                    mutate(Fecha = format(as.Date(Fecha),'%Y-%m-%d'),promedio = round(mean(unlist(venta_x_dia[venta_x_dia$Fecha >= input$fecha[1] &
                                                                                                                venta_x_dia$Fecha <= input$fecha[2] & venta_x_dia$Dia %in% input$dia_picker,'venta_total'])),2), suma = sum(venta_total), ganancia = suma * 0.35))
    
    
  })
  
  ##YOU HAVE TO FILTER THE DATES FIRST, THEN PLOT
  output$ventas_x_dia <- plotly::renderPlotly({
    venta_x_dia %>%
      filter(Fecha >= input$fecha[1], Fecha <= input$fecha[2], Dia %in% input$dia_picker)%>%
      ggplot(aes(x = factor(Fecha), y = venta_total, fill = Dia))+
      geom_col()+
      theme(axis.text.x=element_text(angle=90,hjust=1))+
      xlab('Fecha')+
      ylab('Ventas en Pesos')+
      ggtitle('Ventas por dia')
  })
  
  ####----VENTA X CAT
  #!!!!!!!!!!!!!!!!!!CREATE A GROUP_BY CATEGORY/DESCRIPTION IN AUTOMATIZE
  
  
  output$ventas_x_categoriaprod_tabla <- DT::renderDT({
    
    resumen %>%
      filter(Departamento == input$select_departamento, Fecha >= input$select_departamento_fecha[1], Fecha <=
               input$select_departamento_fecha[2])%>%
      group_by(Departamento,Codigo,Descripcion)%>%
      summarize(venta_total = sum(venta), cantidad_total = sum(Cantidad))%>%
      ungroup()%>%
      mutate(share = round((venta_total/sum(venta_total)*100),2))%>%
      arrange(desc(venta_total))
    
    
    
  })
  
  output$ventas_x_categoriaprod <- plotly::renderPlotly({
    
    resumen %>%
      filter(Departamento == input$select_departamento,Fecha >= input$select_departamento_fecha[1], Fecha <=
               input$select_departamento_fecha[2])%>%
      group_by(Departamento,Codigo,Descripcion)%>%
      summarize(venta_total = sum(venta), cantidad_total = sum(Cantidad))%>%
      arrange(desc(venta_total))%>%
      head(input$select_departamento_artcant)%>%
      ggplot(aes(reorder(x = Descripcion, -venta_total), y = venta_total))+
      geom_col(color='red',fill = 'white')+
      xlab('Producto')+
      ylab('Venta')+
      theme(axis.text.x=element_text(angle=60,hjust=1))
    
    
    
    
  })
  
  output$ventas_x_categoriaprod_share <- plotly::renderPlotly({
    
    resumen %>%
      filter(Departamento == input$select_departamento,Fecha >= input$select_departamento_fecha[1], Fecha <=
               input$select_departamento_fecha[2])%>%
      group_by(Departamento,Codigo,Descripcion)%>%
      summarize(venta_total = sum(venta), cantidad_total = sum(Cantidad))%>%
      ungroup()%>%
      mutate(share = round((venta_total/sum(venta_total)*100),2))%>%
      arrange(desc(venta_total))%>%
      head(input$select_departamento_artcant)%>%
      ggplot(aes(reorder(x = Descripcion, -share), y = share))+
      geom_col(color='red',fill = 'pink')+
      xlab('Producto')+
      ylab('Venta')+
      theme(axis.text.x=element_text(angle=60,hjust=1))
    
    
    
    
  })
  
  
  ##MENSUAL
  
  output$departamento_mes <-  DT::renderDT({
    
    mensual %>%
      filter(Departamento == input$select_departamento_mes)
    
    
  })
  
  
  ####----VENTA X ART
  output$ventas_x_art <- plotly::renderPlotly({
    
    resumen_ventas_xart %>%
      filter(Fecha >= input$select_venta_x_art_fecha[1], Fecha <= input$select_venta_x_art_fecha[2])%>%
      group_by(Codigo,Descripcion)%>%
      summarize(venta_total_art = sum(venta_total), cantidad_total_art = sum(cantidad_total))%>%
      arrange(desc(venta_total_art))%>%
      head(input$select_venta_x_art_head_grafico)%>%
      ggplot(aes(reorder(x = Descripcion, -venta_total_art), y = venta_total_art))+
      geom_col(color='white',fill = 'red')+
      xlab('Producto')+
      ylab('Venta')+
      theme(axis.text.x=element_text(angle=60,hjust=1))
    
    
    
  })
  
  output$venta_x_art_tabla <- DT::renderDT({
    
    
    DT::datatable(resumen_ventas_xart %>%
                    filter(Fecha >= input$select_venta_x_art_fecha[1], Fecha <= input$select_venta_x_art_fecha[2])%>%
                    group_by(Codigo,Descripcion)%>%
                    summarize(venta_total_art = sum(venta_total), cantidad_total_art = sum(cantidad_total))%>%
                    ungroup()%>%
                    mutate(share = round((venta_total_art/sum(venta_total_art))*100,2))%>%
                    arrange(desc(venta_total_art)))
    
    
  })
  
  output$ventas_x_categoria <- plotly::renderPlotly({
    
    
    share_x_cat%>%
      filter(Fecha >= input$select_share_fecha_depto[1], Fecha <= input$select_share_fecha_depto[2])%>%
      group_by(Departamento)%>%
      summarize(venta_total_cat = sum(venta_total))%>%
      arrange(desc(venta_total_cat))%>%
      head(input$select_share_x_cat_head)%>%
      ggplot(aes(reorder(x = Departamento,-venta_total_cat), y = venta_total_cat))+
      geom_col(color='green',fill = 'white')+
      xlab('Departamento')+ylab('Share')+
      theme(axis.text.x=element_text(angle=60,hjust=1))
    
    
    
  })
  
  output$share_x_categoria <- plotly::renderPlotly({
    
    share_x_cat%>%
      filter(Fecha >= input$select_share_fecha_depto[1], Fecha <= input$select_share_fecha_depto[2])%>%
      group_by(Departamento)%>%
      summarize(venta_total_cat = sum(venta_total))%>%
      mutate(share = round((venta_total_cat/sum(venta_total_cat)*100),2))%>%
      arrange(desc(venta_total_cat))%>%
      head(input$select_share_x_cat_head)%>%
      ggplot(aes(reorder(x = Departamento,-share), y = share))+
      geom_col(color='blue',fill = 'white')+
      xlab('Departamento')+ylab('Share')+
      theme(axis.text.x=element_text(angle=60,hjust=1))
    
    
    
  })
  
  
  
  
  output$share_x_categoria_tabla <- DT::renderDT({
    
    share_x_cat%>%
      filter(Fecha >= input$select_share_fecha_depto[1], Fecha <= input$select_share_fecha_depto[2])%>%
      group_by(Departamento)%>%
      summarize(venta_total_cat = sum(venta_total))%>%
      mutate(share = round((venta_total_cat/sum(venta_total_cat)*100),2))%>%
      arrange(desc(venta_total_cat))%>%
      head(input$select_share_x_cat_head)
    
  })
  
  
  output$productos_comun <- plotly::renderPlotly({
    
    validate(need(input$picker_producto_comun != '','Seleccione la(s) semana(s) deseadas'))
    
    productos_comun_de_evolucion%>%
      filter(segmento %in% input$picker_producto_comun)%>%
      arrange(segmento)%>%
      ggplot(aes(x = segmento, y = venta_total,group = 1))+
      geom_line(color = 'red')+
      geom_point()+
      xlab('Semanas')+ylab('Valores')
    
    
    
  })
  
}

shinyApp(ui = ui, server = server)



