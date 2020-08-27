library(shiny)
library(shinydashboard)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)
library(DT)
library(r2d3)
library(openxlsx)
library(readxl)


resumen <- read_xlsx('resumen.xlsx')

meses_lista <- c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre',
              'noviembre','diciembre')

ui <- dashboardPage(
  dashboardHeader(
    title = "LSV",
    titleWidth = 200
  ),
    dashboardSidebar(
      sidebarMenu(
      selectInput(
        inputId = "mes_",
        label = "Mes:",
        choices = meses_lista,
        selected = 'enero',
        size = 13,
        selectize = FALSE
      ),
      actionLink("remove", "Eliminar pestanas detalladas")
   )
  ),
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Dashboard principal",
        value = "page1",
        fluidRow(
          valueBoxOutput("venta_total_box"),
          valueBoxOutput('promedio_x_dia'),
          valueBoxOutput('dia_menos_vendido')),
        fluidRow(),
        fluidRow(
          column(
            width = 6,
            d3Output("venta_total_grafico")
          ),
          column(
            width = 6,
            d3Output("venta_x_cat"))
          
    )
   )
  )
 )
)

server <- function(input, output, session) {
  tab_lista <- NULL
#Funcion reactiva
  base <- reactive({
    res <- resumen %>%
      filter(Mes == input$mes_)
  })
  

#Primer box, ventas totales por mes
  output$venta_total_box <- renderValueBox({

   base()%>%
      summarize(venta = sum(venta))%>%
      select(venta)%>%
      unlist()%>%
      as.vector()%>%
      as.integer() %>%
    prettyNum(big.mark = ".", decimal.mark = ",") %>%
      valueBox(subtitle = "Ventas total", color = 'blue')
  })
#Segundo box, venta promedio por dia
  output$promedio_x_dia <- renderValueBox({
    
    ((base()%>%
      summarize(venta = sum(venta))%>%
      select(venta)%>%
      unlist()%>%
      as.vector()%>%
      as.integer()) /
    (base()%>%
        group_by(Fecha)%>%
        summarize( count = 1)%>%
        ungroup()%>%
        mutate(dias_en_mes = sum(count))%>%
        select(dias_en_mes)%>%
        unlist()%>%
        as.vector()%>%
        head(1)%>%
        as.integer()))%>%
        prettyNum(big.mark = ".", decimal.mark = ",") %>%
        valueBox(subtitle = "Promedio de ventas por dia")
      
      
    
    
  })
#Dia de semana que menos se vendio (acumulado)en mes seleccionado. 
  output$dia_menos_vendido <- renderValueBox({
    
      base() %>%
      filter(Dia != 'domingo')%>%
      group_by(Dia)%>%
      summarize(ventas = sum(venta), count = length(unique(Fecha)))%>%
      mutate(prom_ventas = ventas/count)%>%
      arrange(desc(prom_ventas))%>%
      select(Dia)%>%
      unlist()%>%
      as.vector()%>%
      tail(1)%>%
      valueBox(subtitle = 'Dia menos vendido', color = 'red')
    
    
    
  })
#Primer grafico, grafico en barras de venta por dia.
  output$venta_total_grafico <- renderD3({
    
    venta_grafico <- base()%>%
      group_by(Fecha)%>%
      summarize(Venta = ifelse(nchar(round(sum(venta))) > 5,as.numeric(substr(round(sum(venta)),1,3)),
                as.numeric(substr(round(sum(venta)),1,2))))
    
    
    
    names(venta_grafico) <- c('x','y')
    
    venta_grafico$x <- substr(venta_grafico$x,9,11)
    
    venta_grafico$label <- venta_grafico$x
    

    r2d3(venta_grafico,script = "col_plot.js")
    
    
  })
#segundo grafico, ventas mensuales de cada categoria
  output$venta_x_cat <- renderD3({
    
    cat_grafico <- base()%>%
      group_by(Departamento)%>%
      summarize(Venta = ifelse(nchar(round(sum(venta))) > 5,as.numeric(substr(round(sum(venta)),1,3)),
                               as.numeric(substr(round(sum(venta)),1,2))))%>%
      arrange(desc(Venta))
    
    
    
    names(cat_grafico) <- c('x','y')
  
    cat_grafico$label <- cat_grafico$x
    
    
    r2d3(cat_grafico,script = "bar_plot.js")
    
    
  })
#En caso se seleccionar una barra en primer grafico, abrir el detalle como nueva tab
  observeEvent(input$column_clicked != "", {
      label_id_event <- input$column_clicked
      mes <- input$mes_
      tab_titulo <- paste(
        input$mes_, "-", label_id_event, "-", 'ventas'
      )
      if (!(tab_titulo %in% tab_lista)) {
      appendTab(
          inputId = "tabs",
          tabPanel(
            tab_titulo,
            DT::renderDataTable(
              venta_dia <-  base() %>%
                            filter(label_id == label_id_event)%>%
                            group_by(label_id,Codigo,Descripcion)%>%
                            summarize(Venta_total = sum(venta), Cant_total = sum(Cantidad))%>%
                            arrange(desc(Venta_total))
                            
            )
          )
        )
        
        tab_lista <<- c(tab_lista, tab_titulo)
      }

      updateTabsetPanel(session, "tabs", selected = tab_titulo)
    }
   ,
  ignoreInit = TRUE
  )
  
  #En caso se seleccionar una barra en segundo grafico, abrir el detalle como nueva tab  
  observeEvent(input$bar_clicked, {
    mes <- input$mes_
    cat_sel <- input$bar_clicked
    tab_titulo <- paste(mes, "-", cat_sel)
    if (!(tab_titulo %in% tab_lista)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_titulo,
          DT::renderDataTable(
            base() %>%
              filter(Departamento == input$bar_clicked)%>%
              group_by(Codigo,Descripcion)%>%
              summarize(venta_total = sum(venta), cant_total = sum(Cantidad))%>%
              arrange(desc(venta_total))
          )
        )
      )
      
      tab_lista <<- c(tab_lista, tab_titulo)
    }
    updateTabsetPanel(session, "tabs", selected = tab_titulo)
  }
  )
  
#borrar los tabs de detalles abiertos
  observeEvent(input$remove, {

    tab_lista %>%
      walk(~ removeTab("tabs", .x))
    tab_lista <<- NULL
  })
}


shinyApp(ui, server)


