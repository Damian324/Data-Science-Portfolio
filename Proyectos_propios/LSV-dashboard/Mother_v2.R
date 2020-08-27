rm(list=ls())

####Reading,Wrangling,and Writing Data----

#SOLO CAMBIAR EL PATH RAW, EL RESTO ESTA AUTOMATIZADO

path_raw <- "C:/Users/Usuario/Desktop/Ventas/Agosto_20"

path_inventario <- 'C:/Users/Usuario/Desktop/Ventas/inventario/actualizado'

#-----

script_source <- 'C:/Users/Usuario/Desktop/Ventas/Scripts'

setwd(script_source)

source('Automatizacion_v3.R')

####Joining datasets----

source('Clusterizacion_v2.1.R')

####Run and publish App---

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

deployApp(forceUpdate = TRUE)

beep(4)
