#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(haven)
library(knitr)
library(ggthemes)
library(rgdal)
library(broom)
library(openxlsx)
library(mapview)
library(inspectdf)
library(leaflet)
library(DT)
library(summarytools)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Aplicacion del muestreo bietapico MAS MAS"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type
            textInput(inputId = "caption",
                      label = "Titulo:",
                      value = "Resumen de la variable cuantitativa continua del CENAGRO"),
            
            selectInput("estrato", label = "Estrato: Piso altitudinal",
                        choices = c("1: Costa o Chala", 
                                    "2: Yunga Fluvial",
                                    "3: Quechua",
                                    "4: Suni",
                                    "5: Puna",
                                    "6: Janca",
                                    "7: Rupa Rupa (Selva alta)",
                                    "8: Omagua (Selva baja)",
                                    "9: Yunga Maritima"), selected = "1: Costa o Chala"),
            
            selectInput("variable", label = "Variable a estudiar",
                        choices = c("¿Cuál es la superficie total de todas las parcelas o chacras que trabaja o conduce en este distrito?", 
                                    "Superficie agrícola o Superficie de tierras de cultivo (has) - WSup03",
                                    "Superficie agrícola o Superficie de tierras de cultivo (has) bajo riego - WSup03a",
                                    "Superficie agrícola o Superficie de tierras de cultivo (has) bajo secano - Wsup03b",
                                    "Superficie no agrícola (has) - WSup04",
                                    "Otra clase de tierras (has) - WSup05", 
                                    "Tierras de labranza (has) - WSup06",
                                    "Tierras con cultivos transitorios (has) - WSup07",
                                    "Tierras en barbecho (has) - WSup08", 
                                    "Tierras en descanso (has) - WSup09",
                                    "Tierras con cultivos permanentes (has) - WSup10",
                                    "Pastos cultivados (has) - WSup11", 
                                    "Cultivos forestales (has) - WSup12",
                                    "Tierras con cultivos asociados (has) - WSup13",
                                    "Tierras con pastos naturales (has) - WSup14", 
                                    "Pastos manejados (has) - WSup15", 
                                    "Pastos no manejados (has) - WSup16", 
                                    "Tierras con montes y bosques (has) - WSup17",
                                    "Superficie cultivada (has) - WSup18" ), 
                        selected = "Tierras con pastos naturales (has) - WSup14"),
            
            selectInput("nivel", label = "Nivel geografico",
                        choices = c("Departamento", "Provincia", "Distrito"), selected = "Departamento"),
            
            numericInput("a0", label = "Coeficiente de variacion fijado: ",
                         min = 0.01, max = 0.2, value = 0.1, step = 0.01),
            
            numericInput("alpha", label = "Nivel de significancia: ",
                         min = 0.05, max = 0.1, value = 0.05, step = 0.05),
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Escoge una base de datos:",
                        choices = c("1 - base de datos", 
                                    "2 - conglomerados iniciales",
                                    "3 - data por conglomerado", 
                                    "4 - conglomerados perdidos", 
                                    "5 - data por elemento", 
                                    "6 - primera etapa", 
                                    "7 - segunda etapa")),
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3(textOutput("caption", container = span)),

            # coef var en todas las variables
            DT::dataTableOutput("CoefVar"),            
                        
            #
            DT::dataTableOutput("conclusiones"),
            
            #
            DT::dataTableOutput("preliminares"),
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            DT::dataTableOutput("tabla"),
            
            #
            plotOutput("grafico"),
            
            #
            plotOutput("mapa"),
            
            #
            leafletOutput("geografia"),
            
            #
            plotOutput("mapa2"),
            
            #
            leafletOutput("geografia2")
            
        )
    )
)

