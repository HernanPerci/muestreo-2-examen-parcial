#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic to summarize and view selected dataset ----

server <- function(input, output) {
    
    # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time
    
    # base de datos CENAGRO
    
    bd <- reactive({
      
      bd <- list(NULL)
      
      for (i in c(1:25)) {
        
        bd[[i]] <- read_sav(paste(336 + i, "01_IVCENAGRO_REC01.sav", sep = "-"))
        
      }
      
      base_de_datos <-
        bd %>%
        bind_rows() 
      
      base_de_datos
      
    })

    # nivel dep, prov y dist
  
    niveles <- reactive({
        
        if (input$nivel == "Departamento") {
            
            data_nivel <- 
                readOGR("D:/R-Perci/muestreo-2-examen-parcial/ejemplo3/coordenadas/DEPARTAMENTOS.shp")
            data_nivel
        } 
        else if (input$nivel == "Provincia") {
            
            data_nivel <- 
                readOGR("D:/R-Perci/muestreo-2-examen-parcial/ejemplo3/coordenadas/PROVINCIAS.shp")
            data_nivel
        } 
        else {
            
            data_nivel <-
                readOGR("D:/R-Perci/muestreo-2-examen-parcial/ejemplo3/coordenadas/DISTRITOS.shp")
            data_nivel
        }            
        
    })

    # coef var de todas las variables
    
    TablaCoefVar <- reactive({
      
      tabla <- list(NULL)
      i <- 0
      
      for (var in c("P020_01","WSUP03","WSUP03A","WSUP03B","WSUP04",
                    "WSUP05","WSUP06","WSUP07","WSUP08","WSUP09",
                    "WSUP10","WSUP11","WSUP12","WSUP13","WSUP14",
                    "WSUP15","WSUP16","WSUP17","WSUP18")) {
        
        i <- i+1
        
        est <- switch(input$estrato,
                      "1: Costa o Chala" = 1, 
                      "2: Yunga Fluvial" = 2,
                      "3: Quechua" = 3,
                      "4: Suni" = 4,
                      "5: Puna" = 5,
                      "6: Janca" = 6,
                      "7: Rupa Rupa (Selva alta)" = 7,
                      "8: Omagua (Selva baja)" = 8,
                      "9: Yunga Maritima" = 9)
        
        base_de_datos <-
          bd() %>%
          filter(WPISO == est)        
        
        base_de_datos <-
          base_de_datos %>%
          mutate(
            Departamento = base_de_datos$P001,
            Provincia = paste(base_de_datos$P001,
                              base_de_datos$P002,
                              sep = ""),
            Distrito = paste(base_de_datos$P001,
                             base_de_datos$P002, base_de_datos$P003,
                             sep = "")) %>%
          select(input$nivel, yij = var) %>%
          filter(yij > 0)
        
        ##
        
        conglomerados_iniciales <-
          base_de_datos %>%
          select(input$nivel, yij) %>%
          nest(Data_Poblacion = -input$nivel) %>%
          mutate(Mi = map(Data_Poblacion, ~as.numeric(count(.))),
                 Yi = map(Data_Poblacion, ~sum(.)), 
                 Yij_prom = map(Data_Poblacion, ~mean(unlist(.)))) %>%
          mutate(Mi = as.integer(Mi), Yi = as.double(Yi), 
                 Yij_prom = as.double(Yij_prom))
        
        tamanio_muestra_2da_etapa <- function(data) {
          tamanio <- (nrow(data) * var(data$yij)) / 
            (((input$a0^2) * (mean(data$yij)^2) * nrow(data)) + var(data$yij))
          tamanio <- ceiling(tamanio)
          tamanio
        }
        
        conglomerados_iniciales <-
          conglomerados_iniciales %>%
          mutate(mi = map(Data_Poblacion, ~tamanio_muestra_2da_etapa(data = .))) %>%
          mutate(mi = as.integer(mi))
        
        ##
        
        data_por_conglomerado <-
          conglomerados_iniciales %>%
          filter((Mi > 1) & (mi <= Mi) & (mi > 1))
        
        N <- nrow(data_por_conglomerado)
        Y <- sum(data_por_conglomerado$Yi)
        Y_raya <- mean(data_por_conglomerado$Yi)
        S2_b <- var(data_por_conglomerado$Yi)
        
        data_por_conglomerado <-
          data_por_conglomerado %>%
          mutate(S2wi = map(Data_Poblacion, ~ var(x = .))) %>%
          mutate(S2wi = as.double(S2wi))
        
        data_por_conglomerado <-
          data_por_conglomerado %>%
          mutate(pob_num_segunda_componente = 
                   N * (Mi^2) * ((1 / mi) - (1 / Mi)) * S2wi)
        
        tamanio_muestra_1ra_etapa <- function(data) {
          
          numerador <- ((N^2) * S2_b) + sum(data$pob_num_segunda_componente)
          denominador <- ((input$a0 * Y)^2) + (N * S2_b)
          tamanio <- numerador / denominador
          tamanio <- ceiling(tamanio)
          tamanio
        }
        
        n <- tamanio_muestra_1ra_etapa(data = data_por_conglomerado)
        
        tabla[[i]] <- tibble("variable" = var,
                             "CV" = sqrt((N - 1) * S2_b / N) / Y_raya)
        
      }
      
      tabla_general <-
        tabla %>%
        bind_rows() 
      
      tabla_general
      
    })
    
        
    # conglomerados iniciales 
    
    iniciales <- reactive({

       est <- switch(input$estrato,
                     "1: Costa o Chala" = 1, 
                     "2: Yunga Fluvial" = 2,
                     "3: Quechua" = 3,
                     "4: Suni" = 4,
                     "5: Puna" = 5,
                     "6: Janca" = 6,
                     "7: Rupa Rupa (Selva alta)" = 7,
                     "8: Omagua (Selva baja)" = 8,
                     "9: Yunga Maritima" = 9)
       
        var <- switch(input$variable,
                      "¿Cuál es la superficie total de todas las parcelas o chacras que trabaja o conduce en este distrito?" = "P020_01", 
                      "Superficie agrícola o Superficie de tierras de cultivo (has) - WSup03" = "WSUP03",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo riego - WSup03a" = "WSUP03A",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo secano - Wsup03b" = "WSUP03B",
                      "Superficie no agrícola (has) - WSup04" = "WSUP04",
                      "Otra clase de tierras (has) - WSup05" = "WSUP05", 
                      "Tierras de labranza (has) - WSup06" = "WSUP06",
                      "Tierras con cultivos transitorios (has) - WSup07" = "WSUP07",
                      "Tierras en barbecho (has) - WSup08" = "WSUP08", 
                      "Tierras en descanso (has) - WSup09" = "WSUP09",
                      "Tierras con cultivos permanentes (has) - WSup10" = "WSUP10",
                      "Pastos cultivados (has) - WSup11" = "WSUP11", 
                      "Cultivos forestales (has) - WSup12" = "WSUP12",
                      "Tierras con cultivos asociados (has) - WSup13" = "WSUP13",
                      "Tierras con pastos naturales (has) - WSup14" = "WSUP14",
                      "Pastos manejados (has) - WSup15" = "WSUP15", 
                      "Pastos no manejados (has) - WSup16" = "WSUP16", 
                      "Tierras con montes y bosques (has) - WSup17" = "WSUP17",
                      "Superficie cultivada (has) - WSup18" = "WSUP18")
        
        base_de_datos <-
          bd() %>%
          filter(WPISO == est)
        
        base_de_datos <-
            base_de_datos %>%
            mutate(
                Departamento = base_de_datos$P001,
                Provincia = paste(base_de_datos$P001,
                                  base_de_datos$P002,
                                  sep = ""),
                Distrito = paste(base_de_datos$P001,
                                 base_de_datos$P002, base_de_datos$P003,
                                 sep = "")) %>%
            select(input$nivel, yij = var) %>%
            filter(yij > 0)
        
        conglomerados_iniciales <-
            base_de_datos %>%
            select(input$nivel, yij) %>%
            nest(Data_Poblacion = -input$nivel) %>%
            mutate(Mi = map(Data_Poblacion, ~as.numeric(count(.))),
                   Yi = map(Data_Poblacion, ~sum(.)), 
                   Yij_prom = map(Data_Poblacion, ~mean(unlist(.)))) %>%
            mutate(Mi = as.integer(Mi), Yi = as.double(Yi), 
                   Yij_prom = as.double(Yij_prom))
        
        tamanio_muestra_2da_etapa <- function(data) {
            tamanio <- (nrow(data) * var(data$yij)) / 
                (((input$a0^2) * (mean(data$yij)^2) * nrow(data)) + var(data$yij))
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        conglomerados_iniciales <-
            conglomerados_iniciales %>%
            mutate(mi = map(Data_Poblacion, ~tamanio_muestra_2da_etapa(data = .))) %>%
            mutate(mi = as.integer(mi))
        
        conglomerados_iniciales
        
    })

    # elegir base de datos    mas mas
    
    datasetInput <- reactive({

      est <- switch(input$estrato,
                    "1: Costa o Chala" = 1, 
                    "2: Yunga Fluvial" = 2,
                    "3: Quechua" = 3,
                    "4: Suni" = 4,
                    "5: Puna" = 5,
                    "6: Janca" = 6,
                    "7: Rupa Rupa (Selva alta)" = 7,
                    "8: Omagua (Selva baja)" = 8,
                    "9: Yunga Maritima" = 9)
                    
        var <- switch(input$variable,
                      "¿Cuál es la superficie total de todas las parcelas o chacras que trabaja o conduce en este distrito?" = "P020_01", 
                      "Superficie agrícola o Superficie de tierras de cultivo (has) - WSup03" = "WSUP03",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo riego - WSup03a" = "WSUP03A",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo secano - Wsup03b" = "WSUP03B",
                      "Superficie no agrícola (has) - WSup04" = "WSUP04",
                      "Otra clase de tierras (has) - WSup05" = "WSUP05", 
                      "Tierras de labranza (has) - WSup06" = "WSUP06",
                      "Tierras con cultivos transitorios (has) - WSup07" = "WSUP07",
                      "Tierras en barbecho (has) - WSup08" = "WSUP08", 
                      "Tierras en descanso (has) - WSup09" = "WSUP09",
                      "Tierras con cultivos permanentes (has) - WSup10" = "WSUP10",
                      "Pastos cultivados (has) - WSup11" = "WSUP11", 
                      "Cultivos forestales (has) - WSup12" = "WSUP12",
                      "Tierras con cultivos asociados (has) - WSup13" = "WSUP13",
                      "Tierras con pastos naturales (has) - WSup14" = "WSUP14",
                      "Pastos manejados (has) - WSup15" = "WSUP15", 
                      "Pastos no manejados (has) - WSup16" = "WSUP16", 
                      "Tierras con montes y bosques (has) - WSup17" = "WSUP17",
                      "Superficie cultivada (has) - WSup18" = "WSUP18")
        
        base_de_datos <-
          bd() %>%
          filter(WPISO == est)        
                
        base_de_datos <-
            base_de_datos %>%
            mutate(
                Departamento = base_de_datos$P001,
                Provincia = paste(base_de_datos$P001,
                                  base_de_datos$P002,
                                  sep = ""),
                Distrito = paste(base_de_datos$P001,
                                 base_de_datos$P002, base_de_datos$P003,
                                 sep = "")) %>%
            select(input$nivel, yij = var) %>%
            filter(yij > 0)
        
        ##
        
        conglomerados_iniciales <-
            base_de_datos %>%
            select(input$nivel, yij) %>%
            nest(Data_Poblacion = -input$nivel) %>%
            mutate(Mi = map(Data_Poblacion, ~as.numeric(count(.))),
                   Yi = map(Data_Poblacion, ~sum(.)), 
                   Yij_prom = map(Data_Poblacion, ~mean(unlist(.)))) %>%
            mutate(Mi = as.integer(Mi), Yi = as.double(Yi), 
                   Yij_prom = as.double(Yij_prom))
        
        tamanio_muestra_2da_etapa <- function(data) {
            tamanio <- (nrow(data) * var(data$yij)) / 
                (((input$a0^2) * (mean(data$yij)^2) * nrow(data)) + var(data$yij))
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        conglomerados_iniciales <-
            conglomerados_iniciales %>%
            mutate(mi = map(Data_Poblacion, ~tamanio_muestra_2da_etapa(data = .))) %>%
            mutate(mi = as.integer(mi))
        
        ##
        
        data_por_conglomerado <-
            conglomerados_iniciales %>%
            filter((Mi > 1) & (mi <= Mi) & (mi > 1))
        
        N <- nrow(data_por_conglomerado)
        Y <- sum(data_por_conglomerado$Yi)
        Y_raya <- mean(data_por_conglomerado$Yi)
        S2_b <- var(data_por_conglomerado$Yi)
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(S2wi = map(Data_Poblacion, ~ var(x = .))) %>%
            mutate(S2wi = as.double(S2wi))
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(pob_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * S2wi)
        
        tamanio_muestra_1ra_etapa <- function(data) {
            
            numerador <- ((N^2) * S2_b) + sum(data$pob_num_segunda_componente)
            denominador <- ((input$a0 * Y)^2) + (N * S2_b)
            tamanio <- numerador / denominador
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        n <- tamanio_muestra_1ra_etapa(data = data_por_conglomerado)
        
        var_Y_est <- ((N^2) * ((1 / n) - (1 / N)) * S2_b) + 
            (sum(data_por_conglomerado$pob_num_segunda_componente) / n)
        
        ##
        
        conglomerados_perdidos <-
            conglomerados_iniciales %>%
            filter((Mi <= 1) | (mi > Mi) | (mi <= 1))
        
        data_por_elemento <- 
            data_por_conglomerado %>%
            unnest(-input$nivel) %>%
            select(input$nivel, yij)
        
        M0 <- nrow(data_por_elemento)
        Y_raya_elemento <- mean(data_por_elemento$yij)
        
        set.seed(123)
        primera_etapa <- 
            data_por_conglomerado %>%
            sample_n(size = n, replace = FALSE)
        
        ##
        
        segunda_etapa <-
            primera_etapa
        
        muestra_2da_etapa <- function() {
            
            Data_Muestra <- list()  
            for (i in 1:nrow(segunda_etapa)) {
                mi <- segunda_etapa[[6]][i]
                set.seed(123)
                Data_Muestra[[i]] <- map(segunda_etapa[[2]][[i]], 
                                         sample, size = mi)
            }
            Data_Muestra <- tibble(Data_Muestra)
        }
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(muestra_2da_etapa())
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(yi = map(Data_Muestra, ~sum(unlist(.))), 
                   yi_raya = map(Data_Muestra, ~mean(unlist(.)))) %>%
            mutate(yi = as.double(yi), yi_raya = as.double(yi_raya))
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(s2wi = map(Data_Muestra, ~ var(x = unlist(.))),
                   Mi_por_yi_raya = Mi * yi_raya) %>%
            mutate(s2wi = as.double(s2wi))
        
        s2_b <- var(segunda_etapa$Mi_por_yi_raya)
        
        Y_est <- (sum(segunda_etapa$Mi_por_yi_raya) * N) / n
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(mues_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * s2wi)
        
        switch(input$dataset,
               "1 - base de datos" = base_de_datos,
               "2 - conglomerados iniciales" = conglomerados_iniciales, 
               "3 - data por conglomerado" = data_por_conglomerado, 
               "4 - conglomerados perdidos" = conglomerados_perdidos, 
               "5 - data por elemento" = data_por_elemento,
               "6 - primera etapa" = primera_etapa, 
               "7 - segunda etapa" = segunda_etapa)
    })

    # resumen final    mas mas
    
    datasetInput2 <- reactive({

      est <- switch(input$estrato,
                    "1: Costa o Chala" = 1, 
                    "2: Yunga Fluvial" = 2,
                    "3: Quechua" = 3,
                    "4: Suni" = 4,
                    "5: Puna" = 5,
                    "6: Janca" = 6,
                    "7: Rupa Rupa (Selva alta)" = 7,
                    "8: Omagua (Selva baja)" = 8,
                    "9: Yunga Maritima" = 9)
              
        var <- switch(input$variable,
                      "¿Cuál es la superficie total de todas las parcelas o chacras que trabaja o conduce en este distrito?" = "P020_01", 
                      "Superficie agrícola o Superficie de tierras de cultivo (has) - WSup03" = "WSUP03",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo riego - WSup03a" = "WSUP03A",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo secano - Wsup03b" = "WSUP03B",
                      "Superficie no agrícola (has) - WSup04" = "WSUP04",
                      "Otra clase de tierras (has) - WSup05" = "WSUP05", 
                      "Tierras de labranza (has) - WSup06" = "WSUP06",
                      "Tierras con cultivos transitorios (has) - WSup07" = "WSUP07",
                      "Tierras en barbecho (has) - WSup08" = "WSUP08", 
                      "Tierras en descanso (has) - WSup09" = "WSUP09",
                      "Tierras con cultivos permanentes (has) - WSup10" = "WSUP10",
                      "Pastos cultivados (has) - WSup11" = "WSUP11", 
                      "Cultivos forestales (has) - WSup12" = "WSUP12",
                      "Tierras con cultivos asociados (has) - WSup13" = "WSUP13",
                      "Tierras con pastos naturales (has) - WSup14" = "WSUP14",
                      "Pastos manejados (has) - WSup15" = "WSUP15", 
                      "Pastos no manejados (has) - WSup16" = "WSUP16", 
                      "Tierras con montes y bosques (has) - WSup17" = "WSUP17",
                      "Superficie cultivada (has) - WSup18" = "WSUP18")
        
        base_de_datos <-
          bd() %>%
          filter(WPISO == est)        
                
        base_de_datos <-
            base_de_datos %>%
            mutate(
                Departamento = base_de_datos$P001,
                Provincia = paste(base_de_datos$P001,
                                  base_de_datos$P002,
                                  sep = ""),
                Distrito = paste(base_de_datos$P001,
                                 base_de_datos$P002, base_de_datos$P003,
                                 sep = "")) %>%
            select(input$nivel, yij = var) %>%
            filter(yij > 0)
        
        ##
        
        conglomerados_iniciales <-
            base_de_datos %>%
            select(input$nivel, yij) %>%
            nest(Data_Poblacion = -input$nivel) %>%
            mutate(Mi = map(Data_Poblacion, ~as.numeric(count(.))),
                   Yi = map(Data_Poblacion, ~sum(.)), 
                   Yij_prom = map(Data_Poblacion, ~mean(unlist(.)))) %>%
            mutate(Mi = as.integer(Mi), Yi = as.double(Yi), 
                   Yij_prom = as.double(Yij_prom))
        
        tamanio_muestra_2da_etapa <- function(data) {
            tamanio <- (nrow(data) * var(data$yij)) / 
                (((input$a0^2) * (mean(data$yij)^2) * nrow(data)) + var(data$yij))
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        conglomerados_iniciales <-
            conglomerados_iniciales %>%
            mutate(mi = map(Data_Poblacion, ~tamanio_muestra_2da_etapa(data = .))) %>%
            mutate(mi = as.integer(mi))
        
        ##
        
        data_por_conglomerado <-
            conglomerados_iniciales %>%
            filter((Mi > 1) & (mi <= Mi) & (mi > 1))
        
        N <- nrow(data_por_conglomerado)
        Y <- sum(data_por_conglomerado$Yi)
        Y_raya <- mean(data_por_conglomerado$Yi)
        S2_b <- var(data_por_conglomerado$Yi)
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(S2wi = map(Data_Poblacion, ~ var(x = .))) %>%
            mutate(S2wi = as.double(S2wi))
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(pob_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * S2wi)
        
        tamanio_muestra_1ra_etapa <- function(data) {
            
            numerador <- ((N^2) * S2_b) + sum(data$pob_num_segunda_componente)
            denominador <- ((input$a0 * Y)^2) + (N * S2_b)
            tamanio <- numerador / denominador
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        n <- tamanio_muestra_1ra_etapa(data = data_por_conglomerado)
        
        var_Y_est <- ((N^2) * ((1 / n) - (1 / N)) * S2_b) + 
            (sum(data_por_conglomerado$pob_num_segunda_componente) / n)
        
        conglomerados_perdidos <-
            conglomerados_iniciales %>%
            filter((Mi <= 1) | (mi > Mi) | (mi <= 1))
        
        data_por_elemento <- 
            data_por_conglomerado %>%
            unnest(-input$nivel) %>%
            select(input$nivel, yij)
        
        M0 <- nrow(data_por_elemento)
        Y_raya_elemento <- mean(data_por_elemento$yij)
        
        set.seed(123)
        primera_etapa <- 
            data_por_conglomerado %>%
            sample_n(size = n, replace = FALSE)
        
        segunda_etapa <-
            primera_etapa
        
        muestra_2da_etapa <- function() {
            
            Data_Muestra <- list()  
            for (i in 1:nrow(segunda_etapa)) {
                mi <- segunda_etapa[[6]][i]
                set.seed(123)
                Data_Muestra[[i]] <- map(segunda_etapa[[2]][[i]], 
                                         sample, size = mi)
            }
            Data_Muestra <- tibble(Data_Muestra)
        }
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(muestra_2da_etapa())
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(yi = map(Data_Muestra, ~sum(unlist(.))), 
                   yi_raya = map(Data_Muestra, ~mean(unlist(.)))) %>%
            mutate(yi = as.double(yi), yi_raya = as.double(yi_raya))
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(s2wi = map(Data_Muestra, ~ var(x = unlist(.))),
                   Mi_por_yi_raya = Mi * yi_raya) %>%
            mutate(s2wi = as.double(s2wi))
        
        s2_b <- var(segunda_etapa$Mi_por_yi_raya)
        
        Y_est <- (sum(segunda_etapa$Mi_por_yi_raya) * N) / n
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(mues_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * s2wi)
        
        var_est_Y_est <- ((N^2) * ((1 / n) - (1 / N)) * s2_b) + 
            (sum(segunda_etapa$mues_num_segunda_componente) / n)
        
        z <- qnorm(1 - (input$alpha / 2),0,1)
        t <- qt(1 - (input$alpha / 2), n-1)
        
        IC_Y_inf <- 
            ifelse(n >= 30, 
                   Y_est - (z * sqrt(var_est_Y_est)), 
                   Y_est - (t * sqrt(var_est_Y_est)))
        
        IC_Y_sup <- 
            ifelse(n >= 30, 
                   Y_est + (z * sqrt(var_est_Y_est)), 
                   Y_est + (t * sqrt(var_est_Y_est)))
        
        IC_Y_raya_inf <- 
            ifelse(n >= 30, 
                   (Y_est / N) - (z * sqrt(var_est_Y_est / (N^2))), 
                   (Y_est / N) - (t * sqrt(var_est_Y_est / (N^2))))
        
        IC_Y_raya_sup <- 
            ifelse(n >= 30, 
                   (Y_est / N) + (z * sqrt(var_est_Y_est / (N^2))), 
                   (Y_est / N) + (t * sqrt(var_est_Y_est / (N^2))))
        
        cv_est <- sqrt(var_est_Y_est) / Y
        
        tibble("Parametros" = c("Total", "Promedio"),
              "variable" = c(var, var),
              "CV" = c(input$a0, input$a0),
              "Valores" = c(Y, Y_raya), 
              "Estimaciones" = c(Y_est, Y_est / N), 
              "IC inferior" = c(IC_Y_inf, IC_Y_raya_inf), 
              "IC superior" = c(IC_Y_sup, IC_Y_raya_sup), 
              "CV estimado" = c(cv_est, cv_est))
        
        
    })

    # resultados preliminares  mas mas
    
    datasetInput3 <- reactive({

      est <- switch(input$estrato,
                    "1: Costa o Chala" = 1, 
                    "2: Yunga Fluvial" = 2,
                    "3: Quechua" = 3,
                    "4: Suni" = 4,
                    "5: Puna" = 5,
                    "6: Janca" = 6,
                    "7: Rupa Rupa (Selva alta)" = 7,
                    "8: Omagua (Selva baja)" = 8,
                    "9: Yunga Maritima" = 9)
              
        var <- switch(input$variable,
                      "¿Cuál es la superficie total de todas las parcelas o chacras que trabaja o conduce en este distrito?" = "P020_01", 
                      "Superficie agrícola o Superficie de tierras de cultivo (has) - WSup03" = "WSUP03",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo riego - WSup03a" = "WSUP03A",
                      "Superficie agrícola o Superficie de tierras de cultivo (has) bajo secano - Wsup03b" = "WSUP03B",
                      "Superficie no agrícola (has) - WSup04" = "WSUP04",
                      "Otra clase de tierras (has) - WSup05" = "WSUP05", 
                      "Tierras de labranza (has) - WSup06" = "WSUP06",
                      "Tierras con cultivos transitorios (has) - WSup07" = "WSUP07",
                      "Tierras en barbecho (has) - WSup08" = "WSUP08", 
                      "Tierras en descanso (has) - WSup09" = "WSUP09",
                      "Tierras con cultivos permanentes (has) - WSup10" = "WSUP10",
                      "Pastos cultivados (has) - WSup11" = "WSUP11", 
                      "Cultivos forestales (has) - WSup12" = "WSUP12",
                      "Tierras con cultivos asociados (has) - WSup13" = "WSUP13",
                      "Tierras con pastos naturales (has) - WSup14" = "WSUP14",
                      "Pastos manejados (has) - WSup15" = "WSUP15", 
                      "Pastos no manejados (has) - WSup16" = "WSUP16", 
                      "Tierras con montes y bosques (has) - WSup17" = "WSUP17",
                      "Superficie cultivada (has) - WSup18" = "WSUP18")
        
        base_de_datos <-
          bd() %>%
          filter(WPISO == est)        
                
        base_de_datos <-
            base_de_datos %>%
            mutate(
                Departamento = base_de_datos$P001,
                Provincia = paste(base_de_datos$P001,
                                  base_de_datos$P002,
                                  sep = ""),
                Distrito = paste(base_de_datos$P001,
                                 base_de_datos$P002, base_de_datos$P003,
                                 sep = "")) %>%
            select(input$nivel, yij = var) %>%
            filter(yij > 0)
        
        ##
        
        conglomerados_iniciales <-
            base_de_datos %>%
            select(input$nivel, yij) %>%
            nest(Data_Poblacion = -input$nivel) %>%
            mutate(Mi = map(Data_Poblacion, ~as.numeric(count(.))),
                   Yi = map(Data_Poblacion, ~sum(.)), 
                   Yij_prom = map(Data_Poblacion, ~mean(unlist(.)))) %>%
            mutate(Mi = as.integer(Mi), Yi = as.double(Yi), 
                   Yij_prom = as.double(Yij_prom))
        
        tamanio_muestra_2da_etapa <- function(data) {
            tamanio <- (nrow(data) * var(data$yij)) / 
                (((input$a0^2) * (mean(data$yij)^2) * nrow(data)) + var(data$yij))
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        conglomerados_iniciales <-
            conglomerados_iniciales %>%
            mutate(mi = map(Data_Poblacion, ~tamanio_muestra_2da_etapa(data = .))) %>%
            mutate(mi = as.integer(mi))
        
        ##
        
        data_por_conglomerado <-
            conglomerados_iniciales %>%
            filter((Mi > 1) & (mi <= Mi) & (mi > 1))
        
        N <- nrow(data_por_conglomerado)
        Y <- sum(data_por_conglomerado$Yi)
        Y_raya <- mean(data_por_conglomerado$Yi)
        S2_b <- var(data_por_conglomerado$Yi)
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(S2wi = map(Data_Poblacion, ~ var(x = .))) %>%
            mutate(S2wi = as.double(S2wi))
        
        data_por_conglomerado <-
            data_por_conglomerado %>%
            mutate(pob_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * S2wi)
        
        tamanio_muestra_1ra_etapa <- function(data) {
            
            numerador <- ((N^2) * S2_b) + sum(data$pob_num_segunda_componente)
            denominador <- ((input$a0 * Y)^2) + (N * S2_b)
            tamanio <- numerador / denominador
            tamanio <- ceiling(tamanio)
            tamanio
        }
        
        n <- tamanio_muestra_1ra_etapa(data = data_por_conglomerado)
        
        var_Y_est <- ((N^2) * ((1 / n) - (1 / N)) * S2_b) + 
            (sum(data_por_conglomerado$pob_num_segunda_componente) / n)
        
        conglomerados_perdidos <-
            conglomerados_iniciales %>%
            filter((Mi <= 1) | (mi > Mi) | (mi <= 1))
        
        data_por_elemento <- 
            data_por_conglomerado %>%
            unnest(-input$nivel) %>%
            select(input$nivel, yij)
        
        M0 <- nrow(data_por_elemento)
        Y_raya_elemento <- mean(data_por_elemento$yij)
        
        set.seed(123)
        primera_etapa <- 
            data_por_conglomerado %>%
            sample_n(size = n, replace = FALSE)
        
        segunda_etapa <-
            primera_etapa
        
        muestra_2da_etapa <- function() {
            
            Data_Muestra <- list()  
            for (i in 1:nrow(segunda_etapa)) {
                mi <- segunda_etapa[[6]][i]
                set.seed(123)
                Data_Muestra[[i]] <- map(segunda_etapa[[2]][[i]], 
                                         sample, size = mi)
            }
            Data_Muestra <- tibble(Data_Muestra)
        }
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(muestra_2da_etapa())
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(yi = map(Data_Muestra, ~sum(unlist(.))), 
                   yi_raya = map(Data_Muestra, ~mean(unlist(.)))) %>%
            mutate(yi = as.double(yi), yi_raya = as.double(yi_raya))
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(s2wi = map(Data_Muestra, ~ var(x = unlist(.))),
                   Mi_por_yi_raya = Mi * yi_raya) %>%
            mutate(s2wi = as.double(s2wi))
        
        s2_b <- var(segunda_etapa$Mi_por_yi_raya)
        
        Y_est <- (sum(segunda_etapa$Mi_por_yi_raya) * N) / n
        
        segunda_etapa <-
            segunda_etapa %>%
            mutate(mues_num_segunda_componente = 
                       N * (Mi^2) * ((1 / mi) - (1 / Mi)) * s2wi)
        
        var_est_Y_est <- ((N^2) * ((1 / n) - (1 / N)) * s2_b) + 
            (sum(segunda_etapa$mues_num_segunda_componente) / n)
        
        z <- qnorm(1 - (input$alpha / 2),0,1)
        t <- qt(1 - (input$alpha / 2), n-1)
        
        IC_Y_inf <- 
            ifelse(n >= 30, 
                   Y_est - (z * sqrt(var_est_Y_est)), 
                   Y_est - (t * sqrt(var_est_Y_est)))
        
        IC_Y_sup <- 
            ifelse(n >= 30, 
                   Y_est + (z * sqrt(var_est_Y_est)), 
                   Y_est + (t * sqrt(var_est_Y_est)))
        
        IC_Y_raya_inf <- 
            ifelse(n >= 30, 
                   (Y_est / N) - (z * sqrt(var_est_Y_est / (N^2))), 
                   (Y_est / N) - (t * sqrt(var_est_Y_est / (N^2))))
        
        IC_Y_raya_sup <- 
            ifelse(n >= 30, 
                   (Y_est / N) + (z * sqrt(var_est_Y_est / (N^2))), 
                   (Y_est / N) + (t * sqrt(var_est_Y_est / (N^2))))
        
        cv_est <- sqrt(var_est_Y_est) / Y
        
        tibble("N" = N,
               "n" = n,
               "Media por elemento" = Y_raya_elemento, 
               "M0" = M0, 
               "S2b " = S2_b, 
               "s2b" = s2_b, 
               "z" = z,
               "t" = t)
        
    })    
    
        
    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    
    output$caption <- renderText({
        input$caption
    })

    # coef var de todas las variables
    
    output$CoefVar <- DT::renderDataTable({
      
      TablaCoefVar()
      
    })
    
    # resumen final
    
    output$conclusiones <- DT::renderDataTable({
        
        datasetInput2()

    })

    # resultados preliminares
    
    output$preliminares <- DT::renderDataTable({
        
        datasetInput3()
        
    })    
        
    # Generate a summary of the dataset ----
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated, i.e. whenever the input$dataset changes
    
    output$summary <- renderPrint({
        
        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            dataset <- datasetInput()
            dfSummary(dataset)
            
        } 
        else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
            dataset <- datasetInput()[-2]
            dfSummary(dataset)
            
            
        } 
        else {
            
            dataset <- datasetInput()[-c(2,9)]
            dfSummary(dataset)
            
            
        }

    })
    
    
    # tabla de base de datos elegida
    
    output$tabla <- DT::renderDataTable({
        
        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            datasetInput()

            
        } 
        else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
            datasetInput()[-2]

        } 
        else {
            
            datasetInput()[-c(2,9)]

        }
        
    })
    
    # graficos de variables cuantitativas
    
    output$grafico <- renderPlot({
        datasetInput() %>%
            inspect_num() %>%
            show_plot(col_palette = 2)
    })
    
    # mapa data 2, 3, 4 o 6
    
    output$mapa <- renderPlot({

        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            NULL
            
        } 
        else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
            if (input$nivel == "Departamento") {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDDPTO" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=Yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
                
            } 
            else if (input$nivel == "Provincia") {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDPROV" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=Yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
                
                
            } 
            else {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDDIST" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=Yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
            }
      
            
        } 
        else {
            
            NULL
            
        }
        
    })
    
    # mapa geog data 2, 3, 4 o 6
    
    output$geografia <- renderLeaflet({
        
        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            NULL
            
        } else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
        if (input$nivel == "Departamento") {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDDPTO" = input$nivel)) 
            max_val = max(abs(data_nivel@data$Yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "Yi", at= at_10)
            fig@map
            
            
        } 
        else if (input$nivel == "Provincia") {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDPROV" = input$nivel)) 
            max_val = max(abs(data_nivel@data$Yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "Yi", at= at_10)
            fig@map
            

            
            
        } 
        else {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDDIST" = input$nivel)) 
            max_val = max(abs(data_nivel@data$Yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "Yi", at= at_10)
            fig@map
            
        }    
            
            
        } else {
            
            NULL
            
        }
        
    })

    # mapa data 7 = segunda etapa
    
    output$mapa2 <- renderPlot({
        
        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            NULL
            
        } 
        else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
            NULL
            
        } 
        else {
            
            if (input$nivel == "Departamento") {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDDPTO" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
                
            } 
            else if (input$nivel == "Provincia") {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDPROV" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
                
                
            } 
            else {
                
                
                data <- tidy(niveles())
                
                nombres_data <- data.frame(niveles())
                
                nombres_data$id <- as.character(seq(0, nrow(nombres_data)-1))
                
                data_nivel_mapa <- left_join(data, nombres_data, by = "id")
                
                grafico <- left_join(data_nivel_mapa, datasetInput(), 
                                     by = c("IDDIST" = input$nivel))
                
                ggplot(grafico, aes(x=long, y= lat, group = group)) +
                    geom_polygon(aes(fill=yi), color= "white", size = 0.2) +
                    labs( title = paste("Totales de la variable: ", input$variable, 
                                        ". \nMapa a nivel de conglomerado: ", input$nivel),
                          subtitle = paste("Conglomerados: ", 
                                           nrow(datasetInput()), " de ", 
                                           nrow(iniciales())),
                          caption = "Fuente: IV CENAGRO INEI",
                          fill = "Hectareas (has)") +
                    theme_minimal() +
                    theme(
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        plot.background = element_rect(fill = "snow", color = NA),
                        panel.background = element_rect(fill= "snow", color = NA),
                        plot.title = element_text(size = 16, hjust = 0),
                        plot.subtitle = element_text(size = 12, hjust = 0),
                        plot.caption = element_text(size = 8, hjust = 1),
                        legend.title = element_text(color = "grey40", size = 8),
                        legend.text = element_text(color = "grey40", size = 7, hjust = 0),
                        legend.position = c(0.93, 0.63),
                        plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
                    scale_fill_viridis_c(option = "plasma")
                
            }
            
        }
        

    })
    
    # mapa geo data 7 = segunda etapa
    
    output$geografia2 <- renderLeaflet({
        
        if (input$dataset == "1 - base de datos" | 
            input$dataset == "5 - data por elemento") {
            
            NULL
            
        } 
        else if (input$dataset == "2 - conglomerados iniciales" | 
                   input$dataset == "3 - data por conglomerado" | 
                   input$dataset == "4 - conglomerados perdidos" | 
                   input$dataset == "6 - primera etapa") {
            
            NULL
            
        } 
        else {
            
        if (input$nivel == "Departamento") {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDDPTO" = input$nivel)) 
            max_val = max(abs(data_nivel@data$yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "yi", at= at_10)
            fig@map
            
            
        } 
        else if (input$nivel == "Provincia") {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDPROV" = input$nivel)) 
            max_val = max(abs(data_nivel@data$yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "yi", at= at_10)
            fig@map
            
            
            
            
        } 
        else {
            
            data_nivel <- niveles()
            data_nivel@data <- left_join(data_nivel@data, 
                                         datasetInput(), by = c("IDDIST" = input$nivel)) 
            max_val = max(abs(data_nivel@data$yi), na.rm = T)
            at_10 = lattice::do.breaks(endpoints = c(0, max_val), nint = 10)
            fig<-data_nivel %>%
                mapview::mapview(zcol = "yi", at= at_10)
            fig@map
            
        }   

        }
        
    })
    
}

