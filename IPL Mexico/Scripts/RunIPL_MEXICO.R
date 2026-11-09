#______________________________#
# Indice de Precariedad Laboral
#______________________________#
# Programa que tiene el cálculo del índice
source("IPL.R")

#______________________________#
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(purrr)
library(readr)
library(fs)
library(tibble)

# Año
anio<- 2024

# Archivo
ruta <- "2024_Mexico.xlsx"

# Hojas a leer
hojas <- c("Sheet 1")

# Lectura, renombrado, selección y unión
datos <- map_dfr(
  hojas,
  ~ read_excel(ruta, sheet = .x) %>%
    # renombrar a los nombres solicitados (solo si existen)
    rename(
      horas      = horas,
      ingreso    = ingreso,
      ss         = ss,
      formalidad = formalidad
    ) %>%
    # quedarnos únicamente con las columnas en el orden pedido
    select(ingreso, formalidad, ss, horas)
)

# Vista rápida
dplyr::glimpse(datos)


#___________________________________________#
# salario mínimo  y número de horas
obtener_ideal <- function(anio){
  
  switch(as.character(anio),
         "2023" = list(ingreso = 1452, numero_horas_ideales = 48),
         "2024" = list(ingreso = 1742, numero_horas_ideales = 48),
         "2025" = list(ingreso = 1951, numero_horas_ideales = 48),
         stop("Año no disponible")
  )
}

# Ingresa los siguientes datos
ingreso              <- obtener_ideal(anio)$ingreso #ideal_ing 
numero_horas_ideales <- obtener_ideal(anio)$numero_horas_ideales #ideal_hrs
alpha_op             <- 2 # dejar este valor fijo

#_____________________________________#
#_____________________________________#
#_____________________________________#
# Corremos la función que está en IPL.R
resultado<-indice_individual(
  ing = datos$ingreso,
  f   = datos$formalidad,
  ss  = datos$ss,
  h   = datos$horas,
  ingreso_ref      = ingreso,
  horas_ideal      = numero_horas_ideales,
  alpha            = alpha_op
)

round(resultado,4)



