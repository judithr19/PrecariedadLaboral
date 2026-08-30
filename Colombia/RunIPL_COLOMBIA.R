#______________________________#
# Indice de Precariedad Laboral
#______________________________#
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documentos - MacBook Pro de Apple/Colaboraciones/Dolly/PrecariedadLaboral/Programas/Colombia")
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

# Archivo
ruta <- "GEIH_2025.xlsx"

# Hojas a leer
hojas <- c("Enero", "Febrero","Marzo","Abril","Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Lectura, renombrado, selección y unión
datos <- map_dfr(
  hojas,
  ~ read_excel(ruta, sheet = .x) %>%
    # renombrar a los nombres solicitados (solo si existen)
    rename(
      horas      = P6800,
      ingreso    = INGLABO,
      ss         = P6424S5,
      formalidad = P6920
    ) %>%
    # quedarnos únicamente con las columnas en el orden pedido
    select(ingreso, formalidad, ss, horas)
)

# Vista rápida
dplyr::glimpse(datos)

# Limpieza de los datos: SS
datos <- datos %>%
  # Asegurar que ss es entero (si venía como texto)
  mutate(ss = suppressWarnings(as.integer(ss))) %>%
  # 1) eliminar ss == 0
  filter(!is.na(ss), ss != 0) %>%
  # 2) recodificar 1,2,3 -> 0 ; 4,5 -> 1
  mutate(
    ss = case_when(
      ss %in% 1L ~ 0L,
      ss %in% 2L ~ 1L,
      TRUE                  ~ NA_integer_  # por si aparece algún otro código inesperado
    )
  ) %>%
  # opcional: quitar cualquier ss no mapeado (NA)
  filter(!is.na(ss)) %>%
  # mantener el orden pedido
  select(ingreso, formalidad, ss, horas)

# Formalidad
datos <- datos %>%
  # Asegurar que formalidad sea entero por si viene como texto
  mutate(formalidad = suppressWarnings(as.integer(formalidad))) %>%
  # Recodificación: 2 -> 0
  mutate(
    formalidad = case_when(
      formalidad == 1L ~ 0L,
      formalidad == 2L ~ 1L,
      TRUE             ~ NA_integer_   # por si aparece algo inesperado
    )
  ) %>%
  # Opcional: quitar valores no válidos
  filter(!is.na(formalidad)) %>%
  # mantener el orden de columnas
  select(ingreso, formalidad, ss, horas)

# Quitar ingreso 0
datos$ingreso <- suppressWarnings(as.numeric(datos$ingreso))
datos <- datos[!is.na(datos$ingreso) & datos$ingreso != 0, ]
datos <- datos[, c("ingreso", "formalidad", "ss", "horas")]

# Si el ingreso está dado MENSUAL lo pasamos a semanal
datosmensual<- datos$ingreso
ingsemanal<-datos$ingreso* 12 / 52
datos$ingreso<-ingsemanal

# Datos particulares de cada país
# Salario mínimo mensual: ≈ $1,750,905 COP; Salario mínimo diario: ≈ $58,363 COP
# https://www.salariominimocolombia.net/?utm_source=chatgpt.com
ingsem<-1750905/210*42 # Mensual/horas al mes(42 semanal) 408000 

#___________________________________________#
#names(datos) # checar los nombres de las columnas de mis datos

# Ingresa los siguientes datos
ingreso              <- 355875#290000#325000#355875#ingsem 
numero_horas_ideales <- 44 # en 2026 son 42 horas
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
class(resultado)
round(resultado,4)

