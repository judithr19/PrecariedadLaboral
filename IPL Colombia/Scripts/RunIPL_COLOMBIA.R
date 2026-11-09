# --------------------------------------------------
# Índice de Precariedad Laboral (IPL)
# Colombia 2025
#
# Fuente:
# Gran Encuesta Integrada de Hogares (GEIH)
# DANE, Colombia
#
# Archivo requerido:
# GEIH_2025.xlsx
#
# Variables utilizadas:
# INGLABO  = ingreso laboral
# P6920    = formalidad
# P6424S5  = afiliación a seguridad social
# P6800    = horas trabajadas
#
# Este script reproduce los resultados reportados
# en la Tabla 3 del manuscrito.
# --------------------------------------------------

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
anio<- 2025

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
n0 <- nrow(datos)

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
      TRUE~ NA_integer_  # por si aparece algún otro código inesperado
    )
  ) %>%
  # opcional: quitar cualquier ss no mapeado (NA)
  filter(!is.na(ss)) %>%
  # mantener el orden pedido
  select(ingreso, formalidad, ss, horas)
n1 <- nrow(datos)

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
n2 <- nrow(datos)

# Quitar ingreso 0
datos$ingreso <- suppressWarnings(as.numeric(datos$ingreso))
datos <- datos[!is.na(datos$ingreso) & datos$ingreso != 0, ]
datos <- datos[, c("ingreso", "formalidad", "ss", "horas")]
n3 <- nrow(datos)

# Si el ingreso está dado MENSUAL lo pasamos a semanal
datosmensual<- datos$ingreso
ingsemanal<-datos$ingreso* 12 / 52
datos$ingreso<-ingsemanal

#_____________________________________#
#_____________________________________#
#_____________________________________#
data.frame(
  Paso = c(
    "Original",
    "Seguridad social válida",
    "Formalidad válida",
    "Ingreso positivo",
    "Muestra final"
  ),
  N = c(n0,n1,n2,n3,nrow(datos))
)

#___________________________________________#
# Salario mínimo  y número de horas

obtener_ideal <- function(anio){
  
  switch(as.character(anio),
         "2023" = list(ingreso = 1160000*12/52, numero_horas_ideales = 47),
         "2024" = list(ingreso = 1300000*12/52, numero_horas_ideales = 46),
         "2025" = list(ingreso = 1423500*12/52, numero_horas_ideales = 44),
         stop("Año no disponible")
  )
}

# Ingresa los siguientes datos
ingreso              <- obtener_ideal(anio)$ingreso #ideal_ing 
numero_horas_ideales <- obtener_ideal(anio)$numero_horas_ideales #ideal_hrs
alpha_op             <- 2 

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


