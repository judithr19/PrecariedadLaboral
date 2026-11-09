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
anio<- 2025

# Archivo
ruta <- "ENEyESI2025.xlsx"

# Hojas a leer
hojas <- c("2025")

# Lectura, renombrado, selección y unión
datos <- map_dfr(
  hojas,
  ~ read_excel(ruta, sheet = .x) %>%
    # renombrar a los nombres solicitados (solo si existen)
    rename(
      horas      = c2_1_3,
      ingreso    = ing_t_t,
      ss         = b7b_2,
      formalidad = ocup_form
    ) %>%
    # quedarnos únicamente con las columnas en el orden pedido
    select(ingreso, formalidad, ss, horas)
)

# Vista rápida
dplyr::glimpse(datos)
n0 <- nrow(datos)

# Limpieza de los datos: Seguridad Social
datos <- datos %>%
  # Asegurar que ss es entero (si venía como texto)
  mutate(ss = suppressWarnings(as.integer(ss))) %>%
  # 1) eliminar ss == 0
  filter(!is.na(ss), !ss %in% c(0L, 77L, 88L, 99L)) %>%
  # 2) recodificar 1,2,3 -> 0 ; 4,5 -> 1
  mutate(
    ss = case_when(
      ss %in% c(1L) ~ 0L,
      ss %in% c(2L) ~ 1L,
      TRUE                  ~ NA_integer_  # por si aparece algún otro código inesperado
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

# Horas trabajadas
datos <- datos[!datos$horas %in% c(0, 888, 999), ]
datos$horas<-as.numeric(datos$horas)


# Ingreso
datos$ingreso <- suppressWarnings(as.numeric(datos$ingreso))
datos <- datos[!is.na(datos$ingreso) & datos$ingreso != 0, ]
datos <- datos[, c("ingreso", "formalidad", "ss", "horas")]
n3 <- nrow(datos)

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

#_____________________________________#
#_____________________________________#
#_____________________________________#
datosmensual<- datos$ingreso
ingsemanal<-datos$ingreso* 12 / 52
datos$ingreso<-ingsemanal

# salario mínimo  y horas
# sep 2023: $460.000 CLP y 45 horas semanales
# 2024: $500.000 CLP y 44 horas semanales
# 2025: $529.000 CLP y 44 horas semanales

#___________________________________________#
# salario mínimo  y número de horas
obtener_ideal <- function(anio){
  
  switch(as.character(anio),
         "2023" = list(ingreso = 460000*12/52, numero_horas_ideales = 45),
         "2024" = list(ingreso = 500000*12/52, numero_horas_ideales = 44),
         "2025" = list(ingreso = 529000*12/52, numero_horas_ideales = 44),
         stop("Año no disponible")
  )
}
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

round(resultado,4)

