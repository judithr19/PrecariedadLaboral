#______________________________#
# Indice de Precariedad Laboral
#______________________________#
# Necesita las bases de la ENOE 2023,2024 y 2025, que se pueden descargar de la página del INEGI
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
anio<-2024
# Archivo
ruta <- "IPLENOE2024.xlsx"

# Hojas a leer
hojas <- c("2024_T1", "2024_T2", "2024_T3", "2024_T4")

# Lectura, renombrado, selección y unión
datos <- map_dfr(
  hojas,
  ~ read_excel(ruta, sheet = .x) %>%
    # renombrar a los nombres solicitados (solo si existen)
    rename(
      horas      = hrsocup,
      ingreso    = ingocup,
      ss         = imssissste,
      formalidad = emp_ppal
    ) %>%
    # quedarnos únicamente con las columnas en el orden pedido
    select(ingreso, formalidad, ss, horas)
)

# Vista rápida
dplyr::glimpse(datos)
n0 <- nrow(datos) #original

# Filtro 1:
# Limpieza de los datos: SS
datos <- datos %>%
  # Asegurar que ss es entero (si venía como texto)
  mutate(ss = suppressWarnings(as.integer(ss))) %>%
  # 1) eliminar ss == 0
  filter(!is.na(ss), ss != 0) %>%
  # 2) recodificar 1,2,3 -> 0 ; 4,5 -> 1
  mutate(
    ss = case_when(
      ss %in% c(1L, 2L, 3L) ~ 0L,
      ss %in% c(4L, 5L)     ~ 1L,
      TRUE                  ~ NA_integer_  # por si aparece algún otro código inesperado
    )
  ) %>%
  # opcional: quitar cualquier ss no mapeado (NA)
  filter(!is.na(ss)) %>%
  # mantener el orden pedido
  select(ingreso, formalidad, ss, horas)
n1 <- nrow(datos) #seguridad social

# Filtro 2
# Formalidad
datos <- datos %>%
  # Asegurar que formalidad sea entero por si viene como texto
  mutate(formalidad = suppressWarnings(as.integer(formalidad))) %>%
  # Recodificación: 2 -> 0
  mutate(
    formalidad = case_when(
      formalidad == 1L ~ 1L,
      formalidad == 2L ~ 0L,
      TRUE             ~ NA_integer_   # por si aparece algo inesperado
    )
  ) %>%
  # Opcional: quitar valores no válidos
  filter(!is.na(formalidad)) %>%
  # mantener el orden de columnas
  select(ingreso, formalidad, ss, horas)
n2 <- nrow(datos) # formalidad

# Filtro 3
# Hay muchos con ingreso 0
datos$ingreso <- suppressWarnings(as.numeric(datos$ingreso))
datos <- datos[!is.na(datos$ingreso) & datos$ingreso != 0, ]
datos <- datos[, c("ingreso", "formalidad", "ss", "horas")]
n3<- nrow(datos) #ingreso

datosmensual<- datos$ingreso
ingsemanal<-datos$ingreso* 12 / 52
datos$ingreso<-ingsemanal

#install.packages("openxlsx")
library(openxlsx)

write.xlsx(datos,
           "2024_Mexico.xlsx",
           rowNames = FALSE)
