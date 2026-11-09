#______________________________#
# Indice de Precariedad Laboral: Perú
#______________________________#
# Programa que tiene el cálculo del índice
# Utiliza los datos de la ENAHO 2023, 2024 y 2025
# y crea la base que se necesita en el run_IPLPeru.R 
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

# Archivo: para el 2023 y 2024
datos <- read.csv(
  "Enaho01a-2024-500.csv",
  sep = ",",
  header = TRUE,
  fileEncoding = "latin1"
)

# para el 2025 leer así:
library(data.table)
datos <- fread(
  "Enaho01a-2025-500.csv",
  encoding = "Latin-1"
)

names(datos)
n0 <- nrow(datos)
#str(datos)

# Número de Horas
datos$horas <- datos$P520


# Contrato  Formal o Informal
datos$formalidad <- ifelse(
  datos$P511A %in% c(1,2,3,4,5,6,8),
  0,
  ifelse(datos$P511A == 7, 1, NA)
)  
n2 <- nrow(datos)

# Seguridad Social 
datos$seg_social <- ifelse(
  datos$P558A1 == 1 |
    datos$P558A2 == 1 |
    datos$P558A3 == 1 |
    datos$P558A4 == 1,
  0, 1
)
n1 <- nrow(datos)
#datos$seg_social <- ifelse(datos$P558A5 == 1, 0, 1)

# Ingreso
datos$ingreso <- ifelse(
  !is.na(datos$P524E1) & datos$P524E1 > 0,
  datos$P524E1,
  datos$P530A
)
n3 <- nrow(datos)

library(dplyr)

datos_mov <- datos %>%
  select(
    #AÑO, MES, CONGLOME, VIVIENDA, HOGAR, CODPERSO,
    ingreso, horas, formalidad, seg_social
  ) %>%
  filter(
    !is.na(ingreso),
    ingreso > 0,
    !is.na(horas),
    horas > 0,
    !is.na(formalidad),
    !is.na(seg_social)
  )
n4 <- nrow(datos_mov)

data.frame(
  Paso = c(
    "Original",
    "Seguridad social válida",
    "Formalidad válida",
    "Ingreso positivo",
    "Muestra final"
  ),
  N = c(n0,n1,n2,n3,n4)
)

# Total de registros
nrow(datos)
# NAs por variable
colSums(is.na(datos[, c("ingreso","horas","formalidad","seg_social")]))
# Ingresos
sum(is.na(datos$ingreso))
sum(datos$ingreso <= 0, na.rm = TRUE)
# Horas
sum(is.na(datos$horas))
sum(datos$horas <= 0, na.rm = TRUE)
# Formalidad
sum(is.na(datos$formalidad))
# Seguridad social
sum(is.na(datos$seg_social))


#install.packages("openxlsx")
library(openxlsx)

write.xlsx(datos_mov,
           "2025_Peru.xlsx",
           rowNames = FALSE)
