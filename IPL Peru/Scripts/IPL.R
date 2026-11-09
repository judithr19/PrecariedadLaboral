# ============================================================
# Indice de Precariedad Laboral
# ============================================================
# Input: Ingreso, hmin, hmax, hideal

suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    message("Sugerencia: instala {dplyr} si quieres usar los ejemplos con pipes.")
  }
})

# ============================================================
# Funciones base
# ============================================================
.clip01 <- function(x) pmin(1, pmax(0, x))

d_ingreso <- function(ing, y_ref) {
  stopifnot(is.numeric(y_ref), length(y_ref) == 1, y_ref > 0)
  .clip01(1 - pmin(ing, y_ref) / y_ref)
}

# f: 0 = formal, !=0 = informal; NA -> NA
d_formal <- function(f) ifelse(is.na(f), NA_real_, as.numeric(f != 0))

# ss: 0 = con SS, !=0 = sin SS; NA -> NA
d_ss <- function(s) ifelse(is.na(s), NA_real_, as.numeric(s != 0))

# Método por tramo [h_min, h_max]
d_horas_rel <- function(h, horas_ideal) .clip01(abs(1 - h / horas_ideal))

# ============================================================
# Índice individual (todos los parámetros se pasan al llamar)
# ============================================================
indice_individual <- function(
    ing, f, ss, h,
    ingreso_ref,
    horas_ideal,
    alpha
) {
  # Validaciones mínimas
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0)
  d1 <- d_ingreso(ing, ingreso_ref)
  d2 <- d_formal(f)
  d3 <- d_ss(ss)
  d4 <- d_horas_rel(h, horas_ideal)
  ipl <- ((d1^alpha + d2^alpha + d3^alpha + d4^alpha) / 4)^(1 / alpha)
  
  c(
    d_ingreso = mean(d1),
    d_formal = mean(d2),
    d_ss = mean(d3),
    d_horas = mean(d4),
    IPL = mean(ipl)
  )
}

# ============================================================
# Wrapper para data.frame (pasas inputs explícitos)
# ============================================================
# cols: lista con nombres de columnas en 'data'
#   list(ing="ing", f="f", ss="ss", h="h", out="indice")
# detalle=TRUE agrega d_ingreso, d_formal, d_ss, d_horas
calcular_indice_df <- function(
    data,
    cols = list(ing="ing", f="f", ss="ss", h="h", out="indice"),
    ingreso_ref,
    horas_ideal,
    alpha,
    detalle = FALSE
) {
  stopifnot(is.data.frame(data))
  
  defaults <- list(ing="ing", f="f", ss="ss", h="h", out="indice")
  cols <- modifyList(defaults, cols)
  
  faltan <- setdiff(c(cols$ing, cols$f, cols$ss, cols$h), names(data))
  if (length(faltan) > 0) {
    stop("No se encontraron las columnas en 'data': ", paste(faltan, collapse = ", "))
  }
  
  ing_vec <- data[[cols$ing]]
  f_vec   <- data[[cols$f]]
  ss_vec  <- data[[cols$ss]]
  h_vec   <- data[[cols$h]]
  
  idx <- indice_individual(
    ing = ing_vec, f = f_vec, ss = ss_vec, h = h_vec,
    ingreso_ref = ingreso_ref,
    horas_ideal = horas_ideal,
    alpha = alpha
  )
  
  data[[cols$out]] <- idx
  
  if (isTRUE(detalle)) {
    data[["d_ingreso"]] <- d_ingreso(ing_vec, ingreso_ref)
    data[["d_formal"]]  <- d_formal(f_vec)
    data[["d_ss"]]      <- d_ss(ss_vec)
    data[["d_horas"]]   <- 
      d_horas_rel(h_vec, horas_ideal)
  }
  data
}