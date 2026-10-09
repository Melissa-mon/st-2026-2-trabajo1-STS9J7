# Trabajo 1 - Series de Tiempo
# Archivo: 00-lectura.R
# Código del grupo: STS9J7
# Descripción: Lectura e importación de las series.


# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 00-lectura.R
# Descripcion: Lectura y validacion de las series.
# ============================================================

leer_serie <- function(x, fuente, unidad) {
  
  stopifnot(
    is.character(fuente),
    length(fuente) == 1L,
    !is.na(fuente),
    is.character(unidad),
    length(unidad) == 1L,
    !is.na(unidad)
  )
  
  # Caso 1: objeto ts
  if (is.ts(x)) {
    
    if (!is.null(dim(x))) {
      stop("La serie debe ser univariada.")
    }
    
    y <- as.numeric(x)
    frecuencia <- frequency(x)
    inicio <- start(x)
    
    if (anyNA(y) || any(!is.finite(y))) {
      stop("La serie contiene valores invalidos.")
    }
    
    if (!frecuencia %in% c(1, 4, 12)) {
      stop("Para objetos ts se admite frecuencia 1, 4 o 12.")
    }
    
    if (frecuencia == 1) {
      fecha <- seq.Date(
        as.Date(sprintf("%04d-01-01", inicio[1])),
        by = "year",
        length.out = length(y)
      )
    } else if (frecuencia == 4) {
      mes <- 3L * (inicio[2] - 1L) + 1L
      
      fecha <- seq.Date(
        as.Date(sprintf("%04d-%02d-01", inicio[1], mes)),
        by = "3 months",
        length.out = length(y)
      )
    } else {
      fecha <- seq.Date(
        as.Date(sprintf("%04d-%02d-01",
                        inicio[1], inicio[2])),
        by = "month",
        length.out = length(y)
      )
    }
    
    # Caso 2: ruta a un archivo CSV
  } else if (is.character(x) && length(x) == 1L) {
    
    if (!file.exists(x)) {
      stop("No existe el archivo: ", x)
    }
    
    datos <- read.csv(
      x,
      stringsAsFactors = FALSE
    )
    
    if (!all(c("fecha", "valor") %in% names(datos))) {
      stop("El CSV debe contener las columnas fecha y valor.")
    }
    
    fecha <- as.Date(datos$fecha)
    
    if (is.numeric(datos$valor)) {
      y <- datos$valor
    } else {
      y <- suppressWarnings(as.numeric(datos$valor))
    }
    
    if (length(fecha) == 0L) {
      stop("El archivo no contiene observaciones.")
    }
    
    if (anyNA(fecha) || anyNA(y) || any(!is.finite(y))) {
      stop("Hay fechas o valores invalidos.")
    }
    
    if (anyDuplicated(fecha) ||
        is.unsorted(fecha, strictly = TRUE)) {
      stop("Las fechas deben ser unicas y crecientes.")
    }
    
    # Detectar frecuencia usando las fechas reales
    if (length(fecha) == 1L) {
      frecuencia <- NA_character_
    } else {
      dias <- diff(as.integer(fecha))
      
      meses <- 12L * as.integer(format(fecha, "%Y")) +
        as.integer(format(fecha, "%m"))
      
      saltos_mes <- diff(meses)
      
      if (all(dias == 1L)) {
        frecuencia <- "diaria"
      } else if (all(dias == 7L)) {
        frecuencia <- "semanal"
      } else if (all(saltos_mes == 1L)) {
        frecuencia <- "mensual"
      } else if (all(saltos_mes == 3L)) {
        frecuencia <- "trimestral"
      } else if (all(saltos_mes == 12L)) {
        frecuencia <- "anual"
      } else {
        stop("Las fechas no tienen una frecuencia regular admitida.")
      }
    }
    
  } else {
    stop("x debe ser un objeto ts o una ruta a un CSV.")
  }
  
  resultado <- tibble::tibble(
    t = seq_along(y),
    fecha = fecha,
    y = y
  )
  
  attr(resultado, "frecuencia") <- frecuencia
  attr(resultado, "fuente") <- fuente
  attr(resultado, "unidad") <- unidad
  
  resultado
}
