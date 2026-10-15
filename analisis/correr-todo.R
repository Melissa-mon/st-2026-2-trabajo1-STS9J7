
# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: analisis/correr-todo.R
#
# Ejecucion desde la raiz del repositorio:
# source("analisis/correr-todo.R")
#
# Este archivo:
# 1. Carga los scripts.
# 2. Lee y verifica las tres series.
# 3. Genera la ficha descriptiva y la particion.
# 4. Guarda los datos y la informacion de la sesion.
# 5. Genera y guarda los graficos exploratorios.
# ============================================================


# ------------------------------------------------------------
# 1. CARGAR LOS SCRIPTS DEL PROYECTO
# ------------------------------------------------------------

source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")
source("R/04-metodos.R")
source("R/05-evaluacion.R")

cat("\nScripts cargados correctamente.\n")


# ------------------------------------------------------------
# 2. PREPARAR LAS CARPETAS DE SALIDA
# ------------------------------------------------------------

if (!dir.exists("datos")) {
  stop("No se encuentra la carpeta datos/.")
}

dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("resultados", showWarnings = FALSE, recursive = TRUE)


# ------------------------------------------------------------
# 3. LEER LAS TRES SERIES OFICIALES
# ------------------------------------------------------------

serie1 <- leer_serie(
  "datos/STS9J7-serie-1.csv",
  fuente = "STS9J7-serie-1.csv",
  unidad = "miles de pesos"
)

serie2 <- leer_serie(
  "datos/STS9J7-serie-2.csv",
  fuente = "STS9J7-serie-2.csv",
  unidad = "cientos de clientes"
)

serie3 <- leer_serie(
  "datos/STS9J7-serie-3.csv",
  fuente = "STS9J7-serie-3.csv",
  unidad = "millones de pesos"
)

series <- list(
  "Serie 1" = serie1,
  "Serie 2" = serie2,
  "Serie 3" = serie3
)

cat("Las tres series se leyeron correctamente.\n")


# ------------------------------------------------------------
# 4. VALIDAR LAS OBSERVACIONES
# ------------------------------------------------------------

observaciones_esperadas <- c(
  "Serie 1" = 200,
  "Serie 2" = 180,
  "Serie 3" = 196
)

for (nombre in names(series)) {
  
  datos <- series[[nombre]]
  
  if (!all(c("t", "fecha", "y") %in% names(datos))) {
    stop("Faltan columnas requeridas en ", nombre, ".")
  }
  
  if (nrow(datos) != observaciones_esperadas[[nombre]]) {
    stop(
      "Numero inesperado de observaciones en ", nombre,
      ". Esperadas: ", observaciones_esperadas[[nombre]],
      "; encontradas: ", nrow(datos), "."
    )
  }
  
  if (anyNA(datos$fecha) ||
      anyNA(datos$y) ||
      any(!is.finite(datos$y))) {
    stop("Hay fechas o valores invalidos en ", nombre, ".")
  }
}

cat("Numero y validez de las observaciones verificados.\n")


# ------------------------------------------------------------
# 5. FICHA DESCRIPTIVA Y PARTICION
# ------------------------------------------------------------

ficha_series <- do.call(
  rbind,
  lapply(names(series), function(nombre) {
    
    datos <- series[[nombre]]
    T <- nrow(datos)
    n0 <- floor(0.8 * T)
    
    data.frame(
      serie = nombre,
      unidad = attr(datos, "unidad"),
      frecuencia = attr(datos, "frecuencia"),
      fecha_inicial = as.character(min(datos$fecha)),
      fecha_final = as.character(max(datos$fecha)),
      T = T,
      minimo = min(datos$y),
      maximo = max(datos$y),
      media = mean(datos$y),
      n0 = n0,
      origenes_h1 = T - n0,
      stringsAsFactors = FALSE
    )
  })
)

rownames(ficha_series) <- NULL

cat("\nFICHA DESCRIPTIVA Y PARTICION\n")
print(ficha_series)

write.csv(
  ficha_series,
  "resultados/ficha-series.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  series,
  "resultados/series-leidas.rds"
)

cat("\nFicha guardada en resultados/ficha-series.csv\n")
cat("Series guardadas en resultados/series-leidas.rds\n")


# ------------------------------------------------------------
# 6. FUNCION PARA GUARDAR GRAFICOS PNG
# ------------------------------------------------------------

guardar_grafico <- function(archivo, funcion, serie, ...) {
  
  grDevices::png(
    filename = archivo,
    width = 1200,
    height = 800,
    res = 120
  )
  
  # Cerrar el dispositivo aunque ocurra un error.
  on.exit(grDevices::dev.off(), add = TRUE)
  
  # Se pasa el tibble completo con t, fecha e y.
  resultado <- funcion(serie, ...)
  
  # Imprimir los objetos graficos que se devuelven.
  if (inherits(resultado, "ggplot") ||
      inherits(resultado, "patchwork")) {
    print(resultado)
  }
  
  invisible(archivo)
}


# ------------------------------------------------------------
# 7. GENERAR LOS GRAFICOS EXPLORATORIOS
# ------------------------------------------------------------

for (nombre in names(series)) {
  
  datos <- series[[nombre]]
  prefijo <- gsub(" ", "", tolower(nombre))
  
  # Rezagos exploratorios.
  if (nombre == "Serie 3") {
    rezagos <- c(1, 2, 3, 7, 14, 21)
  } else {
    rezagos <- c(1, 2, 3, 4, 5, 6)
  }
  
  # Regla de la seccion 3:
  # m = min(floor(T / 4), 24).
  m <- min(floor(nrow(datos) / 4), 24)
  
  # 7.1 Grafico de la serie en el tiempo.
  guardar_grafico(
    archivo = file.path(
      "figs",
      paste0(prefijo, "-serie.png")
    ),
    funcion = graficar_serie,
    serie = datos,
    titulo = paste(nombre, "- serie en el tiempo"),
    unidad = attr(datos, "unidad")
  )
  
  # 7.2 Grafico de rezagos.
  guardar_grafico(
    archivo = file.path(
      "figs",
      paste0(prefijo, "-rezagos.png")
    ),
    funcion = grafico_rezagos,
    serie = datos,
    rezagos = rezagos
  )
  
  # 7.3 Correlograma de la serie original.
  guardar_grafico(
    archivo = file.path(
      "figs",
      paste0(prefijo, "-correlograma.png")
    ),
    funcion = correlograma,
    serie = datos,
    m = m,
    titulo = paste(nombre, "- correlograma")
  )
  
  cat("Graficos generados para ", nombre, ".\n", sep = "")
}

cat("\nGraficos exploratorios guardados en figs/.\n")


# ------------------------------------------------------------
# 8. GUARDAR INFORMACION DE LA SESION
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  "sesion-info.txt"
)

cat("Informacion de la sesion guardada en sesion-info.txt\n")


# ------------------------------------------------------------
# 9. RESUMEN DE EJECUCION
# ------------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("EJECUCION COMPLETADA\n")
cat("Equipo: STS9J7\n")
cat("Series procesadas: ", length(series), "\n", sep = "")
cat("Ficha: resultados/ficha-series.csv\n")
cat("Datos: resultados/series-leidas.rds\n")
cat("Graficos: carpeta figs/\n")
cat("Sesion: sesion-info.txt\n")
cat("====================================================\n")

