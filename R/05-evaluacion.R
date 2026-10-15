
# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 05-evaluacion.R
# Descripcion: Optimizacion, origen movil, medidas y
#              validacion de errores de pronostico.
# ============================================================


# ------------------------------------------------------------
# 1. OPTIMIZAR PARAMETROS POR MSE DE UN PASO
# ------------------------------------------------------------

optimizar <- function(y, metodo, rejilla) {
  
  stopifnot(
    is.numeric(y),
    all(is.finite(y)),
    is.function(metodo),
    is.data.frame(rejilla),
    nrow(rejilla) > 0
  )
  
  resultados <- rejilla
  resultados$MSE <- NA_real_
  
  for (i in seq_len(nrow(rejilla))) {
    
    parametros <- as.list(rejilla[i, , drop = FALSE])
    
    ajuste <- tryCatch(
      do.call(metodo, c(list(y = y), parametros)),
      error = function(e) NULL
    )
    
    if (!is.null(ajuste) &&
        !is.null(ajuste$yhat) &&
        length(ajuste$yhat) == length(y)) {
      
      error <- y - ajuste$yhat
      validos <- is.finite(error)
      
      if (any(validos)) {
        resultados$MSE[i] <- mean(error[validos]^2)
      }
    }
  }
  
  if (all(!is.finite(resultados$MSE))) {
    stop("No se pudo calcular el MSE para ninguna combinacion.")
  }
  
  mejor <- which.min(resultados$MSE)
  parametros_optimos <- as.list(
    rejilla[mejor, , drop = FALSE]
  )
  
  ajuste_optimo <- do.call(
    metodo,
    c(list(y = y), parametros_optimos)
  )
  
  list(
    rejilla = resultados,
    optimo = resultados[mejor, , drop = FALSE],
    ajuste_optimo = ajuste_optimo
  )
}


# ------------------------------------------------------------
# 2. EVALUACION DE ORIGEN MOVIL
# ------------------------------------------------------------

origen_movil <- function(
    y,
    metodo,
    parametros = list(),
    h = 1,
    inicial = floor(0.8 * length(y))) {
  
  stopifnot(
    is.numeric(y),
    all(is.finite(y)),
    is.function(metodo),
    is.list(parametros),
    length(h) == 1,
    h >= 1,
    h == as.integer(h),
    inicial >= 2,
    inicial == as.integer(inicial),
    inicial <= length(y) - h
  )
  
  origenes <- seq.int(inicial, length(y) - h)
  
  resultados <- vector("list", length(origenes))
  
  for (i in seq_along(origenes)) {
    
    origen <- origenes[i]
    entrenamiento <- y[seq_len(origen)]
    
    ajuste <- do.call(
      metodo,
      c(list(y = entrenamiento), parametros)
    )
    
    pronosticos <- ajuste$pronosticar(h)
    
    if (length(pronosticos) != h) {
      stop("El metodo no devolvio el numero esperado de pronosticos.")
    }
    
    resultados[[i]] <- data.frame(
      origen = rep(origen, h),
      horizonte = seq_len(h),
      observado = y[origen + seq_len(h)],
      pronostico = as.numeric(pronosticos)
    )
  }
  
  do.call(rbind, resultados)
}


# ------------------------------------------------------------
# 3. MEDIDAS DE ERROR
# ------------------------------------------------------------

medidas <- function(observado, pronostico) {
  
  stopifnot(
    is.numeric(observado),
    is.numeric(pronostico),
    length(observado) == length(pronostico)
  )
  
  validos <- is.finite(observado) &
    is.finite(pronostico)
  
  observado <- observado[validos]
  pronostico <- pronostico[validos]
  
  if (length(observado) == 0) {
    stop("No hay observaciones validas para evaluar.")
  }
  
  error <- observado - pronostico
  
  # Error porcentual absoluto medio.
  # Se excluyen observaciones iguales a cero.
  validos_mape <- observado != 0
  
  MAPE <- if (any(validos_mape)) {
    mean(
      abs(error[validos_mape] / observado[validos_mape])
    ) * 100
  } else {
    NA_real_
  }
  
  data.frame(
    n = length(error),
    ME = mean(error),
    MAD = mean(abs(error)),
    MAE = mean(abs(error)),
    MSE = mean(error^2),
    RMSE = sqrt(mean(error^2)),
    MAPE = MAPE
  )
}


# ------------------------------------------------------------
# 4. VALIDACION DE ERRORES DE PRONOSTICO
# ------------------------------------------------------------

validar_errores <- function(
    observado,
    pronostico,
    p = 0,
    m = NULL,
    graficar = TRUE) {
  
  stopifnot(
    is.numeric(observado),
    is.numeric(pronostico),
    length(observado) == length(pronostico),
    length(p) == 1,
    p >= 0,
    p == as.integer(p)
  )
  
  # Conservar observaciones y pronosticos validos.
  validos <- is.finite(observado) &
    is.finite(pronostico)
  
  observado <- observado[validos]
  pronostico <- pronostico[validos]
  
  error <- observado - pronostico
  n <- length(error)
  
  if (n < 8) {
    stop("Se necesitan al menos 8 errores validos.")
  }
  
  # Estadisticos descriptivos.
  medidas_error <- medidas(observado, pronostico)
  
  # ACF muestral calculada directamente.
  # Incluye r0 = 1 y los rezagos siguientes.
  acf_propia <- function(x, m) {
    
    x_centrado <- x - mean(x)
    denominador <- sum(x_centrado^2)
    
    if (denominador == 0) {
      stop("No se puede calcular la ACF: errores constantes.")
    }
    
    r <- numeric(m + 1)
    r[1] <- 1
    
    for (j in seq_len(m)) {
      r[j + 1] <- sum(
        x_centrado[seq_len(n - j)] *
          x_centrado[(j + 1):n]
      ) / denominador
    }
    
    r
  }
  
  # Numero de rezagos.
  if (is.null(m)) {
    m <- min(6, floor(n / 3))
  }
  
  stopifnot(
    length(m) == 1,
    m >= 1,
    m == as.integer(m),
    m < n,
    m > p
  )
  
  # Correlograma y banda de ruido blanco.
  r_error <- acf_propia(error, m)
  banda <- 1.96 / sqrt(n)
  
  # Prueba t de media cero.
  prueba_media <- t.test(error, mu = 0)
  
  # Ljung-Box propio, definido en 03-dependencia.R.
  prueba_ljung <- ljung_box(
    r = r_error,
    T = n,
    m = m,
    p = p
  )
  
  # Jarque-Bera.
  desviacion <- sqrt(mean((error - mean(error))^2))
  
  if (desviacion == 0) {
    JB <- 0
    p_JB <- 1
  } else {
    
    asimetria <- mean((error - mean(error))^3) /
      sd(error)^3
    
    curtosis <- mean((error - mean(error))^4) /
      sd(error)^4
    
    JB <- n / 6 * (
      asimetria^2 +
        (curtosis - 3)^2 / 4
    )
    
    p_JB <- pchisq(
      JB,
      df = 2,
      lower.tail = FALSE
    )
  }
  
  # Durbin-Watson.
  denominador_DW <- sum(error^2)
  
  DW <- if (denominador_DW > 0) {
    sum(diff(error)^2) / denominador_DW
  } else {
    NA_real_
  }
  
  # Graficos de diagnostico.
  if (graficar) {
    
    par(mfrow = c(2, 1))
    
    plot(
      error,
      type = "l",
      main = "Errores de pronostico",
      xlab = "Indice",
      ylab = "Error"
    )
    
    abline(h = 0, lty = 2)
    
    plot(
      seq_len(m),
      r_error[-1],
      type = "h",
      ylim = range(c(r_error[-1], -banda, banda)),
      main = "Correlograma de los errores",
      xlab = "Rezago",
      ylab = "ACF"
    )
    
    abline(h = 0)
    abline(h = c(-banda, banda), col = "blue", lty = 2)
    
    par(mfrow = c(1, 1))
  }
  
  list(
    n_errores = n,
    errores = error,
    medidas = medidas_error,
    acf = r_error,
    banda_acf = banda,
    prueba_media = prueba_media,
    ljung_box = prueba_ljung,
    jarque_bera = list(
      estadistico = JB,
      grados_libertad = 2,
      valor_critico = qchisq(0.95, df = 2),
      p_valor = p_JB
    ),
    durbin_watson = DW
  )
}
