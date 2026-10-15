
# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 04-metodos.R
# Descripcion: Metodos de pronostico de la seccion 6.
# ============================================================

# Catalogo de metodos de pronostico:
#
# 1.  ajustar_media(y)
#     Metodo: Media simple
#
# 2.  ajustar_ingenuo(y)
#     Metodo: Ingenuo
#
# 3.  ajustar_ingenuo_estacional(y, s)
#     Metodo: Ingenuo estacional
#
# 4.  ajustar_mm(y, k)
#     Metodo: Media movil
#     Parametro: k = 2, ..., 12
#
# 5.  ajustar_ses(y, alpha)
#     Metodo: Suavizamiento exponencial simple
#     Parametro: alpha = 0.02, ..., 0.98
#
# 6.  ajustar_dmm(y, k)
#     Metodo: Doble media movil
#     Parametro: k = 3, ..., 12
#
# 7.  ajustar_tendencia(y, tipo)
#     Metodo: Tendencia lineal, cuadratica o exponencial
#
# 8.  ajustar_tendencia_estacional(y, s)
#     Metodo: Tendencia con ficticias de suma cero
#
# 9.  ajustar_holt(y, alpha, beta)
#     Metodo: Holt
#     Parametros: alpha y beta, paso 0.05
#
# 10. ajustar_hw(y, s, alpha, beta, gamma)
#     Metodo: Holt-Winters aditivo
#     Parametros: alpha, beta y gamma, paso 0.1
#
# 11. ajustar_hw_mult(y, s, alpha, beta, gamma)
#     Metodo: Holt-Winters multiplicativo
#     Parametros: alpha, beta y gamma, paso 0.1
#
# Cada metodo debe devolver:
#   - yhat: pronosticos de un paso
#   - pronosticar: funcion para pronosticos futuros
#   - parametros: parametros del metodo
#
# ============================================================


ajustar_media <- function(y) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y))
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # Pronosticos de un paso usando solo datos anteriores
  if (n >= 2) {
    for (t in 2:n) {
      yhat[t] <- mean(y[1:(t - 1)])
    }
  }
  
  # Pronosticos futuros usando la media de toda la serie
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(mean(y), h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list()
  )
}

## Este método pronostica que el siguiente valor será igual al último valor observado.


ajustar_ingenuo <- function(y) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y))
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  if (n >= 2) {
    yhat[2:n] <- y[1:(n - 1)]
  }
  
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(tail(y, 1), h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list()
  )
}



ajustar_ingenuo_estacional <- function(y, s) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y)),
    length(s) == 1,
    is.finite(s),
    s >= 1,
    s == as.integer(s),
    length(y) > s
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # Usa el valor de la misma temporada anterior
  yhat[(s + 1):n] <- y[1:(n - s)]
  
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    indices <- ((n + seq_len(h) - s - 1) %% s) + 1
    posiciones <- n - s + indices
    
    y[posiciones]
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(s = s)
  )
}


ajustar_mm <- function(y, k) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y)),
    length(k) == 1,
    is.finite(k),
    k >= 2,
    k == as.integer(k),
    length(y) >= k
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # Pronostico de un paso con las k observaciones anteriores
  if (n > k) {
    for (t in (k + 1):n) {
      yhat[t] <- mean(y[(t - k):(t - 1)])
    }
  }
  
  # Pronosticos futuros usando las ultimas k observaciones
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(mean(tail(y, k)), h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(k = k)
  )
}


ajustar_ses <- function(y, alpha) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y)),
    length(alpha) == 1,
    is.finite(alpha),
    alpha > 0,
    alpha < 1
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # Inicializacion con la primera observacion
  nivel <- y[1]
  
  if (n >= 2) {
    for (t in 2:n) {
      yhat[t] <- nivel
      nivel <- alpha * y[t] + (1 - alpha) * nivel
    }
  }
  
  # Pronosticos futuros: ultimo nivel estimado
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(nivel, h)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(alpha = alpha)
  )
}


ajustar_dmm <- function(y, k) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y)),
    length(k) == 1,
    is.finite(k),
    k >= 3,
    k == as.integer(k),
    length(y) >= 2 * k - 1
  )
  
  n <- length(y)
  yhat <- rep(NA_real_, n)
  
  # Primera media movil
  mm1 <- rep(NA_real_, n)
  for (t in k:n) {
    mm1[t] <- mean(y[(t - k + 1):t])
  }
  
  # Segunda media movil
  mm2 <- rep(NA_real_, n)
  for (t in (2 * k - 1):n) {
    mm2[t] <- mean(mm1[(t - k + 1):t])
  }
  
  # Nivel y tendencia estimados
  nivel <- 2 * mm1 - mm2
  tendencia <- 2 / (k - 1) * (mm1 - mm2)
  
  for (t in (2 * k):n) {
    yhat[t] <- nivel[t - 1] + tendencia[t - 1]
  }
  
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    rep(nivel[n], h) +
      seq_len(h) * tendencia[n]
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      k = k,
      nivel = nivel,
      tendencia = tendencia
    )
  )
}



ajustar_tendencia <- function(y, tipo) {
  stopifnot(
    is.numeric(y),
    length(y) >= 1,
    all(is.finite(y)),
    is.character(tipo),
    length(tipo) == 1,
    tipo %in% c("lineal", "cuadratica", "exponencial")
  )
  
  n <- length(y)
  t <- seq_len(n)
  
  construir_X <- function(tiempo) {
    if (tipo == "cuadratica") {
      cbind(1, tiempo, tiempo^2)
    } else {
      cbind(1, tiempo)
    }
  }
  
  transformar_y <- function(valores) {
    if (tipo == "exponencial") {
      stopifnot(all(valores > 0))
      log(valores)
    } else {
      valores
    }
  }
  
  estimar <- function(valores, tiempo) {
    X <- construir_X(tiempo)
    respuesta <- transformar_y(valores)
    
    solve(
      crossprod(X),
      crossprod(X, respuesta)
    )
  }
  
  # Ajuste final con toda la serie
  beta <- estimar(y, t)
  
  predecir <- function(tiempo, coeficientes) {
    X <- construir_X(tiempo)
    valores <- as.vector(X %*% coeficientes)
    
    if (tipo == "exponencial") {
      exp(valores)
    } else {
      valores
    }
  }
  
  # Pronosticos de un paso: cada ajuste usa solo datos anteriores
  yhat <- rep(NA_real_, n)
  
  for (i in 2:n) {
    n_previos <- i - 1
    
    # Minimo de observaciones para estimar el modelo
    minimo <- if (tipo == "cuadratica") 3 else 2
    
    if (n_previos >= minimo) {
      beta_i <- estimar(
        y[1:n_previos],
        t[1:n_previos]
      )
      
      yhat[i] <- predecir(i, beta_i)
    }
  }
  
  
  # Pronosticos futuros usando el ajuste de toda la serie
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    futuro <- n + seq_len(h)
    predecir(futuro, beta)
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      tipo = tipo,
      coeficientes = beta
    )
  )
}

## La diferencia en la cantidad de NA se debe a que cada modelo necesita un número mínimo de observaciones para estimar sus coeficientes.


ajustar_tendencia_estacional <- function(y, s) {
  stopifnot(
    is.numeric(y),
    length(y) > s,
    all(is.finite(y)),
    length(s) == 1,
    is.finite(s),
    s >= 2,
    s == as.integer(s)
  )
  
  n <- length(y)
  tiempo <- seq_len(n)
  estacion <- ((tiempo - 1) %% s) + 1
  
  # Matriz de diseno con tendencia y efectos estacionales
  # de suma cero: se usan s - 1 columnas.
  construir_X <- function(tiempo, estacion) {
    X_est <- matrix(
      0,
      nrow = length(tiempo),
      ncol = s - 1
    )
    
    for (j in seq_len(s - 1)) {
      X_est[, j] <-
        as.numeric(estacion == j) -
        as.numeric(estacion == s)
    }
    
    cbind(intercepto = 1, tendencia = tiempo, X_est)
  }
  
  estimar <- function(valores, tiempos, estaciones) {
    X <- construir_X(tiempos, estaciones)
    
    if (qr(X)$rank < ncol(X)) {
      return(NULL)
    }
    
    as.vector(solve(crossprod(X), crossprod(X, valores)))
  }
  
  predecir <- function(tiempos, estaciones, beta) {
    as.vector(construir_X(tiempos, estaciones) %*% beta)
  }
  
  # Pronosticos de un paso con datos disponibles hasta t - 1
  yhat <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i > 1) {
      indices <- seq_len(i - 1)
      
      beta_i <- estimar(
        y[indices],
        tiempo[indices],
        estacion[indices]
      )
      
      if (!is.null(beta_i)) {
        yhat[i] <- predecir(i, estacion[i], beta_i)
      }
    }
  }
  
  # Ajuste final con toda la serie
  beta <- estimar(y, tiempo, estacion)
  
  if (is.null(beta)) {
    stop("No hay suficientes datos para estimar el modelo estacional.")
  }
  
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    futuro <- n + seq_len(h)
    estacion_futura <- ((futuro - 1) %% s) + 1
    
    predecir(futuro, estacion_futura, beta)
  }
  
  # Efectos estacionales con restriccion de suma cero
  efectos <- c(beta[3:(s + 1)], -sum(beta[3:(s + 1)]))
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      s = s,
      coeficientes = beta,
      tendencia = beta[1:2],
      efectos_estacionales = efectos
    )
  )
}




ajustar_holt <- function(y, alpha, beta) {
  stopifnot(
    is.numeric(y),
    length(y) >= 2,
    all(is.finite(y)),
    alpha > 0, alpha <= 1,
    beta >= 0, beta <= 1
  )
  
  n <- length(y)
  
  nivel <- numeric(n)
  tendencia <- numeric(n)
  yhat <- rep(NA_real_, n)
  
  # Inicializacion segun la Clase 4
  nivel[1] <- y[1]
  tendencia[1] <- 0
  
  # Pronostico de un paso y actualizacion de Holt
  for (t in 2:n) {
    yhat[t] <- nivel[t - 1] + tendencia[t - 1]
    
    nivel[t] <- alpha * y[t] +
      (1 - alpha) * yhat[t]
    
    tendencia[t] <- beta *
      (nivel[t] - nivel[t - 1]) +
      (1 - beta) * tendencia[t - 1]
  }
  
  # Pronosticos futuros
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    nivel[n] + seq_len(h) * tendencia[n]
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      alpha = alpha,
      beta = beta,
      nivel = nivel,
      tendencia = tendencia
    )
  )
}


# ============================================================
# Holt-Winters aditivo
# ============================================================

ajustar_hw <- function(y, s, alpha, beta, gamma) {
  
  stopifnot(
    is.numeric(y),
    length(y) >= 2 * s,
    all(is.finite(y)),
    length(s) == 1,
    s >= 2,
    s == as.integer(s),
    alpha >= 0, alpha <= 1,
    beta >= 0, beta <= 1,
    gamma >= 0, gamma <= 1
  )
  
  n <- length(y)
  
  # Inicializacion del nivel y la tendencia
  nivel <- rep(NA_real_, n)
  tendencia <- rep(NA_real_, n)
  estacional <- rep(NA_real_, n)
  
  nivel[s] <- mean(y[1:s])
  tendencia[s] <- (mean(y[(s + 1):(2 * s)]) -
                     mean(y[1:s])) / s
  
  # Efectos estacionales iniciales
  estacional[1:s] <- y[1:s] - nivel[s]
  
  # Pronosticos de un paso
  yhat <- rep(NA_real_, n)
  
  for (t in (s + 1):n) {
    
    yhat[t] <- nivel[t - 1] +
      tendencia[t - 1] +
      estacional[t - s]
    
    nivel[t] <- alpha * (y[t] - estacional[t - s]) +
      (1 - alpha) * (nivel[t - 1] + tendencia[t - 1])
    
    tendencia[t] <- beta * (nivel[t] - nivel[t - 1]) +
      (1 - beta) * tendencia[t - 1]
    
    estacional[t] <- gamma * (y[t] - nivel[t]) +
      (1 - gamma) * estacional[t - s]
  }
  
  # Pronosticos futuros
  pronosticar <- function(h) {
    
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    futuro <- seq_len(h)
    
    nivel[n] + futuro * tendencia[n] +
      estacional[n - s + 1 + ((futuro - 1) %% s)]
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      s = s,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      nivel = nivel,
      tendencia = tendencia,
      estacional = estacional
    )
  )
}

# ============================================================
# Holt-Winters multiplicativo
# ============================================================

ajustar_hw_mult <- function(y, s, alpha, beta, gamma) {
  
  stopifnot(
    is.numeric(y),
    length(y) >= 2 * s,
    all(is.finite(y)),
    all(y > 0),
    s >= 2,
    s == as.integer(s),
    alpha >= 0, alpha <= 1,
    beta >= 0, beta <= 1,
    gamma >= 0, gamma <= 1
  )
  
  n <- length(y)
  
  # Inicialización de nivel, tendencia y estacionalidad
  nivel <- rep(NA_real_, n)
  tendencia <- rep(NA_real_, n)
  estacional <- rep(NA_real_, n)
  yhat <- rep(NA_real_, n)
  
  nivel[s] <- mean(y[1:s])
  tendencia[s] <- (
    mean(y[(s + 1):(2 * s)]) - mean(y[1:s])
  ) / s
  
  estacional[1:s] <- y[1:s] / nivel[s]
  
  # Pronósticos de un paso y actualización de componentes
  for (t in (s + 1):n) {
    
    yhat[t] <- (nivel[t - 1] + tendencia[t - 1]) *
      estacional[t - s]
    
    nivel[t] <- alpha * (y[t] / estacional[t - s]) +
      (1 - alpha) * (nivel[t - 1] + tendencia[t - 1])
    
    tendencia[t] <- beta * (nivel[t] - nivel[t - 1]) +
      (1 - beta) * tendencia[t - 1]
    
    estacional[t] <- gamma * (y[t] / nivel[t]) +
      (1 - gamma) * estacional[t - s]
  }
  
  # Pronósticos futuros
  pronosticar <- function(h) {
    stopifnot(
      length(h) == 1,
      is.finite(h),
      h >= 1,
      h == as.integer(h)
    )
    
    indices <- ((n + seq_len(h) - 1) %% s) + 1
    
    (nivel[n] + seq_len(h) * tendencia[n]) *
      estacional[indices]
  }
  
  list(
    yhat = yhat,
    pronosticar = pronosticar,
    parametros = list(
      s = s,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      nivel = nivel,
      tendencia = tendencia,
      estacional = estacional
    )
  )
}