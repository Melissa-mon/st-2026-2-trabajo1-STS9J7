# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 03-dependencia.R
# Descripcion: Analisis de dependencia y estacionariedad.
# ============================================================


acf_muestral <- function(y, m = min(floor(length(y) / 4), 24)) {
  
  # Validar entradas
  stopifnot(
    is.numeric(y),
    length(y) >= 2,
    all(is.finite(y)),
    length(m) == 1,
    is.finite(m),
    m == as.integer(m),
    m >= 0,
    m < length(y)
  )
  
  # Número de observaciones y media
  T <- length(y)
  media <- mean(y)
  
  # Centrar la serie
  z <- y - media
  
  # Denominador común para todos los rezagos
  denominador <- sum(z^2)
  
  if (denominador == 0) {
    stop("La ACF no está definida para una serie constante.")
  }
  
  # Calcular la autocorrelación para cada rezago
  r <- numeric(m + 1)
  
  for (h in 0:m) {
    r[h + 1] <- sum(
      z[(h + 1):T] * z[1:(T - h)]
    ) / denominador
  }
  
  # Devolver rezagos y autocorrelaciones
  data.frame(
    rezago = 0:m,
    acf = r
  )
}


# ============================================================
# Funcion: pacf_dl
# Descripcion: Calcula la PACF mediante Durbin-Levinson.
# ============================================================

pacf_dl <- function(r) {
  
  stopifnot(
    is.numeric(r),
    length(r) >= 1,
    all(is.finite(r)),
    abs(r[1] - 1) < 1e-12
  )
  
  m <- length(r) - 1
  
  if (m == 0) {
    return(numeric(0))
  }
  
  phi <- matrix(0, nrow = m, ncol = m)
  pacf <- numeric(m)
  
  for (k in seq_len(m)) {
    
    if (k == 1) {
      phi[k, k] <- r[k + 1]
    } else {
      numerador <- r[k + 1] -
        sum(phi[k - 1, 1:(k - 1)] *
              rev(r[2:k]))
      
      denominador <- 1 -
        sum(phi[k - 1, 1:(k - 1)] * r[2:k])
      
      phi[k, k] <- numerador / denominador
      
      for (j in seq_len(k - 1)) {
        phi[k, j] <- phi[k - 1, j] -
          phi[k, k] * phi[k - 1, k - j]
      }
    }
    
    pacf[k] <- phi[k, k]
  }
  
  pacf
}

# ============================================================
# Funcion: ljung_box
# Descripcion: Prueba de autocorrelacion conjunta de Ljung-Box.
# ============================================================

ljung_box <- function(r, T, m, p = 0) {
  
  stopifnot(
    is.numeric(r),
    length(r) >= m + 1,
    all(is.finite(r)),
    T >= 2,
    T == as.integer(T),
    m >= 1,
    m == as.integer(m),
    m < T,
    p >= 0,
    p == as.integer(p),
    m > p
  )
  
  # Autocorrelaciones desde el rezago 1 hasta m
  rh <- r[2:(m + 1)]
  
  # Estadistico de Ljung-Box
  Q <- T * (T + 2) *
    sum(rh^2 / (T - seq_len(m)))
  
  # Grados de libertad
  gl <- m - p
  
  # Valor critico al 5 %
  valor_critico <- qchisq(0.95, df = gl)
  
  # Valor p
  valor_p <- pchisq(Q, df = gl, lower.tail = FALSE)
  
  data.frame(
    estadistico = Q,
    grados_libertad = gl,
    valor_critico = valor_critico,
    valor_p = valor_p
  )
}

# ============================================================
# Funcion: varianza_largo_plazo
# Descripcion: Estima la varianza de largo plazo con Bartlett.
# ============================================================

varianza_largo_plazo <- function(y, L) {
  
  stopifnot(
    is.numeric(y),
    length(y) >= 2,
    all(is.finite(y)),
    length(L) == 1,
    is.finite(L),
    L == as.integer(L),
    L >= 0,
    L < length(y)
  )
  
  T <- length(y)
  z <- y - mean(y)
  
  # Varianza muestral con divisor T
  gamma_0 <- sum(z^2) / T
  
  # Inicializar estimacion
  varianza_lp <- gamma_0
  
  # Sumar autocovarianzas con pesos de Bartlett
  if (L > 0) {
    for (h in seq_len(L)) {
      
      gamma_h <- sum(
        z[(h + 1):T] * z[1:(T - h)]
      ) / T
      
      peso <- 1 - h / (L + 1)
      
      varianza_lp <- varianza_lp + 2 * peso * gamma_h
    }
  }
  
  if (varianza_lp < 0) {
    warning("La estimacion de varianza de largo plazo es negativa.")
  }
  
  data.frame(
    varianza_largo_plazo = varianza_lp,
    rezagos = L,
    observaciones = T
  )
}