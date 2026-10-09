
# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 02-operadores.R
# Descripcion: Diferenciacion y polinomios de rezago.
# ============================================================


# 1. Diferenciacion ordinaria y estacional
# Aplica (1 - B)^d (1 - B^s)^D sin utilizar diff().

diferenciar <- function(y, d = 0, D = 0, s = 1) {
  
  if (!is.numeric(y) || !is.null(dim(y)) ||
      length(y) == 0L || anyNA(y) ||
      any(!is.finite(y))) {
    stop("y debe ser un vector numerico finito y no vacio.")
  }
  
  validar_entero <- function(x, nombre, minimo) {
    if (length(x) != 1L || is.na(x) ||
        !is.finite(x) || x != as.integer(x) ||
        x < minimo) {
      stop(nombre, " debe ser un entero >= ", minimo, ".")
    }
  }
  
  validar_entero(d, "d", 0)
  validar_entero(D, "D", 0)
  validar_entero(s, "s", 1)
  
  d <- as.integer(d)
  D <- as.integer(D)
  s <- as.integer(s)
  
  resultado <- as.numeric(y)
  
  # Diferencias estacionales: (1 - B^s)^D
  if (D > 0L) {
    for (i in seq_len(D)) {
      
      n <- length(resultado)
      
      if (n <= s) {
        stop("No hay suficientes observaciones para diferenciar.")
      }
      
      resultado <- resultado[(s + 1L):n] -
        resultado[seq_len(n - s)]
    }
  }
  
  # Diferencias ordinarias: (1 - B)^d
  if (d > 0L) {
    for (i in seq_len(d)) {
      
      n <- length(resultado)
      
      if (n <= 1L) {
        stop("No hay suficientes observaciones para diferenciar.")
      }
      
      resultado <- resultado[2L:n] -
        resultado[seq_len(n - 1L)]
    }
  }
  
  resultado
}


# 2. Raices de un polinomio de rezago
# c(B) = 1 + c1*B + ... + cp*B^p

polinomio_rezago <- function(coef) {
  
  if (!is.numeric(coef) || !is.null(dim(coef)) ||
      length(coef) == 0L || anyNA(coef) ||
      any(!is.finite(coef))) {
    stop("coef debe ser un vector numerico finito y no vacio.")
  }
  
  # El polinomio debe tener termino constante igual a 1.
  if (coef[1] != 1) {
    stop("El primer coeficiente debe ser 1.")
  }
  
  # Un polinomio constante no tiene raices.
  if (length(coef) == 1L) {
    return(data.frame(
      real = numeric(0),
      imaginaria = numeric(0),
      modulo = numeric(0)
    ))
  }
  
  # polyroot recibe los coeficientes en orden ascendente.
  raices <- polyroot(coef)
  
  data.frame(
    real = Re(raices),
    imaginaria = Im(raices),
    modulo = Mod(raices)
  )
}

