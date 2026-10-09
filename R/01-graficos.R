
# ============================================================
# Trabajo 1 - Series de Tiempo
# Equipo: STS9J7
# Archivo: 01-graficos.R
# Descripcion: Graficos de series, estacionalidad, rezagos
#              y correlograma.
# ============================================================


# Validacion interna de los datos
.validar_serie_grafica <- function(serie) {
  
  columnas <- c("t", "fecha", "y")
  
  if (!is.data.frame(serie) ||
      !all(columnas %in% names(serie))) {
    stop("La serie debe contener las columnas t, fecha y y.")
  }
  
  if (nrow(serie) < 2L ||
      anyNA(serie$y) ||
      any(!is.finite(serie$y))) {
    stop("La serie debe tener al menos dos valores validos.")
  }
  
  invisible(TRUE)
}


# 1. Grafico de la serie en el tiempo
graficar_serie <- function(serie, titulo = NULL,
                           unidad = NULL) {
  
  .validar_serie_grafica(serie)
  
  if (is.null(titulo)) {
    titulo <- attr(serie, "fuente")
  }
  
  if (is.null(titulo) || length(titulo) == 0L ||
      is.na(titulo[1]) || !nzchar(titulo[1])) {
    titulo <- "Serie de tiempo"
  }
  
  if (is.null(unidad)) {
    unidad <- attr(serie, "unidad")
  }
  
  if (is.null(unidad) || length(unidad) == 0L ||
      is.na(unidad[1]) || !nzchar(unidad[1])) {
    unidad <- "Valor"
  }
  
  ggplot2::ggplot(
    serie,
    ggplot2::aes(x = fecha, y = y)
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = unidad,
      caption = paste("Numero de observaciones:", nrow(serie))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# 2. Grafico estacional
grafico_estacional <- function(serie, s,
                               titulo = "Grafico estacional") {
  
  .validar_serie_grafica(serie)
  
  if (length(s) != 1L || is.na(s) ||
      !is.finite(s) || s < 2 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 2.")
  }
  
  s <- as.integer(s)
  
  indice <- seq_along(serie$y)
  
  datos <- data.frame(
    estacion = ((indice - 1L) %% s) + 1L,
    ciclo = factor((indice - 1L) %/% s + 1L),
    y = serie$y
  )
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = estacion,
      y = y,
      group = ciclo,
      colour = ciclo
    )
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::labs(
      title = titulo,
      x = paste("Posicion dentro del ciclo (s =", s, ")"),
      y = "Valor",
      colour = "Ciclo"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )
}


# 3. Grafico de rezagos
grafico_rezagos <- function(serie,
                            rezagos = c(1, 2, 3, 4, 5, 6)) {
  
  .validar_serie_grafica(serie)
  
  if (length(rezagos) == 0L ||
      anyNA(rezagos) ||
      any(!is.finite(rezagos)) ||
      any(rezagos < 1) ||
      any(rezagos != as.integer(rezagos)) ||
      any(rezagos >= nrow(serie))) {
    stop("Los rezagos deben ser enteros entre 1 y T - 1.")
  }
  
  rezagos <- unique(as.integer(rezagos))
  
  datos <- do.call(
    rbind,
    lapply(rezagos, function(k) {
      data.frame(
        rezago = paste("k =", k),
        anterior = serie$y[seq_len(nrow(serie) - k)],
        actual = serie$y[(k + 1L):nrow(serie)]
      )
    })
  )
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = anterior, y = actual)
  ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::facet_wrap(~ rezago, scales = "free") +
    ggplot2::labs(
      title = "Grafico de rezagos",
      x = expression(y[t-k]),
      y = expression(y[t])
    ) +
    ggplot2::theme_minimal()
}


# 4. Correlograma: autocorrelacion muestral
correlograma <- function(serie, m = NULL,
                         titulo = "Correlograma") {
  
  .validar_serie_grafica(serie)
  
  y <- serie$y
  n <- length(y)
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24L)
  }
  
  if (length(m) != 1L || is.na(m) ||
      !is.finite(m) || m < 1 ||
      m != as.integer(m) || m >= n) {
    stop("m debe ser un entero entre 1 y T - 1.")
  }
  
  m <- as.integer(m)
  
  centrada <- y - mean(y)
  denominador <- sum(centrada^2)
  
  if (denominador == 0) {
    stop("No se puede calcular el correlograma de una serie constante.")
  }
  
  r <- vapply(0:m, function(k) {
    sum(
      centrada[seq_len(n - k)] *
        centrada[(1L + k):n]
    ) / denominador
  }, numeric(1))
  
  datos <- data.frame(
    rezago = 0:m,
    acf = r
  )
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = rezago, y = acf)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "grey40"
    ) +
    ggplot2::geom_hline(
      yintercept = c(-1.96 / sqrt(n), 1.96 / sqrt(n)),
      linetype = "dashed",
      colour = "blue"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = rezago, y = 0, yend = acf)
    ) +
    ggplot2::labs(
      title = titulo,
      x = "Rezago",
      y = "Autocorrelacion muestral"
    ) +
    ggplot2::theme_minimal()
}

