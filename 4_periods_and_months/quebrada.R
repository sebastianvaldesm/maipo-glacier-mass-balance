# Recta quebrada continua para los dos periodos
#
# Reemplaza en el texto de M06 el ajuste de dos rectas separadas por una recta con un quiebre en
# T_CORTE: dh = B + r1 x1 + r2 x2, con x1 = min(t, c) - t0 y x2 = max(t - c, 0). Las dos partes
# comparten B, asi que no hay salto entre ellas y juntas dan el cambio de todo el periodo. Una parte
# se informa solo si tiene al menos 4 escenas en 5 anios. El remuestreo resuelve cada ajuste con la
# regla de Cramer.
# Uso: source("quebrada.R"); src <- aplicar_quebrada(src)

reemplazar_bloque <- function(src, viejo, nuevo) {
  n <- length(viejo)
  i <- which(src == viejo[1])
  i <- i[vapply(i, function(k) k + n - 1 <= length(src) && identical(src[k:(k + n - 1)], viejo), logical(1))]
  if (length(i) != 1) stop("El bloque aparece ", length(i), " veces en M06: ", viejo[1])
  c(src[seq_len(i - 1)], nuevo, src[(i + n):length(src)])
}

aplicar_quebrada <- function(src) {
  viejo1 <- c(
    "  for (k in 1:2) {",
    "    dd <- if (k == 1) d[d$anio_dec < T_CORTE, ] else d[d$anio_dec >= T_CORTE, ]",
    "    nm <- if (k == 1) \"t1\" else \"t2\"",
    "    o[[paste0(\"tasa_\", nm)]] <- NA; o[[paste0(\"error_\", nm)]] <- NA",
    "    if (nrow(dd) >= 4 && diff(range(dd$anio_dec)) >= 5) {",
    "      s2 <- summary(lm(dh_m ~ anio_dec, dd, weights = 1 / dd$NMAD_roca^2))$coefficients",
    "      if (nrow(s2) >= 2) { o[[paste0(\"tasa_\", nm)]] <- s2[2, 1]; o[[paste0(\"error_\", nm)]] <- s2[2, 2] }",
    "    }",
    "  }")
  nuevo1 <- c(
    "  # recta quebrada continua en T_CORTE (quebrada.R): los dos tramos comparten el intercepto",
    "  o$tasa_t1 <- NA; o$error_t1 <- NA; o$tasa_t2 <- NA; o$error_t2 <- NA",
    "  lado1 <- d$anio_dec < T_CORTE",
    "  vale <- function(tt) length(tt) >= 4 && diff(range(tt)) >= 5",
    "  if (sum(lado1) >= 2 && sum(!lado1) >= 2) {",
    "    d$x1 <- pmin(d$anio_dec, T_CORTE) - t0; d$x2 <- pmax(d$anio_dec - T_CORTE, 0)",
    "    sq <- summary(lm(dh_m ~ x1 + x2, d, weights = w))$coefficients",
    "    if (nrow(sq) >= 3) {",
    "      if (vale(d$anio_dec[lado1])) { o$tasa_t1 <- sq[2, 1]; o$error_t1 <- sq[2, 2] }",
    "      if (vale(d$anio_dec[!lado1])) { o$tasa_t2 <- sq[3, 1]; o$error_t2 <- sq[3, 2] }",
    "    }",
    "  }")
  src <- reemplazar_bloque(src, viejo1, nuevo1)

  viejo2 <- c(
    "    if (tramos) for (k in 1:2) {",
    "      m <- if (k == 1) t1m else !t1m",
    "      gg <- factor(key[m], levels = levels(g)); u <- usa[m]",
    "      # tapply y no rowsum: rowsum devuelve solo los glaciares con escenas en el tramo y",
    "      # desalineaba los conteos con rg (corregido el 29-10-2026); tapply deja NA en los demas",
    "      nd <- as.vector(tapply(as.numeric(u), gg, sum))",
    "      rg <- as.vector(tapply(ifelse(u, ta[m], -Inf), gg, max)) - as.vector(tapply(ifelse(u, ta[m], Inf), gg, min))",
    "      rk <- rep(NA_real_, ng); names(rk) <- levels(g)",
    "      rr <- pend(W[m], ta[m], y[m], gg); rk[rownames(rowsum(W[m], gg))] <- rr",
    "      rk[is.na(nd) | nd < 4 | !is.finite(rg) | rg < 5] <- NA",
    "      out[[paste0(\"t\", k)]][b, ] <- rk",
    "    }")
  nuevo2 <- c(
    "    if (tramos) {",
    "      # recta quebrada continua en T_CORTE (quebrada.R), minimos cuadrados ponderados por glaciar",
    "      x1 <- pmin(ta, T_CORTE) - t0; x2 <- pmax(ta - T_CORTE, 0)",
    "      sr <- function(v) as.vector(rowsum(W * v, g))",
    "      s0 <- sr(1); s1 <- sr(x1); s2 <- sr(x2); s11 <- sr(x1 * x1); s22 <- sr(x2 * x2); s12 <- sr(x1 * x2)",
    "      sy <- sr(y); s1y <- sr(x1 * y); s2y <- sr(x2 * y)",
    "      det3 <- function(a, b, c, d, e, f, g_, h, i) a * (e * i - f * h) - b * (d * i - f * g_) + c * (d * h - e * g_)",
    "      D <- det3(s0, s1, s2, s1, s11, s12, s2, s12, s22)",
    "      q1 <- det3(s0, sy, s2, s1, s1y, s12, s2, s2y, s22) / D",
    "      q2 <- det3(s0, s1, sy, s1, s11, s1y, s2, s12, s2y) / D",
    "      ok3 <- is.finite(D) & abs(D) > 1e-9",
    "      for (k in 1:2) {",
    "        m <- if (k == 1) t1m else !t1m",
    "        gg <- factor(key[m], levels = levels(g)); u <- usa[m]",
    "        nd <- as.vector(tapply(as.numeric(u), gg, sum))",
    "        rg <- as.vector(tapply(ifelse(u, ta[m], -Inf), gg, max)) - as.vector(tapply(ifelse(u, ta[m], Inf), gg, min))",
    "        rk <- if (k == 1) q1 else q2",
    "        rk[!ok3 | is.na(nd) | nd < 4 | !is.finite(rg) | rg < 5] <- NA",
    "        out[[paste0(\"t\", k)]][b, ] <- rk",
    "      }",
    "    }")
  src <- reemplazar_bloque(src, viejo2, nuevo2)
  src
}
