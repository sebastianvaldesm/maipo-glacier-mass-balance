# F04. Figura 4 del articulo: cambio de masa acumulado desde 2001 de la cuenca y de sus seis grupos en un
# solo grafico (los mismos datos de F03, sin la banda de percentiles), apaisado.
# Cada linea es la mediana de los glaciares ponderada por area; los anos en que se observa menos del 70 %
# del area del grupo van como punto vacio y la linea no pasa por ellos (igual que en F03).
# Sale: figuras/Figura4_serie_unida.png y .jpg (16 x 11 cm, 300 dpi)
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
RAMA <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas"
leer <- function(f, d = RAMA) read.csv(file.path(d, f), stringsAsFactors = FALSE)
gris <- "#55606B"
COL_G <- c("Maipo basin" = "black", "Mapocho" = "#009E73", "Olivares" = "#E69F00", "Colorado" = "#CC79A7", "Yeso" = "#56B4E9",
           "Volcán" = "#1F3C88", "Maipo headwaters" = "#8C6D31")
ES_EN <- c("Cuenca alta del Maipo" = "Maipo basin", "Mapocho" = "Mapocho", "Olivares" = "Olivares", "Colorado" = "Colorado",
           "Yeso" = "Yeso", "Volcán" = "Volcán", "Cabecera del Maipo" = "Maipo headwaters")

S <- leer("M05c_Serie.csv"); S <- S[S$conjunto == "dga", ]
G <- leer("M06_Tasa_por_Glaciar.csv"); G <- G[G$conjunto == "dga", ]
G$grupo <- unname(ES_EN[G$grupo_cuenca])
TT <- leer("Tramos_2010_Cuenca_Grupos.csv", file.path(base, "06_Memoria_Final", "Tramos_2010"))
TT <- TT[TT$corte == 2010.5 & TT$inventario == "dga", ]

ok <- S[S$motivo_serie %in% "ok" & !is.na(S$dh_m), ]
ok$anio <- floor(ok$anio_dec)
ref <- S[S$es_referencia %in% c(TRUE, "True"), ]
ref_anio <- tapply(floor(ref$anio_dec), ref$gid, min)
ok <- ok[ok$gid %in% names(ref_anio)[ref_anio == 2001], ]
A <- aggregate(dh_m ~ gid + anio, ok, median)
A <- rbind(A, data.frame(gid = names(ref_anio)[ref_anio == 2001], anio = 2001, dh_m = 0))
A$mwe <- A$dh_m * 850 / 1000
A$area <- G$area_utm_km2[match(A$gid, G$gid)]; A$grupo <- G$grupo[match(A$gid, G$gid)]

cuantil_p <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; cw <- cumsum(w) / sum(w); x[which(cw >= p)[1]] }
UMBRAL_AREA <- 0.70
resumen <- function(d, area_total) {
  do.call(rbind, lapply(split(d, d$anio), function(e) {
    if (nrow(e) < 3) return(NULL)
    data.frame(anio = e$anio[1], frac_area = sum(e$area) / area_total, q50 = cuantil_p(e$mwe, e$area, 0.50))
  }))
}
grupos <- c("Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters", "Maipo basin")   # la cuenca encima
area_grupo <- function(g) { gids <- unique(A$gid[if (g == "Maipo basin") TRUE else A$grupo %in% g]); sum(G$area_utm_km2[G$gid %in% gids]) }
R <- lapply(grupos, function(g) resumen(if (g == "Maipo basin") A else A[A$grupo %in% g, ], area_grupo(g)))
names(R) <- grupos
ylim <- range(unlist(lapply(R, function(r) r$q50)))
ylim <- c(floor(ylim[1] / 5) * 5, max(5, ceiling(ylim[2])))
f2 <- function(x) { s <- formatC(abs(x), format = "f", digits = 2); ifelse(round(x, 2) < 0, paste0("\u2212", s), ifelse(round(x, 2) > 0, paste0("+", s), s)) }
nom_tt <- c("Maipo basin" = "Cuenca alta del Maipo", "Maipo headwaters" = "Cabecera del Maipo")

# nombres al final de cada linea, separados para que no se toquen
fin <- sapply(grupos, function(g) { r <- R[[g]]; r <- r[r$frac_area >= UMBRAL_AREA, ]; r$q50[which.max(r$anio)] })
separar <- function(y, gap) {
  o <- order(y); z <- y[o]
  for (k in 2:length(z)) if (z[k] - z[k - 1] < gap) z[k] <- z[k - 1] + gap
  z <- z - (mean(z) - mean(y[o]))
  y[o] <- z; y
}

dibujo <- function() {
  par(mar = c(3.4, 3.8, 0.6, 0.6), mgp = c(2.3, 0.5, 0), las = 1, cex = 1, tcl = -0.25)
  plot(NA, xlim = c(2000.5, 2026.5), ylim = ylim, xlab = "Year", ylab = "Cumulative mass change (m w.e.)", xaxs = "i", yaxt = "n")
  yt <- seq(ylim[1], ylim[2], 5); axis(2, at = yt, labels = sub("^-", "\u2212", yt))
  rect(2010, ylim[1] - 10, 2022.99, ylim[2] + 10, col = "gray93", border = NA)
  abline(h = yt, col = "white", lwd = 0.8); abline(h = 0, col = "gray60", lwd = 0.7)
  abline(v = 2010.5, col = "gray45", lty = 2, lwd = 0.8)
  text(2016.5, ylim[2] - 0.6, "Megadrought", col = "gray45", cex = 0.9, font = 3)
  for (g in grupos) {
    r <- R[[g]]; col <- COL_G[[g]]; k <- r$frac_area >= UMBRAL_AREA; rk <- r[k, ]
    gruesa <- g == "Maipo basin"
    lines(rk$anio, rk$q50, col = col, lwd = if (gruesa) 2.6 else 1.5)
    points(rk$anio, rk$q50, pch = 21, bg = col, col = "white", cex = if (gruesa) 1.0 else 0.75, lwd = 0.4)
    # los anos con menos del 70 % del area observada no se dibujan: en un solo grafico saturan
  }
  box(col = gris, lwd = 0.6)
  # leyenda con el color de cada grupo y el balance de los dos periodos, abajo a la izquierda (ahi no hay
  # datos); el alto de cada fila va en unidades de texto, para que no dependa del tamano del grafico
  orden <- c("Maipo basin", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters")
  dy <- 1.85 * strheight("M", cex = 0.8)
  x0 <- 2001.3; y0 <- ylim[1] + (length(orden) + 2.0) * dy
  xa <- x0 + 8.4; xb <- x0 + 11.6   # borde derecho de las dos columnas de balance
  rect(x0 - 0.4, y0 - (length(orden) + 1.6) * dy, xb + 0.4, y0 + 0.9 * dy, col = "white", border = gris, lwd = 0.4)
  text(x0, y0, expression(bold("Mass balance (m w.e. a"^"\u22121"*")")), adj = 0, cex = 0.8)
  text(xa, y0 - dy, "2001\u20132010", adj = 1, cex = 0.78); text(xb, y0 - dy, "2010\u20132025", adj = 1, cex = 0.78)
  for (i in seq_along(orden)) {
    g <- orden[i]; t <- TT[TT$grupo == (if (g %in% names(nom_tt)) nom_tt[[g]] else g), ]; y <- y0 - (i + 1) * dy
    segments(x0, y, x0 + 0.8, y, col = COL_G[[g]], lwd = if (g == "Maipo basin") 2.6 else 1.8)
    text(x0 + 1.1, y, g, adj = 0, cex = 0.78, font = if (g == "Maipo basin") 2 else 1)
    text(xa, y, f2(t$balance_2001_2010), adj = 1, cex = 0.78); text(xb, y, f2(t$balance_2010_2025), adj = 1, cex = 0.78)
  }
}
W <- 16 / 2.54; H <- 10.5 / 2.54
for (fmt in c("png", "jpg")) {
  f <- file.path(AQUI, "figuras", paste0("Figura4_serie_unida.", fmt))
  if (fmt == "png") png(f, width = W, height = H, units = "in", res = 300, pointsize = 9, type = "cairo")
  if (fmt == "jpg") jpeg(f, width = W, height = H, units = "in", res = 300, pointsize = 9, quality = 95, type = "cairo")
  dibujo(); invisible(dev.off())
}
cat("Figura 4 unida:", round(W * 2.54, 1), "x", round(H * 2.54, 1), "cm\n")
