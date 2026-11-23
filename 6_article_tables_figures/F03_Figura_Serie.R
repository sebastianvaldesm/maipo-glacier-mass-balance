# F03. Figura 3 del articulo: cambio de masa acumulado desde 2001 de la cuenca y de sus seis grupos,
# con la mediana y los percentiles 25 y 75 de los glaciares ponderados por area, y el balance de los
# dos periodos.
# Sale: figuras/Figura3_serie.png y .jpg
base <- "C:/maipo/proyecto"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
RAMA <- "C:/maipo/trabajo/cadena_propia/gdem_cpd/tablas"
leer <- function(f, d = RAMA) read.csv(file.path(d, f), stringsAsFactors = FALSE)
azul <- "#1B4F72"; gris <- "#55606B"
COL_G <- c("Maipo basin" = "gray25", "Mapocho" = "#009E73", "Olivares" = "#E69F00", "Colorado" = "#CC79A7", "Yeso" = "#56B4E9",
           "Volcán" = "#1F3C88", "Maipo headwaters" = "#8C6D31")
ES_EN <- c("Cuenca alta del Maipo" = "Maipo basin", "Mapocho" = "Mapocho", "Olivares" = "Olivares", "Colorado" = "Colorado",
           "Yeso" = "Yeso", "Volcán" = "Volcán", "Cabecera del Maipo" = "Maipo headwaters")

S <- leer("M05c_Serie.csv"); S <- S[S$conjunto == "dga", ]
G <- leer("M06_Tasa_por_Glaciar.csv"); G <- G[G$conjunto == "dga", ]
G$grupo <- unname(ES_EN[G$grupo_cuenca])
TT <- leer("Tramos_2010_Cuenca_Grupos.csv", file.path(base, "06_Memoria_Final", "Tramos_2010"))
TT <- TT[TT$corte == 2010.5 & TT$inventario == "dga", ]

# serie de cada glaciar: cero en la referencia y las escenas usadas, en m w.e.
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
# El conjunto de glaciares observados cambia de un ano a otro (una escena no ve todos los glaciares), y
# una mediana sobre glaciares distintos puede saltar sin que cambie el hielo. Por eso la banda y la
# linea pasan solo por los anos en que se observa al menos el 70 % del area del grupo; los demas anos
# van como punto vacio.
UMBRAL_AREA <- 0.70
resumen <- function(d, area_total) {
  do.call(rbind, lapply(split(d, d$anio), function(e) {
    if (nrow(e) < 3) return(NULL)
    data.frame(anio = e$anio[1], n = nrow(e), frac_area = sum(e$area) / area_total,
               q25 = cuantil_p(e$mwe, e$area, 0.25), q50 = cuantil_p(e$mwe, e$area, 0.50), q75 = cuantil_p(e$mwe, e$area, 0.75))
  }))
}
paneles <- c("Maipo basin", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters")
area_grupo <- function(g) { gids <- unique(A$gid[if (g == "Maipo basin") TRUE else A$grupo %in% g]); sum(G$area_utm_km2[G$gid %in% gids]) }
R <- lapply(paneles, function(g) resumen(if (g == "Maipo basin") A else A[A$grupo %in% g, ], area_grupo(g)))
names(R) <- paneles
ylim <- range(unlist(lapply(R, function(r) c(r$q25[r$frac_area >= UMBRAL_AREA], r$q75[r$frac_area >= UMBRAL_AREA], r$q50))))
ylim <- c(floor(ylim[1] / 5) * 5, max(2, ceiling(ylim[2])))
f2 <- function(x) { s <- formatC(abs(x), format = "f", digits = 2); ifelse(round(x, 2) < 0, paste0("\u2212", s), ifelse(round(x, 2) > 0, paste0("+", s), s)) }
nom_tt <- c("Maipo basin" = "Cuenca alta del Maipo", "Maipo headwaters" = "Cabecera del Maipo")

dibujo <- function() {
  par(mfrow = c(4, 2), mar = c(1.6, 3.3, 1.5, 0.6), oma = c(1.8, 0, 0, 0), mgp = c(2.0, 0.45, 0), las = 1, cex = 1, tcl = -0.25)
  for (g in paneles) {
    r <- R[[g]]; col <- COL_G[[g]]
    plot(NA, xlim = c(2000.5, 2026.5), ylim = ylim, xlab = "", ylab = "Mass change (m w.e.)", xaxs = "i", cex.lab = 0.95, yaxt = "n")
    yt <- seq(ylim[1], ylim[2], 5); axis(2, at = yt, labels = sub("^-", "−", yt))
    rect(2010, ylim[1] - 10, 2022.99, ylim[2] + 10, col = "gray92", border = NA)
    abline(h = seq(ylim[1], ylim[2], 5), col = "white", lwd = 0.8); abline(h = 0, col = "gray60", lwd = 0.7)
    abline(v = 2010.5, col = "gray45", lty = 2, lwd = 0.8)
    k <- r$frac_area >= UMBRAL_AREA; rk <- r[k, ]
    polygon(c(rk$anio, rev(rk$anio)), c(rk$q25, rev(rk$q75)), col = adjustcolor(col, 0.28), border = NA)
    lines(rk$anio, rk$q50, col = col, lwd = 1.6)
    points(rk$anio, rk$q50, pch = 21, bg = col, col = "white", cex = 0.85, lwd = 0.4)
    points(r$anio[!k], r$q50[!k], pch = 21, bg = "white", col = col, cex = 0.85, lwd = 0.9)
    box(col = gris, lwd = 0.6)
    t <- TT[TT$grupo == (if (g %in% names(nom_tt)) nom_tt[[g]] else g), ]
    n_g <- if (g == "Maipo basin") length(unique(A$gid)) else length(unique(A$gid[A$grupo %in% g]))
    title(sprintf("%s (%d glaciers)", g, n_g), adj = 0, line = 0.35, cex.main = 1.0, font.main = 4, col.main = if (g == "Maipo basin") "black" else col)
    v1 <- f2(t$balance_2001_2010); v2 <- f2(t$balance_2010_2025)
    text(2001, ylim[1] + 0.20 * diff(ylim), bquote("2001\u20132010: " * .(v1) * " m w.e. a"^"\u22121"), adj = 0, cex = 0.85)
    text(2001, ylim[1] + 0.08 * diff(ylim), bquote("2010\u20132025: " * .(v2) * " m w.e. a"^"\u22121"), adj = 0, cex = 0.85)
  }
  # ultimo cuadro: como leer la figura
  plot.new(); par(xpd = NA)
  rect(0.02, 0.68, 0.12, 0.84, col = adjustcolor("gray40", 0.28), border = NA); segments(0.02, 0.76, 0.12, 0.76, lwd = 1.6, col = "gray25")
  text(0.16, 0.76, "Median and 25th to 75th percentiles\nof the glaciers, weighted by area", adj = 0, cex = 0.9)
  points(0.07, 0.52, pch = 21, bg = "white", col = "gray25", cex = 1.1, lwd = 0.9)
  text(0.16, 0.52, "Median of a year in which less than\n70% of the glacier area was observed", adj = 0, cex = 0.9)
  rect(0.02, 0.24, 0.12, 0.36, col = "gray92", border = NA)
  text(0.16, 0.30, "Megadrought, 2010\u20132022", adj = 0, cex = 0.9)
  segments(0.07, 0.04, 0.07, 0.16, lty = 2, col = "gray45")
  text(0.16, 0.10, "Limit of the two periods (mid 2010)", adj = 0, cex = 0.9)
  par(xpd = FALSE)
  mtext("Year", side = 1, outer = TRUE, line = 0.6, cex = 1)
}
W <- 16 / 2.54; H <- 16 / 2.54
for (fmt in c("png", "jpg")) {
  f <- file.path(AQUI, "figuras", paste0("Figura3_serie.", fmt))
  if (fmt == "png") png(f, width = W, height = H, units = "in", res = 300, pointsize = 8, type = "cairo")
  if (fmt == "jpg") jpeg(f, width = W, height = H, units = "in", res = 300, pointsize = 8, quality = 95, type = "cairo")
  dibujo(); invisible(dev.off())
}
cat(sprintf("Figura serie: %.1f x %.1f cm; glaciares con referencia en 2001: %d de %d\n", W * 2.54, H * 2.54,
            length(unique(A$gid)), nrow(G)))
print(R[["Maipo basin"]], row.names = FALSE)
