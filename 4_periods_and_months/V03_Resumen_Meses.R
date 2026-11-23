# V03. Resume la prueba de los meses (Resumen_Meses.csv) y dibuja la Figura 4 del articulo en las dos
# formas del error.
# Uso: Rscript V03_Resumen_Meses.R
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(base, "06_Memoria_Final", "Meses_MicMac")
VARV <- "C:/Users/sebas/Prueba_MMASTER/variantes_meses_mm"
RAMA <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas"
BASE_T <- file.path(base, "06_Memoria_Final", "Tramos_2010", "corte_2010.5", "tablas")
dir.create(file.path(AQUI, "figuras"), showWarnings = FALSE)
leer <- function(d, f) read.csv(file.path(d, f), stringsAsFactors = FALSE)

VAR <- c(solo_enero = "January", solo_febrero = "February", solo_marzo = "March", solo_abril = "April",
         solo_marzo_abril = "March and April", regla = "Best month (rule)", todos_los_meses = "All months")
dir_m06 <- function(v) if (v == "regla") BASE_T else file.path(VARV, v, "m06", "tablas")
dir_m05c <- function(v) if (v == "regla") RAMA else file.path(VARV, v, "tablas")
falta <- names(VAR)[!file.exists(file.path(sapply(names(VAR), dir_m06), "M06_Agregados.csv"))]
if (length(falta)) stop("Faltan variantes: ", paste(falta, collapse = ", "))

GR <- c("Cuenca alta del Maipo" = "Maipo basin", "Maipo | Mapocho" = "Mapocho", "Maipo | Olivares" = "Olivares",
        "Maipo | Colorado" = "Colorado", "Maipo | Yeso" = "Yeso", "Maipo | Volcán" = "Volcán",
        "Maipo | Cabecera del Maipo" = "Maipo headwaters")
GR_TASA <- c("Cuenca alta del Maipo" = NA, "Mapocho" = "Mapocho", "Olivares" = "Olivares", "Colorado" = "Colorado",
             "Yeso" = "Yeso", "Volcán" = "Volcán", "Cabecera del Maipo" = "Maipo headwaters")
wmed <- function(x, w) { o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) / sum(w) >= 0.5)[1]] }

G0 <- leer(dir_m06("regla"), "M06_Tasa_por_Glaciar.csv"); G0 <- G0[G0$conjunto == "dga", ]
filas <- list()
for (v in names(VAR)) {
  A <- leer(dir_m06(v), "M06_Agregados.csv"); A <- A[A$conjunto == "dga" & A$grupo %in% names(GR), ]
  G <- leer(dir_m06(v), "M06_Tasa_por_Glaciar.csv"); G <- G[G$conjunto == "dga", ]
  S <- leer(dir_m05c(v), "M05c_Serie.csv"); S <- S[S$conjunto == "dga" & (S$es_referencia %in% c(TRUE, "True") | S$motivo_serie %in% "ok"), ]
  S$doy <- as.integer(format(as.Date(substr(S$escena, 1, 10)), "%j"))
  nfe <- table(S$gid)
  # anios 2001-2025 con al menos una fecha en la serie de cada glaciar (la referencia cuenta como 2001)
  S$anio <- floor(S$anio_dec)
  nan <- tapply(S$anio, S$gid, function(z) length(unique(z[z >= 2001 & z <= 2025])))
  for (g in names(GR)) {
    a <- A[A$grupo == g, ]
    gg <- G[if (g == "Cuenca alta del Maipo") TRUE else G$grupo_cuenca == sub("Maipo \\| ", "", g), ]
    pr <- gg$propio %in% c(TRUE, "TRUE")
    m <- merge(gg[pr, c("gid", "tasa", "area_utm_km2")], G0[G0$propio %in% c(TRUE, "TRUE"), c("gid", "tasa")], by = "gid", suffixes = c("", "_regla"))
    filas[[length(filas) + 1]] <- data.frame(
      variante = v, meses = VAR[[v]], grupo = GR[[g]], glaciares = a$n, propios = a$n_propio, area_km2 = a$area_km2,
      area_propia_pct = a$area_propio_pct, fechas_por_glaciar = median(as.integer(nfe[gg$gid]), na.rm = TRUE),
      dia_del_anio_mediano = median(S$doy[S$gid %in% gg$gid]),
      # un solo mes puede no tener escenas en 2001: entonces la serie parte despues (marzo y abril, 2003-2004)
      anio_ref_mediano = median(gg$anio_ref[pr], na.rm = TRUE),
      area_ref_2001_pct = 100 * sum(gg$area_utm_km2[pr & gg$anio_ref %in% 2001]) / sum(gg$area_utm_km2),
      tasa = a$tasa_m_a, error_fb = a$error_conservador, error_boot = a$error_tasa,
      tasa_2001_2010 = a$tasa_2001_2013, error_t1_fb = a$error_t1_conservador, error_t1_boot = a$error_t1,
      tasa_2010_2025 = a$tasa_2013_2026, error_t2_fb = a$error_t2_conservador, error_t2_boot = a$error_t2,
      balance_850 = a$balance_850, B_mediano_m = wmed(gg$B_m[pr], gg$area_utm_km2[pr]),
      glaciares_comparados = nrow(m), dif_glaciar_mediana = median(m$tasa - m$tasa_regla),
      # escenas distintas de la serie en cada periodo, sin la referencia (panel b de la figura)
      escenas_2001_2010 = length(unique(S$escena[S$gid %in% gg$gid & !(S$es_referencia %in% c(TRUE, "True")) & S$anio_dec < 2010.5])),
      escenas_2010_2025 = length(unique(S$escena[S$gid %in% gg$gid & !(S$es_referencia %in% c(TRUE, "True")) & S$anio_dec >= 2010.5])),
      # anios con alguna escena en el grupo, y porcentaje de los 25 anios que ve cada glaciar, ponderado por area
      # (un glaciar sin serie cuenta como 0); el segundo va en el panel a de la figura
      anios_con_escena = length(unique(S$anio[S$gid %in% gg$gid & S$anio >= 2001 & S$anio <= 2025])),
      anios_vistos_pct = 100 * sum(ifelse(is.na(nan[gg$gid]), 0, nan[gg$gid]) / 25 * gg$area_utm_km2) / sum(gg$area_utm_km2),
      stringsAsFactors = FALSE)
  }
}
R <- do.call(rbind, filas)
base_r <- R[R$variante == "regla", c("grupo", "tasa", "error_fb", "error_boot")]
R$dif_tasa <- R$tasa - base_r$tasa[match(R$grupo, base_r$grupo)]
R$dif_en_error_fb <- R$dif_tasa / base_r$error_fb[match(R$grupo, base_r$grupo)]
R$dif_en_error_boot <- R$dif_tasa / base_r$error_boot[match(R$grupo, base_r$grupo)]
num <- sapply(R, is.numeric); R[num] <- lapply(R[num], round, 3)
write.csv(R, file.path(AQUI, "Resumen_Meses.csv"), row.names = FALSE)
print(R[R$grupo == "Maipo basin", c("meses", "propios", "fechas_por_glaciar", "dia_del_anio_mediano", "anio_ref_mediano", "area_ref_2001_pct", "tasa", "error_fb", "error_boot",
                                    "tasa_2001_2010", "tasa_2010_2025", "B_mediano_m", "dif_tasa", "dif_en_error_fb", "dif_en_error_boot",
                                    "dif_glaciar_mediana")], row.names = FALSE)

# --- figura para el articulo ---------------------------------------------------------------------------
azul <- "#1B4F72"; gris <- "#55606B"
COL_G <- c("Mapocho" = "#009E73", "Olivares" = "#E69F00", "Colorado" = "#CC79A7", "Yeso" = "#56B4E9",
           "Volcán" = "#1F3C88", "Maipo headwaters" = "#8C6D31")
etq <- c("Jan", "Feb", "Mar", "Apr", "Mar and\nApr", "Best\nmonth", "All\nmonths")
menos <- function(v) sub("^-", "\u2212", formatC(v, format = "fg"))
# etiquetas del eje x una por una (axis descarta las que se tocan, y las de dos lineas van mas abajo)
eje_x <- function(x) {
  axis(1, at = x, labels = FALSE)
  mtext(etq, side = 1, at = x, line = ifelse(grepl("\n", etq), 1.5, 0.8), cex = 0.9)
}
figura <- function(forma) {
  e <- if (forma == "fb") "error_fb" else "error_boot"
  e1 <- if (forma == "fb") "error_t1_fb" else "error_t1_boot"
  e2 <- if (forma == "fb") "error_t2_fb" else "error_t2_boot"
  cu <- R[R$grupo == "Maipo basin", ]; cu <- cu[match(names(VAR), cu$variante), ]
  x <- seq_along(VAR)
  par(mfrow = c(1, 2), mar = c(3.6, 3.9, 1.8, 0.6), mgp = c(2.4, 0.5, 0), las = 1, cex = 1, tcl = -0.25)
  # el mismo eje y en los dos paneles, para compararlos; abajo queda espacio para la leyenda y los conteos
  yl <- range(c(R$tasa, cu$tasa - cu[[e]], cu$tasa + cu[[e]], 0,
                cu$tasa_2001_2010 - cu[[e1]], cu$tasa_2001_2010 + cu[[e1]],
                cu$tasa_2010_2025 - cu[[e2]], cu$tasa_2010_2025 + cu[[e2]]), na.rm = TRUE)
  yl <- c(yl[1] - 0.12 * diff(yl), yl[2] + 0.21 * diff(yl))
  yl[1] <- yl[1] - 0.3   # espacio para la leyenda, que tapaba el punto mas bajo
  # (a) tasa 2001-2025: la cuenca con su error y los seis grupos
  plot(NA, xlim = c(0.5, length(x) + 0.5), ylim = yl, xaxt = "n", yaxt = "n", xlab = "",
       ylab = expression("Rate of elevation change (m a"^"\u22121"*")"))
  rect(5.5, yl[1] - 1, 6.5, yl[2] + 1, col = "gray93", border = NA)
  at <- pretty(yl); axis(2, at = at, labels = menos(at)); eje_x(x)
  abline(h = 0, col = "gray70")
  for (k in seq_along(COL_G)) {
    gk <- R[R$grupo == names(COL_G)[k], ]; gk <- gk[match(names(VAR), gk$variante), ]
    points(x + (k - 3.5) * 0.07, gk$tasa, pch = 21, bg = COL_G[k], col = "white", lwd = 0.3, cex = 0.9)
  }
  arrows(x, cu$tasa - cu[[e]], x, cu$tasa + cu[[e]], angle = 90, code = 3, length = 0.03, lwd = 0.9)
  points(x, cu$tasa, pch = 21, bg = "black", col = "white", cex = 1.4)
  # glaciares con ajuste propio en cada variante (con solo abril quedan pocos)
  text(x, yl[2] - 0.035 * diff(yl), cu$propios, cex = 0.8, col = gris)
  text(0.55, yl[2] - 0.035 * diff(yl), "n", cex = 0.8, col = gris, font = 3, adj = 0)
  # porcentaje de los anios 2001-2025 que ve cada glaciar, ponderado por area (con un solo mes quedan muchos anios sin ver)
  text(x, yl[2] - 0.095 * diff(yl), paste0(round(cu$anios_vistos_pct), "%"), cex = 0.8, col = gris)
  text(0.55, yl[2] - 0.095 * diff(yl), "y", cex = 0.8, col = gris, font = 3, adj = 0)
  box(col = gris, lwd = 0.6)
  title("(a) 2001–2025", adj = 0, cex.main = 1, col.main = azul, line = 0.5)
  legend("bottomleft", c("Maipo basin", names(COL_G)), pt.bg = c("black", COL_G), pch = 21, col = "white", pt.cex = c(1.3, rep(0.9, 6)),
         cex = 0.72, bty = "n", text.font = c(1, rep(3, 6)), ncol = 2, x.intersp = 0.6)
  # (b) los dos tramos de la cuenca
  yl2 <- yl
  plot(NA, xlim = c(0.5, length(x) + 0.5), ylim = yl2, xaxt = "n", yaxt = "n", xlab = "",
       ylab = expression("Rate of the Maipo basin (m a"^"\u22121"*")"))
  rect(5.5, yl2[1] - 1, 6.5, yl2[2] + 1, col = "gray93", border = NA)
  at <- pretty(yl2); axis(2, at = at, labels = menos(at)); eje_x(x)
  abline(h = 0, col = "gray70")
  arrows(x - 0.12, cu$tasa_2001_2010 - cu[[e1]], x - 0.12, cu$tasa_2001_2010 + cu[[e1]], angle = 90, code = 3, length = 0.03, lwd = 0.8, col = "gray55")
  points(x - 0.12, cu$tasa_2001_2010, pch = 21, bg = "gray75", col = "gray40", cex = 1.2)
  arrows(x + 0.12, cu$tasa_2010_2025 - cu[[e2]], x + 0.12, cu$tasa_2010_2025 + cu[[e2]], angle = 90, code = 3, length = 0.03, lwd = 0.8)
  points(x + 0.12, cu$tasa_2010_2025, pch = 21, bg = "gray20", col = "white", cex = 1.2)
  # escenas de cada periodo: 2001-2010 arriba y 2010-2025 abajo, para leer los cambios junto con los datos que los sostienen
  text(x, yl2[2] - 0.035 * diff(yl2), cu$escenas_2001_2010, cex = 0.8, col = "gray40")
  text(x, yl2[1] + 0.035 * diff(yl2), cu$escenas_2010_2025, cex = 0.8, col = "gray15")
  text(0.55, yl2[2] - 0.035 * diff(yl2), "s", cex = 0.8, col = "gray40", font = 3, adj = 0)
  text(0.55, yl2[1] + 0.035 * diff(yl2), "s", cex = 0.8, col = "gray15", font = 3, adj = 0)
  box(col = gris, lwd = 0.6)
  legend("bottomleft", c("2001\u20132010", "2010\u20132025"), pt.bg = c("gray75", "gray20"), col = c("gray40", "white"), pch = 21,
         pt.cex = 1.1, cex = 0.8, bty = "n", inset = c(0, 0.07))
  title("(b) 2001\u20132010 and 2010\u20132025", adj = 0, cex.main = 1, col.main = azul, line = 0.5)
}
for (forma in c("fb", "boot")) for (fmt in c("png", "jpg")) {
  f <- file.path(AQUI, "figuras", sprintf("Figura_meses_%s.%s", forma, fmt))
  if (fmt == "png") png(f, width = 16 / 2.54, height = 8.4 / 2.54, units = "in", res = 300, pointsize = 8, type = "cairo")
  if (fmt == "jpg") jpeg(f, width = 16 / 2.54, height = 8.4 / 2.54, units = "in", res = 300, pointsize = 8, quality = 95, type = "cairo")
  figura(forma); invisible(dev.off())
}
cat("Listo:", file.path(AQUI, "Resumen_Meses.csv"), "y figuras/Figura_meses_*\n")
