# F04. Figura 5 del articulo: contraste glaciar por glaciar con Dussaillant et al. (2019) y Hugonnet et
# al. (2021), y por subcuenca con Farias-Barahona et al. (2020).
# Sale: figuras/Figura5_contraste.png y .jpg
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
RAMA <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas"
leer <- function(f) read.csv(file.path(RAMA, f), stringsAsFactors = FALSE, check.names = FALSE)
azul <- "#1B4F72"; gris <- "#55606B"
COL_G <- c("Mapocho" = "#009E73", "Olivares" = "#E69F00", "Colorado" = "#CC79A7", "Yeso" = "#56B4E9",
           "Volcán" = "#1F3C88", "Maipo headwaters" = "#8C6D31")
GRUPOS <- names(COL_G)
ES_EN <- c("Mapocho" = "Mapocho", "Olivares" = "Olivares", "Colorado" = "Colorado", "Yeso" = "Yeso",
           "Volcán" = "Volcán", "Cabecera del Maipo" = "Maipo headwaters")
pt <- function(x, d = 2) formatC(x, format = "f", digits = d)
menos <- function(v) sub("^-", "\u2212", v)
titulo <- function(s) title(s, adj = 0, cex.main = 1, col.main = azul, line = 0.5)
W <- 16 / 2.54

guardar <- function(nombre, H, expr) {
  for (fmt in c("png", "jpg")) {
    f <- file.path(AQUI, "figuras", paste0(nombre, ".", fmt))
    if (fmt == "png") png(f, width = W, height = H / 2.54, units = "in", res = 300, pointsize = 8, type = "cairo")
    if (fmt == "jpg") jpeg(f, width = W, height = H / 2.54, units = "in", res = 300, pointsize = 8, quality = 95, type = "cairo")
    eval(expr); invisible(dev.off())
  }
}

# --- contraste glaciar por glaciar -----------------------------------------------------------------
Du <- leer("M06_Dussaillant_por_glaciar.csv"); Du <- Du[Du$en_maipo %in% c(TRUE, "TRUE") & !is.na(Du$MB_propio) & !is.na(Du$MB), ]
Hu <- leer("M06c_Hugonnet_por_glaciar.csv"); Hu <- Hu[Hu$en_maipo %in% c(TRUE, "TRUE") & Hu$comparable %in% c(TRUE, "TRUE"), ]
Du$g <- unname(ES_EN[Du$grupo_cuenca]); Hu$g <- unname(ES_EN[Hu$grupo_cuenca])
dentro <- function(a, ea, b, eb) 100 * mean(abs(a - b) <= sqrt(ea^2 + eb^2), na.rm = TRUE)
# el eje se acota a donde esta la gran mayoria de los glaciares; los pocos que quedan fuera se cuentan en el panel
LIM <- c(-1.3, 0.5)
panel <- function(x, y, area, g, xl, ttl, txt, ylab = TRUE) {
  tk <- seq(-1, 0.5, 0.5)
  plot(x, y, type = "n", xlim = LIM, ylim = LIM, xlab = "", ylab = if (ylab) expression("This study (m w.e. a"^"\u22121"*")") else "",
       asp = 1, axes = FALSE, xaxs = "i", yaxs = "i")
  axis(1, at = tk, labels = menos(tk)); axis(2, at = tk, labels = menos(tk))
  mtext(xl, side = 1, line = 1.7, cex = 0.95); mtext(expression("(m w.e. a"^"−1"*")"), side = 1, line = 2.8, cex = 0.95)
  abline(0, 1, col = "gray50", lty = 2); abline(h = 0, v = 0, col = "gray88")
  o <- order(-area)
  points(x[o], y[o], pch = 21, col = "white", lwd = 0.3, bg = adjustcolor(COL_G[g[o]], 0.85), cex = 0.4 + 2.0 * sqrt(area[o] / max(area)))
  fuera <- sum(x < LIM[1] | x > LIM[2] | y < LIM[1] | y > LIM[2])
  text(LIM[1] + 0.05, LIM[2] - 0.05, paste0(txt, if (fuera) sprintf("\n%d outside", fuera) else ""), adj = c(0, 1), cex = 0.75)
  box(col = gris, lwd = 0.6)
  titulo(ttl)
}
pD <- dentro(Du$MB_propio, Du$MB_propio_err, Du$MB, Du$MB_err)
pH <- dentro(Hu$MB20, Hu$MB20_err, Hu$Hug, Hu$Hug_err)
lin <- function(n, r, p) sprintf("%d glaciers\nr = %s\n%s%% within error", n, pt(r), round(p))
# (c) Farias-Barahona et al. (2020) publica solo subcuencas, en 2000-2013 y sobre el inventario nacional. Su Molina
# (05720) y su San Francisco (05721) son las dos partes del Mapocho: se juntan en un punto para que la figura use
# los mismos seis grupos que el resto del articulo. Su valor junto es el promedio por area (los dos valen -0.17);
# el de este estudio es el del grupo Mapocho en M06_Agregados (primer tramo, error por remuestreo y densidad, con
# la misma formula que M06 usa para cada subcuenca).
FB0 <- leer("M06_Farias_Barahona.csv")
AG <- leer("M06_Agregados.csv"); AG <- AG[AG$conjunto == "dga" & AG$grupo == "Maipo | Mapocho", ]
FB_EN <- c("Olivares" = "Olivares", "Colorado" = "Colorado", "Yeso" = "Yeso", "Volcán" = "Volcán",
           "Maipo alto" = "Maipo headwaters", "Molina" = "Mapocho", "San Francisco" = "Mapocho")
FB0$g <- unname(FB_EN[FB0$subcuenca])
mp <- FB0[FB0$g == "Mapocho", ]
FB <- rbind(FB0[FB0$g != "Mapocho", c("g", "MB_FB", "err_FB", "area_FB", "area_este", "MB_este_t1", "err_este_t1")],
            data.frame(g = "Mapocho", MB_FB = sum(mp$MB_FB * mp$area_FB) / sum(mp$area_FB),
                       err_FB = sum(mp$err_FB * mp$area_FB) / sum(mp$area_FB), area_FB = sum(mp$area_FB),
                       area_este = AG$area_km2, MB_este_t1 = AG$tasa_2001_2013 * 850 / 1000,
                       err_este_t1 = sqrt((AG$error_t1 * 850)^2 + (AG$tasa_2001_2013 * 60)^2) / 1000))
write.csv(FB, file.path(AQUI, "tablas", "Figura5_Farias_seis_grupos.csv"), row.names = FALSE)
pF <- 100 * mean(abs(FB$MB_este_t1 - FB$MB_FB) <= sqrt(FB$err_este_t1^2 + FB$err_FB^2))
panel_fb <- function() {
  lim <- c(-0.95, 0.45); tk <- seq(-0.8, 0.4, 0.4)
  plot(NA, xlim = lim, ylim = lim, xlab = "", ylab = "", asp = 1, axes = FALSE, xaxs = "i", yaxs = "i")
  axis(1, at = tk, labels = menos(formatC(tk, format = "f", digits = 1))); axis(2, at = tk, labels = menos(formatC(tk, format = "f", digits = 1)))
  mtext("Farías-Barahona et al. (2020)", side = 1, line = 1.7, cex = 0.95)
  mtext(expression("(m w.e. a"^"−1"*")"), side = 1, line = 2.8, cex = 0.95)
  abline(0, 1, col = "gray50", lty = 2); abline(h = 0, v = 0, col = "gray88")
  # los mas grandes primero, para que los chicos (Yeso, Mapocho) queden encima y se vean
  o <- order(-FB$area_este); F <- FB[o, ]; col <- COL_G[F$g]
  arrows(F$MB_FB - F$err_FB, F$MB_este_t1, F$MB_FB + F$err_FB, F$MB_este_t1, angle = 90, code = 3, length = 0.02, col = col, lwd = 0.8)
  arrows(F$MB_FB, F$MB_este_t1 - F$err_este_t1, F$MB_FB, F$MB_este_t1 + F$err_este_t1, angle = 90, code = 3, length = 0.02, col = col, lwd = 0.8)
  points(F$MB_FB, F$MB_este_t1, pch = 21, col = "white", lwd = 0.6, bg = adjustcolor(col, 0.9), cex = 0.8 + 1.6 * sqrt(F$area_este / max(F$area_este)))
  # cada nombre pasado el extremo de su barra horizontal (a la derecha o a la izquierda), y Colorado sobre su barra
  # vertical; Mapocho un poco mas abajo, porque Yeso queda casi a la misma altura
  lado <- c("Olivares" = -1, "Colorado" = 0, "Yeso" = 1, "Volcán" = -1, "Maipo headwaters" = 0, "Mapocho" = 1)
  dy <- c("Olivares" = 0, "Colorado" = 0, "Yeso" = 0.01, "Volcán" = 0, "Maipo headwaters" = 0, "Mapocho" = -0.03)
  for (i in seq_len(nrow(F))) {
    g <- F$g[i]
    if (lado[[g]] == 0) {
      arriba <- g == "Colorado"   # Colorado sobre su barra vertical y la cabecera bajo la suya
      text(F$MB_FB[i], F$MB_este_t1[i] + (if (arriba) 1 else -1) * (F$err_este_t1[i] + 0.03), g, adj = c(0.5, if (arriba) 0 else 1),
           cex = 0.7, font = 3, col = "gray25")
    } else {
      text(F$MB_FB[i] + lado[[g]] * (F$err_FB[i] + 0.015), F$MB_este_t1[i] + dy[[g]], g, adj = if (lado[[g]] > 0) 0 else 1,
           cex = 0.7, font = 3, col = "gray25")
    }
  }
  text(lim[1] + 0.04, lim[2] - 0.04, sprintf("%d groups\n%d of %d within error", nrow(FB), round(pF / 100 * nrow(FB)), nrow(FB)),
       adj = c(0, 1), cex = 0.75)
  box(col = gris, lwd = 0.6)
  titulo("(c) 2001 to 2013, sub-basins")
}
guardar("Figura5_contraste", 7.8, quote({
  par(mfrow = c(1, 3), mar = c(3.9, 3.3, 1.7, 1.0), oma = c(1.3, 0, 0, 0), mgp = c(2.0, 0.45, 0), las = 1, cex = 1, tcl = -0.25)
  panel(Du$MB, Du$MB_propio, Du$area_km2, Du$g, "Dussaillant et al. (2019)",
        "(a) 2001 to April 2018", lin(nrow(Du), cor(Du$MB, Du$MB_propio), pD))
  panel(Hu$Hug, Hu$MB20, Hu$area_km2, Hu$g, "Hugonnet et al. (2021)",
        "(b) 2001 to 2019", lin(nrow(Hu), cor(Hu$Hug, Hu$MB20), pH), ylab = FALSE)
  panel_fb()
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE); plot.new()
  legend("bottom", GRUPOS, pt.bg = COL_G, pch = 21, col = "white", pt.cex = 1.2, cex = 0.85, bty = "n", text.font = 3,
         horiz = TRUE, x.intersp = 0.6, text.width = NA)
}))
cat(sprintf("Contraste: Dussaillant %d glaciares, r %s, %.1f%% dentro; Hugonnet %d glaciares, r %s, %.1f%% dentro; Farias-Barahona %d%% de las subcuencas dentro\n",
            nrow(Du), pt(cor(Du$MB, Du$MB_propio)), pD, nrow(Hu), pt(cor(Hu$Hug, Hu$MB20)), pH, round(pF)))

# --- opcional: los tres inventarios por grupo (CPD) ------------------------------------------------
A <- leer("M06_Agregados.csv")
filas <- c(paste("Maipo |", c("Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")), "Cuenca alta del Maipo")
INV <- c(dga = "National inventory", rgi70 = "RGI 7.0", rgi60 = "RGI 6.0")
colI <- c(dga = azul, rgi70 = "#7FA7C9", rgi60 = "#C9D6E3")
agr <- function(conj, grupo) A[A$conjunto == conj & A$grupo == grupo, ]
B <- sapply(names(INV), function(c) sapply(filas, function(g) agr(c, g)$balance_850))
Mm <- sapply(names(INV), function(c) sapply(filas, function(g) agr(c, g)$dM_Mt_850 / 1000))
nom <- c("Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Headwaters", "Basin")
guardar("Figura_opcional_inventarios", 8.2, quote({
  par(mfrow = c(1, 2), mar = c(4.6, 3.8, 1.8, 0.6), mgp = c(2.4, 0.55, 0), las = 1, cex = 1, tcl = -0.25)
  bp <- barplot(t(B), beside = TRUE, col = colI, border = NA, ylim = c(min(B) * 1.15, 0), names.arg = rep("", nrow(B)),
                ylab = expression("Mass balance (m w.e. a"^"\u22121"*")"), axes = FALSE)
  at <- pretty(c(min(B) * 1.15, 0)); axis(2, at = at, labels = menos(at))
  abline(h = 0, col = "gray40")
  text(colMeans(bp), par("usr")[3] - 0.03, nom, srt = 35, adj = c(1, 1), xpd = TRUE, cex = 0.95, font = 3)
  legend("bottomright", INV, fill = colI, border = NA, bty = "n", cex = 0.9)
  titulo("(a) Specific mass balance")
  Mg <- Mm[-nrow(Mm), ]
  bp <- barplot(t(Mg), beside = TRUE, col = colI, border = NA, ylim = c(min(Mg) * 1.15, 0), names.arg = rep("", nrow(Mg)),
                ylab = "Mass change 2001\u20132025 (Gt)", axes = FALSE)
  at <- pretty(c(min(Mg) * 1.15, 0)); axis(2, at = at, labels = menos(at))
  abline(h = 0, col = "gray40")
  text(colMeans(bp), par("usr")[3] - 0.02, nom[-length(nom)], srt = 35, adj = c(1, 1), xpd = TRUE, cex = 0.95, font = 3)
  mtext(sprintf("Basin total: %s, %s and %s Gt", menos(pt(Mm[nrow(Mm), 1])), menos(pt(Mm[nrow(Mm), 2])), menos(pt(Mm[nrow(Mm), 3]))),
        side = 1, line = 3.6, cex = 0.85, col = gris)
  titulo("(b) Mass change by group")
}))
cat("Listo: Figura4_contraste y Figura_opcional_inventarios\n")
