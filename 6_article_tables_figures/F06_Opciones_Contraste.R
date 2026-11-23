# F06 opciones. Tres formas de mostrar el contraste con lo publicado (la Figura 6 del articulo), para elegir una:
#   A, pares publicado / este estudio con sus errores, una fila por comparacion (valores de la Tabla A1);
#   B, linea de tiempo por carriles, una barra por estudio y periodo, coloreada por el balance;
#   C, distribucion de los glaciares (violines partidos, ponderados por area) de cada estudio y este estudio.
# Sale: figuras/Figura6_opcion_A.png, _B.png y _C.png (y .jpg).
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
RAMA <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas"
leer <- function(f, d = RAMA) read.csv(file.path(d, f), stringsAsFactors = FALSE)
si <- function(x) x %in% c(TRUE, "TRUE")
gris <- "#55606B"; azul <- "#1B4F72"; pub <- "#8C8C8C"
f2 <- function(x) { s <- formatC(abs(x), format = "f", digits = 2); ifelse(round(x, 2) < 0, paste0("\u2212", s), ifelse(round(x, 2) > 0, paste0("+", s), s)) }
eje <- function(at) axis(1, at = at, labels = f2(at))
unid <- expression("Mass balance (m w.e. a"^"\u22121"*")")

# --- valores de la cuenca (los mismos de la Tabla A1) -------------------------------------------------
DU <- leer("M06_Dussaillant_resumen.csv"); DU <- DU[DU$grupo == "Cuenca alta del Maipo", ]
HU <- leer("M06c_Hugonnet_resumen.csv"); HU <- HU[HU$grupo == "Cuenca alta del Maipo", ]
FB <- leer("Figura5_Farias_seis_grupos.csv", file.path(AQUI, "tablas"))
EC <- leer("M06_Echaurren.csv"); EC <- EC[EC$conjunto == "rgi60", ]
TT <- leer("Tramos_2010_Cuenca_Grupos.csv", file.path(base, "06_Memoria_Final", "Tramos_2010"))
TT <- TT[TT$corte == 2010.5 & TT$inventario == "dga" & TT$grupo == "Cuenca alta del Maipo", ]
wm <- function(x, w) sum(x * w) / sum(w)
# balance de un periodo propio con el error de la tasa por remuestreo y el de la densidad (como en M06)
bal <- function(tasa, e) c(tasa * 0.85, sqrt((e * 850)^2 + (tasa * 60)^2) / 1000)
p1 <- bal(TT$tasa_2001_2010, TT$error_2001_2010_boot); p2 <- bal(TT$tasa_2010_2025, TT$error_2010_2025_boot)
pt <- bal(TT$tasa_2001_2025, TT$error_boot)
C <- data.frame(
  estudio = c("Dussaillant et al. (2019)", "Hugonnet et al. (2021)", "Hugonnet et al. (2021), first decade",
              "Hugonnet et al. (2021), second decade", "Farías-Barahona et al. (2020)", "McPhee et al. (2026), Echaurren Norte"),
  t0 = c(2000, 2000, 2000, 2010, 2000, 2009), t1 = c(2018.3, 2020, 2010, 2020, 2013, 2024),
  e0 = c(2001, 2001, 2001, 2010, 2001, 2009), e1 = c(2018.3, 2019.3, 2010, 2019.3, 2013.3, 2024),
  publ = c(DU$MB_Dussaillant, HU$MB_Hugonnet, HU$MB_Hugonnet_d1, HU$MB_Hugonnet_d2, wm(FB$MB_FB, FB$area_FB), -1.57),
  publ_e = c(DU$err_Dussaillant, HU$err_Hugonnet, HU$err_Hugonnet_d1, HU$err_Hugonnet_d2, wm(FB$err_FB, FB$area_FB), NA),
  este = c(DU$MB_este, HU$MB_este, HU$MB_este_d1, HU$MB_este_d2, wm(FB$MB_este_t1, FB$area_este), EC$balance_2009_2024),
  este_e = c(DU$err_este, HU$err_este, HU$err_este_d1, HU$err_este_d2, wm(FB$err_este_t1, FB$area_este), EC$error_balance_2009_2024),
  escala = c("291 glaciers", "280 glaciers", "245 glaciers", "245 glaciers", "6 sub-basins", "1 glacier"),
  stringsAsFactors = FALSE)
print(C)
guardar <- function(nombre, H, expr) for (fmt in c("png", "jpg")) {
  f <- file.path(AQUI, "figuras", paste0(nombre, ".", fmt))
  if (fmt == "png") png(f, width = 16 / 2.54, height = H / 2.54, units = "in", res = 300, pointsize = 8.5, type = "cairo")
  if (fmt == "jpg") jpeg(f, width = 16 / 2.54, height = H / 2.54, units = "in", res = 300, pointsize = 8.5, quality = 95, type = "cairo")
  eval(expr); invisible(dev.off())
}

# --- A: pares publicado / este estudio ----------------------------------------------------------------
guardar("Figura6_opcion_A", 9.5, quote({
  n <- nrow(C); y <- rev(seq_len(n)) + 1.2
  par(mar = c(3.4, 15.5, 0.8, 0.8), mgp = c(2.2, 0.5, 0), las = 1, tcl = -0.25)
  plot(NA, xlim = c(-1.75, 0.35), ylim = c(0.4, max(y) + 0.6), yaxt = "n", xaxt = "n", xlab = unid, ylab = "", yaxs = "i")
  eje(seq(-1.5, 0.25, 0.25)); abline(v = 0, col = "gray80")
  # banda del periodo completo de este estudio como referencia
  rect(pt[1] - pt[2], -1, pt[1] + pt[2], 99, col = adjustcolor(azul, 0.10), border = NA)
  abline(v = pt[1], col = azul, lty = 3)
  for (i in seq_len(n)) {
    segments(C$publ[i], y[i], C$este[i], y[i], col = "gray70", lwd = 2.2)
    if (!is.na(C$publ_e[i])) arrows(C$publ[i] - C$publ_e[i], y[i] + 0.12, C$publ[i] + C$publ_e[i], y[i] + 0.12, angle = 90, code = 3, length = 0.025, col = pub)
    arrows(C$este[i] - C$este_e[i], y[i] - 0.12, C$este[i] + C$este_e[i], y[i] - 0.12, angle = 90, code = 3, length = 0.025, col = azul)
    points(C$publ[i], y[i], pch = 21, bg = "white", col = pub, cex = 1.5, lwd = 1.4)
    points(C$este[i], y[i], pch = 21, bg = azul, col = "white", cex = 1.5, lwd = 0.6)
  }
  per <- sprintf("%s\u2013%s", c("2000", "2000", "2000", "2010", "2000", "2009"), c("Apr 2018", "2020", "2010", "2020", "2013", "2024"))
  mtext(C$estudio, side = 2, at = y + 0.15, line = 0.4, cex = 0.82, adj = 1)
  mtext(paste0(per, ", ", C$escala), side = 2, at = y - 0.2, line = 0.4, cex = 0.72, adj = 1, col = gris)
  # ultima fila, este estudio solo
  points(pt[1], 1, pch = 21, bg = azul, col = "white", cex = 1.6)
  arrows(pt[1] - pt[2], 1, pt[1] + pt[2], 1, angle = 90, code = 3, length = 0.025, col = azul)
  points(p1[1], 1, pch = 24, bg = "white", col = azul, cex = 1.2); points(p2[1], 1, pch = 25, bg = azul, col = azul, cex = 1.2)
  mtext("This study", side = 2, at = 1.15, line = 0.4, cex = 0.82, adj = 1, font = 2)
  mtext("2001\u20132025, 2001\u20132010 and 2010\u20132025", side = 2, at = 0.8, line = 0.4, cex = 0.72, adj = 1, col = gris)
  abline(h = 1.6, col = "gray85")
  legend("topleft", c("Published", "This study, same period and outlines", "This study 2001\u20132010", "This study 2010\u20132025"),
         pch = c(21, 21, 24, 25), pt.bg = c("white", azul, "white", azul), col = c(pub, "white", azul, azul), pt.cex = 1.2,
         bty = "n", cex = 0.78, inset = c(0.01, 0.01))
  box(col = gris, lwd = 0.6)
}))

# --- B: linea de tiempo por carriles ------------------------------------------------------------------
pal <- colorRampPalette(c("#7F0000", "#D7301F", "#FC8D59", "#FDD49E", "#FFF7EC", "#C6DBEF", "#4292C6"))(140)
colv <- function(v) pal[pmax(1, pmin(140, round((v + 1.6) / 1.9 * 139) + 1))]   # escala de -1.6 a +0.3
guardar("Figura6_opcion_B", 10.5, quote({
  par(mar = c(3.0, 13.5, 0.8, 4.2), mgp = c(2.0, 0.5, 0), las = 1, tcl = -0.25)
  n <- nrow(C); filas <- 2 * n + 1; y <- filas:1
  plot(NA, xlim = c(1999.5, 2026.5), ylim = c(0.3, filas + 1.1), xaxs = "i", yaxs = "i", yaxt = "n", xlab = "Year", ylab = "")
  rect(2010, -1, 2022.99, 99, col = "gray95", border = NA)
  text(2016.5, filas + 0.75, "Megadrought", col = "gray45", cex = 0.8, font = 3)
  barra <- function(t0, t1, yy, v, borde) {
    rect(t0, yy - 0.36, t1, yy + 0.36, col = colv(v), border = borde, lwd = 0.8)
    text((t0 + t1) / 2, yy, f2(v), cex = 0.78, col = if (v < -0.9) "white" else "black")
  }
  for (i in seq_len(n)) {
    ya <- y[2 * i - 1]; yb <- y[2 * i]
    barra(C$t0[i], C$t1[i], ya, C$publ[i], "gray40"); barra(C$e0[i], C$e1[i], yb, C$este[i], azul)
    mtext(C$estudio[i], side = 2, at = ya, line = 0.4, cex = 0.75, adj = 1)
    mtext("This study", side = 2, at = yb, line = 0.4, cex = 0.75, adj = 1, col = azul)
    if (i < n) abline(h = yb - 0.5, col = "gray85")
  }
  abline(h = 1.5, col = "gray60")
  barra(2001, 2010.5, 1, p1[1], azul); barra(2010.5, 2026, 1, p2[1], azul)
  mtext("This study, two periods", side = 2, at = 1, line = 0.4, cex = 0.75, adj = 1, col = azul, font = 2)
  box(col = gris, lwd = 0.6)
  # barra de colores a la derecha
  par(xpd = NA); u <- par("usr"); x0 <- u[2] + 0.6; x1 <- x0 + 0.7
  vv <- seq(-1.6, 0.3, length.out = 140); yy <- seq(u[3] + 1, u[4] - 1, length.out = 141)
  rect(x0, yy[-141], x1, yy[-1], col = pal, border = NA); rect(x0, yy[1], x1, yy[141], border = gris, lwd = 0.5)
  for (t in c(-1.5, -1, -0.5, 0)) { yt <- yy[1] + (t + 1.6) / 1.9 * (yy[141] - yy[1]); segments(x1, yt, x1 + 0.2, yt); text(x1 + 0.3, yt, f2(t), adj = 0, cex = 0.72) }
  text(x0, yy[141] + 0.6, expression("m w.e. a"^"\u22121"), adj = 0, cex = 0.72)
  par(xpd = FALSE)
}))

# --- C: distribucion de los glaciares, violines partidos ---------------------------------------------
Du <- leer("M06_Dussaillant_por_glaciar.csv"); Du <- Du[si(Du$en_maipo) & !is.na(Du$MB_propio) & !is.na(Du$MB), ]
Hu <- leer("M06c_Hugonnet_por_glaciar.csv"); Hu <- Hu[si(Hu$en_maipo) & si(Hu$comparable), ]
G <- leer("M06_Tasa_por_Glaciar.csv", file.path(base, "06_Memoria_Final", "Tramos_2010", "corte_2010.5", "tablas"))
G <- G[G$conjunto == "dga" & si(G$en_maipo) & !is.na(G$balance_850), ]
cq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; sapply(p, function(pp) x[which(cumsum(w) / sum(w) >= pp)[1]]) }
media_violin <- function(x, w, at, lado, col, ancho = 0.42) {
  d <- density(x, weights = w / sum(w), from = -1.6, to = 0.6, bw = 0.07)
  s <- d$y / max(d$y) * ancho
  k <- s > 0.004   # solo donde hay glaciares, para que el borde no cruce todo el grafico
  polygon(c(at + lado * s[k], at, at), c(d$x[k], max(d$x[k]), min(d$x[k])), col = adjustcolor(col, 0.55), border = col, lwd = 0.7)
  q <- cq(x, w, c(0.25, 0.5, 0.75))
  segments(at, q[1], at, q[3], lwd = 3, col = "gray20", lend = 1)
  segments(at + lado * 0.02, q[2], at + lado * 0.2, q[2], lwd = 2, col = "gray10")
  text(at + lado * 0.24, q[2], f2(q[2]), adj = if (lado > 0) 0 else 1, cex = 0.75)
}
guardar("Figura6_opcion_C", 9.0, quote({
  par(mar = c(4.2, 4.0, 0.8, 0.8), mgp = c(2.5, 0.5, 0), las = 1, tcl = -0.25)
  plot(NA, xlim = c(0.4, 4.6), ylim = c(-1.6, 0.6), xaxt = "n", yaxt = "n", xlab = "", ylab = unid, yaxs = "i")
  axis(2, at = seq(-1.5, 0.5, 0.5), labels = f2(seq(-1.5, 0.5, 0.5))); abline(h = 0, col = "gray80")
  media_violin(Du$MB, Du$area_km2, 1, -1, pub); media_violin(Du$MB_propio, Du$area_km2, 1, 1, azul)
  media_violin(Hu$Hug, Hu$area_km2, 2, -1, pub); media_violin(Hu$MB20, Hu$area_km2, 2, 1, azul)
  # Farias-Barahona: solo seis grupos, como puntos con su area
  sz <- function(a) 0.8 + 1.8 * sqrt(a / max(FB$area_este))
  points(rep(2.85, nrow(FB)), FB$MB_FB, pch = 21, bg = "white", col = pub, cex = sz(FB$area_FB), lwd = 1.2)
  points(rep(3.15, nrow(FB)), FB$MB_este_t1, pch = 21, bg = adjustcolor(azul, 0.8), col = "white", cex = sz(FB$area_este))
  segments(2.85, FB$MB_FB, 3.15, FB$MB_este_t1, col = "gray75")
  media_violin(G$balance_850, G$area_utm_km2, 4, 1, "black", ancho = 0.5)
  mtext(c("Dussaillant et al. (2019)", "Hugonnet et al. (2021)", "Farías-Barahona et al. (2020)", "This study"), side = 1,
        at = 1:4, line = 0.6, cex = 0.8)
  mtext(c("2001\u2013Apr 2018, 291 glaciers", "2001\u20132019, 280 glaciers", "2001\u20132013, 6 sub-basins", "2001\u20132025, 300 glaciers"),
        side = 1, at = 1:4, line = 1.6, cex = 0.72, col = gris)
  legend("bottomleft", c("Published", "This study, same period"), fill = adjustcolor(c(pub, azul), 0.55), border = c(pub, azul),
         bty = "n", cex = 0.8, inset = c(0.01, 0.01))
  box(col = gris, lwd = 0.6)
}))
cat("Listo: figuras/Figura6_opcion_A, _B y _C\n")
