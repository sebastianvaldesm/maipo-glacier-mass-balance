# F01. Figura 1 del articulo: la cuenca alta del Maipo con sus seis grupos de subcuencas, los glaciares
# del inventario de la DGA y la ubicacion en Chile. Los rios son de OpenStreetMap (datos/rios_osm.json).
# Sale: figuras/Figura1_zona.png y .jpg (16 cm de ancho, 300 dpi)
suppressPackageStartupMessages({ library(terra); library(jsonlite) })
terraOptions(progress = 0)
base <- "C:/maipo/proyecto"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
dir.create(file.path(AQUI, "figuras"), showWarnings = FALSE)
f_dga <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventario_DGA_2014_cuenca.gpkg")
f_rm  <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "RGI_RM_UTM19S.gpkg")
f_ssc <- file.path(base, "01_Datos/Brutos", "Cuencas_DGA", "SubSubCuencas_BNA_057_054_060.geojson")
f_gad <- file.path(base, "01_Datos/Capas_Auxiliares", "gadm41_CHL_1.json")
f_rio <- file.path(AQUI, "datos", "rios_osm.json")

azul <- "#1B4F72"; gris <- "#55606B"; agua <- "#2471A3"
COL_G <- c("Mapocho" = "#009E73", "Olivares" = "#E69F00", "Colorado" = "#CC79A7", "Yeso" = "#56B4E9",
           "Volcán" = "#1F3C88", "Maipo headwaters" = "#8C6D31")
GRUPOS <- names(COL_G)
GRUPO_SSC <- c("05720" = "Mapocho", "05721" = "Mapocho", "05706" = "Olivares", "05705" = "Colorado",
               "05703" = "Yeso", "05702" = "Volcán", "05700" = "Maipo headwaters", "05701" = "Maipo headwaters")
tinte <- function(col, f = 0.5) sapply(col, function(k) colorRampPalette(c("white", k))(101)[round(100 * f) + 1])
halo <- function(x, y, lab, cex = 1, font = 2, col = "black", adj = 0.5, r = 0.09, srt = 0) {
  d <- r * strheight("M", cex = cex)
  for (a in seq(0, 2 * pi, length.out = 17)[-17])
    text(x + d * cos(a) * 1.6, y + d * sin(a), lab, cex = cex, font = font, col = "white", adj = adj, srt = srt)
  text(x, y, lab, cex = cex, font = font, col = col, adj = adj, srt = srt)
}

# --- capas ------------------------------------------------------------------------------------
dga <- vect(f_dga, "dga"); crs_utm <- crs(dga)
dga$grupo <- unname(GRUPO_SSC[dga$COD_SSCUEN])
ssc <- project(vect(f_ssc), crs_utm); ssc$grupo <- unname(GRUPO_SSC[ssc$COD_SSUBC])
alta <- ssc[ssc$COD_SUBC %in% "0570" | ssc$COD_SSUBC %in% c("05720", "05721")]
grp <- aggregate(alta[!is.na(alta$grupo)], "grupo"); cont <- aggregate(alta)
lim_cl <- vect(f_rm, "limite_chile")
stgo <- project(vect(cbind(-70.65, -33.44), crs = "EPSG:4326"), crs_utm); xy_stgo <- crds(stgo)[1, ]
ec <- ext(cont)
# extension hacia el oeste hasta Santiago, con 3 km de margen en los otros lados
E <- ext(xy_stgo[1] - 7000, ec$xmax + 3000, ec$ymin - 3000, ec$ymax + 3000)
dx <- E$xmax - E$xmin; dy <- E$ymax - E$ymin

dem_ll <- vrt(list.files(file.path(base, "01_Datos/Brutos", "ASTER_GDEM_V3"), "_dem\\.tif$", full.names = TRUE))
bb <- ext(project(as.polygons(E, crs = crs_utm), "EPSG:4326"))
dem <- project(crop(dem_ll, ext(bb$xmin - 0.05, bb$xmax + 0.05, bb$ymin - 0.05, bb$ymax + 0.05)),
               rast(E, resolution = 90, crs = crs_utm), method = "bilinear")
relieve <- shade(terrain(dem, "slope", unit = "radians"), terrain(dem, "aspect", unit = "radians"), angle = 40, direction = 315)

# rios: OpenStreetMap si esta; si no, los cauces del GDEM con mas de 150 km2 de area aportante
rios <- NULL
if (file.exists(f_rio) && grepl("^\\s*\\{", readChar(f_rio, 5))) {
  J <- fromJSON(f_rio, simplifyVector = FALSE)$elements
  J <- Filter(function(e) !is.null(e$geometry) && !is.null(e$tags$name), J)
  if (length(J)) {
    geoms <- lapply(seq_along(J), function(i) cbind(i, 1, t(sapply(J[[i]]$geometry, function(p) c(p$lon, p$lat)))))
    rios <- vect(do.call(rbind, geoms), type = "lines", crs = "EPSG:4326")
    rios$nombre <- sapply(J, function(e) e$tags$name)
    rios <- crop(project(rios, crs_utm), E)
    fuente_rios <- "OpenStreetMap"
  }
}
if (is.null(rios)) {
  fuente_rios <- "GDEM"
  fd <- terrain(dem, "flowdir")
  if (exists("flowAccumulation", where = asNamespace("terra"))) {
    fa <- terra::flowAccumulation(fd)
    cauce <- classify(fa * prod(res(dem)) / 1e6 > 150, cbind(0, NA))
    rios <- as.lines(as.polygons(cauce))
    rios$nombre <- NA
  } else warning("Sin rios: no hay datos/rios_osm.json y esta version de terra no tiene flowAccumulation")
}
principal <- function(v) !is.na(v$nombre) & grepl("^R[íi]o (Maipo|Mapocho)$", v$nombre)

# --- tamanos: 16 cm de ancho; el panel (a) se dimensiona para que el mapa llene su cuadro con asp = 1
pulg <- 1 / 2.54
W <- 16 * pulg
mai_b <- c(0.46, 0.10, 0.22, 0.40)        # panel (b): abajo, izquierda, arriba, derecha (pulgadas)
ancho_a <- 9.3 * pulg                      # ancho del panel (a)
alto_mapa <- (ancho_a - 0.06) * dy / dx
H <- alto_mapa + mai_b[1] + mai_b[3]
mai_a <- c(mai_b[1], 0.03, mai_b[3], 0.03)
fa_a <- ancho_a / W

dibujo <- function() {
  # (a) mapa
  par(fig = c(0, fa_a, 0, 1), mai = mai_a, xaxs = "i", yaxs = "i", cex = 1)
  plot(NA, xlim = c(E$xmin, E$xmax), ylim = c(E$ymin, E$ymax), asp = 1, axes = FALSE, xlab = "", ylab = "")
  plot(relieve, col = gray.colors(100, 0.60, 1), legend = FALSE, axes = FALSE, add = TRUE)
  rect(E$xmin, E$ymin, E$xmax, E$ymax, col = adjustcolor("white", 0.45), border = NA)
  for (g in GRUPOS) plot(grp[grp$grupo == g], col = adjustcolor(tinte(COL_G[[g]], 0.55), 0.80), border = NA, add = TRUE)
  for (g in GRUPOS) lines(grp[grp$grupo == g], col = "white", lwd = 1.2)
  lines(cont, col = "black", lwd = 1.1)
  lines(crop(as.lines(lim_cl), E), col = "black", lwd = 0.8, lty = 2)
  if (!is.null(rios)) {
    lines(rios[!principal(rios)], col = agua, lwd = 0.7)
    lines(rios[principal(rios)], col = agua, lwd = 1.4)
  }
  plot(dga, col = "black", border = NA, add = TRUE)
  points(stgo, pch = 15, cex = 1.1, col = "#7B241C")
  halo(xy_stgo[1] + 2000, xy_stgo[2] + 3200, "Santiago", cex = 0.95, font = 2, adj = c(0, 0.5))
  # nombres de los grupos
  pos <- list(Mapocho = c(0, 0), Olivares = c(1500, -2500), Colorado = c(2000, 3500), Yeso = c(-3500, 0),
              "Volcán" = c(-4500, 1500), "Maipo headwaters" = c(-5000, 6000))
  for (g in GRUPOS) {
    c1 <- crds(centroids(grp[grp$grupo == g], inside = TRUE)); p <- pos[[g]]
    halo(c1[1] + p[1], c1[2] + p[2], if (g == "Maipo headwaters") "Maipo\nheadwaters" else g, cex = 0.9, font = 4, col = "gray10")
  }
  # nombres de los dos rios principales, en cursiva
  if (!is.null(rios) && any(principal(rios))) {
    for (nm in c("Río Maipo", "Río Mapocho")) {
      r <- rios[!is.na(rios$nombre) & rios$nombre == nm]
      if (!nrow(r)) next
      xy <- crds(r); xy <- xy[xy[, 1] < xy_stgo[1] + 30000 & xy[, 1] > E$xmin + 3000, , drop = FALSE]
      if (!nrow(xy)) next
      k <- xy[which.min(abs(xy[, 1] - (xy_stgo[1] + if (nm == "Río Maipo") 14000 else 9000))), ]
      halo(k[1], k[2] + if (nm == "Río Maipo") -2600 else 2600, if (nm == "Río Maipo") "Maipo River" else "Mapocho River",
           cex = 0.85, font = 3, col = agua)
    }
  }
  # escala y norte
  x0 <- E$xmin + 25000; y0 <- E$ymin + 3000
  rect(x0, y0, x0 + 10000, y0 + 600, col = "black", border = NA); rect(x0 + 10000, y0, x0 + 20000, y0 + 600, col = "white", border = "black", lwd = 0.5)
  text(c(x0, x0 + 10000, x0 + 20000), y0 + 2300, c("0", "10", "20 km"), cex = 0.8)
  arrows(E$xmax - 4500, E$ymax - 14000, E$xmax - 4500, E$ymax - 8000, length = 0.06, lwd = 1.3)
  text(E$xmax - 4500, E$ymax - 6000, "N", font = 2, cex = 0.9)
  # leyenda: en el margen de abajo, en dos filas, para no tapar la cuenca (los grupos van rotulados en el mapa)
  par(xpd = NA)
  fila1 <- E$ymin - 0.030 * dy; fila2 <- E$ymin - 0.058 * dy; L <- 0.028 * dx
  xs <- E$xmin + c(0.00, 0.47, 0.73) * dx
  rect(xs[1], fila1 - 0.006 * dy, xs[1] + L, fila1 + 0.006 * dy, col = "black", border = NA)
  text(xs[1] + 1.3 * L, fila1, "Glacier, national inventory (2014)", adj = 0, cex = 0.8)
  segments(xs[2], fila1, xs[2] + L, fila1, col = agua, lwd = 1.4)
  text(xs[2] + 1.3 * L, fila1, "River", adj = 0, cex = 0.8)
  points(xs[3] + L / 2, fila1, pch = 15, cex = 1.0, col = "#7B241C")
  text(xs[3] + 1.3 * L, fila1, "City", adj = 0, cex = 0.8)
  segments(xs[1], fila2, xs[1] + L, fila2, col = "black", lwd = 1.1)
  text(xs[1] + 1.3 * L, fila2, "Basin outline", adj = 0, cex = 0.8)
  segments(xs[3], fila2, xs[3] + L, fila2, col = "black", lwd = 0.8, lty = 2)
  text(xs[3] + 1.3 * L, fila2, "Chile–Argentina border", adj = 0, cex = 0.8)
  par(xpd = FALSE)
  box(col = gris, lwd = 0.6)
  mtext("(a)", side = 3, line = 0.15, adj = 0, cex = 1, font = 2, col = azul)
  # recuadro de ubicacion, abajo a la izquierda (fuera de la cuenca)
  par(fig = c(0.012, 0.012 + 2.0 * pulg / W, mai_a[1] / H + 0.012, mai_a[1] / H + 0.012 + 3.0 * pulg / H), new = TRUE,
      mai = c(0, 0, 0, 0))
  caja <- ext(-74.5, -66.5, -38.5, -28.5)
  reg <- crop(vect(f_gad), caja)
  plot(NA, xlim = c(caja$xmin, caja$xmax), ylim = c(caja$ymin, caja$ymax), asp = 1.2, axes = FALSE, xlab = "", ylab = "")
  rect(caja$xmin, caja$ymin, caja$xmax, caja$ymax, col = "#EAF2F8", border = NA)
  plot(aggregate(reg), col = "#E8F3E5", border = "#7F8C8D", lwd = 0.4, add = TRUE)
  rect(-74.5, -38.5, -66.5, -28.5, border = gris, lwd = 0.6)
  be <- ext(project(as.polygons(E, crs = crs_utm), "EPSG:4326"))
  rect(be$xmin, be$ymin, be$xmax, be$ymax, border = "#B03A3A", lwd = 1.2)
  text(-73.2, -31.0, "Chile", cex = 0.7, font = 3, col = "#2E7D32")
  text(-67.9, -33.2, "Argentina", cex = 0.6, font = 3, col = "#B5651D", srt = 90)
  text(-73.4, -36.6, "Pacific\nOcean", cex = 0.6, font = 3, col = agua)

  # (b) cota mediana contra la posicion norte-sur, en el mismo eje vertical que el mapa
  par(fig = c(fa_a, 1, 0, 1), mai = mai_b, new = TRUE, xaxs = "i", yaxs = "i", mgp = c(1.8, 0.45, 0), cex = 1)
  c0 <- crds(centroids(dga, inside = TRUE))
  amax <- max(dga$AREA_Km2); tam <- function(a) 0.35 + 2.3 * sqrt(a / amax)
  plot(NA, xlim = c(2700, 6400), ylim = c(E$ymin, E$ymax), axes = FALSE, xlab = "Median elevation (m a.s.l.)", ylab = "")
  abline(v = seq(3000, 6000, 500), col = "gray90", lwd = 0.6)
  o <- order(-dga$AREA_Km2)
  points(dga$HMEDIANA[o], c0[o, 2], pch = 21, bg = adjustcolor(COL_G[dga$grupo[o]], 0.9), col = "white", lwd = 0.3,
         cex = tam(dga$AREA_Km2[o]))
  axis(1, at = seq(3000, 6000, 1000), labels = formatC(seq(3000, 6000, 1000), big.mark = ",", format = "d"),
       tcl = -0.25, cex.axis = 0.9)
  # latitud en el eje derecho
  la <- seq(-33.2, -34.2, by = -0.2)
  yl <- crds(project(vect(cbind(-70.1, la), crs = "EPSG:4326"), crs_utm))[, 2]
  axis(4, at = yl, labels = sprintf("%.1f\u00B0 S", abs(la)), las = 1, tcl = -0.25, cex.axis = 0.85, mgp = c(1.8, 0.35, 0))
  box(col = gris, lwd = 0.6)
  # leyenda del tamano: area del glaciar, en km2, del menor al mayor del inventario
  ref <- c(0.1, 1, 5, 20)
  lyb <- E$ymin + 0.03 * dy; xb <- 5650
  rect(4950, lyb - 0.012 * dy, 6380, lyb + 0.205 * dy, col = "white", border = gris, lwd = 0.4)
  text(5665, lyb + 0.185 * dy, "Glacier area (km²)", cex = 0.72)
  for (i in seq_along(ref)) {
    y <- lyb + (i - 0.5) * 0.04 * dy
    points(xb - 380, y, pch = 21, bg = "gray60", col = "white", cex = tam(ref[i]))
    text(xb - 50, y, formatC(ref[i], format = "fg"), adj = 0, cex = 0.72)
  }
  text(5665, lyb - 0.004 * dy + 0.0, sprintf("range %s to %s", formatC(min(dga$AREA_Km2), format = "f", digits = 2),
                                               formatC(amax, format = "f", digits = 1)), cex = 0.62, col = gris)
  mtext("(b)", side = 3, line = 0.15, adj = 0, cex = 1, font = 2, col = azul)
}

for (fmt in c("png", "jpg")) {
  f <- file.path(AQUI, "figuras", paste0("Figura1_zona.", fmt))
  if (fmt == "png") png(f, width = W, height = H, units = "in", res = 300, pointsize = 8, type = "cairo")
  if (fmt == "jpg") jpeg(f, width = W, height = H, units = "in", res = 300, pointsize = 8, quality = 95, type = "cairo")
  dibujo(); invisible(dev.off())
}
cat(sprintf("Figura 1: %.1f x %.1f cm, rios de %s\n", W * 2.54, H * 2.54, fuente_rios))
cat("Area de los glaciares:", range(dga$AREA_Km2), "\n")
