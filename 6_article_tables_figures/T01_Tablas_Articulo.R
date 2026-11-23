# T01. Tablas del articulo con MicMac + GDEM y CPD (Tablas 2, 3 y A1), desde las tablas de la rama, los
# tramos cortados en 2010 y los contrastes. Escribe tambien las cifras que cita el texto.
# Uso: Rscript T01_Tablas_Articulo.R
# Sale: tablas/Tablas_Articulo.xlsx, tablas.json y cifras_resultados.json
suppressPackageStartupMessages({ library(jsonlite); library(writexl) })
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
RAMA <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas"
TR <- file.path(base, "06_Memoria_Final", "Tramos_2010")
dir.create(file.path(AQUI, "tablas"), showWarnings = FALSE)
leer <- function(f, d = RAMA) read.csv(file.path(d, f), stringsAsFactors = FALSE, check.names = FALSE)

RHO <- 850; S_RHO <- 60; S_AREA <- 0.05
INV <- c(dga = "National inventory", rgi70 = "RGI 7.0", rgi60 = "RGI 6.0")
GR_ES <- c("Cuenca alta del Maipo", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")
GR_EN <- setNames(c("Maipo basin", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters"), GR_ES)
GR_AG <- setNames(c("Cuenca alta del Maipo", paste("Maipo |", GR_ES[-1])), GR_ES)   # nombres en M06_Agregados

# numeros con signo menos tipografico y + en los positivos cuando el signo importa
f2 <- function(x, d = 2, mas = FALSE) {
  s <- formatC(abs(x), format = "f", digits = d)
  ifelse(round(x, d) < 0, paste0("\u2212", s), ifelse(mas & round(x, d) > 0, paste0("+", s), s))
}
pm <- function(x, e, d = 2, mas = FALSE) paste0(f2(x, d, mas), " \u00B1 ", formatC(e, format = "f", digits = d))
miles <- function(x) formatC(x, format = "d", big.mark = ",")

TT <- leer("Tramos_2010_Cuenca_Grupos.csv", TR); TT <- TT[TT$corte == 2010.5, ]
A <- leer("M06_Agregados.csv")
G <- leer("M06_Tasa_por_Glaciar.csv"); G <- G[G$en_maipo %in% c(TRUE, "TRUE"), ]
P <- leer("M05c_Pares.csv")
P$ok <- P$motivo == "aceptado"
# fechas de la serie de cada glaciar (la referencia mas las escenas que quedan tras la regla de temporada);
# sirve tambien para los glaciares sin ajuste propio, que en M06 no tienen n
SE <- leer("M05c_Serie.csv")
SE <- SE[SE$es_referencia %in% c(TRUE, "True") | SE$motivo_serie %in% "ok", ]
n_serie <- table(paste(SE$conjunto, SE$gid))
G$n_fechas <- as.integer(n_serie[paste(G$conjunto, G$gid)]); G$n_fechas[is.na(G$n_fechas)] <- 0L

# =============================================================================
# Tabla 1: datos. Texto de la tabla que estaba en el Word, con los cambios de
# la guia: categorias con los nombres de las subsecciones 3.1.1 a 3.1.5, "National glacier inventory,
# 2022 update", "Landsat 7 ETM+" y dos filas nuevas en los balances publicados (notas 164, 170, 172).
# =============================================================================
T1 <- list(
  list("Glacier inventories", "First national glacier inventory of Chile", "Reference glacier outlines", "Polygons",
       "Mainly Landsat 7 ETM+ images of March 2000 and 2001 in the basin", "Dirección General de Aguas (2014); Barcaza et al. (2017)"),
  list("", "Randolph Glacier Inventory 7.0", "Comparison outlines", "Polygons",
       "Chile, 2003 according to its attributes; Argentina, 2010 and 2011", "RGI Consortium (2023)"),
  list("", "Randolph Glacier Inventory 6.0", "Comparison outlines; comparison with Dussaillant et al. (2019) and Hugonnet et al. (2021)",
       "Polygons", "2000 to May 2003", "Pfeffer et al. (2014); RGI Consortium (2017)"),
  list("", "National glacier inventory, 2022 update", "Exclusion of glaciers and rock glaciers from the stable rock; sub-basin of each glacier",
       "Polygons", "Images of 2010 to 2022", "Dirección General de Aguas (2022)"),
  list("Elevation models", "ASTER level 1A stereo pairs, version 004", "Elevation models generated with MicMac, one per strip", "30 m",
       "2001 to 2026, January to April", "NASA/METI/AIST/Japan Spacesystems and U.S./Japan ASTER Science Team (2025); Girod et al. (2017)"),
  list("", "ASTER GDEM, version 3", "Topographic base, slope, stable rock and alignment reference", "1 arc second (about 30 m)",
       "Scenes of 2000 to 2013", "NASA/METI/AIST/Japan Spacesystems and U.S./Japan ASTER Science Team (2019); Abrams et al. (2020)"),
  list("", "TanDEM-X global elevation model", "Second alignment reference, for comparison", "90 m", "2011 to 2015", "Rizzoli et al. (2017)"),
  list("Hydrological setting", "Sub-basin boundaries", "Limits of the basin and its sub-basins", "Polygons", "Current",
       "Dirección General de Aguas (2026)"),
  list("Stable rock", "GLIMS glacier database", "Exclusion of glaciers, including Argentine rock glaciers", "Polygons", "Several dates",
       "GLIMS Consortium (2005)"),
  list("", "Global mining land use", "Exclusion of open pits, waste dumps and tailings", "Polygons", "Images of 2019", "Maus et al. (2022)"),
  list("", "ESA WorldCover 2021", "Exclusion of every cover other than bare ground, grassland or lichen", "10 m", "2021", "Zanaga et al. (2022)"),
  list("", "OpenStreetMap", "Exclusion of roads, water, streams, mines, ski areas, works and dams", "Vector", "Queried in 2026",
       "OpenStreetMap contributors (2026)"),
  list("Published mass balances", "Mass balance of each glacier", "Comparison glacier by glacier", "Per glacier (RGI 6.0)",
       "2000 to 2018 and 2000 to 2020", "Dussaillant et al. (2019); Hugonnet et al. (2021)"),
  list("", "Mass balance of each sub-basin", "Comparison by sub-basin", "Sub-basin (national inventory)", "2000 to 2013",
       "Farías-Barahona et al. (2020)"),
  list("", "Field mass balance of Echaurren Norte", "Comparison with field measurements", "Glacier", "1975 to 2024", "McPhee et al. (2026)"))
tabla1 <- list(id = "T1", titulo = "Table 1. Datasets used in this study, with their use, spatial resolution, period and source. The categories are those of Sections 3.1.1 to 3.1.5.",
               cabecera = list(list("Dataset category", "Dataset", "Dataset overview", "Spatial resolution", "Period", "Source")),
               filas = T1, anchos = c(2.3, 2.9, 3.6, 1.9, 2.6, 3.2), unir_primera = TRUE, nota = "")

# =============================================================================
# Tabla 2: datos por grupo e inventario
# =============================================================================
gid_de <- function(cj, g) {
  d <- G[G$conjunto == cj, ]
  if (g != "Cuenca alta del Maipo") d <- d[d$grupo_cuenca == g, ]
  d
}
rng <- function(x, d = 0) sprintf("%s (%s\u2013%s)", formatC(median(x), format = "f", digits = d),
                                  formatC(min(x), format = "f", digits = d), formatC(max(x), format = "f", digits = d))
filas2 <- list(); resumen2 <- list()
for (g in GR_ES) {
  fila <- list(GR_EN[[g]])
  gids_todos <- c()
  for (cj in names(INV)) {
    d <- gid_de(cj, g)
    n_esc <- d$n_fechas
    fila <- c(fila, list(as.character(nrow(d)), formatC(sum(d$area_utm_km2), format = "f", digits = 1), rng(n_esc)))
    gids_todos <- c(gids_todos, paste(cj, d$gid))
    resumen2[[length(resumen2) + 1]] <- data.frame(grupo = GR_EN[[g]], inventario = INV[[cj]], glaciares = nrow(d),
      area_km2 = sum(d$area_utm_km2), propios = sum(d$propio %in% c(TRUE, "TRUE")),
      area_propia_pct = 100 * sum(d$area_utm_km2[d$propio %in% c(TRUE, "TRUE")]) / sum(d$area_utm_km2),
      escenas_mediana = median(n_esc), escenas_min = min(n_esc), escenas_max = max(n_esc),
      error_glaciar_mediana = median(d$error_usado, na.rm = TRUE))
  }
  pp <- P[paste(P$conjunto, P$gid) %in% gids_todos, ]
  acc <- pp[pp$ok, ]
  acc_u <- acc[!duplicated(paste(acc$escena, acc$uid_alinea)), ]   # cada escena y unidad de alineacion una vez
  acc_dga <- acc[acc$conjunto == "dga", ]   # franjas: con el inventario nacional, como el texto
  fila <- c(list(fila[[1]], as.character(length(unique(acc_dga$escena)))), fila[-1],
            list(sprintf("%s (%s\u2013%s)", f2(median(acc_u$NMAD_roca), 1), f2(min(acc_u$NMAD_roca), 1), f2(max(acc_u$NMAD_roca), 1))))
  filas2[[length(filas2) + 1]] <- fila
}
R2 <- do.call(rbind, resumen2)
tabla2 <- list(id = "T2",
  titulo = paste("Table 2. ASTER data for the Maipo basin and its six groups of sub-basins with each inventory. Strips, number of ASTER strips with at least one accepted pair of scene and glacier on the national inventory.",
                 "Glaciers and area of each inventory, and number of scenes in the series of each glacier, median (minimum\u2013maximum).",
                 "Rock NMAD, normalized median absolute deviation of the elevation differences on the stable rock after the alignment, in the accepted pairs, median (minimum\u2013maximum)."),
  cabecera = list(list("Group", "Strips", "National inventory", "", "", "RGI 7.0", "", "", "RGI 6.0", "", "", "Rock NMAD (m)"),
                  list("", "", "Glaciers", "Area (km^2^)", "Scenes per glacier", "Glaciers", "Area (km^2^)", "Scenes per glacier",
                       "Glaciers", "Area (km^2^)", "Scenes per glacier", "")),
  unir_cabecera = list(c(1, 3, 3), c(1, 6, 3), c(1, 9, 3), c(1, 1, -2), c(1, 2, -2), c(1, 12, -2)),
  filas = filas2, anchos = c(2.1, 1.0, 1.35, 1.15, 1.55, 1.35, 1.15, 1.55, 1.35, 1.15, 1.55, 1.9), unir_primera = FALSE, nota = "",
  negrita_primera_fila = TRUE)

# =============================================================================
# Tabla 3: resultados por grupo e inventario, en dos versiones del error
# =============================================================================
fila_tt <- function(cj, g) TT[TT$inventario == cj & TT$grupo == g, ]
fila_ag <- function(cj, g) A[A$conjunto == cj & A$grupo == GR_AG[[g]], ]
valores3 <- function(cj, g, forma) {
  t <- fila_tt(cj, g); a <- fila_ag(cj, g)
  e <- if (forma == "fb") t$error_fb else t$error_boot
  e1 <- if (forma == "fb") t$error_2001_2010_fb else t$error_2001_2010_boot
  e2 <- if (forma == "fb") t$error_2010_2025_fb else t$error_2010_2025_boot
  r <- t$tasa_2001_2025
  rb <- sqrt((e / r)^2 + (S_RHO / RHO)^2)
  rb9 <- sqrt((e / r)^2 + (S_RHO / 900)^2)
  rm <- sqrt((e / r)^2 + S_AREA^2 + (S_RHO / RHO)^2)
  list(r = r, e = e, r1 = t$tasa_2001_2010, e1 = e1, r2 = t$tasa_2010_2025, e2 = e2,
       b = a$balance_850, eb = abs(a$balance_850) * rb, b9 = a$balance_900, eb9 = abs(a$balance_900) * rb9,
       m = a$dM_Mt_850 / 1000, em = abs(a$dM_Mt_850 / 1000) * rm, m9 = a$dM_Mt_900 / 1000)
}
VARS <- c("Rate 2001\u20132025 (m a^\u22121^)", "Rate 2001\u20132010 (m a^\u22121^)", "Rate 2010\u20132025 (m a^\u22121^)",
          "Balance, 850 kg m^\u22123^ (m w.e. a^\u22121^)", "Balance, 900 kg m^\u22123^ (m w.e. a^\u22121^)", "Mass change, 850 kg m^\u22123^ (Gt)")
tabla3 <- function(forma) {
  filas <- list(); plano <- list()
  for (g in GR_ES) {
    V <- lapply(names(INV), valores3, g = g, forma = forma)
    celdas <- list(
      sapply(V, function(v) pm(v$r, v$e)), sapply(V, function(v) pm(v$r1, v$e1, mas = TRUE)), sapply(V, function(v) pm(v$r2, v$e2)),
      sapply(V, function(v) pm(v$b, v$eb)), sapply(V, function(v) pm(v$b9, v$eb9)), sapply(V, function(v) pm(v$m, v$em)))
    for (k in seq_along(VARS)) {
      filas[[length(filas) + 1]] <- c(list(if (k == 1) GR_EN[[g]] else "", VARS[k]), as.list(celdas[[k]]))
      plano[[length(plano) + 1]] <- data.frame(grupo = GR_EN[[g]], variable = gsub("\\^", "", VARS[k]),
                                               t(setNames(celdas[[k]], INV)), check.names = FALSE)
    }
  }
  nota_err <- if (forma == "fb") {
    "The error of each rate is the mean of the errors of the glaciers weighted by area (Farías-Barahona et al., 2020)."
  } else {
    "The error of each rate comes from resampling the scenes."
  }
  list(id = paste0("T3_", forma),
       titulo = paste("Table 3. Elevation change and mass balance of the Maipo basin and its six groups of sub-basins with the three inventories, from January 2001 to January 2026, and in 2001\u20132010 and 2010\u20132025.",
                      nota_err, "The balance includes the error of the density (60 kg m^\u22123^) and the mass change also that of the area (5%)."),
       cabecera = list(list("Group", "Variable", "National inventory", "RGI 7.0", "RGI 6.0")),
       filas = filas, anchos = c(2.4, 4.8, 3.0, 3.0, 3.0), unir_primera = TRUE, bloques = length(VARS), nota = "",
       plano = do.call(rbind, plano))
}
T3fb <- tabla3("fb"); T3bo <- tabla3("boot")

# =============================================================================
# Tabla A1: contraste con lo publicado (rama con el corte en 2013.5, que es el que se compara)
# =============================================================================
DU <- leer("M06_Dussaillant_resumen.csv"); DU <- DU[DU$grupo == "Cuenca alta del Maipo", ]
HU <- leer("M06c_Hugonnet_resumen.csv"); HU <- HU[HU$grupo == "Cuenca alta del Maipo", ]
HG <- leer("M06c_Hugonnet_por_glaciar.csv"); HG <- HG[HG$en_maipo %in% c(TRUE, "TRUE") & HG$comparable_dec %in% c(TRUE, "TRUE"), ]
FBt <- leer("M06_Farias_Barahona.csv")
EC <- leer("M06_Echaurren.csv"); EC <- EC[EC$conjunto == "rgi60", ]
Gd <- G[G$conjunto == "dga", ]
Gd$COD_SSCUEN <- sprintf("%05d", as.integer(Gd$COD_SSCUEN))   # read.csv borra el cero inicial del codigo
fb_conserv <- sapply(FBt$codigos, function(cc) {
  d <- Gd[grepl(paste0("^(", cc, ")$"), Gd$COD_SSCUEN) & !is.na(Gd$error_t1_usado), ]; w <- d$area_utm_km2
  sqrt((sum(w * d$error_t1_usado) / sum(w) * RHO)^2 + (sum(w * d$tasa_t1_usada) / sum(w) * S_RHO)^2) / 1000
})
wm <- function(x, w) sum(w * x) / sum(w)
d1_fb <- wm(HG$MB_d1_err, HG$area_km2); d2_fb <- wm(HG$MB_d2_err, HG$area_km2)
SUB_EN <- c("Olivares" = "Olivares", "Colorado" = "Colorado", "Yeso" = "Yeso", "Volcán" = "Volcán",
            "Maipo alto" = "Maipo headwaters", "Molina" = "Molina", "San Francisco" = "San Francisco")
tablaA1 <- function(forma) {
  e_du <- if (forma == "fb") DU$err_este_conservador else DU$err_este
  e_hu <- if (forma == "fb") HU$err_este_conservador else HU$err_este
  e_d1 <- if (forma == "fb") d1_fb else HU$err_este_d1
  e_d2 <- if (forma == "fb") d2_fb else HU$err_este_d2
  e_fb <- if (forma == "fb") fb_conserv else FBt$err_este_t1
  filas <- list(
    list("Dussaillant et al. (2019)", sprintf("Maipo basin, %d glaciers", DU$glaciares), "RGI 6.0", "2000 to April 2018",
         pm(DU$MB_Dussaillant, DU$err_Dussaillant), pm(DU$MB_este, e_du)),
    list("Hugonnet et al. (2021)", sprintf("Maipo basin, %d glaciers", HU$glaciares), "RGI 6.0", "2000 to 2020",
         pm(HU$MB_Hugonnet, HU$err_Hugonnet), pm(HU$MB_este, e_hu)),
    list("", sprintf("First decade, %d glaciers", HU$glaciares_dec), "RGI 6.0", "2000 to 2010",
         pm(HU$MB_Hugonnet_d1, HU$err_Hugonnet_d1), pm(HU$MB_este_d1, e_d1, mas = TRUE)),
    list("", sprintf("Second decade, %d glaciers", HU$glaciares_dec), "RGI 6.0", "2010 to 2020",
         pm(HU$MB_Hugonnet_d2, HU$err_Hugonnet_d2), pm(HU$MB_este_d2, e_d2)))
  for (i in seq_len(nrow(FBt))) {
    filas[[length(filas) + 1]] <- list(if (i == 1) "Farías-Barahona et al. (2020)" else "", SUB_EN[[FBt$subcuenca[i]]],
      "National inventory", "2000 to 2013", pm(FBt$MB_FB[i], FBt$err_FB[i], mas = TRUE), pm(FBt$MB_este_t1[i], e_fb[i]))
  }
  filas[[length(filas) + 1]] <- list("McPhee et al. (2026), field measurements", "Echaurren Norte", "RGI 6.0", "2009 to 2024",
                                     "\u22121.57", pm(EC$balance_2009_2024, EC$error_balance_2009_2024))
  n_dentro <- sum(abs(FBt$MB_este_t1 - FBt$MB_FB) <= sqrt(e_fb^2 + FBt$err_FB^2))
  list(id = paste0("TA1_", forma),
       titulo = paste("Table A1. Comparison with published mass balances, in m w.e. a^\u22121^. This study is measured on the same outlines and from the first scene of 2001 to the end of each published period.",
                      if (forma == "fb") "The errors of this study are the mean of the errors of the glaciers weighted by area." else "The errors of this study come from resampling the scenes.",
                      "Published errors are those given by each study."),
       cabecera = list(list("Study", "Unit", "Outlines", "Period", "Published", "This study")),
       filas = filas, anchos = c(3.6, 3.4, 2.4, 2.4, 2.1, 2.1), unir_primera = FALSE, nota = "",
       n_farias_dentro = n_dentro, sigma_farias = round((FBt$MB_este_t1 - FBt$MB_FB) / sqrt(e_fb^2 + FBt$err_FB^2), 2))
}
TA1fb <- tablaA1("fb"); TA1bo <- tablaA1("boot")

# =============================================================================
# Cifras que citan los textos de resultados (pantalla y json)
# =============================================================================
Pd <- P[P$conjunto == "dga", ]
mot <- sort(table(Pd$motivo), decreasing = TRUE)
Pacc <- Pd[Pd$ok, ]
esc <- leer("M06_Escenas.csv"); esc$anio <- floor(esc$anio_dec)
Gdg <- G[G$conjunto == "dga", ]
cif <- list(
  franjas = nrow(esc), anios = range(esc$anio), franjas_por_anio = range(as.integer(table(factor(esc$anio, levels = 2001:2026)))),
  franjas_con_aceptado_dga = length(unique(Pacc$escena)),
  pares_dga = nrow(Pd), aceptados_dga = nrow(Pacc), pct_aceptados = 100 * nrow(Pacc) / nrow(Pd),
  motivos_pct = as.list(round(100 * c(mot) / nrow(Pd), 1)),
  nmad_roca_mediana = median(Pacc$NMAD_roca[!duplicated(paste(Pacc$escena, Pacc$uid_alinea))]),
  nmad_roca_antes_mediana = median(Pacc$NMAD_roca_antes[!duplicated(paste(Pacc$escena, Pacc$uid_alinea))], na.rm = TRUE),
  desplaz_mediano = median(Pacc$desplaz_m[!duplicated(paste(Pacc$escena, Pacc$uid_alinea))], na.rm = TRUE),
  escenas_glaciar_mediana = median(Gdg$n_fechas), escenas_fit_mediana = median(Gdg$n, na.rm = TRUE),
  propios = sum(Gdg$propio %in% c(TRUE, "TRUE")),
  area_propia_pct = 100 * sum(Gdg$area_utm_km2[Gdg$propio %in% c(TRUE, "TRUE")]) / sum(Gdg$area_utm_km2),
  tasas_rango = range(Gdg$tasa_usada), tasa_mediana = median(Gdg$tasa_usada),
  positivos = sum(Gdg$tasa_usada > 0), area_positivos = sum(Gdg$area_utm_km2[Gdg$tasa_usada > 0]),
  positivos_2sigma = sum(Gdg$tasa_usada > 2 * Gdg$error_usado),
  area_2sigma_negativo_pct = 100 * sum(Gdg$area_utm_km2[Gdg$tasa_usada < -2 * Gdg$error_usado]) / sum(Gdg$area_utm_km2),
  area_mas_1m_pct = 100 * sum(Gdg$area_utm_km2[Gdg$tasa_usada < -1]) / sum(Gdg$area_utm_km2),
  positivos_por_grupo = as.list(c(table(Gdg$grupo_cuenca[Gdg$tasa_usada > 0]))),
  farias_dentro_fb = TA1fb$n_farias_dentro, farias_dentro_boot = TA1bo$n_farias_dentro,
  farias_sigma_fb = setNames(TA1fb$sigma_farias, FBt$subcuenca), farias_sigma_boot = setNames(TA1bo$sigma_farias, FBt$subcuenca),
  dussaillant = DU, hugonnet = HU[, c("glaciares", "MB_Hugonnet", "MB_este", "pct_dentro_1", "correlacion", "glaciares_dec",
                                      "MB_Hugonnet_d1", "MB_este_d1", "MB_Hugonnet_d2", "MB_este_d2")],
  d1_fb = d1_fb, d2_fb = d2_fb, echaurren = EC)
# cota: media por area en franjas de 250 m de cota mediana
ok <- !is.na(Gdg$zmed)
fr <- seq(3000, 6000, 250); k <- cut(Gdg$zmed[ok], fr)
cif$cota <- as.list(round(sapply(split(seq_along(k), k), function(i) if (length(i) >= 3) wm(Gdg$tasa_usada[ok][i], Gdg$area_utm_km2[ok][i]) else NA), 2))
cif$mayores_5 <- c(n = sum(Gdg$area_utm_km2 > 5), tasa = wm(Gdg$tasa_usada[Gdg$area_utm_km2 > 5], Gdg$area_utm_km2[Gdg$area_utm_km2 > 5]))
cif$menores_01 <- c(n = sum(Gdg$area_utm_km2 < 0.1), tasa = wm(Gdg$tasa_usada[Gdg$area_utm_km2 < 0.1], Gdg$area_utm_km2[Gdg$area_utm_km2 < 0.1]))
cif$colorado_alto_pct <- with(Gdg[Gdg$grupo_cuenca == "Colorado", ], 100 * sum(area_utm_km2[zmed > 5000], na.rm = TRUE) / sum(area_utm_km2))
may <- Gdg[order(-Gdg$area_utm_km2), c("gid", "area_utm_km2", "grupo_cuenca", "tasa_usada", "error_usado")][1:3, ]
cif$mayores_olivares <- may
cif$tabla2 <- R2
cif$tabla3_fb <- T3fb$plano

write_json(list(tablas = list(tabla1, tabla2, T3fb[names(T3fb) != "plano"], T3bo[names(T3bo) != "plano"], TA1fb, TA1bo)),
           file.path(AQUI, "tablas", "tablas.json"), auto_unbox = TRUE, pretty = TRUE)
write_json(cif, file.path(AQUI, "tablas", "cifras_resultados.json"), auto_unbox = TRUE, pretty = TRUE, digits = 4)

# Excel con las mismas tablas (la revista pide las tablas en Word o Excel, en archivos aparte)
plano <- function(t) {
  h <- unlist(t$cabecera[[length(t$cabecera)]])
  if (length(t$cabecera) > 1) {
    h1 <- unlist(t$cabecera[[1]]); for (i in seq_along(h1)) if (!nzchar(h1[i]) && i > 1) h1[i] <- h1[i - 1]
    h <- ifelse(nzchar(h), paste(h1, h, sep = ", "), h1)
  }
  m <- do.call(rbind, lapply(t$filas, function(f) gsub("\\^", "", unlist(f))))
  colnames(m) <- make.unique(gsub("\\^", "", h)); as.data.frame(m, check.names = FALSE)
}
write_xlsx(list("Tabla 1" = plano(tabla1), "Tabla 2" = plano(tabla2), "Tabla 3 (Farias-Barahona)" = plano(T3fb),
                "Tabla 3 (remuestreo)" = plano(T3bo), "Tabla A1 (Farias-Barahona)" = plano(TA1fb),
                "Tabla A1 (remuestreo)" = plano(TA1bo)), file.path(AQUI, "tablas", "Tablas_Articulo.xlsx"))

str(cif[c("franjas", "anios", "franjas_por_anio", "franjas_con_aceptado_dga", "pares_dga", "aceptados_dga", "pct_aceptados",
          "nmad_roca_mediana", "nmad_roca_antes_mediana", "desplaz_mediano", "escenas_glaciar_mediana", "propios", "area_propia_pct",
          "tasas_rango", "tasa_mediana", "positivos", "area_positivos", "positivos_2sigma", "area_2sigma_negativo_pct",
          "area_mas_1m_pct", "colorado_alto_pct", "farias_dentro_fb", "farias_dentro_boot", "d1_fb", "d2_fb")])
print(cif$motivos_pct); print(cif$cota); print(cif$mayores_5); print(cif$menores_01); print(unlist(cif$positivos_por_grupo))
print(cif$farias_sigma_fb); print(cif$farias_sigma_boot); print(may); print(R2, digits = 3)
print(T3fb$plano[T3fb$plano$grupo == "Maipo basin", ])
cat("Listo:", file.path(AQUI, "tablas"), "\n")
