# M03. Base topografica: GDEM, mascara de roca estable e hipsometria
#
#   1. Lleva las teselas del GDEM (M01) a la grilla regional de 30 m en UTM 19 sur (bilineal; el
#      NUM por vecino mas cercano).
#   2. Calcula pendiente y orientacion.
#   3. Mascara de roca estable: pendiente entre 2 y 30 grados, fuera de glaciares y glaciares
#      rocosos (200 m), mineria (500 m), caminos, agua, obras y esqui, y solo en suelo desnudo,
#      pastizal o liquen. Se hace solo con el modelo y capas vectoriales, nunca con diferencias de
#      altura, para no sacar de la roca justamente lo que cambia.
#   4. Hipsometria de cada glaciar en bandas de 100 m.
# Sale: 01_Datos/Mascara_Estable/MascaraEstable_RM_ASTER/

suppressPackageStartupMessages(library(terra))
terraOptions(progress = 0)

base    <- "C:/maipo/proyecto"
dir_g   <- file.path(base, "01_Datos/Brutos", "ASTER_GDEM_V3")
dir_old <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_Regional")
dir_new <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER")
dir_tab <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM", "tablas")
f_inv   <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")
dir.create(dir_new, recursive = TRUE, showWarnings = FALSE)

pendiente_min <- 2; pendiente_max <- 30; minimo_parche_px <- 10
buffer_glaciar <- 200; min_escenas <- 3

plantilla <- rast(file.path(dir_old, "03_MascaraEstable.tif"))
plantilla <- rast(ext(plantilla), resolution = res(plantilla), crs = crs(plantilla))

# --- 1. GDEM y NUM en la grilla regional ---------------------------------------
dem_ll <- vrt(list.files(dir_g, "_dem\\.tif$", full.names = TRUE))
num_ll <- vrt(list.files(dir_g, "_num\\.tif$", full.names = TRUE))
gdem <- project(dem_ll, plantilla, method = "bilinear"); names(gdem) <- "GDEM_V3"
gdem <- ifel(gdem <= -9999, NA, gdem)
num  <- project(num_ll, plantilla, method = "near"); names(num) <- "NUM"

# Origen de cada pixel segun la tabla 3 de la guia del GDEM V3
origen <- function(n) {
  ifelse(n >= 1 & n <= 50, "ASTER (GDEM V3)",
  ifelse(n >= 60 & n <= 110, "ASTER (relleno GDEM V2)",
  ifelse(n >= 131 & n <= 184, "ALOS PRISM",
  ifelse(n >= 201 & n <= 234, "SRTM",
  ifelse(n == 250, "interpolacion", "otro o sin dato")))))
}
es_aster <- (num >= min_escenas & num <= 50) | (num >= 60 + min_escenas & num <= 110)

pend <- terrain(gdem, v = "slope", unit = "degrees", neighbors = 8)
orie <- terrain(gdem, v = "aspect", unit = "degrees", neighbors = 8)

# --- 2. Mascara estable ------------------------------------------------------------
# Exclusiones de la mascara regional (Regional_05): glaciares RGI 7.0 e IPG 2022
# con sus glaciares rocosos y glaciaretes, agua, cauces, caminos, mineria y
# urbano de OSM. Se rasterizan por tipo para poder contar cuanto quita cada una.
excl <- vect(file.path(dir_old, "02_Exclusiones.gpkg"))
r6 <- vect(f_inv, layer = "rgi60")
r6b <- buffer(r6, width = buffer_glaciar)
ras <- function(v) rasterize(v, plantilla, field = 1, background = 0, touches = TRUE)
grupo <- c(glaciar_RGI = "glaciares", glaciar_DGA = "glaciares", glaciar_rocoso_DGA = "glaciares",
           glaciarete_DGA = "glaciares", agua_cuerpo = "agua", agua_rio = "agua", agua_estero = "agua",
           camino = "caminos", mineria_rajo = "mineria_osm", mineria_industrial = "mineria_osm",
           mineria_botadero = "mineria_osm", mineria_relaves = "mineria_osm", urbano = "urbano",
           manual_AOI_original = "urbano")
EX <- list()
for (gr in unique(grupo)) EX[[gr]] <- ras(excl[excl$TIPO %in% names(grupo)[grupo == gr], ])
EX$glaciares <- max(EX$glaciares, ras(r6b))

# Exclusiones nuevas (M01b), para que la mascara sea roca que no cambia de altura
#. Buffers con el mismo criterio de Regional_05:
# 500 m para mineria, 100 m para contornos glaciares secundarios y humedales,
# 50 a 200 m para obras lineales y represas.
f_ex2 <- file.path(base, "01_Datos/Capas_Auxiliares", "exclusiones_rm_aster.gpkg")
capa2 <- function(n, ancho) {
  if (!(n %in% vector_layers(f_ex2))) return(NULL)
  v <- vect(f_ex2, layer = n)
  if (nrow(v) == 0) return(NULL)
  ras(buffer(v, width = ancho))
}
juntar <- function(...) { l <- Filter(Negate(is.null), list(...)); if (length(l)) max(rast(l)) else NULL }
EX$mineria_maus <- capa2("mineria_maus2022", 500)
EX$glims <- capa2("glims", 100)
EX$esqui <- juntar(capa2("osm_pista", 100), capa2("osm_andarivel", 50), capa2("osm_esqui", 100))
EX$obras <- juntar(capa2("osm_construccion", 200), capa2("osm_represa", 200), capa2("osm_embalse", 100))
EX$osm_glaciar_humedal <- juntar(capa2("osm_glaciar", 100), capa2("osm_humedal", 100))
EX <- Filter(Negate(is.null), EX)

# Cobertura de suelo ESA WorldCover 2021 (10 m): una celda de 30 m entra solo si
# todo su suelo es desnudo o con vegetacion escasa (60), pastizal (30) o musgo y
# liquen (100). Quedan fuera arboles (10), matorral (20), cultivos (40),
# construcciones (50), nieve y hielo (70), agua (80) y humedales (90). Hugonnet
# et al. (2022) consideran estable la roca desnuda y el pastizal, y excluyen
# bosque, agua y nieve estacional.
wc <- rast(file.path(base, "01_Datos/Brutos", "ESA_WorldCover", "ESA_WorldCover_2021_v200_RM.tif"))
wc_ok <- project(wc %in% c(30, 60, 100), plantilla, method = "average")
wc_ok <- !is.na(wc_ok) & wc_ok >= 0.999
names(wc_ok) <- "cobertura_ok"
ex_total <- max(rast(EX))
bruta <- ifel(!is.na(gdem) & !is.na(pend) & pend >= pendiente_min & pend < pendiente_max &
                ex_total == 0 & wc_ok & es_aster, 1, NA)
pa <- patches(bruta, directions = 8, zeroAsNA = TRUE)
fp <- freq(pa)
masc <- ifel(pa %in% fp$value[fp$count >= minimo_parche_px], 1, NA)
names(masc) <- "terreno_estable"

esc <- function(r, n, tipo = "FLT4S") {
  p <- file.path(dir_new, n)
  if (file.exists(p) && !file.remove(p)) stop("Cerrar QGIS: ", p)
  writeRaster(r, p, datatype = tipo, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
}
esc(gdem, "01_GDEM.tif"); esc(num, "01_GDEM_NUM.tif", "INT1U")
esc(pend, "01_Pendiente.tif"); esc(orie, "01_Orientacion.tif")
# La mascara anterior, sin las exclusiones de M01b, se guarda una vez como
# respaldo para medir cuanto cambio (Jaccard en M03_Mascara.csv).
f_actual <- file.path(dir_new, "03_MascaraEstable.tif")
f_resp <- file.path(dir_new, "03_MascaraEstable_131026.tif")
if (file.exists(f_actual) && !file.exists(f_resp)) file.copy(f_actual, f_resp)
esc(masc, "03_MascaraEstable.tif", "INT1U")

# Embudo de la mascara, paso a paso: cuanto quita cada exclusion despues de las
# anteriores, en todo el rectangulo y en la roca a menos de 2 km de un glaciar
# (la que usan las ventanas de alineacion).
n <- function(r) global(r, "sum", na.rm = TRUE)[1, 1]
km2 <- function(px) round(px * 900 / 1e6, 1)
r7 <- vect(f_inv, layer = "rgi70")
g76 <- max(rasterize(r7, plantilla, field = 1, background = 0), rasterize(r6, plantilla, field = 1, background = 0))
d76 <- distance(ifel(g76 == 1, 1, NA), unit = "m")
cerca <- d76 <= 2000
nombres_paso <- c(glaciares = "glaciares y glaciares rocosos de los inventarios (RGI 6.0, RGI 7.0, IPG 2022)",
                  glims = "otros contornos glaciares de GLIMS (incluye el inventario argentino)",
                  agua = "agua y cauces (OSM)", caminos = "caminos (OSM)",
                  mineria_osm = "mineria, industria, botaderos y relaves (OSM)",
                  mineria_maus = "mineria no dibujada en OSM (Maus et al. 2022)",
                  urbano = "zonas urbanas y revision visual",
                  esqui = "centros de esqui: pistas y andariveles (OSM)",
                  obras = "obras, represas y embalses (OSM)",
                  osm_glaciar_humedal = "glaciares y humedales dibujados en OSM")
actual <- ifel(!is.na(gdem), 1, 0)
filas <- list(data.frame(paso = "rectangulo regional con GDEM", km2 = km2(n(actual)), km2_cerca = km2(n(actual & cerca))))
quita <- function(nueva, etiqueta) {
  d <- data.frame(paso = etiqueta, km2 = km2(n(nueva)), km2_cerca = km2(n(nueva & cerca)))
  filas[[length(filas) + 1]] <<- d
  nueva
}
actual <- quita(actual & pend >= pendiente_min & pend < pendiente_max, "pendiente entre 2 y 30 grados")
for (k in names(nombres_paso)) if (!is.null(EX[[k]])) actual <- quita(actual & EX[[k]] == 0, paste("sin", nombres_paso[k]))
actual <- quita(actual & wc_ok, "cobertura de suelo desnudo, pastizal o liquen (ESA WorldCover 2021)")
actual <- quita(actual & es_aster, "GDEM de origen ASTER con al menos 3 escenas")
actual <- quita(!is.na(masc), "sin parches de menos de 10 px (mascara final)")
M03 <- do.call(rbind, filas)
M03$pct_rectangulo <- round(100 * M03$km2 / km2(ncell(plantilla)), 2)
M03$quita_km2 <- c(NA, -diff(M03$km2)); M03$quita_cerca_km2 <- c(NA, -diff(M03$km2_cerca))
f_prev <- file.path(dir_new, "03_MascaraEstable_131026.tif")
if (file.exists(f_prev)) {
  old <- rast(f_prev)
  comun <- n(!is.na(old) & !is.na(masc))
  M03 <- rbind(M03, data.frame(paso = "coincidencia con la mascara sin las exclusiones nuevas (Jaccard)",
                               km2 = km2(comun), km2_cerca = km2(n(!is.na(old) & !is.na(masc) & cerca)),
                               pct_rectangulo = round(100 * comun / (n(!is.na(old)) + n(masc) - comun), 2),
                               quita_km2 = NA, quita_cerca_km2 = NA))
}
write.csv(M03, file.path(dir_tab, "M03_Mascara.csv"), row.names = FALSE)
print(M03, row.names = FALSE)

# --- 3. Origen del GDEM: en todo el rectangulo, en la mascara y sobre hielo ------------
r7 <- vect(f_inv, layer = "rgi70")
g7 <- rasterize(r7, plantilla, field = 1, background = 0, touches = FALSE)
g6 <- rasterize(r6, plantilla, field = 1, background = 0, touches = FALSE)
vn <- values(num, mat = FALSE); o <- origen(vn)
tab_origen <- function(sel, nombre) {
  t <- table(o[sel]); data.frame(dominio = nombre, origen = names(t), pct = round(100 * as.numeric(t) / sum(t), 2))
}
O <- rbind(tab_origen(!is.na(values(gdem, mat = FALSE)), "rectangulo regional"),
           tab_origen(values(g7, mat = FALSE) == 1, "glaciares RGI 7.0"),
           tab_origen(values(g6, mat = FALSE) == 1, "glaciares RGI 6.0"))
vs <- vn[values(g7, mat = FALSE) == 1]
O <- rbind(O, data.frame(dominio = "glaciares RGI 7.0", origen = "mediana de escenas apiladas (V3)",
                         pct = median(vs[vs >= 1 & vs <= 50])))
write.csv(O, file.path(dir_tab, "M03_Origen_GDEM.csv"), row.names = FALSE)
print(O, row.names = FALSE)

# --- 4. Hipsometria por glaciar --------------------------------------------------------
hipso <- function(v, idcol, archivo) {
  h <- extract(c(gdem, es_aster), v, cells = TRUE, exact = TRUE)
  names(h)[2:3] <- c("z", "aster")
  h <- h[!is.na(h$z), ]
  h$id <- v[[idcol]][[1]][h$ID]; h$banda <- floor(h$z / 100) * 100
  h$area_m2 <- h$fraction * 900
  a <- aggregate(area_m2 ~ id + banda, h, sum)
  f <- aggregate(cbind(aster = aster * fraction, fraction) ~ id, h, sum)
  a$frac_gdem_aster <- f$aster[match(a$id, f$id)] / f$fraction[match(a$id, f$id)]
  names(a)[1] <- idcol
  write.csv(a, file.path(dir_new, archivo), row.names = FALSE)
  cat(archivo, ":", length(unique(a[[idcol]])), "glaciares,", round(sum(a$area_m2) / 1e6, 1), "km2\n")
}
hipso(r7, "rgi_id", "01_Hipsometria_rgi70.csv")
hipso(r6, "RGIId", "01_Hipsometria_rgi60.csv")
