# Regional_01. Dominio de trabajo
#
# Todos los glaciares del RGI 7.0 que tocan la Region Metropolitana, conservados enteros aunque
# crucen a Argentina o a las regiones vecinas, y el rectangulo de trabajo regional alineado a la
# grilla de 30 m.
# Sale: 01_Datos/Procesados/Inventarios_UTM19S/RGI_RM_UTM19S.gpkg y tablas de resumen del dominio

suppressPackageStartupMessages(library(terra))

base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"

dir_inv    <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S")
dir_tablas <- file.path(base, "07_Avances_Reuniones", "Avance_Reunion_3",
                        "Expansion_Regional", "tablas")
dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)

crs_trabajo <- "EPSG:32719"
origen_x <- 376109.5
origen_y <- 6319778.5
res_m    <- 30
margen_aoi_m <- 5000

# Tolerancia para decidir que un tramo de perimetro "corre pegado" a la
# frontera: dos pixeles SRTM.
tol_frontera_m <- 60

# -----------------------------------------------------------------------------
# 1. Limites
# -----------------------------------------------------------------------------

gadm <- vect(file.path(base, "01_Datos/Capas_Auxiliares", "gadm41_CHL_1.json"))
rm_ll <- gadm[gadm$HASC_1 == "CL.RM", ]
if (nrow(rm_ll) != 1) stop("No se encontro la Region Metropolitana en GADM.")

rm_utm    <- makeValid(project(rm_ll, crs_trabajo))

# La union de todo Chile falla por una geometria invalida en Magallanes que no
# interesa aqui. Se recorta GADM a la ventana de la region (con medio grado de
# margen) antes de unir: la frontera que importa es la de esta ventana y ningun
# glaciar seleccionado se acerca al borde del recorte.
e_rm <- ext(rm_ll) + 0.5
chile_utm <- aggregate(makeValid(project(crop(gadm, e_rm), crs_trabajo)))

message("Region Metropolitana: ", round(expanse(rm_utm, unit = "km"), 0), " km2")

# -----------------------------------------------------------------------------
# 2. RGI 7.0: los que tocan la region, enteros
# -----------------------------------------------------------------------------

# Se lee solo la ventana de la region para no cargar 85 MB de todo el sur de
# los Andes. Un margen de medio grado asegura que entren los poligonos que
# tocan el borde desde afuera.
rgi_ll <- vect(file.path(base, "01_Datos/Brutos", "Inventario_RGI",
                         "RGI2000-v7.0-G-17_southern_andes.shp"),
               extent = e_rm)
message("RGI leidos en la ventana: ", nrow(rgi_ll))

rgi <- project(rgi_ll, crs_trabajo)
toca <- relate(rgi, rm_utm, "intersects")[, 1]
gl   <- rgi[toca, ]
message("RGI que tocan la Region Metropolitana: ", nrow(gl))

# Area medida aqui, en UTM 19S, para no depender del campo del inventario
gl$area_utm_km2 <- expanse(gl, unit = "km")

# Fraccion dentro de la region y dentro de Chile
frac_en <- function(x, y) {
  out <- numeric(nrow(x))
  for (i in seq_len(nrow(x))) {
    p <- intersect(x[i, ], y)
    out[i] <- if (nrow(p)) expanse(p, unit = "km") / x$area_utm_km2[i] else 0
  }
  out
}
gl$frac_en_RM    <- round(frac_en(gl, rm_utm), 4)
gl$frac_en_Chile <- round(frac_en(gl, chile_utm), 4)

gl$situacion <- ifelse(gl$frac_en_Chile < 0.999, "transfronterizo",
                ifelse(gl$frac_en_RM < 0.999, "cruza_a_region_vecina", "dentro_RM"))

# Dos niveles del dominio. Se midio que los transfronterizos mas
# grandes son glaciares argentinos que tocan la region por un borde: 57 km2
# con el 1,4 % en Chile, 28 km2 con el 0,03 %, Marmolejo 20 km2 con el 0,1 %.
# Sumarlos a "los glaciares de la Region Metropolitana" inflaria el total con
# hielo que la DGA no inventaria. Se conservan y se miden, pero aparte:
#   nucleo_RM        : al menos la mitad del area en Chile (glaciares chilenos,
#                      enteros aunque una cola cruce la frontera)
#   franja_argentina : menos de la mitad en Chile; tocan la region desde el
#                      otro lado de la cresta
gl$nivel <- ifelse(gl$frac_en_Chile >= 0.5, "nucleo_RM", "franja_argentina")

# -----------------------------------------------------------------------------
# 3. Comprobacion: el RGI no corta en la frontera (decision 3)
# -----------------------------------------------------------------------------

frontera <- as.lines(chile_utm)
tf <- gl[gl$situacion == "transfronterizo", ]

perimetro_en_frontera <- function(p) {
  # Longitud del perimetro del poligono que queda a menos de tol de la frontera
  per <- as.lines(p)
  franja <- buffer(frontera, tol_frontera_m)
  dentro <- intersect(per, franja)
  if (nrow(dentro) == 0) return(0)
  sum(perim(dentro))
}

if (nrow(tf)) {
  tf$perimetro_m          <- round(perim(tf), 0)
  tf$perimetro_frontera_m <- round(vapply(seq_len(nrow(tf)),
                                          function(i) perimetro_en_frontera(tf[i, ]), 0), 0)
  tf$pct_perimetro_frontera <- round(100 * tf$perimetro_frontera_m / tf$perimetro_m, 1)

  # Segunda prueba, mas directa: comparte este poligono un tramo de borde
  # (> 200 m) con OTRO poligono RGI? Si el RGI cortara los glaciares en la
  # frontera, cada mitad tendria enfrente a la otra. Se anota el vecino y de
  # que lado esta, y se compara la orientacion de ambos: dos glaciares que
  # fluyen en sentidos opuestos desde la cresta son una divisoria, no un corte.
  rgi$frac_chile_v <- NA_real_
  tf$vecinos_borde_n     <- 0L
  tf$borde_compartido_m  <- 0
  tf$vecino_rgi_id       <- ""
  tf$vecino_en_chile     <- ""
  tf$vecino_aspecto_deg  <- ""
  for (i in seq_len(nrow(tf))) {
    p   <- tf[i, ]
    vec <- rgi[relate(rgi, p, "touches")[, 1] | relate(rgi, p, "overlaps")[, 1], ]
    vec <- vec[vec$rgi_id != p$rgi_id, ]
    if (!nrow(vec)) next
    for (j in seq_len(nrow(vec))) {
      s <- suppressWarnings(intersect(as.lines(p), buffer(vec[j, ], 5)))
      L <- if (nrow(s)) sum(perim(s)) else 0
      if (L <= 200) next
      fr <- suppressWarnings(intersect(vec[j, ], chile_utm))
      f_ch <- if (nrow(fr)) sum(expanse(fr, unit = "km")) / expanse(vec[j, ], unit = "km") else 0
      tf$vecinos_borde_n[i]    <- tf$vecinos_borde_n[i] + 1L
      tf$borde_compartido_m[i] <- tf$borde_compartido_m[i] + L
      tf$vecino_rgi_id[i]      <- trimws(paste(tf$vecino_rgi_id[i], vec$rgi_id[j]))
      tf$vecino_en_chile[i]    <- trimws(paste(tf$vecino_en_chile[i], if (f_ch >= 0.5) "si" else "no"))
      tf$vecino_aspecto_deg[i] <- trimws(paste(tf$vecino_aspecto_deg[i], round(vec$aspect_deg[j])))
    }
  }
  tf$borde_compartido_m <- round(tf$borde_compartido_m)
}

# -----------------------------------------------------------------------------
# 4. Contraste con el Inventario Publico de Glaciares 2022 v2
# -----------------------------------------------------------------------------

ipg <- vect(file.path(base, "01_Datos/Brutos", "Inventario_DGA", "IPG_2022_v2",
                      "INV_PG_2022_v2.shp"), extent = e_rm)
ipg <- project(ipg, crs_trabajo)
ipg_rm <- ipg[ipg$COD_REG == "13" | grepl("Metropolitana", ipg$REGION), ]
message("IPG 2022 en la Region Metropolitana: ", nrow(ipg_rm))

ipg_rm$area_utm_km2 <- expanse(ipg_rm, unit = "km")

x04 <- aggregate(area_utm_km2 ~ CLASIFICA, data = as.data.frame(ipg_rm), FUN = sum)
x04$n <- as.integer(table(ipg_rm$CLASIFICA)[x04$CLASIFICA])
x04 <- x04[order(-x04$area_utm_km2), c("CLASIFICA", "n", "area_utm_km2")]
x04$area_utm_km2 <- round(x04$area_utm_km2, 2)

# Cuenca y subcuenca DGA de cada glaciar RGI, por mayor solape con el IPG.
# El IPG trae COD_CUEN/COD_SCUEN; el RGI no trae nada de eso.
cu <- intersect(gl[, "rgi_id"], ipg_rm[, c("NOM_CUEN", "COD_CUEN", "COD_SCUEN", "COD_SSCUEN", "CLASIFICA", "NOMBRE")])
if (nrow(cu)) {
  cu$a <- expanse(cu, unit = "km")
  d <- as.data.frame(cu)
  d <- d[order(d$rgi_id, -d$a), ]
  d <- d[!duplicated(d$rgi_id), ]
  gl <- merge(gl, d[, c("rgi_id", "NOM_CUEN", "COD_CUEN", "COD_SCUEN", "COD_SSCUEN",
                        "CLASIFICA", "NOMBRE")], by = "rgi_id", all.x = TRUE)
  names(gl)[names(gl) == "CLASIFICA"] <- "clase_IPG"
  names(gl)[names(gl) == "NOMBRE"]    <- "nombre_IPG"
}

# Las siete unidades ya estudiadas, para marcarlas dentro del dominio
est <- vect(file.path(dir_inv, "RGI_glaciares_estudio_UTM19S.gpkg"))
est <- project(est, crs_trabajo)
gl$unidad_estudio <- NA_character_
for (i in seq_len(nrow(est))) {
  hit <- relate(gl, est[i, ], "intersects")[, 1]
  # El poligono de estudio y el RGI son el mismo objeto: coincidencia por area
  if (any(hit)) {
    cand <- which(hit)
    a_int <- vapply(cand, function(j) {
      p <- suppressWarnings(intersect(gl[j, ], est[i, ]))
      if (nrow(p)) sum(expanse(p, unit = "km")) else 0
    }, 0)
    k <- cand[which.max(a_int)]
    if (a_int[which.max(a_int)] > 0.5 * gl$area_utm_km2[k]) gl$unidad_estudio[k] <- est$ARCHIVO[i]
  }
}

# -----------------------------------------------------------------------------
# 5. Rectangulo regional alineado a la grilla del proyecto
# -----------------------------------------------------------------------------

alinear <- function(v, o, r, abajo) { n <- (v - o) / r; o + (if (abajo) floor(n) else ceiling(n)) * r }
e <- ext(gl)
ext_aoi <- ext(alinear(e$xmin - margen_aoi_m, origen_x, res_m, TRUE),
               alinear(e$xmax + margen_aoi_m, origen_x, res_m, FALSE),
               alinear(e$ymin - margen_aoi_m, origen_y, res_m, TRUE),
               alinear(e$ymax + margen_aoi_m, origen_y, res_m, FALSE))
aoi <- as.polygons(ext_aoi); crs(aoi) <- crs_trabajo
aoi$nombre <- "AOI_regional_RM"

aoi_orig <- project(vect(file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable", "01_AOI.gpkg")), crs_trabajo)
anidado  <- relate(aoi_orig, aoi, "within")[1, 1]

ncol_aoi <- (ext_aoi$xmax - ext_aoi$xmin) / res_m
nrow_aoi <- (ext_aoi$ymax - ext_aoi$ymin) / res_m

# -----------------------------------------------------------------------------
# 6. Tablas
# -----------------------------------------------------------------------------

d <- as.data.frame(gl)
d$clase_tamano <- cut(d$area_utm_km2, c(0, 0.01, 0.1, 0.5, 1, 5, Inf),
                      labels = c("<0.01", "0.01-0.1", "0.1-0.5", "0.5-1", "1-5", ">5"),
                      right = FALSE)

x01 <- data.frame(
  concepto = c("Region Metropolitana (GADM 4.1), km2",
               "Glaciares RGI 7.0 que tocan la RM, n",
               "  area total, km2",
               "  enteramente dentro de la RM, n",
               "  cruzan a region vecina, n",
               "  transfronterizos (Argentina), n",
               "  area transfronteriza, km2",
               "  area que queda fuera de Chile, km2",
               "NUCLEO RM (>= 50 % del area en Chile), n",
               "  area nucleo, km2",
               "  area nucleo dentro de Chile, km2",
               "FRANJA ARGENTINA (< 50 % en Chile), n",
               "  area franja, km2",
               "  de los cuales son unidades de estudio, n",
               "  area de las unidades de estudio, km2",
               "  glaciares con nombre en RGI, n",
               "  mayor de 1 km2, n",
               "  area en los > 1 km2, km2",
               "  mayor de 0.5 km2, n",
               "  area en los > 0.5 km2, km2",
               "  elevacion mediana min, m",
               "  elevacion mediana max, m",
               "IPG 2022 v2 en la RM, n (todas las clases)",
               "  area IPG, km2",
               "  glaciares rocosos IPG, n",
               "  area glaciares rocosos IPG, km2",
               "AOI regional: ancho, km", "AOI regional: alto, km", "AOI regional: area, km2",
               "AOI regional: columnas", "AOI regional: filas",
               "AOI original anidado pixel a pixel",
               "factor de crecimiento del AOI"),
  valor = c(round(expanse(rm_utm, unit = "km"), 0),
            nrow(gl), round(sum(gl$area_utm_km2), 2),
            sum(gl$situacion == "dentro_RM"),
            sum(gl$situacion == "cruza_a_region_vecina"),
            sum(gl$situacion == "transfronterizo"),
            round(sum(gl$area_utm_km2[gl$situacion == "transfronterizo"]), 2),
            round(sum(gl$area_utm_km2 * (1 - gl$frac_en_Chile)), 2),
            sum(gl$nivel == "nucleo_RM"),
            round(sum(gl$area_utm_km2[gl$nivel == "nucleo_RM"]), 2),
            round(sum((gl$area_utm_km2 * gl$frac_en_Chile)[gl$nivel == "nucleo_RM"]), 2),
            sum(gl$nivel == "franja_argentina"),
            round(sum(gl$area_utm_km2[gl$nivel == "franja_argentina"]), 2),
            sum(!is.na(gl$unidad_estudio)),
            round(sum(gl$area_utm_km2[!is.na(gl$unidad_estudio)]), 2),
            sum(!is.na(gl$glac_name) & gl$glac_name != "S/N" & gl$glac_name != ""),
            sum(gl$area_utm_km2 > 1), round(sum(gl$area_utm_km2[gl$area_utm_km2 > 1]), 2),
            sum(gl$area_utm_km2 > 0.5), round(sum(gl$area_utm_km2[gl$area_utm_km2 > 0.5]), 2),
            round(min(gl$zmed_m, na.rm = TRUE), 0), round(max(gl$zmed_m, na.rm = TRUE), 0),
            nrow(ipg_rm), round(sum(ipg_rm$area_utm_km2), 2),
            sum(grepl("ROCOSO", toupper(ipg_rm$CLASIFICA))),
            round(sum(ipg_rm$area_utm_km2[grepl("ROCOSO", toupper(ipg_rm$CLASIFICA))]), 2),
            round((ext_aoi$xmax - ext_aoi$xmin) / 1000, 2),
            round((ext_aoi$ymax - ext_aoi$ymin) / 1000, 2),
            round(ncol_aoi * nrow_aoi * res_m^2 / 1e6, 1),
            ncol_aoi, nrow_aoi, as.integer(anidado),
            round(ncol_aoi * nrow_aoi / (717 * 748), 1))
)
write.csv(x01, file.path(dir_tablas, "X01_Resumen_Dominio.csv"), row.names = FALSE)

x02 <- d[order(-d$area_utm_km2), c("rgi_id", "glac_name", "nombre_IPG", "area_km2", "area_utm_km2",
                                    "zmin_m", "zmax_m", "zmed_m", "slope_deg", "aspect_deg",
                                    "primeclass", "term_type", "src_date", "cenlon", "cenlat",
                                    "situacion", "nivel", "frac_en_RM", "frac_en_Chile",
                                    "NOM_CUEN", "COD_CUEN", "COD_SCUEN", "COD_SSCUEN",
                                    "clase_IPG", "clase_tamano", "unidad_estudio")]
write.csv(x02, file.path(dir_tablas, "X02_Glaciares_RM.csv"), row.names = FALSE)

x03a <- aggregate(area_utm_km2 ~ clase_tamano, d, sum)
x03a$n <- as.integer(table(d$clase_tamano)[as.character(x03a$clase_tamano)])
x03a$pct_area <- round(100 * x03a$area_utm_km2 / sum(d$area_utm_km2), 1)
x03a$criterio <- "tamano_km2"; names(x03a)[1] <- "clase"
x03b <- aggregate(area_utm_km2 ~ situacion, d, sum)
x03d <- aggregate(area_utm_km2 ~ nivel, d, sum); x03d$n <- as.integer(table(d$nivel)[x03d$nivel]); x03d$pct_area <- round(100 * x03d$area_utm_km2 / sum(d$area_utm_km2), 1); x03d$criterio <- "nivel"; names(x03d)[1] <- "clase"
x03b$n <- as.integer(table(d$situacion)[x03b$situacion])
x03b$pct_area <- round(100 * x03b$area_utm_km2 / sum(d$area_utm_km2), 1)
x03b$criterio <- "situacion"; names(x03b)[1] <- "clase"
x03c <- aggregate(area_utm_km2 ~ COD_SCUEN, d[!is.na(d$COD_SCUEN), ], sum)
x03c$n <- as.integer(table(d$COD_SCUEN)[x03c$COD_SCUEN])
x03c$pct_area <- round(100 * x03c$area_utm_km2 / sum(d$area_utm_km2), 1)
x03c$criterio <- "subcuenca_DGA"; names(x03c)[1] <- "clase"
x03 <- rbind(x03a, x03b, x03d, x03c)[, c("criterio", "clase", "n", "area_utm_km2", "pct_area")]
x03$area_utm_km2 <- round(x03$area_utm_km2, 2)
write.csv(x03, file.path(dir_tablas, "X03_Glaciares_por_Clase.csv"), row.names = FALSE)

write.csv(x04, file.path(dir_tablas, "X04_Contraste_IPG_2022.csv"), row.names = FALSE)

if (nrow(tf)) {
  x05 <- as.data.frame(tf)[, c("rgi_id", "glac_name", "area_utm_km2", "frac_en_Chile", "nivel", "aspect_deg",
                               "perimetro_m", "perimetro_frontera_m", "pct_perimetro_frontera", "vecinos_borde_n", "borde_compartido_m", "vecino_rgi_id", "vecino_en_chile", "vecino_aspecto_deg")]
  x05 <- x05[order(-x05$area_utm_km2), ]
  write.csv(x05, file.path(dir_tablas, "X05_Transfronterizos.csv"), row.names = FALSE)
}

# -----------------------------------------------------------------------------
# 7. Escritura vectorial
# -----------------------------------------------------------------------------

gpkg <- file.path(dir_inv, "RGI_RM_UTM19S.gpkg")
if (file.exists(gpkg) && !file.remove(gpkg)) stop("Cerrar QGIS: no se pudo reemplazar ", gpkg)

writeVector(gl,        gpkg, layer = "glaciares_rm",  overwrite = TRUE)
writeVector(rm_utm,    gpkg, layer = "limite_rm",     insert = TRUE)
writeVector(chile_utm, gpkg, layer = "limite_chile",  insert = TRUE)
writeVector(aoi,       gpkg, layer = "aoi_regional",  insert = TRUE)
writeVector(ipg_rm,    gpkg, layer = "ipg2022_rm",    insert = TRUE)

shp <- file.path(dir_inv, "RGI_RM_UTM19S.shp")
writeVector(gl, shp, overwrite = TRUE)

# -----------------------------------------------------------------------------
# 8. Resumen en pantalla
# -----------------------------------------------------------------------------

cat("\n=== DOMINIO REGIONAL ===\n")
print(x01, row.names = FALSE)
cat("\n=== POR CLASE ===\n")
print(x03, row.names = FALSE)
cat("\n=== IPG 2022 v2 EN LA RM ===\n")
print(x04, row.names = FALSE)
if (nrow(tf)) {
  cat("\n=== TRANSFRONTERIZOS: cuanto del perimetro corre por la frontera ===\n")
  print(head(x05, 15), row.names = FALSE)
  cat(sprintf("\nMediana del %% de perimetro pegado a la frontera: %.1f %%\n",
              median(x05$pct_perimetro_frontera)))
}
cat("\nEscrito: ", gpkg, "\n")
