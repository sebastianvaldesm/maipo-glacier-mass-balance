# I01. RGI 7.0 y RGI 6.0 recortados al limite de la cuenca alta del Maipo (Maipo en El Manzano mas
# Mapocho en Los Almendros, subcuencas de la Direccion General de Aguas), el mismo contorno exterior
# del inventario nacional. Dentro de ese contorno cada inventario conserva sus poligonos.
#
# Entra: los poligonos del RGI de la cuenca que mide la rama MicMac + GDEM con CPD, el limite de las
#        subcuencas y el GDEM de la mascara de la cadena.
# Sale:  datos/Inventarios_recortados.gpkg (rgi70 y rgi60 recortados, con su area; las demas capas
#        copiadas sin cambios para M06) y datos/01_Hipsometria_rgi70.csv y _rgi60.csv (como M03).
# Uso:   Rscript I01_Recortar_Inventarios.R
suppressPackageStartupMessages(library(terra)); terraOptions(progress = 0)
base  <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
aqui  <- file.path(base, "06_Memoria_Final", "Inventarios_Recortados")
datos <- file.path(aqui, "datos"); dir.create(datos, showWarnings = FALSE, recursive = TRUE)
f_inv <- file.path(base, "01_Datos/Procesados/Inventarios_UTM19S/Inventarios_RM_ASTER.gpkg")
f_ssc <- file.path(base, "01_Datos/Brutos/Cuencas_DGA/SubSubCuencas_BNA_057_054_060.geojson")
d_msk <- file.path(base, "01_Datos/Mascara_Estable/MascaraEstable_RM_ASTER")
G     <- read.csv("C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas/M06_Tasa_por_Glaciar.csv")
f_out <- file.path(datos, "Inventarios_recortados.gpkg")
if (file.exists(f_out) && !file.remove(f_out)) stop("No se pudo borrar ", f_out, " (cerrar QGIS)")

v7 <- vect(f_inv, "rgi70")
ssc <- project(vect(f_ssc), crs(v7))
cuenca <- aggregate(ssc[ssc$COD_SUBC %in% "0570" | ssc$COD_SSUBC %in% c("05720", "05721")])

recortar <- function(capa, idc) {
  v <- vect(f_inv, capa)
  ids <- G$gid[G$conjunto == capa & G$en_maipo %in% c(TRUE, "TRUE")]
  v <- v[v[[idc]][[1]] %in% ids]
  a0 <- expanse(v, unit = "km")
  r <- intersect(v, cuenca)
  r <- r[, names(v)]   # sin las columnas del limite (el limite es un solo poligono: queda uno por glaciar)
  if (anyDuplicated(r[[idc]][[1]])) stop("Hay glaciares repetidos despues del recorte")
  r$area_utm_km2 <- expanse(r, unit = "km")
  cat(sprintf("%s: %d poligonos de la cuenca, %.1f km2 -> recortados %d, %.1f km2 (fuera del limite %.1f km2)\n",
              capa, length(v), sum(a0), length(r), sum(r$area_utm_km2), sum(a0) - sum(r$area_utm_km2)))
  r
}
r7 <- recortar("rgi70", "rgi_id")
r6 <- recortar("rgi60", "RGIId")

# las demas capas del archivo de inventarios, sin cambios (M06 las lee)
for (capa in vector_layers(f_inv)) {
  v <- if (capa == "rgi70") r7 else if (capa == "rgi60") r6 else vect(f_inv, capa)
  writeVector(v, f_out, layer = capa, insert = file.exists(f_out))
}

# hipsometria por franjas de 100 m sobre el GDEM, igual que M03
gdem <- rast(file.path(d_msk, "01_GDEM.tif"))
num  <- rast(file.path(d_msk, "01_GDEM_NUM.tif"))
min_escenas <- 3
es_aster <- (num >= min_escenas & num <= 50) | (num >= 60 + min_escenas & num <= 110)
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
  write.csv(a, file.path(datos, archivo), row.names = FALSE)
  cat(archivo, ":", length(unique(a[[idcol]])), "glaciares,", round(sum(a$area_m2) / 1e6, 1), "km2\n")
}
hipso(r7, "rgi_id", "01_Hipsometria_rgi70.csv")
hipso(r6, "RGIId", "01_Hipsometria_rgi60.csv")
