# M03b. Hipsometria de la union de los inventarios (dominio de medicion) y de las superficies que
# solo trae uno de los dos inventarios.
# Sale: 01_Hipsometria_union.csv y las de las diferencias, en la carpeta de la mascara.

suppressPackageStartupMessages(library(terra))
base    <- "C:/maipo/proyecto"
dir_new <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER")
f_inv   <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")
gdem <- rast(file.path(dir_new, "01_GDEM.tif"))
# Tambien el inventario de la DGA (IPG 2014, capa dga de M02b), que es el
# principal: M05c lo mide con esta hipsometria.
f_dga   <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventario_DGA_2014_cuenca.gpkg")
capas <- c("union", "solo_rgi60", "solo_rgi70", if (file.exists(f_dga)) "dga")
for (capa in capas) {
  v <- vect(if (capa == "dga") f_dga else f_inv, layer = capa)
  idcol <- if (capa == "union") "uid" else "gid"
  h <- extract(gdem, v, cells = TRUE, exact = TRUE)
  names(h)[2] <- "z"; h <- h[!is.na(h$z), ]
  h$id <- v[[idcol]][[1]][h$ID]; h$banda <- floor(h$z / 100) * 100; h$area_m2 <- h$fraction * 900
  a <- aggregate(area_m2 ~ id + banda, h, sum)
  names(a)[1] <- idcol
  write.csv(a, file.path(dir_new, paste0("01_Hipsometria_", capa, ".csv")), row.names = FALSE)
  cat(capa, ":", length(unique(a[[idcol]])), "poligonos,", round(sum(a$area_m2) / 1e6, 1), "km2\n")
}
