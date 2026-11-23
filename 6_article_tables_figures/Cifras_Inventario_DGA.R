# Cifras del inventario de la DGA (IPG 2014) en la cuenca alta del Maipo (subcuencas 0570 y 0572) que
# cita el articulo: glaciares y glaciares rocosos, areas y fechas de las imagenes de los contornos.
# Cada poligono es un glaciar: 4 pares comparten COD_GLA pero son glaciares distintos.
# Sale: tablas/cifras_dga.tex

suppressPackageStartupMessages(library(terra))

base <- "C:/maipo/proyecto"
ART  <- file.path(base, "06_Memoria_Final", "Tablas_Figuras_Articulo")
f_ipg <- file.path(base, "01_Datos/Brutos/Inventario_DGA/IPG_2014",
                   "Inventario Publico de Glaciares 2014", "IPG_2014", "IPG_2014.shp")

d <- as.data.frame(vect(f_ipg))
d <- d[d$COD_SUBC %in% c("0570", "0572"), ]
if (sum(duplicated(d$COD_GLA)) != 4) stop("Cambió el número de COD_GLA repetidos: revisar qué es un glaciar")
roc <- d$CLASIFICA == "GLACIAR ROCOSO"
g <- d[!roc, ]

# Imagen de origen de los contornos que se miden
landsat <- grepl("LANDSAT", g$FUENTE_DIG) & substr(g$FUENTE_FEC, 7, 10) %in% c("2000", "2001")
fechas_landsat <- sort(unique(g$FUENTE_FEC[landsat]))
# El texto dice "imagenes Landsat ETM+ de marzo de 2000 y de 2001" (son tres:
# 31-03-2000, 11-03-2001 y 18-03-2001)
if (!all(substr(fechas_landsat, 4, 5) == "03"))
  stop("Hay fechas Landsat fuera de marzo: ", paste(fechas_landsat, collapse = ", "))
cat("Fechas Landsat:", fechas_landsat, "\n")
# El resto: SPOT-5 o Google Earth de 2003 a 2010 y una RapidEye con la fecha
# escrita "25/03/11" (anio de dos cifras)
otras <- g$FUENTE_FEC[!landsat]
anio <- sub("^.*/", "", otras)
anio <- as.integer(ifelse(nchar(anio) == 2, paste0("20", anio), anio))
anios_otras <- range(anio)

coma <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
miles <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
tipo <- function(t) g[g$CLASIFICA == t, ]
mac <- c(
  nGlaDga         = miles(nrow(g)),
  areaGlaDga      = coma(sum(g$AREA_Km2)),
  nRocDga         = miles(sum(roc)),
  areaRocDga      = coma(sum(d$AREA_Km2[roc])),
  nValleDga       = miles(nrow(tipo("GLACIAR DE VALLE"))),
  areaValleDga    = coma(sum(tipo("GLACIAR DE VALLE")$AREA_Km2)),
  nMontanaDga     = miles(nrow(tipo("GLACIAR DE MONTAÑA"))),
  areaMontanaDga  = coma(sum(tipo("GLACIAR DE MONTAÑA")$AREA_Km2)),
  nGlaciareteDga  = miles(nrow(tipo("GLACIARETE"))),
  areaGlaciareteDga = coma(sum(tipo("GLACIARETE")$AREA_Km2)),
  pctLandsatDga   = as.character(round(100 * sum(g$AREA_Km2[landsat]) / sum(g$AREA_Km2))),
  anioOtrasIniDga = as.character(anios_otras[1]),
  anioOtrasFinDga = as.character(anios_otras[2])
)
if (nrow(g) != sum(table(g$CLASIFICA))) stop("Clases sin contar")

lin <- c("% Generado por Cifras_Inventario_DGA.R. No editar a mano.",
         sprintf("\\newcommand{\\%s}{%s}", names(mac), mac))
dir.create(file.path(ART, "tablas"), showWarnings = FALSE)
con <- file(file.path(ART, "tablas", "cifras_dga.tex"), open = "wb")
writeLines(enc2utf8(lin), con, useBytes = TRUE); close(con)
cat(lin, sep = "\n")
