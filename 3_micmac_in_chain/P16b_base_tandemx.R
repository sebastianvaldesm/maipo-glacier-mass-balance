# P16b. Mascara estable e hipsometria con el TanDEM-X: corre M03 y M03b de la cadena cambiando solo la
# entrada y la salida, con las mismas reglas que con el GDEM.

args <- commandArgs(trailingOnly = TRUE)
rama <- if (length(args)) args[1] else ""
if (!rama %in% c("tdx90", "tdx30")) stop("Uso: Rscript P16b_base_tandemx.R tdx90|tdx30")
base_raiz <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
cadena <- file.path(base_raiz, "02_Balance_Masa_RM", "Cadena_RM")
carpeta <- paste0("MascaraEstable_RM_", toupper(rama))
dir_res <- file.path(base_raiz, "03_DEM_MicMac", "resultados", paste0("P16_base_", rama))
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(file.path(base_raiz, "01_Datos/Mascara_Estable", carpeta, "00_TDX_fiable.tif")))
  stop("Falta correr antes: python P16_base_tandemx.py ", rama, " preparar")

# reemplaza un texto que tiene que aparecer exactamente una vez (si la cadena cambio, se detiene)
cambiar <- function(src, viejo, nuevo) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != 1) stop("El texto aparece ", length(i), " veces en vez de una: ", viejo)
  src[i] <- sub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
# reemplaza el bloque de lineas entre dos marcas (la primera incluida, la segunda excluida)
bloque <- function(src, desde, hasta, nuevo) {
  a <- grep(desde, src, fixed = TRUE); b <- grep(hasta, src, fixed = TRUE)
  if (length(a) != 1 || length(b) != 1 || b <= a) stop("No se encontro el bloque ", desde)
  c(src[seq_len(a - 1)], nuevo, src[b:length(src)])
}

src <- readLines(file.path(cadena, "M03_Base_GDEM.R"), encoding = "UTF-8")
src <- cambiar(src, 'dir_new <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER")',
               sprintf('dir_new <- file.path(base, "01_Datos/Mascara_Estable", "%s")', carpeta))
src <- cambiar(src, 'dir_tab <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM", "tablas")',
               sprintf('dir_tab <- "%s"', dir_res))
src <- bloque(src, "# --- 1. GDEM y NUM en la grilla regional", "pend <- terrain(gdem", c(
  "# --- 1. (P16b) TanDEM-X ya llevado a la grilla regional y a EGM96 por P16_base_tandemx.py ---------",
  'gdem <- rast(file.path(dir_new, "00_TDX_EGM96.tif")); names(gdem) <- "TanDEM_X"',
  'fiab <- rast(file.path(dir_new, "00_TDX_fiable.tif"))',
  "if (!compareGeom(gdem, plantilla, stopOnError = FALSE)) stop(\"El TanDEM-X no esta en la grilla regional\")",
  "# en el papel del criterio 'GDEM de origen ASTER': HEM < 0,5 m (y, en el EDEM, sin edicion)",
  "es_aster <- !is.na(fiab) & fiab == 1",
  ""))
src <- cambiar(src, 'esc(gdem, "01_GDEM.tif"); esc(num, "01_GDEM_NUM.tif", "INT1U")',
               'esc(gdem, "01_GDEM.tif"); esc(fiab, "01_TDX_fiable.tif", "INT1U")')
src <- cambiar(src, "if (file.exists(f_actual) && !file.exists(f_resp)) file.copy(f_actual, f_resp)",
               "# (P16b) sin copia de respaldo: la coincidencia se mide contra la mascara del GDEM")
src <- cambiar(src, 'f_prev <- file.path(dir_new, "03_MascaraEstable_131026.tif")',
               'f_prev <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER", "03_MascaraEstable.tif")')
src <- cambiar(src, '"coincidencia con la mascara sin las exclusiones nuevas (Jaccard)"',
               '"coincidencia con la mascara del GDEM (Jaccard)"')
src <- cambiar(src, '"rectangulo regional con GDEM"', '"rectangulo regional con TanDEM-X"')
src <- cambiar(src, '"GDEM de origen ASTER con al menos 3 escenas"',
               sprintf('"%s"', if (rama == "tdx90") "TanDEM-X fiable (con valor y HEM < 0,5 m)" else
                 "TanDEM-X fiable (con valor, sin edicion en la EDM y HEM < 0,5 m)"))
src <- bloque(src, "# --- 3. Origen del GDEM", "# --- 4. Hipsometria por glaciar", c(
  "# --- 3. (P16b) Fraccion fiable del TanDEM-X: en el rectangulo y sobre el hielo -------------------",
  "r7 <- vect(f_inv, layer = \"rgi70\")",
  "g7 <- rasterize(r7, plantilla, field = 1, background = 0, touches = FALSE)",
  "g6 <- rasterize(r6, plantilla, field = 1, background = 0, touches = FALSE)",
  "vz <- !is.na(values(gdem, mat = FALSE)); vf <- values(es_aster, mat = FALSE) %in% TRUE",
  "o <- ifelse(vf, \"fiable\", ifelse(vz, \"con valor, no fiable\", \"sin valor\"))",
  "tab_origen <- function(sel, nombre) {",
  "  t <- table(o[sel]); data.frame(dominio = nombre, origen = names(t), pct = round(100 * as.numeric(t) / sum(t), 2))",
  "}",
  "O <- rbind(tab_origen(rep(TRUE, length(o)), \"rectangulo regional\"),",
  "           tab_origen(values(g7, mat = FALSE) == 1, \"glaciares RGI 7.0\"),",
  "           tab_origen(values(g6, mat = FALSE) == 1, \"glaciares RGI 6.0\"))",
  "write.csv(O, file.path(dir_tab, \"M03_Origen_TDX.csv\"), row.names = FALSE)",
  "print(O, row.names = FALSE)",
  ""))
cat("M03 de la cadena con la base", rama, "->", file.path(base_raiz, "01_Datos/Mascara_Estable", carpeta), "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())

src <- readLines(file.path(cadena, "M03b_Hipsometria_Diferencias.R"), encoding = "UTF-8")
src <- cambiar(src, 'dir_new <- file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER")',
               sprintf('dir_new <- file.path(base, "01_Datos/Mascara_Estable", "%s")', carpeta))
cat("M03b de la cadena con la base", rama, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
