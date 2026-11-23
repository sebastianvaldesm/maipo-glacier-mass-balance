# P17. Series, balance y contraste de una rama de MicMac: corre M06 y M06c de la cadena cambiando solo
# las rutas de entrada y salida. Valen solo las filas de la cuenca alta del Maipo y sus grupos: las
# demas filas incluyen unidades que la rama no mide.
# Uso: Rscript P17_series_y_balance.R gdem|tdx90   (o gdem_cpd|tdx90_cpd)

args <- commandArgs(trailingOnly = TRUE)
rama <- if (length(args)) args[1] else ""
bases <- c(gdem = "MascaraEstable_RM_ASTER", tdx90 = "MascaraEstable_RM_TDX90", tdx30 = "MascaraEstable_RM_TDX30")
# la combinacion con CPD usa la base de su rama
bases <- c(bases, setNames(bases, paste0(names(bases), "_cpd")))
if (!rama %in% names(bases)) stop("Uso: Rscript P17_series_y_balance.R gdem|tdx90|tdx30 (o <rama>_cpd)")
raiz <- Sys.getenv("P15_RAIZ", "C:/Users/sebas/Prueba_MMASTER/cadena_propia")
dir_rama <- gsub("\\\\", "/", file.path(raiz, rama))
cadena <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo/02_Balance_Masa_RM/Cadena_RM"
if (!file.exists(file.path(dir_rama, "tablas", "M05c_Serie.csv"))) stop("Falta M05c de la rama: ", dir_rama)
dir.create(file.path(dir_rama, "tex"), showWarnings = FALSE)
Sys.setenv(CADENA_RM_SUFIJO = "", CADENA_RM_SALIDA = "M05_salida")

cambiar <- function(src, viejo, nuevo, veces = 1) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != veces) stop("El texto aparece ", length(i), " veces en vez de ", veces, ": ", viejo)
  src[i] <- gsub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
q <- function(x) sprintf('"%s"', x)

src <- readLines(file.path(cadena, "M06_Series_y_Balance.R"), encoding = "UTF-8")
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(dir_rama)))
src <- cambiar(src, 'file.path(dir_tab, "M02_Sistemas.csv")', sprintf('file.path(%s, "M02_Sistemas.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_tab, "M02_Subsubcuencas_DGA.csv")',
               sprintf('file.path(%s, "M02_Subsubcuencas_DGA.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")',
               sprintf('file.path(%s, "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")', q(file.path(cadena, "datos_externos"))))
src <- cambiar(src, '"MascaraEstable_RM_ASTER"', q(bases[[rama]]), veces = 2)
cat("M06 de la cadena sobre la rama", rama, "->", dir_rama, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())

src <- readLines(file.path(cadena, "M06c_Contraste_Hugonnet.R"), encoding = "UTF-8")
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(dir_rama)))
src <- cambiar(src, 'dir_tex <- file.path(base, "02_Balance_Masa_RM", "Reporte_1_RM", "tab")',
               paste("dir_tex <-", q(file.path(dir_rama, "tex"))))
src <- cambiar(src, 'f_hug   <- file.path(dir_c, "datos_externos", "hugonnet_2021_ds_rgi60_pergla_rates_10_20_region17.csv")',
               sprintf('f_hug   <- file.path(%s, "hugonnet_2021_ds_rgi60_pergla_rates_10_20_region17.csv")',
                       q(file.path(cadena, "datos_externos"))))
cat("M06c de la cadena sobre la rama", rama, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
