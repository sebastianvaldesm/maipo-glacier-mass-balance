# C01. Las versiones anteriores del metodo medidas como la final (Tabla A2 del articulo): corre M06 con
# error por remuestreo y recta quebrada en 2010,5 sobre
#   AST_13M        modelos AST14DEM de la NASA, trece variantes de xDEM
#   AST_13M_ALT50  lo mismo sin la regla del 90 % de los pixeles
#   AST_NKD        AST14DEM con una sola variante, Nuth y Kaab mas plano
#   MM_TDX90_13M   MicMac alineado al TanDEM-X de 90 m, trece variantes
#   MM_GDEM_13M    MicMac alineado al GDEM, trece variantes (la final suma CPD)
# Uso: Rscript C01_M06_Metodologias.R <metodologia>     (despues C02_Tabla_A2.R)
# Sale: C:/Users/sebas/Prueba_MMASTER/metodologias_m06/<metodologia>/
args <- commandArgs(trailingOnly = TRUE)
RAIZ   <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
cadena <- file.path(RAIZ, "02_Balance_Masa_RM", "Cadena_RM")
MM     <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia"
VAR    <- "C:/Users/sebas/Prueba_MMASTER/variantes_m05c"
# metodologia = c(dir_c con M05_salida y M05b_salida, carpeta del M05c, nombre de M05_salida, mascara)
DEF <- list(
  AST_13M       = c(cadena, file.path(RAIZ, "06_Memoria_Final/Metodologias_anteriores/datos/ast14dem/tablas"), "M05_salida", "ASTER"),
  AST_13M_ALT50 = c(cadena, file.path(VAR, "AST_13M_ALT50"), "M05_salida", "ASTER"),
  AST_NKD       = c(cadena, file.path(VAR, "AST_NKD"), "M05_NKDeramp", "ASTER"),
  MM_TDX90_13M  = c(file.path(MM, "tdx90"), file.path(MM, "tdx90", "tablas"), "M05_salida", "TDX90"),
  MM_GDEM_13M   = c(file.path(MM, "gdem"), file.path(MM, "gdem", "tablas"), "M05_salida", "ASTER"))
if (length(args) != 1 || !args[1] %in% names(DEF)) stop("Uso: Rscript C01_M06_Metodologias.R <", paste(names(DEF), collapse = "|"), ">")
m <- args[1]; d <- DEF[[m]]
out <- file.path("C:/Users/sebas/Prueba_MMASTER/metodologias_m06", m)
dir.create(file.path(out, "tablas"), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(d[2], "M05c_Serie.csv"))) stop("Falta M05c: ", d[2])
if (!dir.exists(file.path(d[1], d[3]))) stop("Falta ", file.path(d[1], d[3]))
Sys.setenv(CADENA_RM_SUFIJO = "", CADENA_RM_SALIDA = d[3])

cambiar <- function(src, viejo, nuevo, veces = 1) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != veces) stop("El texto aparece ", length(i), " veces en vez de ", veces, ": ", viejo)
  src[i] <- gsub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
q <- function(x) sprintf('"%s"', x)

src <- readLines(file.path(cadena, "M06_Series_y_Balance.R"), encoding = "UTF-8")
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(d[1])))
src <- cambiar(src, 'file.path(dir_tab, "M02_Sistemas.csv")', sprintf('file.path(%s, "M02_Sistemas.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_tab, "M02_Subsubcuencas_DGA.csv")',
               sprintf('file.path(%s, "M02_Subsubcuencas_DGA.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")',
               sprintf('file.path(%s, "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")', q(file.path(cadena, "datos_externos"))))
if (d[4] == "TDX90") src <- cambiar(src, '"MascaraEstable_RM_ASTER"', q("MascaraEstable_RM_TDX90"), veces = 2)
src <- cambiar(src, 'file.path(dir_tab, paste0("M05c_', sprintf('file.path(%s, paste0("M05c_', q(d[2])), veces = 4)
src <- cambiar(src, 'dir_out <- if (SUF == "") dir_tab else file.path(dir_tab, paste0("variante", SUF))', paste("dir_out <-", q(file.path(out, "tablas"))))
src <- cambiar(src, 'file.path(if (SUF == "") dir_c else dir_out, "M06_resultados.rds")', sprintf('file.path(%s, "M06_resultados.rds")', q(out)))
src <- cambiar(src, 'T_CORTE <- 2013.5', 'T_CORTE <- 2010.5')
source(file.path(RAIZ, "06_Memoria_Final", "Tramos_2010", "quebrada.R"))
src <- aplicar_quebrada(src)
cat("M06 de la cadena,", m, "->", out, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
