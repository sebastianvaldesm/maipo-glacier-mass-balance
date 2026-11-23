# T01. Los dos periodos cortados a mitad de 2010, antes y durante la megasequia
#
# Corre M06 sobre una rama de MicMac con CPD (lee sus tablas sin tocarlas), con T_CORTE = 2010.5 y la
# recta quebrada de quebrada.R. El verano de 2010 queda en el primer periodo. Con 2010.0 pasa al
# segundo (prueba de sensibilidad). Las columnas siguen llamandose tasa_2001_2013 y tasa_2013_2026,
# pero aqui son 2001-2010 y 2010-2025.
# Uso: Rscript T01_Tramos_2010.R [2010.5|2010.0] [gdem_cpd|tdx90_cpd]
# Sale: corte_<corte>/ (o tdx90_cpd/corte_<corte>/)
args  <- commandArgs(trailingOnly = TRUE)
corte <- if (length(args)) args[1] else "2010.5"
nrama <- if (length(args) > 1) args[2] else "gdem_cpd"
if (!corte %in% c("2010.5", "2010.0") || !nrama %in% c("gdem_cpd", "tdx90_cpd"))
  stop("Uso: Rscript T01_Tramos_2010.R [2010.5|2010.0] [gdem_cpd|tdx90_cpd]")
raiz_t <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo/06_Memoria_Final/Tramos_2010"
aqui   <- if (nrama == "gdem_cpd") file.path(raiz_t, paste0("corte_", corte)) else file.path(raiz_t, nrama, paste0("corte_", corte))
cadena <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo/02_Balance_Masa_RM/Cadena_RM"
rama   <- file.path("C:/Users/sebas/Prueba_MMASTER/cadena_propia", nrama)
dir_t  <- file.path(aqui, "tablas")
dir.create(dir_t, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(rama, "tablas", "M05c_Serie.csv"))) stop("Falta M05c de la rama: ", rama)
Sys.setenv(CADENA_RM_SUFIJO = "", CADENA_RM_SALIDA = "M05_salida")

cambiar <- function(src, viejo, nuevo, veces = 1) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != veces) stop("El texto aparece ", length(i), " veces en vez de ", veces, ": ", viejo)
  src[i] <- gsub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
q <- function(x) sprintf('"%s"', x)

src <- readLines(file.path(cadena, "M06_Series_y_Balance.R"), encoding = "UTF-8")
# lo mismo que P17 para la rama gdem_cpd
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(rama)))
src <- cambiar(src, 'file.path(dir_tab, "M02_Sistemas.csv")', sprintf('file.path(%s, "M02_Sistemas.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_tab, "M02_Subsubcuencas_DGA.csv")',
               sprintf('file.path(%s, "M02_Subsubcuencas_DGA.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")',
               sprintf('file.path(%s, "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")', q(file.path(cadena, "datos_externos"))))
if (nrama == "tdx90_cpd") src <- cambiar(src, '"MascaraEstable_RM_ASTER"', q("MascaraEstable_RM_TDX90"), veces = 2)
# lo propio de esta variante: escribir aqui y cortar en 2010
src <- cambiar(src, 'dir_out <- if (SUF == "") dir_tab else file.path(dir_tab, paste0("variante", SUF))', paste("dir_out <-", q(dir_t)))
src <- cambiar(src, 'file.path(if (SUF == "") dir_c else dir_out, "M06_resultados.rds")', sprintf('file.path(%s, "M06_resultados.rds")', q(aqui)))
src <- cambiar(src, 'T_CORTE <- 2013.5', paste('T_CORTE <-', corte))
# los dos periodos son una recta quebrada continua en el corte (ver quebrada.R)
source(file.path(raiz_t, "quebrada.R"))
src <- aplicar_quebrada(src)
cat("M06 de la cadena sobre", rama, "con corte en", corte, "->", aqui, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
