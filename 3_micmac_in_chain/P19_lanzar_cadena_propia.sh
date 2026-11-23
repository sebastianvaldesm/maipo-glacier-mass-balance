#!/bin/bash
# P19. Lanza la cadena sobre los modelos de MicMac (P15 a P18), con prioridad baja de Windows.
#   Fase 1, por rama (GDEM y TanDEM-X 90 m), con trece variantes: P15 M05 en NPROC partes hasta que no
#   quede franja; P15 M05c, P17 y P18.
#   Fase 2, solo CPD: P15 M05cpd en NPROC_CPD partes, P15 combinar, y M05c y P17 sobre <rama>_cpd.
#   Con CPD cada proceso usa hasta 1,6 GB de memoria.
# Uso: bash P19_lanzar_cadena_propia.sh
#      P19_RAMAS="gdem" P19_FASES="2" bash P19_lanzar_cadena_propia.sh
# Se puede cortar y volver a lanzar: retoma lo hecho. Corre desde una copia de los scripts, asi que
# editarlos no afecta una corrida en curso.
M=/c/Users/sebas/Prueba_MMASTER
CP=$M/cadena_propia
R="/c/Users/sebas/OneDrive/Desktop/Memoria de Titulo/03_DEM_MicMac"
AV="/c/Users/sebas/OneDrive/Desktop/Memoria de Titulo/06_Memoria_Final/Metodologias_anteriores/datos"
EC=$CP/en_curso
if [ "$1" != "--copia" ]; then
    mkdir -p "$CP"
    rm -rf "$EC"; cp -r "$R/scripts" "$EC"
    exec bash "$EC/P19_lanzar_cadena_propia.sh" --copia
fi
S=$EC
PY=/c/Users/sebas/venvs/xdem/Scripts/python.exe
RS="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
H=/c/Users/sebas/miniconda3/envs/hydro
export PROJ_DATA=$(cygpath -w $H/Library/share/proj) GDAL_DATA=$(cygpath -w $H/Library/share/gdal) GDAL_PAM_ENABLED=NO
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 MPLBACKEND=Agg
NPROC=${NPROC:-7}
NPROC_CPD=${NPROC_CPD:-6}
RAMAS=${P19_RAMAS:-"gdem tdx90 tdx30"}
FASES=${P19_FASES:-"1 2"}
LOG=$CP/lanzador.log
mkdir -p $CP/logs
pyb() { "$PY" "$S/baja_prioridad.py" -u -W ignore "$@"; }
marca() { echo "$(date '+%d-%m %H:%M') $*" | tee -a $LOG; }

# rondas de M05 (o M05cpd) hasta que no quede nada; $1 rama, $2 etapa, $3 etapa de pendientes, $4 partes
rondas() {
    local rama=$1 etapa=$2 pend=$3 n=$4 sin_avance=0 antes despues
    while true; do
        antes=$(pyb "$S/P15_cadena_propia.py" $rama $pend 2>/dev/null | tail -1)
        [ -z "$antes" ] && { marca "$rama: no se pudo contar lo pendiente ($etapa)"; return 1; }
        [ "$antes" -eq 0 ] && return 0
        marca "$rama: ronda $etapa con $antes franjas, $n partes"
        if [ $etapa = M05 ] && ! pyb "$S/P15_cadena_propia.py" $rama preparar >> $CP/logs/P15_${rama}_preparar.log 2>&1; then
            marca "$rama: FALLO preparar (ver logs/P15_${rama}_preparar.log)"; return 1
        fi
        for ((p = 0; p < n; p++)); do
            pyb "$S/P15_cadena_propia.py" $rama $etapa $p $n >> $CP/logs/${etapa}_${rama}_$p.log 2>&1 &
        done
        wait
        despues=$(pyb "$S/P15_cadena_propia.py" $rama $pend 2>/dev/null | tail -1)
        marca "$rama: ronda $etapa terminada, quedan $despues de $antes"
        if [ "$despues" -ge "$antes" ]; then sin_avance=$((sin_avance + 1)); else sin_avance=0; fi
        if [ $sin_avance -ge 3 ]; then
            marca "$rama: tres rondas $etapa sin avance; quedan $despues (ver logs/${etapa}_${rama}_*.log)"; return 1
        fi
    done
}

# M05c, P17 y copia al avance 5 de una rama (gdem, gdem_cpd, ...)
cerrar() {
    local r=$1
    pyb "$S/P15_cadena_propia.py" $r M05c > $CP/logs/M05c_$r.log 2>&1 || { marca "$r: fallo M05c"; return 1; }
    "$RS" "$S/P17_series_y_balance.R" $r > $CP/logs/P17_$r.log 2>&1 || { marca "$r: fallo P17"; return 1; }
    if [ -f $CP/$r/M06_resultados.rds ]; then
        mkdir -p "$AV/mmaster_$r"
        cp -r $CP/$r/tablas $CP/$r/tex $CP/$r/M06_resultados.rds $CP/$r/franjas.csv "$AV/mmaster_$r/"
        marca "$r: terminada; resultados copiados a 06_Memoria_Final/Metodologias_anteriores/datos/mmaster_$r"
    fi
}

marca "inicio: ramas $RAMAS, fases $FASES | $NPROC partes sin CPD, $NPROC_CPD con CPD"
for fase in $FASES; do
    for rama in $RAMAS; do
        if [ $fase = 1 ]; then
            # --- base TanDEM-X ----------------------------------------------------------------------------
            if [ $rama != gdem ]; then
                B="/c/Users/sebas/OneDrive/Desktop/Memoria de Titulo/01_Datos/Mascara_Estable/MascaraEstable_RM_${rama^^}"
                if [ ! -f "$B/01_Hipsometria_dga.csv" ]; then
                    marca "$rama: base (P16 preparar, P16b, P16 resumen)"
                    pyb "$S/P16_base_tandemx.py" $rama preparar > $CP/logs/P16_${rama}_preparar.log 2>&1 \
                        && "$RS" "$S/P16b_base_tandemx.R" $rama > $CP/logs/P16b_$rama.log 2>&1 \
                        && pyb "$S/P16_base_tandemx.py" $rama resumen > $CP/logs/P16_${rama}_resumen.log 2>&1
                    if [ ! -f "$B/01_Hipsometria_dga.csv" ]; then
                        marca "$rama: FALLO la base (ver $CP/logs/P16*_$rama*.log); se salta la rama"
                        continue
                    fi
                fi
            fi
            pyb "$S/P15_cadena_propia.py" $rama lista >> $CP/logs/P15_${rama}_lista.log 2>&1
            export P15_SIN_CPD=1
            rondas $rama M05 pendientes $NPROC
            unset P15_SIN_CPD
            cerrar $rama
            "$PY" -u -W ignore "$S/P18_comparacion_cadena_propia.py" alineacion $rama > $CP/logs/P18_$rama.log 2>&1
        else
            [ -f $CP/$rama/franjas.csv ] || { marca "$rama: sin franjas.csv, se salta la fase 2"; continue; }
            rondas $rama M05cpd pendientes_cpd $NPROC_CPD
            pyb "$S/P15_cadena_propia.py" $rama combinar > $CP/logs/P15_${rama}_combinar.log 2>&1 \
                || marca "$rama: combinar dejo franjas sin las dos fases (ver logs/P15_${rama}_combinar.log)"
            tail -2 $CP/logs/P15_${rama}_combinar.log | while read -r l; do marca "$rama: $l"; done
            cerrar ${rama}_cpd
        fi
    done
done
"$H/python.exe" -u -W ignore "$S/P18_comparacion_cadena_propia.py" final > $CP/logs/P18_final.log 2>&1 \
    && marca "comparacion final en resultados/P18_comparacion_resumen.csv" || marca "fallo P18 final"
marca "fin"
