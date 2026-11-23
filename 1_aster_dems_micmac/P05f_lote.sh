#!/bin/bash
# P05f. Lote de franjas MMASTER: por cada franja de la seleccion (P05e) convierte sus granulos L1A (P02), corre
# MicMac (P03) y borra los temporales de MicMac (unos 7 GB por franja), conservando PROCESSED_FINAL y tiempos.csv.
# Dos franjas a la vez (MicMac usa la CPU al 65 % en la correlacion final y el pico de memoria es 7,3 GB).
# Es incremental: una franja con su DEM final no se vuelve a correr.
# Uso: bash P05f_lote.sh seleccion.csv [paralelas]
#   BOX (opcional, "xmin,ymin,xmax,ymax" en UTM 19S) limita la correlacion final, ver P03.
# El nombre de cada franja son los 25 primeros caracteres de su primer granulo (AST_L1A_004MMDDAAAAhhmmss), como
# en el flujo de Hugonnet; asi pyddem lee la fecha del nombre.
# S (carpeta de P02 y P03) se puede cambiar desde fuera, para correr un lote largo con una copia de los scripts.
# Los L1A se buscan en L1A/volcan y en L1A/cuenca (P11).
S="${S:-/c/Users/sebas/OneDrive/Desktop/Memoria de Titulo/03_DEM_MicMac/scripts}"
W=/c/Users/sebas/Prueba_MMASTER/trabajo
L1A="/c/Users/sebas/Prueba_MMASTER/L1A/volcan /c/Users/sebas/Prueba_MMASTER/L1A/cuenca"
E=/c/Users/sebas/miniconda3/envs/mmaster
export S W L1A E BOX="${BOX:-}"
una() {
    granulos="$1"
    primero=${granulos%% *}
    name=${primero:0:25}
    fin="$W/$name/PROCESSED_FINAL/$name/${name}_Z.tif"
    [ -f "$fin" ] && { echo "ya estaba $name"; return 0; }
    rm -rf "$W/$name"; mkdir -p "$W/$name"
    t0=$(date +%s)
    for g in $granulos; do
        # si la descarga va en paralelo, se espera el L1A hasta 12 h (P01b lo deja con su nombre solo al terminar)
        hdf=""; espera=0
        while :; do
            for d in $L1A; do [ -f "$d/$g.hdf" ] && hdf="$d/$g.hdf"; done
            [ -n "$hdf" ] || [ $espera -ge 720 ] && break
            sleep 60; espera=$((espera + 1))
        done
        [ -n "$hdf" ] || { echo "FALTA L1A $name ($g)"; return 1; }
        GDAL_DATA=$(cygpath -w $E/Library/share/gdal) PROJ_DATA=$(cygpath -w $E/Library/share/proj) \
            "$E/python.exe" -I "$S/P02_hdf_a_legacy.py" "$hdf" "$W/$name" >> "$W/$name.log" 2>&1 \
            || { echo "FALLO conversion $name ($g)"; return 1; }
    done
    ( cd "$W" && bash "$S/P03_mmaster_franja.sh" "$name" "19 +south" >> "$W/$name.log" 2>&1 )
    # temporales de MicMac fuera; queda PROCESSED_FINAL y tiempos.csv
    find "$W/$name" -mindepth 1 -maxdepth 1 ! -name PROCESSED_FINAL ! -name tiempos.csv -exec rm -rf {} +
    if [ -f "$fin" ]; then echo "ok $name $(( $(date +%s) - t0 )) s"; else echo "SIN DEM $name $(( $(date +%s) - t0 )) s"; fi
}
export -f una
# columna 4 de la seleccion: granulos separados por espacio
awk -F, 'NR > 1 {print $4}' "$1" | xargs -P "${2:-2}" -I{} bash -c 'una "$@"' _ {}
echo "terminado $(date +%T)"
