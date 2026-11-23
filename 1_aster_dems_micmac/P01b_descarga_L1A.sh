#!/bin/bash
# P01b. Descarga incremental de los L1A V004 (HDF) o AST14DEM (GeoTIFF) de un catalogo de P01, cuatro a la vez.
# Uso: bash P01b_descarga_L1A.sh catalogo.csv carpeta_destino
# Credenciales: Earthdata Login en ~/.netrc (curl -n). No se imprimen.
cat_csv="$1"
dest="$2"
mkdir -p "$dest"
export dest
# firma valida: HDF4 (0e 03 13 01) o TIFF/BigTIFF little-endian (49 49 2a 00 / 49 49 2b 00)
firma_ok() { case "$(head -c 4 "$1" 2>/dev/null | od -An -tx1 | tr -d ' ')" in 0e031301|49492a00|49492b00) return 0;; esac; return 1; }
export -f firma_ok
baja() {
    url="$1"
    f="$dest/$(basename "$url")"
    # incremental: un archivo completo trae su firma y pesa mas de 1 MB
    if [ -s "$f" ] && [ "$(stat -c %s "$f")" -gt 1000000 ] && firma_ok "$f"; then
        return 0
    fi
    ck="$dest/.cookies_$$_$RANDOM"
    for intento in 1 2 3; do
        curl -s -n -L -b "$ck" -c "$ck" -o "$f.part" "$url"
        if firma_ok "$f.part"; then
            mv "$f.part" "$f"; rm -f "$ck"; echo "ok $(basename "$f")"; return 0
        fi
        sleep 5
    done
    rm -f "$f.part" "$ck"; echo "FALLO $(basename "$url")"
}
export -f baja
# columna url del catalogo (5a); sin cabecera
awk -F, 'NR > 1 && $5 ~ /^https/ {print $5}' "$cat_csv" | xargs -P 4 -I{} bash -c 'baja "$@"' _ {}
echo "terminado: $(ls "$dest"/*.hdf 2>/dev/null | wc -l) HDF en $dest"
