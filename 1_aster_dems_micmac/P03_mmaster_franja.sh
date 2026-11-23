#!/bin/bash
# P03. Un DEM MMASTER por franja ASTER L1A, con los comandos y parámetros que usa Hugonnet et al. (2021):
# WorkFlowASTER.sh -s FRANJA -z "19 +south" -a -i 2  seguido de PostProcessMicMac.sh (ver process_l1a.sh del
# repositorio MMASTER-workflows). Corre en Git Bash sobre Windows.
#
# Uso, desde la carpeta que contiene la franja:
#   P03_mmaster_franja.sh FRANJA "19 +south"
# FRANJA es una carpeta con los <granulo>.zip y <granulo>.zip.met que escribe P02_hdf_a_legacy.py; su nombre
# puede ser cualquiera sin espacios (Hugonnet usa los 25 primeros caracteres del primer gránulo). Si NBPROC está
# definida, se pasa NbProc=$NBPROC a Malt (MMTestOrient no tiene esa opción y usa todos los núcleos).
# Si BOX="xmin,ymin,xmax,ymax" (UTM) está definida, la correlación final (Malt) se limita a esa caja con
# BoxTerrain; todo lo anterior (desfase de lado, corrección de la imagen 3B) sigue sobre la franja entera. Es la
# única desviación de tiempo de cómputo respecto de Hugonnet, y se valida en P05d.
# Salida: FRANJA/PROCESSED_FINAL/FRANJA/FRANJA_Z.tif (DEM a 30 m, -9999 sin dato), _CORR.tif (0-100),
#         _HS.tif, _V123.tif, TrackAngleMap_3N/3B.tif, el .zip.met; y FRANJA/tiempos.csv.
#
# Cambios respecto de WorkFlowASTER.sh y PostProcessMicMac.sh (mismos comandos mm3d y mismos parámetros):
#  1. MicMac es la compilación MMASTER_v1.1 para Windows: es la única que
#     trae ApplyParralaxCor con FitASTER=2, ExportFitASTER y ASTERSceneName, la llamada que usaba el flujo
#     entre 2019 y noviembre de 2021 (cuando Hugonnet procesó). La v1.0.beta14_MMASTER (2021) y la v1.2.0
#     (2026) sólo traen FitASTER booleano (versión 1). El comando se escribe «ApplyParralaxCor», como en el
#     flujo de esa época.
#     mm3d se llama con LANG=C: la v1.0.beta14 (Qt 5.14.2) se bloquea en QCoreApplication::setLibraryPaths
#     con Windows en es-CL (Qt emite un aviso al leer el idioma y, para registrarlo, pide el mismo cerrojo que
#     ya tiene tomado). La v1.1 no lo sufre, pero LANG=C no estorba.
#  2. bc no existe en Git Bash ni en conda-forge para Windows: las cuentas de Min/Max/NbLvl/Mean/Inc se hacen
#     con awk, con el mismo truncamiento entero.
#  3. GDAL y gdal_calc.py salen del entorno conda `mmaster`.
#  4. Las llamadas a mosaic_micmac_tiles.py del original tienen las comillas mal ('${$lastMASK...}' no se
#     expande); aquí se pasa el nombre correcto de cada familia de teselas.
#  5. Se añade el registro de tiempos por etapa (tiempos.csv) y un límite de 12 h por llamada a mm3d.
#  6. «Satelib ASTERStrip2MM» se escribe «SateLib» (MicMac no distingue mayúsculas en el comando).
#  7. mm3d corre con un PATH que sólo tiene las carpetas de Windows: MicMac paraleliza con su make.exe, y si
#     make encuentra el sh de Git Bash en el PATH lo usa como intérprete, que se come las barras invertidas
#     de las rutas («C:UserssebasPrueba...: command not found»). Sin sh, make usa cmd.exe. Además se quita
#     la variable SHELL (Git Bash la exporta y el make de Windows también la obedece).
#  8. Con un solo gránulo, los archivos se renombran a «FRANJA.<resto>» y no a los 25 primeros caracteres, para
#     que la carpeta pueda llamarse como se quiera (con el nombre de Hugonnet el resultado es el mismo).
#  9. La ortofoto V123 se lleva al tamaño de la máscara antes de enmascararla (resize_rasters del original).
set -u
MM3D=/c/Users/sebas/Prueba_MMASTER/herramientas/micmac_v11/bin/mm3d.exe
E=/c/Users/sebas/miniconda3/envs/mmaster
MOSAICO=/c/Users/sebas/Prueba_MMASTER/herramientas/mmaster_repo/MMASTER-workflows-master/bin/mosaic_micmac_tiles.py
export PATH="$E/Library/bin:$E:$E/Scripts:$PATH" GDAL_DATA=$E/Library/share/gdal PROJ_DATA=$E/Library/share/proj
gdal_calc() { "$E/python.exe" "$E/Scripts/gdal_calc.py" "$@"; }

name=${1%/}
proj="+proj=utm +zone=$2 +datum=WGS84 +units=m +no_defs"
# parámetros de Hugonnet (-a -i 2) y los valores por omisión de WorkFlowASTER.sh
ZoomF=1; RESTERR=30; CorThr=0.7; SzW=5; do_angle=true; fitVersion=2
NP=""; [ -n "${NBPROC:-}" ] && NP="NbProc=$NBPROC"
BX=""; [ -n "${BOX:-}" ] && BX="BoxTerrain=[$BOX]"
N="_3N"; B="_3B"; Nx="_3N.xml"; Bx="_3B.xml"; Nt="_3N.tif"; Bt="_3B.tif"; Bcor="_3B.tif_corrected.tif"

T0=$(date +%s)
etapa() { echo "$1,$(( $(date +%s) - T0 ))" >> tiempos.csv; echo "=== $(date +%T) $1"; }
mm() { PATH="/c/WINDOWS/system32:/c/WINDOWS:/c/WINDOWS/System32/Wbem" LANG=C /usr/bin/env -u SHELL -u MAKEFLAGS -u MAKELEVEL \
           -u MFLAGS /usr/bin/timeout 12h "$MM3D" "$@" < /dev/null
      local r=$?; [ $r -ne 0 ] && echo "!!! mm3d $1 $2 termina con $r"; return $r; }

echo "Franja: $name   Proyección: $proj"
cd "$name" || exit 1
rm -f tiempos.csv; echo "etapa,segundos_desde_inicio" > tiempos.csv
etapa inicio
mkdir -p RawData zips
nscenes=$(ls *.zip | wc -l)
for f in *.zip; do unzip -q -o "$f" -d RawData; mv "$f" zips/; done
for f in *.met; do [ -f "$f" ] && mv "$f" zips/; done
cd RawData
if [ "$nscenes" -gt 1 ]; then
    mm SateLib ASTERStrip2MM "AST_L1A.*" "$name" || exit 1
else
    for f in *.*; do split=($(echo "$f" | sed 's/\./ /')); mv "$f" "$name.${split[1]}"; done
    mm SateLib ASTERGT2MM "$name" || exit 1
fi
cd ..
etapa ASTERGT2MM

for x in "$name$Bx" "$name$Nx" "FalseColor_$name.xml"; do
    mm SateLib Aster2Grid "$x" 20 "$proj" HMin=-500 HMax=9000 expDIMAP=1 expGrid=1 || exit 1
done
etapa Aster2Grid_1

mm Malt Ortho ".*$name(()|_3N|_3B).tif" GRIBin ImMNT="$name(_3N|_3B).tif" MOri=GRID ZMoy=2500 ZInc=2500 ZoomF=8 ZoomI=32 \
   ResolTerrain=30 NbVI=2 EZA=1 Regul=0.1 DefCor=$CorThr DoOrtho=0 DirMEC=MEC-Mini $NP || exit 1
etapa Malt_baja

st=$(gdalinfo -nomd -norat -noct -nofl -stats MEC-Mini/Z_Num6_DeZoom8_STD-MALT.tif | tr -d '\r' | grep 'Minimum=')
read Min Max NbLvl Mean Inc <<< $(echo "$st" | awk -F'[=,]' '{
    mn = sprintf("%.0f", $2) + 0; mx = sprintf("%.0f", $4) + 0
    if (mn < -420) mn = -420; if (mx > 8850) mx = 8850
    if (mn > 8850) mn = -420;  if (mx < -420) mx = 8850
    nl = int((mx - mn) / 200); if (nl < 10) nl = 10
    me = int((mx + mn) / 2); inc = sprintf("%.0f", mx - me)
    print mn, mx, nl, me, inc }')
echo "Min=$Min Max=$Max NbLvl=$NbLvl Mean=$Mean Inc=$Inc"
echo "Min Max NbLvl Mean Inc" > Stats.txt; echo "$Min $Max $NbLvl $Mean $Inc" >> Stats.txt

for x in "$name$Bx" "$name$Nx" "FalseColor_$name.xml"; do
    mm SateLib Aster2Grid "$x" $NbLvl "$proj" HMin=$Min HMax=$Max expDIMAP=1 expGrid=1 || exit 1
done
etapa Aster2Grid_2

mm MMTestOrient "$name$Bt" "$name$Nt" GRIBin PB=1 MOri=GRID ZoomF=1 ZInc=$Inc ZMoy=$Mean || exit 1
etapa MMTestOrient

mm SateLib ApplyParralaxCor "$name$Bt" GeoI-Px/Px2_Num16_DeZoom1_Geom-Im.tif FitASTER=$fitVersion ExportFitASTER=1 \
   ASTERSceneName=$name || exit 1
mkdir -p ImOrig; mv "$name$Bt" "ImOrig/$name$Bt"; mv "$name$Bcor" "$name$Bt"
etapa ApplyParallaxCor

mm Malt Ortho ".*$name(()|_3N|_3B).tif" GRIBin ImMNT="$name(_3N|_3B).tif" ImOrtho="FalseColor_$name.tif" MOri=GRID \
   ZInc=$Inc ZMoy=$Mean ZoomF=$ZoomF ZoomI=32 ResolTerrain=10 NbVI=2 EZA=1 DefCor=0 Regul=0.1 ResolOrtho=1 SzW=$SzW $NP $BX || exit 1
etapa Malt_final

mm Tawny Ortho-MEC-Malt/ RadiomEgal=0 || exit 1
etapa Tawny

cd MEC-Malt
if ls Z_Num*_DeZoom*_STD-MALT_Tile_*.tif > /dev/null 2>&1; then
    for fam in $(ls Z_Num*_DeZoom*_STD-MALT_Tile_*.tif AutoMask_STD-MALT_Num_*_Tile_*.tif Correl_STD-MALT_Num*_Tile_*.tif \
                 | sed -E 's/_Tile_[0-9]+_[0-9]+\.tif$//' | sort -u); do
        "$E/python.exe" "$MOSAICO" -filename "$fam"
    done
    rm -f *Tile*.tif
    echo "teselas unidas"
fi
lastDEM=$(ls Z_Num*_DeZoom*_STD-MALT.tif | tail -1); lastMask=$(ls AutoMask_STD-MALT_Num_*.tif | tail -1)
lastcor=$(ls Correl_STD-MALT_Num*.tif | tail -1); lastNuage=$(ls NuageImProf_STD-MALT_Etape_*.xml | tail -1)
laststr="${lastDEM%.*}"
mv "$lastcor" Correl_FullRes.tif; cp "$laststr.tfw" Correl_FullRes.tfw
gdal_translate -q -tr $RESTERR $RESTERR -a_srs "$proj" Correl_FullRes.tif "$lastcor"
mv "$lastMask" AutoMask_FullRes.tif; cp "$laststr.tfw" AutoMask_FullRes.tfw
gdal_translate -q -tr $RESTERR $RESTERR -a_srs "$proj" AutoMask_FullRes.tif "$lastMask"
mv "$lastDEM" Z_FullRes.tif; mv "$laststr.tfw" Z_FullRes.tfw; mv "$laststr.xml" Z_FullRes.xml
gdal_translate -q -tr $RESTERR $RESTERR -r cubicspline -a_srs "$proj" -co TFW=YES Z_FullRes.tif "$lastDEM"
cd ..
etapa remuestreo_30m

if [ "$do_angle" = true ]; then
    for band in "$name$N" "$name$B"; do
        mm SateLib ASTERProjAngle "MEC-Malt/$laststr" "MEC-Malt/$lastMask" "$band" || exit 1
        sfx=${band: -2}
        if [ -f "TrackAngleMap_${sfx}_Tile_0_0.tif" ]; then "$E/python.exe" "$MOSAICO" -filename "TrackAngleMap_${sfx}"; fi
        cp "MEC-Malt/$laststr.tfw" TrackAngleMap_nonGT.tfw; mv TrackAngleMap.tif TrackAngleMap_nonGT.tif
        gdal_translate -q -a_srs "$proj" -a_nodata 0 TrackAngleMap_nonGT.tif "TrackAngleMap_${sfx}.tif"
        rm -f TrackAngleMap_nonGT*
    done
fi
etapa ASTERProjAngle

cd Ortho-MEC-Malt
[ -f Orthophotomosaic_Tile_0_0.tif ] && "$E/python.exe" "$MOSAICO" -filename Orthophotomosaic
mv Orthophotomosaic.tif Orthophotomosaic_FullRes.tif; mv Orthophotomosaic.tfw Orthophotomosaic_FullRes.tfw
gdal_translate -q -tr 15 15 -r bilinear -a_srs "$proj" Orthophotomosaic_FullRes.tif Orthophotomosaic.tif
cd ..

# --- PostProcessMicMac.sh (rama con MEC-Malt), para esta franja ---
out=PROCESSED_FINAL/$name; mkdir -p "$out"
cd MEC-Malt
lastimg=$(ls Z_Num[0-9]_DeZoom[0-9]_STD-MALT.tif | tail -1); lastmsk=$(ls AutoMask_STD-MALT_Num_[0-9].tif | tail -1)
lastcor=$(ls Correl_STD-MALT_Num_[0-9].tif | tail -1)
gdal_translate -q -a_nodata 0 -a_srs "$proj" "$lastcor" "${name}_CORR.tif"
gdal_translate -q -a_srs "$proj" "$lastimg" tmp_geo.tif
gdal_translate -q -a_srs "$proj" -a_nodata 0 "$lastmsk" tmp_msk.tif
cd ../Ortho-MEC-Malt
gdal_translate -q -tr 15 15 -a_srs "$proj" -a_nodata 0 "../MEC-Malt/$lastmsk" tmp_mskDouble.tif
gdal_translate -q -tr 15 15 -a_srs "$proj" Orthophotomosaic.tif tmp_V123.tif
tam=$(gdalinfo tmp_mskDouble.tif | tr -d '\r' | grep 'Size is' | grep -o '[0-9]*' | tr '\n' ' ')
esq=$(gdalinfo tmp_mskDouble.tif | tr -d '\r' | grep -E 'Upper Left|Lower Right' | sed -E 's/^[^(]*\( *([-0-9.]+), *([-0-9.]+)\).*/\1 \2/' | tr '\n' ' ')
read ulx uly lrx lry <<< "$esq"
gdalwarp -q -overwrite -te $ulx $lry $lrx $uly -ts $tam tmp_V123.tif tmp_V123_r.tif && mv tmp_V123_r.tif tmp_V123.tif
cd ../MEC-Malt
gdal_calc -A tmp_msk.tif -B tmp_geo.tif --outfile="${name}_Z.tif" --calc="B*(A>0)" --NoDataValue=-9999 --quiet
cp "${name}_Z.tif" "../$out/"
gdaldem hillshade -q "${name}_Z.tif" "../$out/${name}_HS.tif"
gdal_calc -A "${name}_CORR.tif" --outfile="../$out/${name}_CORR.tif" --calc="((A.astype(float)-127)/128)*100" \
          --NoDataValue=-9999 --quiet
rm -f tmp_msk.tif tmp_geo.tif "${name}_Z.tif" "${name}_CORR.tif"
cd ../Ortho-MEC-Malt
gdal_calc -B tmp_mskDouble.tif -A tmp_V123.tif --outfile="${name}_V123.tif" \
          --calc="((A!=255)*(A+1)+(A==255)*A)*(B>0)" --NoDataValue=0 --allBands=A --quiet 2>/dev/null \
  || echo "aviso: V123 no se pudo enmascarar (tamaños distintos); no afecta al DEM"
[ -f "${name}_V123.tif" ] && mv "${name}_V123.tif" "../$out/"
rm -f tmp_V123.tif tmp_mskDouble.tif
cd ..
cp TrackAngleMap*.tif "$out/" 2>/dev/null
cp zips/*.met "$out/" 2>/dev/null
etapa postproceso
echo "Listo: $out/${name}_Z.tif"
