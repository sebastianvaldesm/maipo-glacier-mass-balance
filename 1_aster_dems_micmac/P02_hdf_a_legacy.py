# P02. Convierte un gránulo ASTER L1A V004 (HDF-EOS2) al paquete «GeoTIFF heredado» que lee MicMac
# (mm3d SateLib ASTERGT2MM / ASTERStrip2MM) y que espera el flujo MMASTER (WorkFlowASTER.sh).
#
# Uso (entorno conda `mmaster`, con GDAL_DATA y PROJ_DATA exportados):
#   python -I P02_hdf_a_legacy.py <granulo.hdf> <carpeta_salida>
# Escribe en <carpeta_salida>: <granulo>.zip (los archivos de abajo) y <granulo>.zip.met.
#
# Qué lee MicMac (src/SateLib/ASTERGT2MM.cpp, rama IncludeALGLIB), por banda VNIR_Band1, 2, 3N, 3B:
#   .ImageData.tif              imagen uint8 (3N y bandas 1-2: 4200 x 4100; 3B: 5400 x 5000)
#   .RadiometricCorrTable.txt   tres números por línea (D A G): DN corregido = A*DN/G + D, una línea por columna
# y sólo para 3N y 3B:
#   .LatticePoint.txt           pares «columna línea» (x y) de la grilla de 11 puntos por fila
#   .SatellitePosition.txt      «x y z» ECEF, una línea por fila de la grilla
#   .Latitude.txt/.Longitude.txt  11 valores por fila; latitud GEOCÉNTRICA (MicMac la convierte a ECEF con la
#                               fórmula geocéntrica, que es como viene en el L1A)
#   .ObservationTime.txt        4 números (sólo 3N; da la fecha del XML)
# Todo se lee como números separados por espacios, así que el formato de línea no importa.
#
# Qué corrige respecto de asterV4HDF_to_legacyGEOTIFF.py del repositorio MMASTER (declarado «vibecoded»):
#  1. Latitud y longitud: aquél las leía con pyhdf por nombre, y como todas las bandas comparten el nombre
#     «Latitude» sólo queda una, que copiaba a las cuatro bandas VNIR. La 3B tiene otra grilla (15 x 11 contra
#     12 x 11) y otra geometría. Aquí se leen por banda con HDF4_EOS:EOS_SWATH_GEOL.
#  2. ObservationTime: el L1A guarda los milisegundos del día partidos en dos uint16 (ms = b*65536 + c), pero
#     MicMac calcula b*100000 + c. Se reescribe b = ms // 100000, c = ms % 100000 para que la fecha salga bien.
#  3. Posiciones del satélite con 17 cifras: ASTERStrip2MM une gránulos buscando posiciones IGUALES entre uno y
#     el siguiente, así que no se pueden redondear.
#  4. El .met: pymmaster (get_aster_footprint) busca líneas con «GRingPointLatitude/Longitude». El V004 no las
#     trae; se escriben con las cuatro esquinas de la grilla de 3N.
import os
import sys
import zipfile

import numpy as np
from osgeo import gdal

gdal.UseExceptions()
BANDAS = ["VNIR_Band1", "VNIR_Band2", "VNIR_Band3N", "VNIR_Band3B"]


def leer(hdf, banda, campo, geol=False):
    tipo = "EOS_SWATH_GEOL" if geol else "EOS_SWATH"
    return gdal.Open(f'HDF4_EOS:{tipo}:"{hdf}":{banda}:{campo}').ReadAsArray()


def escribir(ruta, filas, fmt):
    with open(ruta, "w") as f:
        for fila in filas:
            f.write(" ".join(fmt % v for v in np.ravel(fila)) + "\n")


def convertir(hdf, salida):
    nombre = os.path.splitext(os.path.basename(hdf))[0]
    tmp = os.path.join(salida, nombre + "_tmp")
    os.makedirs(tmp, exist_ok=True)
    archivos = []
    for b in BANDAS:
        base = os.path.join(tmp, f"{nombre}.{b}.")
        gdal.Translate(base + "ImageData.tif", f'HDF4_EOS:EOS_SWATH:"{hdf}":{b}:ImageData', format="GTiff")
        archivos.append(base + "ImageData.tif")
        escribir(base + "RadiometricCorrTable.txt", leer(hdf, b, "RadiometricCorrTable"), "%.9g")
        lp = leer(hdf, b, "LatticePoint")            # (filas, 11, 2): [columna, línea]
        escribir(base + "LatticePoint.txt", lp.reshape(-1, 2), "%d")
        escribir(base + "SatellitePosition.txt", leer(hdf, b, "SatellitePosition"), "%.17g")
        escribir(base + "SatelliteVelocity.txt", leer(hdf, b, "SatelliteVelocity"), "%.17g")
        escribir(base + "SightVector.txt", leer(hdf, b, "SightVector").reshape(-1, 3), "%.17g")
        escribir(base + "Latitude.txt", leer(hdf, b, "Latitude", geol=True), "%.17g")
        escribir(base + "Longitude.txt", leer(hdf, b, "Longitude", geol=True), "%.17g")
        ot = leer(hdf, b, "ObservationTime").astype(np.int64)   # día desde 1958, ms (2 x uint16), µs
        ms = ot[:, 1] * 65536 + ot[:, 2]
        escribir(base + "ObservationTime.txt", np.column_stack([ot[:, 0], ms // 100000, ms % 100000, ot[:, 3]]), "%d")
        archivos += [base + s for s in ["RadiometricCorrTable.txt", "LatticePoint.txt", "SatellitePosition.txt",
                                        "SatelliteVelocity.txt", "SightVector.txt", "Latitude.txt", "Longitude.txt",
                                        "ObservationTime.txt"]]
    # contorno: esquinas de la grilla de 3N. La latitud del L1A es geocentrica (asi la usa MicMac, que la lee de
    # Latitude.txt); pymmaster lee la del .met como geodesica WGS84, asi que aqui se convierte. Sin esto la huella
    # quedaba 18 a 21 km al norte del DEM.
    lat = leer(hdf, "VNIR_Band3N", "Latitude", geol=True)
    lat = np.degrees(np.arctan(np.tan(np.radians(lat)) / (1 - 0.00669437999014)))
    lon = leer(hdf, "VNIR_Band3N", "Longitude", geol=True)
    esq = [(0, 0), (0, -1), (-1, -1), (-1, 0)]
    la = ", ".join("%.6f" % lat[i, j] for i, j in esq)
    lo = ", ".join("%.6f" % lon[i, j] for i, j in esq)
    from pyhdf.SD import SD, SDC
    core = SD(hdf, SDC.READ).attributes().get("coremetadata.0", "")
    with open(os.path.join(salida, nombre + ".zip.met"), "w") as f:
        f.write(core.rstrip("\x00") + "\n")
        f.write("OBJECT = GRingPointLatitude\n  NUM_VAL = 4\n  VALUE = (%s)\nEND_OBJECT = GRingPointLatitude\n" % la)
        f.write("OBJECT = GRingPointLongitude\n  NUM_VAL = 4\n  VALUE = (%s)\nEND_OBJECT = GRingPointLongitude\n" % lo)
    with zipfile.ZipFile(os.path.join(salida, nombre + ".zip"), "w", zipfile.ZIP_DEFLATED) as z:
        for a in archivos:
            z.write(a, arcname=os.path.basename(a))
    for a in archivos:
        os.remove(a)
    for a in os.listdir(tmp):          # .aux.xml que pudiera dejar GDAL
        os.remove(os.path.join(tmp, a))
    os.rmdir(tmp)
    print(nombre, "->", os.path.join(salida, nombre + ".zip"))


if __name__ == "__main__":
    os.makedirs(sys.argv[2], exist_ok=True)
    convertir(os.path.abspath(sys.argv[1]), sys.argv[2])
