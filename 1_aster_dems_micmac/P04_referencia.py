# P04. Referencia de MicMac: ASTER GDEM V3 (solo pixeles hechos con escenas ASTER) a 90 m y sobre la
# elipsoide WGS84, que es donde MMASTER entrega las alturas; la ondulacion EGM96 para pasar de una
# superficie a la otra; y el RGI 6.0 de la zona como mascara de glaciares.
# Uso: python P04_referencia.py carpeta_salida
import os
import sys
import zipfile

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.merge import merge
from rasterio.warp import Resampling, calculate_default_transform, reproject, transform_bounds
from shapely.geometry import box

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
B = "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
UTM = "EPSG:32719"
LON0, LAT0, LON1, LAT1 = -71.0, -35.0, -69.0, -32.0       # extension de las franjas (180 km N-S, 60 km E-O)
ZONA = (-70.05, -33.80, -69.82, -33.66)                    # zona de prueba Volcan-Marmolejo (lon/lat)
RES = 90.0

# 1. ASTER GDEM V3, las seis teselas de 01_Datos/Brutos/ASTER_GDEM_V3, para que todo sea ASTER. Como en la
#    cadena (M03), sólo los pixeles hechos con al menos 3 escenas
#    ASTER (NUM 3-50, o 63-110 en el relleno del V3); los rellenos con otros modelos quedan vacios, que es lo que
#    hacia el filtro de Hugonnet sobre el TanDEM-X. Se lleva a 90 m promediando, la resolucion de la referencia de
#    Hugonnet, lo que ademas baja el ruido del GDEM.
DIR_G = os.path.join(B, "01_Datos/Brutos/ASTER_GDEM_V3")
def mosaico(capa):
    ts = [rasterio.open(os.path.join(DIR_G, f"ASTGTMV003_S{lat:02d}W{lon:03d}_{capa}.tif"))
          for lat in (33, 34, 35) for lon in (71, 70)]
    m, t = merge(ts, bounds=(LON0, LAT0, LON1, LAT1))
    return m[0], t, ts[0].crs

dem, tr, src_crs = mosaico("dem")
num, _, _ = mosaico("num")
valido = ((num >= 3) & (num <= 50)) | ((num >= 63) & (num <= 110))
x0, y0, x1, y1 = transform_bounds("EPSG:4326", UTM, LON0, LAT0, LON1, LAT1)
x0, y0 = np.floor(x0 / RES) * RES, np.floor(y0 / RES) * RES
x1, y1 = np.ceil(x1 / RES) * RES, np.ceil(y1 / RES) * RES
W, H = int((x1 - x0) / RES), int((y1 - y0) / RES)
dst_tr = rasterio.transform.from_origin(x0, y1, RES, RES)
gdem = np.full((H, W), np.nan, dtype="float32")
reproject(np.where(valido, dem, np.nan).astype("float32"), gdem, src_transform=tr, src_crs=src_crs, src_nodata=np.nan,
          dst_transform=dst_tr, dst_crs=UTM, resampling=Resampling.average, dst_nodata=np.nan)
# se conserva una celda de 90 m sólo si todos sus pixeles de 30 m son de origen ASTER
fv = np.full((H, W), np.nan, dtype="float32")
reproject(valido.astype("float32"), fv, src_transform=tr, src_crs=src_crs, dst_transform=dst_tr, dst_crs=UTM,
          resampling=Resampling.average)
gdem = np.where(fv > 0.999, gdem, np.nan).astype("float32")
print(f"referencia GDEM: {np.isfinite(gdem).mean():.1%} de las celdas con dato de origen ASTER")

# 2. Ondulaciones del geoide (grillas de PROJ, lectura parcial por HTTP)
def ondulacion(url):
    with rasterio.open("/vsicurl/" + url) as g:
        win = g.window(LON0 - 0.5, LAT0 - 0.5, LON1 + 0.5, LAT1 + 0.5)
        a = g.read(1, window=win).astype("float32")
        t = g.window_transform(win)
        o = np.full((H, W), np.nan, dtype="float32")
        reproject(a, o, src_transform=t, src_crs="EPSG:4326", dst_transform=dst_tr, dst_crs=UTM, resampling=Resampling.bilinear)
    return o

n08 = ondulacion("https://cdn.proj.org/us_nga_egm08_25.tif")
n96 = ondulacion("https://cdn.proj.org/us_nga_egm96_15.tif")
perfil = dict(driver="GTiff", height=H, width=W, count=1, dtype="float32", crs=UTM, transform=dst_tr, nodata=np.nan,
              compress="deflate", tiled=True)
# el GDEM V3 esta sobre el geoide EGM96; MMASTER entrega alturas sobre la elipsoide
for nombre, arr in (("referencia_GDEM_elipsoide.tif", gdem + n96), ("ondulacion_EGM08.tif", n08),
                    ("ondulacion_EGM96.tif", n96)):
    with rasterio.open(os.path.join(out, nombre), "w", **perfil) as d:
        d.write(arr, 1)
print(f"referencia {W} x {H} px a {RES:.0f} m; N08 {np.nanmin(n08):.1f}..{np.nanmax(n08):.1f} m; "
      f"N08-N96 {np.nanmin(n08 - n96):+.2f}..{np.nanmax(n08 - n96):+.2f} m")

# 3. RGI 6.0 (region 17) de la extension de las franjas y de la zona de prueba
zp = os.path.join(B, "01_Datos/Brutos/Inventario_RGI/nsidc0770_17.rgi60.SouthernAndes.zip")
with zipfile.ZipFile(zp) as z:
    shp = [n for n in z.namelist() if n.endswith(".shp")][0]
rgi = gpd.read_file(f"zip://{zp}!{shp}", bbox=(LON0, LAT0, LON1, LAT1))
rgi.to_crs(UTM).to_file(os.path.join(out, "rgi60_franjas.shp"))
zona = rgi[rgi.intersects(box(*ZONA))]
zona.to_crs(UTM).to_file(os.path.join(out, "rgi60_zona.shp"))
print(f"RGI 6.0: {len(rgi)} glaciares en las franjas; {len(zona)} tocan la zona de prueba, "
      f"{zona.Area.sum():.1f} km2")
