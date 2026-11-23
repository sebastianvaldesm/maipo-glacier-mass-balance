# -*- coding: utf-8 -*-
"""
P16. Base TanDEM-X 90 m, para comparar la alineacion contra la referencia que uso Hugonnet et al.
(2021). Lleva el TanDEM-X global a la grilla regional de 30 m, usa solo los pixeles con error de
altura (HEM) bajo 0,5 m y pasa las alturas de la elipsoide a EGM96. P16b arma despues su mascara e
hipsometria con el codigo de M03.
Uso: python P16_base_tandemx.py preparar|resumen
"""

import glob
import os
import sys
import zipfile

os.environ["GDAL_PAM_ENABLED"] = "NO"
import numpy as np
import pandas as pd
import rasterio
from rasterio.enums import Resampling
from rasterio.merge import merge
from rasterio.warp import reproject

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
D90 = os.path.join(RAIZ, "01_Datos", "Brutos", "TanDEM-X_90m")
D30 = os.path.join(RAIZ, "01_Datos", "Brutos", "TanDEM-X_30m_EDEM")
GDEM = os.path.join(RAIZ, "01_Datos", "Mascara_Estable", "MascaraEstable_RM_ASTER")
RES = os.path.join(RAIZ, "03_DEM_MicMac", "resultados")
F_N96 = r"C:\Users\sebas\Prueba_MMASTER\referencia\ondulacion_EGM96.tif"
F_INV = os.path.join(RAIZ, "01_Datos", "Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")
F_TASA = os.path.join(RAIZ, "02_Balance_Masa_RM", "Cadena_RM", "tablas", "M06_Tasa_por_Glaciar.csv")
TESELAS = ["S33W071", "S33W070", "S34W071", "S34W070", "S35W071", "S35W070"]
HEM_MAX = 0.5
INVALIDO = -32767.0
FILTRO_EN_HIELO = os.environ.get("P16_FILTRO_EN_HIELO", "") == "1"

if len(sys.argv) < 3 or sys.argv[1] not in ("tdx90", "tdx30") or sys.argv[2] not in ("preparar", "resumen"):
    sys.exit(__doc__)
RAMA, ETAPA = sys.argv[1], sys.argv[2]
OUT = os.path.join(RAIZ, "01_Datos", "Mascara_Estable", "MascaraEstable_RM_" + RAMA.upper())
os.makedirs(OUT, exist_ok=True)

with rasterio.open(os.path.join(GDEM, "01_GDEM.tif")) as r:
    TR, CRS, SH = r.transform, r.crs, r.shape
    PERFIL = r.profile


def a_grilla(arr, transform, crs, metodo):
    o = np.full(SH, np.nan, dtype="float32")
    reproject(arr.astype("float32"), o, src_transform=transform, src_crs=crs, src_nodata=np.nan,
              dst_transform=TR, dst_crs=CRS, dst_nodata=np.nan, resampling=metodo)
    return o


def escribir(nombre, arr, tipo="float32", nodata=np.nan):
    p = dict(PERFIL, dtype=tipo, nodata=nodata, compress="deflate", tiled=True, count=1)
    with rasterio.open(os.path.join(OUT, nombre), "w", **p) as d:
        d.write(arr.astype(tipo), 1)


def mosaico(fs, nodata=INVALIDO):
    """Las seis teselas unidas; nodata=None usa el que declara el archivo (la EDM declara 0)."""
    ds = [rasterio.open(f) for f in fs]
    m, t = merge(ds, nodata=nodata)
    crs = ds[0].crs
    for d in ds:
        d.close()
    return m[0], t, crs


def descomprimir_edem():
    """Descomprime en su misma carpeta los zip del EDEM que esten completos (los que se estan bajando no
    tienen todavia directorio central y zipfile los rechaza)."""
    for t in TESELAS:
        zp = os.path.join(D30, "TDM1_EDEM_10_%s_V01_C.zip" % t)
        dest = os.path.join(D30, "TDM1_EDEM_10_%s_V01_C" % t)
        if os.path.isdir(dest) or not os.path.exists(zp):
            continue
        try:
            with zipfile.ZipFile(zp) as z:
                if z.testzip() is not None:
                    print("zip danado, se deja:", zp)
                    continue
                z.extractall(D30)
                print("descomprimido:", zp)
        except zipfile.BadZipFile:
            print("zip incompleto (se esta bajando?):", zp)


def codigo_sin_editar(fs_edm, fs_hem, fs_xml):
    """Codigo de la EDM que significa 'non edited'. No se supone: el XML de cada tesela dice que el HEM es
    invalido donde el EDEM esta editado, asi que el codigo sin editar es aquel cuyos pixeles tienen HEM valido.
    Se contrasta ademas la frecuencia de cada codigo con el porcentaje que el XML da para cada tipo de edicion."""
    import re
    filas = []
    for fe, fh, fx in zip(fs_edm, fs_hem, fs_xml):
        with rasterio.open(fe) as a, rasterio.open(fh) as b:
            e = a.read(1); h = b.read(1)
        xml = open(fx, encoding="utf8").read()
        stats = dict(re.findall(r'<editingSourceStats name="([^"]+)" units="percentage">([0-9.]+)<', xml))
        for c in np.unique(e):
            s = e == c
            filas.append(dict(tesela=os.path.basename(fe)[13:20], codigo=int(c), px=int(s.sum()),
                              pct=100 * s.mean(), hem_valido_pct=100 * (h[s] > -32766).mean(),
                              xml_ediciones=";".join("%s=%s" % (k, v) for k, v in stats.items() if float(v) > 0)))
    T = pd.DataFrame(filas)
    T.to_csv(os.path.join(RES, "P16_EDM_codigos.csv"), index=False)
    print(T.to_string(index=False))
    g = T.groupby("codigo").apply(lambda d: pd.Series(dict(px=d["px"].sum(),
                                                           hem=np.average(d["hem_valido_pct"], weights=d["px"]))))
    sin = g[g["hem"] > 99.0].index.tolist()
    if len(sin) != 1:
        sys.exit("No se pudo identificar un unico codigo 'non edited' en la EDM: %s" % g.to_string())
    print("Codigo EDM sin editar (HEM valido en %.2f %% de sus pixeles): %d" % (g.loc[sin[0], "hem"], sin[0]))
    return sin[0]


if ETAPA == "preparar":
    if RAMA == "tdx90":
        dirs = [os.path.join(D90, "TDM1_DEM__30_%s_V01_C" % t) for t in TESELAS]
        fd = [os.path.join(d, "DEM", os.path.basename(d)[:20] + "_DEM.tif") for d in dirs]
        fh = [os.path.join(d, "AUXFILES", os.path.basename(d)[:20] + "_HEM.tif") for d in dirs]
    else:
        descomprimir_edem()
        dirs = [os.path.join(D30, "TDM1_EDEM_10_%s_V01_C" % t) for t in TESELAS]
        fd = [os.path.join(d, "EDEM", os.path.basename(d)[:20] + "_EDEM_W84.tif") for d in dirs]
        fh = [os.path.join(d, "EDEM_AUXFILES", os.path.basename(d)[:20] + "_HEM.tif") for d in dirs]
        fm = [os.path.join(d, "EDEM_AUXFILES", os.path.basename(d)[:20] + "_EDM.tif") for d in dirs]
    falta = [f for f in fd + fh if not os.path.exists(f)]
    if falta:
        sys.exit("Faltan teselas de %s (descarga incompleta?):\n  %s" % (RAMA, "\n  ".join(falta)))
    dem, t, crs = mosaico(fd)
    hem, _, _ = mosaico(fh)
    con_dato = dem > INVALIDO + 1
    fiable = con_dato & (hem > INVALIDO + 1) & (hem < HEM_MAX)
    filas = [dict(nivel="teselas", paso="con valor", pct=100 * con_dato.mean()),
             dict(nivel="teselas", paso="con valor y HEM < 0,5 m", pct=100 * fiable.mean())]
    if RAMA == "tdx30":
        edm, _, _ = mosaico(fm, nodata=None)
        fx = [os.path.join(d, os.path.basename(d)[:20] + ".xml") for d in dirs]
        sin_editar = edm == codigo_sin_editar(fm, fh, fx)
        fiable &= sin_editar
        filas.append(dict(nivel="teselas", paso="sin editar (EDM)", pct=100 * sin_editar.mean()))
        filas.append(dict(nivel="teselas", paso="con valor, sin editar y HEM < 0,5 m", pct=100 * fiable.mean()))
        escribir("00_TDX_EDM.tif", a_grilla(edm.astype("float32"), t, crs, Resampling.nearest), "float32")
    # a la grilla regional: el modelo por bilineal, como M03 con el GDEM; un pixel tiene valor (o es fiable)
    # solo si todas las celdas que entran a su interpolacion lo tienen (o lo son)
    z = a_grilla(np.where(con_dato, dem, np.nan), t, crs, Resampling.bilinear)
    c30 = a_grilla(con_dato.astype("float32"), t, crs, Resampling.bilinear) >= 0.999
    f30 = a_grilla(fiable.astype("float32"), t, crs, Resampling.bilinear) >= 0.999
    h30 = a_grilla(np.where(hem > INVALIDO + 1, hem, np.nan), t, crs, Resampling.bilinear)
    with rasterio.open(F_N96) as g:
        n96 = a_grilla(g.read(1), g.transform, g.crs, Resampling.bilinear)
    z = np.where(c30, z - n96, np.nan)
    if FILTRO_EN_HIELO:
        z = np.where(f30, z, np.nan)
    escribir("00_TDX_EGM96.tif", z)
    escribir("00_TDX_fiable.tif", (f30 & np.isfinite(z)).astype("uint8"), "uint8", 255)
    escribir("00_TDX_HEM.tif", h30)
    with rasterio.open(os.path.join(GDEM, "01_GDEM.tif")) as r:
        gd = r.read(1)
    d = z - gd
    s = np.isfinite(d) & f30
    print("%s menos GDEM, sobre los pixeles fiables: mediana %+.2f m, NMAD %.2f m" % (
        RAMA, np.median(d[s]), 1.4826 * np.median(np.abs(d[s] - np.median(d[s])))))
    filas.append(dict(nivel="grilla 30 m", paso="con valor", pct=100 * np.mean(c30[np.isfinite(gd)])))
    filas.append(dict(nivel="grilla 30 m", paso="fiable", pct=100 * np.mean(f30[np.isfinite(gd)])))
    filas.append(dict(nivel="grilla 30 m", paso="mediana TDX - GDEM (m) en fiables", pct=float(np.median(d[s]))))
    pd.DataFrame(filas).to_csv(os.path.join(RES, "P16_preparar_%s.csv" % RAMA), index=False)
    print(pd.DataFrame(filas).to_string(index=False))
    print("Listo: %s" % OUT)
    sys.exit(0)

# --- resumen: fracciones y roca por unidad, despues de P16b -----------------------------------------------
import geopandas as gpd
from rasterio.features import rasterize
from scipy import ndimage


def leer(p):
    with rasterio.open(p) as r:
        return r.read(1)


fiable = leer(os.path.join(OUT, "00_TDX_fiable.tif")) == 1
z = leer(os.path.join(OUT, "01_GDEM.tif"))
m_t = leer(os.path.join(OUT, "03_MascaraEstable.tif")) == 1
m_g = leer(os.path.join(GDEM, "03_MascaraEstable.tif")) == 1
gd = leer(os.path.join(GDEM, "01_GDEM.tif"))
T = pd.read_csv(F_TASA)
maipo = T["en_maipo"].isin([True, "TRUE", "True"])
U = gpd.read_file(F_INV, layer="union")
U = U[U["uid"].isin(set(T.loc[(T["conjunto"] == "union") & maipo, "gid"]))].reset_index(drop=True)
R6 = gpd.read_file(F_INV, layer="rgi60")
R6 = R6[R6["RGIId"].isin(set(T.loc[(T["conjunto"] == "rgi60") & maipo, "gid"]))].reset_index(drop=True)
ras = lambda geoms: rasterize([(g, k + 1) for k, g in enumerate(geoms)], out_shape=SH, transform=TR, fill=0,
                              dtype="int32")
iu = ras(U.geometry.values); i6 = ras(R6.geometry.values)
hielo = iu > 0
d_hielo = ndimage.distance_transform_edt(~hielo) * 30.0
cerca = d_hielo <= 2000
filas = []
for nombre, sel in (("rectangulo", np.isfinite(gd)), ("roca de la mascara del GDEM", m_g),
                    ("roca de la mascara del GDEM a < 2 km del hielo de la cuenca", m_g & cerca),
                    ("hielo de la cuenca (union)", hielo)):
    filas.append(dict(dominio=nombre, px=int(sel.sum()), pct_con_valor=100 * np.isfinite(z[sel]).mean(),
                      pct_fiable=100 * fiable[sel].mean()))
filas.append(dict(dominio="mascara estable %s (km2)" % RAMA, px=int(m_t.sum()), pct_con_valor=m_t.sum() * 900 / 1e6))
filas.append(dict(dominio="mascara estable GDEM (km2)", px=int(m_g.sum()), pct_con_valor=m_g.sum() * 900 / 1e6))
filas.append(dict(dominio="mascaras %s y GDEM: Jaccard (%%)" % RAMA, px=int((m_t & m_g).sum()),
                  pct_con_valor=100 * (m_t & m_g).sum() / (m_t | m_g).sum()))
filas.append(dict(dominio="roca a < 2 km del hielo de la cuenca, %s (km2)" % RAMA, px=int((m_t & cerca).sum()),
                  pct_con_valor=(m_t & cerca).sum() * 900 / 1e6))
filas.append(dict(dominio="roca a < 2 km del hielo de la cuenca, GDEM (km2)", px=int((m_g & cerca).sum()),
                  pct_con_valor=(m_g & cerca).sum() * 900 / 1e6))
# hielo: cuantos poligonos del RGI 6.0 de la cuenca tendrian menos del 90 % de sus pixeles con valor fiable
# (lo que perderian todos si el filtro se aplicara tambien al modelo, P16_FILTRO_EN_HIELO=1)
n6 = np.bincount(i6.ravel(), minlength=len(R6) + 1)[1:]
f6 = np.bincount(i6.ravel(), weights=fiable.ravel(), minlength=len(R6) + 1)[1:]
v6 = np.bincount(i6.ravel(), weights=np.isfinite(z).ravel(), minlength=len(R6) + 1)[1:]
ok = n6 > 0
a6 = R6["Area"].values if "Area" in R6 else n6 * 900 / 1e6
filas.append(dict(dominio="RGI 6.0 de la cuenca con < 90 % de pixeles fiables (poligonos / % del area)",
                  px=int((f6[ok] / n6[ok] < 0.9).sum()), pct_con_valor=100 * a6[ok][f6[ok] / n6[ok] < 0.9].sum() / a6[ok].sum()))
filas.append(dict(dominio="RGI 6.0 de la cuenca con < 90 % de pixeles con valor (poligonos / % del area)",
                  px=int((v6[ok] / n6[ok] < 0.9).sum()), pct_con_valor=100 * a6[ok][v6[ok] / n6[ok] < 0.9].sum() / a6[ok].sum()))
F = pd.DataFrame(filas)
F.to_csv(os.path.join(RES, "P16_fracciones_%s.csv" % RAMA), index=False)
print(F.to_string(index=False))
# roca por unidad: pixeles de la mascara a <= 2 km y <= 10 km de cada unidad de la cuenca, con las dos mascaras
filas = []
for k, u in U.iterrows():
    x0, y0, x1, y1 = u.geometry.bounds
    c0 = max(int((x0 - 10000 - TR.c) / 30), 0); c1 = min(int((x1 + 10000 - TR.c) / 30) + 1, SH[1])
    f0 = max(int((TR.f - y1 - 10000) / 30), 0); f1 = min(int((TR.f - y0 + 10000) / 30) + 1, SH[0])
    mg = iu[f0:f1, c0:c1] == k + 1
    if not mg.any():
        continue
    dd = ndimage.distance_transform_edt(~mg) * 30.0
    fila = dict(uid=u["uid"], area_km2=u["area_utm_km2"])
    for nm, m in (("gdem", m_g), (RAMA, m_t)):
        mm = m[f0:f1, c0:c1]
        fila["roca2km_" + nm] = int((mm & (dd <= 2000)).sum())
        fila["roca10km_" + nm] = int((mm & (dd <= 10000)).sum())
    filas.append(fila)
P = pd.DataFrame(filas)
P.to_csv(os.path.join(RES, "P16_roca_por_unidad_%s.csv" % RAMA), index=False)
for nm in ("gdem", RAMA):
    print("%-6s unidades con < 3.000 px de roca a 2 km: %d de %d (%.1f km2); con < 1.000 px a 10 km: %d" % (
        nm, (P["roca2km_" + nm] < 3000).sum(), len(P), P.loc[P["roca2km_" + nm] < 3000, "area_km2"].sum(),
        (P["roca10km_" + nm] < 1000).sum()))
