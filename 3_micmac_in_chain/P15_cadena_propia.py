# -*- coding: utf-8 -*-
"""
P15. Los modelos de MicMac pasados por la cadena (Cadena_RM): alineacion por glaciar y medicion.

No copia ni modifica la cadena: importa comun_rm.py, le cambia solo la entrada y la salida, y corre
M05 y M05c con runpy. Lo que cambia:
  - cada franja de MicMac terminada hace de escena (resultados/P14_franjas_usadas.csv);
  - se lee el <franja>_Z.tif de MicMac y se le resta la ondulacion EGM96 (MicMac entrega alturas
    elipsoidales y la cadena trabaja sobre EGM96, como el GDEM);
  - a las trece variantes de la cadena se suma CPD;
  - la base (GDEM o TanDEM-X 90 m) es la de la rama;
  - el dominio son las unidades de la cuenca alta del Maipo.
CPD tarda mucho mas que las otras variantes, asi que corre en una segunda fase: M05 con trece
variantes, M05cpd solo con CPD, y combinar junta las dos como si se hubieran probado las catorce
a la vez (CPD reemplaza a la elegida solo si su NMAD de validacion es menor).

Uso: python P15_cadena_propia.py <rama> lista|pendientes|preparar
     python P15_cadena_propia.py <rama> M05 <parte> <de>
     python P15_cadena_propia.py <rama> M05c
     python P15_cadena_propia.py <rama> M05cpd <parte> <de>
     python P15_cadena_propia.py <rama> combinar
     python P15_cadena_propia.py <rama>_cpd M05c
Sale: C:/Users/sebas/Prueba_MMASTER/cadena_propia/<rama>/ (fuera de OneDrive)
"""

import atexit
import os
import sys

# antes de importar numpy y GDAL: un hilo de algebra lineal por proceso y sin .aux.xml
for _v in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "NUMEXPR_NUM_THREADS"):
    os.environ[_v] = "1"
os.environ["GDAL_PAM_ENABLED"] = "NO"

import glob
import runpy
import shutil

import numpy as np
import pandas as pd

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
CADENA = os.path.join(RAIZ, "02_Balance_Masa_RM", "Cadena_RM")
PRUEBA = os.path.join(RAIZ, "03_DEM_MicMac")
MM = r"C:\Users\sebas\Prueba_MMASTER"
SALIDA = os.environ.get("P15_RAIZ", os.path.join(MM, "cadena_propia"))
COMUN = os.path.join(MM, "cadena_propia", "comun")
BASES = {"gdem": "MascaraEstable_RM_ASTER", "tdx90": "MascaraEstable_RM_TDX90", "tdx30": "MascaraEstable_RM_TDX30"}
BASES.update({r + "_cpd": b for r, b in list(BASES.items())})   # la combinacion con CPD usa la misma base
F_N96 = os.path.join(MM, "referencia", "ondulacion_EGM96.tif")
F_FRANJAS = os.path.join(PRUEBA, "resultados", "P14_franjas_usadas.csv")
F_CATALOGO = os.path.join(PRUEBA, "resultados", "P01_catalogo_cuenca_2026.csv")
F_INV_CUENCA = os.path.join(COMUN, "Inventarios_cuenca.gpkg")

if len(sys.argv) < 3 or sys.argv[1] not in BASES:
    sys.exit(__doc__)
RAMA, ETAPA = sys.argv[1], sys.argv[2]
DIR_RAMA = os.path.join(SALIDA, RAMA)
F_LISTA = os.path.join(DIR_RAMA, "franjas.csv")
os.makedirs(os.path.join(DIR_RAMA, "tablas"), exist_ok=True)
os.makedirs(COMUN, exist_ok=True)


def franja_a_escena(franja):
    """AST_L1A_004MMDDAAAAhhmmss -> (AAAA-MM-DD, hh:mm:ss)."""
    s = franja[len("AST_L1A_004"):]
    return "%s-%s-%s" % (s[4:8], s[0:2], s[2:4]), "%s:%s:%s" % (s[8:10], s[10:12], s[12:14])


def franjas_terminadas():
    """Franjas de enero a abril (P14) cuyo MicMac termino: tiempos.csv con la etapa postproceso y el _Z.tif."""
    fr = pd.read_csv(F_FRANJAS)
    cat = pd.read_csv(F_CATALOGO)
    nube = dict(zip(cat["granulo"], cat["nube"]))
    filas = []
    for r in fr.itertuples():
        d = os.path.join(MM, "trabajo", r.franja)
        z = os.path.join(d, "PROCESSED_FINAL", r.franja, r.franja + "_Z.tif")
        t = os.path.join(d, "tiempos.csv")
        if not (os.path.exists(t) and "postproceso" in open(t).read() and os.path.exists(z)):
            continue
        fecha, hora = franja_a_escena(r.franja)
        f = pd.Timestamp(fecha)
        ns = [nube[g] for g in str(r.granulos).split() if g in nube]
        filas.append(dict(granulo=r.franja, archivo=z, fecha=fecha, hora_utc=hora, mes=f.month,
                          dia_del_anio=f.dayofyear, n_granulos=r.n_granulos,
                          nube_media_pct=float(np.mean(ns)) if ns else np.nan, nube_max_pct=r.nube_max,
                          escena=fecha + "_" + hora.replace(":", ""),
                          anio_dec=f.year + (f.dayofyear - 0.5) / 365.25))
    return pd.DataFrame(filas).sort_values("escena").reset_index(drop=True)


if ETAPA == "lista":
    L = franjas_terminadas()
    L.to_csv(F_LISTA, index=False)
    print("Franjas terminadas: %d de %d (%s a %s)" % (len(L), len(pd.read_csv(F_FRANJAS)),
                                                       L["fecha"].min(), L["fecha"].max()))
    sys.exit(0)

if not os.path.exists(F_LISTA):
    sys.exit("Falta %s: correr antes la etapa 'lista'" % F_LISTA)

def hechas_en(carpeta):
    return {os.path.basename(f)[:-len("_unidades.csv")] for f in glob.glob(os.path.join(carpeta, "*_unidades.csv"))}


D_SIN = os.path.join(DIR_RAMA, "M05_salida")
D_CPD = os.path.join(DIR_RAMA, "M05_salida_cpd")
D_14 = os.path.join(DIR_RAMA, "M05_salida_14metodos")

if ETAPA == "pendientes":
    L = pd.read_csv(F_LISTA)
    print(int((~L["escena"].isin(hechas_en(D_SIN))).sum()))
    sys.exit(0)

if ETAPA == "pendientes_cpd":
    L = pd.read_csv(F_LISTA)
    print(int((~L["escena"].isin(hechas_en(D_CPD) | hechas_en(D_14))).sum()))
    sys.exit(0)

# --- la cadena, con lo que depende de la entrada y la salida cambiado -------------------------------------
sys.path.insert(0, CADENA)
import comun_rm as C            # noqa: E402
import geopandas as gpd         # noqa: E402
import rasterio                 # noqa: E402
import xdem                     # noqa: E402
from rasterio.enums import Resampling   # noqa: E402
from rasterio.features import rasterize  # noqa: E402
from rasterio.warp import reproject      # noqa: E402

C.DIR_BASE = os.path.join(RAIZ, "01_Datos", "Mascara_Estable", BASES[RAMA])
C.DIR_CADENA = DIR_RAMA                       # M05 escribe en DIR_CADENA/M05_salida; M05c lee de ahi
C.DIR_TAB = os.path.join(DIR_RAMA, "tablas")  # M05c escribe sus tablas aqui
for f in ("01_GDEM.tif", "03_MascaraEstable.tif", "01_Hipsometria_union.csv", "01_Hipsometria_rgi60.csv",
          "01_Hipsometria_rgi70.csv", "01_Hipsometria_dga.csv"):
    if not os.path.exists(os.path.join(C.DIR_BASE, f)):
        sys.exit("Falta la base de la rama %s: %s" % (RAMA, os.path.join(C.DIR_BASE, f)))

# inventario: la union y el RGI 7.0 enteros (mismos indices que la cadena), el RGI 6.0 solo de la cuenca
T = pd.read_csv(os.path.join(CADENA, "tablas", "M06_Tasa_por_Glaciar.csv"))
EN_MAIPO = T["en_maipo"].isin([True, "TRUE", "True"])
UID_MAIPO = set(T.loc[(T["conjunto"] == "union") & EN_MAIPO, "gid"])
RGI60_MAIPO = set(T.loc[(T["conjunto"] == "rgi60") & EN_MAIPO, "gid"])
if not os.path.exists(F_INV_CUENCA):
    tmp = F_INV_CUENCA + ".tmp.gpkg"
    if os.path.exists(tmp):
        os.remove(tmp)
    for capa in ("union", "rgi70", "rgi60"):
        g = gpd.read_file(C.F_INV, layer=capa)
        if capa == "rgi60":
            g = g[g["RGIId"].isin(RGI60_MAIPO)]
        g.to_file(tmp, layer=capa, driver="GPKG")
    os.replace(tmp, F_INV_CUENCA)
F_INV_CADENA = C.F_INV
C.F_INV = F_INV_CUENCA

_asignacion = C.asignacion_union


def asignacion_union(base, dist_max=2000.0):
    """La de la cadena (con el RGI 6.0 de la cuenca), dejando solo las unidades que hacen falta: las de la
    cuenca, las que miden algun pixel de un poligono del RGI 6.0 de la cuenca y las que miden algun pixel
    de un glaciar del inventario de la DGA (IPG 2014, el principal)."""
    asig, en_union, uids = _asignacion(base, dist_max)
    k_maipo = {k + 1 for k, u in enumerate(uids) if u in UID_MAIPO}
    K = set(k_maipo)
    for f_inv, capa in ((C.F_INV, "rgi60"), (C.F_INV_DGA, "dga")):
        r = gpd.read_file(f_inv, layer=capa)
        p = rasterize([(g, 1) for g in r.geometry.values], out_shape=base.shape, transform=base.transform,
                      fill=0, dtype="uint8", all_touched=False).astype(bool)
        K |= set(np.unique(asig[p & (asig > 0)]).tolist())
    K = np.array(sorted(K), dtype=asig.dtype)
    asig = np.where(np.isin(asig, K), asig, 0).astype(asig.dtype)
    if ETAPA == "preparar":
        pd.DataFrame(dict(uid=[uids[k - 1] for k in K], en_maipo=[uids[k - 1] in UID_MAIPO for k in K])).to_csv(
            os.path.join(COMUN, "unidades_procesadas.csv"), index=False)
    return asig, en_union, uids


C.asignacion_union = asignacion_union


def lista_escenas():
    L = pd.read_csv(F_LISTA)
    if ETAPA == "M05cpd":
        # las franjas ya corridas con los catorce metodos no se repiten
        L = L[~L["escena"].isin(hechas_en(D_14))].reset_index(drop=True)
    return L


C.lista_escenas = lista_escenas

_leer = C.leer_escena
_N96 = {}


def leer_escena(ruta, base):
    """Lector de la cadena sobre el DEM de MicMac, y de la elipsoide al geoide EGM96."""
    r = _leer(ruta, base)
    if r is None:
        return None
    arr, (f0, f1, c0, c1) = r
    if "n" not in _N96:
        n = np.full(base.shape, np.nan, dtype="float32")
        with rasterio.open(F_N96) as g:
            reproject(g.read(1).astype("float32"), n, src_transform=g.transform, src_crs=g.crs, src_nodata=np.nan,
                      dst_transform=base.transform, dst_crs=base.crs, dst_nodata=np.nan,
                      resampling=Resampling.bilinear)
        if not np.isfinite(n).all():
            raise SystemExit("La ondulacion EGM96 no cubre toda la grilla regional")
        _N96["n"] = n
    return arr - _N96["n"][f0:f1, c0:c1], (f0, f1, c0, c1)


C.leer_escena = leer_escena

_metodos = C.metodos_todos


def metodos_todos():
    m = dict(_metodos())
    if os.environ.get("P15_SIN_CPD", "") != "1":
        m["CPD"] = lambda: xdem.coreg.CPD()      # como M05b: configuracion por omision
    return m


C.metodos_todos = metodos_todos

# las variables de la cadena, fijadas para que ninguna herencia del entorno cambie el metodo (la fase 2 corre
# M05 solo con CPD y escribe en M05_salida_cpd)
if ETAPA == "M05cpd" and os.environ.get("P15_SIN_CPD", "") == "1":
    sys.exit("M05cpd necesita CPD: no definir P15_SIN_CPD")
os.environ.update(CADENA_RM_SALIDA="M05_salida_cpd" if ETAPA == "M05cpd" else "M05_salida",
                  CADENA_RM_METODO="CPD" if ETAPA == "M05cpd" else "", CADENA_RM_B_KM="2", CADENA_RM_SUFIJO="",
                  CADENA_RM_TEMPORADA="jerarquia", CADENA_RM_PX_VALIDOS="90")


def leer_tabla(f):
    try:
        return pd.read_csv(f)
    except pd.errors.EmptyDataError:
        return pd.DataFrame()


def combinar():
    """Fase 1 (trece metodos) + fase 2 (solo CPD) -> lo que daria M05 con los catorce (ver la cabecera)."""
    d_out_rama = os.path.join(SALIDA, RAMA + "_cpd")
    d_out = os.path.join(d_out_rama, "M05_salida")
    os.makedirs(d_out, exist_ok=True)
    os.makedirs(os.path.join(d_out_rama, "tablas"), exist_ok=True)
    L = pd.read_csv(F_LISTA)
    L.to_csv(os.path.join(d_out_rama, "franjas.csv"), index=False)
    asig, _e, uids = C.asignacion_union(C.Base())
    asig_dom = asig[asig > 0]                 # mismo orden que el mosaico de M05 (pos sobre el dominio)
    k_de = {u: k + 1 for k, u in enumerate(uids)}
    sufijos = ("_dh.npz", "_metodos.csv", "_error.csv", "_unidades.csv")
    pares, n14, nmez, nfalta = [], 0, 0, 0
    for esc in L["escena"]:
        fin = os.path.join(d_out, esc)
        p14, ps, pc = (os.path.join(d, esc) for d in (D_14, D_SIN, D_CPD))
        if os.path.exists(p14 + "_unidades.csv"):
            if not os.path.exists(fin + "_unidades.csv"):
                for s in sufijos:
                    shutil.copy2(p14 + s, fin + s)
            U14 = leer_tabla(p14 + "_unidades.csv")
            if "uid" in U14:
                for r in U14.itertuples():
                    pares.append(dict(escena=esc, uid=r.uid, origen="catorce metodos", metodo_final=r.metodo,
                                      gana_cpd=r.metodo == "CPD"))
            n14 += 1
            continue
        if not (os.path.exists(ps + "_unidades.csv") and os.path.exists(pc + "_unidades.csv")):
            nfalta += 1
            continue
        Us, Uc = leer_tabla(ps + "_unidades.csv"), leer_tabla(pc + "_unidades.csv")
        Ms, Mc = leer_tabla(ps + "_metodos.csv"), leer_tabla(pc + "_metodos.csv")
        Es, Ec = leer_tabla(ps + "_error.csv"), leer_tabla(pc + "_error.csv")
        mos = np.load(ps + "_dh.npz")["dh_dm"].copy()
        mos_c = np.load(pc + "_dh.npz")["dh_dm"]
        if "uid" in Us:
            cpd = Uc.set_index("uid") if "uid" in Uc else pd.DataFrame()
            filas = []
            for r in Us.to_dict("records"):
                c = cpd.loc[r["uid"]].to_dict() if r["uid"] in cpd.index else None
                v_sin = r.get("NMAD_validacion") if r.get("decision_roca") == "alineada" else np.inf
                v_cpd = c.get("NMAD_validacion") if c is not None and c.get("decision_roca") == "alineada" else np.inf
                v_sin = np.inf if pd.isna(v_sin) else v_sin
                v_cpd = np.inf if pd.isna(v_cpd) else v_cpd
                gana = bool(v_cpd < v_sin)
                pares.append(dict(escena=esc, uid=r["uid"], origen="fase 1 + CPD", metodo_sin_cpd=r.get("metodo"),
                                  NMAD_validacion_sin_cpd=v_sin, NMAD_validacion_cpd=v_cpd, gana_cpd=gana,
                                  metodo_final="CPD" if gana else r.get("metodo")))
                if gana:
                    c["uid"] = r["uid"]
                    filas.append(c)
                    m = asig_dom == k_de[r["uid"]]
                    mos[m] = mos_c[m]
                    if len(Es) and "uid" in Es:
                        Es = Es[Es["uid"] != r["uid"]]
                    if len(Ec) and "uid" in Ec:
                        Es = pd.concat([Es, Ec[Ec["uid"] == r["uid"]]], ignore_index=True)
                else:
                    filas.append(r)
            Us = pd.DataFrame(filas)[list(Us.columns)]
        np.savez_compressed(fin + "_dh.npz", dh_dm=mos)
        pd.concat([Ms, Mc], ignore_index=True).to_csv(fin + "_metodos.csv", index=False)
        Es.to_csv(fin + "_error.csv", index=False)
        Us.to_csv(fin + "_unidades.csv", index=False)
        nmez += 1
    P = pd.DataFrame(pares)
    P.to_csv(os.path.join(d_out_rama, "tablas", "P15_CPD_por_par.csv"), index=False)
    al = P[P["metodo_final"].notna()] if len(P) else P
    print("Combinacion %s: %d franjas copiadas de los catorce metodos, %d combinadas, %d sin las dos fases"
          % (RAMA, n14, nmez, nfalta))
    if len(al):
        print("CPD gana en %d de %d pares alineados (%.1f %%)"
              % (int(al["gana_cpd"].sum()), len(al), 100 * al["gana_cpd"].mean()))
    return nfalta


@atexit.register
def _memoria():
    try:
        import psutil
        mi = psutil.Process().memory_info()
        print("Memoria maxima del proceso: %.2f GB" % (getattr(mi, "peak_wset", mi.rss) / 2**30), flush=True)
    except Exception:
        pass


print("Rama %s | base %s | salida %s | inventario %s" % (RAMA, C.DIR_BASE, DIR_RAMA, C.F_INV), flush=True)
if ETAPA == "preparar":
    # una sola vez antes de las partes de M05: deja hecho el GeoPackage de la cuenca y la lista de unidades,
    # para que las partes no los escriban a la vez
    _a, _e, _u = C.asignacion_union(C.Base())
    print("Unidades procesadas: %d | pixeles del dominio: %d" % (len(np.unique(_a[_a > 0])), int((_a > 0).sum())))
elif ETAPA in ("M05", "M05cpd"):
    sys.argv = [os.path.join(CADENA, "M05_Coregistracion_Glaciar.py")] + sys.argv[3:5]
    runpy.run_path(sys.argv[0], run_name="__main__")
elif ETAPA == "combinar":
    sys.exit(1 if combinar() else 0)
elif ETAPA == "M05c":
    sys.argv = [os.path.join(CADENA, "M05c_Medicion_Primera_Fecha.py")]
    runpy.run_path(sys.argv[0], run_name="__main__")
else:
    sys.exit("Etapa desconocida: " + ETAPA)
