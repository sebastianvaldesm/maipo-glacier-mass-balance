# -*- coding: utf-8 -*-
"""
M04. Experimento de la ventana de roca para alinear.
  Alinea una muestra de pares escena-glaciar con la roca de toda la escena o con la roca a 2, 3, 5
  o 10 km del glaciar, y mide el dh que queda en un anillo de roca de control a menos de 1 km del
  glaciar, que no entra en ningun ajuste. La roca cercana comparte el error de la escena con el
  glaciar; la lejana no.
Sale: M04_salida/ (lo resume M04b)
"""

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import numpy as np
import pandas as pd

import comun_rm as C

parte = int(sys.argv[1]) if len(sys.argv) > 2 else 0
de = int(sys.argv[2]) if len(sys.argv) > 2 else 1

DIR_OUT = os.path.join(C.DIR_CADENA, "M04_salida")
os.makedirs(DIR_OUT, exist_ok=True)

ANILLO = 1000.0
BUFFERS = [2000.0, 3000.0, 5000.0, 10000.0]
MIN_ANILLO = 50
MIN_AJUSTE = 1000
MET = {"NuthKaab": C.metodos()["NuthKaab"], "NuthKaab+Deramp": C.metodos()["NuthKaab+Deramp"]}

base = C.Base()
gl = C.inventario("rgi70")
esc = C.lista_escenas()

# Mascara de todos los glaciares (los dos inventarios) y distancia al mas cercano,
# para definir la ZONA sin los anillos de control.
todos = np.zeros(base.shape, bool)
for nom in ("rgi70", "rgi60"):
    g = C.inventario(nom)
    todos |= C.mascara_poligono(g.unary_union, base, 0, base.shape[0], 0, base.shape[1])
dist_todos = C.distancia_m(todos)

idx = [i for i in range(len(esc)) if i % de == parte]
print("Escenas: %d | en esta parte: %d" % (len(esc), len(idx)), flush=True)

for i in idx:
    e = esc.iloc[i]
    sal = os.path.join(DIR_OUT, e["escena"] + ".csv")
    if os.path.exists(sal):
        continue
    t0 = time.time()
    filas = []
    r = C.leer_escena(e["archivo"], base)
    if r is None:
        pd.DataFrame([dict(escena=e["escena"], nota="fuera de la grilla")]).to_csv(sal, index=False)
        continue
    arr, (f0, f1, c0, c1) = r
    ref = base.gdem[f0:f1, c0:c1]
    est = base.estable[f0:f1, c0:c1]
    dh0 = arr - ref
    ok = np.isfinite(dh0)
    med0 = np.nanmedian(dh0[est & ok]) if (est & ok).sum() > 0 else 0.0
    robusto = est & ok & (np.abs(dh0 - med0) <= C.LIM_PIXEL)

    # --- ZONA: una alineacion por escena -------------------------------------------
    zona = {}
    inl_z = robusto & (dist_todos[f0:f1, c0:c1] > ANILLO)
    if inl_z.sum() >= MIN_AJUSTE:
        for nm, cons in MET.items():
            try:
                zona[nm] = C.alinear(ref, arr, inl_z, base, f0, c0, cons)
            except Exception as err:
                zona[nm] = None

    # --- glaciares que la escena ve --------------------------------------------------
    x0, y0 = base.transform_de(f0, c0).c, base.transform_de(f0, c0).f
    xmax, ymin = x0 + (c1 - c0) * C.RES, y0 - (f1 - f0) * C.RES
    cand = gl.cx[x0:xmax, ymin:y0]
    for _, g in cand.iterrows():
        bx = g.geometry.bounds
        v = base.ventana_px(bx[0] - BUFFERS[-1], bx[1] - BUFFERS[-1], bx[2] + BUFFERS[-1], bx[3] + BUFFERS[-1])
        if v is None:
            continue
        # interseccion con la ventana de la escena
        g0, g1 = max(v[0], f0), min(v[1], f1)
        h0, h1 = max(v[2], c0), min(v[3], c1)
        if g1 - g0 < 10 or h1 - h0 < 10:
            continue
        sl = (slice(g0 - f0, g1 - f0), slice(h0 - c0, h1 - c0))
        mg = C.mascara_poligono(g.geometry, base, g0, g1, h0, h1)
        if not (mg & ok[sl]).any():
            continue
        d = C.distancia_m(mg)
        rob = robusto[sl]
        anillo = rob & (d <= ANILLO)
        if anillo.sum() < MIN_ANILLO:
            continue
        cobert = float((mg & ok[sl]).sum() / max(mg.sum(), 1))
        comunes = dict(escena=e["escena"], fecha=e["fecha"], anio_dec=e["anio_dec"], gid=g["gid"],
                       area_km2=g["area_utm_km2"], nivel=g["nivel"], cobertura_glaciar=round(cobert, 3),
                       n_anillo=int(anillo.sum()), nube_media_pct=e["nube_media_pct"])
        # Antes de alinear
        filas.append(dict(comunes, variante="sin alinear", metodo="-", n_ajuste=0,
                          anillo_mediana=float(np.median(dh0[sl][anillo])),
                          anillo_nmad=C.nmad(dh0[sl][anillo]), sx=np.nan, sy=np.nan))
        # Zona
        for nm, res in zona.items():
            if res is None:
                continue
            a, (sx, sy, sz) = res
            dd = a[sl] - ref[sl]
            vals = dd[anillo & np.isfinite(dd)]
            if vals.size < MIN_ANILLO:
                continue
            filas.append(dict(comunes, variante="zona", metodo=nm, n_ajuste=int(inl_z.sum()),
                              anillo_mediana=float(np.median(vals)), anillo_nmad=C.nmad(vals), sx=sx, sy=sy))
        # Local B
        for B in BUFFERS:
            vb = base.ventana_px(bx[0] - B, bx[1] - B, bx[2] + B, bx[3] + B)
            b0, b1 = max(vb[0], f0), min(vb[1], f1)
            k0, k1 = max(vb[2], c0), min(vb[3], c1)
            sb = (slice(b0 - g0, b1 - g0), slice(k0 - h0, k1 - h0))      # dentro de la ventana del glaciar
            se = (slice(b0 - f0, b1 - f0), slice(k0 - c0, k1 - c0))      # dentro de la escena
            inl = rob[sb] & (d[sb] > ANILLO) & (d[sb] <= B)
            if inl.sum() < MIN_AJUSTE:
                continue
            an = anillo[sb]
            for nm, cons in MET.items():
                try:
                    a, (sx, sy, sz) = C.alinear(ref[se], arr[se], inl, base, b0, k0, cons)
                except Exception:
                    continue
                dd = a - ref[se]
                vals = dd[an & np.isfinite(dd)]
                if vals.size < MIN_ANILLO:
                    continue
                filas.append(dict(comunes, variante="local %d km" % (B / 1000), metodo=nm,
                                  n_ajuste=int(inl.sum()), anillo_mediana=float(np.median(vals)),
                                  anillo_nmad=C.nmad(vals), sx=sx, sy=sy))
    df = pd.DataFrame(filas) if filas else pd.DataFrame([dict(escena=e["escena"], nota="sin glaciares con anillo")])
    df.to_csv(sal, index=False)
    print("[%d] %s  glaciares %d  filas %d  (%.0f s)" % (i, e["escena"], df["gid"].nunique() if "gid" in df else 0,
                                                        len(df), time.time() - t0), flush=True)
print("Parte %d de %d terminada." % (parte, de), flush=True)
