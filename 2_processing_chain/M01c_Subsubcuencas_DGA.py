# -*- coding: utf-8 -*-
"""
M01c. Poligonos de las subsubcuencas BNA de la DGA (cuencas 057, 054 y 060), para agrupar y
dibujar los resultados por subcuenca.
Sale: 01_Datos/Brutos/Cuencas_DGA/SubSubCuencas_BNA_057_054_060.geojson
"""

import json
import os
import urllib.parse
import urllib.request

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
DIR_OUT = os.path.join(RAIZ, "01_Datos/Brutos", "Cuencas_DGA")
F_OUT = os.path.join(DIR_OUT, "SubSubCuencas_BNA_057_054_060.geojson")
URL = ("https://services3.arcgis.com/aSoEm9TBK2shtWjP/arcgis/rest/services/"
       + urllib.parse.quote("Delimitación_de_SubSubCuencas_BNA") + "/FeatureServer/0/query")

os.makedirs(DIR_OUT, exist_ok=True)
if os.path.exists(F_OUT):
    print("Ya existe:", F_OUT)
else:
    feats = []
    for cod in ("057", "054", "060"):
        q = urllib.parse.urlencode(dict(where="COD_CUEN='%s'" % cod, outFields="*", outSR=4326,
                                        f="geojson", returnGeometry="true"))
        with urllib.request.urlopen(URL + "?" + q, timeout=300) as r:
            d = json.load(r)
        n = len(d.get("features", []))
        print("Cuenca %s: %d subsubcuencas" % (cod, n))
        if d.get("properties", {}).get("exceededTransferLimit"):
            raise SystemExit("La consulta excedio el limite de registros")
        feats += d["features"]
    with open(F_OUT, "w", encoding="utf-8") as f:
        json.dump(dict(type="FeatureCollection", features=feats), f)
    print("Guardado:", F_OUT, "(%d poligonos)" % len(feats))
