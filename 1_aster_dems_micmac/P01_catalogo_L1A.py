# P01. Catalogo CMR de los AST_L1A V004 de dia sobre un rectangulo, hasta fecha_fin.
# Uso: python P01_catalogo_L1A.py oeste sur este norte salida.csv [producto] [fecha_fin]
# producto: AST_L1A (por omision) o AST14DEM.
import csv
import json
import sys
import urllib.request

w, s, e, n = [float(x) for x in sys.argv[1:5]]
salida = sys.argv[5]
prod = sys.argv[6] if len(sys.argv) > 6 else "AST_L1A"
fin = sys.argv[7] if len(sys.argv) > 7 else "2020-01-01"
ext = ".hdf" if prod == "AST_L1A" else ".tif"
url0 = (f"https://cmr.earthdata.nasa.gov/search/granules.json?short_name={prod}&version=004"
        f"&bounding_box={w},{s},{e},{n}&temporal=2000-01-01T00:00:00Z,{fin}T00:00:00Z"
        "&day_night_flag=day&page_size=2000&sort_key=start_date")
filas, pagina = [], 1
while True:
    with urllib.request.urlopen(url0 + f"&page_num={pagina}") as r:
        ent = json.load(r)["feed"]["entry"]
    for g in ent:
        hdf = [l["href"] for l in g.get("links", []) if l["href"].endswith(ext) and l["href"].startswith("https")]
        poly = g.get("polygons", [[""]])[0][0]
        filas.append({"granulo": g["producer_granule_id"], "fecha": g["time_start"][:19], "nube": g.get("cloud_cover", ""),
                      "mb": g.get("granule_size", ""), "url": hdf[0] if hdf else "", "poligono": poly})
    if len(ent) < 2000:
        break
    pagina += 1
with open(salida, "w", newline="") as f:
    wr = csv.DictWriter(f, fieldnames=list(filas[0].keys()))
    wr.writeheader()
    wr.writerows(filas)
print(len(filas), "granulos de dia")
