# MODIS NDVI change after 2013 (2013-2021 mean minus 2003-2012 mean) from three MODIS
# products, to test whether the MacroSheds MODIS series (Terra, Collection 6) rises
# relative to Landsat because of the MODIS product rather than the land surface.
#   mod_c6:  MODIS/006/MOD13Q1 (Terra, Collection 6; what MacroSheds used)
#   mod_c61: MODIS/061/MOD13Q1 (Terra, Collection 6.1)
#   myd_c61: MODIS/061/MYD13Q1 (Aqua, Collection 6.1; Aqua starts mid-2002, hence 2003)
# Composites are kept where SummaryQA == 0 (as in MacroSheds) and are among each site's
# growing-season composites (start days listed in `doys`, from 11_modis_divergence.R).
#
# Outputs (data_working/greenness/modis_check):
#   site_shift.csv: watershed-weighted mean change per site and product
#   cell_shift.csv: per-MODIS-cell change around the four mapped watersheds
#
# Run with: ~/anaconda3/envs/leohs_env/bin/python src/greenness/11a_modis_gee.py

import json
import os
import warnings

import ee
import pandas as pd

warnings.filterwarnings('ignore', category=DeprecationWarning)
ee.Initialize(project='macrosheds-293818')

d = os.path.join('data_working', 'greenness', 'modis_check')
products = {'mod_c6': 'MODIS/006/MOD13Q1',
            'mod_c61': 'MODIS/061/MOD13Q1',
            'myd_c61': 'MODIS/061/MYD13Q1'}
periods = {'pre': ('2003-01-01', '2013-01-01'), 'post': ('2013-01-01', '2022-01-01')}
proj = ee.ImageCollection(products['mod_c61']).first().projection()


def change_image(coll_id, doys):
    ic = (ee.ImageCollection(coll_id)
          .map(lambda i: i.set('doy', ee.Date(i.get('system:time_start')).getRelative('day', 'year').add(1)))
          .filter(ee.Filter.inList('doy', doys)))
    ndvi = lambda i: i.select('NDVI').updateMask(i.select('SummaryQA').eq(0)).multiply(0.0001)
    pre = ic.filterDate(*periods['pre']).map(ndvi).mean().rename('pre')
    post = ic.filterDate(*periods['post']).map(ndvi).mean().rename('post')
    return pre.addBands(post).setDefaultProjection(proj)


def run(fc_path, reducer, id_cols, out_csv):
    gj = json.load(open(fc_path))
    rows = []
    windows = sorted({f['properties']['doys'] for f in gj['features']})
    for w in windows:
        doys = [int(x) for x in w.split(',')]
        sub = [f for f in gj['features'] if f['properties']['doys'] == w]
        fc = ee.FeatureCollection([ee.Feature(ee.Geometry(f['geometry'], **({} if f['geometry']['type'] == 'Point' else {'geodesic': False})),
                                              {k: f['properties'][k] for k in id_cols}) for f in sub])
        for name, cid in products.items():
            # Aqua composites start 8 days after Terra's (day 9, 25, ...)
            dd = [x + 8 for x in doys] if name.startswith('myd') else doys
            res = change_image(cid, dd).reduceRegions(collection=fc, reducer=reducer,
                                                        scale=proj.nominalScale(), crs=proj)
            try:
                feats = res.getInfo()['features']
            except ee.EEException as e:
                print(f'  {name}, {len(doys)} composites, {[f["properties"]["site_code"] for f in sub]}: {e}')
                continue
            for ft in feats:
                p = ft['properties']
                rows.append({**{k: p[k] for k in id_cols}, 'product': name,
                             'pre': p.get('pre'), 'post': p.get('post')})
        print(f'{len(doys)} composites: {len(sub)} features done', flush=True)
    out = pd.DataFrame(rows)
    out.to_csv(os.path.join(d, out_csv), index=False)
    print(out.assign(chg=out.post - out.pre).groupby('product').chg.median())


run(os.path.join(d, 'watersheds.geojson'), ee.Reducer.mean(), ['site_code'], 'site_shift.csv')
run(os.path.join(d, 'map_cells.geojson'), ee.Reducer.first(), ['site_code', 'cell'], 'cell_shift.csv')
