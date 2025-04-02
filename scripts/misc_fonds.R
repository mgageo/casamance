# <!-- coding: utf-8 -->
#
# les différents fonds
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
# http://maps.theia-land.fr/couches-cartographiques-theia.html
# http://maps.theia-land.fr/mapcache_2020_ndvi/wmts?service=WMTS&version=1.0.0&request=GetCapabilities
#
# lecture de la nomenclature
fonds_oso_nomenclature_lire <- function(annee = '2017') {
  carp()
  dsn <- sprintf("%s/nomenclature_%s_mga.txt", osoDir, annee);
  df <- read.table(dsn,header=TRUE, sep=";", blank.lines.skip = TRUE, comment.char= "", stringsAsFactors=FALSE, quote="")
  return(invisible(df))
}
#
#
fonds_ogc_references <- function() {
  carp()
# le format n'est utilisé que pour le service wms
# https://geobretagne.fr/geoserver/ddtm35/wfs?service=wfs&version=2.0.0&request=getFeature&typeName=l_carto_cours_eau_l_035&outputFormat=shape-zip
  df <- read.table(text="fond;service;layer;url;format;style
be_rrb;WMS;reservoirs_biodiversite_SRCE;https://geobretagne.fr/geoserver/bretagneenvironnement/wms?;image/geotiff;
bv_reserves;WFS;reserves_bretagne_vivante;http://geobretagne.fr/geoserver/bretagnevivante/wfs?;image/geotiff;
cbnbrest_all35;WMS;Teledetection:all35;http://www.cbnbrest.fr/geoserver/ows?;image/geotiff;
#cbnbrest_cgtv35;clipi;ocs_cbnbrest_lire;cbnbrest_cgtv35;image/geotiff;
cbnbrest_cgtv35;clipc;cbnbrest_cgtv35;cbnbrest_cgtv35;image/geotiff;
cartocdn;WMTS;cartocdn;https://basemaps.cartocdn.com/;image/geotiff;
draaf_bocage;WMS;BOCAGE_L_2014_R53;http://geobretagne.fr/geoserver/draaf/ows?;image/geotiff;
carte0;WMS;carte;https://geobretagne.fr/geoserver/dreal_b/ows?;image/geotiff;
carte;WMS;ign:ign_scan;http://geobretagne.fr/geoserver/wms?;image/geotiff;
cd35_ens;WFS;site_espace_naturel_protege_poly_35;https://geobretagne.fr/geoserver/cd35/wfs?;image/geotiff;
draaf_bocage_v;WFS;BOCAGE_L_2014_R53;http://geobretagne.fr/geoserver/draaf/wfs?;image/geotiff;
ddtm35_cours_eau_wfs;WFS;ddtm35:l_carto_cours_eau_l_035;https://geobretagne.fr/geoserver/ddtm35/ows?SERVICE=WFS;image/geotiff;
ddtm35_cours_eau;clip;CouchesDDTM35/l_carto_cours_eau_l_035.shp;;image/geotiff;
gb_carte;WMTS;carte;http://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff;
gb_ancien;WMS;photo:satellite-ancien;https://geobretagne.fr/geoserver/wms?;image/geotiff;
gb_photo;WMS;photo:ortho-composite;https://geobretagne.fr/geoserver/wms?;image/geotiff;
gb_scan;WMS;ign:ign_scan;https://geobretagne.fr/geoserver/wms?;image/geotiff;
osm_carte;WMS;osm:map;https://osm.geobretagne.fr/gwc01/service/wms?;image/png;
gb_satellite;WMTS;satellite;https://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff;
# http://wxs.ign.fr/b62wb2j8nh1ymfuouehvvjh8/geoportail/r/wms?service=wms&request=GetCapabilities;
# http://wxs.ign.fr/b62wb2j8nh1ymfuouehvvjh8/geoportail/wmts?service=wmts&request=GetCapabilities;
# http://wxs.ign.fr/1j199p5d9r9vswce59oe5tqk/geoportail/r/wms?service=wms&request=GetCapabilities;
# http://wxs.ign.fr/1j199p5d9r9vswce59oe5tqk/geoportail/wmts?service=wmts&request=GetCapabilities;
#;
geo_contour;geopf;altimetrie;ELEVATION.CONTOUR.LINE;image/png;normal
geo_mns;geo;altimetrie;ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS;image/png;normal
geo_shadow;geopf;altimetrie;ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS.SHADOW;image/png;normal
geo_plan;geopf;cartes;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2;image/png;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2
geo_planj1;geopf;cartes;GEOGRAPHICALGRIDSYSTEMS.MAPS.BDUNI.J1;image/png;normal
geo_photos;geo;essentiels;ORTHOIMAGERY.ORTHOPHOTOS;image/png;normal
geo_photos_hr;geopf;essentiels;HR.ORTHOIMAGERY.ORTHOPHOTOS;image/jpeg;normal
geo_photos2023;geopf;ortho;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-EXPRESS.2023;image/jpeg;normal
geo_photos1950;geopf;orthohisto;ORTHOIMAGERY.ORTHOPHOTOS.1950-1965;image/jpeg;
geo_scan50;geopf;orthohisto;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN50.1950;image/jpeg;
geo_rpg2021;geo;agriculture;LANDUSE.AGRICULTURE2021;image/png;normal
geo_spot2013;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2013;image/jpeg;normal
geo_spot2014;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2014;image/jpeg;normal
geo_spot2015;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2015;image/jpeg;normal
geo_spot2016;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2016;image/jpeg;normal
geo_spot2017;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2017;image/jpeg;normal
geo_spot2018;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2018;image/jpeg;normal
geo_spot2019;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2019;image/jpeg;normal
geo_spot2020;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2020;image/jpeg;normal
geo_spot2021;geo;satellite;ORTHOIMAGERY.ORTHO-SAT.SPOT.2021;image/jpeg;normal
#ign_carte;wms;gp_gm;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.STANDARD;image/png;
ign_carte;geo;cartes;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2;image/png;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2
ign_clc2018;WMS;LANDCOVER.CLC18;http://wxs.ign.fr/corinelandcover/geoportail/r/wms?;image/geotiff;
ign_gm;wms;gp_gm;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.STANDARD;image/png;
# https://geoservices.ign.fr/services-web-issus-des-scans-ign
# https://wxs.ign.fr/1j199p5d9r9vswce59oe5tqk/geoportail/r/wms?SERVICE=WMS&REQUEST=GetCapabilities
ign_scan25;wms;gp_gm;GEOGRAPHICALGRIDSYSTEMS.MAPS;image/jpeg;
ign_gmgris;wmts;gp_gmgris;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.NIVEAUXGRIS.L93;image/png;
#ign_plan;wmts;gp_planign;GEOGRAPHICALGRIDSYSTEMS.PLANIGN;image/jpeg;
#ign_plan;wms;gp_planign;GEOGRAPHICALGRIDSYSTEMS.PLANIGN;image/jpeg;
ign_plan;geo;cartes;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2;image/png;GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2
ign_etatmajor40;wmts;gp_etatmajor40;GEOGRAPHICALGRIDSYSTEMS.ETATMAJOR40;image/jpeg;
ign_express2017;wms;gp_express2017;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-EXPRESS.2017;image/jpeg;
ign_express2018;wms;gp_express2017;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-EXPRESS.2018;image/jpeg;
ign_pac2018;wms;gp_pac2018;ORTHOIMAGERY.ORTHOPHOTOS.SOCLE-ASP.2018;image/jpeg;
ign_pac2020;wms;gp_pac2020;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-ASP_PAC2020;image/jpeg;
ign_pac2021;wms;gp_pac2021;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-ASP_PAC2021;image/jpeg;
ign_photo1950;wms;gp_photo1950;ORTHOIMAGERY.ORTHOPHOTOS.1950-1965;image/jpeg;
ign_photo2000;wms;gp_photo2000;ORTHOIMAGERY.ORTHOPHOTOS2000;image/jpeg;
ign_photo2007;wms;gp_photo2007;ORTHOIMAGERY.ORTHOPHOTOS2007;image/jpeg;
ign_photo2012;wms;gp_photo2012;ORTHOIMAGERY.ORTHOPHOTOS2012;image/jpeg;
ign_photo2016;wms;gp_photo2012;ORTHOIMAGERY.ORTHOPHOTOS2016;image/jpeg;
ign_photo2000_2005;wms;gp_photo2006;ORTHOIMAGERY.ORTHOPHOTOS2000-2005;image/jpeg;
ign_photo2006_2010;wms;gp_photo2006;ORTHOIMAGERY.ORTHOPHOTOS2006-2010;image/jpeg;
ign_photo2018;wms;gp_photo2018;ORTHOIMAGERY.ORTHOPHOTOS.ORTHO-EXPRESS.2018;image/jpeg;
ign_photo_hr;wms;gp_photo_hr;HR.ORTHOIMAGERY.ORTHOPHOTOS;image/jpeg;
#ign_photo;wms;gp_photo;HR.ORTHOIMAGERY.ORTHOPHOTOS;image/jpeg;
ign_photo;geo;essentiels;ORTHOIMAGERY.ORTHOPHOTOS;image/png;normal
ign_pentes;wmts;pentes;GEOGRAPHICALGRIDSYSTEMS.SLOPES.PAC;image/geotiff;
# geolittoral
gl_littoral_v1;WMS;ortholittorale;https://geolittoral.din.developpement-durable.gouv.fr/wxs?;image/jpeg;
gl_littoral_v2;WMS;ortholittorale_v2_rvb;https://geolittoral.din.developpement-durable.gouv.fr/wxs?;image/jpeg;
onf_pf;WFS;Parcelles_Forestieres;http://ws.carmencarto.fr/WFS/105/ONF_Forets?;image/geotiff;
nica_cesbio;WMTS;cesbio;http://ogc.nicalala.com/wmts?;image/geotiff;
osm_tous_fr;TMS;osm_tous_fr;http://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff;
osm_landuse;WMS3857;osmlanduse:osm_lulc_combined_osm4eo;https://maps.heigit.org/osmlanduse/service?;image/GeoTIFF;
rb_faune_rb_vn;WFS;rb:faune_rb_vn;https://ows.region-bretagne.fr/geoserver/rb/wfs?SERVICE=WFS;json;
rm_pvci_;WMTS;ref_fonds:pvci;https://public.sig.rennesmetropole.fr/geowebcache/service/wmts?;image/geotiff;
rm_pvci;WMS;ref_fonds:pvci;https://public.sig.rennesmetropole.fr/geoserver/ows?;image/geotiff;
rm_ortho2014;WMTS;raster:ortho2014;https://public.sig.rennesmetropole.fr/geowebcache/service/wmts?;image/geotiff;
rm_ortho2017;WMS;raster:ortho2017;https://public.sig.rennesmetropole.fr/geoserver/ows?;image/geotiff;
#rm_ortho2020;WMS;raster:ortho2020;https://public.sig.rennesmetropole.fr/geowebcache/service/wms?;image/geotiff;
rm_ortho2020;WMS;raster:ortho2020;https://public.sig.rennesmetropole.fr/geoserver/ows?;image/geotiff;
#rpg2017;BBOX;RPG2017;geoportail;image/geotiff;
rpg2017_wfs;wfs;RPG2017;RPG.2017:parcelles_graphiques;image/geotiff;
rpg2016_wfs;wfs;RPG2016;RPG.2016:parcelles_graphiques;image/geotiff;
rpg2015_wfs;wfs;RPG2015;RPG.2015:parcelles_graphiques;image/geotiff;
rpg2014_wfs;wfs;RPG2014;RPG.2014:ilots_anonymes;image/geotiff;
rpg2013_wfs;wfs;RPG2013;RPG.2013:rpg_2013;image/geotiff;
rpg2012_wfs;wfs;RPG2012;RPG.2012:rpg_2012;image/geotiff;
bocage_haie;clip;CouchesGeoBretagne/bocage_haie.json;;image/geotiff;
rpg2010;clip;CouchesRPG/RPG_1-0__SHP_LAMB93_R053-2010_2010-01-01/RPG/1_DONNEES_LIVRAISON_2010/RPG_1-0_SHP_LAMB93_R053-2010/ILOTS_ANONYMES.shp;;image/geotiff;
rpg2011;clip;CouchesRPG/RPG_1-0__SHP_LAMB93_R53-2011_2011-01-01/RPG/1_DONNEES_LIVRAISON_2011/RPG_1-0_SHP_LAMB93_R53-2011/ILOTS_ANONYMES.shp;;image/geotiff;
rpg2012;clip;CouchesRPG/RPG_1-0__SHP_LAMB93_R53-2012_2012-01-01/RPG/1_DONNEES_LIVRAISON_2012/RPG_1-0_SHP_LAMB93_R53-2012/ILOTS_ANONYMES.shp;;image/geotiff;
rpg2013;clip;CouchesRPG/RPG_1-0_SHP_LAMB93_FXX_2016-02-16/RPG/1_DONNEES_LIVRAISON_2013/RPG_1-0_SHP_LAMB93_R53-2013/ILOTS_ANONYMES.shp;;image/geotiff;
rpg2014;clip;CouchesRPG/RPG_1-0__SHP_LAMB93_FXX_2017-04-07/RPG/1_DONNEES_LIVRAISON_2014/RPG_1-0_SHP_LAMB93_R053-2014/ILOTS_ANONYMES.shp;;image/geotiff;
rpg2015;clip;CouchesRPG/RPG_2-0__SHP_LAMB93_R53-2015_2015-01-01/RPG/1_DONNEES_LIVRAISON_2015/RPG_2-0_SHP_LAMB93_R53-2015/PARCELLES_GRAPHIQUES.shp;;image/geotiff;
rpg2016;clip;CouchesRPG/RPG_2-0__SHP_LAMB93_R53-2016_2016-01-01/RPG/1_DONNEES_LIVRAISON_2016/RPG_2-0_SHP_LAMB93_R53-2016/PARCELLES_GRAPHIQUES.shp;;image/geotiff;
rpg2017;clip;CouchesRPG/RPG_2-0__SHP_LAMB93_R53-2017_2017-01-01/RPG/1_DONNEES_LIVRAISON_2017/RPG_2-0_SHP_LAMB93_R53-2017/PARCELLES_GRAPHIQUES.shp;;image/geotiff;
vegetation_wfs;wfs;vegetation;BDTOPO_BDD_WLD_WGS84G:zone_vegetation;image/geotiff;
bdtopo_hydro;wfs;xxx;BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau;image/geotiff;
gp_em40;crop;CouchesBMSM/ign/BMSM_GP_EM40_13_2154.tif;;image/geotiff;
gmses13;crop;gmses13;imgGMSES13;image/geotiff;
gmses10;crop;gmses10;imgGMSES10;image/geotiff;
cesbio;crop;cesbio;imgCESBIO;image/geotiff;
cesbio2017;crop;cesbio2017;;image/geotiff;
cesbio2018;crop;cesbio2018;;image/geotiff;
cesbio2019;crop;cesbio2019;;image/geotiff;
sc1000;crop;sc1000;imgSC1000;image/geotiff;
planign17;CROP;planign17;Rocade_GP_PLANIGN_17_2154.tif;image/geotiff;
planign16;CROP;planign16;Lezard_GP_PLANIGN_16_2154.tif;image/geotiff;
clc2018;clipc;clc2018;clc2018;geojson;
", header=TRUE, sep=";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote = "")
  return(invisible(df))
}
#
#
fonds_ogc_dl_bbox <- function(sf, dsn, f, marge = 500, size_max = 4096) {
  carp("f: %s", f)
  library(tidyverse)
  library(raster)
  fonds.df <- fonds_ogc_references()
  df <- fonds.df %>%
    dplyr::filter(fond == f) %>%
    glimpse()
  if(nrow(df) != 1 ) {
    stop('*** f:', f)
  }
  s <- df[1, 'service']
  switch(s,
    'BBOX' = fonds_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'clip' = fonds_clip(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'clipc' = fonds_clipc(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'clipi' = fonds_clipi(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'crop' = fonds_crop(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'CROP' = fonds_CROP(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
# modifier en clipi
#    'intersect' = fonds_intersect(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'TMS' = fonds_tms_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'wms' = fonds_wms_dl_bbox(sf, dsn, df[1,'url'], df[1,'url'], marge=marge, size_max=size_max),
    'geo' = fonds_geo_dl_bbox(sf, dsn, df[1,'url'], df[1,'layer'], marge=marge, size_max=size_max, style = df[1,'style']),
    'geopf' = fonds_geopf_dl_bbox(sf, dsn, df[1,'url'], df[1,'layer'], marge=marge, size_max=size_max, style = df[1,'style']),
    'WMTS' = fonds_WMTS_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'wmts' = fonds_wmts_dl_bbox(sf, dsn, df[1,'url'], marge=marge, size_max=size_max),
    'WMS' = fonds_WMS_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], df[1,'format'], marge=marge, size_max=size_max),
    'WMS3857' = fonds_WMS_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], df[1,'format'], epsg = 3857, marge=marge, size_max=size_max),
    'wfs' = fonds_wfs_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'WFS' = fonds_wfs_dl(dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    {
      carp('inconnu: %s', s)
      stop("$$$$$$$$$")
    }
  )
#  stop('***')
  carp('dsn: %s', dsn)
}
# https://wxs.ign.fr/an7nvfzojv5wa96dsga5nk8w/geoportail/wmts?layer=GEOGRAPHICALGRIDSYSTEMS.SLOPES.PAC&style=GEOGRAPHICALGRIDSYSTEMS.SLOPES.PAC&tilematrixset=PM&Service=WMTS&Request=GetTile&Version=1.0.0&Format=image%2Fpng&TileMatrix=15&TileCol=16237&TileRow=11398
fonds_dl_bbox <- function(sf, dsn, s, l, marge=500, size_max=4096) {
  carp()
  library(tidyverse)
# le format n'est utilisé que pour le service wms
  fonds.df <- read.table(text="service;layer;url;format
onf_pf;WFS;Parcelles_Forestieres;http://ws.carmencarto.fr/WFS/105/ONF_Forets?;image/geotiff
cd35_ens;WFS;site_espace_naturel_protege_poly_35;https://geobretagne.fr/geoserver/cd35/wfs?;image/geotiff
bv_reserves;WFS;reserves_bretagne_vivante;http://geobretagne.fr/geoserver/bretagnevivante/wfs?;image/geotiff
draaf_bocage_v;WFS;BOCAGE_L_2014_R53;http://geobretagne.fr/geoserver/draaf/wfs?;image/geotiff
draaf_bocage;WMS;BOCAGE_L_2014_R53;http://geobretagne.fr/geoserver/draaf/ows?;image/geotiff
carte;WMS;carte;https://geobretagne.fr/geoserver/dreal_b/ows?;image/geotiff
ign_scan;WMS;ign:ign_scan;http://geobretagne.fr/geoserver/wms?;image/geotiff
ign_photo;WMS;photo:ortho-composite;http://geobretagne.fr/geoserver/wms?;image/geotiff
osm_carte;WMS;osm:map;https://osm.geobretagne.fr/gwc01/service/wms?;image/png
rm_pvci;WMTS;ref_fonds:pvci;https://public.sig.rennesmetropole.fr/geowebcache/service/wmts?;image/geotiff
rm_ortho2014;WMTS;raster:ortho2014;https://public.sig.rennesmetropole.fr/geowebcache/service/wmts?;image/geotiff
gb_carte;WMTS;carte;http://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff
gb_statellite;WMTS;satellite;http://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff
ign_gm;wmts;gp_gm;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.STANDARD;image/png
ign_gmgris;wmts;gp_gmgris;GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.NIVEAUXGRIS.L93;image/png
ign_plan;wmts;gp_planign;GEOGRAPHICALGRIDSYSTEMS.PLANIGN.L93;image/jpeg
osm_tous_fr;TMS;osm_tous_fr;http://tile.geobretagne.fr/gwc02/service/wmts?;image/geotiff
rpg2017;BBOX;RPG2017;geoportail;image/geotiff
rpg2017_wfs;wfs;RPG2017;RPG.2017:parcelles_graphiques;image/geotiff
rpg2016_wfs;wfs;RPG2016;RPG.2016:parcelles_graphiques;image/geotiff
vegetation_wfs;wfs;vegetation;BDTOPO_BDD_WLD_WGS84G:zone_vegetation;image/geotiff
ign_pentes;wmts;pentes;GEOGRAPHICALGRIDSYSTEMS.SLOPES.PAC;image/geotiff
gmses13;crop;gmses13;imgGMSES13;image/geotiff
gmses10;crop;gmses10;imgGMSES10;image/geotiff
sc1000;crop;sc1000;imgSC1000;image/geotiff
", header=TRUE, sep = ";", blank.lines.skip = TRUE, stringsAsFactors = FALSE, quote="")
  df <- fonds.df %>%
#    glimpse() %>%
    dplyr::filter(service == s, layer ==l) %>%
      glimpse()
  s <- df[1, 'service']
  switch(s,
    'BBOX' = fonds_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'crop' = fonds_crop(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'TMS' = fonds_tms_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'WMTS' = fonds_WMTS_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'WMS' = fonds_wms_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], df[1,'format'], marge=marge, size_max=size_max),
    'wfs' = fonds_wfs_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'WFS' = fonds_wfs_dl(dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
    'wmts' = fonds_wmts_dl_bbox(sf, dsn, df[1, 'url'], df[1,'layer'], marge=marge, size_max=size_max),
  )
#  stop('***')
}
#
# génération des différents fonds (image ou vecteur) sur une bbox donnée
fonds_ogc_fonds <- function(df, nc, p, test, force_ogc = FALSE) {
  carp("p: %s", p)
  for ( i in 1:nrow(df) ) {
    e <- df[i, 'fonction']
    fond <- df[i, 'fond']
#    test <- df[i, 'test']
    param <- df[i, 'param']
    marge <- df[i, 'marge']
    size_max <- df[i, 'size_max']
    carp("fond: %s", fond)
    dsn <- sprintf(param, varDir, p, fond)
    if ( force_ogc == TRUE | ! file.exists(dsn)) {
      img <- fonds_ogc_dl_bbox(nc, dsn, fond, size_max=size_max, marge=marge)
    }
  }
  return(invisible(dsn))
#  stop('***')
}


#
# la bbox pour extraction via script perl
fonds_bbox <- function(sf, dsn, ogc_url, ogc_layer, marge=300) {
  carp()
  library(sf)
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("IKA %s %s %s %s %s",dsn,  xmin, ymax, xmax, ymin)
  carp("bbox %s", bbox)
  return(invisible(bbox))
}
fonds_extent <- function(sf, marge = 500, aligne = 0, offsetX = 0, offsetY = 0) {
  library(raster)
  e <- as.vector(st_bbox(sf))
  if ( aligne == 1 ) {
    e[1] <- floor(e[1] / marge) * marge - marge
    e[2] <- floor(e[2] / marge) * marge - marge
    e[3] <- ceiling(e[3] / marge) * marge + marge
    e[4] <- ceiling(e[4] / marge) * marge + marge
  } else {
    e[1] <- e[1] - marge
    e[2] <- e[2] - marge
    e[3] <- e[3] + marge
    e[4] <- e[4] + marge
  }
  e[1] <- e[1] - offsetX
  e[2] <- e[2] - offsetY
  e[3] <- e[3] + offsetX
  e[4] <- e[4] + offsetY
  e <- extent(e[1], e[3], e[2], e[4])
  return(invisible(e))
}
fonds_extent_sfc <- function(sf, marge = 500, aligne = 0, offsetX = 0, offsetY = 0) {
  e <- as.vector(st_bbox(sf))
  if ( aligne == 1 ) {
    e[1] <- floor(e[1]/marge) * marge - marge
    e[2] <- floor(e[2]/marge) * marge - marge
    e[3] <- ceiling(e[3]/marge) * marge + marge
    e[4] <- ceiling(e[4]/marge) * marge + marge
  } else {
    e[1] <- e[1] - marge
    e[2] <- e[2] - marge
    e[3] <- e[3] + marge
    e[4] <- e[4] + marge
  }
  e[1] <- e[1] - offsetX
  e[2] <- e[2] - offsetY
  e[3] <- e[3] + offsetX
  e[4] <- e[4] + offsetY
  sfc1 <- st_as_sfc(st_bbox(c(xmin = e[1], xmax = e[3], ymax = e[2], ymin = e[4]), crs = st_crs(2154)))
  return(invisible(sfc1))
}
fonds_extent_crop <- function(sf, img, marge=500, aligne=0, offsetX=0, offsetY=0) {
  library(raster)
  e <- as.vector(st_bbox(sf))
  if ( aligne == 1 ) {
    e[1] <- floor(e[1]/marge) * marge - marge
    e[2] <- floor(e[2]/marge) * marge - marge
    e[3] <- ceiling(e[3]/marge) * marge + marge
    e[4] <- ceiling(e[4]/marge) * marge + marge
  } else {
    e[1] <- e[1] - marge
    e[2] <- e[2] - marge
    e[3] <- e[3] + marge
    e[4] <- e[4] + marge
  }
  e[1] <- e[1] - offsetX
  e[2] <- e[2] - offsetY
  e[3] <- e[3] + offsetX
  e[4] <- e[4] + offsetY
  e <- extent(e[1], e[3], e[2], e[4])
  r <- extent(img)
  if ( e[1] < r[1] | e[2] > r[2] | e[3] < r[3] | e[4] > r[4] ) {
    carp('e: xmin %0f xmax %0f ymin %0f ymax %0f', e[1], e[2], e[3], e[4])
    carp('r: xmin %0f xmax %0f ymin %0f ymax %0f', r[1], r[2], r[3], r[4])
    stop('***')
  }
  img <- crop(img, e)
  return(invisible(img))
}
#
# le clip d'un fichier vecteur
# le fichier source est mis en cache, ce qui peut être une mauvaise idée
fonds_clip <- function(sf, dsn, ogc_url, ogc_layer, marge=300, size_max=10000) {
  carp('layer: %s', ogc_layer)
  e <- fonds_extent(sf, marge)
  file <- sprintf("%s/bvi35/%s", Drive, ogc_layer);
  if ( ! exists('fonds_vecteur.list')) {
    fonds_vecteur.list <<- list()
  }
  if ( ! exists(ogc_layer, where=fonds_vecteur.list)) {
#    fonds_vecteur.list[[ogc_layer]] <<- st_read(file, options = "ENCODING=windows-1252") %>%
    fonds_vecteur.list[[ogc_layer]] <<- st_read(file) %>%
      st_transform(2154)
  }
  nc <- fonds_vecteur.list[[ogc_layer]] %>%
    st_crop(., e) %>%
    glimpse()
  st_write(nc, dsn, driver='GeoJSON', delete_dsn=TRUE)
  return(invisible(nc))
}
#
# le clip d'un fichier vecteur en cache
fonds_clipc <- function(sf, dsn, ogc_url, ogc_layer, marge=300, size_max=10000) {
  carp('layer: %s', ogc_layer)
  e <- fonds_extent(sf, marge) %>%
    glimpse()
  nc <- fonds_cache.list[[ogc_layer]] %>%
    st_crop(., e) %>%
    glimpse()
  st_write(nc, dsn, driver='GeoJSON', delete_dsn=TRUE)
  return(invisible(nc))
}
#
# le clip (intersect) d'un fichier vecteur
fonds_clipi <- function(sf, dsn, ogc_url, ogc_layer, marge=300, size_max=10000) {
  carp('dsn: %s ogc_layer: %s', dsn, ogc_layer)
  a <- list(test=2)
  sf <- st_buffer(sf, marge) %>%
    glimpse()
  Carp("sf: %s", st_crs(sf))
  nc <- do.call(ogc_layer, a) %>%
    glimpse()
  Carp("nc: %s", st_crs(nc))
  nc <- st_transform(nc, 2154)
  nc <- st_crop(nc, sf) %>%
    glimpse()
  if (grepl('geojson$', dsn)) {
    nc <- st_transform(nc, 4326)
  }
  st_write(nc, dsn, driver='GeoJSON', delete_dsn=TRUE)
  return(invisible(nc))
}
#
# le crop d'une image
# https://cran.r-project.org/web/packages/raster/raster.pdf
# pb de datatype tiff
fonds_crop <- function(sf, dsn, ogc_url, ogc_layer, marge=300, size_max=10000) {
  library(terra)
  carp('ogc_layer: %s dsn: %s', ogc_layer, dsn)
  e <- fonds_extent(sf, marge)
  carp('e: %s', e)
  img <- fonds_cache.list[[ogc_layer]] %>%
    glimpse()
#  carp('crop1 dataType: %s', dataType(img))
#  dT <- dataType(img)
  img <- crop(img, e)
#  mga <<- img;  stop("*****")
#  carp('crop2 dataType: %s', dataType(img))
#  dataType(img) <- dT
#  carp('crop3 dataType: %s', dataType(img))
#  plotImg(img);  stop('***')
  tifoptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=2")
#  writeRaster(img, filename=dsn, format="GTiff", overwrite=TRUE, options=tifoptions)
#  writeRaster(img, filename=dsn, format = "GTiff", datatype=dT, overwrite=TRUE)
#  writeRaster(img, filename=dsn, format = "GTiff", overwrite = TRUE, options = "COMPRESS=LZW")
  terra::writeRaster(img, filename = dsn, overwrite = TRUE)
#  stop('***')
  return(invisible(img))
}

#
# le crop d'une image IGN
fonds_CROP <- function(sf, dsn, ogc_url, ogc_layer, marge=300, size_max=10000) {
  carp()
  e <- fonds_extent(sf, marge)
  if (  exists('fonds')) {
    fonds <<-list()
    f_raster <- sprintf('%s/%s', ignDir, ogc_url)
    carp('f_raster: %s', f_raster)
    carp('ogc_layer: %s', ogc_layer)
    fonds[[ogc_layer]] <<- brick(f_raster)
  }
  img <- fonds[[ogc_layer]]
#  plotRGB(img)
#  stop('***')
  img <- crop(img, e)
#  plotImg(img);  stop('***')
  tifoptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=2")
#  writeRaster(img, filename=dsn, format="GTiff", overwrite=TRUE, options=tifoptions)
  writeRaster(img, filename=dsn, format="GTiff", overwrite=TRUE)
#  stop('***')
  carp('dsn: %s', dsn)
  return(invisible(img))
}

#
#
# via le mini-driver xml de gdal
fonds_tms_dl_bbox <- function(sf, file, ogc_url, ogc_layer, format='image/geotiff', marge=300, size_max=3072) {
  carp('file: %s', file)
  library(gdalUtils)
  library(tidyverse)
  rasterFic <- 'd:/tms.tif'
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("%s,%s,%s,%s", xmin, ymin, xmax, ymax)
  largeur <- xmax-xmin
  hauteur <- ymax-ymin
  carp("lxh:%sx%s", largeur, hauteur)
  f_xml <- 'geo/tous_fr.xml'
	if (file.exists(f_xml)) file.remove(f_xml)
#  rastersource <- gdalinfo(f_xml); glimpse(rastersource)
  gdal_translate(f_xml, file, of="GTiff", outsize = c(size_max, 0), projwin=c(xmin, ymax, xmax, ymin), projwin_srs='EPSG:2154', co=c("COMPRESS=JPEG"), verbose=TRUE)
  return()
# on met dans la bonne projection
  img <- brick(rasterFic)
  img1 <- projectRaster(img, crs=CRS("+init=epsg:2154"))
  e <- fonds_extent(sf, marge)
  img2 <- crop(img1, e)
  plotRGB(img2)
  stop('***')
  writeRaster(img2, filename=file, format="GTiff", options="INTERLEAVE=BAND", overwrite=TRUE)
}
#
# le wfs de geoportail

# ogr2ogr -spat 353000 6797000 356500 6801000 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "d:/bvi35/CouchesIKA/ogc/Cmb_rpg2016.geojson" "wfs:http://wxs.ign.fr/g0w523xhdzjfgkjsem0d4965/geoportail/wfs?service=WFS" "RPG.2016:parcelles_graphiques"
# ogr2ogr -spat 346000 6761000 349500 6763500 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" Ljl_rpg2017.geojson "wfs:http://wxs.ign.fr/g0w523xhdzjfgkjsem0d4965/geoportail/wfs?service=WFS" "RPG.2016:parcelles_graphiques"
#
# "C:\Program Files\QGIS 3.10\bin\ogr2ogr.exe" -spat 366310 6810323 368521 6812346 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "d:/bvi35/CouchesIKA/ogc/Amb_rpg_2016.json" "wfs:http://marc:gauthier@wxs.ign.fr/1j199p5d9r9vswce59oe5tqk/geoportail/wfs?service=WFS" "RPG.2016:parcelles_graphiques"


fonds_wfs_dl_bbox <- function(sf, dsn, ogc_layer, ogc_url, marge=300, size_max=3072) {
  carp()
  gdal_lib()
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
  cle <-
  ogc_url <- sprintf("http://%s:%s@wxs.ign.fr/%s/geoportail/wfs", mes_options('geoportail_user'), mes_options('geoportail_password'), mes_options('geoportail_cle'))
  ogc_url <- sprintf("wfs:%s?service=WFS", ogc_url)
  carp("ogc_url: %s", ogc_url)
  carp("ogc_layer: %s", ogc_layer)
	if (file.exists(dsn)) {
    file.remove(dsn)
  }
  carp("dsn: %s", dsn)
  mga_ogr2ogr(ogc_url, dsn, ogc_layer, t_srs="EPSG:2154", spat_srs="EPSG:2154", f="GeoJSON", spat=c(xmin, ymin, xmax, ymax), verbose=TRUE )
	if (! file.exists(dsn)) {
    stop('****')
  }
}

# ogr2ogr -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "d:/bvi35/CouchesONC35/ogc/site_espace_naturel_protege_poly_35.geojson" "https://geobretagne.fr/geoserver/cd35/wfs?" "site_espace_naturel_protege_poly_35"
# ogr2ogr -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "d:/bvi35/CouchesONC35/ogc/site_espace_naturel_protege_poly_35.geojson" "https://geobretagne.fr/geoserver/cd35/wfs?"  "site_espace_naturel_protege_poly_35"
fonds_wfs_dl <- function(dsn, ogc_url, ogc_layer, marge=300, size_max=3072) {
  gdal_lib()
#  ogc_url <- sprintf("wfs:%s", ogc_url)
  carp("ogc_url: %s", ogc_url)
  carp("ogc_layer: %s", ogc_layer)
#  stop('***')
	if (file.exists(dsn)) file.remove(dsn)
  ogr2ogr(ogc_url, dsn, ogc_layer, t_srs="EPSG:2154", spat_srs="EPSG:2154", f="GeoJSON", verbose=TRUE )
}

#
# http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9026
fonds_WMS_dl_bbox <- function(sf, file, ogc_url, ogc_layer, format = "image/geotiff", epsg = "2154", marge=300, size_max=3072) {
  carp()
  bb <- fonds_emprise(sf, marge, epsg)
#  glimpse(sf);  glimpse(bb); stop("****")
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("%s,%s,%s,%s", xmin, ymin, xmax, ymax)
  largeur <- xmax-xmin
  hauteur <- ymax-ymin
  carp("lxh:%sx%s", largeur, hauteur)
  wms_max <- size_max
  if ( largeur > hauteur ) {
    ratio = wms_max / largeur;
  } else {
    ratio = wms_max / hauteur;
  }
  largeur <- as.integer(largeur * ratio)
  hauteur <- as.integer(hauteur * ratio)
  if ( largeur > wms_max ) {
    ratio <- largeur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  if ( hauteur > wms_max ) {
    ratio <- hauteur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  carp("lxh:%sx%s", largeur, hauteur)
  url <- sprintf("%sSERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&BBOX=%s&WIDTH=%s&HEIGHT=%s&SRS=EPSG:%s&LAYERS=%s&STYLES=&FORMAT=%s&DPI=96&TRANSPARENT=TRUE", ogc_url, bbox, largeur, hauteur, epsg, ogc_layer, format)
  carp("url: %s", url)
  url <- URLencode(url)
  options(timeout = 600)
  download.file(url, file, quiet = FALSE, mode = "wb")
  if ( interactive() ) {
    img <- brick(file)
    plotRGB(img)
  }
  if ( interactive() ) {
#    img <- raster(file)
#    plot(img)
  }
#  stop('***')
}
#
# pour les geoservices de l'ign
fonds_geo_dl_bbox <- function(sf, file, ogc_url, ogc_layer, format='image/geotiff', marge=300, size_max=3072, style = 'normal') {
  carp()
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("%s,%s,%s,%s", xmin, ymin, xmax, ymax)
  largeur <- xmax-xmin
  hauteur <- ymax-ymin
  carp("lxh:%sx%s", largeur, hauteur)
  wms_max <- size_max
  if ( largeur > hauteur ) {
    ratio = wms_max / largeur;
  } else {
    ratio = wms_max / hauteur;
  }
  largeur <- as.integer(largeur * ratio)
  hauteur <- as.integer(hauteur * ratio)
  if ( largeur > wms_max ) {
    ratio <- largeur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  if ( hauteur > wms_max ) {
    ratio <- hauteur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  carp("lxh:%sx%s", largeur, hauteur)
  couche_url <- sprintf("https://wxs.ign.fr/%s/geoportail/r/wms", ogc_layer)
  query <- list(
    service = "WMS",
    version = "1.3.0",
    request = "GetMap",
    BBOX = bbox,
    WIDTH = largeur,
    HEIGHT = hauteur,
    CRS = "EPSG:2154",
    LAYERS = ogc_url,
    STYLES = style,
    FORMAT = format
#    DPI = 96,
#    TRANSPARENT = "TRUE"
  )
  query <- paste(names(query), query, sep = "=", collapse = "&" )
  url <- sprintf("%s?%s", couche_url, query)
  carp("url: %s", url)
  url <- URLencode(url)
  options(timeout = 600)
  download.file(url, file, quiet = FALSE, mode = "wb")
  if ( interactive() ) {
    img <- brick(file)
    plotRGB(img)
  }
  if ( interactive() ) {
#    img <- raster(file)
#    plot(img)
  }
#  stop(file)
}
#
# pour les geoservices de l'ign
fonds_geopf_dl_bbox <- function(sf, file, ogc_url, ogc_layer, format='image/geotiff', marge=300, size_max=3072, style = 'normal') {
  carp()
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("%s,%s,%s,%s", xmin, ymin, xmax, ymax)
  largeur <- xmax-xmin
  hauteur <- ymax-ymin
  carp("lxh:%sx%s", largeur, hauteur)
  wms_max <- size_max
  if ( largeur > hauteur ) {
    ratio = wms_max / largeur;
  } else {
    ratio = wms_max / hauteur;
  }
  largeur <- as.integer(largeur * ratio)
  hauteur <- as.integer(hauteur * ratio)
  if ( largeur > wms_max ) {
    ratio <- largeur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  if ( hauteur > wms_max ) {
    ratio <- hauteur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  carp("lxh:%sx%s", largeur, hauteur)
  couche_url <- sprintf("https://data.geopf.fr/wms-r/wms")
  query <- list(
    service = "WMS",
    version = "1.3.0",
    request = "GetMap",
    BBOX = bbox,
    WIDTH = largeur,
    HEIGHT = hauteur,
    CRS = "EPSG:2154",
    LAYERS = ogc_url,
    STYLES = style,
    FORMAT = format
#    DPI = 96,
#    TRANSPARENT = "TRUE"
  )
  query <- paste(names(query), query, sep = "=", collapse = "&" )
  url <- sprintf("%s?%s", couche_url, query)
  carp("url: %s", url)
  url <- URLencode(url)
  options(timeout = 600)
  download.file(url, file, quiet = FALSE, mode = "wb")
  if ( interactive() ) {
    img <- brick(file)
    plotRGB(img)
  }
  if ( interactive() ) {
#    img <- raster(file)
#    plot(img)
  }
#  stop(file)
}
#
# http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9026
fonds_wms_dl_bbox <- function(sf, file, ogc_url, ogc_layer, format='image/geotiff', marge=300, size_max=3072) {
  carp()
  bb <- fonds_emprise(sf, marge)
#  glimpse(bb)
  xmin <- bb[1]
  ymin <- bb[2]
  xmax <- bb[3]
  ymax <- bb[4]
# http://docs.geoserver.org/latest/en/user/services/wms/reference.html
  bbox <- sprintf("%s,%s,%s,%s", xmin, ymin, xmax, ymax)
  largeur <- xmax-xmin
  hauteur <- ymax-ymin
  carp("lxh:%sx%s", largeur, hauteur)
  wms_max <- size_max
  if ( largeur > hauteur ) {
    ratio = wms_max / largeur;
  } else {
    ratio = wms_max / hauteur;
  }
  largeur <- as.integer(largeur * ratio)
  hauteur <- as.integer(hauteur * ratio)
  if ( largeur > wms_max ) {
    ratio <- largeur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  if ( hauteur > wms_max ) {
    ratio <- hauteur / wms_max
    largeur <- as.integer(largeur / ratio)
    hauteur <- as.integer(hauteur / ratio)
  }
  carp("lxh:%sx%s", largeur, hauteur)
  couche_url <- sprintf("https://%s:%s@wxs.ign.fr/%s/geoportail/r/wms?", mes_options('geoportail_user'), mes_options('geoportail_password'), mes_options('geoportail_cle'))
  url <- sprintf("%sSERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&BBOX=%s&WIDTH=%s&HEIGHT=%s&SRS=EPSG:2154&LAYERS=%s&STYLES=&FORMAT=%s&DPI=96&TRANSPARENT=TRUE", couche_url, bbox, largeur, hauteur, ogc_layer, format)
  carp("url: %s", url)
  url <- URLencode(url)
  options(timeout = 600)
  download.file(url, file, quiet = FALSE, mode = "wb")
  if ( interactive() ) {
#    img <- brick(file)
#    plotRGB(img)
  }
  if ( interactive() ) {
#    img <- raster(file)
#    plot(img)
  }
#  stop('***')
}
#
# téléchargement d'une couche en wmts
# http://blog.cleverelephant.ca/2015/02/geotiff-compression-for-dummies.html
fonds_WMTS_dl_bbox <- function(spdf, file, couche_url="http://tile.geobretagne.fr/gwc02/service/wmts?", couche_typename = "satellite", marge=500, size_max=4096) {
  carp("file: %s",file)
#  stop('****')
  gdal_lib()
  e <- fonds_emprise(spdf, marge)
  carp("emprise: %s %s %s %s", e[1], e[2], e[3], e[4])
  carp("layer: %s",couche_typename)
  dsn <- sprintf("WMTS:%sservice=wmts&request=getCapabilities,layer=%s,tilematrixset=EPSG:2154", couche_url, couche_typename)
  carp("dsn:%s", dsn)
  f_xml <- sprintf("%s/wmts.xml", cfgDir)
  if (file.exists(f_xml)) {
    file.remove(f_xml)
  }
  gdal_translate(dsn, f_xml, of = "WMTS", verbose=TRUE)
#  file <- sprintf("%s/%s_%s.tif", varDir, secteur, couche_typename)
# -projwin ulx uly lrx lry
  gdal_translate(f_xml, file, of="GTiff", outsize = c(size_max, 0), projwin=c(e[1], e[4], e[3], e[2]), co=c("COMPRESS=JPEG"), verbose=TRUE)
  carp("file: %s", file)
}
#
# téléchargement d'une couche en wmts, version Géoportail
# http://blog.cleverelephant.ca/2015/02/geotiff-compression-for-dummies.html
fonds_wmts_dl_bbox <- function(spdf, file,  couche_typename = "satellite", marge=500, size_max=3072) {
  carp()
  couche_url <- sprintf("https://%s:%s@wxs.ign.fr/%s/geoportail/wmts?", mes_options('geoportail_user'), mes_options('geoportail_password'), mes_options('geoportail_cle'))
  gdal_lib()
  e <- fonds_emprise(spdf, marge)
  carp("emprise: %s %s %s %s", e[1], e[2], e[3], e[4])
  carp("layer: %s",couche_typename)
  dsn <- sprintf("WMTS:%sservice=wmts&request=getCapabilities,layer=%s", couche_url, couche_typename)
  carp("dsn:%s", dsn)
  f_xml <- sprintf("%s/wmts.xml", cfgDir)
#  mga_gdal_translate(dsn, f_xml, of = "WMTS", verbose=TRUE)
  gdal_translate(dsn, f_xml, of = "WMTS", verbose=TRUE)
#  file <- sprintf("%s/%s_%s.tif", varDir, secteur, couche_typename)
# -projwin ulx uly lrx lry
  gdal_translate(f_xml, file, of="GTiff", outsize = c(size_max, 0), projwin=c(e[1], e[4], e[3], e[2]), co=c("COMPRESS=JPEG"), verbose=TRUE)
  stop('***')
}

#
# on arrondit l'emprise
fonds_emprise <- function(spdf, pas = 500, epsg = 2154) {
#  toto <<- spdf
  if ( epsg != 2154 ) {
    carp("epsg: %s", epsg)
    spdf <- spdf %>%
      st_buffer(pas) %>%
      st_transform(epsg)
  }
  if ( "sf" %in% class(spdf) ) {
    e <- as.vector(st_bbox(spdf))
  }
  if ( "spdf" %in% class(spdf) ) {
    e <- as.vector(bbox(spdf))
  }
  if ( "bbox" %in% class(spdf) ) {
    e <- as.vector(spdf)
  }
  e <- round(e)
  w <- e[3] - e[1]
#  carp("w: %s pas: %s e: %s %s %s %s", w, pas, e[1], e[2], e[3], e[4])
#  pas <- round(0.05*w)
  if ( 1 == 2 ) {
    e[1] <- floor(e[1]/pas) * pas - pas
    e[2] <- floor(e[2]/pas) * pas - pas
    e[3] <- ceiling(e[3]/pas) * pas + pas
    e[4] <- ceiling(e[4]/pas) * pas + pas
  } else {
    e[1] <- e[1] - pas
    e[2] <- e[2] - pas
    e[3] <- e[3] + pas
    e[4] <- e[4] + pas
  }
#  carp("w: %s pas: %s e: %s %s %s %s", w, pas, e[1], e[2], e[3], e[4])
  return(e)
}

fonds_emprise_wgs84 <- function(pas=100) {
  library(raster)
  e <- fonds_emprise()
  sp <- as(extent(e[1], e[3], e[2], e[4]), 'SpatialPolygons')
  proj4string(sp) <- CRS("+init=epsg:2154")
  sp <- spTransform(sp, CRS("+init=epsg:4326"))
  e <- as.vector(bbox(sp))
  Log(sprintf("fonds_emprise_wgs84() e: %s %s %s %s", e[1], e[2], e[3], e[4]))
  return(e)
}
fonds_commune <- function() {
  carp()
  nc <- fonds_commune_lire_sf()
  opar <- par(mar = c(0,0,0,0))
  plot(st_geometry(nc), bg='lightblue', col='white')
}
#
# lecture pour
fonds_commune_lire_sf <- function(communes=c('35'), force=FALSE) {
  if ( exists('fonds_commune.sf') & force==FALSE) {
    return(invisible(fonds_commune.sf))
  }
  library(sf)
  library(tidyverse)
  nc <- ign_ade_lire_sf('COMMUNE')

  nc <- filter(nc, INSEE_DEP %in% communes)
  nc <- st_transform(nc, 2154)
  fonds_commune.sf <<- nc
  return(invisible(nc))
}
# source("geo/scripts/ika.R");fonds_departement()
fonds_departement <- function() {
  carp()
  nc <- fonds_departement_lire_sf()
  opar <- par(mar = c(0,0,0,0))
  plot(st_geometry(nc), bg='lightblue', col='white')
}
fonds_departement_lire_sf <- function() {
  library(sf)
  library(tidyverse)
#  nc <- ign_ade_lire_sf('DEPARTEMENT')
  nc <- ign_adminexpress_lire_sf('DEPARTEMENT')
  departements <- c('35')
  nc <- filter(nc, INSEE_DEP %in% departements)
  nc <- st_transform(nc, 2154)
  return(invisible(nc))
}
#
# le téléchargement des différentes couches
fonds_ogc_ecrire_v0 <- function(nc, nom='taupe', test=1, size_max=2048, marge=150) {
  carp()
  library(sf)
  ogcDir <- sprintf("%s/ogc", varDir)
  dir.create(ogcDir, showWarnings = FALSE)
  carp("nom: %s", nom)
  dsn <- sprintf("%s/%s_osm_tous_fr.tif", ogcDir, nom)
  if ( test == 1 | ! file.exists(dsn)) {
    img <- fonds_dl_bbox(nc, dsn, 'TMS', 'osm_tous_fr', size_max=size_max, marge=marge)
  }
  dsn <- sprintf("%s/%s_satellite.tif", ogcDir, nom)
  if ( test == 1 | ! file.exists(dsn)) {
    img <- fonds_dl_bbox(nc, dsn, 'WMTS', 'satellite', size_max=size_max, marge=marge)
  }
  dsn <- sprintf("%s/%s_carte.tif", ogcDir, nom)
  if ( test == 1 | ! file.exists(dsn)) {
    img <- fonds_dl_bbox(nc, dsn, 'WMTS', 'carte', size_max=size_max, marge=marge)
  }
}
#
# le téléchargement des différentes couches
fonds_ogc_ecrire <- function(nc, nom='taupe', size_max=2048, marge=150, couches=c('carte', 'gb_photo', 'bdtopo_hydro.json'), force=FALSE) {
  carp()
  library(sf)
  ogcDir <- sprintf("%s/ogc", varDir)
  dir.create(ogcDir, showWarnings = FALSE)
  carp("nom: %s", nom)
  for (couche in couches) {
    if (grepl('\\.', couche) ) {
      fn <- couche
      couche <- gsub('\\..*$', '', couche)
    } else {
      fn <- sprintf('%s.tif', couche)
    }
    dsn <- sprintf("%s/%s_%s", ogcDir, nom, fn)
    if ( force == TRUE | ! file.exists(dsn)) {
      img <- fonds_ogc_dl_bbox(nc, dsn, couche, size_max=size_max, marge=marge)
    }
  }
  return()
  dsn <- sprintf("%s/%s_bdtopo_hydro.json", ogcDir, nom)
  if ( force == TRUE | ! file.exists(dsn)) {
    img <- fonds_ogc_dl_bbox(nc, dsn, 'bdtopo_hydro', marge=marge)
  }
  dsn <- sprintf("%s/%s_carte.tif", ogcDir, nom)
  if ( force == TRUE | ! file.exists(dsn)) {
    img <- fonds_dl_bbox(nc, dsn, 'WMTS', 'carte', size_max=size_max, marge=marge)
  }
  return()

  dsn <- sprintf("%s/%s_osm_tous_fr.tif", ogcDir, nom)
  if ( force == TRUE | ! file.exists(dsn)) {
    img <- fonds_dl_bbox(nc, dsn, 'TMS', 'osm_tous_fr', size_max=size_max, marge=marge)
  }

}
#
# la lecture des différentes couches
fonds_ogc_lire_v0 <- function(nom='taupe', couches=c('carte', 'satellite', 'osm_tous_fr'), test=1) {
  carp()
  library(raster)
  ogcDir <- sprintf("%s/ogc", varDir)
  carp("nom: %s", nom)
  if ( test == 1 | ! exists('imgs')) {
    i <- list()
    for( couche in couches ) {
      dsn <- sprintf("%s/%s_%s.tif", ogcDir, nom, couche)
      i[[couche]] <- brick(dsn)
    }
  }
  imgs <<- i
}
fonds_brick_lire <- function(dsn) {
  carp()
  library(raster)
  carp('dsn: %s', dsn)
  if( ! file.exists(dsn) ) {
    stop("******")
  }
# https://github.com/rspatial/raster/issues/59
  x <- tryCatch(
    {
      img <- brick(dsn)
      img <- reclassify(img, cbind(NA, 0))
    },
    error = function(condition){
      img <- raster(dsn)
    }
  )
  return(img)
}
fonds_ogc_lire <- function(nom='taupe', couches=c('carte', 'satellite', 'osm_tous_fr', 'gb_photo', 'gp_em40', "ign_photo_hr"), force=FALSE) {
  carp()
  library(raster)
  ogcDir <- sprintf("%s/ogc", varDir)
  carp("nom: %s", nom)
  imgs <- list()
  for( couche in couches ) {
    dsn <- sprintf("%s/%s_%s.tif", ogcDir, nom, couche)
    carp('dsn: %s', dsn)
    if( file.exists(dsn) ) {
# https://github.com/rspatial/raster/issues/59
      x <- tryCatch(
        {
          img <- brick(dsn)
          img2 <- reclassify(img, cbind(NA, 0))
        },
        error = function(condition){
          img2 <- raster(dsn)
        }
      )
      imgs[[couche]] <- img2
    }
  }
  return(imgs)
  for( couche in c('bdtopo_hydro', 'ddtm35_cours_eau') ) {
    dsn <- sprintf("%s/%s_%s.json", ogcDir, nom, couche)
    if( file.exists(dsn) ) {
# https://github.com/rspatial/raster/issues/59
      nc <- st_read(dsn)
      imgs[[couche]] <- nc
    }
  }
  return(imgs)
}
#
# ogr2ogr.exe -spat 365060 6838082 366341 6839266 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "toto.geojson" "wfs:https://marc:gauthier@wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_V3_BETA:troncon_hydrographique"
# ADMINEXPRESS_COG_2017:commune
# ogr2ogr.exe -spat 365000 6838000 367000 6841000 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:4326" "toto.geojson" "wfs:https://marc:gauthier@wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau"
#
#ogr2ogr.exe -spat 365060 6838082 366341 6839266 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "tutu.geojson" "wfs:https://wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau"

#ogr2ogr.exe -spat 365060 6838082 366341 6839266 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "toto.geojson" "wfs:https://marc:gauthier@wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau"
# cmd <- '"C:\\Program Files\\QGIS 3.6\\bin\\ogr2ogr.exe" -spat 365060 6838082 366341 6839266 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "d:/bvi35/CouchesBMSM/ogc/pontorson_echangeur_bdtopo_hydro.geojson" "wfs:https://marc:gauthier@wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau"'
# system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE,    minimized = FALSE, invisible = TRUE, timeout = 0)
#ogr2ogr.exe -spat 330000 6876000 390000 6820000 -f "GeoJSON" -spat_srs "EPSG:2154" -t_srs "EPSG:2154" "toto.geojson" "wfs:https://marc:gauthier@wxs.ign.fr/phrttcyaqyfx21803lvk6c0p/geoportail/wfs?service=WFS" "BDTOPO_BDD_WLD_WGS84G:troncon_cours_eau"
#
#
## la mise en cache d'une couche ogc
#
#
fonds_cache_ajout <- function(file, ogc_layer, type = 'brick', force = FALSE) {
  library(terra)
  library(sf)
  carp('file: %s', file)
  if ( ! exists('fonds_cache.list')) {
    fonds_cache.list <<- list()
    fonds_cache_type.list <<- list()
  }
  if ( exists(ogc_layer, where = fonds_cache.list) & force == FALSE) {
    return()
  }
  fonds_cache_type.list[[ogc_layer]] <- type
  switch(type,
    'brick' = {
      fonds_cache.list[[ogc_layer]] <<- brick(file)
    },
    'raster' = {
      fonds_cache.list[[ogc_layer]] <<- terra::rast(file)
    },
    'json' = {
      fonds_cache.list[[ogc_layer]] <<- st_read(file, stringsAsFactors = FALSE) %>%
      st_transform(2154) %>%
      glimpse()
    },
    'gpkg' = {
      fonds_cache.list[[ogc_layer]] <<- st_read(file, stringsAsFactors = FALSE) %>%
      st_transform(2154) %>%
      glimpse()
    },
    'rds' = {
      nc <- readRDS(file) %>%
        st_transform(2154) %>%
        glimpse()
      carp("st_make_valid")
      nc <- st_make_valid(nc)
      fonds_cache.list[[ogc_layer]] <<- nc
    },
    {carp("type:%s", type); stop('***') }
  )
}
# lecture des fonds de carte en format tiff
fonds_images_lire <- function(force=FALSE) {
  library(raster)
  carp()
  rasterFic <- sprintf("%s/SC1000/SC1000_0050_7130_L93_E100.jp2", ignDir)
  fonds_cache_ajout(rasterFic, 'sc1000', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_10_2154.tif", varDir)
#  fonds_cache_ajout(rasterFic, 'GMSES10', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_13_2154.tif", varDir)
#  fonds_cache_ajout(rasterFic, 'GMSES13', force=force)
  rasterFic <-  sprintf("%s/OCS_2018_CESBIO.tif", osoDir);
  fonds_cache_ajout(rasterFic, 'cesbio2018', type='raster', force=force)
  rasterFic <-  sprintf("%s/OCS_2017_CESBIO.tif", osoDir);
  fonds_cache_ajout(rasterFic, 'cesbio2017', type='raster', force=force)
  dsn <- sprintf('%s/CGTV_35.shp', cbnbrestDir)
#  fonds_cache_ajout(dsn, 'cbnbrest_cgtv35', type='json', force=force)
  dsn <- sprintf('%s/ocs_cbnbrest.Rds', cbnbrestDir)
#  fonds_cache_ajout(dsn, 'cbnbrest_cgtv35', type='rds', force=force)
  dsn <- sprintf('%s/ocs_cbnbrest.tif', cbnbrestDir)
#  fonds_cache_ajout(dsn, 'cbnbrest_all35', type='raster', force=force)
  dsn <- sprintf("%s/clc2018_35.tif", copernicusDir)
  fonds_cache_ajout(dsn, 'clc2018', type='raster', force=force)
}
#
# lecture des fonds de carte en format tiff
fonds_images_lire_carrieres <- function(force=FALSE) {
  library(raster)
  carp()
  rasterFic <- sprintf("%s/SC1000/SC1000_0050_7130_L93_E100.jp2", ignDir)
  fonds_cache_ajout(rasterFic, 'sc1000', type='raster', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_10_2154.tif", ignDir)
  fonds_cache_ajout(rasterFic, 'gmses10', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_13_2154.tif", ignDir)
  fonds_cache_ajout(rasterFic, 'gmses13', type='raster', force=force)
}
#
# lecture des fonds de carte en format tiff
fonds_images_lire_ika <- function(force=FALSE) {
  library(raster)
  carp()
  rasterFic <- sprintf("%s/SC1000/SC1000_0050_7130_L93_E100.jp2", ignDir)
  fonds_cache_ajout(rasterFic, 'sc1000', type='raster', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_10_2154.tif", varDir)
  fonds_cache_ajout(rasterFic, 'gmses10', force=force)
  rasterFic <- sprintf("%s/couches/35_GP_GMSES_13_2154.tif", varDir)
  fonds_cache_ajout(rasterFic, 'gmses13', type='raster', force=force)
  rasterFic <-  sprintf("%s/OCS_2018_CESBIO.tif", osoDir);
  fonds_cache_ajout(rasterFic, 'cesbio2018', type='raster', force=TRUE)
  rasterFic <-  sprintf("%s/OCS_2017_CESBIO.tif", osoDir);
  fonds_cache_ajout(rasterFic, 'cesbio', type='raster', force=TRUE)
  fonds_cache_ajout(rasterFic, 'cesbio2017', type='raster', force=TRUE)
  dsn <- sprintf('%s/ocs_cbnbrest.tif', cbnbrestDir)
  dsn <- sprintf('%s/ocs_cbnbrest.Rds', cbnbrestDir)
#  fonds_cache_ajout(dsn, 'cbnbrest_cgtv35', type='rds', force=force)
}