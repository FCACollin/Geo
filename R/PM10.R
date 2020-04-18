#==============================================================================#
#
# title:    "R code"
# subtitle: "Map of PM10 concentration in Poland"
# encoding: "utf-8"
# author:   "Francois C A COLLIN, Ph.D."
# date:     "2020-03-30"
# Note:     "vim-user friendly script"
# keywords: GIOS, API, Raster, PM10, leaflet
# 
#==============================================================================#

## @knitr settings -------------------------------------------------------------

library(httr);
library(jsonlite);
library(rgdal);
library(raster);
library(gstat);
library(leaflet);
library(htmlwidgets);

dir_wd <- file.path(".");
setwd(dir_wd);
paths <- list(
  f01 = file.path(
    'extdep', 'gisco', 'ref-nuts-2016-60m', 'NUTS_RG_60M_2016_3035'
    ),
  f02 = file.path(getwd(), "docs", "html", "html_widget", "PM10_map.html"),
  f03 = file.path("data", "station.rda"),
  f04 = file.path("data", "sensor.rda"),
  f05 = file.path("data", "measure.rda"),
  f06 = file.path("data", "shape.rda")
);

write_ref <- c(TRUE, FALSE)[2];
use_ref   <- c(TRUE, FALSE)[1]

## @knitr data_station ---------------------------------------------------------

path     <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll";
request  <- httr::GET(url = path);
response <- httr::content(request, as = "text", encoding = "UTF-8");
station  <- jsonlite::fromJSON(response, flatten = TRUE);

head(station)


## @knitr retrieve_data_station ------------------------------------------------

if(write_ref) {
  save(station, file = paths$f03)
} else if(use_ref){
  load(paths$f03, verbose = TRUE);
} else {}


## @knitr table_station --------------------------------------------------------

# knitr::kable( x = head(station));
DT::datatable(
  data = subset(
    station,
    select = -c(
      addressStreet, city.id, city.commune.communeName,
      city.commune.districtName
    )
    ),
  rownames = FALSE
  );


## @knitr data_sensor ----------------------------------------------------------

sensor <- lapply(
  X   = station$id, 
  FUN = function(x){

    y <- paste0("http://api.gios.gov.pl/pjp-api/rest/station/sensors/", x);
    y <- httr::GET(url = y);
    y <- httr::content(y, as = "text", encoding = "UTF-8");
    y <- jsonlite::fromJSON(y, flatten = TRUE);

    return(y);
  }
  );

sensor <- do.call(sensor, what = rbind);

head(sensor);


## @knitr retrieve_data_sensor -------------------------------------------------

if(write_ref) {
  save(sensor, file = paths$f04)
} else if(use_ref) {
  load(paths$f04, verbose = TRUE);
} else {}


## @knitr table_sensor ---------------------------------------------------------

DT::datatable(data = sensor, rownames = FALSE);


## @knitr data_measure ---------------------------------------------------------

param_name <- unique(sensor$param.paramName);

measure    <- subset(sensor, param.paramName == "pył zawieszony PM10");
measure    <- split( measure, f = measure$id);

measure <- lapply(
  X   = measure,
  FUN = function(x){

    #     print(x$id);
    y <- paste0("http://api.gios.gov.pl/pjp-api/rest/data/getData/", x$id);
    y <- httr::GET(url = y);
    y <- httr::content(y, as = "text", encoding = "UTF-8");
    y <- jsonlite::fromJSON(y, flatten = TRUE);
    y <- merge(x, y$value);

    return(y);
  }
  );

measure <- do.call(rbind, measure);

head(measure)

date_range <- range(
  strptime(unique(measure$date), format = "%Y-%m-%d %H:%M:%S")
  );


## @knitr retrieve_data_measure -------------------------------------------------

if(write_ref) {
  save(measure, date_range, file = paths$f05)
} else if(use_ref) {
  load(paths$f05, verbose = TRUE);
} else {}


## @knitr table_measure --------------------------------------------------------

DT::datatable(
  data = measure, rownames = FALSE,
  caption = paste0(
    "The API query return only recent aquisition; at the time of this
    analysis, measure were available on an hour basis from ",
    paste(format(date_range, "%A %d %B %Hh%M"), collapse = " to "), 
    "."
  )
  );


## @knitr data_geo_shape_pl ----------------------------------------------------

gis_sh <- rgdal::readOGR(dsn = paths$f01, layer = "NUTS_RG_60M_2016_3035");
gis_sh <- subset(     gis_sh, CNTR_CODE == "PL" & LEVL_CODE == 2);
gis_sh <- sp::spTransform(gis_sh, sp::CRS("+proj=longlat +datum=WGS84"));


## @knitr retrieve_data_gis -------------------------------------------------

if(write_ref) {
  save(gis_sh, file = paths$f06)
} else if (use_ref){
  load(paths$f06, verbose = TRUE);
} else {}


## @knitr data_api_geo_shape_pl ------------------------------------------------

gis_sh <- geojsonio::geojson_read(
  file.path(
    "https://ec.europa.eu/eurostat/cache",
    "GISCO/distribution/v2/nuts/geojson",
    "NUTS_RG_60M_2016_3035_LEVL_2.geojson"
    ),
  what = "sp"
  );
gis_sh <- subset(gis_sh, CNTR_CODE == "PL");
gis_sh <- sp::spTransform(gis_sh, sp::CRS("+proj=longlat +datum=WGS84"));



## @knitr map_geo_shape_pl ----------------------------------------------------
sp::plot(gis_sh);


## @knitr data_geo_points ------------------------------------------------------

sel_date <- with(measure, table(date[!is.na(value)]));
sel_date <- sel_date[order(names(sel_date))];
sel_date <- sel_date[sel_date >= quantile(sel_date, probs = 0.85)];
sel_date <- names(rev(sel_date))[1];
if(use_ref) { sel_date <- "2020-04-16 14:00:00" }

gis_pts <- subset(measure, date == sel_date);
gis_pts <- subset(gis_pts, !(is.na(value)));
gis_pts <- merge(
  x = station, by.x = "id",
  y = gis_pts, by.y = "stationId"
  );

gis_pts <- within(
  gis_pts,
  {
    lon <- as.numeric(gegrLon);
    lat <- as.numeric(gegrLat);
  }
  );

gis_pts <- sp::SpatialPointsDataFrame(
  coords = gis_pts[c('lon', 'lat')],
  data   = gis_pts,
  proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  );


## @knitr map_geo_points ------------------------------------------------------

sp::plot(gis_sh);
sp::plot(gis_pts, add = TRUE);


## @knitr data_gis_raster ------------------------------------------------------

gis_grid <- raster::raster(
    ncols=300, nrows=300,
    #     xmn = min(gis_pts$lon) - 10, xmx = max(gis_pts$lon) + 10,
    #     ymn = min(gis_pts$lat) - 10, ymx = max(gis_pts$lat) + 10,
    ext = extent(gis_sh),
    crs = sp::CRS("+proj=longlat +datum=WGS84")
    ); 
gis_raster <- raster::rasterize(
  x = gis_pts, field = "value", fun = mean,
  y = gis_grid
  );


## @knitr graph_gis_raster -----------------------------------------------------

sp::plot(gis_sh);
sp::plot(gis_raster, add= TRUE); 
sp::plot(gis_pts, add = TRUE, pch = 1);


## @knitr data_gis_raster_intpl ------------------------------------------------

gis_mod <- gstat::gstat(id = "PM10", formula = value~1, data = gis_pts);

gis_raster_intpl <- raster::interpolate(gis_raster , gis_mod);
gis_raster_intpl <- raster::crop(gis_raster_intpl, raster::extent(gis_sh));
gis_raster_intpl <- raster::mask(gis_raster_intpl, gis_sh);


## @knitr graph_gis_raster_intpl -----------------------------------------------

sp::plot(gis_raster_intpl); 
sp::plot(gis_sh, add= TRUE);
sp::plot(gis_pts, add = TRUE, pch = 1);


## @knitr graph_gis_raster_intpl_2 ---------------------------------------------

sp::plot(
  gis_raster_intpl, col = colorRampPalette(c("green", "orange", "red4"))(15)
  ); 
sp::plot(gis_sh, add = TRUE, border = 'gray75');
sp::plot(gis_pts, add = TRUE, col = 'blue');
sp::plot(raster::rasterToContour(gis_raster_intpl), add = TRUE)


## @knitr data_map_leaflet -----------------------------------------------------

mymax <- 150;
gis_raster_intpl[gis_raster_intpl >= mymax] <- mymax-1;

pal  <- leaflet::colorNumeric(
  colorRampPalette(c("green", "orange", "red4"))(15),
  domain   = c(0, mymax),
  reverse  = FALSE,
  na.color = NA
  );

m <- leaflet::leaflet(gis_sh);
m <- leaflet::addTiles(map = m);
m <- leaflet::addProviderTiles(
  map = m, leaflet::providers[[37]]#3, 36, 42, 53,61, 62, 63
  );
m <- leaflet::addRasterImage(
  map = m, gis_raster_intpl, colors = pal, opacity = .8
  );
m <- leaflet::addLegend(
  map = m, pal = pal, values = raster::values(gis_raster_intpl)
  );
m <- leaflet::addPolylines(
  map = m, weight = 1, opacity = .8, color = "#EEEEEE"
  );
m <- leaflet::addPolylines(
  map = m, weight = 1, opacity = .8, color = "#949494",
  data = raster::rasterToContour(gis_raster_intpl)
  );
m <- leaflet::addCircleMarkers(
  map   = m,
  lat   = gis_pts$lat,
  lng   = gis_pts$lon,
  popup = paste(gis_pts$value, gis_pts$stationName),
  clusterOptions = leaflet::markerClusterOptions(),
  opacity     = 1,
  weight      = 2,
  fillOpacity = 1,
  fillColor   = pal(ifelse(gis_pts$value >= mymax, mymax -1, gis_pts$value))
  );
m <- leaflet::addControl(
  map = m,
  paste0(
    '<div style="text-align:center"><h3>',sel_date, '</h3>',
    '<a href="https://fcacollin.github.io/Latarnia">
    <img
    src="http://fcacollin.github.io/Latarnia/signature/logo_signature.png"
    width="100px">
    </a>
    <p>Administrative boundaries:<br>
    © EuroGeographics for the<br>
    administrative boundaries</p>
    </div>'
    ),
  position = 'bottomright'
  );

htmlwidgets::saveWidget(m, file = paths$f02);
# m;


## @knitr END ------------------------------------------------------------------


# [modeline]: # ( vim: set foldlevel=0: )

