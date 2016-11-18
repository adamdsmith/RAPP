# devtools::install_github("Rdatatable/data.table")
# devtools::install_github("adamdsmith/geobird")
# devtools::install_github("bhaskarvk/leaflet.extras")
pacman::p_load(raster, dplyr, readxl, sp, rgdal, rgeos, geobird, 
               leaflet, leaflet.extras, tidyr, viridis, geojsonio)

keep_rapp <- c(1.08, 1.20, seq(1.30, 1.43, 0.01), 1.71, 5.41, 5.04, 5.07, 5.47, 6.09, 7.06)
keep_rapp <- formatC(round(keep_rapp, 2), format='f', digits=2)
rapp <- read_excel("./Data/Allregionmeasures.xls", skip = 1) %>%
  dplyr::select(Zone, Complex, Location, RappMeasure, FY, Target, Actual) %>%
  filter(grepl(paste(keep_rapp, collapse = "|"), RappMeasure))

# Load US counties and refuge polygons
ref <- readOGR("./GIS", "refuges")
ref@data <- mutate(ref@data,
                   ORGNAME = geobird:::Cap(ORGNAME),
                   Location = gsub("National Wildlife Refuge", "NWR", ORGNAME)) %>%
  select(Location)
# usa <- readOGR("./GIS", "us_counties")
# usa@data <- mutate(usa@data,
#                    state = substr(county, 4, 5))
# usa <- spTransform(usa, CRSobj = CRS(proj4string(ref)))
# usa <- raster::aggregate(usa, by = "state")
# se <- usa[usa$state %in% c("LA", "MS", "AL", "GA", "FL", "SC", "NC", "TN", "KY", "AR"),]

# A few edits to make compatible
rapp$Location <- gsub("Dale Bumpers ", "", rapp$Location)
rapp$Location <- gsub("DArbonne", "D'arbonne", rapp$Location)
rapp$Location <- gsub("J.N. Ding", "J. N. Ding", rapp$Location)
rapp$Location <- gsub("ACE Basin", "Ace Basin", rapp$Location)

# Confirm compatability
refs_rapp <- sort(unique(rapp$Location))
refs_poly <- sort(unique(ref$Location))
if (length(refs_rapp[which(!(refs_rapp %in% refs_poly))]) > 0) {
  cat("KEEP TRYING!\nThis one still doesn't match!\n")
  refs_rapp[which(!(refs_rapp %in% refs_poly))]
} else cat("NICE JOB!")

# Time to tidy RAPP
new_scale <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

rapp <- rapp %>% 
  mutate(Zone = as.integer(gsub("R4 Zone ", "", Zone)),
         Complex = gsub(" Total| Complex", "", Complex),
         RAPP = substr(RappMeasure, 1, 4),
         RAPP_cat = gsub("\\d.\\d\\d[[:space:]]+", "", RappMeasure),
         Actual = as.numeric(gsub(",", "", Actual))) %>%
  select(Zone, Complex, Location, RAPP, RAPP_cat, Actual) %>%
  group_by(RAPP) %>%
  mutate(Scaled_measure = new_scale(Actual)) %>%
  as.data.frame()

# Look up table for RAPP codes with tidied text
rapp_lu <- unique(rapp[, c("RAPP", "RAPP_cat")]) %>%
  mutate(RAPP_cat = gsub("of ", "", RAPP_cat),
         RAPP_cat = gsub(" for | by ", ": ", RAPP_cat),
         RAPP_cat = gsub(" or ", "/", RAPP_cat),
         RAPP_cat = gsub("Number", "#", RAPP_cat),
         RAPP_cat = gsub("identified ", "", RAPP_cat),
         RAPP_cat = gsub("non-native, ", "", RAPP_cat))

rapp_wide <- rapp %>%
  select(Zone, Complex, Location, RAPP, Actual) %>%
  spread(RAPP, Actual)

# Filter shapefile, then join and tidy
ref <- ref[ref$Location %in% refs_rapp, ]
ref@data <- left_join(ref@data, rapp_wide, by = "Location") 
# geojson_write(ref, file = "./GIS/refuge_rapp.geojson")
# gj <- readr::read_file("./GIS/refuge_rapp.geojson")


# Create map
p <- leaflet() %>%
  setView(-80.969436, 30.264745, 5) %>%
  # Base map 
  addProviderTiles("CartoDB.DarkMatter") %>%
  addBootstrapDependency()

for (r in rapp_lu$RAPP) {
  # Set up separate overlays/colors by rapp measure
  rapp_colors = colorNumeric(palette = viridis(256), domain = ref[[r]])
  grp <- rapp_lu[rapp_lu$RAPP == r, "RAPP_cat"]
  p <- p %>% 
    addGeoJSONChoropleth(
      gj,
      valueProperty = r,
      opacity = 1, fillOpacity = 1,
      scale = viridis(8), 
      mode = "q", steps = 8, #padding = c(0.2,0),
      labelProperty = c("Location"),
      popupProperty = propstoHTMLTable(
        props = c("Zone", "Complex", "Location", r),
        table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
      color=rapp_colors(ref[[r]]), 
      legendOptions = legendOptions(title=grp),
      group = grp)
}

p %>% addLayersControl(baseGroups = rapp_lu$RAPP_cat,
                       options = layersControlOptions(collapsed = FALSE)) %>%
  addFullscreenControl()







