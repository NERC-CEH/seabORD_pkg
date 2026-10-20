# Load in the UK coastline for plotting later:
coastline <- seabORD::cef_coast_3035

UK_coastline <- dplyr::filter(coastline, COUNTRY == "United Kingdom")

uk_coast_3035 <- sf::st_transform(UK_coastline,
                                      crs= "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" )

usethis::use_data(uk_coast_3035, overwrite = TRUE)
