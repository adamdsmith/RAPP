pacman::p_load(dplyr, ggplot2, ggmap, gridExtra, grid, leaflet, viridis, Hmisc, Cairo)

# Setting theme for producing figures
theme_set(theme_bw(base_size = 20))
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major= element_blank(),
             panel.border = element_blank(),
             panel.background= element_blank(),
             axis.line = element_blank())

# Load RAPP tables
load("./Output/RAPP_tables.rda", verbose = TRUE)

# Find RAPP categories reported (i.e., response > 0) by at least 10 refuges
rapp_lu <- rapp_lu[which(colSums(rapp_wide[, rapp_lu$RAPP] != 0) >= 10), ]

# Set up receiving list
rapp_maps <- vector("list", nrow(rapp_lu))
names(rapp_maps) <- rapp_lu$RAPP

se <- c(left = -95, bottom = 17, right = -64, top = 39.25)
se_map <- get_stamenmap(se, maptype = "toner-lite", zoom = 6)

for (r in rapp_lu$RAPP) {
  
  d <- rapp_wide[, c("Zone", "Complex", "Location", "lat", "lon", r)]
  d <- d[d[[r]] > 0, ]
  d$quant<- cut2(d[[r]], g = 4, digits = 20)
  
  # Hocus pocus to make labels look nicer
  fac_names <- levels(d$quant) 
  fac_names[1] <- sub(".*, ", "< ", fac_names[1])
  fac_names[4] <- sub(",.*$|, .*$", "", fac_names[4]) %>%
    sub("\\[|\\[ ", "", .)
  fac_names <- fac_names %>%
    gsub("\\[|\\[ +", "", .) %>%
    gsub(", |,", " - ", .) %>%
    gsub(")", "", .)
  meas <- paste(strwrap(rapp_lu[rapp_lu$RAPP == r, "RAPP_cat"], 23))
  meas <- paste(c(paste0("RAPP ", r, ":"), meas), collapse = "\n")
  
  rapp_maps[[r]] <- ggmap(se_map, extent = "device") +
    geom_point(data = d, aes(x=lon, y=lat, fill=quant, size=quant), pch = 21) + 
    scale_fill_manual(meas, values = viridis(4, option = "plasma"),
                      labels = paste0(c("", "", "", "\u2265 "), fac_names)) +
    scale_size_manual(meas, values = c(1, 2, 3, 4),
                      labels = paste0(c("", "", "", "\u2265 "), fac_names)) +
    theme(legend.position = c(0.6, 0.7),
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key = element_blank())
}

#rl <- marrangeGrob(rapp_maps, nrow = 1, ncol = 1, top = NULL)
CairoPDF("./Output//FY2010_SE_RAPP_maps.pdf", width = 10.34, height = 8.5)
invisible(lapply(rapp_maps, print))
dev.off()
