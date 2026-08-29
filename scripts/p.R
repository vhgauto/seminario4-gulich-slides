# paquetes ---------------------------------------------------------------

library(terra)
library(ggplot2)
library(tidyterra)

# datos ------------------------------------------------------------------

mm <- readRDS("scripts/nn_turb.rds")

r <- rast("scripts/20260224.tif") / 10000

s <- 3

terra::stretch(sqrt(r), minv = 0, maxv = s) |>
  plotRGB(r = 4, g = 3, b = 2, smooth = FALSE, scale = s, alpha = 1)

mndwi <- (r$B03 - r$B11) / (r$B03 + r$B11)

mndwi_mask <- thresh(mndwi)

mndwi_mask[isFALSE(mndwi_mask)] <- NA

w <- r * mndwi_mask

turb <- tidysdm::predict_raster(tune::extract_workflow(mm), w)

rgb <- maptiles::get_tiles(
  w,
  provider = "Esri.WorldImagery",
  zoom = 16,
  crop = TRUE
)

p <- grass_db |>
  dplyr::filter(pal == "aspect") |>
  dplyr::pull(hex)

# p <- grass_db[grass_db$pal == "precipitation_monthly", ]$hex
# p <- hypsometric_tints_db[
#   hypsometric_tints_db$pal == "precipitation monthly",
# ]$hex
# p <- tidyterra::wiki.colors(100)
# p <- tidyterra::whitebox.colors(palette = "muted", n = 100)
p <- tidyterra::princess.colors(100, "maori")

scales::show_col(p, labels = FALSE)

n <- 100
col <- colorRampPalette(p)(n)
scales::show_col(col[1:(length(col) - 1)], labels = FALSE)

logo_shiny <- grid::rasterGrob(
  png::readPNG("scripts/shiny.png"),
  interpolate = TRUE
)

logo_r <- grid::rasterGrob(
  png::readPNG("scripts/Rlogo.png"),
  interpolate = TRUE
)

# e <- vect(ext(turb), crs = crs(turb))

delta <- .25

x_r <- .1
x_r2 <- x_r + delta
y_r <- .02
y_r2 <- y_r + delta

x_shiny <- .4
x_shiny2 <- x_shiny + delta * .85
y_shiny <- .02
y_shiny2 <- y_shiny + delta * .85

g <- ggplot() +
  geom_spatraster_rgb(data = rgb, interpolate = FALSE, maxcell = 2e6) +
  geom_spatraster(data = turb, interpolate = FALSE, show.legend = FALSE) +
  annotation_custom(
    grob = logo_r,
    xmin = I(x_r),
    xmax = I(x_r2),
    ymin = I(y_r),
    ymax = I(y_r2)
  ) +
  annotation_custom(
    grob = logo_shiny,
    xmin = I(x_shiny),
    xmax = I(x_shiny2),
    ymin = I(y_shiny),
    ymax = I(y_shiny2)
  ) +
  coord_sf(expand = FALSE) +
  scale_fill_gradientn(colors = rev(col), na.value = NA) +
  theme_void()

ggsave("p.png", g, width = 25, height = 25, units = "cm")

browseURL("p.png")
