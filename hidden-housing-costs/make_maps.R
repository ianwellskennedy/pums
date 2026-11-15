# Draw the quarterly metro affordability maps (total payment and each cost
# component as a share of renters' median household income), laid out to
# match the published ArcGIS maps.
#
# Input is the csv written by rebuild_index_from_prior_quarter.R. Boundaries are
# 2023 cartographic (1:20m) CBSAs and states from tigris. Run from the repo root.

suppressMessages({
  library(dplyr); library(sf); library(tigris); library(ggplot2); library(grid); library(ragg)
})
options(tigris_use_cache = TRUE)

config <- list(
  input     = "hidden-housing-costs/outputs/cbsa_affordability_2026_09.csv",
  out_dir   = "hidden-housing-costs/outputs/maps",
  quarter   = "3q26",
  acs_year  = 2024,
  font      = "Avenir Next",
  byline    = "hidden-housing-costs/fonts/KaushanScript-Regular.ttf",
  n_labels  = 25
)

systemfonts::register_font("Kaushan Script", config$byline)

# Measure -> title wording and class breaks (upper bounds, as shares)
maps <- list(
  total = list(col = "renter_affordability_ratio", what = "total monthly\npayment",
               breaks = c(.40, .50, .60, .70, .80, .90)),
  insurance = list(col = "insurance_burden", what = "monthly\nhazard/fire/flood ins. bill",
                   breaks = c(.0175, .02, .03, .04, .05, .06)),
  taxes = list(col = "tax_burden", what = "monthly\nproperty tax bill",
               breaks = c(.02, .04, .06, .08, .10, .12)),
  # Electricity + gas + other fuels; water is mapped separately
  utilities = list(col = "utility_burden", what = "monthly\nutility bill",
                   breaks = c(.03, .04, .05, .06, .07, .08)),
  electricity = list(col = "elec_burden", what = "monthly\nelectricity bill",
                     breaks = c(.0175, .02, .03, .04, .05, .06)),
  gas = list(col = "gas_burden", what = "monthly\nnatural gas bill",
             breaks = c(.01, .0125, .015, .0175, .02, .0225)),
  water = list(col = "wat_burden", what = "monthly\nwater bill",
               breaks = c(.005, .0075, .01, .0125, .015, .0175))
)

# Short labels for the most populous metros
abbrev <- c(
  "35620" = "NYC", "31080" = "LA",  "16980" = "CHI", "19100" = "DFW", "26420" = "HOU",
  "33100" = "MIA", "47900" = "DC",  "12060" = "ATL", "37980" = "PHI", "38060" = "PHX",
  "14460" = "BOS", "40140" = "RIV", "41860" = "SF",  "19820" = "DET", "42660" = "SEA",
  "33460" = "MSP", "45300" = "TAM", "41740" = "SD",  "19740" = "DEN", "36740" = "ORL",
  "16740" = "CHA", "12580" = "BAL", "41180" = "STL", "41700" = "SAA", "12420" = "AUS",
  "38900" = "POR"
)

# Label position inside the zoom insets, as a share of the inset's width and height
inset_offsets <- list(DC = c(-.25, -.05), BAL = c(-.05, .12), PHI = c(-.3, .05))
inset_default <- c(.18, .2)
inset_labels <- list(SF = "SF", BOS = "BOS", DC = c("DC", "BAL", "PHI"), NYC = c("NYC", "PHI", "BAL"))

palette <- c("#4575B4", "#91BFDB", "#E0F3F8", "#FFFFBF", "#FEE090", "#FC8D59", "#D73027")
no_data <- "#CCCCCC"

# Page layout, as fractions of a 12 x 7.92 in page: left, bottom, width, height
page <- c(w = 12, h = 7.92)
layout <- list(
  main = c(.062, .061, .855, .867),
  SF   = c(.005, .811, .0525, .122),
  BOS  = c(.895, .477, .0975, .182),
  DC   = c(.875, .258, .1175, .208),
  NYC  = c(.856, .057, .1365, .189),
  AK   = c(.1578, .037, .1294, .2082),
  HI   = c(.2976, .037, .1066, .1145)
)

lwd <- list(cbsa = .45, state = .55, leader = .8)
label_size <- 4.3  # mm

# Page areas main-map labels must stay out of: left, bottom, right, top
keep_out <- list(
  title    = c(.08, .88, .92, 1),
  byline   = c(.60, .78, .87, .90),
  legend   = c(0, .03, .135, .325),
  footnote = c(.49, .04, .75, .15),
  source   = c(0, 0, .42, .03)
)

# 1. DATA ----

d <- read.csv(config$input, colClasses = c(CBSA_CODE = "character"))

cbsa <- core_based_statistical_areas(year = 2023, cb = TRUE, resolution = "20m", progress_bar = FALSE) %>%
  select(GEOID) %>%
  inner_join(d, by = c("GEOID" = "CBSA_CODE"))
states <- states(year = 2023, cb = TRUE, resolution = "20m", progress_bar = FALSE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))

crs <- list(conus = 5070, AK = 3338, HI = "ESRI:102007")
conus_states <- filter(states, !STUSPS %in% c("AK", "HI"))
conus_box <- st_bbox(st_transform(conus_states, crs$conus))

in_state <- function(x, abb) {
  pts <- st_point_on_surface(st_geometry(st_transform(x, crs$conus)))
  x[lengths(st_intersects(pts, st_union(st_transform(filter(states, STUSPS == abb), crs$conus)))) > 0, ]
}

top <- d %>% arrange(desc(population)) %>% head(config$n_labels) %>% pull(CBSA_CODE)

# 2. HELPERS ----

class_labels <- function(b) {
  pct <- function(x) paste0(format(x * 100, drop0trailing = TRUE, trim = TRUE), "%")
  c(paste0("<= ", pct(b[1])), paste0(pct(head(b, -1)), " to ", pct(b[-1])), paste0("> ", pct(tail(b, 1))))
}

# Map extent centred on a bounding box, padded to fill an inset box
fit_box <- function(b, box, pad) {
  cx <- mean(b[c("xmin", "xmax")]); cy <- mean(b[c("ymin", "ymax")])
  aspect <- (box[4] * page["h"]) / (box[3] * page["w"])
  half_w <- max((b["xmax"] - b["xmin"]) / 2, (b["ymax"] - b["ymin"]) / 2 / aspect) + pad
  list(x = unname(cx + c(-1, 1) * half_w), y = unname(cy + c(-1, 1) * half_w * aspect))
}
zoom_box <- function(codes, box, pad = 15000) {
  fit_box(st_bbox(st_transform(filter(cbsa, GEOID %in% codes), crs$conus)), box, pad)
}

# Alaska framed as on the published maps: mainland filling the box, the Aleutians
# and the panhandle running off the edges. All of Hawaii.
ak_box <- list(x = c(-840000, 1124000), y = c(453000, 2539000))
hi_box <- fit_box(st_bbox(st_transform(filter(states, STUSPS == "HI"), crs$HI)), layout$HI, 15000)
zooms <- list(
  SF  = zoom_box("41860", layout$SF),
  BOS = zoom_box(c("14460", "39300"), layout$BOS),
  DC  = zoom_box(c("47900", "12580", "37980"), layout$DC),
  NYC = zoom_box(c("35620", "37980"), layout$NYC)
)

# Metres of map per inch of page, used to size halos and leader-line gaps
m_per_in <- function(xlim, box) diff(xlim) / (box[3] * page["w"])

map_layer <- function(polys, st, crs_code, xlim = NULL, ylim = NULL) {
  ggplot() +
    geom_sf(data = st_transform(st, crs_code), fill = no_data, colour = "black", linewidth = lwd$state) +
    geom_sf(data = st_transform(polys, crs_code), aes(fill = class), colour = "black", linewidth = lwd$cbsa) +
    scale_fill_manual(values = palette, drop = FALSE, guide = "none") +
    coord_sf(crs = crs_code, xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
    theme_void()
}

# Leader lines (with a gap at each end) and haloed labels. `scale` is metres per
# inch, so halos and gaps are the same size on the page in every panel.
# Half the width and height of a haloed label, in inches
label_half <- function(lab) {
  list(w = systemfonts::string_width(lab, family = config$font, weight = "bold",
                                     size = label_size * .pt, res = 72) / 72 / 2 + .05,
       h = .1)
}

label_layers <- function(labs, scale, size) {
  r <- .02 * scale
  half <- label_half(labs$lab)
  # Leader runs from just off the metro's point to just short of the label's edge
  gaps <- labs %>%
    mutate(dx = abs(lx - x), dy = abs(ly - y), len = sqrt(dx^2 + dy^2),
           f1 = pmax(1 - half$w * scale / dx, 1 - half$h * scale / dy, na.rm = TRUE)
                - .03 * scale / len,
           f0 = .02 * scale / len) %>%
    filter(f1 > f0)
  halo <- lapply(seq(0, 2 * pi, length.out = 17)[-17], function(a) {
    geom_text(data = labs, aes(lx + r * cos(a), ly + r * sin(a), label = lab),
              family = config$font, fontface = "bold", size = size, colour = "white")
  })
  c(list(geom_segment(data = gaps, aes(x + f0 * (lx - x), y + f0 * (ly - y),
                                       xend = x + f1 * (lx - x), yend = y + f1 * (ly - y)),
                      linewidth = lwd$leader, lineend = "round")),
    halo,
    list(geom_text(data = labs, aes(lx, ly, label = lab),
                   family = config$font, fontface = "bold", size = size)))
}

# Interior point of each labelled metro, in CONUS Albers
label_points <- function(polys) {
  p <- polys %>% filter(GEOID %in% top) %>% st_transform(crs$conus)
  xy <- st_coordinates(st_point_on_surface(st_geometry(p)))
  tibble(GEOID = p$GEOID, lab = unname(abbrev[p$GEOID]), x = xy[, 1], y = xy[, 2])
}

# Text with a thick black outline and white fill (the byline)
outlined_text <- function(label, x, y, gp, r = unit(1.6, "pt")) {
  for (a in seq(0, 2 * pi, length.out = 25)[-25]) {
    grid.text(label, unit(x, "npc") + r * cos(a), unit(y, "npc") + r * sin(a),
              gp = modifyList(gp, gpar(col = "black")))
  }
  grid.text(label, x, y, gp = modifyList(gp, gpar(col = "white")))
}

# 3. LABEL PLACEMENT ----

# Main-map extent and the page -> map transform (coord_sf keeps the aspect
# ratio, so the map is centred in its box at `m_in` metres per inch)
main_x <- unname(conus_box[c("xmin", "xmax")]); main_y <- unname(conus_box[c("ymin", "ymax")])
m_in <- max(diff(main_x) / (layout$main[3] * page["w"]), diff(main_y) / (layout$main[4] * page["h"]))
to_map <- function(r) {
  cx <- layout$main[1] + layout$main[3] / 2; cy <- layout$main[2] + layout$main[4] / 2
  c(mean(main_x) + (r[c(1, 3)] - cx) * page["w"] * m_in,
    mean(main_y) + (r[c(2, 4)] - cy) * page["h"] * m_in)[c(1, 3, 2, 4)]
}
rect <- function(xmin, ymin, xmax, ymax) {
  st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin), ncol = 2, byrow = TRUE)))
}
page_rect <- function(r) { m <- to_map(r); rect(m[1], m[2], m[3], m[4]) }

# Area of each polygon inside another polygon
area_inside <- function(polys, poly) {
  x <- suppressWarnings(st_intersection(st_sf(id = seq_along(polys), geometry = polys), st_sf(geometry = poly)))
  out <- numeric(length(polys))
  if (nrow(x) > 0) out[x$id] <- as.numeric(st_area(x))
  out
}

# Length of each line inside a polygon
cross_len <- function(lines, poly) {
  x <- suppressWarnings(st_intersection(st_sf(id = seq_along(lines), geometry = lines), st_sf(geometry = poly)))
  out <- numeric(length(lines))
  if (nrow(x) > 0) out[x$id] <- as.numeric(st_length(x))
  out
}

# Place each main-map label near its metro, preferring grey or water. Labels
# never cover insets, page furniture or other labels; covering a metro is
# penalised rather than ruled out, so a label stays close to its metro when
# there's no clear spot nearby. Crowded metros go first.
place_labels <- function(pts) {
  metros <- st_transform(cbsa, crs$conus) %>% filter(GEOID %in% cbsa$GEOID)
  metros <- metros[st_intersects(metros, rect(main_x[1], main_y[1], main_x[2], main_y[2]), sparse = FALSE)[, 1], ]
  all_metros <- st_union(st_geometry(metros))
  blocked <- c(lapply(layout[c("SF", "BOS", "DC", "NYC", "AK", "HI")],
                      function(b) page_rect(c(b[1], b[2], b[1] + b[3], b[2] + b[4]) + c(-.005, -.005, .005, .005))),
               lapply(keep_out, page_rect))
  frame <- rect(main_x[1], main_y[1], main_x[2], main_y[2])

  half_w <- label_half(pts$lab)$w * m_in
  half_h <- label_half(pts$lab)$h * m_in
  crowding <- sapply(seq_len(nrow(pts)), function(i) sum(sqrt((pts$x - pts$x[i])^2 + (pts$y - pts$y[i])^2) < 4e5))
  grid <- expand.grid(d = seq(60, 220, by = 10) * 1000, a = seq(0, 350, by = 10) * pi / 180)

  pts$lx <- NA_real_; pts$ly <- NA_real_
  placed <- list()
  for (i in order(-crowding)) {
    own <- st_union(st_geometry(filter(metros, GEOID == pts$GEOID[i])))
    cand <- grid %>% mutate(lx = pts$x[i] + d * cos(a), ly = pts$y[i] + d * sin(a))
    boxes <- st_sfc(lapply(seq_len(nrow(cand)), function(k)
      rect(cand$lx[k] - half_w[i], cand$ly[k] - half_h, cand$lx[k] + half_w[i], cand$ly[k] + half_h)), crs = crs$conus)
    # Leave room for a visible leader: the metro's point stays well outside the label
    clear <- abs(cand$lx - pts$x[i]) > half_w[i] + .22 * m_in | abs(cand$ly - pts$y[i]) > half_h + .22 * m_in
    ok <- clear & st_within(boxes, st_sfc(frame, crs = crs$conus), sparse = FALSE)[, 1] &
      !apply(st_intersects(boxes, st_sfc(c(blocked, placed), crs = crs$conus), sparse = FALSE), 1, any)
    cand <- cand[ok, ]
    if (nrow(cand) == 0) stop("No label position found for ", pts$lab[i])
    leaders <- st_sfc(lapply(seq_len(nrow(cand)), function(k)
      st_linestring(rbind(c(pts$x[i], pts$y[i]), c(cand$lx[k], cand$ly[k])))), crs = crs$conus)
    # Score in metres: distance, + 400 km for a label fully on metros, + the
    # leader's run across other metros
    cover <- area_inside(boxes[ok], all_metros) / (4 * half_w[i] * half_h)
    cand$score <- cand$d + 4e5 * cover +
      pmax(cross_len(leaders, all_metros) - cross_len(leaders, own), 0)
    best <- cand[which.min(cand$score), ]
    pts$lx[i] <- best$lx; pts$ly[i] <- best$ly
    placed[[length(placed) + 1]] <- rect(best$lx - half_w[i], best$ly - half_h, best$lx + half_w[i], best$ly + half_h)
  }
  pts
}

# 4. DRAW ----

draw_map <- function(name, spec) {
  polys <- cbsa %>%
    mutate(class = cut(.data[[spec$col]], c(-Inf, spec$breaks, Inf), labels = class_labels(spec$breaks)))
  pts <- label_points(polys)

  main <- map_layer(polys, conus_states, crs$conus, main_x, main_y) +
    label_layers(main_labs, m_in, size = label_size)

  ak <- map_layer(in_state(polys, "AK"), filter(states, STUSPS == "AK"), crs$AK, ak_box$x, ak_box$y)
  hi <- map_layer(in_state(polys, "HI"), filter(states, STUSPS == "HI"), crs$HI, hi_box$x, hi_box$y)

  zoom_plots <- lapply(names(zooms), function(z) {
    zx <- zooms[[z]]$x; zy <- zooms[[z]]$y
    scale <- m_per_in(zx, layout[[z]])
    labs <- pts %>%
      filter(lab %in% inset_labels[[z]], x > zx[1], x < zx[2], y > zy[1], y < zy[2]) %>%
      mutate(off = lapply(lab, function(l) if (is.null(inset_offsets[[l]])) inset_default else inset_offsets[[l]]),
             lx = x + sapply(off, `[`, 1) * diff(zx), ly = y + sapply(off, `[`, 2) * diff(zy),
             # keep labels inside the frame (half label width/height, in inches)
             hw = (nchar(lab) * .065 + .05) * scale, hh = .1 * scale,
             lx = pmin(pmax(lx, zx[1] + hw), zx[2] - hw),
             ly = pmin(pmax(ly, zy[1] + hh), zy[2] - hh)) %>%
      select(-off, -hw, -hh)
    map_layer(polys, conus_states, crs$conus, zx, zy) +
      label_layers(labs, scale, size = 4.3)
  })
  names(zoom_plots) <- names(zooms)

  file <- file.path(config$out_dir, paste0("metro_renter_affordability_", config$quarter, "_", name, ".png"))
  agg_png(file, width = page["w"], height = page["h"], units = "in", res = 300)
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  panel <- function(p, box, frame = TRUE) {
    vp <- viewport(box[1], box[2], box[3], box[4], just = c("left", "bottom"))
    print(p, vp = vp)
    if (frame) { pushViewport(vp); grid.rect(gp = gpar(fill = NA, lwd = 2.2)); popViewport() }
  }
  panel(main, layout$main, frame = FALSE)
  panel(ak, layout$AK)
  panel(hi, layout$HI)
  for (z in names(zoom_plots)) panel(zoom_plots[[z]], layout[[z]])

  grid.text(paste0("Share of renters' median household income needed to afford ", spec$what,
                   " on purchase of a 'typical-value' home*"),
            .5, .99, vjust = 1,
            gp = gpar(fontfamily = config$font, fontface = "bold", fontsize = 19.5, lineheight = .92))
  outlined_text("By: Ian Kennedy", .735, .858,
                gpar(fontfamily = "Kaushan Script", fontsize = 22))
  grid.text(paste0("Labels for top ", config$n_labels, " metros by pop."), .735, .81,
            gp = gpar(fontfamily = config$font, fontsize = 13))

  lab <- rev(class_labels(spec$breaks)); fill <- rev(palette)
  for (i in seq_along(lab)) {
    y <- .284 - (i - 1) * .0412
    grid.rect(.006, y, .034, .0326, just = c("left", "bottom"), gp = gpar(fill = fill[i], lwd = 1.6))
    grid.text(lab[i], .0465, y + .0163, just = "left", gp = gpar(fontfamily = config$font, fontsize = 14))
  }

  grid.text(paste0("*assumes a 20% down payment on the typical\n",
                   "value for a home in the 33rd to 67th percentile\n",
                   "range. Monthly payment includes mortgage\n",
                   "payment at current rates, homeowner's insurance,\n",
                   "property taxes, utilities, and water."),
            .62, .095, gp = gpar(fontfamily = config$font, fontsize = 9.5, lineheight = 1.05))
  grid.text(paste0("Source: Zillow; US Census Bureau, ", config$acs_year, " ACS PUMS via tidycensus."),
            .006, .016, just = c("left", "center"), gp = gpar(fontfamily = config$font, fontsize = 11))
  dev.off()
  file
}

main_labs <- place_labels(label_points(cbsa))

dir.create(config$out_dir, showWarnings = FALSE, recursive = TRUE)
for (m in names(maps)) cat("Saved", draw_map(m, maps[[m]]), "\n")
