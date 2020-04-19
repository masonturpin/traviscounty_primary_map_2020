library(tidyverse)
library(sf)
library(leaflet)
library(pdftools)
library(tabulizer)
library(ggmap)

# Reads in the file. Presidential results are every 13th page, so i'm only reading those pages
file <- pdf_subset("data/Dem_Pct.pdf", pages = seq(1,3199,13))
# Extract the tables using tabulizer, but the percent column moves around
tables <- extract_tables(file)
# Gets list of precints via the text reading of the pdf which i'll have to rewrite because i deleted it like a dumbass
tc_results_text <- pdf_text(file) %>%
  read_lines()
pcts <- tc_results_text[seq(4,8614,35)]
# The percent column moves around in each table, fixed via the below for loop
pres_results <- list()
for (i in 1:247) {
  pres_results[[i]] <- as_tibble(as.data.frame(tables[[i]][1:19,]) %>% 
                                      janitor::row_to_names(1) %>% 
                                      janitor::clean_names()) %>%
    select(candidate = x, total, vote_percent) %>%
    mutate(precinct = pcts[i], vote_percent = as.numeric(sub("%", "", vote_percent)))
}

pres_results <- bind_rows(pres_results)

# problem_whole_percs <- pres_results_v2 %>%
#   filter((vote_percent %% 1) == 0 & total > 1)
# table(problem_whole_percs$precinct)
# 
# problem_posi_percs <- pres_results_v2 %>%
#   filter((vote_percent %% 1) == 0 & vote_percent > 0)
# table(problem_posi_percs$precinct)
# 
# problem_na <- pres_results_v2 %>%
#   filter(is.na(vote_percent) == TRUE)
# table(problem_na$precinct)

plot_table <- pres_results %>%
  group_by(precinct) %>%
  filter(candidate %in% c("Bernie Sanders", "Joseph R. Biden", "Elizabeth Warren", "Michael R. Bloomberg")) %>%
  rename(PCT = precinct) %>%
  arrange(PCT, desc(vote_percent)) %>%
  mutate(rank = min_rank(desc(vote_percent))) %>%
  ungroup()

winners_table <- pres_results %>%
  group_by(precinct) %>%
  top_n(1, vote_percent) %>%
  rename(PCT = precinct)

winner_precincts <- st_read("data/TravisCountyElectionPrecincts/TravisCountyElectionPrecincts.shp") %>%
  left_join(winners_table) 

streets <- st_read("data/CoAStreetCenterline/geo_export_afe69ada-658c-435a-953d-7f03eeaa0a64.shp") %>%
  st_transform(4326) %>%
  filter(road_class %in% c(1,2))

water <- st_read("data/rivers/MajorRivers_dd83.shp") %>%
  st_transform(4326)

county <- st_read("data/tc_boundary/Boundary.shp") %>%
  st_transform(4326) %>%
  st_polygonize()

tc_water <- st_read("data/water/geo_export_27ab0c40-7710-431d-bcf0-c50a0d3cc1b4.shp") %>%
  st_transform(4326)
  
county_water <- county %>%
  st_intersection(tc_water)

county_streets <- county %>%
  st_intersection(streets)
  

theme_map <- function(base_size = 9, base_family = "") {
  require(grid)
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0,0))
}


palette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2")
# extrafont::font_import(paths = "D:/Documents/fonts")
extrafont::loadfonts(device = "win")
# ggmap(atx_basemap)

ggplot() +
  geom_sf(data = winner_precincts, aes(fill = candidate), color = "white", size = 0.4, inherit.aes = FALSE) +
  geom_sf(data = county_water, color = "#0072B2", fill = "#0072B2") +
  geom_sf(data = county_streets, color = "grey20") +
  coord_sf(crs = st_crs(4326)) +
  theme_map() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "Source Sans Pro")) +
  scale_fill_manual(values = palette) +
  labs(title = "Travis County 2020 Democratic Presidential Primary",
       subtitle = "Winners by Precinct")

# ggmap(atx_basemap) +
#   geom_sf(data = winner_precincts, aes(fill = candidate), color = "grey80", size = 0.4, inherit.aes = FALSE) +
#   geom_sf(data = streets, color = "white") +
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         text = element_text(family = "Source Sans Pro")) +
#   scale_fill_manual(values = palette) +
#   labs(title = "Travis County 2020 Democratic Presidential Primary",
#        subtitle = "Winners by Precinct")


winner_precinctsWGS84 <- st_transform(winner_precincts, 4326)
factpal <- colorFactor(topo.colors(10), winner_precinctsWGS84$candidate)
p_popup <- paste0("<strong>Precinct ", winner_precinctsWGS84$PCT, "</strong>", "<br/>",
                  "<strong>", winner_precinctsWGS84$candidate, ": </strong>", winner_precinctsWGS84$vote_percent)
leaflet(winner_precinctsWGS84) %>%
  addTiles() %>%
  addPolygons(color = "white",
              weight = 1,
              fillColor = ~factpal(candidate), # set fill color with function from above and value
              fillOpacity = 0.6, smoothFactor = 0.5,
              popup = p_popup) %>%
  addLegend("bottomright", 
            pal = factpal, 
            values = ~candidate,
            title = 'Precinct Winners') 

# possibly filter on rank == 1 or is.na == TRUE for the polygon fills
# create lists of 1st place, 2nd, 3rd, 4th then joining them with the PCT numbers so that NAs exist,
# then use those lists for the popups
plot_tableWGS84 <- st_read("data/TravisCountyElectionPrecincts/TravisCountyElectionPrecincts.shp") %>%
  left_join(plot_table) %>%
  st_transform(4326)
group_popup <- paste0("<strong>", plot_tableWGS84$candidate[plot_tableWGS84$rank==1], ": </strong>", plot_tableWGS84$vote_percent[plot_tableWGS84$rank==1])
leaflet(winner_precinctsWGS84) %>%
  addTiles(group = "OSM") %>%
  addPolygons(color = "white",
              weight = 1,
              fillColor = ~factpal(candidate), # set fill color with function from above and value
              fillOpacity = 0.8, smoothFactor = 0.5,
              popup = p_popup) %>%
  addLegend("bottomright", 
            pal = factpal, 
            values = ~candidate,
            title = 'Precinct Winners') 

# pres_results <- list()
# pres_results[[1]] <- data.frame(tables[[1]][2:19,c(1,2,3)]) %>%
#   rename(candidate = X1, votes = X2, percent = X3) %>%
#   as_tibble()
# for (i in 2:4) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# pres_results[[5]] <- data.frame(tables[[5]][2:19,c(1,2,3)]) %>%
#   rename(candidate = X1, votes = X2, percent = X3) %>%
#   as_tibble()
# for (i in 6:8) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in 9:247) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,3)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in which(pcts %in% list(114,120,163,224,325,368,412,444))) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# 
# 
# 
# for (i in 1:247) {
#   pres_results[[i]] <- pres_results[[i]] %>%
#     mutate(precinct = pcts[i], percent = as.numeric(sub("%", "", percent)), 
#            candidate = as.character(candidate), precinct = as.character(precinct), votes = as.numeric(votes))
# }
# 
# pres_results <- bind_rows(pres_results)
# 
# # Checking for incorrect percent columns first by looking at whole number percentages (suggesting percent column has votes)
# problem_whole_percs <- pres_results %>%
#   filter((percent %% 1) == 0 & votes > 1)
# table(problem_whole_percs$precinct)
# 
# problem_posi_percs <- pres_results %>%
#   filter((percent %% 1) == 0 & percent > 0)
# table(problem_posi_percs$precinct)
# 
# problem_na <- pres_results %>%
#   filter(is.na(percent) == TRUE)
# table(problem_na$precinct)
# 
# #Remaking results table using info from problem DFs
# pres_results <- list()
# pres_results[[1]] <- data.frame(tables[[1]][2:19,c(1,2,3)]) %>%
#   rename(candidate = X1, votes = X2, percent = X3) %>%
#   as_tibble()
# for (i in 2:4) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# pres_results[[5]] <- data.frame(tables[[5]][2:19,c(1,2,3)]) %>%
#   rename(candidate = X1, votes = X2, percent = X3) %>%
#   as_tibble()
# for (i in 6:8) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in 9:247) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,3)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in which(pcts %in% list(114,120,163,224,325,368,412,444))) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in which(pcts %in% unique(problem_na$precinct))) {
#   pres_results[[i]] <- data.frame(tables[[i]][2:19,c(1,2,4)]) %>%
#     rename(candidate = X1, votes = X2, percent = X3) %>%
#     as_tibble()
# }
# 
# for (i in 1:247) {
#   pres_results[[i]] <- pres_results[[i]] %>%
#     mutate(precinct = pcts[i], percent = as.numeric(sub("%", "", percent)), 
#            candidate = as.character(candidate), precinct = as.character(precinct), votes = as.numeric(votes))
# }
# 
# pres_results <- bind_rows(pres_results)


