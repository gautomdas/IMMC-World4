pkg <- c("ggplot2", "GGally", "network", "sna", "dplyr", "tidyverse")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}
require(GGally)
require(network)
require(sna)
require(ggplot2)
require(dplyr)
require(tidyverse)


q <- 0.70 #Constant determining how high correlation should be

spearm <- rename(select(read.csv("spearm.csv"), -X), Var1 = X0, Var2 = X1, Correlation = X2)
spearm[[1]] <- as.character(spearm[[1]])
spearm[[2]] <- as.character(spearm[[2]])
spearm[[3]] <- as.numeric(spearm[[3]])

nodes <- data.frame(stat = c(unique(union(spearm[[1]], spearm[[2]]))))
nodes <- nodes %>% rowid_to_column("id")
nodes[[1]] <- as.numeric(nodes[[1]])
nodes[[2]] <- as.character(nodes[[2]])

pairs <- spearm %>% filter(abs(Correlation) > q) %>% group_by(Var1, Var2) %>% ungroup()

edges <- pairs %>% left_join(nodes, by = c("Var1" = "stat")) %>% rename(from = id)
edges <- edges %>% left_join(nodes, by = c("Var2" = "stat")) %>% rename(to = id)
edges <- select(edges, from, to, Correlation)

stat_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore_eval = FALSE, directed = FALSE)

ggnet2(stat_network, node.size = 6, edge.size = 1, edge.color = 'grey', mode = 'circle', color = "stat", color.legend = "Statistic (q-value = 0.85)",
        palette = c("Average Annual Precipitation (mm/year)" = "#d6a46e",
                    "Cropland (Eco. Footprint, gha)" = "#685ed3",
                    "Emissions of CO2eq from Forests (gigagrams)" = "#9ebe35",
                    "Emissions of CO2eq from Agriculture (gigagrams)" = "#a33caa",
                    "Built-up Land (Biocapacity, gha)" = "#62c655",
                    "Emissions of CO2eq from Waste (gigagrams)" = "#d56ad9",
                    "Emissions of CO2eq from Transportation (gigagrams)" = "#509b2e",
                    "Industrial Production (% Relative to 2015)" = "#cd4a97",
                    "Food supply (kcal/capita/day)" = "#54bd7b",
                    "Major livestock density in agricultural areas (LSU/ha)" = "#da4376",
                    "Area with Permanent Snow/Glaciers (CCI-LC, 1000 ha)" = "#55c8b0",
                    "Total Renewable Water Resources (10^9 m^3/year)" = "#ce364a",
                    "Forest Products (Eco. Footprint, gha)" = "#46aed7",
                    "Carbon (Eco. Footprint, gha)" = "#cb4626",
                    "Death Rate, Crude (annually per 1000 people)" = "#528fdf",
                    "Cropland (Biocapacity, gha)" = "#e08f2e",
                    "Forest Products (Biocapacity, gha)" = "#907fe0",
                    "Grazing Land (Eco. Footprint, gha)" = "#aead3b",
                    "Built-up Land (Eco. Footprint, gha)" = "#8a549b",
                    "Total Emissions of CO2eq (gigagrams)" = "#cea339",
                    "Emissions of CO2eq from Land Use Sources (gigagrams)" = "#5162a5",
                    "Birth Rate, Crude (annually per 1000 people)" = "#e87c47",
                    "Grazing Land (Biocapacity, gha)" = "#3a9a87",
                    "Emissions of CO2eq from Other Sources (gigagrams)" = "#d4645d",
                    "Total (Eco. Footprint, gha)" = "#488133",
                    "Total Renewable Water Resources Per Capita (m^3/capita/year)" = "#d28cd4",
                    "Emissions of CO2eq from Energy Production (gigagrams)" = "#6c751f",
                    "Total (Biocapacity, gha)" = "#9a9cdb",
                    "Fishing Grounds (Biocapacity, gha)" = "#a95f26",
                    "Population (1000 persons)" = "#347c52",
                    "Fishing Grounds (Eco. Footprint, gha)" = "#ee91aa",
                    "Use of Potash per area of cropland (kg/ha)" = "#9db269",
                    "Use of Nitrogen per area of cropland (kg/ha)" = "#96405f",
                    "Use of Phosphate per area of cropland (kg/ha)" = "#627037",
                    "Carbon (Biocapacity, gha)" = "#c26d8a",
                    "Use of Pesticides per area of cropland (kg/ha)" = "#8c6d2c",
                    "Year" = "#b26850"))