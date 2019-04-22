library(dplyr)
load("IMMC_Data.RData")

f <- function(df) {
	dfs <- split(df, df$Element)
	for (i in 1:length(dfs)) {
		if (length(unique(dfs[[i]][["Area"]])) > 1)
			dfs[[i]] <- select(dfs[[i]], Year, Area, Element, Item, Value, Unit)
		else if (length(unique(dfs[[i]][["Country"]])) > 1) {
			dfs[[i]] <- select(dfs[[i]], Year, Area = Country, Element, Item, Value, Unit)
		}
	}
	dfs
}

emissions_agri_dfs <- f(emissions_agri)
emissions_landuse_dfs <- f(emissions_landuse)
environment_dfs <- f(environment)
foodbal_dfs <- f(foodbal)
foodsupply_dfs <- f(foodsupply)
forestry_dfs <- f(forestry)
population_dfs <- f(population)

prod_dfs <- split(prod, prod$Element)
for (i in 1:length(prod_dfs)) {
  var.name <- paste0(names(prod_dfs)[[i]])
  if (length(unique(prod_dfs[[i]][["Area"]])) > 1) {
    prod_dfs[[i]] <- rename(select(prod_dfs[[i]], c(Year, Country, Item, Value)), paste0(names(prod_dfs)[[i]], " (", head(prod_dfs[[i]][["Unit"]]), ")") = Value)
  }
  else if (length(unique(prod_dfs[[i]][["Country"]])) > 1) {
    prod_dfs[[i]] <- rename(select(prod_dfs[[i]], c(Year, Country, Item, Value)), Area = Country, paste0(names(prod_dfs)[[i]], " (", prod_dfs[[i]][["Unit"]][[1]], ")") = Value)
  }
}

rm(emissions_agri, emissions_landuse, environment, foodbal, foodsupply, forestry, population, prod)
rm(asti, commodity, cpi, deflators, employment, exchange, foodaidshipments, foodsecdata, inputs, investment, pricesA, pricesE, pricesM, trade, value)

population <- population_dfs[[2]]
glaciers_permasnow <- filter(environment_dfs[[5]], Item=="Permanent snow and glaciers")
livestock_density <- filter(environment_dfs[[11]], Item=="Major livestock types")
co2eq_total <- filter(environment_dfs[[12]], Item=="Sources total")
co2eq_from_agriculture <- filter(environment_dfs[[12]], Item=="Agriculture total")
co2eq_from_energy_prod <- filter(environment_dfs[[12]], Item=="Energy total")
co2eq_from_land_use <- filter(environment_dfs[[12]], Item=="Land use sources")
co2eq_from_forests <- filter(environment_dfs[[12]], Item=="Forest")
co2eq_from_waste <- filter(environment_dfs[[12]], Item=="Waste")
co2eq_from_other <- filter(environment_dfs[[12]], Item=="Other sources")
co2eq_from_transport <- filter(environment_dfs[[12]], Item=="Transport")
avg_nitrogen_per_cropland <- filter(environment_dfs[[43]], Item=="Nutrient nitrogen N (total)")
avg_phosphate_per_cropland <- filter(environment_dfs[[43]], Item=="Nutrient phosphate P2O5 (total)")
avg_potash_per_cropland <- filter(environment_dfs[[43]], Item=="Nutrient potash K2O (total)")
avg_pesticides_per_cropland <- filter(environment_dfs[[43]], Item=="Pesticides (total)")
kcal_per_person_per_day <- filter(foodbal_dfs[["Food supply (kcal/capita/day)"]], Item=="Grand Total")

avg_nitrogen_per_cropland <- rename(select(avg_nitrogen_per_cropland, c(Year, Area, Value)), "Use of Nitrogen per area of cropland (kg/ha)"=Value)
avg_pesticides_per_cropland <- rename(select(avg_pesticides_per_cropland, c(Year, Area, Value)), "Use of Pesticides per area of cropland (kg/ha)"=Value)
avg_phosphate_per_cropland <- rename(select(avg_phosphate_per_cropland, c(Year, Area, Value)), "Use of Phosphate per area of cropland (kg/ha)"=Value)
avg_potash_per_cropland <- rename(select(avg_potash_per_cropland, c(Year, Area, Value)), "Use of Potash per area of cropland (kg/ha)"=Value)
glaciers_permasnow <- rename(select(glaciers_permasnow, c(Year, Area, Value)), "Area with Permanent Snow/Glaciers (CCI-LC, 1000 ha)"=Value)
livestock_density <- rename(select(livestock_density, c(Year, Area, Value)), "Major livestock density in agricultural areas (LSU/ha)"=Value)
co2eq_total <- rename(select(co2eq_total, c(Year, Area, Value)), "Total Emissions of CO2eq (gigagrams)"=Value)
co2eq_from_waste <- rename(select(co2eq_from_waste, c(Year, Area, Value)), "Emissions of CO2eq from Waste (gigagrams)"=Value)
co2eq_from_energy_prod <- rename(select(co2eq_from_energy_prod, c(Year, Area, Value)), "Emissions of CO2eq from Energy Production (gigagrams)"=Value)
co2eq_from_forests <- rename(select(co2eq_from_forests, c(Year, Area, Value)), "Emissions of CO2eq from Forests (gigagrams)"=Value)
co2eq_from_land_use <- rename(select(co2eq_from_land_use, c(Year, Area, Value)), "Emissions of CO2eq from Land Use Sources (gigagrams)"=Value)
co2eq_from_other <- rename(select(co2eq_from_other, c(Year, Area, Value)), "Emissions of CO2eq from Other Sources (gigagrams)"=Value)
co2eq_from_transport <- rename(select(co2eq_from_transport, c(Year, Area, Value)), "Emissions of CO2eq from Transportation (gigagrams)"=Value)
co2eq_from_agriculture <- rename(select(co2eq_from_agriculture, c(Year, Area, Value)), "Emissions of CO2eq from Agriculture (gigagrams)"=Value)
temp_change <- rename(aggregate(temp_change["Value"], by=temp_change[c("Year", "Area")], function(x){sum(x)/12}), "Temperature change (Celsius/year)"=Value)
#std_dev_temp <- rename(select(std_dev_temp, c(Year, Area, Value)), "Standard Deviation of Temperature (Celsius)"=Value)
kcal_per_person_per_day <- rename(select(kcal_per_person_per_day, c(Year, Area, Value)), "Food supply (kcal/capita/day)"=Value)
population <- rename(select(filter(population, Year > 1960 & Year < 2017 & Area == "World"), c(Year, Area, Value)), "Population (1000 persons)"=Value)

final_list <- list(population, glaciers_permasnow, livestock_density, co2eq_total, co2eq_from_agriculture, co2eq_from_energy_prod, co2eq_from_land_use, co2eq_from_forests, co2eq_from_waste, co2eq_from_other, co2eq_from_transport, avg_nitrogen_per_cropland, avg_phosphate_per_cropland, avg_potash_per_cropland, avg_pesticides_per_cropland, kcal_per_person_per_day)

final_df <- Reduce(function(x, y) merge(x, y, by=c("Year", "Area"), all=TRUE), final_list)
#final_df <- filter(final_df, Year > 1990 & Year < 2015)
world_df <- aggregate(select(final_df, -c("Area", "Year")), by=final_df["Year"], function(x){sum(x, na.rm=TRUE)})

industrial_oecd <- read.csv("DP_LIVE_19042019021654237.csv")
world_df[["Industrial Production (% Relative to 2015)"]] <- c(rep(0,14), filter(industrial_oecd, LOCATION=="OECD")$Value)

aquastat_tables <- read.csv("aquastat_tables.csv")
aquastat_tables <- aquastat_tables %>% dplyr::filter(row_number() %% 6 != 5 & row_number() %% 6 != 4 & row_number() %% 6 != 0)
avg_precipitation <- select(aquastat_tables %>% filter(row_number() %% 3 == 1), c(X1978.1982, X1983.1987, X1988.1992, X1993.1997, X1998.2002, X2003.2007, X2008.2012, X2013.2017))
total_renewable_rscs <- select(aquastat_tables %>% filter(row_number() %% 3 == 2), c(X1978.1982, X1983.1987, X1988.1992, X1993.1997, X1998.2002, X2003.2007, X2008.2012, X2013.2017))
total_renewable_rscs_per_capita <- select(aquastat_tables %>% filter(row_number() %% 3 == 0), c(X1978.1982, X1983.1987, X1988.1992, X1993.1997, X1998.2002, X2003.2007, X2008.2012, X2013.2017))
avg_precipitation[avg_precipitation == ""] <- NA
total_renewable_rscs[total_renewable_rscs == ""] <- NA
total_renewable_rscs_per_capita[total_renewable_rscs_per_capita == ""] <- NA
avg_precipitation <- colSums(sapply(avg_precipitation, as.numeric), na.rm=TRUE)
total_renewable_rscs <- colSums(sapply(total_renewable_rscs, as.numeric), na.rm=TRUE)
total_renewable_rscs_per_capita <- colSums(sapply(total_renewable_rscs_per_capita, as.numeric), na.rm=TRUE)
world_precip <- rep(NA, 39)
world_renewable_water <- rep(NA, 39)
world_rn_water_per_capita <- rep(NA, 39)

for (i in 1:39) {
  world_precip[[i]] <- avg_precipitation[[ceiling(i/5)]]
  world_renewable_water[[i]] <- total_renewable_rscs[[ceiling(i/5)]]
  world_rn_water_per_capita[[i]] <- total_renewable_rscs_per_capita[[ceiling(i/5)]]
}

world_df[["Average Annual Precipitation (mm/year)"]] <- c(rep(0, 17), world_precip)
world_df[["Total Renewable Water Resources (10^9 m^3/year)"]] <- c(rep(0, 17), world_renewable_water)
world_df[["Total Renewable Water Resources Per Capita (m^3/capita/year)"]] <- c(rep(0, 17), world_rn_water_per_capita)

biocapacity <- read.csv("Biocapacity.csv")
eco_footprint <- read.csv("ecological_footprint.csv")
biocapacity <- rename(select(biocapacity, -c(Country.Name, Short.Name, Record, Data.Quality.Score, isoa2)), Year=year, "Built-up Land (Biocapacity, gha)"=Built.up.Land, "Carbon (Biocapacity, gha)"=Carbon, "Cropland (Biocapacity, gha)"=Cropland, "Fishing Grounds (Biocapacity, gha)"=Fishing.Grounds, "Forest Products (Biocapacity, gha)"=Forest.Products, "Grazing Land (Biocapacity, gha)"=Grazing.Land, "Total (Biocapacity, gha)"=Total)
eco_footprint <- rename(select(eco_footprint, -c(Country.Name, Short.Name, Record, Data.Quality.Score, isoa2)), Year=year, "Built-up Land (Eco. Footprint, gha)"=Built.up.Land, "Carbon (Eco. Footprint, gha)"=Carbon, "Cropland (Eco. Footprint, gha)"=Cropland, "Fishing Grounds (Eco. Footprint, gha)"=Fishing.Grounds, "Forest Products (Eco. Footprint, gha)"=Forest.Products, "Grazing Land (Eco. Footprint, gha)"=Grazing.Land, "Total (Eco. Footprint, gha)"=Total)
world_df <- merge(merge(world_df, eco_footprint, by="Year", all=TRUE), biocapacity, all=TRUE)

birth_rate <- read.csv("Birth Rate.csv", header=FALSE)
death_rate <- read.csv("Death Rate.csv", header=FALSE)
brdr_header <- c("Country Name", "Country Code", "Indicator Name", "Indicator Code", paste0(rep("Y", 60), 1960:2019))
names(birth_rate) <- brdr_header -> names(death_rate)
birth_rate <- select(filter(birth_rate, `Country Name`=="World"), paste0(rep("Y", 56), 1961:2016))
death_rate <- select(filter(death_rate, `Country Name`=="World"), paste0(rep("Y", 56), 1961:2016))
world_df[["Birth Rate, Crude (annually per 1000 people)"]] <- t(birth_rate)
world_df[["Death Rate, Crude (annually per 1000 people)"]] <- t(death_rate)

write.csv(world_df, "World_Data.csv")