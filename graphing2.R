require(tidyverse)
require(ggthemes)

df <- read_csv("World_Data.csv") %>%
	select(-X1) %>%
	filter(Year >= 1990)

colnames(df) <- c("Year", "Population", "Area with Permanent Snow/Glaciers",
									"Major livestock density in agricultural areas",
									"Total Emissions of CO2eq",
									"Emissions of CO2eq from Agriculture",
									"Emissions of CO2eq from Energy Production",
									"Emissions of CO2eq from Land Use Sources",
									"Emissions of CO2eq from Forests",
									"Emissions of CO2eq from Waste",
									"Emissions of CO2eq from Other Sources",
									"Emissions of CO2eq from Transportation",
									"Use of Nitrogen per area of cropland",
									"Use of Phosphate per area of cropland",
									"Use of Potash per area of cropland",
									"Use of Pesticides per area of cropland",
									"Food supply",
									"Industrial Production",
									"Average Annual Precipitation",
									"Total Renewable Water Resources",
									"Total Renewable Water Resources Per Capita",
									"Built-up Land (Eco. Footprint)",
									"Carbon (Eco. Footprint)",
									"Cropland (Eco. Footprint)",
									"Fishing Grounds (Eco. Footprint)",
									"Forest Products (Eco. Footprint)",
									"Grazing Land (Eco. Footprint)",
									"Total (Eco. Footprint)",
									"Built-up Land (Biocapacity)",
									"Carbon (Biocapacity)",
									"Cropland (Biocapacity)",
									"Fishing Grounds (Biocapacity)",
									"Forest Products (Biocapacity)",
									"Grazing Land (Biocapacity)",
									"Total (Biocapacity)",
									"Birth Rate",
									"Death Rate")

df[3:ncol(df)] <- lapply(df[3:ncol(df)], scale)

df <- df %>%
	gather(key = "Measure", value = "Value", -Year, -Population)# %>%
	#mutate(Value = scale(Value))# %>%
	# filter(Measure %in% c("Total (Eco. Footprint, gha)", "Total (Biocapacity, gha)", "Carbon (Eco. Footprint, gha)", "Forest Products (Biocapacity, gha)", "Cropland (Biocapacity, gha)", "Cropland (Eco. Footprint, gha)", "Forest Products (Eco. Footprint, gha)", "Grazing Land (Biocapacity, gha)", "Fishing Grounds (Biocapacity, gha)"))

p <- df %>%
	ggplot(aes(x = Year, y = Value, color = Measure)) +
	geom_line() +
	theme_fivethirtyeight() +
	theme(text = element_text(family = "CMU Serif", size = 20),
				legend.position = "bottom",
				legend.text = element_text(size = 12),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				axis.line.y = element_blank(),
				axis.title = element_text(),
				# plot.title = element_text(size = 25, hjust = 0.5),
				plot.title = element_blank(),
				legend.title = element_blank(),
				plot.background = element_blank(),
				panel.border = element_blank(),
				panel.background = element_blank(),
				legend.background = element_blank(),
				legend.key = element_blank()) +
	guides(color = guide_legend(ncol = 3)) +
	ggtitle("Sample Factor Data by Year")

print(p)

ggsave(file="test.svg", width = 20, height = 14)

