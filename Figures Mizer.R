# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# R-code to plot Figures of :                                #
# "A Decade of Mizer: Advancements and Applications of Size  #
# Spectrum # Modeling in Marine and Freshwater Ecosystems"   #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Last modified 20/10/2024 #                                  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Maria Grazia Pennino  #
# & Francisco Izquierdo #
# ~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# Start here -------------------------------------------------------------------

rm(list = ls())

# Colors -----------------------------------------------------------------------

# Color scale for cathegories

# Define the models with the recoded names
names_cathegories <- c("Applications in Fisheries Management and Policy", 
                       "Modeling Ecosystem Dynamics and Species Interactions", 
                       "Methodological Advances and Model Validation", 
                       "Broad-Scale Ecological and Theoretical Studies", 
                       "Climate Change and Environmental Impact Projections"
                      )

colors_cathegories <- c("#FF8C27", # Applications in Fisheries Management and Policy
                        "#FFC186", # Modeling Ecosystem Dynamics and Species Interactions
                        "#A2E294", # Methodological Advances and Model Validation
                        "#40A942", # Broad-Scale Ecological and Theoretical Studies
                        "#DA3C3D") # Climate Change and Environmental Impact Projections


# Define the colors specific based on the names or cathegories
colors_cat <- setNames(colors_cathegories, names_cathegories)

# Color scale for countries 

# Define the models with the recoded names
names_countries <- c("Italy", "Netherlands", "Malaysia", "Denmark", "China", "USA", "UK", 
                      "Australia", "Canada", "Belgium", "Turkey", "Portugal", "Sweden", "Taiwan", 
                      "New Zealand", "South Africa", "France", "Spain", "Chile", "Switzerland", 
                      "Norway", "Germany")

colors_countries<- c( "#FF8C27",  # Italy
                      "#FFC186",  # Netherlands
                      "#BFBD94",  # Malaysia
                      "#B7CCEB",  # Denmark
                      "#40A942",  # China
                      "#A2E294",  # USA
                      "#3584BC",  # UK
                      "#DA3C3D",  # Australia
                      "#9F76C4",  # Canada
                      "#745B55",  # Belgium
                      "#D8B2C3",  # Turkey
                      "#6C6C6C",  # Portugal
                      "#C2C339",  # Sweden
                      "#3F8C94",  # Taiwan
                      "#FFA2A0",  # New Zealand
                      "#CBABA3",  # South Africa
                      "#DA86C1",  # France
                      "#C9B8D8",  # Spain
                      "#A1A1A1",  # Chile
                      "#9ABBC0",  # Switzerland
                      "#447290",  # Norway
                      "#8C9AAD"   # Germany
                      )

# Define the colors specific based on the names or cathegories
colors_count <- setNames(colors_countries, names_countries)

# F1 ---------------------------------------------------------------------------

# Cumulative Figure

# Load the necessary packages
library(ggplot2)
library(ggflowchart)

# Set up data
new = c(1,3,6,3,2,5,6,3,8,3,3)# create a vector with number of paper by year
cumulative <- cumsum(new)#create cumulative vector
line_cumulative <- data.frame(year = c(2014:2024,# preparing lines cumulative data
                                       2014:2024),
                              categ = c(rep('new',length(new)),rep('cumulative',length(new))),
                              freq = c(new,cumulative))


# Importing data
load("./Data/data.RData")

# Figure 1:yearly and cumulative scientific production plot by focus of research
ggplot()  + 
 
  geom_ribbon(data = line_cumulative[line_cumulative$categ == "cumulative",], 
              aes(x = year, ymin = 0, ymax = cumulative / 4), 
              fill = "black", alpha = 0.1) +  # Ajusta el color y transparencia del sombreado
  
  geom_bar(data = data, aes(x = Year, y = Count, fill = Category), stat = "identity") +
  
  geom_line(data = line_cumulative[line_cumulative$categ == "cumulative",], 
            aes(x = year, y = cumulative / 4), 
            stat = "identity", color = "black", size = 1, linetype="solid") +
  
  geom_point(data = line_cumulative[line_cumulative$categ == "cumulative",], 
             aes(x = year, y = cumulative / 4), 
             stat = "identity", color = "black", size = 1.3) +
  scale_fill_manual(values = colors_cat) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 4, name = "Cumulative (shaded line)")) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  ylab("Yearly (bars)") +
  xlab("Year") +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),  # Quita el título de la leyenda si no es necesario
    legend.text = element_text(size = 10)  # Ajusta el tamaño del texto si es necesario
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE))  # Coloca la leyenda en 3 filas

#  Save plot 
ggsave("./Figures/Figure_1.jpeg", width=6, height=6, dpi=300)

# F2 ---------------------------------------------------------------------------

# Map Figure
library(maps)
library(ggplot2)
library(mapdata)
library(ggrepel)

world_map <- map_data("world")

# Create the data frame with your locations and counts 
data <- data.frame(
  Area = c("North Sea", "Global", "Haizhou Bay, China", "UK shelf-seas",
           "South-east China Sea", "Ria Formosa, Portugal", "Hawaii",
           "Eastern Bering Sea", "North Yellow Sea, China", "Baltic Sea",
           "Lake Nipissing, Ontario", "Central-eastern tropical Pacific",
           "Eastern United States", "Australian Southern and Eastern Scalefish",
           "Tasmania", "NW Mediterranean", "Mörrum river, Sweden"),
  Count = c(9, 3, 5, 1, 1, 1, 2, 2, 3, 3, 2, 1, 1, 1, 1, 1, 1),
  Latitude = c(56.0, 0, 31.5, 55.0, 25.0, 56.0, 21.3, 60.0, 38.0, 57.0,
               46.3, 10.0, 37.0, -42.8, 41.0, 35.5, 56.2),  # Example latitudes
  Longitude = c(3.0, 0, 121.0, -4.0, 121.0, -9.0, -157.8, -179.0, 124.0, 19.0,
                -79.5, -156.0, -77.0, 147.0, 0.0, 3.0, 14.6)  # Example longitudes
)

# Map
ggplot() +
  # Plot the world map
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "grey85", color = "grey65") +
  
  # Add points for each location
  geom_point(data = data, aes(x = Longitude, y = Latitude, size = Count),
             color = "#DA3C3D", alpha = 0.5) +
  
  # Add labels
  geom_text_repel(data = data, aes(x = Longitude, y = Latitude, label = Area),
                  size = 3, box.padding = 0.5) +
  
  # Set up the map theme
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = " ", x = " ", y = " ") +
  
  # Adjust the size of the points in the legend (reduce size)
  scale_size_continuous(name = "Count", range = c(3, 14)) +  # Reduce size range
  
  # Increase the number of ticks on x and y axes with customized labels
  scale_x_continuous(
    breaks = seq(-180, 180, by = 60),
    labels = function(x) {
      sapply(x, function(x) {
        if (x < 0) {
          paste0(-x, "ºW")
        } else if (x > 0) {
          paste0(x, "ºE")
        } else {
          paste0(x, "º")
        }
      })
    }
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),
    labels = function(y) {
      sapply(y, function(y) {
        if (y < 0) {
          paste0(-y, "ºS")
        } else if (y > 0) {
          paste0(y, "ºN")
        } else {
          paste0(y, "º")
        }
      })
    }
  ) +
  
  coord_fixed(ratio = 1) 

# Save plot 
ggsave("./Figures/Figure_2.jpeg", width=8, height=5, dpi=300)

# F3 ---------------------------------------------------------------------------

# Sankey diagram Figure

# Load the necessary packages 
library(networkD3)
library(dplyr)
library(htmlwidgets)
library(webshot)

# Define the papers, countries, and corresponding categories
papers <- list(
  list(countries = c("UK", "Denmark", "Italy", "Netherlands"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("UK"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("China", "USA"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("UK", "Australia"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("Denmark", "UK"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("UK", "Australia"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("UK", "Australia"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("China", "USA"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("China", "USA"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("China", "USA"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("Denmark", "USA"), category = "Methodological Advances and Model Validation"),
  list(countries = c("Australia", "New Zealand", "Canada"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("USA"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("China","USA"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("UK", "Australia"), category = "Methodological Advances and Model Validation"),
  list(countries = c("USA", "Canada", "Spain", "Belgium", "Australia", "South Africa", "UK", "France", "Turkey"), category = "Methodological Advances and Model Validation"),
  list(countries = c("Portugal", "UK"), category = "Methodological Advances and Model Validation"),
  list(countries = c("Australia", "UK"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("USA", "Australia"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("USA", "Australia"), category = "Methodological Advances and Model Validation"),
  list(countries = c("UK", "Canada", "New Zealand", "Australia"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("Australia"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("USA", "Australia"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("China"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("Chile","UK"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("Sweden"), category = "Methodological Advances and Model Validation"),
  list(countries = c("UK"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("Canada"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("Australia"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("UK", "Australia", "Denmark","USA", "Canada", "Malaysia"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("China"), category = "Methodological Advances and Model Validation"),
  list(countries = c("Canada"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("Sweden", "Australia"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("China", "USA"), category = "Modeling Ecosystem Dynamics and Species Interactions"),
  list(countries = c("USA"), category = "Methodological Advances and Model Validation"),
  list(countries = c("Taiwan"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("Australia", "New Zealand"), category = "Broad-Scale Ecological and Theoretical Studies"),
  list(countries = c("Australia", "UK"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("Spain", "UK"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("Denmark", "Sweden"), category = "Applications in Fisheries Management and Policy"),
  list(countries = c("China"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("South Africa", "Sweden", "Australia", "USA", "Canada", "Spain", "France", "Switzerland", "New Zealand", "Norway"), category = "Climate Change and Environmental Impact Projections"),
  list(countries = c("USA", "UK", "Sweden", "Australia", "Germany"), category = "Climate Change and Environmental Impact Projections")
)

# Prepare the data for the Sankey diagram (countries and categories only)
links <- data.frame(
  source = character(),
  target = character(),
  value = numeric(),
  stringsAsFactors = FALSE
)

for (paper in papers) {
  category <- paper$category
  for (country in paper$countries) {
    # Link country to category
    links <- rbind(links, data.frame(source = country, target = category, value = 1))
  }
}

# Create nodes and links for Sankey plot
nodes <- data.frame(name = unique(c(links$source, links$target)))

# Map node names to indices
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Plot the Sankey diagram
sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                             Value = "value", NodeID = "name", units = "papers", fontSize = 12, 
                             nodeWidth = 30)

# Save the plot manually in jpeg
sankey_plot

#saveWidget(sankey_plot, "temp_sankey.html", selfcontained = TRUE)

# webshot("temp_sankey.html", "./Figures/Figure_3.jpeg", vwidth = 1000, vheight = 800)

# F4 ---------------------------------------------------------------------------

# Collaboration Figure

# Load necessary packages
library(igraph)
library(dplyr)
library(ggplot2)
library(ggraph)

# Define the papers and corresponding countries
papers <- list(
  list(title = "Blanchard et al. (2014)", countries = c("UK", "Denmark", "Italy", "Netherlands")),
  list(title = "Jennings & Collingridge (2015)", countries = c("UK")),
  list(title = "Zhang et al. (2015)", countries = c("China", "USA")),
  list(title = "Hyder et al. (2015)", countries = c("UK", "Australia")),
  list(title = "Andersen et al. (2016)", countries = c("Denmark", "UK")),
  list(title = "Datta & Blanchard (2016)", countries = c("UK", "Australia")),
  list(title = "Spence et al. (2016)", countries = c("UK", "Australia")),
  list(title = "Zhang et al. (2016a)", countries = c("China", "USA")),
  list(title = "Zhang et al. (2016b)", countries = c("China", "USA")),
  list(title = "Zhang et al. (2016c)", countries = c("China", "USA")),
  list(title = "Jacobsen et al. (2017)", countries = c("Denmark", "USA")),
  list(title = "Edwards et al. (2017)", countries = c("Australia", "New Zealand", "Canada")),
  list(title = "Szuwalski et al. (2017)", countries = c("USA")),
  list(title = "Zhang et al. (2018)", countries = c("China", "USA")),
  list(title = "Spence et al. (2018)", countries = c("UK", "Australia")),
  list(title = "Fu et al. (2019)", countries = c("USA", "Canada", "Spain", "Belgium", "Australia", "South Africa", "UK",
                                                 "France", "Turkey")),
  list(title = "West (2019)", countries = c("Portugal", "UK")),
  list(title = "Clements et al. (2019)", countries = c("Australia", "UK")),
  list(title = "Woodworth-Jefcoats et al. (2019)", countries = c("USA", "Australia")),
  list(title = "Reum et al. (2019)", countries = c("USA", "Australia")),
  list(title = "Edwards et al. (2020)", countries = c("UK", "Canada", "New Zealand", "Australia")),
  list(title = "Forestier et al. (2020)", countries = c("Australia")),
  list(title = "Reum et al. (2020)", countries = c("USA", "Australia")),
  list(title = "Wo et al. (2020)", countries = c("China")),
  list(title = "Canales et al. (2020)", countries = c("Chile", "UK")),
  list(title = "Lindmark (2020)", countries = c("Sweden")),
  list(title = "Spence et al. (2021)", countries = c("UK")),
  list(title = "Benoit et al. (2021)", countries = c("Canada")),
  list(title = "Forestier (2021)", countries = c("Australia")),
  list(title = "Robinson et al. (2022)", countries = c("UK", "Australia", "Denmark", "USA", "Canada", "Malaysia")),
  list(title = "Wo et al. (2022)", countries = c("China")),
  list(title = "Benoit et al. (2022)", countries = c("Canada")),
  list(title = "Lindmark et al. (2022)", countries = c("Sweden", "Australia")),
  list(title = "Lin et al. (2022)", countries = c("China", "USA")),
  list(title = "Falciani et al. (2022)", countries = c("USA")),
  list(title = "Kuo et al. (2022)", countries = c("Taiwan")),
  list(title = "Novaglio et al. (2022)", countries = c("Australia", "New Zealand")),
  list(title = "Audzijonyte et al. (2023)", countries = c("Australia", "UK")),
  list(title = "de Juan et al. (2023)", countries = c("Spain", "UK")),
  list(title = "Hansen et al. (2023)", countries = c("Denmark", "Sweden")),
  list(title = "Wo et al. (2024)", countries = c("China")),
  list(title = "Ortega-Cisneros et al. (2024)", countries = c("South Africa", "Sweden", "Australia", "USA", "Canada",
                                                              "Spain", "France", "Switzerland", "New Zealand", "Norway")),
  list(title = "Reum et al. (2024)", countries = c("USA", "UK", "Sweden", "Australia", "Germany"))
)

# Extract pairs of countries for collaborations
collaborations <- do.call(rbind, lapply(papers, function(paper) {
  if (length(paper$countries) > 1) {
    combn(paper$countries, 2, simplify = FALSE)
  } else {
    NULL
  }
}))

# Convert to a data frame (removing NULL values)
collaborations_df <- do.call(rbind, lapply(collaborations, function(x) {
  if (!is.null(x)) {
    data.frame(from = x[1], to = x[2], stringsAsFactors = FALSE)
  } else {
    NULL
  }
}))

if (nrow(collaborations_df) == 0) {
  stop("No collaborations found.")
}

# Count the number of collaborations between each pair of countries
collab_count <- collaborations_df %>%
  count(from, to)

# Count the number of publications per country
country_count <- data.frame(country = unlist(lapply(papers, `[[`, "countries")), stringsAsFactors = FALSE) %>%
  count(country)

# Create the graph object
g <- graph_from_data_frame(d = collab_count, vertices = country_count, directed = FALSE)

# Set attributes for nodes and edges
V(g)$size <- country_count$n[match(V(g)$name, country_count$country)]
E(g)$width <- collab_count$n

if (any(!country_count$country %in% V(g)$name)) {
  stop("Some countries are missing in the graph.")
}

# Define a color vector for each country (ensure 'colors_count' is defined)
colors_count <- setNames(RColorBrewer::brewer.pal(n = length(unique(country_count$country)), name = "Paired"),
                         unique(country_count$country))

# Create the graph using ggraph (Adjusted Option 1)
ggraph(g, layout = "fr") +  
  geom_edge_link(aes(width = width), color = "grey") +
  geom_node_point(aes(size = size, color = name)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width(name = "Frequency",
                   breaks = c(50, 100, 150, 200, 250),
                   range = c(0.5, 5)) +
  scale_size_continuous(name = "Publications", range = c(3, 13)) +
  scale_color_manual(values = colors_count, guide = "none") +
  theme_bw() + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.3, "cm"),  # Further reduced spacing to 0.3 cm
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  guides(size = guide_legend(nrow = 3, override.aes = list(color = "grey")),
         edge_width = guide_legend(
           title = "Frequency",
           override.aes = list(color = "grey"),
           ncol = 1,
           keyheight = unit(0.4, "cm")
         )) + 
  xlab(" ") + 
  ylab(" ")

# Save the adjusted graph with a height of 4
ggsave("./Figures/Figure_4.jpeg", width = 6, height = 3.5)


# Summary metrics
num_nodes <- vcount(g)
num_edges <- ecount(g)
avg_degree <- mean(degree(g))
density <- edge_density(g)
clustering <- transitivity(g, type = "global")
net_diameter <- diameter(g)

# F5 ---------------------------------------------------------------------------

# Journals Figure

# Load the necessary packages
library(ggplot2)

# Create a data frame with journal and count data, sorted alphabetically
data <- data.frame(
  Journal = c("Acta Oceanologica Sinica", 
              "Canadian Journal of Fisheries and Aquatic Sciences",
              "Ecological Indicators", 
              "Ecological Modelling",
              "Ecosphere",
              "Earth’s Future",
              "Fisheries Research", 
              "Fish and Fisheries", 
              "Frontiers in Marine Science",
              "Frontiers in Marine Science", 
              "Global Change Biology", 
              "ICES Journal of Marine Science",
              "Limnology and Oceanography", 
              "Marine Ecology Progress Series", 
              "Marine Policy", 
              "Methods in Ecology and Evolution",
              "Nature Communications", 
              "Oikos", 
              "PLoS Biology",
              "PLoS ONE", 
              "Proceedings of the National Academy of Sciences",
              "Journal of Applied Ecology",
              "Ecology and Evolution"),
  Count = c(2, 3, 2, 2, 1, 2, 4, 4, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 2)
)

# Set the levels of the factor "Journal" in alphabetical order
data$Journal <- factor(data$Journal, levels = sort(unique(data$Journal)))

# Create the bar plot with journals in alphabetical order 
ggplot(data, aes(x = reorder(Journal, Journal), y = Count)) +
  geom_bar(stat = "identity", fill = "#A2E294", width = 0.8) +
  labs(title = " ",
       x = "Journal",
       y = "Count") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis tick labels by 90 degrees
        axis.text.y = element_text(angle = 0, hjust = 1),   # Set y-axis ticks to horizontal
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
        coord_flip() + # Flip the coordinates for horizontal bars 
        scale_x_discrete(limits = rev(levels(data$Journal))) 

# Save plot 
ggsave("./Figures/Figure_5.jpeg", width=6, height=8, dpi=300)

# F6, F7 and F8 are diagrams made in PPT/Canvas, not in R
