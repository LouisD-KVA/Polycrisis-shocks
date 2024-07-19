setwd("C:/Users/basti/Documents/GitHub/Shocks")
# Install and load necessary libraries using a vector
packages <- c("circlize","tidygraph","scico","dplyr", "readr","stats","tidyr","ggplot2","mFilter","zoo","dplR","tools","countrycode","ggraph","igraph")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = ifelse(pkg == "filteR", "http://R-Forge.R-project.org", "https://cloud.r-project.org"))
    library(pkg, character.only = TRUE)
  }
}

library(tidyr)
library(dplyr)
library(purrr)


# Read CSV file
data_original <- read_csv("Data/shocks_count.csv")%>% 
    rename(country = `Country name`,  
        year = Year,
        shock_category = `Shock Category`, 
        shock_type = `Shock Type`) %>% 
        mutate(count = as.integer(count), year = as.integer(year))

glimpse(data_original)
original_plot <- ggplot(data_original %>% filter(count>0,country %in% c("Israel","Palestine")), 
  aes(x = year, y=count,color=country,shape=shock_category))+
geom_point()+theme_bw() + labs(title="original dataset") 

data_original_grouped <- data_original %>% group_by(country,shock_category) %>% summarise(count=sum(count,na.rm=TRUE))

ggplot(data_original_grouped %>% filter(country %in% c("Israel","Palestine"), shock_category != "CONFLICTS"),aes(y=count,x=country,fill=shock_category))+
geom_bar(stat="identity")+theme_bw() + labs(title="Shock in Original Dataset (Conflicts category not shown)") 


new_plot <- ggplot(data %>% filter(count>0,country %in% c("Israel","Palestine")), 
  aes(x = year, y=count,color=country,shape=shock_category))+
geom_point()+theme_bw() + labs(title="new data")

library(ggpubr)
ggarrange(original_plot,new_plot,ncol=2,common.legend=TRUE,legend="right")

data <- read_csv("Data/Shocks_Database_counts.csv") %>% 
    rename(country = `Country name`,  
        year = Year,
        shock_category = `Shock category`, 
        shock_type = `Shock type`) %>% 
        mutate(count = as.integer(count), year = as.integer(year)) %>% 
        filter(shock_category %in% c("CLIMATIC", "CONFLICTS", "ECOLOGICAL", "ECONOMIC", "GEOPHYSICAL", "TECHNOLOGICAL")) %>%
        mutate(shock_category = toTitleCase(tolower(shock_category))) %>%
        mutate(countrycode = countrycode(country,origin="country.name",destination="iso3c")) 

regions <-  read_csv("Data/r5regions.csv") %>% rename(region = Column1, countrycode=Column2)
glimpse(regions)

data <- left_join(data, regions, by = "countrycode") %>%
  mutate(region = sub("R5", "", region))  # Remove 'R5' from the region column

data_master <- data
glimpse(data)


data_bycat <- data %>% group_by(shock_category,year,countrycode,region) %>% summarise(count = sum(count,na.rm=TRUE))


ggplot(data_bycat %>% filter(count>0), aes(x = year, y=count,fill=region))+
#ggplot(data %>% filter(count>0), aes(x = year, y=count,fill=shock_type))+
geom_bar(stat="identity")+
facet_wrap(~shock_category, scales="free_y")




library(RColorBrewer)

# Generate base colors for each shock_category
categories <- unique(data$shock_category)
base_colors <- brewer.pal(length(categories), "Set2")

# Create a named vector for base colors
category_colors <- setNames(base_colors, categories)
lighter_color <- adjustcolor(category_colors[1], alpha.f = 0.5)

lighter_color
category_colors[1]
colorRampPalette(c(category_colors[1], lighter_color))(5)

# Function to generate shades of a color
# Function to generate shades of a color
generate_shades <- function(base_color, n) {
  lighter_color <- adjustcolor(base_color, alpha.f = 0.3)  # Adjust alpha to lighten the color
  colorRampPalette(c(base_color, lighter_color))(n)
}

# Function to generate lighter shades of a color
# Function to generate lighter shades of a color
generate_shades <- function(base_color, n) {
  # Convert hex to RGB
  rgb_color <- col2rgb(base_color) / 255
  # Generate a sequence of lighter colors
  lighter_colors <- sapply(seq(0, 0.8, length.out = n), function(x) {
    rgb((1 - x) * rgb_color[1] + x * 1, (1 - x) * rgb_color[2] + x * 1, (1 - x) * rgb_color[3] + x * 1)
  })
  return(lighter_colors)
}

data <- data %>%
  mutate(shock_type = ifelse(grepl("wet|dry", shock_type, ignore.case = TRUE),
                             shock_type,
                             gsub("\\s*\\([^\\)]+\\)", "", shock_type))) %>% 
  mutate(shock_type = factor(shock_type, levels = unique(shock_type[order(shock_category)])))
# Create a color palette for each shock_type within each shock_category
shock_type_colors <- data %>%
  group_by(shock_category, shock_type) %>%
  summarise(count = n()) %>%
  group_by(shock_category) %>%
  mutate(shade = generate_shades(category_colors[shock_category], n())) %>%
  ungroup() %>%
  select(shock_category, shock_type, shade)

# Create a named vector for shock_type colors
shock_type_colors_vector <- setNames(shock_type_colors$shade, shock_type_colors$shock_type)
# Create a dummy dataframe for legend separators
legend_separators <- data.frame(
  shock_type = rep("", length(categories)),
  shock_category = categories,
  count = 0,
  year = 0
)

# Combine the original data with the dummy legend separators
data_with_separators <- bind_rows(data, legend_separators)

# Plot with custom colors
ggplot(data %>% filter(count > 0), aes(x = year, y = count, fill = shock_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~shock_category, scales = "free_y",ncol=2) +
  scale_fill_manual(values = shock_type_colors_vector) +
  labs(title = "Shock Counts by Year and Category",
       x = "Year", y = "Count", fill = "Shock Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = guide_legend(ncol = 1, override.aes = list(color = NA, size = 1)))+theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.text = element_text(size = 8),  # Adjust legend text size
    legend.key.size = unit(0.5, "lines")  # Adjust legend key size
  ) 

  #ggsave("Shock_Counts_by_Year_and_Category_2cols_size2.png",dpi = 300)
  #ggsave("Shock_Counts_by_Year_and_Category_2cols_size2.jpg",dpi = 300)



glimpse(data_bycat)









# Group by country and type of shock
grouped_data <- data %>%
  group_by(shock_category,year) %>%
  summarise(count = sum(count,na.rm=TRUE))

glimpse(grouped_data)
# Function to process and plot data for each shock category
process_and_plot <- function(data, category) {
  # Filter data for the current category
  cat_data <- data %>% filter(shock_category == category)
  
  # Linear interpolation of missing values (if any)
  cat_data$count <- na.approx(cat_data$count)
  data_1 <- cat_data %>% ungroup() %>% as.data.frame()
    mt <- lm(count~I(year-1970), data = data_1)
    t <- resid(mt)    
  
  # Apply Butterworth filter for different window sizes
  results <- list()
  windows <- c(3,5,8,10,15)
  for (w in windows) {
    
    
    filtered <- pass.filt(t,W=w, type="low", method="Butterworth")

    results[[paste(w, "years")]] <- filtered
  }
  
  # Prepare data frame for plotting
  tdf <- data.frame(year = cat_data$year, temp = t, Filter = "Unfiltered",cat=category,intercept=mt[1]$coefficients[1],slope=mt[1]$coefficients[2])
  for (wi in 1:length(results)) {
    w <- names(results)[wi]
    tdf <- rbind(tdf, data.frame(year = cat_data$year, temp = results[[w]]-max(t)*(wi^1.1), Filter = w,cat=category,intercept=mt[1]$coefficients[1],slope=mt[1]$coefficients[2]))
    #glimpse(tdf)
  }
  return(tdf)
}

# Apply the function to each shock category
unique_categories <- unique(grouped_data$shock_category)
i <- 1
for (cat in unique_categories) {
  if (i==1){
    filtered_shocks <-  process_and_plot(grouped_data, cat)
    i <- 2
  }else{
    filtered_shocks <- rbind(filtered_shocks, process_and_plot(grouped_data, cat))
  }
}

    glimpse(filtered_shocks)
    filtered_shocks <- filtered_shocks %>%
      mutate(cat = toTitleCase(tolower(cat))) %>% 
 mutate(Filter = factor(filtered_shocks$Filter, levels = c("Unfiltered", "3 years", "5 years", "8 years", "10 years", "15 years")))


  levels(factor(filtered_shocks$Filter))
            
  ggplot(filtered_shocks, aes(x = year, y = temp, color = Filter)) +
    geom_line() +
    facet_wrap(~cat,scales="free_y") +
    theme_bw() +
    labs(title = paste("Filtered Data for Types of Shocks"),
         x = "Year",
         y = "Count",
         color = "Filter") +
    theme_bw()+
            theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#f5f5f5"),
        strip.background = element_rect(fill = "#d6dbe0", color = "#d6dbe0"))  +
        scale_color_manual(values = c("Unfiltered" = "#6e6e6e",  # Dark gray for unfiltered
                                "3 years" = "#1b9e77",    # Teal
                                "5 years" = "#d95f02",    # Orange
                                "8 years" = "#7570b3",    # Purple
                                "10 years" = "#e7298a",   # Magenta
                                "15 years" = "#66a61e")) +  # Green
                                guides(color=guide_legend(position="right"))
                              

#ggsave("Filtered_Shocks_bottom.png", dpi = 300)

  ggplot(filtered_shocks)+
  geom_point(aes(x=intercept,y=slope))


### HeatMap
data %>%
  group_by(year, region, shock_category) %>%
  filter(!is.na(region)) %>%
  summarise(total_shocks = sum(count, na.rm = TRUE), .groups = 'drop') %>%
  # Calculate total shocks per region and year for normalization
  group_by(year, shock_category) %>%
  mutate(total_in_region = sum(total_shocks)) %>%
  ungroup() %>%
  # Calculate the proportion of each shock type
  mutate(proportion = total_shocks / total_in_region) %>%
  ggplot(aes(x = year, y = region, fill = proportion)) +
  geom_tile() +
  facet_grid(shock_category ~ ., scales = "free_y", space = "free_y") +  # Facet by shock type
  labs(title = "Heatmap of Shocks by Region and Category",
       x = "Year", y = "", fill = "Proportion of shocks \nper category per year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.y = element_text(angle = 0)) + # Rotate facet labels for better readability
    scale_fill_scico(palette = 'lapaz',direction=-1) +
    guides(fill = guide_legend(title = "Proportion of shocks \nper category per year",position="bottom",
                               title.position = "top", hjust=0.5))

#ggsave("Shocks_by_Region_and_Category.jpg", dpi = 300)

## Coocurrence through time (diferent categories)


## Coocurrence through time (diferent categories)


# Identify co-occurrences by year and country
glimpse(data)
co_occurrences <- data %>%
  #mutate(shock_type = sub(" ", "\n", shock_type)) %>% 
  group_by(country, year,region) %>%
  summarise(shock_types = list(shock_type), .groups = 'drop') %>%
  filter(lengths(shock_types) > 1) %>%
  mutate(pairs = map(shock_types, ~ combn(.x, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair = map_chr(pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  select(country, year, region, pair)

  glimpse(co_occurrences)

ggplot(co_occurrences %>% filter(!is.na(region)) %>% group_by(year,region) %>% summarise(count=n()), 
aes(x=year,y=count,color=region)) +
geom_point()+theme_bw()+geom_smooth(method="loess")

# Count total co-occurrences
total_co_occurrences <- co_occurrences %>%
  group_by(pair) %>%
  summarise(count = n(), .groups = 'drop')

# Split pairs into two columns for the circos plot
total_co_occurrences <- total_co_occurrences %>%
  separate(pair, into = c("shock_type1", "shock_type2"), sep = "-")

  total_co_occurrences %>% as.data.frame() %>% filter(shock_type1=="Air" | shock_type2=="Air")

shock_type_colors <- data %>% 
  #mutate(shock_type = sub(" ", "\n", shock_type)) %>% 
  group_by(shock_category, shock_type) %>%
  summarise(count = n()) %>%
  group_by(shock_category) %>%
  mutate(shade = generate_shades(category_colors[shock_category], n())) %>%
  ungroup() %>%
  select(shock_category, shock_type, shade)

# Create a named vector for shock_type colors
shock_type_colors_vector <- setNames(shock_type_colors$shade, shock_type_colors$shock_type)
# Create a named vector for shock_type colors
shock_type_colors_vector <- setNames(shock_type_colors$shade, shock_type_colors$shock_type)



#ggsave("Co-occurrence_of_Shocks_by_Region_and_Category.png", dpi = 300)
#write.csv(total_co_occurrences,"total_co_occurrences.csv")

dev.off()
circos.clear()  # Clear any existing plots
windows()
library(svglite)
svglite("co_occurrence_circos_plot4.svg", width = 20, height = 20)

circos.par(#"track.margin" = c(0.1, 0.1), "cell.padding" = c(0.02, 0.02, 0.02, 0.02),
           "gap.degree" = 4)  # Increase gap 

total_co_occurrences <- total_co_occurrences %>%
  mutate(shock_type1 = factor(shock_type1, levels = names(shock_type_colors_vector)),
         shock_type2 = factor(shock_type2, levels = names(shock_type_colors_vector))) %>%
  arrange(shock_type1, shock_type2)

chordDiagram(total_co_occurrences, 
             transparency = 0.5, 
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.1),
             grid.col = shock_type_colors_vector,  # Set sector colors
             link.lwd = 2,  # Set link line width
             link.border = shock_type_colors_vector)  # Set link border colors

# Add labels and colors
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sector.name = get.cell.meta.data("sector.index")
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name, 
              facing = "clockwise", 
              niceFacing = TRUE, adj = c(-0.1,0.5),cex=1.3)
  #circos.axis(h = "top", labels.cex = 0.6, major.tick.percentage = 0.2)
}, bg.border = NA)


# Add title
title("Annual Co-occurrence of Shocks Within Countries")
#dev.copy(png, filename = "co_occurrence_circos_plot_4.pdf", width = 2000, height = 2000, res = 300)
dev.off()  # Close the device















# Identify co-occurrences by year and country
#co_occurrences <- 
# Identify co-occurrences by year and country
co_occurrences <- data_master %>% filter(count>0) %>% 
  group_by(country, year, region) %>%
  summarise(shock_types = list(shock_type), .groups = 'drop') %>%
  filter(lengths(shock_types) > 1) %>%
  mutate(pairs = map(shock_types, ~ combn(.x, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair = map_chr(pairs, ~ paste(sort(.x), collapse = "-"))) %>%
  select(country, year, region, pair)

glimpse(co_occurrences)

co_occurrences %>% filter(year==1971,region=="REF")
data_master %>% filter(year==1971,region=="REF")
# Split pairs into two columns for the shock types
co_occurrences <- co_occurrences %>%
  separate(pair, into = c("shock_type1", "shock_type2"), sep = "-")

# Join the original data to get the shock categories for each shock type
data_shock_categories <- data_master %>% filter(count>0) %>%
  select(shock_type, shock_category) %>%
  distinct()

co_occurrences <- co_occurrences %>%
  left_join(data_shock_categories, by = c("shock_type1" = "shock_type")) %>%
  rename(shock_category1 = shock_category) %>%
  left_join(data_shock_categories, by = c("shock_type2" = "shock_type")) %>%
  rename(shock_category2 = shock_category)

glimpse(co_occurrences)
# Filter pairs to keep only those where the shock types are from different categories
co_occurrences_diff_cat <- co_occurrences %>%
  filter(shock_category1 != shock_category2) %>% 
  filter(!is.na(region))  %>% 
  group_by(year, region) %>% summarise(count = n())%>% ungroup()
glimpse(co_occurrences_diff_cat)

ts <- data_master %>% 
      group_by(year,region,country) %>% filter(count>0)%>% 
      summarise(count = n()) %>%
      summarise(total_shocks=count*(count-1)/2) %>% ungroup() %>% group_by(year,region) %>% 
      summarise(total_shocks=sum(total_shocks,na.rm=TRUE)) %>% ungroup()
      glimpse(ts)

dim_shocks <- data_master %>% group_by(shock_type) %>% summarise(n=n())%>% dim()
total_combinations <- dim_shocks[1]*(dim_shocks[1]-1)/2

data_master %>% filter(region=="REF",year==1971)%>% filter(count>0) %>%
      group_by(year,region,country) %>% 
      summarise(count = n()) %>%
      summarise(total_shocks=count*(count-1)/2) %>% ungroup() %>% group_by(year,region) %>% 
      summarise(total_shocks=sum(total_shocks,na.rm=TRUE)) %>% ungroup()
      glimpse(ts)
      


data_master %>% filter(region=="REF",year==1989) %>%
      group_by(year,region,country) %>% 
      summarise(total_shocks=count*(count-1)/2) %>% ungroup() 
      
# Calculate the count of pairs of shocks from different categories and normalize by the total number of shocks
normalized_counts_diff <- co_occurrences_diff_cat %>% ungroup() %>% 
  left_join(ts, by = c("year", "region")) %>%
  mutate(normalized_count = count / total_shocks) %>% 
  mutate(type="Different Shock Categories") %>% 
  as.data.frame()

normalized_counts_diff %>% as.data.frame()

normalized_counts <- co_occurrences %>%
  filter(!is.na(region)) %>% 
  group_by(year, region) %>% summarise(count = n())%>% 
  left_join(ts, by = c("year", "region")) %>%
  mutate(normalized_count = count / total_shocks) %>% 
  mutate(type="All Co-occurrences") %>% 
  as.data.frame()

coocur <- rbind(normalized_counts_diff,normalized_counts)

ggplot(coocur,# %>% filter(type=="Different Shock Categories"), 
  aes(x=year,y=100*count/total_combinations))+
geom_point(aes(size=total_shocks,color=region),alpha=0.4)+
facet_wrap(~type)+
theme_bw()+
ylab("Number of co-ocurrences") + 
labs(title="",
     x="Year",
     y="Co-ocurrences \n(% of Max)",
     color="Region",
     linetype="Type", size="Number of Shocks")+
     guides(linetype="none") + 
     scale_color_scico_d(palette = "batlow",begin=0.1,end=0.9,direction=-1) +
geom_hline(yintercept=100,linetype="dashed")+
geom_text(data=data.frame(x=1985,y=97),
  aes(x=x,y=y,label="Maximum theoretical co-ocurrences"),size=2.4)
  ggsave("Co-ocurrences_of_shocks_by_region_and_type.png", dpi = 300)

