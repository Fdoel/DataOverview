library(tidyverse)
library(naniar)
library(ggpubr)
library(ggthemes)

# Import dataset
dataset = "Jester"
datafile = sprintf("%s.RData", dataset)
load(datafile)

users = ncol(df)
items = nrow(df)
total_obs = users*items
density = round((total_obs - sum(is.na(df)))/total_obs, 3)

# range assumes each extreme rating is given at least once
range = sprintf("[%f2 - %f2]", min(df, na.rm = T), max(df, na.rm = T))

informationDf <- data.frame(category = factor(c("Cells", "Users", "Items", "Density", "Range")),
                            identity = c(total_obs, users, items, density, range))
# Put the observed values into a vector
ratings_vector <- na.omit(as.vector(as.matrix(df)))


# Plot the frequency of ratings
ratings_plot <- ggplot() +
  geom_bar(aes(x = factor(ratings_vector), fill =  factor(ratings_vector)),
           stat = "count", show.legend = F) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
  theme_cleveland()

missingness_plot <- vis_miss(df, warn_large_data = F)

# Combine plots
summary_plot <- ggarrange(ratings_plot, missingness_plot, ncol = 2, nrow = 1)

# Save the plot and table
filename = sprintf("%svisuals.Rdata", dataset)
save(informationDf, summary_plot, file= filename)
