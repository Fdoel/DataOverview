library(tidyverse)
library(naniar)
library(ggpubr)
library(ggthemes)

# Import dataset
dataset = "movielens100"
datafile = sprintf("%s.RData", dataset)
load(datafile)

items = ncol(df)
users = nrow(df)
total_obs = users*items
density = round((total_obs - sum(is.na(df)))/total_obs, 3)

# range assumes each extreme rating is given at least once
range = sprintf("[%.2f - %.2f]", min(df, na.rm = T), max(df, na.rm = T))

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

# Boxplots of average rating per user and per item

user_mean <- data.frame(rowMeans(df, na.rm = T))
colnames(user_mean) <- c("averageRating")
item_mean <- data.frame(colMeans(df, na.rm = T))
colnames(item_mean) <- c("averageRating")

user_mean_plot <- ggplot(user_mean) +
  geom_boxplot(aes(y = averageRating)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("rating") +
  ggtitle("Average rating given per user") +
  theme_cleveland()

item_mean_plot <- ggplot(item_mean) +
  geom_boxplot(aes(y = averageRating)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("rating") +
  ggtitle("Average rating given per item") +
  theme_cleveland()

# Boxplot standard deviation per user and per item

user_sd <- data.frame(apply(df, 1 , function(x) sd(x, na.rm = TRUE)))
colnames(user_sd) <- c("sigma")

item_sd <- data.frame(apply(df, 2 , function(x) sd(x, na.rm = TRUE)))
colnames(item_sd) <- c("sigma")

user_sd_plot <- ggplot(user_sd) +
  geom_boxplot(aes(y = sigma)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Standard deviation") +
  ggtitle("Standard deviation of rating per user") +
  theme_cleveland()

item_sd_plot <- ggplot(item_sd) +
  geom_boxplot(aes(y = sigma)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Standard deviation") +
  ggtitle("Standard deviation of rating per item") +
  theme_cleveland()

missingness_plot <- vis_miss(df, warn_large_data = F)

# Combine plots
summary_plot <- ggarrange(ratings_plot, missingness_plot,
                                          user_mean_plot, item_mean_plot,
                                          user_sd_plot, item_sd_plot,
                                          ncol = 2, nrow = 3)

# Save the plot and table
filename = sprintf("%svisuals.Rdata", dataset)
save(informationDf, summary_plot, file= filename)
