library(tidyverse)
library(naniar)
library(ggpubr)
library(ggthemes)
library("fsttable")

# Import dataset
dataset = "personality"
datafile = sprintf("%s/%s.RData", dataset, dataset)
load(datafile)

df <- data.table::data.table(df)

items = ncol(df)
users = nrow(df)
total_obs = users*items
density = round((total_obs - sum(is.na(df)))/total_obs, 3)

# range assumes each extreme rating is given at least once
range = sprintf("[%.2f - %.2f]", min(df, na.rm = T), max(df, na.rm = T))

informationDf <- data.frame(category = factor(c("Cells", "Users", "Items", "Density", "Range")),
                            identity = c(total_obs, users, items, density, range))
# Put the observed values into a vector
ratings_vector <- data.table::data.table(na.omit(as.vector(as.matrix(df))))

# Plot the frequency of ratings
ratings_plot <- ggplot(ratings_vector, aes(x = V1, fill = factor(V1))) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
  theme_minimal()

filepng = sprintf("%s/%s%s.png", dataset, dataset, "ratings")
ggsave(filepng, width = 3, height = 4)

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

filepng = sprintf("%s/%s%s.png", dataset, dataset, "user_mean_plot")
ggsave(filepng, width = 3, height = 4)

item_mean_plot <- ggplot(item_mean) +
  geom_boxplot(aes(y = averageRating)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("rating") +
  ggtitle("Average rating given per item") +
  theme_cleveland()

filepng = sprintf("%s/%s%s.png", dataset, dataset, "item_mean_plot")
ggsave(filepng, width = 3, height = 4)

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

filepng = sprintf("%s/%s%s.png", dataset, dataset, "user_sd_plot")
ggsave(filepng, width = 3, height = 4)

item_sd_plot <- ggplot(item_sd) +
  geom_boxplot(aes(y = sigma)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Standard deviation") +
  ggtitle("Standard deviation of rating per item") +
  theme_cleveland()

filepng = sprintf("%s/%s%s.png", dataset, dataset, "item_sd_plot")
ggsave(filepng, width = 3, height = 4)

missingness_plot <- vis_miss(df, warn_large_data = F)
filepng = sprintf("%s/%s%s.png", dataset, dataset, "missingness")
ggsave(filepng, width = 3, height = 4)

# Combine plots
# Do not do this for big data as its very slow
#summary_plot <- ggarrange(ratings_plot, missingness_plot,
#                                          user_mean_plot, item_mean_plot,
#                                          user_sd_plot, item_sd_plot,
#                                          ncol = 2, nrow = 3)
#
# Save the plot and table
#filename = sprintf("%s/%svisuals.Rdata", dataset, dataset)
#filepng = sprintf("%s/%s.png", dataset, dataset)



