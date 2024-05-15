library(tidyverse)
library(ggpubr)
library(ggthemes)

# Import dataset
dataset = "MovieLens25M"
datafile = sprintf("%s/%s.RData", dataset, dataset)
load(datafile)

df <- data.table::data.table(df)

items = as.double(length(unique(df$item_id)))
users = as.double(length(unique(df$user_id)))
total_obs = users*items
density = round((nrow(df))/total_obs, 3)

# range assumes each extreme rating is given at least once
range = sprintf("[%.2f - %.2f]", min(df$rating, na.rm = T), max(df$rating, na.rm = T))

informationDf <- data.frame(category = factor(c("Cells", "Users", "Items", "Density", "Range")),
                            identity = c(total_obs, users, items, density, range))
# Put the observed values into a vector
ratings_vector <- data.table::data.table(na.omit(as.vector(as.matrix(df$rating))))

# Plot the frequency of ratings
ratings_plot <- ggplot(ratings_vector, aes(x = V1, fill = factor(V1))) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
  theme_minimal()

filepng = sprintf("%s/%s%s.png", dataset, dataset, "ratings")
ggsave(filepng, width = 3, height = 4)

# Boxplots of average rating per user and per item

user_mean <- data.frame(df %>% group_by(user_id) %>% summarise(mean = mean(rating)))
user_mean <- user_mean[2]
colnames(user_mean) <- c("averageRating")
item_mean <- data.frame(df %>% group_by(item_id) %>% summarise(mean = mean(rating)))
item_mean <- item_mean[2]
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

user_sd <- data.frame(df %>% group_by(user_id) %>% summarise(sigma = sd(rating)))
user_sd <- user_sd[2]

item_sd <-data.frame(df %>% group_by(item_id) %>% summarise(sigma = sd(rating)))
item_sd <- item_sd[2]

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

# Save the descriptive table
filename = sprintf("%s/%sinfo.RData", dataset, dataset)
save(informationDf, file = filename)


# Dont think this works in this format
#missingness_plot <- vis_miss(df, warn_large_data = F)
#filepng = sprintf("%s/%s%s.png", dataset, dataset, "missingness")
#ggsave(filepng, width = 3, height = 4)

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



