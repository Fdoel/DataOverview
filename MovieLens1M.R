library(reshape2)

# import dataset
df <- read.table("movielens1M.dat", sep = ":")
df <- df[c(1, 3, 5)]
colnames(df) <- c("user_id", "item_id", "rating")

# reshape into dataframe where cols are users and rows items
df <- dcast(df, user_id ~ item_id, value.var = "rating")
df <- df[-1]
# Save the dataframe as Rdata file
save(df, file="MovieLens1M.Rdata")

