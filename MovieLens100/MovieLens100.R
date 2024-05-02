library(reshape2)

# import dataset
df <- read.table("MovieLens100.data")
colnames(df) <- c("user_id", "item_id", "rating", "timestamp")

# reshape into dataframe where cols are users and rows items
df <- dcast(df, user_id ~ item_id, value.var = "rating")

# Remove the first column as they are user id's
df <- df[, -1]
save(df, file="MovieLens100.Rdata")
