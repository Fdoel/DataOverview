library(reshape2)

# import dataset
df <- readxl::read_xls("jester.xls", col_names = F)

# first row is number of items rated, remove that row.
df <- df[-1]

df[] <- lapply(df, function(x) ifelse(x == 99.0, NA, x))

# Save the dataframe as Rdata file
save(df, file="Jester.Rdata")
