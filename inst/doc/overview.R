## -----------------------------------------------------------------------------
library(babynames)
library(fozziejoin)
library(tibble)

# Seed for reproducibility
set.seed(1337)

# Restrict to names from years 2000 or later
babynames <- babynames[babynames$year >= 2000, ]

# Sample rows from babynames dataset
sample_df <- babynames[sample(nrow(babynames), 10), 'name']

# Mutate a single character in the 'name' field for sample
mutate_char <- function(x) {
  if (nchar(x) == 0) return(x)
  pos <- sample(1:nchar(x), 1)
  new_char <- sample(letters, 1)
  substr(x, pos, pos) <- new_char
  return(x)
}
sample_df$name <- sapply(sample_df$name, mutate_char)

## -----------------------------------------------------------------------------
fozzie <- fozzie_string_join(
    babynames, sample_df, how='inner', method='jaccard', q=3,
    by = c('name')
)
print(head(fozzie))
print(nrow(fozzie))

## -----------------------------------------------------------------------------
# If both neither input is `tibble`, `data.frame` is returned.
fozzie_df <- fozzie_string_join(
    as.data.frame(babynames),
    as.data.frame(sample_df),
    how='inner',
    method='jaccard',
    q=3,
    by = c('name')
)
head(fozzie_df)

## -----------------------------------------------------------------------------
# Simulate data
size <- 1000
df1 <- tibble(
  x = round(runif(size, min = 0, max = 100), 2),
  y = round(runif(size, min = 0, max = 100), 2)
)
df2 <- tibble(
  x = round(runif(size, min = 0, max = 100), 2),
  y = round(runif(size, min = 0, max = 100), 2)
)

## -----------------------------------------------------------------------------
# Absolute difference join (per column)
diff_join <- fozzie_difference_join(
  df1, df2, max_distance=1, distance_col = 'diff'
)
print(head(diff_join))

# Manhattan distance join (across all columns)
dist_join <- fozzie_distance_join(
  df1, df2, method='manhattan', max_distance=1, distance_col='dist'
)
print(head(dist_join))

## -----------------------------------------------------------------------------
size <- 1000

# Simulate left data
starts1 <- runif(size, min = 0, max = 500)
ends1 <- starts1 + runif(size, min = 0, max = 10)
df1 <- tibble(start = starts1, end = ends1)

# Simulate right data
starts2 <- runif(size, min = 0, max = 500)
ends2 <- starts2 + runif(size, min = 0, max = 10)
df2 <- tibble(start = starts2, end = ends2)

# Perform interval join using real-valued ranges
real_olaps <- fozzie_interval_join(
  df1, df2,
  by = c(start = "start", end = "end"),
  how = "inner",
  overlap_type = "any",
  maxgap = 0,
  minoverlap = 0,
  interval_mode = "real"
)

## -----------------------------------------------------------------------------
df1 <- data.frame(time = as.POSIXct(c(
  "2023-01-01 12:00:00", "2023-01-01 13:00:00"
)))
df2 <- data.frame(time = as.POSIXct(c(
  "2023-01-01 12:00:05", "2023-01-01 14:00:00"
)))

result <- fozzie_temporal_inner_join(
  df1, df2, by = c("time"), max_distance = 10, unit = "seconds"
)
print(head(result))

## ----error=TRUE---------------------------------------------------------------
try({
# An error results if matching on `Date` with unit other than `days`
df1$date <- as.Date(df1$time)
df2$date <- as.Date(df2$time)
result <- fozzie_temporal_inner_join(
  df1, df2, by = c("date"), max_distance = 10, unit = "seconds"
)
})

## -----------------------------------------------------------------------------
# Succeeds
result <- fozzie_temporal_inner_join(
  df1, df2, by = c("date"), max_distance = 10
)

## -----------------------------------------------------------------------------
df1 <- data.frame(
  start = as.Date(c("2023-01-01", "2023-01-05")),
  end = as.Date(c("2023-01-03", "2023-01-07"))
)
df2 <- data.frame(
  start = as.Date(c("2023-01-02", "2023-01-06")),
  end = as.Date(c("2023-01-04", "2023-01-08"))
)

result <- fozzie_temporal_interval_inner_join(
  df1, df2,
  by = c(start = "start", end = "end"),
  overlap_type = "any",
  maxgap = 0,
  minoverlap = 0,
  unit = "days"
)

head(result)

