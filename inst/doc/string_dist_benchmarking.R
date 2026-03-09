## ----message = FALSE----------------------------------------------------------
library(microbenchmark)
library(fuzzyjoin)
library(fozziejoin)
library(qdapDictionaries)
library(tibble)
library(dplyr)

nsamp <- 100
seed <- 2016

params <- list(
  list(method = "osa", mode = "inner", max_dist = 1, q = 0),
  list(method = "jaccard", mode = "inner", max_dist = 0.5, q = 2),
  list(method = "lv", mode = "inner", max_dist = 1)
)

data(misspellings)
sub_misspellings <- misspellings[sample(seq_len(nrow(misspellings)), nsamp), ]

words <- as.data.frame(DICTIONARY)
results <- data.frame()

## -----------------------------------------------------------------------------
for (p in params) {
  set.seed(seed)

  bench <- microbenchmark(
    fuzzy = fuzzy <- stringdist_join(
      sub_misspellings, words,
      by = c(misspelling = "word"),
      method = p$method, mode = p$mode,
      max_dist = p$max_dist, q = p$q
    ),
    fozzie = fozzie <- fozzie_string_join(
      sub_misspellings, words,
      by = c(misspelling = "word"),
      method = p$method, how = p$mode,
      max_distance = p$max_dist, q = p$q
    ),
    times = 10
  )

  # Check for equal output
  if (!isTRUE(all.equal(fuzzy, fozzie))) {
    message("Mismatch detected for method: ", p$method)
  }

  # Convert benchmark results to df, append to running list
  df <- as.data.frame(bench)
  df$method <- p$method
  df$n_comps <- nrow(sub_misspellings) * nrow(words)
  df$os <- Sys.info()["sysname"]
  results <- rbind(results, df)
}

## -----------------------------------------------------------------------------
summary_stats <- aggregate(
  time ~ expr + method + n_comps,
  data = results,
  FUN = function(x) mean(x)
)

summary_df <- data.frame(
  expr = summary_stats$expr,
  method = summary_stats$method,
  n_comps = summary_stats$n_comps,
  mean_time = summary_stats$time / 1e6
)

wide_df <- reshape(
  summary_df,
  idvar = c("n_comps", "method"),
  timevar = "expr",
  direction = "wide"
)

wide_df$mean_ratio <- wide_df$mean_time.fuzzy / wide_df$mean_time.fozzie

clean_df <- tibble(wide_df[order(wide_df$method), c(
  "method", "n_comps", "mean_time.fuzzy", "mean_time.fozzie", "mean_ratio"
)])

print(clean_df)

## -----------------------------------------------------------------------------
fozzie_cos <- fozzie_string_join(
  sub_misspellings, words, 
  by = c(misspelling = 'word'), 
  method='cosine', q = 3, max_distance = 0.5,
  distance_col = 'dist'
)
fuzzy_cos <- stringdist_join(
  sub_misspellings, words,
  by = c(misspelling = 'word'),
  method = 'cosine', q = 3, max_dist = 0.5,
  distance_col = 'dist'
)

## -----------------------------------------------------------------------------
joinby <- c('misspelling', 'correct', 'word')
only_fozzie <- anti_join(fozzie_cos, fuzzy_cos, by = joinby)
print(table(only_fozzie$dist))

only_fuzzy <- anti_join(fuzzy_cos, fozzie_cos, by = joinby)
print(paste("Fozzie contains all fuzzy rows:", nrow(only_fuzzy) == 0))

