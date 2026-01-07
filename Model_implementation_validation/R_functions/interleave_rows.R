interleave_rows <- function(datasets, chunk_size) {
  n_chunks <- nrow(datasets[[1]]) / chunk_size
  interleaved <- do.call(rbind, lapply(1:n_chunks, function(i) {
    do.call(rbind, lapply(datasets, function(dataset) {
      dataset[((i - 1) * chunk_size + 1):(i * chunk_size), ]
    }))
  }))
  return(interleaved)
}

