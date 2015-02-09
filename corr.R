corr <- function(directory, threshold = 0) {
  corrs = vector("numeric")
  com_counts <- complete(directory)
  for (num in 1:nrow(com_counts)) {
    c <- com_counts$nobs[num]
    if (c > threshold) {
      obid <- com_counts$id[num]
      filename <- sprintf("%s/%03d.csv", directory, obid)
      table <- read.csv(filename)
      corrs = c(corrs, cor(table$sulfate, table$nitrate, use="complete.obs"))
    }    
  }
  corrs
}