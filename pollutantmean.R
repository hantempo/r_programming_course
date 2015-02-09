pollutantmean <- function(directory, pollutant, id = 1:332) {
  values = vector()
  for (index in id) {
    tables <- read.csv(sprintf("%s/%03d.csv", directory, index))
    values <- c(values, tables[[pollutant]])
  }  
  mean(values, na.rm = TRUE)
}