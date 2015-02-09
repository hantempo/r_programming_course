complete <- function(directory, id = 1:332) {
  len = length(id)
  ids = vector("integer", len)
  nobs = vector("integer", len)
  for (number in seq_along(id)) {
    obid <- id[number]
    filename <- sprintf("%s/%03d.csv", directory, obid)
    table <- read.csv(filename)
    ids[number] <- obid
    nobs[number] <- sum(!is.na(table$sulfate) & !is.na(table$nitrate))
  }
  data.frame(id=ids, nobs=nobs)
}