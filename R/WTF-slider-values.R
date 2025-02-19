make_slider_values <- function(labels) {
  values <- tolower(gsub("[^[:alnum:]]", "_", labels))
  values <- gsub("_{2,}", "_", values)  # Replace multiple underscores with single
  values <- gsub("^_|_$", "", values)   # Remove leading/trailing underscores
  names(values) <- labels
  return(values)
}


ob <- make_slider_values(c(5:10))
names(ob)
as.numeric(ob)





tts <- c('slider_int')

gsub('^.*_', '', tts)
