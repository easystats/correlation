#' @keywords internal
.clean_data <- function(data, include_factors = TRUE, random = FALSE){
  if(random == FALSE){
    if(include_factors){
      data <- parameters::convert_data_to_numeric(data)
    } else{
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}