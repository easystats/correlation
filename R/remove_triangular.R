#' Remove redundant correlations
#'
#' @param cor \link{correlation} object.
#' @param which_column Which column to use for the filtering.
#' @importFrom utils combn
#' @export
remove_triangular <- function(cor, which_column = NULL){

  if(is.null(which_column)){
    if("r" %in% names(cor)){
      which_column <- "r"
    } else if("Median" %in% names(cor)){
      which_column <- "Median"
    } else{
      stop("Please specify `which_column`.")
    }
  }

  m <- as.table(cor, which_column = which_column)

  # Filter parameter1
  x <- names(m)[!names(m) %in% c("Group", "Parameter")]
  cor <- cor[cor$Parameter1 %in% x,]
  cor$Parameter1 <- factor(cor$Parameter1, levels=x)

  # Filter parameter2
  y <- m$Parameter
  cor <- cor[cor$Parameter2 %in% y,]
  cor$Parameter2 <- factor(cor$Parameter2, levels=rev(y))

  # Remove NANs
  cor <- cor[as.character(cor$Parameter1) != as.character(cor$Parameter2), ]
  for(i in 1:nrow(cor)){
    current_row <- cor[i, ]
    cor[i, which_column] <- m[m$Parameter == current_row$Parameter2, names(m)==current_row$Parameter1]
  }

  cor <- cor[!is.na(cor[[which_column]]),]

  cor
}

