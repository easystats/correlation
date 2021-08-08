#' Visualisation Recipe for 'correlation' Objects
#'
#' Visualisation recipe for 'correlation' objects.
#'
#' @param x A correlation object.
#' @param tile,scale_fill,labs Additional aesthetics and parameters for the geoms (see customization example).
#' @param ... Other arguments passed to other functions.
#'
#' @examples
#' # ==============================================
#' # Correlation Matrix
#' # ==============================================
#' if (require("ggplot2")) {
#' rez <- correlation(mtcars)
#'
#' x <- cor_sort(as.matrix(rez))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize
#' x <- summary(rez)
#' layers <- visualisation_recipe(x,
#'                                scale_fill = list(high = "green", low = "red"),
#'                                labs = list(title = "My Plot"))
#' plot(layers) + theme_minimal()
#' }
#' @export
visualisation_recipe.easycormatrix <- function(x,
                                               tile = NULL,
                                               scale_fill = NULL,
                                               labs = NULL,
                                               ...) {

  # Format as summary() if true matrix
  if(inherits(x, "matrix")) {
    x <- cbind(data.frame(Parameter1 = row.names(x)), as.data.frame(x))
  } else {
    names(x)[names(x) == "Parameter"] <- "Parameter1"
    x <- as.data.frame(x)
  }
  colnames <- names(x)[names(x) != "Parameter1"]

  # Reshape to long
  data <- datawizard::reshape_longer(x,
                                     cols = colnames,
                                     colnames_to = "Parameter2",
                                     values_to = "r")
  data <- data[!is.na(data$r), ] # filter NAns
  data$Parameter1 <- factor(data$Parameter1, levels = rev(x$Parameter1))
  data$Parameter2 <- factor(data$Parameter2, levels = colnames)

  # TODO: add other columns with more info from attributes


  # Initialize layers list
  layers <- list()

  # Layers -----------------------
  l <- 1

  # Add tiles
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_tile(data, x = "Parameter2", y = "Parameter1", fill = "r", tile = tile)
  l <- l + 1

  # Color tiles
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale_fill(scale_fill = scale_fill)
  l <- l + 1

  # Origin at 0
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale(which = "x", ...)
  l <- l + 1
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale(which = "y", ...)
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_labs(labs)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers

}



# Layer - Ribbon -------------------------------------------------------------

.visualisation_easycormatrix_tile <- function(data, x, y, fill, tile = NULL) {
  out <- list(
    geom = "tile",
    data = data,
    aes = list(
      y = y,
      x = x,
      fill = fill
    )
  )
  if (!is.null(tile)) out <- utils::modifyList(out, tile) # Update with additional args
  out
}



# Layer - Scale Fill -------------------------------------------------------------

.visualisation_easycormatrix_scale_fill <- function(scale_fill = NULL) {
  out <- list(
    geom = "scale_fill_gradient2",
    low = "#2196F3",
    mid = "white",
    high = "#F44336",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name="Correlation"
  )
  if (!is.null(scale_fill)) out <- utils::modifyList(out, scale_fill) # Update with additional args
  out
}


# Layer - Scale -------------------------------------------------------------------


.visualisation_easycormatrix_scale <- function(which = "x", scale = NULL) {
  out <- list(
    geom = paste0("scale_", which, "_discrete"),
    expand = c(0, 0)
  )
  if (!is.null(scale)) out <- utils::modifyList(out, scale) # Update with additional args
  out
}


# Layer - Labs -------------------------------------------------------------------


.visualisation_easycormatrix_labs <- function(labs = NULL) {
  out <- list(
    geom = "labs",
    x = NULL,
    y = NULL,
    title = "Correlation Matrix"
  )
  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}