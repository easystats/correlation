#' Visualisation Recipe for 'correlation' Objects
#'
#' Visualisation recipe for 'correlation' objects.
#'
#' @param x A correlation object.
#' @param show_text Show labels with matrix values.
#' @param show_data Show data. For correlation matrices, can be \code{"tile"} (default) or \code{"point"}.
#' @param tile,point,text,scale_fill,smooth,labs Additional aesthetics and parameters for the geoms (see customization example).
#' @param ... Other arguments passed to other functions.
#'
#' @examples
#' # ==============================================
#' # Correlation Matrix
#' # ==============================================
#' if (require("see")) {
#' rez <- correlation(mtcars)
#'
#' x <- cor_sort(as.matrix(rez))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' #' Get more details using `summary()`
#' x <- summary(rez, redundant = TRUE, digits = 3)
#' plot(visualisation_recipe(x))
#'
#' # Customize
#' x <- summary(rez)
#' layers <- visualisation_recipe(x,
#'                                show_data = "points",
#'                                scale = list(range = c(10, 20)),
#'                                scale_fill = list(high = "#FF5722",
#'                                                  low = "#673AB7",
#'                                                  name = "r"),
#'                                text = list(color = "white"),
#'                                labs = list(title = "My Plot"))
#' plot(layers) + theme_modern()
#' }
#' @export
visualisation_recipe.easycormatrix <- function(x,
                                               show_data = "tile",
                                               show_text = "text",
                                               tile = NULL,
                                               point = NULL,
                                               text = NULL,
                                               scale_fill = NULL,
                                               labs = NULL,
                                               ...) {

  # Format as summary() if true matrix
  if(inherits(x, "matrix")) {
    data_text <- NULL
    x <- cbind(data.frame(Parameter1 = row.names(x)), as.data.frame(x))
  } else {
    data_text <- format(x, ...)
    names(data_text)[names(data_text) == "Parameter"] <- "Parameter1"
    names(x)[names(x) == "Parameter"] <- "Parameter1"
    x <- as.data.frame(x)
  }
  colnames <- names(x)[names(x) != "Parameter1"]

  # Reshape to long
  data <- datawizard::reshape_longer(x,
                                     cols = colnames,
                                     colnames_to = "Parameter2",
                                     values_to = "r")

  # Text
  if(is.null(data_text)) {
    data$Text <- paste0(insight::format_value(data$r, zap_small = TRUE))
  } else {
    temp <- datawizard::reshape_longer(data_text,
                                       cols = colnames,
                                       colnames_to = "Parameter2",
                                       values_to = "Text")
    data <- merge(data, temp, all.x = TRUE, all.y = FALSE)
  }

  # Format
  data$Parameter1 <- factor(data$Parameter1, levels = rev(x$Parameter1))
  data$Parameter2 <- factor(data$Parameter2, levels = colnames)
  # filter NAns
  data <- data[!is.na(data$r), ]


  # Initialize layers list
  layers <- list()

  # Layers -----------------------
  l <- 1

  # Add tiles
  if(!is.null(show_data)) {
    if(show_data == TRUE || show_data %in% c("tile", "tiles")) {
      layers[[paste0("l", l)]] <- .visualisation_easycormatrix_data(type = "tile", data, x = "Parameter2", y = "Parameter1", fill = "r", args = tile)
    } else {
      layers[[paste0("l", l)]] <- .visualisation_easycormatrix_data(type = "point", data, x = "Parameter2", y = "Parameter1", fill = "r", args = point)
    }
    l <- l + 1
  }



  # Add text
  if(!is.null(show_text) && show_text != FALSE) {
    if(show_text == TRUE) show_text <- "text"
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_text(data, x = "Parameter2", y = "Parameter1", label = "Text", text = text)
    l <- l + 1
  }

  # Color tiles
  if(!is.null(show_data) && show_data %in% c("tile", "tiles")) {
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale_fill(type = "fill", data, scale_fill = scale_fill)
    l <- l + 1
  } else if (show_data %in% c("point", "points")){
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale_fill(type = "colour", data, scale_fill = scale_fill)
    l <- l + 1
  }

  # Origin at 0
  if(!is.null(show_data) && show_data %in% c("tile", "tiles")) {
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale(which = "x_discrete", ...)
    l <- l + 1
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale(which = "y_discrete", ...)
    l <- l + 1
  } else if (show_data %in% c("point", "points")){
    layers[[paste0("l", l)]] <- .visualisation_easycormatrix_scale(which = "size", ...)
    l <- l + 1
  }

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_easycormatrix_labs(labs)

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers

}



# Layer - Data -------------------------------------------------------------

.visualisation_easycormatrix_data <- function(type = "tile", data, x, y, fill, args = NULL) {
  out <- list(
    geom = type,
    data = data,
    aes = list(
      y = y,
      x = x
    )
  )
  if(type == "point") {
    out$aes$colour <- fill
    out$aes$size <- paste0("abs(", fill, ")")
    out$stroke = 0
    out$shape = 16
    out$show.legend <- c("size" = FALSE)
  } else {
    out$aes$fill <- fill
  }
  if (!is.null(args)) out <- utils::modifyList(out, args) # Update with additional args
  out
}



# Layer - Scale Fill -------------------------------------------------------------

.visualisation_easycormatrix_scale_fill <- function(type = "fill", data, scale_fill = NULL) {

  low_lim <- ifelse(min(data$r) < 0, -1, 0)
  high_lim <- ifelse(max(data$r) > 0, 1, 0)

  out <- list(
    geom = paste0("scale_", type, "_gradient2"),
    low = "#2196F3",
    mid = "white",
    high = "#F44336",
    midpoint = 0,
    limit = c(low_lim, high_lim),
    space = "Lab",
    name="Correlation"
  )
  if (!is.null(scale_fill)) out <- utils::modifyList(out, scale_fill) # Update with additional args
  out
}


# Layer - Text -------------------------------------------------------------

.visualisation_easycormatrix_text <- function(data, x, y, label, show_text = "text", text = NULL) {
  out <- list(
    geom = show_text,
    data = data,
    aes = list(
      y = y,
      x = x,
      label = label
    )
  )
  if (!is.null(text)) out <- utils::modifyList(out, text) # Update with additional args
  out
}


# Layer - Scale -------------------------------------------------------------------


.visualisation_easycormatrix_scale <- function(which = "x_discrete", scale = NULL) {
  if(which == "size") {
    out <- list(
      geom = "scale_size",
      range = c(5, 15)
    )
  } else {
    out <- list(
      geom = paste0("scale_", which),
      expand = c(0, 0)
    )
  }

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