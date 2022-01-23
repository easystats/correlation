#' @rdname visualisation_recipe.easycormatrix
#'
#' @examples
#' \donttest{
#' # ==============================================
#' # Correlation Test
#' # ==============================================
#'   if (require("see")) {
#'     rez <- cor_test(mtcars, "mpg", "wt")
#'
#'     layers <- visualisation_recipe(rez, labs = list(x = "Miles per Gallon (mpg)"))
#'     layers
#'     plot(layers)
#'
#'     plot(rez,
#'       show_text = "label",
#'       point = list(color = "#f44336"),
#'       text = list(fontface = "bold"),
#'       show_statistic = FALSE, show_ci = FALSE, stars = TRUE
#'     )
#'   }
#' }
#' @export
visualisation_recipe.easycor_test <- function(x,
                                              show_data = "point",
                                              show_text = "subtitle",
                                              smooth = NULL,
                                              point = NULL,
                                              text = NULL,
                                              labs = NULL,
                                              ...) {
  data <- attributes(x)$data

  # Text
  subtitle <- NULL
  title <- NULL
  if (!is.null(show_text) && show_text == "subtitle") subtitle <- cor_text(x, ...)
  if (!is.null(show_text) && show_text == "title") title <- cor_text(x, ...)


  # Get scatter plot
  layers <- .see_scatter(data,
    cor_results = x,
    x = x$Parameter1,
    y = x$Parameter2,
    show_data = show_data,
    show_text = show_text,
    smooth = smooth,
    point = point,
    text = text,
    labs = labs,
    title = title,
    subtitle = subtitle,
    ...
  )

  # Text
  if (!is.null(show_text) && show_text != FALSE && show_text %in% c("text", "label")) {
    # Add text
    x$label <- cor_text(x, ...)
    x$label_x <- max(data[[x$Parameter1]], na.rm = TRUE)
    x$label_y <- max(data[[x$Parameter2]], na.rm = TRUE) + 0.05 * diff(range(data[[x$Parameter2]], na.rm = TRUE))

    l <- paste0("l", length(layers) + 1)
    layers[[l]] <- list(
      geom = show_text,
      data = x,
      hjust = 1,
      aes = list(
        label = "label",
        x = "label_x",
        y = "label_y"
      )
    )
    if (!is.null(text)) layers[[l]] <- utils::modifyList(layers[[l]], text)
  }

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}




# see_scatter -------------------------------------------------------------


.see_scatter <- function(data,
                         cor_results,
                         x,
                         y,
                         show_data = "point",
                         show_text = "text",
                         smooth = NULL,
                         point = NULL,
                         text = NULL,
                         labs = NULL,
                         title = NULL,
                         subtitle = NULL,
                         ...) {


  # Initialize layers list
  layers <- list()

  # Layers -----------------------
  l <- 1

  # Smooth
  layers[[paste0("l", l)]] <- list(
    geom = "smooth",
    data = data,
    method = "lm",
    aes = list(
      x = x,
      y = y
    )
  )
  if (!is.null(smooth)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], smooth)
  }
  l <- l + 1

  # Point
  layers[[paste0("l", l)]] <- list(
    geom = show_data,
    data = data,
    aes = list(
      x = x,
      y = y
    )
  )
  if (!is.null(point)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], point)
  }
  l <- l + 1

  # Side density
  # TODO: fix once https://github.com/jtlandis/ggside/issues/24
  # insight::check_if_installed("ggside")
  # layers[[paste0("l", l)]] <- list(geom = "ggside::geom_xsidedensity",
  #                                  data = data,
  #                                  aes = list(x = x)
  # )
  # l <- l + 1
  # layers[[paste0("l", l)]] <- list(geom = "ggside::geom_ysidedensity",
  #                                  data = data,
  #                                  aes = list(x = x)
  # )
  # l <- l + 1

  # layers[[paste0("l", l)]] <- list(geom = "ggside::scale_xsidey_continuous", breaks = NULL)
  # l <- l + 1
  # layers[[paste0("l", l)]] <- list(geom = "ggside::scale_ysidex_continuous", breaks = NULL)
  # l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- list(geom = "labs", subtitle = subtitle, title = title)
  if (!is.null(labs)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], labs)
  }

  layers
}
