#' @rdname visualisation_recipe.easycormatrix
#'
#' @examples
#' # ==============================================
#' # Correlation Results (easycorrelation)
#' # ==============================================
#' if (require("see") && require("tidygraph") && require("ggraph")) {
#'   rez <- correlation(iris)
#'
#'   layers <- visualisation_recipe(rez)
#'   layers
#'   plot(layers)
#' }
#' @export
visualisation_recipe.easycorrelation <- function(x, ...) {
  insight::check_if_installed("tidygraph")

  x$width <- abs(x$r)
  data <- tidygraph::as_tbl_graph(x)

  # Initialize layers list
  layers <- list()

  layers[["l1"]] <- list(
    geom = "ggraph::geom_edge_arc",
    strength = 0.1,
    aes = list(edge_colour = "r", edge_width = "width")
  )
  layers[["l2"]] <- list(geom = "ggraph::geom_node_point", size = 22)
  layers[["l3"]] <- list(
    geom = "ggraph::geom_node_text",
    aes = list(label = "name"), colour = "white"
  )
  layers[["l4"]] <- list(geom = "ggraph::theme_graph")
  layers[["l5"]] <- list(geom = "guides", edge_width = "none")

  # Out
  class(layers) <- c("visualisation_recipe", "see_visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  attr(layers, "layout") <- "kk"
  attr(layers, "ggraph") <- TRUE
  layers
}
