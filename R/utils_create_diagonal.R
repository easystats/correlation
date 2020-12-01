#' @keywords internal
.create_diagonal <- function(params) {
  diagonal <- data.frame(
    "Parameter1" = unique(params$Parameter1),
    "Parameter2" = unique(params$Parameter1)
  )

  if ("Group" %in% names(params)) diagonal$Group <- unique(params$Group)[1]
  if ("r" %in% names(params)) diagonal$r <- 1
  if ("rho" %in% names(params)) diagonal$rho <- 1
  if ("tau" %in% names(params)) diagonal$tau <- 1
  if ("p" %in% names(params)) diagonal$p <- 0
  if ("t" %in% names(params)) diagonal$t <- Inf
  if ("S" %in% names(params)) diagonal$S <- Inf
  if ("z" %in% names(params)) diagonal$z <- Inf
  if ("df" %in% names(params)) diagonal$df <- unique(params$df)[1]
  if ("df_error" %in% names(params)) diagonal$df_error <- unique(params$df_error)[1]
  if ("CI_low" %in% names(params)) diagonal$CI_low <- 1
  if ("CI_high" %in% names(params)) diagonal$CI_high <- 1
  if ("Method" %in% names(params)) diagonal$Method <- unique(params$Method)[1]
  if ("n_Obs" %in% names(params)) diagonal$n_Obs <- unique(params$n_Obs)[1]

  if ("Median" %in% names(params)) diagonal$Median <- 1
  if ("Mean" %in% names(params)) diagonal$Mean <- 1
  if ("MAP" %in% names(params)) diagonal$MAP <- 1
  if ("SD" %in% names(params)) diagonal$SD <- 0
  if ("MAD" %in% names(params)) diagonal$MAD <- 0
  if ("CI_low" %in% names(params)) diagonal$CI_low <- 1
  if ("CI_high" %in% names(params)) diagonal$CI_high <- 1
  if ("pd" %in% names(params)) diagonal$pd <- 1
  if ("ROPE_Percentage" %in% names(params)) diagonal$ROPE_Percentage <- 0
  if ("BF" %in% names(params)) diagonal$BF <- Inf
  if ("Prior_Distribution" %in% names(params)) diagonal$Prior_Distribution <- unique(params$Prior_Distribution)[1]
  if ("Prior_Location" %in% names(params)) diagonal$Prior_Location <- unique(params$Prior_Location)[1]
  if ("Prior_Scale" %in% names(params)) diagonal$Prior_Scale <- unique(params$Prior_Scale)[1]

  for (var in names(diagonal)[!names(diagonal) %in% names(params)]) {
    diagonal[[var]] <- unique(params[[var]])[1]
  }

  diagonal
}
