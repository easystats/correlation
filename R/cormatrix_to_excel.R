#' @title Easy export of correlation matrix to Excel
#'
#' @description Easily output a correlation matrix and export it to
#' Microsoft Excel, with the first row and column frozen, and
#' correlation coefficients colour-coded based on effect size
#' (0.0-0.2: small (no colour); 0.2-0.4: medium (pink/light blue);
#' 0.4-1.0: large (red/dark blue)), following Cohen's suggestions
#' for small (.10), medium (.30), and large (.50) correlation sizes.
#'
#' @param data The data frame
#' @param filename Desired filename (path can be added before hand
#' but no need to specify extension).
#' @param overwrite Whether to allow overwriting previous file.
#' @param print.mat Logical, whether to also print the correlation matrix
#'                  to console.
#' @param ... Parameters to be passed to [correlation::correlation()]
#'
#' @keywords correlation matrix Excel
#' @author Adapted from @JanMarvin (JanMarvin/openxlsx2#286) and
#' the original `rempsyc::cormatrix_excel`.
#' @return A Microsoft Excel document, containing the colour-coded
#'         correlation matrix with significance stars, on the first
#'         sheet, and the colour-coded p-values on the second sheet.
#' @export
#' @examplesIf requireNamespace("openxlsx2", quietly = TRUE)
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Basic example
#' suppressWarnings(cormatrix_to_excel(mtcars,
#'   select = c("mpg", "cyl", "disp", "hp", "carb"), filename = "cormatrix1"
#' ))
#' suppressWarnings(cormatrix_to_excel(iris,
#'   p_adjust = "none",
#'   filename = "cormatrix2"
#' ))
#' suppressWarnings(cormatrix_to_excel(airquality,
#'   method = "spearman",
#'   filename = "cormatrix3"
#' ))
#' \dontshow{
#' setwd(.old_wd)
#' }
cormatrix_to_excel <- function(data,
                               filename,
                               overwrite = TRUE,
                               print.mat = TRUE,
                               ...) {
  if (missing(filename)) {
    insight::format_error("Argument 'filename' required (as per CRAN policies).")
  }

  insight::check_if_installed("openxlsx2")

  # create correlation matrix with p values
  cormatrix <- correlation::correlation(data, ...)
  cormatrix <- summary(cormatrix, redundant = TRUE)
  all.columns <- 2:(ncol(cormatrix))
  if (isTRUE(print.mat)) {
    print(cormatrix)
  }
  p_val <- attr(cormatrix, "p")

  # Define colours
  style_gray <- openxlsx2::wb_colour(hex = "C1CDCD")
  style_black <- openxlsx2::wb_colour(hex = "000000")
  style_pink <- openxlsx2::wb_colour(hex = "FBCAC0")
  style_peach <- openxlsx2::wb_colour(hex = "F79681")
  style_red <- openxlsx2::wb_colour(hex = "F65534")
  style_lightblue <- openxlsx2::wb_colour(hex = "97FFFF")
  style_midblue <- openxlsx2::wb_colour(hex = "0AF3FF")
  style_darkblue <- openxlsx2::wb_colour(hex = "00BFFF")
  style_green1 <- openxlsx2::wb_colour(hex = "698B22")
  style_green2 <- openxlsx2::wb_colour(hex = "9ACD32")
  style_green3 <- openxlsx2::wb_colour(hex = "B3EE3A")

  # Colours
  gray_style <- openxlsx2::create_dxfs_style(
    bg_fill = style_gray,
    font_color = style_black,
    num_fmt = "#.#0 _*_*_*"
  )

  p_style <- openxlsx2::create_dxfs_style(
    bg_fill = "",
    font_color = style_black,
    num_fmt = "#.##0 _*_*_*"
  )
  p_style1 <- openxlsx2::create_dxfs_style(
    bg_fill = style_green1,
    font_color = style_black,
    num_fmt = "#.##0 _*_*_*"
  )
  p_style2 <- openxlsx2::create_dxfs_style(
    bg_fill = style_green2,
    font_color = style_black,
    num_fmt = "#.##0 _*_*_*"
  )
  p_style3 <- openxlsx2::create_dxfs_style(
    bg_fill = style_green3,
    font_color = style_black,
    num_fmt = "#.##0 _*_*_*"
  )

  # no star
  no_star <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 _*_*_*",
    font_color = style_black,
    bg_fill = ""
  )

  # one star
  one_star_pink <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_pink
  )
  one_star_peach <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_peach
  )
  one_star_red <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_red
  )
  one_star_lightblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_lightblue
  )
  one_star_midblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_midblue
  )
  one_star_darkblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*_*_*",
    font_color = style_black,
    bg_fill = style_darkblue
  )

  # two stars
  two_stars_pink <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_pink
  )
  two_stars_peach <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_peach
  )
  two_stars_red <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_red
  )
  two_stars_lightblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_lightblue
  )
  two_stars_midblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_midblue
  )
  two_stars_darkblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*_*",
    font_color = style_black,
    bg_fill = style_darkblue
  )

  # three stars
  three_stars_pink <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_pink
  )
  three_stars_peach <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_peach
  )
  three_stars_red <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_red
  )
  three_stars_lightblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_lightblue
  )
  three_stars_midblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_midblue
  )
  three_stars_darkblue <- openxlsx2::create_dxfs_style(
    num_fmt = "#.#0 \\*\\*\\*",
    font_color = style_black,
    bg_fill = style_darkblue
  )

  # create openxlsx2 workbook
  wb <- openxlsx2::wb_workbook()

  # assign all the required styles to the workbook
  wb$add_style(gray_style)
  wb$add_style(no_star)
  wb$add_style(one_star_pink)
  wb$add_style(one_star_peach)
  wb$add_style(one_star_red)
  wb$add_style(one_star_lightblue)
  wb$add_style(one_star_midblue)
  wb$add_style(one_star_darkblue)
  wb$add_style(two_stars_pink)
  wb$add_style(two_stars_peach)
  wb$add_style(two_stars_red)
  wb$add_style(two_stars_lightblue)
  wb$add_style(two_stars_midblue)
  wb$add_style(two_stars_darkblue)
  wb$add_style(three_stars_pink)
  wb$add_style(three_stars_peach)
  wb$add_style(three_stars_red)
  wb$add_style(three_stars_lightblue)
  wb$add_style(three_stars_midblue)
  wb$add_style(three_stars_darkblue)
  wb$add_style(p_style)
  wb$add_style(p_style1)
  wb$add_style(p_style2)
  wb$add_style(p_style3)
  # wb$styles_mgr$styles$dxfs
  # wb$styles_mgr$dxf

  # create the worksheets and write the data to the worksheets.
  wb$add_worksheet("r_values")$add_data(x = cormatrix)
  wb$add_worksheet("p_values")$add_data(x = p_val)

  # create conditional formatting for the stars (as well as colours as we have no)
  # one star
  # Compute the cell range for styling
  dims_fmt <- openxlsx2::wb_dims(cols = all.columns, rows = all.columns)
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= .2, B2 > 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_pink"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_peach"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_red"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= -.2, B2 < 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_lightblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_midblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .05)",
    style = "one_star_darkblue"
  )

  # two stars
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= .2, B2 > 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_pink"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_peach"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_red"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= -.02, B2 < 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_lightblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_midblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .01)",
    style = "two_stars_darkblue"
  )

  # three stars
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= .2, B2 > 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_pink"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_peach"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= .4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_red"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 >= -.2, B2 < 0, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_lightblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.2, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_midblue"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 <= -.4, INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) < .001)",
    style = "three_stars_darkblue"
  )

  # Other formatting
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(B2 = 1)",
    style = "gray_style"
  )
  wb$add_conditional_formatting(
    "r_values",
    dims = dims_fmt,
    rule = "AND(INDEX(p_values!$B:$ZZ, ROW(), COLUMN()) >= .05)",
    style = "no_star"
  )

  # p-values
  wb$add_conditional_formatting(
    "p_values",
    dims = dims_fmt,
    rule = "< 10",
    style = "p_style"
  )
  wb$add_conditional_formatting(
    "p_values",
    dims = dims_fmt,
    rule = "< .05",
    style = "p_style1"
  )
  wb$add_conditional_formatting(
    "p_values",
    dims = dims_fmt,
    rule = "< .01",
    style = "p_style2"
  )
  wb$add_conditional_formatting(
    "p_values",
    dims = dims_fmt,
    rule = "< .001",
    style = "p_style3"
  )
  wb$add_conditional_formatting(
    "p_values",
    dims = dims_fmt,
    rule = "== 0",
    style = "gray_style"
  )

  ## Freeze Panes
  wb$freeze_pane("r_values", first_col = TRUE, first_row = TRUE)
  wb$freeze_pane("p_values", first_col = TRUE, first_row = TRUE)

  # Save Excel
  cat(paste0(
    "\n\n [Correlation matrix '", filename,
    ".xlsx' has been saved to working directory (or where specified).]"
  ))
  openxlsx2::wb_save(wb, file = paste0(filename, ".xlsx"), overwrite = TRUE)

  # open in Excel
  openxlsx2::xl_open(paste0(filename, ".xlsx"))
}
