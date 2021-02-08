# display and print method works - markdown -----------------------------

if (require("gt") && packageVersion("insight") >= "0.12.0.1") {
  test_that("display and print method works - markdown", {
    skip_on_cran()

    expect_equal(
      display(correlation(iris)),
      structure(c(
        "Table: Correlation Matrix (pearson-method)", "",
        "|Parameter1   |   Parameter2 |     r |         95% CI | t(148) |         p |",
        "|:------------|:------------:|:-----:|:--------------:|:------:|:---------:|",
        "|Sepal.Length |  Sepal.Width | -0.12 |  (-0.27, 0.04) |  -1.44 | 0.152     |",
        "|Sepal.Length | Petal.Length |  0.87 |   (0.83, 0.91) |  21.65 | < .001*** |",
        "|Sepal.Length |  Petal.Width |  0.82 |   (0.76, 0.86) |  17.30 | < .001*** |",
        "|Sepal.Width  | Petal.Length | -0.43 | (-0.55, -0.29) |  -5.77 | < .001*** |",
        "|Sepal.Width  |  Petal.Width | -0.37 | (-0.50, -0.22) |  -4.79 | < .001*** |",
        "|Petal.Length |  Petal.Width |  0.96 |   (0.95, 0.97) |  43.39 | < .001*** |",
        "p-value adjustment method: Holm (1979)", "Observations: 150"
      ), format = "pipe", class = c("knitr_kable", "character"))
    )

    expect_output(print(correlation(iris)))

    expect_snapshot(display(correlation(iris)), cran = FALSE)
  })

  # display and print method works - HTML -----------------------------

  test_that("display and print method works - HTML", {
    skip_on_cran()

    expect_equal(
      display(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"),
      structure(list(
        `_data` = structure(list(
          Parameter1 = "wt", Parameter2 = "mpg",
          r = "-0.87", `95% CI` = "(-0.93, -0.74)", `t(30)` = "-9.56",
          p = "< .001***"
        ), row.names = c(NA, -1L), class = c(
          "tbl_df",
          "tbl", "data.frame"
        )), `_boxhead` = structure(list(
          var = c(
            "Parameter1",
            "Parameter2", "r", "95% CI", "t(30)", "p"
          ), type = c(
            "default",
            "default", "default", "default", "default", "default"
          ), column_label = list(
            "Parameter1", "Parameter2", "r", "95% CI", "t(30)", "p"
          ),
          column_align = c(
            "left", "center", "center", "center", "center",
            "center"
          ), column_width = list(
            NULL, NULL, NULL, NULL, NULL,
            NULL
          ), hidden_px = list(
            NULL, NULL, NULL, NULL, NULL,
            NULL
          )
        ), row.names = c(NA, -6L), class = c(
          "tbl_df", "tbl",
          "data.frame"
        )), `_stub_df` = structure(list(
          rownum_i = 1L, groupname = NA_character_,
          rowname = NA_character_
        ), row.names = c(NA, -1L), class = c(
          "tbl_df",
          "tbl", "data.frame"
        )), `_row_groups` = character(0), `_stub_others` = NA_character_,
        `_heading` = list(
          title = "Correlation Matrix (pearson-method)",
          subtitle = NULL
        ), `_spanners` = structure(list(
          vars = list(),
          spanner_label = list(), gather = logical(0), built = character(0)
        ), row.names = integer(0), class = c(
          "tbl_df",
          "tbl", "data.frame"
        )), `_stubhead` = list(label = NULL),
        `_footnotes` = structure(list(
          locname = character(0), grpname = character(0),
          colname = character(0), locnum = numeric(0), rownum = integer(0),
          colnum = integer(0), footnotes = character(0)
        ), row.names = integer(0), class = c(
          "tbl_df",
          "tbl", "data.frame"
        )), `_source_notes` = list(list(
          "p-value adjustment method: Holm (1979)",
          "Observations: 32"
        )), `_formats` = list(), `_styles` = structure(list(
          locname = character(0), grpname = character(0), colname = character(0),
          locnum = numeric(0), rownum = integer(0), colnum = integer(0),
          styles = list()
        ), row.names = integer(0), class = c(
          "tbl_df",
          "tbl", "data.frame"
        )), `_summary` = list(), `_options` = structure(list(
          parameter = c(
            "container_width", "container_height",
            "container_overflow_x", "container_overflow_y", "table_id",
            "table_width", "table_layout", "table_margin_left", "table_margin_right",
            "table_background_color", "table_additional_css", "table_font_names",
            "table_font_size", "table_font_weight", "table_font_style",
            "table_font_color", "table_font_color_light", "table_border_top_include",
            "table_border_top_style", "table_border_top_width", "table_border_top_color",
            "table_border_right_style", "table_border_right_width",
            "table_border_right_color", "table_border_bottom_include",
            "table_border_bottom_style", "table_border_bottom_width",
            "table_border_bottom_color", "table_border_left_style",
            "table_border_left_width", "table_border_left_color",
            "heading_background_color", "heading_align", "heading_title_font_size",
            "heading_title_font_weight", "heading_subtitle_font_size",
            "heading_subtitle_font_weight", "heading_border_bottom_style",
            "heading_border_bottom_width", "heading_border_bottom_color",
            "heading_border_lr_style", "heading_border_lr_width",
            "heading_border_lr_color", "column_labels_background_color",
            "column_labels_font_size", "column_labels_font_weight",
            "column_labels_text_transform", "column_labels_vlines_style",
            "column_labels_vlines_width", "column_labels_vlines_color",
            "column_labels_border_top_style", "column_labels_border_top_width",
            "column_labels_border_top_color", "column_labels_border_bottom_style",
            "column_labels_border_bottom_width", "column_labels_border_bottom_color",
            "column_labels_border_lr_style", "column_labels_border_lr_width",
            "column_labels_border_lr_color", "column_labels_hidden",
            "row_group_background_color", "row_group_font_size",
            "row_group_font_weight", "row_group_text_transform",
            "row_group_padding", "row_group_border_top_style", "row_group_border_top_width",
            "row_group_border_top_color", "row_group_border_right_style",
            "row_group_border_right_width", "row_group_border_right_color",
            "row_group_border_bottom_style", "row_group_border_bottom_width",
            "row_group_border_bottom_color", "row_group_border_left_style",
            "row_group_border_left_width", "row_group_border_left_color",
            "table_body_hlines_style", "table_body_hlines_width",
            "table_body_hlines_color", "table_body_vlines_style",
            "table_body_vlines_width", "table_body_vlines_color",
            "table_body_border_top_style", "table_body_border_top_width",
            "table_body_border_top_color", "table_body_border_bottom_style",
            "table_body_border_bottom_width", "table_body_border_bottom_color",
            "data_row_padding", "stub_background_color", "stub_font_size",
            "stub_font_weight", "stub_text_transform", "stub_border_style",
            "stub_border_width", "stub_border_color", "summary_row_padding",
            "summary_row_background_color", "summary_row_text_transform",
            "summary_row_border_style", "summary_row_border_width",
            "summary_row_border_color", "grand_summary_row_padding",
            "grand_summary_row_background_color", "grand_summary_row_text_transform",
            "grand_summary_row_border_style", "grand_summary_row_border_width",
            "grand_summary_row_border_color", "footnotes_font_size",
            "footnotes_padding", "footnotes_background_color", "footnotes_margin",
            "footnotes_border_bottom_style", "footnotes_border_bottom_width",
            "footnotes_border_bottom_color", "footnotes_border_lr_style",
            "footnotes_border_lr_width", "footnotes_border_lr_color",
            "footnotes_sep", "footnotes_marks", "source_notes_padding",
            "source_notes_background_color", "source_notes_font_size",
            "source_notes_border_bottom_style", "source_notes_border_bottom_width",
            "source_notes_border_bottom_color", "source_notes_border_lr_style",
            "source_notes_border_lr_width", "source_notes_border_lr_color",
            "row_striping_background_color", "row_striping_include_stub",
            "row_striping_include_table_body"
          ), scss = c(
            FALSE, FALSE,
            FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
            FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, FALSE, FALSE
          ), category = c(
            "container",
            "container", "container", "container", "table", "table",
            "table", "table", "table", "table", "table", "table",
            "table", "table", "table", "table", "table", "table",
            "table", "table", "table", "table", "table", "table",
            "table", "table", "table", "table", "table", "table",
            "table", "heading", "heading", "heading", "heading",
            "heading", "heading", "heading", "heading", "heading",
            "heading", "heading", "heading", "column_labels", "column_labels",
            "column_labels", "column_labels", "table_body", "table_body",
            "table_body", "column_labels", "column_labels", "column_labels",
            "column_labels", "column_labels", "column_labels", "column_labels",
            "column_labels", "column_labels", "column_labels", "row_group",
            "row_group", "row_group", "row_group", "row_group", "row_group",
            "row_group", "row_group", "row_group", "row_group", "row_group",
            "row_group", "row_group", "row_group", "row_group", "row_group",
            "row_group", "table_body", "table_body", "table_body",
            "table_body", "table_body", "table_body", "table_body",
            "table_body", "table_body", "table_body", "table_body",
            "table_body", "data_row", "stub", "stub", "stub", "stub",
            "stub", "stub", "stub", "summary_row", "summary_row",
            "summary_row", "summary_row", "summary_row", "summary_row",
            "grand_summary_row", "grand_summary_row", "grand_summary_row",
            "grand_summary_row", "grand_summary_row", "grand_summary_row",
            "footnotes", "footnotes", "footnotes", "footnotes", "footnotes",
            "footnotes", "footnotes", "footnotes", "footnotes", "footnotes",
            "footnotes", "footnotes", "source_notes", "source_notes",
            "source_notes", "source_notes", "source_notes", "source_notes",
            "source_notes", "source_notes", "source_notes", "row",
            "row", "row"
          ), type = c(
            "px", "px", "overflow", "overflow",
            "value", "px", "value", "px", "px", "value", "values",
            "values", "px", "value", "value", "value", "value", "logical",
            "value", "px", "value", "value", "px", "value", "logical",
            "value", "px", "value", "value", "px", "value", "value",
            "value", "px", "value", "px", "value", "value", "px",
            "value", "value", "px", "value", "value", "px", "value",
            "value", "value", "px", "value", "value", "px", "value",
            "value", "px", "value", "value", "px", "value", "logical",
            "value", "px", "value", "value", "px", "value", "px",
            "value", "value", "px", "value", "value", "px", "value",
            "value", "px", "value", "value", "px", "value", "value",
            "px", "value", "value", "px", "value", "value", "px",
            "value", "px", "value", "px", "value", "value", "value",
            "px", "value", "px", "value", "value", "value", "px",
            "value", "px", "value", "value", "value", "px", "value",
            "px", "px", "value", "px", "value", "px", "value", "value",
            "px", "value", "value", "values", "px", "value", "px",
            "value", "px", "value", "value", "px", "value", "value",
            "logical", "logical"
          ), value = list(
            "auto", "auto", "auto",
            "auto", NA_character_, "auto", "fixed", "auto", "auto",
            "#FFFFFF", character(0), c(
              "-apple-system", "BlinkMacSystemFont",
              "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell",
              "Helvetica Neue", "Fira Sans", "Droid Sans", "Arial",
              "sans-serif"
            ), "16px", "normal", "normal", "#333333",
            "#FFFFFF", TRUE, "solid", "2px", "#A8A8A8", "none",
            "2px", "#D3D3D3", TRUE, "solid", "2px", "#A8A8A8",
            "none", "2px", "#D3D3D3", NA_character_, "center",
            "125%", "initial", "85%", "initial", "solid", "2px",
            "#D3D3D3", "none", "1px", "#D3D3D3", NA_character_,
            "100%", "normal", "inherit", "none", "1px", "#D3D3D3",
            "solid", "2px", "#D3D3D3", "solid", "2px", "#D3D3D3",
            "none", "1px", "#D3D3D3", FALSE, NA_character_, "100%",
            "initial", "inherit", "8px", "solid", "2px", "#D3D3D3",
            "none", "1px", "#D3D3D3", "solid", "2px", "#D3D3D3",
            "none", "1px", "#D3D3D3", "solid", "1px", "#D3D3D3",
            "none", "1px", "#D3D3D3", "solid", "2px", "#D3D3D3",
            "solid", "2px", "#D3D3D3", "8px", NA_character_,
            "100%", "initial", "inherit", "solid", "2px", "#D3D3D3",
            "8px", NA_character_, "inherit", "solid", "2px",
            "#D3D3D3", "8px", NA_character_, "inherit", "double",
            "6px", "#D3D3D3", "90%", "4px", NA_character_, "0px",
            "none", "2px", "#D3D3D3", "none", "2px", "#D3D3D3",
            "<br />", "numbers", "4px", NA_character_, "90%",
            "none", "2px", "#D3D3D3", "none", "2px", "#D3D3D3",
            "rgba(128,128,128,0.05)", FALSE, FALSE
          )
        ), row.names = c(
          NA,
          -133L
        ), class = c("tbl_df", "tbl", "data.frame")), `_transforms` = list(),
        `_has_built` = FALSE
      ), class = c("gt_tbl", "list"))
    )

    expect_output(print(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"))
  })
}
