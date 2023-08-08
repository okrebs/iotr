#' Visualize data for WIOD countries
#'
#' Plots a concise map of all countries from the World Input Output Database's
#' (WIOD) 2016 release, spit into three groups (broadly Asia, Americas, Europe),
#' to obtain a larger visualization for individual countries. Countries will be
#' colored according to the dataset passed to the function.
#'
#' @param plot_data dataframe/tibble containint a column "country" with iso3
#'   codes from the WIOD and another column holding the data to visualize on a
#'   map
#' @param row_label if TRUE a label with the value of 'ROW' in '`plot_data`'
#'   will be placed in the ocean west of Australia
#' @param percent multiplies label values by 100, TRUE/FALSE
#' @param suffix character suffix to add to labels, e.g. "%"
#' @param digits number of digits to round labels to
#' @param reverse_colors if TRUE color scale will be reversed
#' @return Return a plot of all WIOD countries filled with the appropriate color
#' @export io_plot_wiot_map
io_plot_wiot_map <-
  function(plot_data,
           row_label = FALSE,
           percent = FALSE,
           suffix = NULL,
           digits = 2,
           reverse_colors = FALSE) {

  if (length(colnames(plot_data)) != 2) {
    stop("Plot data must be a dataframe/tibble with a column 'country' and only one further data column")
  }
  if (!(any(colnames(plot_data) == "country") &
        any(colnames(plot_data) != "country"))) {
    stop("Plot data must be a dataframe/tibble with a column 'country' and only one further data column")
  }

  data_name <- colnames(plot_data)[which(colnames(plot_data) != "country")]
  data_col_id <- which(colnames(plot_data) == data_name)
  colnames(plot_data)[data_col_id] <- "data_column"

  # add geography so that data can be cropped according to subplot maps
  plot_data <- dplyr::full_join(io_world_map, plot_data, by = "country")
  sf::st_agr(plot_data) <- "constant" # avoid warnings

  # bounding box will not cut rectangles with s2 see
  # https://github.com/r-spatial/sf/issues/1725
  s2_state <- sf_use_s2()
  suppressMessages(sf_use_s2(FALSE))

  plot_america <- subplot_wiot_map(
    suppressMessages(
      sf::st_crop(plot_data, c("xmin" = -170, "xmax" = -35, "ymin" = -60, "ymax" = 80))
    ),
    plot_data,
    percent,
    suffix,
    digits,
    reverse_colors
  )
  plot_europe <- subplot_wiot_map(
    suppressMessages(
      sf::st_crop(plot_data, c("xmin" = -10, "xmax" = 45, "ymin" = 35, "ymax" = 70))
    ),
    plot_data,
    reverse_colors = reverse_colors
  ) +
    ggplot2::theme(legend.position = "none")
  plot_asia <- subplot_wiot_map(
    suppressMessages(
      sf::st_crop(plot_data, c("xmin" = 65, "xmax" = 160, "ymin" = -43, "ymax" = 60))
    ),
    plot_data,
    reverse_colors = reverse_colors
  ) +
    ggplot2::theme(legend.position = "none")

  suppressMessages(sf_use_s2(s2_state))

  if (row_label) {

    row_value <- plot_data$data_column[plot_data$country == "ROW"]
    if (percent) {
      row_value <- round(row_value * 100, digits)
    } else {
      row_value <- round(row_value, digits)
    }
    row_value <- sprintf(paste0("%.", digits, "f"), row_value)
    row_value <- paste0(row_value, suffix)

    plot_asia <- plot_asia +
      ggplot2::geom_sf_label(
        # anker to Australie
        data = dplyr::filter(plot_data, country == "AUS"),
        nudge_x = -50,
        nudge_y = 5,
        size = 6,
        label = paste0("ROW: ", row_value)
      )
  }

  suppressWarnings(
  cowplot::plot_grid(
    plot_america + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA)),
    plot_europe + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA)),
    plot_asia + ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA)),
    nrow = 1, ncol = 3, align = "hv"))
}

#' @noRd
subplot_wiot_map <- function(subplot_data, plot_data, percent = FALSE, suffix = NULL, digits = 1, reverse_colors = FALSE) {
  scale_labels <- seq(
    min(plot_data$data_column, na.rm = TRUE),
    max(plot_data$data_column, na.rm = TRUE),
    length.out = 4)
  if (percent) {
    scale_labels <- round(scale_labels * 100, digits)
  } else {
    scale_labels <- round(scale_labels, digits)
  }
  scale_labels <- sprintf(paste0("%.", digits, "f"), scale_labels)
  if (!is.null(suffix)) {
    scale_labels <- paste0(scale_labels, suffix)
  }

  p <- ggplot2::ggplot(subplot_data) +
    ggplot2::geom_sf(
      color = "white",
      ggplot2::aes(fill = data_column)
    ) +
    # common scale according to values form all subplots
    viridis::scale_fill_viridis(
      NULL,
      option = "A",
      direction = (-1)^reverse_colors,
      na.value = "grey90",
      breaks =
        seq(min(plot_data$data_column + 1e-9, na.rm = TRUE),
            max(plot_data$data_column - 1e-9, na.rm = TRUE),
            length.out = 4),
      labels = scale_labels,
      limits = range(plot_data$data_column, na.rm = TRUE)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 24),
          legend.position = c(0.2,0.3),
          legend.key.size = ggplot2::unit(12, "mm"),
          legend.text = ggplot2::element_text(size = 16))
  return(p)
}
