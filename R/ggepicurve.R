#' Create a ggepicurve plot
#'
#' ggepicurve takes a data set of case based epidemiological data. This data set has to contain one column of the class [Date] by which the curve (actually columns) will be plotted.
#' This column is internally transformed into a factor so keep that in mind for any transformations of the X-axis labels.
#'
#'@details **suggested_theme** adjusts the following theme elements:
#' text = element_text(size = 12), panel.border = element_blank(), panel.background = element_rect(fill = NA), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
#' panel.grid.major.y = element_line(colour = "lightgrey"), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", vjust = 0.5, hjust = 0.5), axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1), legend.position = "top", legend.direction = "horizontal")
#'
#'@return ggepicurve returns a gg object consisiting of a call to at least [ggplot2::ggplot()], [ggplot2::scale_x_discrete()], [ggplot2::scale_y_continuous()] and [ggepicurve:::stat_epicurve()].
#' Additional gg objects can be added as usual with "+" but are not guaranteed to work. (e.g. [ggplot2::facet_wrap()] should work properly, but [ggplot2::facet_grid()] often causes problems)
#'
#'@seealso [ggepicurve::annotation_date()] for creating pretty X-axis annotations based on week, month, quarter or year.
#'
#'@param mapping 	Set of aesthetic mappings created by [ggplot2::aes()] or [ggplot2::aes_()] for [ggplot2::geom_col()]. This will not count as the default mapping at the top level of the plot.
#'@param data Data set in which each line represents a case. Has to contain column of class date. For aggregated data (e.g. weekly cases) any date of the aggregation period (e.g. monday) can be used.
#'@param date Name of column which contains date by which cases are to be plotted.
#'@param epi_blocks Schould each case be plotted as a separate block? Can be TRUE, FALSE or a numeric. If it's a numeric, blocks will be drawn if the highest case count of the panel doesn't surpass its value.
#'@param date_unit Should the data be aggregated? Defaults to "day" (i.e. no aggregation). Other possible values are "week", "month", "quarter" and "year".
#' All cases of the respective date unit will be aggregated on the X-axis to its first day (except for "week" in which case the Thursday of each week will be the aggregation point).
#'@param date_labels Will be passed to [ggplot2::scale_x_discrete] as the labels argument. If a function is passed, the input will be the dates on the X-axis as a **character** vector.
#'@param start_date Start date of X-axis. Character string in "YYYY-MM-DD" format.
#'@param end_date End date of X-axis. Character string in "YYYY-MM-DD" format.
#'@param gg_first List of gg objects to plot before (backgorund of) the epicurve.
#'@param col_par Additional parameters for the geom_col call.
#'@param facet_x_scale Should be only used when creating a faceted plot with a free X-scale.
#'@param suggested_theme Should suggested [ggplot2::theme()] adjustments be performed? Defaults to TRUE. See Details.
#'
#'
#'@export
#'@import ggplot2
#'@import rlang
#'@import scales
#'@import dplyr

ggepicurve <- function(data,
                       date,
                       mapping = list(),
                       epi_blocks = FALSE,
                       date_unit = "day",
                       date_labels = function(x) x,
                       start_date = NULL,
                       end_date = NULL,
                       gg_first = NULL,
                       col_par = list(),
                       x_scale = NULL,
                       y_scale = NULL,
                       suggested_theme = TRUE) {


  # input checks ------------------------------------------------------------

  checkmate::assert_data_frame(data)

  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    abort("Mapping should be created with `aes()` or `aes_()`.")
  }

  if(is.logical(epi_blocks))
    checkmate::check_logical(epi_blocks, any.missing = FALSE, len = 1)
  else
    checkmate::check_number(epi_blocks, lower = 0)

  checkmate::assert_subset(date_unit,
                           c("day",
                             "week",
                             "month",
                             "quarter",
                             "year",
                             "custom"))  # custom not worked out yet

  # checkmate::assert_subset(color_scheme,c("bundesland", "covid_quelle"))
  checkmate::assert_subset(date, names(data), empty.ok = FALSE)

  checkmate::assert_list(gg_first, null.ok = TRUE)
  checkmate::assert_list(col_par, null.ok = FALSE)
  checkmate::assert_choice(x_scale, c("year", "free"), null.ok = TRUE)



  # inputs ------------------------------------------------------------------

  # fill_aes <- enquo(fill_aes)
  # label_aes <- enquo(label_aes)

  data$date_used <- data[[date]]
  if (!is.null(start_date)) {
    checkmate::assert_date(as.Date(start_date))
    start_date <- as.Date(start_date)
  }
  if (!is.null(end_date)) {
    checkmate::assert_date(as.Date(end_date))
    end_date <- as.Date(end_date)
  }

  if(is.function(date_labels))


  # get factor date_range with levels for each possible  value --------------
  if (is.null(start_date))
    start_date <- min(data$date_used)

  if (is.null(end_date))
    end_date <- max(data$date_used)

  date_range <- start_date + 0:(end_date - start_date)
  date_range <- unique(transform_time(date_range, date_unit))


  # create column date_unit with allocation of each case --------------------
  data$.date_unit <- factor(as.character(transform_time(data$date_used, date_unit)),
                          levels = as.character(date_range))


  # initialize plot ---------------------------------------------------------

  gg <- ggplot(data, aes(x = .date_unit, y = 1)) +
    gg_first


  # depending on episquares add correct layer -------------------------------
  col_par0 <- list(geom = "col",
                   col = "black",
                   width = 1,
                   position = "stack",
                   mapping = aes(),
                   max_squares = case_when(is.numeric(epi_blocks) ~ as.numeric(epi_blocks),
                                           epi_blocks == TRUE ~ Inf,
                                           epi_blocks == FALSE ~ 0),
                   x_scale = x_scale,
                   date_unit = date_unit)

  col_par <- utils::modifyList(col_par0, col_par)
  col_par$mapping <- utils::modifyList(col_par$mapping, mapping)

  gg <- gg + do.call(stat_epicurve, col_par)

  # add rest of stuff -------------------------------------------------------
  gg <- gg +
    scale_x_discrete(labels = date_labels,
                     drop = FALSE,
                     na.translate = FALSE) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = my_pretty_breaks(10)) +
    labs(x = "",
         y = "Cases")

  if(suggested_theme)
  gg <- gg +
    theme(text = element_text(size = 12),
          panel.border = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey"),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(face = "bold", vjust = 0.5, hjust = 0.5),
          axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
          legend.position = "top",
          legend.direction = "horizontal")


  # add color_scheme --------------------------------------------------------

  # if (!is.null(color_scheme)) {
  #   if (color_scheme == "bundesland") {
  #     cols <- epitools::colorbrewer.palette(nclass = 9, type = "q", palette = "a")
  #     cols[5] <- "gold1"
  #
  #   }
  #   if (color_scheme == "covid_quelle") {
  #     cols <- c("#c1c1bd", "limegreen", "#4056a1", "#d79922", "#225b30", "#d08582", "#af2a2a", "white")
  #   }
  #   gg <- gg + scale_fill_manual(values = cols, drop = FALSE)
  # }

  gg

}
