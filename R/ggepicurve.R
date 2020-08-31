#' Create a ggepicurve plot
#'
#'@export
#'@import ggplot2
#'@import rlang
#'@import scales
#'@import dplyr

ggepicurve <- function(df_cases,
                        date = "1. pos. LaborDiagnose",
                        epi_squares = FALSE,
                        fill_aes = NULL,
                        label_aes = NULL,
                        time_unit = "day",
                        time_breaks = function(x) x,
                        time_labels = function(x) x,
                        start_date = NULL,
                        end_date = NULL,
                        gg_first = NULL,
                        color_scheme = NULL,
                        bar_par = list()) {


  # input checks ------------------------------------------------------------
  checkmate::assert_subset(time_unit,c(
    "day",
    "week",
    "month",
    "quarter",
    "year",
    "custom"))  # custom not worked out yet

  checkmate::assert_subset(color_scheme,c("bundesland", "covid_quelle"))

  checkmate::assert_subset(date, names(df_cases), empty.ok = FALSE)



  # inputs ------------------------------------------------------------------

  fill_aes <- enquo(fill_aes)
  label_aes <- enquo(label_aes)
  dat <- df_cases
  dat$date_used <- dat[[date]]
  if (!is.null(start_date) & is.character(start_date))
    start_date <- as.Date(start_date)


  # get factor time_range with levels for each possible  value --------------
  # browser()
  if (is.null(start_date))
    start_date <- min(dat$date_used)

  if (is.null(end_date))
    end_date <- max(dat$date_used)

  time_range <- start_date + 0:(end_date - start_date)
  time_range <- unique(transform_time(time_range, time_unit))


  # create column time_unit with allocation of each case --------------------
  dat$time_unit <- factor(as.character(transform_time(dat$date_used, time_unit)),
                          levels = as.character(time_range))

  # epi_squares <- max(table(dat$time_unit)) <= epi_squares

  # initialize plot ---------------------------------------------------------

  gg <- ggplot(dat, aes(x = time_unit)) +
    gg_first


  # depending on episquares add correct layer -------------------------------


  bar_par0 <- list(geom = "col",
                   col = "black",
                   width = 1,
                   position = "stack",
                   mapping = aes(fill = !!fill_aes,
                                 y = 1),
                   max_squares = case_when(is.numeric(epi_squares) ~ as.numeric(epi_squares),
                                           epi_squares == TRUE ~ Inf,
                                           epi_squares == FALSE ~ 0),
                   fun.y = identity,
                   fun.ymax = sum)

  bar_par <- utils::modifyList(bar_par0, bar_par)


  gg <- gg + do.call(ggepicurve:::stat_episquares, bar_par)

  # add labels if set (only with episquares)
  if (!quo_is_null(label_aes)) {
    if (epi_squares) {
      gg <- gg + geom_text(aes(label = !!label_aes, y = 1, group = !!fill_aes),
                           position = position_stack(vjust = 0.5))
    } else {
      warning("epi_squares must be TRUE if label is to be printed")
    }

  }



  # add rest of stuff -------------------------------------------------------
  gg <- gg +
    scale_x_discrete(breaks = time_breaks,
                     labels = time_labels,
                     drop = FALSE,
                     na.translate = FALSE) +
    theme(text = element_text(size = 12),
          panel.border = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey"),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(face = "bold", vjust = 0.5, hjust = 0.5),
          axis.line.x = element_line(),
          axis.line.y = element_line(),
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.background = element_rect(fill = NA),
          legend.key = element_blank(),
          legend.title = element_blank()) +
    scale_y_continuous(expand = c(0, 0), breaks = my_pretty_breaks(10))


  # add color_scheme --------------------------------------------------------

  if (!is.null(color_scheme)) {
    if (color_scheme == "bundesland") {
      cols <- epitools::colorbrewer.palette(nclass = 9, type = "q", palette = "a")
      cols[5] <- "gold1"

    }
    if (color_scheme == "covid_quelle") {
      cols <- c("#c1c1bd", "limegreen", "#4056a1", "#d79922", "#225b30", "#d08582", "#af2a2a", "white")
    }
    gg <- gg + scale_fill_manual(values = cols, drop = FALSE)
  }

  gg

}
