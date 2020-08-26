annotation_date <- function(time_unit = "month",
                            y_percentage = 0.075,
                            panel_extra_lines = 2,
                            plot_extra_lines = 2,
                            text_par = list(),
                            format = "%Y-%m-%d") {


  list(
    layer(
      data = NULL,
      geom = "text",
      stat = StatDateLabel,
      position = "identity",
      mapping = aes(y = 1,
                    label = stat(time_label)),
      params = list(time_unit = time_unit,
                    format = format,
                    y_percentage = y_percentage)),
    theme(panel.spacing = unit(panel_extra_lines, "lines"),
          plot.margin = unit(c(0, 0, plot_extra_lines, 0), "lines")),
    coord_cartesian0(clip = "off"))
}


StatDateLabel <- ggproto("StatDateLabel", Stat,
                         required_aes = c("x"),
                         clip = "off",
                         compute_panel = function(data, scales, fun.data = NULL, fun.y = identity,
                                                  fun.ymax = sum, fun.ymin = NULL, fun.args = list(),
                                                  na.rm = FALSE, time_unit = NULL, format = NULL,
                                                  y_percentage = NULL) {

                           fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
                           out_dat <- ggplot2:::summarise_by_x(data, fun)

                           y_max <- out_dat %>%
                             group_by(x) %>%
                             summarise(n = n()) %>% pull(n) %>% max

                           time_range <- do.call(what = ISOformat,
                                                 args = list(x = transform_time(as.Date(scales$x$range$range), time_unit),
                                                             format))

                           time_label <- tibble(time_label = factor(time_range, levels = unique(time_range))) %>%
                             group_by(time_label) %>%
                             summarise(n = n()) %>%
                             mutate(time_label = case_when(n < max(n) / 2 ~ "",
                                                          TRUE ~ as.character(time_label)),
                                    x = cumsum(n) - n / 2 + 0.5)
                           # browser()
                           time_label$y <- -y_max * y_percentage
                           time_label
                         }
)
