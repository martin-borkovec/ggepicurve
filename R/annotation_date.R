annotation_date <- function(time_unit = "month",
                            y_percentage = 0.075,
                            extra_lines = 0,
                            text_par = list(),
                            line_par = list(),
                            format = "%Y-%m-%d") {


  params_text <- list(time_unit = time_unit,
                      format = format,
                      y_percentage = y_percentage,
                      type = "text")

  params_text <- c(params_text, text_par)

  params_segment <- list(time_unit = time_unit,
                         format = format,
                         y_percentage = y_percentage,
                         type = "segment")

  params_segment <- c(params_segment, line_par)

  list(
    layer(
      data = NULL,
      geom = "text",
      stat = StatDateLabel,
      position = "identity",
      mapping = aes(y = 1,
                    label = stat(time_label)),
      params = params_text),
    theme(panel.spacing = unit(extra_lines, "lines"),
          plot.margin = unit(c(0, 0, extra_lines, 0), "lines")),
    coord_cartesian0(clip = "off"),
    layer(
      data = NULL,
      geom = "segment",
      stat = StatDateLabel,
      position = "identity",
      mapping = aes(y = 1),
      params = params_segment))
}


StatDateLabel <- ggproto("StatDateLabel", Stat,
                         required_aes = c("x"),
                         clip = "off",
                         compute_panel = function(data, scales, fun.data = NULL, fun.y = identity,
                                                  fun.ymax = sum, fun.ymin = NULL, fun.args = list(),
                                                  na.rm = FALSE, time_unit = NULL, format = NULL,
                                                  y_percentage = NULL, type = type) {

                           fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
                           out_dat <- ggplot2:::summarise_by_x(data, fun)
                           y_max <- out_dat %>%
                             group_by(x) %>%
                             summarise(n = n()) %>% pull(n) %>% max

                           time_label <- data.frame(time_label = do.call(what = ISOformat,
                                                                         args = list(x = transform_time(as.Date(scales$x$range$range), time_unit),
                                                                                     format)),
                                                    time_label_year = do.call(what = ISOformat,
                                                                              args = list(x = transform_time(as.Date(scales$x$range$range), time_unit),
                                                                                          paste(format, "%Y", collapse = "")))) %>%
                             mutate(time_label_year = factor(time_label_year, unique(time_label_year))) %>%
                             group_by(time_label_year) %>%
                             summarise(n = n(),
                                       time_label = time_label[1]) %>%
                             mutate(time_label = case_when(n < max(n) / 2 ~ "",
                                                           TRUE ~ as.character(time_label)),
                                    x = cumsum(n) - n / 2 + 0.5)

                           time_label$y <- -y_max * y_percentage

                           if(type == "text") return (time_label)

                           data.frame(
                             x = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
                             xend = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
                             y = -y_max * (y_percentage + 0.05),
                             yend = 0)


                         }
)
