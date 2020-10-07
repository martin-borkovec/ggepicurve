#' @export
annotation_date <- function(time_unit = "month",
                            y_percentage = 0.075,
                            y_scale = NULL,
                            extra_lines = 0,
                            text_par = list(),
                            line_par = list(),
                            format = "%Y-%m-%d") {

  if (!is.null(format)) {
    format <-   switch(time_unit,
                       week = "%ISOW",
                       month = "%b",
                       year = "%Y"
                       )
  }

  params_text <- list(time_unit = time_unit,
                      format = format,
                      y_percentage = y_percentage,
                      y_scale = y_scale,
                      type = "text",
                      size = 4)

  # browser()
  params_text <- utils::modifyList(params_text, text_par)

  params_segment <- list(time_unit = time_unit,
                         format = format,
                         y_percentage = y_percentage,
                         y_scale = y_scale,
                         type = "segment",
                         text_size = params_text$size)

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
    layer(
      data = NULL,
      geom = "segment",
      stat = StatDateLabel,
      position = "identity",
      mapping = aes(y = 1),
      params = params_segment),
    theme(panel.spacing = unit(extra_lines, "lines"),
          plot.margin = unit(c(0, 0, extra_lines, 0), "lines")),
    coord_cartesian(clip = "off", ylim = c(0, NA)))
}


StatDateLabel <- ggproto("StatDateLabel", Stat,
                         required_aes = c("x"),
                         clip = "off",
                         compute_panel = function(data,
                                                  scales,
                                                  layout = layout,
                                                  na.rm = FALSE,
                                                  time_unit = NULL,
                                                  format = NULL,
                                                  y_percentage = NULL,
                                                  y_scale = NULL,
                                                  type = type,
                                                  text_size = NULL) {

                           fun <- ggplot2:::make_summary_fun(NULL, identity, sum, NULL, NULL)
                           out_dat <- ggplot2:::summarise_by_x(data, fun)
                           y_max <- out_dat %>%
                             group_by(x, PANEL) %>%
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
                             mutate(time_label = case_when(n < max(n) / 3 ~ "",
                                                           TRUE ~ as.character(time_label)),
                                    x = cumsum(n) - n / 2 + 0.5)

                           time_label$y <- -y_max * y_percentage

                           if(type == "text") return (time_label)
                           if(nrow(time_label) == 1) return(data.frame())

                           data.frame(
                             x = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
                             xend = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
                             y = -y_max * y_percentage  - 0.025 * y_max * text_size / 4,
                             yend = 0)
                         },
                         finish_layer = function(self, data, params) {
                           if (!is.null(params$y_scale) && params$y_scale == "fixed") data$y = min(data$y)
                           data
                         }



)
