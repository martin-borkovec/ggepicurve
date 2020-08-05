# add date_label ----------------------------------------------------------

add_date_label <- function(gg,
                           time_unit = "month",
                           y_percentage = 0.075,
                           extra_lines = 2,
                           text_par = list(size = 6),
                           format = "%Y-%m-%d",
                           facet = FALSE) {

  maxima <- suppressWarnings(sapply(ggplot_build(gg)$data, FUN = function(x) max(x$ymax, na.rm = TRUE)))
  y_max <- max(maxima[is.finite(maxima)])



  time_range <- do.call(what = ISOformat,
                        args = list(x = transform_time(as.Date(levels(gg$data$time_unit)), time_unit),
                                    format))

  time_label <- tibble(time_unit = factor(time_range, levels = unique(time_range))) %>%
    group_by(time_unit) %>%
    summarise(n = n()) %>%
    mutate(time_unit = case_when(n < max(n) / 2 ~ "",
                                 TRUE ~ as.character(time_unit)),
           x = cumsum(n) - n / 2 + 0.5)

  text_par0 <- list(size = 6)

  if (!is.null(text_par))
    text_par0 <- utils::modifyList(text_par0, text_par)


  if (!facet) {
    gg +
      do.call(annotate, c(list(geom = "text",
                               x = time_label$x,
                               y = -y_max * y_percentage,
                               label = time_label$time_unit),
                          text_par0)) +
      annotate(geom = "segment",
               x = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
               xend = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
               y = -y_max * (y_percentage + 0.025 * text_par0$size / 6) ,
               yend = 0,
               colour = "black") +
      coord_cartesian(ylim = c(0, y_max),
                      clip = "off") +
      theme(plot.margin = unit(c(0, 0, extra_lines, 0), "lines"),
            axis.title.x = element_blank())
  } else {

    anno_geoms <- list()

    for (i in seq_len(nrow(time_label))) {
      anno_geoms[[i]] <- do.call(annotate, c(list(geom = "text",
                                                  x = time_label$x[i],
                                                  y = -y_max * y_percentage,
                                                  label = time_label$time_unit[i]),
                                             text_par0))
    }

    gg +
      anno_geoms +
      scale_y_continuous(expand = c(y_percentage + 0.075 * text_par0$size / 6,0)) +
      annotate(geom = "segment",
               x = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
               xend = cumsum(time_label$n)[-nrow(time_label)] + 0.5,
               y = -y_max * (y_percentage + 0.025 * text_par0$size / 6) ,
               yend = 0,
               colour = "black") +
      coord_cartesian(ylim = c(0, y_max),
                      clip = "off") +
      theme(axis.title.x = element_blank())
  }

}
