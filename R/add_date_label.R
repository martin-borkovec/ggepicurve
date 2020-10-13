# add date_label ----------------------------------------------------------

add_date_label <- function(gg,
                           date_unit = "month",
                           y_percentage = 0.075,
                           extra_lines = 2,
                           text_par = list(size = 6),
                           format = "%Y-%m-%d",
                           facet = FALSE) {

  maxima <- suppressWarnings(sapply(ggplot_build(gg)$data, FUN = function(x) max(x$ymax, na.rm = TRUE)))
  y_max <- max(maxima[is.finite(maxima)])



  date_range <- do.call(what = ISOformat,
                        args = list(x = transform_time(as.Date(levels(gg$data$date_unit)), date_unit),
                                    format))

  date_label <- tibble(date_unit = factor(date_range, levels = unique(date_range))) %>%
    group_by(date_unit) %>%
    summarise(n = n()) %>%
    mutate(date_unit = case_when(n < max(n) / 2 ~ "",
                                 TRUE ~ as.character(date_unit)),
           x = cumsum(n) - n / 2 + 0.5)

  text_par0 <- list(size = 6)

  if (!is.null(text_par))
    text_par0 <- utils::modifyList(text_par0, text_par)


  if (!facet) {
    gg +
      do.call(annotate, c(list(geom = "text",
                               x = date_label$x,
                               y = -y_max * y_percentage,
                               label = date_label$date_unit),
                          text_par0)) +
      annotate(geom = "segment",
               x = cumsum(date_label$n)[-nrow(date_label)] + 0.5,
               xend = cumsum(date_label$n)[-nrow(date_label)] + 0.5,
               y = -y_max * (y_percentage + 0.025 * text_par0$size / 6) ,
               yend = 0,
               colour = "black") +
      coord_cartesian(ylim = c(0, y_max),
                      clip = "off") +
      theme(plot.margin = unit(c(0, 0, extra_lines, 0), "lines"),
            axis.title.x = element_blank())
  } else {

    anno_geoms <- list()

    for (i in seq_len(nrow(date_label))) {
      anno_geoms[[i]] <- do.call(annotate, c(list(geom = "text",
                                                  x = date_label$x[i],
                                                  y = -y_max * y_percentage,
                                                  label = date_label$date_unit[i]),
                                             text_par0))
    }

    gg +
      anno_geoms +
      scale_y_continuous(expand = c(y_percentage + 0.075 * text_par0$size / 6,0)) +
      annotate(geom = "segment",
               x = cumsum(date_label$n)[-nrow(date_label)] + 0.5,
               xend = cumsum(date_label$n)[-nrow(date_label)] + 0.5,
               y = -y_max * (y_percentage + 0.025 * text_par0$size / 6) ,
               yend = 0,
               colour = "black") +
      coord_cartesian(ylim = c(0, y_max),
                      clip = "off") +
      theme(axis.title.x = element_blank())
  }

}
