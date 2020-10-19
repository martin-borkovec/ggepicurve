stat_epicurve <- function(mapping = NULL, data = NULL,
                          geom = "pointrange", position = "identity",
                          ...,
                          max_squares = NULL,
                          x_scale = NULL,
                          date_unit = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEpicurve,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      max_squares = max_squares,
      x_scale = x_scale,
      date_unit = date_unit,
      na.rm = na.rm,
      ...
    )
  )
}


StatEpicurve <- ggproto("StatEpicurve", Stat,
                        required_aes = c("x", "y"),

                        compute_panel = function(data, scales, fun.data = NULL, x_scale = x_scale,
                                                 na.rm = FALSE, max_squares = max_squares, date_unit = NULL) {

                          fun <- ggplot2:::make_summary_fun(NULL, identity, sum, NULL, NULL)
                          out_dat <- ggplot2:::summarise_by_x(data, fun)

                          # if (max(out_dat$x) > length(scales$x$range$range) & !is.null(x_scale) && x_scale == "year") {
                          #   stop("x_scale 'year' can not be applied since panel(s) cover more than a single year")
                          # }
                          if (max(out_dat$x) <= length(scales$x$range$range) & !is.null(x_scale) && x_scale == "year") {

                            first_case_of_year <- scales$x$range$range[min(out_dat$x)]
                            year <- unique(format(as.Date(scales$x$range$range[range(out_dat$x)[1]:range(out_dat$x)[2]]), "%Y"))


                            # scales$x$limits <- year_dates

                            # if (date_unit == "day") {
                            year_dates <-seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by = 1)
                            year_dates <- as.character(unique(transform_time(year_dates, date_unit)))

                            #   out_dat$x <- out_dat$x - min(out_dat$x) + which(first_case_of_year == year_dates) }
                            # if (date_unit == "week") {
                            #   year_dates <- as.character(seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by = 1))
                            scales$x$range$range <- year_dates
                            out_dat$x <- out_dat$x - min(out_dat$x) + which(first_case_of_year == year_dates)
                          }

                        if (!is.null(x_scale) && x_scale == "free") {
                          # browser()
                          #
                          # scales$x$limits <- scales$x$range$range[range(out_dat$x)[1]:range(out_dat$x)[2]]
                          scales$x$range$range <- scales$x$range$range[range(out_dat$x)[1]:range(out_dat$x)[2]]
                          out_dat$x <- out_dat$x - min(out_dat$x) + 1
                        }


                        if (any(out_dat %>% group_by(x) %>% summarise(n = n()) %>% pull(n) > max_squares)) {
                          out_dat <- out_dat %>%
                            group_by(group) %>%
                            mutate(.index = 1:n()) %>%
                            filter(.index == 1) %>%
                            mutate(y = max(ymax))
                        }

                        out_dat
                        }
)
