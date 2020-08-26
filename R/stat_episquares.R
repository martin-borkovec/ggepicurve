stat_episquares <- function(mapping = NULL, data = NULL,
                            geom = "pointrange", position = "identity",
                            ...,
                            fun.data = NULL,
                            fun.y = NULL,
                            fun.ymax = NULL,
                            fun.ymin = NULL,
                            fun.args = list(),
                            max_squares = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEpisquares,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      fun.y = fun.y,
      fun.ymax = fun.ymax,
      fun.ymin = fun.ymin,
      fun.args = fun.args,
      max_squares = max_squares,
      na.rm = na.rm,
      ...
    )
  )
}


StatEpisquares <- ggproto("StatEpisquares", Stat,
                          required_aes = c("x", "y"),

                          compute_panel = function(data, scales, fun.data = NULL, fun.y = identity,
                                                   fun.ymax = sum, fun.ymin = NULL, fun.args = list(),
                                                   na.rm = FALSE, max_squares = max_squares) {

                            fun <- ggplot2:::make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
                            out_dat <- ggplot2:::summarise_by_x(data, fun)

                            if(any(out_dat %>% group_by(x) %>% summarise(n = n()) %>% pull(n) > max_squares))
                              out_dat <- out_dat %>%
                              group_by(group) %>%
                              mutate(.index = 1:n()) %>%
                              filter(.index == 1) %>%
                              mutate(y = max(ymax))

                            out_dat
                          }
)
