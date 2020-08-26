coord_cartesian0 <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                             default = FALSE, clip = "on") {
  ggproto(NULL, CoordCartesian0,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          default = default,
          clip = clip
  )
}

CoordCartesian0 <- ggproto("CoordCartesian0", Coord,

                           is_linear = function() TRUE,
                           is_free = function() TRUE,

                           distance = function(x, y, panel_params) {
                             max_dist <- dist_euclidean(panel_params$x.range, panel_params$y.range)
                             dist_euclidean(x, y) / max_dist
                           },

                           range = function(panel_params) {
                             list(x = panel_params$x.range, y = panel_params$y.range)
                           },

                           backtransform_range = function(self, panel_params) {
                             self$range(panel_params)
                           },

                           transform = function(data, panel_params) {
                             rescale_x <- function(data) rescale(data, from = panel_params$x.range)
                             rescale_y <- function(data) rescale(data, from = panel_params$y.range)

                             data <- transform_position(data, rescale_x, rescale_y)
                             transform_position(data, squish_infinite, squish_infinite)
                           },

                           setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                             train_cartesian <- function(scale, limits, name) {
                               range <- scale_range(scale, limits, self$expand)
                               if(name == "y") range[1] <- 0
                               out <- scale$break_info(range)

                               out$arrange <- scale$axis_order()
                               names(out) <- paste(name, names(out), sep = ".")
                               out
                             }

                             c(
                               train_cartesian(scale_x, self$limits$x, "x"),
                               train_cartesian(scale_y, self$limits$y, "y")
                             )
                           }
)

scale_range <- function(scale, limits = NULL, expand = TRUE) {
  expansion <- if (expand) ggplot2:::expand_default(scale) else c(0, 0)
  if (is.null(limits)) {
    scale$dimension(expansion)
  } else {
    range <- range(scale$transform(limits))
    expand_range(range, expansion[1], expansion[2])
  }
}
