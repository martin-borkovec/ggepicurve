ggepicurve
================

plotting epicurves with **ggplot2**

## Install

``` r
devtools::install_github("martin-borkovec/ggepicurve", 
                         dependencies = TRUE,
                         build_vignettes = TRUE)
```

Make sure to checkout the vignette.

``` r
vignette("ggepicurve", package = "ggepicurve")
```

## Example

``` r
library(ggepicurve)

dat <- ggepicurve::sim_data
dat <- dat[dat$date <= as.Date("2012-04-01") & dat$date > as.Date("2011-10-01"),]

ggepicurve(data = dat,
           mapping = aes(fill = death),
           date = "date") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18)) +
  annotation_date(date_unit = "month",
                  format = "%b",
                  y_percentage = 0.05,
                  text_par = list(size = 7)
                  ) +
  annotation_date(date_unit = "year",
                  format = "%Y",
                  y_percentage = 0.12,
                  extra_lines = 5,
                  text_par = list(fontface = "bold", size = 12)
                  )
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
