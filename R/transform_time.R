# transform_time helper function ------------------------------------------


transform_time <- function(x, time_unit) {

  if (length(x) == 1 && is.na(x)) return(NA)

  switch(time_unit,
         "day" = x,
         "week" = as.Date(cut(x, "week")) + 3,
         "month" = as.Date(cut(x, "month")),
         "quarter" = as.Date(cut(x, "quarter")),
         "year" = as.Date(cut(x, "year")),
         "custom" = date_fun(x))
}
