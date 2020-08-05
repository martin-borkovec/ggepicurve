# integer pretty breaks
my_pretty_breaks <- function (n = 5) {
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n)
    names(breaks) <- attr(breaks, "labels")
    as.integer(breaks)
  }
}



#day

labels_7days <- function(x) {
  x <- format(as.Date(x), "%d.")
  x[!x %in% c("07.","14.","21.", "28.")] <- ""
  x
}

# like format.Date but uses ISOweek for "%ISOW"
ISOformat <- function(x, format) {

  xx <- vector("character", length(x))

  for(i in seq_along(x)) {

    format_i <- gsub("%ISOW",
                     substr(ISOweek::ISOweek(x[i]), 7, 8),
                     format)

    xx[i] <- format(x[i], format_i)
  }

  xx
}
