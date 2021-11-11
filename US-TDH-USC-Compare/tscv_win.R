tscv_win <- function(y, forecastfunction, h=1, window=NULL, xreg=NULL, initial=0, winend=NULL, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if(initial >= n) stop("initial period too long")
  tsp(e) <- tsp(y)
  if (!is.null(xreg)) {
    # Make xreg a ts object to allow easy subsetting later
    xreg <- ts(as.matrix(xreg))
    if(NROW(xreg) != length(y))
      stop("xreg must be of the same size as y")
    # Pad xreg with NAs
    xreg <- ts(rbind(xreg, matrix(NA, nrow=h, ncol=NCOL(xreg))),
               start = start(y),
               frequency = frequency(y))
  }
  if (is.null(window))
    indx <- seq(1+initial, n - 1L)
  else
    indx <- seq(window+initial, n - 1L, by = 1L)
  for (i in indx) {
    y_subset <- subset(
      y,
      start = ifelse(is.null(window), 1L,
                     ifelse(i - window >= 0L, i - window + 1L, stop("small window"))),
      end = i-winend)
    if (is.null(xreg)) {
      fc <- try(suppressWarnings(
        forecastfunction(y_subset, h = h, ...)
      ), silent = TRUE)
    }
    else {
      xreg_subset <- subset(
        xreg,
        start = ifelse(is.null(window), 1L,
                       ifelse(i - window >= 0L, i - window + 1L, stop("small window"))),
        end = i)
      xreg_future <- subset(
        xreg,
        start = i+1,
        end = i+h)
      fc <- try(suppressWarnings(
        forecastfunction(y_subset, h = h, xreg = xreg_subset, newxreg=xreg_future)
      ), silent = TRUE)
    }
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + seq(h)] - fc$mean[seq(h)]
    }
  }
  if (h == 1) {
    return(e[, 1L])
  } else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}

