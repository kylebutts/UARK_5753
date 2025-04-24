parse_seasonality_args <- function(m, name, arg, auto.disable, default.order) {
  if (arg == 'auto') {
    fourier.order <- 0
    if (name %in% names(m$seasonalities)) {
      message('Found custom seasonality named "', name,
              '", disabling built-in ', name, ' seasonality.')
    } else if (auto.disable) {
      message('Disabling ', name, ' seasonality. Run prophet with ', name,
              '.seasonality=TRUE to override this.')
    } else {
      fourier.order <- default.order
    }
  } else if (isTRUE(arg)) {
    fourier.order <- default.order
  } else if (isFALSE(arg)) {
    fourier.order <- 0
  } else {
    fourier.order <- arg
  }
  return(fourier.order)
}
assignInNamespace("parse_seasonality_args", parse_seasonality_args, ns = "prophet")
