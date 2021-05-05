#' Converts currency using api.exchangerate.host
#' @param from Currency to convert from
#' @param to Currency to convert to
#' @param retried Hidden parameter used as an exponent of wait time between failed attempts.
#' @return exchange rate
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom checkmate assert_number
#' @importFrom logger log_info log_error
#' @examples
#' convert_currency(from = "EUR", to = "HUF")
convert_currency <- function(from = "USD", to = "HUF", retried = 0) {
  tryCatch(
    {
      response <- fromJSON(glue('https://api.exchangerate.host/convert?from={from}&to={to}'))
      exchange_rate <- assert_number(response$result, lower = 250, upper = 400)
    },
    error = function(e) {
      log_error(e$message)
      Sys.sleep(2 ^ retried)
      convert_currency(from, to, retried = retried + 1)
    }
  )
  log_info('Exchange rate of 1 {from} = {exchange_rate} {to}')
  return(exchange_rate)
}
