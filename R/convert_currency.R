#' Converts currency using api.exchangerate.host
#' @param from Currency to convert from
#' @param to Currency to convert to
#' @param retried Hidden parameter used as an exponent of wait time between failed attempts.
#' @return exchange rate
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom checkmate assert_number
#' @importFrom logger log_info log_error
#' @importFrom glue glue
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


#' Look up the historical value of a US Dollar in Hungarian Forints
#' @param start_date date
#' @param end_date date
#' @inheritParams convert_currency
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom checkmate assertNumeric
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_usdhufs <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0){
  tryCatch(
    {
      response <- GET(
        'https://api.exchangerate.host/timeseries',
        query = list(
          base = 'USD',
          symbols = 'HUF',
          start_date = start_date,
          end_date = end_date
        )
      )
      if (response$status_code == 200) {
        exchange_rates <- content(response)$rates
        usdhufs <- data.table(
          date = as.Date(gsub(pattern = ".HUF", "", names(unlist(exchange_rates)))),
          usdhuf = unname(unlist(exchange_rates)))
        assertNumeric(usdhufs$usdhuf, lower = 250, upper = 400)
      } else {
        log_error("Error in request to api. Status code: {response$status_code}")
        Sys.sleep(2 ^ retried)
        convert_currency(start_date = start_date, end_date = end_date, from, to, retried = retried + 1)
      }
    },
    error = function(e) {
      log_error(e$message)
      Sys.sleep(2 ^ retried)
      # match.call -> repeats a call
      # instead of repeating: convert_currency(start_date = start_date, end_date = end_date, from, to, retried = retried)
      mc <- match.call()
      mc$retried <- mc$retried + 1
      eval(mc)
    }
  )
  return(usdhufs)
}


#' Look up the historical value of a symbol in a specified base currency in a given time period.
#' @param symbol
#' @param base
#' @param start_date date
#' @param end_date date
#' @inheritParams convert_currency
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom checkmate assertNumeric
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_exchange_rates <- function(symbol = 'HUF', base = 'USD', start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0){
  tryCatch(
    {
      response <- GET(
        'https://api.exchangerate.host/timeseries',
        query = list(
          base = base,
          symbols = symbol,
          start_date = start_date,
          end_date = end_date
        )
      )
      if (response$status_code == 200) {
        exchange_rates <- content(response)$rates
        usdhufs <- data.table(
          date = as.Date(gsub(pattern = ".HUF", "", names(unlist(exchange_rates)))),
          usdhuf = unname(unlist(exchange_rates)))
        assertNumeric(usdhufs$usdhuf, lower = 250, upper = 400)
      } else {
        log_error("Error in request to api. Status code: {response$status_code}")
        Sys.sleep(2 ^ retried)
        convert_currency(start_date = start_date, end_date = end_date, from, to, retried = retried + 1)
      }
    },
    error = function(e) {
      log_error(e$message)
      Sys.sleep(2 ^ retried)
      # match.call -> repeats a call
      # instead of repeating: convert_currency(start_date = start_date, end_date = end_date, from, to, retried = retried)
      mc <- match.call()
      mc$retried <- mc$retried + 1
      eval(mc)
    }
  )
  return(usdhufs)
}
