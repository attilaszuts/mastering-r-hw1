#' Formats numbers as Hungarian Forints
#' @param x A numeric vector
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(1231232351.323)
forint <- function(x) {
  assert_numeric(x)
  dollar(x, prefix = "", suffix = " Ft")
}
