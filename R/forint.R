#' Formats numbers as Hungarian Forints
#' @param x A number
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(1231232351.323)
forint <- function(x) {
  assert_number(x)
  dollar(x, prefix = "", suffix = " Ft")
}
