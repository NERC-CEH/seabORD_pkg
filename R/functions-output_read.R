
#' Identify prey values meeting calibration criteria
#'
#' Evaluates adult mass loss and chick productivity outputs from a
#' seabORD calibration run and identifies prey values that satisfy both
#' calibration criteria simultaneously.
#'
#' @param sbordout A seabORD output object containing calibration
#'   results, including `output_a0$data` and `output_c0$data`.
#'
#' @param masslower Lower acceptable bound for proportional adult mass
#'   loss. Defaults to `0.09`.
#'
#' @param massupper Upper acceptable bound for proportional adult mass
#'   loss. Defaults to `0.11`.
#'
#' @param prodlower Lower acceptable bound for chick productivity
#'   (`ChicksPerNest`). Defaults to `0.45`.
#'
#' @param produpper Upper acceptable bound for chick productivity
#'   (`ChicksPerNest`). Defaults to `0.55`.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{Prey}{The prey value tested during calibration.}
#'   \item{adult_ok}{Logical indicating whether adult mass loss falls
#'   within the specified bounds.}
#'   \item{chick_ok}{Logical indicating whether chick productivity falls
#'   within the specified bounds.}
#'   \item{overlap}{Logical indicating whether both criteria are met
#'   simultaneously.}
#' }
#'
#' @details
#' Adult mass loss is calculated as:
#' \deqn{
#' (BM\_adult\_t0.mn - BM\_adult.mn) / BM\_adult\_t0.mn
#' }
#'
#' Prey values where both `adult_ok` and `chick_ok` are `TRUE` represent
#' the calibration range that satisfies the specified moderate
#' conditions.
#'
#' @examples
#' \dontrun{
#' sbordout <- seabORD::example_calibration_output_dd
#'
#' calibration_range <- find_calibration_range(
#'   sbordout = sbordout,
#'   masslower = 0.09,
#'   massupper = 0.11,
#'   prodlower = 0.45,
#'   produpper = 0.55
#' )
#'
#' # View prey values satisfying both criteria
#' calibration_range[calibration_range$overlap, ]
#' }
#'
#' @export
find_calibration_range <- function(
    sbordout,
    masslower = 0.09,
    massupper = 0.11,
    prodlower = 0.45,
    produpper = 0.55
) {

  adults <- sbordout$output_a0$data %>%
    dplyr::mutate(
      percentage_loss =
        (BM_adult_t0.mn - BM_adult.mn) /
        BM_adult_t0.mn,
      adult_ok =
        percentage_loss >= masslower &
        percentage_loss <= massupper
    ) %>%
    dplyr::select(
      Prey,
      adult_ok
    )

  chicks <- sbordout$output_c0$data %>%
    dplyr::mutate(
      chick_ok =
        ChicksPerNest >= prodlower &
        ChicksPerNest <= produpper
    ) %>%
    dplyr::select(
      Prey,
      chick_ok
    )

  out <- dplyr::left_join(
    adults,
    chicks,
    by = "Prey"
  ) %>%
    dplyr::mutate(
      overlap = adult_ok & chick_ok
    )

  out
}
