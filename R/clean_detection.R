#' @title Clean Cytokine Concentration Data
#' @param data [data.frame] of cytokine detection limit
#' @returns Returns a [data.frame] after initial data cleaning

clean_detection <- function(data) {
  data |>
    t() |>
    tibble::as_tibble() |>
    dplyr::rename(
      Concentration = `1`,
      Cytokine = `2`
    ) |>
    dplyr::relocate(Cytokine, .before = Concentration) |>
    dplyr::mutate(
      Concentration = as.numeric(Concentration)
    )
}
