clean_tgfb <- function(data) {
  data <- data |>
    dplyr::filter(
      !(APP.MIND.ID %in% c('109939-100', '110056-100', '105097-100'))
    ) |>
    dplyr::mutate(`TGF-b.(18)` = `TGF-b.(18)` |> as.numeric())

  # get min / 2 for imputation
  imp_value <- min(data$`TGF-b.(18)`, na.rm = TRUE) / 2

  # impute
  data |>
    dplyr::mutate(
      `TGF-b.(18)` = ifelse(
        test = is.na(`TGF-b.(18)`),
        yes = imp_value,
        no = `TGF-b.(18)`
      )
    )
}
