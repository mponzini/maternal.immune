#' @title Impute cytokines
#' @param data A [data.frame] that includes cytokine concentrations
#' @param cyto_vars A vector of cytokine variable names
#' @param threshold A [numeric] value that indicates the level of missingness
#' to tolerate for imputation. Values above the threshold will be excluded.
#' @param my_seed [numeric] Set seed for reproducibility
#' @returns A [data.frame] with imputed cytokine data for all cytokines with
#' missingness below the threshold. Summary of cytokine missingness can be
#' found in the 'missing' attribute.
#' @export

impute_cytokines <- function(
    data,
    cyto_vars = c(
      'sCD40L (12)', 'EGF (13)', 'Eotaxin (14)', 'FGF-2 (15)', 'FLT-3L (18)',
      'Fractalkine (19)', 'G-CSF (20)', 'GM-CSF (21)', 'GROa (22)',
      'IFNa2 (25)', 'IFNg (26)', 'IL-1a (27)', 'IL-1b (28)', 'IL-1ra (29)',
      'IL-2 (30)', 'IL-3 (33)', 'IL-4 (34)', 'IL-5 (35)', 'IL-6 (36)',
      'IL-7 (37)', 'IL-8 (38)', 'IL-9 (39)', 'IL-10 (42)', 'IL-12p40 (43)',
      'IL-12p70 (44)', 'IL-13 (46)', 'IL-15 (47)', 'IL-17a (48)', 'IL-17e (51)',
      'IL-17F (53)', 'IL-18 (54)', 'IL-22 (55)', 'IL-27 (56)', 'IP-10 (57)',
      'MCP-1 (61)', 'MCP-3 (62)', 'M-CSF (63)', 'MDC (64)', 'MIG (65)',
      'MIP-1a (66)', 'MIP-1b (67)', 'PDGF-AA (72)', 'PDGF-AB/BB (73)',
      'TGFa (75)', 'TNFa (76)', 'TNFb (77)', 'VEGF-A (78)'
    ),
    threshold = 0.55,
    my_seed = 649
) {
  cyto_only <- data |>
    dplyr::filter(
      !(
        dplyr::if_all(
          .cols = dplyr::all_of(cyto_vars),
          ~ is.na(.x)
        )
      )
    ) |>
    dplyr::select(
      subj_id, Immune, dplyr::all_of(cyto_vars)
    )
  # calculate % missing per cytokine
  cyto_na <- (colSums(is.na(cyto_only[-c(1,2 )])) / nrow(cyto_only)) |>
    tibble::enframe()
  # exclude cytokines with % missing > threshold from imputation
  cyto_exclude <- cyto_na |>
    dplyr::filter(value >= threshold) |>
    dplyr::pull(name)
  imp_vars <- dplyr::setdiff(cyto_vars, cyto_exclude)
  # subset data to subj_id, Immune status, and cytokines to impute
  tmp <- cyto_only |>
    dplyr::select(subj_id, Immune, dplyr::all_of(imp_vars)) |>
    # convert subj_id to numeric and Immune to factor for imputation
    dplyr::mutate(
      subj_id = gsub("-", "", subj_id) |> as.numeric(),
      Immune = factor(Immune, levels = c("No", "Yes"))
    )

  set.seed(my_seed)
  cytokines_rf_imp <- missForest::missForest(tmp)$ximp

  # convert subj_id back to char
  cytokines_rf_imp <- cytokines_rf_imp |>
    dplyr::mutate(
      subj_id = stringi::stri_sub_replace(subj_id, 7, 6, value = "-")
    )

  # replace original cytokine data with imputed
  to_return <- data |>
    # remove originals
    dplyr::select(-dplyr::all_of(imp_vars)) |>
    dplyr::left_join(
      x = _,
      y = cytokines_rf_imp,
      by = c("subj_id", "Immune")
    )

  # include missing data table as attribute
  attr(to_return, "missing") <- cyto_na

  # return data with imputed values and summary of % missing
  return(to_return)
}
