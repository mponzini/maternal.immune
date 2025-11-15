#' @title Create Immune Activation Indicators
#' @param data [data.frame] of the cytokine concentrations
#' @param immune_vars A vector of variables used to construct `Immune`
#' @param atopic_vars A vector of variables used to construct `Atopic Allergy`
#' @returns Returns a [data.frame] with the immune activation indicators
#' @export

create_immune_indicators <- function(
    data,
    immune_vars = c(
      "m_type_1_db", "m_ms", "m_hashimotos", "m_graves",
      "m_rheumatoid_arthritis", "m_psoriasis", "m_rheumatic_carditis",
      "m_alopecia", "m_crohns_disease", "m_ulcerative_colitis", "m_coeliac",
      "m_lupus_erythematosus", "m_sjogrens", "m_antiphospholipid_syndrome",
      "m_asthma", "m_allergy", "m_eczema",
      "m_idiopathic_thrombocytopenic_purpura", "m_uticarial_vasculitis",
      "m_white_dot_syndrome", "m_inflammatory_bowel_type_unknown", "m_kawasaki",
      "m_raynauds", "m_stevens_johnson", "m_eosinophile_follicueitis",
      "m_other_autoimmune"
    ),
    atopic_vars = c(
      "m_asthma",
      "m_allergy",
      "m_eczema"
    )
) {
  data |>
    dplyr::mutate(
      # immune activation
      Immune = ifelse(
        dplyr::if_any(dplyr::all_of(immune_vars), ~.x == 1),
        "Yes",
        "No"
      ),
      # atopic allergy
      atopic_allergy = ifelse(
        dplyr::if_any(dplyr::all_of(atopic_vars), ~.x == 1),
        "Yes",
        "No"
      )
    )
}
