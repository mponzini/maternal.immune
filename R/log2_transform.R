log2_transform <- function(
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
      'TGFa (75)', 'TNFa (76)', 'TNFb (77)', 'VEGF-A (78)',
      'TGF-b.(18)'
    )
) {
  data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(cyto_vars),
        ~ .x |> log2(),
        .names = "{col}_log2"
      )
    )
}
