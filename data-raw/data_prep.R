## prepare data for analysis ##
devtools::load_all()

s_path <- build_s_path()

# import cytokine data
cyto_raw <- openxlsx::read.xlsx(
  xlsxFile = paste0(
    s_path, "/Data/Copy of APPGAIN PLASMA 47 plex results.xlsx"
  ),
  sheet = "OOR=milipore ",
  startRow = 4,
  sep.names = " "
)
# import tgfb data
tgfb_conc_raw <- openxlsx::read.xlsx(
  paste0(
    s_path, "/Data/Copy of APPGAIN PLASMA 47 plex results.xlsx"
  ),
  sheet = "Ratio MillTGFb",
  rows = c(733:1089),
  cols = c(1:6)
)
# import detection limit data
cyto_lowest <- openxlsx::read.xlsx(
  paste0(
    s_path, "/Data/Copy of APPGAIN PLASMA 47 plex results.xlsx"
  ),
  sheet = "OOR=milipore ",
  rows = c(3, 4),
  cols = c(8:54),
  colNames = FALSE
)
# import clinical data
clinical_raw <- openxlsx::read.xlsx(
  paste0(
    s_path, "/Data/Copy of AshwoodData_SP_11042020.xlsx"
  ),
  sheet = "Sheet1"
)
# import gi data
gi_data <- haven::read_sas(
  paste0(
    "S:/MIND/IDDRC Cores/",
    "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
    "Nordahl_R01MH10443801/MIND133/April2021/",
    "gisymp_2021.sas7bdat"
  )
)

## clean data ##
# transform detection limit data from wide to long
cyto_low <- clean_detection(cyto_lowest)
# clean names and remove duplicate variables
clinical <- clinical_raw |>
  janitor::clean_names() |>
  dplyr::select(-c(gender_2, diagnosis))
# exclude cases with duplicates
cyto <- cyto_raw |>
  # exclude "109939-100" & "110056-100" for now  due to duplicates
  dplyr::filter(
    !(`APP MIND ID` %in% c("109939-100", "110056-100"))
  ) |>
  clean_cyto(cyto_limits = cyto_low)
# filter cases and impute using half-min
tgfb_conc <- tgfb_conc_raw |> clean_tgfb()
## merge data ##
# merge cytokine and clinical data
app_data <- dplyr::left_join(
  clinical,
  cyto,
  by = c(
    "subj_id" = "APP MIND ID",
    "gender" = "gender",
    "app_diagnosis" = "app_diagnosis",
    "study_cohort" = "study_cohort"
  )
)

# merge gi data with app data
all_data <- dplyr::left_join(
  app_data,
  gi_data |>
    dplyr::filter(visit == 1) |>
    dplyr::select(subj_id:gi_diagnosis),
  by = c(
    "subj_id" = "subj_id",
    "visit" = "visit",
    "gender" = "gender",
    "app_diagnosis" = "app_diagnosis"
  )
)

# merge all_data with tgf-b data
all_data2 <- all_data |>
  dplyr::left_join(
    x = _,
    y = tgfb_conc |> clean_tgfb(),
    by = c("subj_id" = "APP.MIND.ID")
  ) |>
  # create immune indicator variables
  create_immune_indicators()

all_data_imputed <- all_data2 |>
  impute_cytokines() |>
  log2_transform()

openxlsx::write.xlsx(
  x = list(
    "data" = all_data_imputed,
    "missingness" = attributes(all_data_imputed)$missing |>
      dplyr::arrange(value)
  ),
  file = paste0(
    s_path,
    "/Data/app_gain_immune_activation_20251114.xlsx"
  )
)

write.csv(
  x = all_data_imputed,
  file = paste0(
    s_path,
    "/Data/app_gain_immune_activation_20251114.csv"
  )
)
