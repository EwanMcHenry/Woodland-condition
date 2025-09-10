# Render WECA report

library(quarto)

dir.create("outputs/WECA reports", showWarnings = FALSE)

# Define your sites
sites <- list(
  ## Beechy Wood ----
  list(
    site_name = "Beechy Wood",
    zone_name = "1",
    site_survey_folderpath = "Data\\Field Test\\Beechy Wood, Sussex - Corsican Pine PAWS\\Z01\\",
    site_survey_filename = "WCA - Beechy Wood - Field test 22.05.25.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  ),
  
  ## Ladypark Wood ----
  list(
    site_name = "Ladypark Wood",
    zone_name = "1",
    site_survey_folderpath = "Data\\Field Test\\Ladypark Wood, Wye valley - Non-intervention 70yrs broadleaf SSSI ASNW\\Z01\\",
    site_survey_filename = "WCA - Lady park wood - Field test - 12.06.2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  ),
  ## Inverliever, Argyll ----
  list(
    site_name = "Inverliever, Argyll",
    zone_name = "1",
    site_survey_folderpath = "Data\\Field Test\\Inverliever, Argyll - Conifer clearfell FLS\\Z01\\",
    site_survey_filename = "WCA - Inverliever - Field test 06.06.25.xlsx",
    site_survey_habitat_type = "acidic_upland"
  ),
  ## Harrisons Wood, Louth, Lincs ----
  list(
    site_name = "Harrisons Wood",
    zone_name = "Z001",
    site_survey_folderpath = "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z001\\",
    site_survey_filename = "WECA - Harrisons Woodland Z001 BNG - Field test - 2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  ),
  list(
    site_name = "Harrisons Wood",
    zone_name = "Z015a",
    site_survey_folderpath = "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z15a\\",
    site_survey_filename = "WECA - Harrisons Woodland Z15a BNG - Field test - 2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  ),
  list(
    site_name = "Harrisons Wood",
    zone_name = "Z020",
    site_survey_folderpath = "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z020\\",
    site_survey_filename = "WECA - Harrisons Woodland Z020 BNG - Field test - 2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  ),
  list(
    site_name = "Harrisons Wood",
    zone_name = "Z021",
    site_survey_folderpath = "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z021\\",
    site_survey_filename = "WECA - Harrisons Woodland Z021 BNG - Field test - 2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
  )
  
)


# render - 
for (site in sites) {
  # Set the switch value here (TRUE to embed, FALSE to link)
  site$embed_photos <- FALSE
  site$include_photos <- T
  
  filename <- paste0("WECA report - ", site$site_name, ", Zone ", site$zone_name, ".html")
  
  # Render to working directory
  quarto_render(
    input = "weca report template.qmd",
    output_file = filename,
    execute_params = site
  )
  
  # Move output to the folder
  file.rename(
    from = filename,
    to = file.path("outputs", "WECA reports", filename)
  )
}

