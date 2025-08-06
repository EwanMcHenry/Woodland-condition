# Render WECA report

library(quarto)

# Define your sites
sites <- list(
  list(
    site_name = "Ladypark Wood",
    zone_name = "1",
    site_survey_folderpath = "Data\\Field Test\\Ladypark Wood, Wye valley - Non-intervention 70yrs broadleaf SSSI ASNW\\",
    site_survey_filename = "WCA - Lady park wood - Field test - 12.06.2025.xlsx",
    site_survey_habitat_type = "base_rich_lowland"
    )
)

# Render the report for each site
for (site in sites) {
  quarto_render(
    input = "weca report template.qmd",      # your .qmd template
    output_file = paste0("WECA report - ", site$site_name, ", Zone ", site$zone_name, ".html"),
    execute_params = site
  )
}


