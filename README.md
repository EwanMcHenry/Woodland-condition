# Woodland-condition

![General approach to estimating woodland condition](images/General%20approach%20combo.excalidraw.png)

This repository contains the code and data for analysis to inform The Woodland Trust's evolving approach to Woodland Condition assessment.

The repository is structured as follows:

-   Quorto book files, numbered. 'index.qmd' is the landing page:
    -   A page for each indicator, displaying the proposed measurement method, estimates of value functions and comments from the expert panel.
-   `Data/` contains the data used in the analysis:
    -   Excel forms, completed by expert woodland practitioners, containing data on their estimated value functions and weightings for different condition indicators
    -   data on appropriate tree and plant species for different types of site and woodland structure, adapted from The Woodland Trust's Tree Species Handbook.
-   `Scripts/` contains the scripts used to analyse the data:
    -   `functions_delphi_analysis.R` contains many of the the functions used repeatedly in the analysis and loads the libraries required for them. Functions include:
        -   `config_for_delphi_round()`
        -   `generate_interactive_PCA()`
        -   `continuous_vf_fig()`
        -   `ggplot_gam_resp_vf()`
        -   `ggplot_resp_cat_vf()`
        -   `ggplot_resp_weight_ind()`
        -   `categorised_vf_fig()`
    -   `Extract_expert_info.R` extracts the expert information from the Excel forms
    -   `curation of extracted data.R` curates the extracted data
    -   `indicator template.qmd` is used as a template for indicator pages of the Quorto book
    -   `Round 2 response checking.R` exploratory analysis of the second round of expert responses
    -   `indicator matcher data.R` sets the different names used for the same indicators so they can be matched. Ran within `Extract_expert_info.R` and dependent on that.
    -   `Archive/` exploratory scripts and functions that were used in the analysis but are not part of the final analysis:
        -   `Delphi results processing.R` prelude to developed code that does some of extraction of data and analysis of value functions and weights. Superseded by Quarto book
        -   `heatmap script.R` initial exploratory work to investigate correlations between indicators and experts using heat maps

Some of the Code Structure

![](images/Condition%20code%20structure%202024-11-11%2016.43.03.excalidraw.png){width="1500"}
