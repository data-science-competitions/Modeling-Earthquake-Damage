tidy_data <- FeatureStore$new()$tidy_data

# Method 1 ----------------------------------------------------------------
treat_plan <-
    tidy_data %>%
    dplyr::filter(source %in% "historical_data") %>%
    vtreat::designTreatmentsN(
        varlist = "geo_level_2_id",
        outcome = "damage_grade",
        verbose = FALSE
    )

scoreFrame <- treat_plan$scoreFrame

dtreated <-
    vtreat::prepare(
        treat_plan,
        tidy_data,
        pruneSig = NULL,
        varRestriction = "geo_level_2_id_catN"
    ) %>%
    dplyr::select(geo_level_2_id_catN)

# Method 2 ----------------------------------------------------------------
set.seed(1949)
treat_plan <- vtreat::mkCrossFrameNExperiment(
    dframe = tidy_data %>% dplyr::filter(source %in% "historical_data"),
    varlist = "geo_level_2_id",
    outcome = "damage_grade",
    ncross = 2^3,
    verbose = !FALSE
)

tidy_treated_data <- vtreat::prepare(treatmentplan = treat_plan$treatments, dframe = tidy_data)
