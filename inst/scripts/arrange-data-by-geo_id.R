historical_data <-
    historical_data %>%
    dplyr::arrange(geo_level_1_id, geo_level_2_id, geo_level_3_id) %>%
    dplyr::select(building_id, geo_level_1_id, geo_level_2_id, geo_level_3_id, damage_grade)
