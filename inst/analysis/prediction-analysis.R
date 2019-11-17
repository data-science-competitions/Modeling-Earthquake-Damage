# Visualisation -----------------------------------------------------------
accuracy <- model_performance %>% dplyr::filter(.metric %in% "accuracy", is.na(.class))
(grand_accuracy <- sum(accuracy$.estimate * accuracy$.n) / sum(accuracy$.n))
## Metrics Correlation Plot
model_performance %>%
    dplyr::filter(.metric %in% c("accuracy", "mae", "rmse", "rsq"), is.na(.class)) %>%
    dplyr::select(-.estimator) %>%
    dplyr::mutate(.metric = paste0("metric_", .metric)) %>%
    tidyr::spread(".metric", ".estimate") %>%
    dplyr::select(dplyr::starts_with("metric_")) %>%
    dplyr::rename_all(function(x) stringr::str_remove_all(x, "metric_")) %>%
    as.matrix() %>%
    PerformanceAnalytics::chart.Correlation(method = "spearman", histogram = FALSE)
## Density Plot
par(pty = "m")
accuracy %>% .$.estimate %>% density(from = 0, to = 1) %>% plot(main = "")
title("Accuracy ~ geo_level_1_id Density Plot")
abline(v = grand_accuracy, lty = 2, col = "gray")
## Scatter Plot
par(pty = "s")
accuracy %>% dplyr::select(.n, .estimate) %>% plot(ylim = c(0.5, 1), pch = 21, cex = 1, bg = "orange", col = "gray")
title("Accuracy ~ geo_level_1_id Scatter Plot")
abline(h = grand_accuracy, lty = 2, col = "gray")
with(accuracy, text(.estimate ~ .n, labels = geo_level_1_id, pos = 4, cex = 0.5))
