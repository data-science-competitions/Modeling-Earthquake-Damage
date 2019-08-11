GlobalEnv_funs <- as.vector(lsf.str(envir = .GlobalEnv))
source(list.files(pattern = "testthat-helpers.R$", recursive = TRUE, full.names = TRUE))
.setup()

.run_unit_tests()
.run_component_tests()
.run_integration_tests()
.run_coverage_tests()

.cleanup()
rm(GlobalEnv_funs, list = setdiff(as.vector(lsf.str(envir = .GlobalEnv)), GlobalEnv_funs))
