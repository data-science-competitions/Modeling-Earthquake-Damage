library(tic)
invisible(sapply(list.files("./.tic", full.names = TRUE), source))

# Stage: Before Install --------------------------------------------------------
get_stage("before_install") %>%
    add_step(step_install_development_tools()) %>%
    add_step(step_install_deps())

# Stage: Install
get_stage("install") %>%
    add_step(step_install_local_package())

# Stage: Before Script ---------------------------------------------------------
get_stage("before_script") %>%
    add_step(step_run_code(devtools::document()))

# Stage: Script ----------------------------------------------------------------
get_stage("script") %>%
    add_step(step_build_and_check(job_name = ci_get_job_name())) %>%
    add_step(step_run_test_suite(job_name = ci_get_job_name())) %>%
    add_step(step_render_report(job_name = ci_get_job_name()))

# Stage: After Failure ----------------------------------------------------------
get_stage("after_failure") %>%
    add_step(step_run_code(show_error_log()))
