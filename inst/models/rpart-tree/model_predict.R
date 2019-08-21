#' @title Predict a Test Set Using Archived Models
#' @param test_set (`data.frame`) A table where samples are in rows and features are in columns.
model_predict <- function(test_set)
{
    ###########################
    ## Defensive Programming ##
    ###########################
    ## Do not edit this part by hand
    assertive::assert_has_rows(test_set)
    assertive::assert_all_are_existing(
        c("model_uid", "split_num", "dataset_key_column", "model_archive"),
        envir = .GlobalEnv)
    ## Here you may add your assertions


    ###########
    ## Setup ##
    ###########
    # Compose stages tag slugs
    previous_stage_tags <- rmonic::compose_tags(slug_model_fit, split = split_num)


    ##########################
    ## Predict the Test Set ##
    ##########################
    ## Retrieve the prediction models keys
    models_md5hashes <- find_artifacts(tags = previous_stage_tags, path = model_archive)
    ## Use the models to predict the test set
    foreach(m = seq_along(models_md5hashes)) %dopar% {
        ## Retrieve the model
        md5hash <- models_md5hashes[m]
        model_object <- load_artifact(tags = md5hash, path = model_archive)
        model_meta <- get_artifact_tags(artifact = md5hash, path = model_archive) %>% decompose_tags()
        model_meta["source"] <- "model_predict"
        assertive::assert_is_non_empty_model(model_object)
        ## Predict the test set
        response_vars <- base_learner_predict(data = test_set, model_object = model_object)
        assert_objects_have_the_same_number_of_observations(test_set, response_vars)
        ## Structure the prediction data in a key-value table
        uids <- test_set[,dataset_key_column]
        y_fit <- kv_table(key = uids, value = response_vars[,"fit"])
        # y_upr <- kv_table(key = uids, value = response_vars[,"upr"])
        # y_lwr <- kv_table(key = uids, value = response_vars[,"lwr"])
        ## Compose artifacts tags
        tags_fit <- compose_tags(model_meta, response_type = "fit")
        # tags_upr <- compose_tags(model_meta, response_type = "upr")
        # tags_lwr <- compose_tags(model_meta, response_type = "lwr")
        ## Save prediction tables in database
        save_artifact(y_fit, tags_fit, model_archive)
        # save_artifact(y_upr, tags_upr, model_archive)
        # save_artifact(y_lwr, tags_lwr, model_archive)
    }


    ############
    ## Return ##
    ############
    ## Do not edit this part by hand
    return(invisible())
}
