# code gender analysis post review
demographics_study2 <- read_excel("data_study2/Demographics_FINAL.xlsx", sheet = 1)

S2_STAI_datasetD <- S2_STAI_dataset %>% left_join(demographics_study2, by = "ID")
## study 1

#hypothesis 2: Sleep deprivation leads to an increase in state anxiety
study1_fullmodel_plus_gender_step0 <- brm(state_anxiety_score_zscore ~ 1, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
study1_fullmodel_plus_gender_step2a <- brm(state_anxiety_score_zscore ~ woman, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(study1_fullmodel_plus_gender_step0,study1_fullmodel_plus_gender_step2a, denominator = 1, verbose = TRUE)

study1_fullmodel_plus_gender_step2b <- brm(state_anxiety_score_zscore ~ woman + sd, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(study1_fullmodel_plus_gender_step2a,study1_fullmodel_plus_gender_step2b, denominator = 1, verbose = TRUE)

#hypothesis 3: Trait anxiety does not predict the effect of sleep deprivation on state anxiety.
study1_fullmodel_plus_gender_step3 <- brm(state_anxiety_score_zscore ~ woman + sd + trait_anxiety_score_zscore, data = S1_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bf_models(study1_fullmodel_plus_gender_step2b,study1_fullmodel_plus_gender_step3, denominator = 1, verbose = TRUE)

study1_fullmodel_plus_gender_step4 <- brm(state_anxiety_score_zscore ~ woman + sd*trait_anxiety_score_zscore, data = S1_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors, iter=40000, chains = 8, cores = 8)
bf_models(study1_fullmodel_plus_gender_step3,study1_fullmodel_plus_gender_step4, denominator = 1, verbose = TRUE)







## study 2

#hypothesis 3: Trait anxiety does not predict the effect of sleep restriction on state anxiety.
study2_fullmodel_plus_gender_1 <- brm(state_anxiety_score_zscore ~ 1 + (1|ID), data = S2_STAI_datasetD, save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
study2_fullmodel_plus_gender_2 <- brm(state_anxiety_score_zscore ~ female + (1|ID), data = S2_STAI_datasetD, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
study2_fullmodel_plus_gender_3 <- brm(state_anxiety_score_zscore ~ female + SR + (1|ID), data = S2_STAI_datasetD, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
study2_fullmodel_plus_gender_4 <- brm(state_anxiety_score_zscore ~ female + SR + trait_anxiety_score_zscore + (1|ID), data = S2_STAI_datasetD, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
study2_fullmodel_plus_gender_5 <- brm(state_anxiety_score_zscore ~ female + SR*trait_anxiety_score_zscore + (1|ID), data = S2_STAI_datasetD, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)

bayestestR::bf_models(study2_fullmodel_plus_gender_1,study2_fullmodel_plus_gender_2, denominator = 1, verbose = TRUE)
bayestestR::bf_models(study2_fullmodel_plus_gender_2,study2_fullmodel_plus_gender_3, denominator = 1, verbose = TRUE)
bayestestR::bf_models(study2_fullmodel_plus_gender_3,study2_fullmodel_plus_gender_4, denominator = 1, verbose = TRUE)
bayestestR::bf_models(study2_fullmodel_plus_gender_4,study2_fullmodel_plus_gender_5, denominator = 1, verbose = TRUE)


#making tables of the main model output
tab_model(study1_fullmodel_plus_gender_step4, file = "tables/TableS4a. gender covariate.doc") #study 1
tab_model(study2_fullmodel_plus_gender_5, file = "tables/TableS4b. gender covariate.doc") #study 2
