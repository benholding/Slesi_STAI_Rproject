#required packages
pacman::p_load(dplyr, readxl, readr, brms, sjPlot, bayestestR, tidyr, effectsize)

options(scipen = 100)
#importing data
key <- read_csv("data_study1/Masterkey.csv") %>% rename_all(tolower)

### state ###
S1_STAI_state_raw <- readxl::read_excel("data_study1/STAI-S clean.xlsx") %>% 
  mutate(STS_1_rev = 5-STS_1,
         STS_2_rev = 5-STS_2,
         STS_5_rev = 5-STS_5,
         STS_8_rev = 5-STS_8,
         STS_10_rev = 5-STS_10,
         STS_11_rev = 5-STS_11,
         STS_15_rev = 5-STS_15,
         STS_16_rev = 5-STS_16,
         STS_19_rev = 5-STS_19,
         STS_20_rev = 5-STS_20) %>% 
  mutate(count_na = rowSums(is.na(.))) %>%#counting how many NAs per row
  filter(count_na < 2) #one participant did not complete the state STAI (133, NS)

S1_STAI_state <- S1_STAI_state_raw %>%
  rowwise() %>% 
  mutate(state_anxiety_score = sum(STS_1_rev,STS_2_rev,STS_3,STS_4,STS_5_rev,
                                   STS_6,STS_7,STS_8_rev,STS_9,STS_10_rev,
                                   STS_11_rev,STS_12,STS_13,STS_14,STS_15_rev,
                                 STS_16_rev,STS_17,STS_18,STS_19_rev,STS_20_rev)) %>% 
  select(id,state_anxiety_score)

### trait ###

S1_STAI_trait_raw <- readxl::read_excel("data_study1/STAI-T clean.xlsx") %>% 
  mutate(STT_1_rev = 5-STT_1,
         STT_6_rev = 5-STT_6,
         STT_7_rev = 5-STT_7,
         STT_10_rev = 5-STT_10,
         STT_13_rev = 5-STT_13,
         STT_16_rev = 5-STT_16,
         STT_19_rev = 5-STT_19) %>% 
  mutate(count_na = rowSums(is.na(.))) %>%#counting how many NAs per row
  filter(count_na < 2) #one participant did not complete the trait STAI (275, SD)

S1_STAI_trait <- S1_STAI_trait_raw %>% 
  rowwise() %>% 
  mutate(trait_anxiety_score = sum(STT_1_rev,STT_2,STT_3,STT_4,STT_5,
                                   STT_6_rev,STT_7_rev,STT_8,STT_9,STT_10_rev,
                                   STT_11,STT_12,STT_13_rev,STT_14,STT_15,
                                   STT_16_rev,STT_17,STT_18,STT_19_rev,STT_20)) %>% 
  select(id,trait_anxiety_score)

S1_STAI_dataset_preexclusions1 <- key %>% 
  left_join(S1_STAI_trait) %>% 
  left_join(S1_STAI_state) %>% 
  drop_na() %>% #removes two participants (133, 275)
  mutate(sd = factor(sd, labels = c("Normal sleep", "Sleep loss")),
         woman = as.factor(woman),
         state_anxiety_score_zscore = scale(state_anxiety_score),
         trait_anxiety_score_zscore = scale(trait_anxiety_score),
         trait_to_state_change = state_anxiety_score-trait_anxiety_score,
         zscore_trait_to_state_change = state_anxiety_score_zscore-trait_anxiety_score_zscore,
         mean_split_high = as.factor(if_else(trait_anxiety_score_zscore>0, 1,0)),
         top_vs_bottomsd = if_else(trait_anxiety_score_zscore >1, 1,
                                             if_else(trait_anxiety_score_zscore < -1, 0, 77)),
         top_vs_bottomsd = factor(na_if(top_vs_bottomsd, 77), levels = c(0,1), labels = c("Low", "High")))

S1_STAI_dataset_preexclusions2 <- S1_STAI_dataset_preexclusions1 %>% 
  filter(state_anxiety_score_zscore < 3 & state_anxiety_score_zscore > -3)
length(unique(S1_STAI_dataset_preexclusions1$id))-length(unique(S1_STAI_dataset_preexclusions2$id)) #1 removed due to state over 3sd away

S1_STAI_dataset_preexclusions1 %>% #the 1 removed was 180 normal sleep
  mutate(check = if_else(state_anxiety_score_zscore < 3 & state_anxiety_score_zscore > -3, 1, 0 )) %>% select(id, sd, check) %>% filter(check == 0)

S1_STAI_dataset <- S1_STAI_dataset_preexclusions2 %>% 
         filter(trait_anxiety_score_zscore < 3 & trait_anxiety_score_zscore > -3)
length(unique(S1_STAI_dataset_preexclusions2$id))-length(unique(S1_STAI_dataset$id)) #2 removed due to trait over 3sd away

S1_STAI_dataset_preexclusions2 %>% #the 2 removed were 223 normal sleep, 280 normal sleep
  mutate(check = if_else(trait_anxiety_score_zscore < 3 & trait_anxiety_score_zscore > -3, 1, 0 )) %>% select(id, sd, check) %>% filter(check == 0)
### analysis

priors <- set_prior("normal(0,1)", class = "b")

#hypothesis 1: Trait anxiety is positively correlated with state anxiety
step0 <- brm(state_anxiety_score_zscore ~ 1, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
step1 <- brm(state_anxiety_score_zscore ~ trait_anxiety_score_zscore, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(step0,step1, denominator = 1, verbose = TRUE)

#hypothesis 2: Sleep deprivation leads to an increase in state anxiety
step2 <- brm(state_anxiety_score_zscore ~ sd, data = S1_STAI_dataset,save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(step0,step2, denominator = 1, verbose = TRUE)
#hypothesis 3: Trait anxiety does not predict the effect of sleep restriction on state anxiety.
step3 <- brm(state_anxiety_score_zscore ~ sd + trait_anxiety_score_zscore, data = S1_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
step4 <- brm(state_anxiety_score_zscore ~ sd*trait_anxiety_score_zscore, data = S1_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors, iter=40000, chains = 8, cores = 8)

bf_models(step2,step4, denominator = 1, verbose = TRUE)
8.54*10^9

bf_models(step3,step4, denominator = 1, verbose = TRUE)



