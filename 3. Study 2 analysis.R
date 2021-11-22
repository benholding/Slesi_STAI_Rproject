#code study 2

pacman::p_load(dplyr, readxl, brms, sjPlot, bayestestR, tidyr, effectsize)

#state anxiety calculation#

S2_STAI_state_raw <- read_excel("data_study2/STAI-all.xlsx", sheet = 1) %>% 
  filter(STAI_T == 0) %>% 
  mutate(STAI_1_rev = 5-STAI_1,
         STAI_2_rev = 5-STAI_2,
         STAI_5_rev = 5-STAI_5,
         STAI_8_rev = 5-STAI_8,
         STAI_10_rev = 5-STAI_10,
         STAI_11_rev = 5-STAI_11,
         STAI_15_rev = 5-STAI_15,
         STAI_16_rev = 5-STAI_16,
         STAI_19_rev = 5-STAI_19,
         STAI_20_rev = 5-STAI_20) %>% 
  mutate(count_na = rowSums(is.na(.))) %>% #counting how many NAs per row
  group_by(SR) %>% 
  filter(count_na < 3) %>% #one participant did not complete the STAI for one of the sessions - 7029 SR
  mutate(STAI_3 = if_else(is.na(STAI_3),#if not many, i'm replacing missing item with median of condition
                          median(STAI_3,na.rm = T),
                          STAI_3))

S2_STAI_state <- S2_STAI_state_raw %>%
  rowwise() %>% 
  mutate(state_anxiety_score = sum(
    STAI_1_rev,STAI_2_rev,STAI_3,STAI_4, STAI_5_rev,STAI_6,STAI_7,
    STAI_8_rev,STAI_9,STAI_10_rev,STAI_11_rev,STAI_12,STAI_13,STAI_14,STAI_15_rev,STAI_16_rev,
    STAI_17,STAI_18,STAI_19_rev,STAI_20_rev)) %>% 
  select(ID,SR, state_anxiety_score)

#trait anxiety calculation#

S2_STAI_trait_raw <- read_excel("data_study2/STAI-all.xlsx", sheet = 1) %>% 
  filter(STAI_T == 1) %>% 
  mutate(STAI_1_rev = 5-STAI_1,
         STAI_6_rev = 5-STAI_6,
         STAI_7_rev = 5-STAI_7,
         STAI_10_rev = 5-STAI_10,
         STAI_13_rev = 5-STAI_13,
         STAI_16_rev = 5-STAI_16,
         STAI_19_rev = 5-STAI_19) %>% 
  mutate(count_na = rowSums(is.na(.))) %>% #counting how many NAs per row
  group_by(SR) %>% 
  filter(count_na < 3) %>% #no one excluded
  mutate(STAI_1 = if_else(is.na(STAI_1),#if not many, i'm replacing missing item with median of condition
                          median(STAI_1,na.rm = T),
                          STAI_1),
         STAI_1_rev = if_else(is.na(STAI_1_rev),
                          median(STAI_1_rev,na.rm = T),
                          STAI_1_rev),
         STAI_17 = if_else(is.na(STAI_17),
                          median(STAI_17,na.rm = T),
                          STAI_17)) %>% 
  ungroup()


S2_STAI_trait <- S2_STAI_trait_raw %>% 
  rowwise() %>% 
  mutate(trait_anxiety_score = sum(STAI_1_rev,STAI_2,STAI_3,STAI_4,STAI_5,
                                   STAI_6_rev,STAI_7_rev,STAI_8,STAI_9,STAI_10_rev,
                                   STAI_11,STAI_12,STAI_13_rev,STAI_14,STAI_15,
                                   STAI_16_rev,STAI_17,STAI_18,STAI_19_rev,STAI_20)) %>% 
  select(ID,trait_anxiety_score) %>% 
  drop_na() %>% #no one excluded
  mutate(trait_anxiety_score_zscore = scale(trait_anxiety_score))

S2_STAI_dataset_preexclusion1 <- S2_STAI_state %>% 
  left_join(S2_STAI_trait, by = "ID") %>% 
  drop_na() %>% 
  mutate(SR = factor(SR, labels = c("Normal sleep", "Sleep restricted"), levels = c(0,1)),
         state_anxiety_score_zscore = scale(state_anxiety_score),
         trait_to_state_change = state_anxiety_score-trait_anxiety_score,
         zscore_trait_to_state_change = state_anxiety_score_zscore-trait_anxiety_score_zscore,
         mean_split_high = as.factor(if_else(trait_anxiety_score_zscore>0, 1,0)),
         top_vs_bottomsd = if_else(trait_anxiety_score_zscore >1, 1,
                                   if_else(trait_anxiety_score_zscore < -1, 0, 77)),
         top_vs_bottomsd = factor(na_if(top_vs_bottomsd, 77), levels = c(0,1), labels = c("Low", "High")))

S2_STAI_dataset_preexclusion2 <- S2_STAI_dataset_preexclusion1 %>% 
  filter(state_anxiety_score_zscore < 3 & state_anxiety_score_zscore > -3) 

S2_STAI_dataset_preexclusion1 %>% 
  mutate(check = if_else(state_anxiety_score_zscore < 3 & state_anxiety_score_zscore > -3, 1, 0)) %>% select(ID, SR, check) %>% filter(check ==0) # two participant sessions removed (8008 SR, 8014 SR)

S2_STAI_dataset <- S2_STAI_dataset_preexclusion2 %>% 
  filter(trait_anxiety_score_zscore < 3 & trait_anxiety_score_zscore > -3)

S2_STAI_dataset_preexclusion2 %>% 
  mutate(check = if_else(trait_anxiety_score_zscore < 3 & trait_anxiety_score_zscore > -3, 1, 0)) %>% select(ID, SR, check) %>% filter(check ==0)  # zero participant sessions removed


#final sample
S2_STAI_dataset %>% group_by(ID) %>% summarise(n()) %>% filter(`n()` <2) # participants with only 1 session (all three were missing SR)

#
### analysis
priors <- set_prior("normal(0,1)", class = "b")

#hypothesis 1: Trait anxiety is positively correlated with state anxiety
model1 <- brm(state_anxiety_score_zscore ~ 1 + (1|ID), data = S2_STAI_dataset, save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 4)
model2 <- brm(state_anxiety_score_zscore ~ trait_anxiety_score_zscore+ (1|ID), data = S2_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 4)
bayestestR::bf_models(model1,model2, denominator = 1, verbose = TRUE)

#hypothesis 2: Sleep restriction leads to an increase in state anxiety
model3 <- brm(state_anxiety_score_zscore ~ SR + (1|ID), data = S2_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(model1,model3, denominator = 1, verbose = TRUE) #3.89e+03
3.89*10^03

#hypothesis 3: Trait anxiety does not predict the effect of sleep restriction on state anxiety.
model4 <- brm(state_anxiety_score_zscore ~ SR + trait_anxiety_score_zscore+ (1|ID), data = S2_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
model5 <- brm(state_anxiety_score_zscore ~ SR*trait_anxiety_score_zscore+ (1|ID), data = S2_STAI_dataset, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(model3,model4, denominator = 1, verbose = TRUE)#6.37e+08
6.37*10^8
bayestestR::bf_models(model4,model5, denominator = 1, verbose = TRUE) #0.219


#hypothesis 4: State anxiety at the two time points (normal sleep and sleep restriction) are correlated
hypothesis4_data <- S2_STAI_dataset %>% select(ID, SR, state_anxiety_score_zscore) %>% pivot_wider(id_col=ID, names_from = SR, values_from = state_anxiety_score_zscore) %>% rename(state_anxiety_score_zscore_NS = `Normal sleep`, state_anxiety_score_zscore_SR = `Sleep restricted`) %>% drop_na()
model6 <- brm(state_anxiety_score_zscore_NS ~ 1, data = hypothesis4_data, save_pars = save_pars(all = TRUE), iter=40000, chains = 8, cores = 8)
model7 <- brm(state_anxiety_score_zscore_NS ~ state_anxiety_score_zscore_SR, data = hypothesis4_data, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bayestestR::bf_models(model6,model7, denominator = 1, verbose = TRUE)














######## robustness checks for hypothesis 3 ########
## same analysis but mean split
meansplit <- brm(state_anxiety_score_zscore ~ SR*mean_split_high, data = STAI_dataset)
plot_model(meansplit, type = "pred", terms = c("mean_split_high","SR"))

## same analysis but top and bottom sd
top_vs_bottom <- brm(state_anxiety_score_zscore ~ SR*top_vs_bottomsd, data = STAI_dataset)
plot_model(top_vs_bottom, type = "pred", terms = c("top_vs_bottomsd","SR"))



#control participants STAI-T
mean(trait$trait_anxiety_score[which(trait$sd == 0)]) 
sd(trait$trait_anxiety_score[which(trait$sd == 0)]) 

#TSD participants STAI-T
mean(trait$trait_anxiety_score[which(trait$sd == 1)], na.rm=T )
sd(trait$trait_anxiety_score[which(trait$sd == 1)], na.rm=T)

t.test(trait$trait_anxiety_score[which(trait$sd == 0)],
       trait$trait_anxiety_score[which(trait$sd == 1)])

#control participants STAI-S
mean(state$state_anxiety_score[which(state$sd == 0)], na.rm=T)
sd(state$state_anxiety_score[which(state$sd == 0)], na.rm=T) 

mean(state$state_anxiety_score[which(state$sd == 1)], na.rm=T) 
sd(state$state_anxiety_score[which(state$sd == 1)], na.rm = T)