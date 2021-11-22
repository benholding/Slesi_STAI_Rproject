pacman::p_load(sjPlot,ggpubr)

#descriptives

table1_part1 <- S1_STAI_dataset %>% 
  group_by(sd) %>% 
  summarise(
    mean_state_anxiety_score = mean(state_anxiety_score),
    sd_state_anxiety_score = sd(state_anxiety_score), 
    range_state_anxiety_score = paste0(min(state_anxiety_score), ", ", max(state_anxiety_score)),
    mean_trait_anxiety_score = mean(trait_anxiety_score), 
    sd_trait_anxiety_score = sd(trait_anxiety_score),
    range_trait_anxiety_score = paste0(min(trait_anxiety_score), ", ", max(trait_anxiety_score))) %>% 
  rename(condition = sd)

table1_part2 <- rbind(table1_part1,c(NA,0,0,NA,0,0,NA))

table1_part3 <- S2_STAI_dataset %>% 
  group_by(SR) %>% 
  summarise(
    mean_state_anxiety_score = mean(state_anxiety_score),
    sd_state_anxiety_score = sd(state_anxiety_score), 
    range_state_anxiety_score = paste0(min(state_anxiety_score), ", ", max(state_anxiety_score)),
    mean_trait_anxiety_score = mean(trait_anxiety_score), 
    sd_trait_anxiety_score = sd(trait_anxiety_score),
    range_trait_anxiety_score = paste0(min(trait_anxiety_score), ", ", max(trait_anxiety_score)))%>% 
  rename(condition = SR)

table1 <- rbind(table1_part2, table1_part3)
write.csv(table1, "tables/table1.csv")

#tables
tab_model(step4, file = "tables/Table2. results from study 1.doc") #study 1
tab_model(model5, file = "tables/Table3. results from study 2.doc") #study 2

#plots
study1_plot <- plot_model(step4, type = "pred", terms = c("trait_anxiety_score_zscore","sd"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 1. Sleep deprivation", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep deprivation state anxiety (z-score)") +
  xlab("Trait anxiety (z-score)") +
  ylim(-2, 3) +
  xlim(-2, 2.6)

study2_plot <- plot_model(model5, type = "pred", terms = c("trait_anxiety_score_zscore","SR"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 2. Sleep restriction", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep restriction state anxiety (z-score)") +
  xlab("Trait anxiety (z-score)") +
  ylim(-2, 3) +
  xlim(-2, 2.6)


panelplot <- ggarrange(study1_plot, study2_plot, common.legend = T, legend = "bottom")
cowplot::save_plot(panelplot,filename = "plots/Figure1. interaction.pdf", device = "pdf", ncol=1.5, nrow = 1.2)


## robustness checks ##

## same analysis but top and bottom sd
#STUDY 1#
S1_top_vs_bottom <- brm(state_anxiety_score_zscore ~ sd*top_vs_bottomsd, data = S1_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
S1_top_vs_bottom_plot <- plot_model(S1_top_vs_bottom, type = "pred", terms = c("top_vs_bottomsd","sd"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 1. Sleep deprivation", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep deprivation state anxiety (z-score)") +
  xlab("Trait anxiety") 

S1_top_vs_bottom_intercept <- brm(state_anxiety_score_zscore ~ 1, data = S1_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
S1_top_vs_bottom_sd <- brm(state_anxiety_score_zscore ~ sd, data = S1_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
S1_top_vs_bottom_anxiety <- brm(state_anxiety_score_zscore ~ sd + top_vs_bottomsd, data = S1_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bf_models(S1_top_vs_bottom_intercept,S1_top_vs_bottom_sd, denominator = 1, verbose = TRUE) #BF = 0.936
bf_models(S1_top_vs_bottom_sd,S1_top_vs_bottom_anxiety, denominator = 1, verbose = TRUE) #BF = 23100
bf_models(S1_top_vs_bottom_anxiety,S1_top_vs_bottom, denominator = 1, verbose = TRUE) #BF = 0.471

#STUDY 2#
S2_top_vs_bottom <- brm(state_anxiety_score_zscore ~ SR*top_vs_bottomsd + (1|ID), data = S2_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
S2_top_vs_bottom_plot <- plot_model(S2_top_vs_bottom, type = "pred", terms = c("top_vs_bottomsd","SR"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 2. Sleep restriction", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep restriction state anxiety (z-score)") +
  xlab("Trait anxiety") 

S2_top_vs_bottom_intercept <- brm(state_anxiety_score_zscore ~ 1 + (1|ID), data = S2_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
S2_top_vs_bottom_sd <- brm(state_anxiety_score_zscore ~ SR + (1|ID), data = S2_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)) %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
S2_top_vs_bottom_anxiety <- brm(state_anxiety_score_zscore ~ SR + top_vs_bottomsd + (1|ID), data = S2_STAI_dataset %>% filter(!is.na(top_vs_bottomsd)) %>% filter(!is.na(top_vs_bottomsd)), save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bf_models(S2_top_vs_bottom_intercept,S2_top_vs_bottom_sd, denominator = 1, verbose = TRUE) #BF = 0.634
bf_models(S2_top_vs_bottom_sd,S2_top_vs_bottom_anxiety, denominator = 1, verbose = TRUE) #BF = 2490
bf_models(S2_top_vs_bottom_anxiety,S2_top_vs_bottom, denominator = 1, verbose = TRUE) #BF = 0.547




supplement_panelplot <- ggarrange(S1_top_vs_bottom_plot, S2_top_vs_bottom_plot, common.legend = T, legend = "bottom")
cowplot::save_plot(supplement_panelplot,filename = "plots/FigureS1. topvsbottomsd.pdf", device = "pdf", ncol=1.5, nrow = 1.2)

tab_model(S1_top_vs_bottom, file = "tables/TableS1. robustness results from study 1.doc") #study 1
tab_model(S2_top_vs_bottom, file = "tables/TableS2. robustness results from study 2.doc") #study 2


### robustness - without exclusions ###
noexclusions_study1_model <- brm(state_anxiety_score_zscore ~ sd*trait_anxiety_score_zscore, data = S1_STAI_dataset_preexclusions1, save_pars = save_pars(all = TRUE), prior = priors, iter=40000, chains = 8, cores = 8)
noexclusions_study2_model <- brm(state_anxiety_score_zscore ~ SR*trait_anxiety_score_zscore + (1|ID), data = S2_STAI_dataset_preexclusion1, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)

tab_model(noexclusions_study1_model, file = "tables/TableS2a. no outliers removed study 1.doc") #study 1
tab_model(noexclusions_study2_model, file = "tables/TableS2b. no outliers removed study 2.doc") #study 2


noexclusions_study1_model_plot <- plot_model(noexclusions_study1_model, type = "pred", terms = c("trait_anxiety_score_zscore","sd"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 1. Sleep deprivation", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep deprivation state anxiety (z-score)") +
  xlab("Trait anxiety (z-score)") +
  ylim(-1.95, 4) +
  xlim(-2.1, 4.5)

noexclusions_study2_model_plot <- plot_model(noexclusions_study2_model, type = "pred", terms = c("trait_anxiety_score_zscore","SR"),show.data = T,jitter =0.2,colors = c("slategray", "firebrick"), title = "Study 2. Sleep restriction", legend.title= "") +
  theme_sjplot() +
  ylab("Post sleep restriction state anxiety (z-score)") +
  xlab("Trait anxiety (z-score)") +
  ylim(-1.95, 4) +
  xlim(-2.1, 4.5)

supplement2_panelplot <- ggarrange(noexclusions_study1_model_plot, noexclusions_study2_model_plot, common.legend = T, legend = "bottom")
cowplot::save_plot(supplement2_panelplot,filename = "plots/FigureS2. none_excluded.pdf", device = "pdf", ncol=1.5, nrow = 1.2)

#BF study 1
noexclusions_study1_model_intercept <- brm(state_anxiety_score_zscore ~ 1, data = S1_STAI_dataset_preexclusions1, save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
noexclusions_study1_model_sd <- brm(state_anxiety_score_zscore ~ sd, data = S1_STAI_dataset_preexclusions1, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
noexclusions_study1_model_anxiety <- brm(state_anxiety_score_zscore ~ sd + trait_anxiety_score_zscore, data = S1_STAI_dataset_preexclusions1, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bf_models(noexclusions_study1_model_intercept,noexclusions_study1_model_sd, denominator = 1, verbose = TRUE) #BF = 10.35
bf_models(noexclusions_study1_model_sd,noexclusions_study1_model_anxiety, denominator = 1, verbose = TRUE) #BF = 1.92e+11
bf_models(noexclusions_study1_model_anxiety,noexclusions_study1_model, denominator = 1, verbose = TRUE) #BF = 0.152

#BF study 2
noexclusions_study2_model_intercept <- brm(state_anxiety_score_zscore ~ 1 + (1|ID), data = S2_STAI_dataset_preexclusion1, save_pars = save_pars(all = TRUE),iter=40000, chains = 8, cores = 8)
noexclusions_study2_model_SR <- brm(state_anxiety_score_zscore ~ SR + (1|ID), data = S2_STAI_dataset_preexclusion1, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
noexclusions_study2_model_anxiety <- brm(state_anxiety_score_zscore ~ SR + trait_anxiety_score_zscore + (1|ID), data = S2_STAI_dataset_preexclusion1, save_pars = save_pars(all = TRUE), prior = priors,iter=40000, chains = 8, cores = 8)
bf_models(noexclusions_study2_model_intercept,noexclusions_study2_model_SR, denominator = 1, verbose = TRUE) #BF = 12000
bf_models(noexclusions_study2_model_SR,noexclusions_study2_model_anxiety, denominator = 1, verbose = TRUE) #BF = 13000000000
bf_models(noexclusions_study2_model_anxiety,noexclusions_study2_model, denominator = 1, verbose = TRUE) #BF = 0.124
