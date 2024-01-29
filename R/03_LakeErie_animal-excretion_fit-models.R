  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(lindia) # to look at model diagnostics
  library(performance) # to compare models
  library(car) # for Anova() function
<<<<<<< HEAD
<<<<<<< HEAD
  library(emmeans) # for posthoc
  library(lme4) # to add random effect to lm
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
  library(boot) # for boostrapping
  
  # ANOVA ----
  # ..Figure 1 - Season effect ----
<<<<<<< HEAD
<<<<<<< HEAD
  aovN.seas <- lm(log10(masscorr.N.excr) ~ Season * Species.code, data = excr)
  Anova(aovN.seas)
  aovP.seas <- lm(log10(masscorr.P.excr) ~ Season * Species.code, data = excr)
  Anova(aovP)
  
  # ..Figure 2 - Season effect ----
  aovN.sp <- lm(log10(masscorr.N.excr) ~ Species.code, data = excr)
  Anova(aovN.seas)
  aovP.seas <- lm(log10(masscorr.P.excr) ~ Season * Species.code, data = excr)
  Anova(aovP)
  
  # need posthoc for species
  # LM & emmeans ----
  emm <- function(lm){
    # pairwise comparisons using emmeans
    m1 <- emmeans(lm, pairwise ~ Species.code, type = 'response')
    m2 <- emmeans(lm, pairwise ~ Species.code, type = 'response')
    contrasts <- m1$contrasts %>% summary(infer = T) %>% 
      as_tibble() %>%  arrange(p.value)
    print(m2)
    #print(contrasts, n = 40)
    
    # Extracting effects from emmeans
    emm_df <- as_tibble(m1$emmeans)
    
    # Return a list containing the results
    result_list <- list(contrasts = contrasts, 
                        emmeans = emm_df)
    return(result_list)
  }
  
  pwcN <- emm(aovN.sp)
  
  # GLM ----
  # data distribution
  hist(excr$masscorr.N.excr)
  
  # ..Figure 2 - Season effect ----
  lmN.temp <- lmer(log10(masscorr.N.excr) ~ Temp + (1|Species.code), data = excr)
  lmN.temp2 <- lm(log10(masscorr.N.excr) ~ Temp, data = excr)
  check_model(lmN.temp)
  AIC(lmN.temp, lmN.temp2)
  Anova(lmN.temp)
  summary(lmN.temp)
  
  lmP.temp <- lmer(log10(masscorr.P.excr) ~ Temp + (1|Species.code), data = excr)
  lmP.temp2 <- lm(log10(masscorr.P.excr) ~ Temp, data = excr)
  check_model(lmP.temp)
  AIC(lmP.temp, lmP.temp2)
  Anova(lmP.temp)
  
  # ..Figure 2 - stable isotopes ----
  corr(excr$Season, excr$d15N)
  
  # make lm and lmer + model performance function
  perf_si <- function(x, y, data) {
    df <- data %>%  filter(!is.na(data[[y]]))
    df$log10.y <- log10(df[[y]])
    
    m1 <- lm(log10.y ~ df[[x]], data = df)
    m2 <- lmer(log10.y ~ df[[x]] + (1|Species.code), data = df)
    m3 <- lmer(log10.y ~ df[[x]] * Season + (1|Species.code), data = df)
    
    perf <- compare_performance(m1, m2, m3, rank = T)
    return(perf)
  }
  
  # N excretion vs d15N
  perf_si("d15N", "masscorr.N.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  NexcrSI.15N <- lmer(log10(masscorr.N.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  check_model(NexcrSI.15N)
  Anova(NexcrSI.15N, type = 'III')
  summary(NexcrSI.15N)
  
  # N excretion vs d13C
  perf_si("d13C", "masscorr.N.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  NexcrSI.13C <- lmer(log10(masscorr.N.excr) ~ d13C + (1|Species.code), 
                      data = excr)
  check_model(NexcrSI.13C)
  Anova(NexcrSI.13C, type = 'III')
  summary(NexcrSI.13C)

  NexcrSI.null <- lmer(log10(masscorr.N.excr) ~ (1|Species.code), 
                       data = excr %>% filter(!is.na(d15N)))
  Anova(NexcrSI.null, type = 'III')
  AIC(NexcrSI.15N, NexcrSI.13C, NexcrSI.null)
  
  # P excretion vs d15N
  perf_si("d15N", "masscorr.P.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  PexcrSI.15N <- lmer(log10(masscorr.P.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  check_model(PexcrSI.15N)
  Anova(PexcrSI.15N, type = 'III')
  summary(PexcrSI.15N)
  
  # P excretion vs d13C
  perf_si("d13C", "masscorr.P.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  PexcrSI.13C <- lmer(log10(masscorr.P.excr) ~ d13C + (1|Species.code), 
                      data = excr)
  check_model(PexcrSI.13C)
  Anova(PexcrSI.13C, type = 'III')
  summary(PexcrSI.13C)
  
  PexcrSI.null <- lmer(log10(masscorr.P.excr) ~ (1|Species.code), 
                       data = excr %>% filter(!is.na(d15N)))
  Anova(PexcrSI.null, type = 'III')
  AIC(PexcrSI.15N, PexcrSI.13C, PexcrSI.null)
=======
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
  excr.verts <- excr %>% dplyr::filter(Species.code != 'QM')
  anova(lm(log10(massnorm.N.excr) ~ Incub..Temperature, data = excr.verts))
  anovaN <- lm(log10(massnorm.N.excr) ~ Season*Species.code, data = excr.verts)
  anova(anovaN)
  
  # GLM ----
  # data distribution
  hist(excr$massnorm.N.excr)
  
  # make glm function
  glm_si <- function(x, y) {
    m <- glm(log10(x) ~ y*Season, 
             data = excr %>% filter(Species.code == 'QM'))
    plot(m)
    Anova(m, type = 'III')
    return(m)
  }
  # ..Figure 2 - stable isotopes ----
  # N excretion vs d15N
  NexcrSI.15N <- glm(massnorm.N.excr ~ d15N, 
                     data = excr, family = Gamma(link = 'log'))
  
  NexcrSI.15N <- lm(log10(massnorm.N.excr) ~ d15N*Season, 
                    data = excr)
  plot(NexcrSI.15N)
  gg_diagnose(NexcrSI.15N)
  Anova(NexcrSI.15N, type = 'III')
  NexcrSI.null <- lm(Log10.massnorm.N.excr ~ 1,
                     data = excr)
  Anova(NexcrSI.null, type = 'III')
  anova(NexcrSI, NexcrSI.null)
  compare_performance(NexcrSI, NexcrSI.null, rank = T)
  
  # N excretion vs d13C
  NexcrSI.13C <- lm(log10(massnorm.N.excr) ~ d13C*Season, 
                    data = excr)
  plot(NexcrSI.15N)
  gg_diagnose(NexcrSI.13C)
  Anova(NexcrSI.13C, type = 'III')
  NexcrSI.null <- lm(Log10.massnorm.N.excr ~ 1,
                     data = excr)
  Anova(NexcrSI.null, type = 'III')
  anova(NexcrSI, NexcrSI.null)
  compare_performance(NexcrSI, NexcrSI.null, rank = T)
  
  # P excretion vs d15N
  PexcrSI.15N <- lm(log10(massnorm.P.excr) ~ d15N*Season, 
                    data = excr)
  Anova(PexcrSI.15N, type = 'III')
  summary(PexcrSI.15N)
  gg_diagnose(PexcrSI.15N)
  
  PexcrSI.null <- lm(Log10.massnorm.P.excr ~ 1,
                     data = excr)
  Anova(PexcrSI.null, type = 'III')
  
  compare_performance(PexcrSI, PexcrSI.null, rank = T)
  
  # P excretion vs d13C
  PexcrSI.13C <- lm(log10(massnorm.P.excr) ~ d13C, 
                    data = excr)
  Anova(PexcrSI.13C, type = 'III')
  gg_diagnose(PexcrSI.13C)
  
  PexcrSI.null <- lm(Log10.massnorm.P.excr ~ 1,
                     data = excr)
  Anova(PexcrSI.null, type = 'III')
  
  compare_performance(PexcrSI, PexcrSI.null, rank = T)

<<<<<<< HEAD
>>>>>>> 561e07aec1796006abeee7626848271440840a25
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
  
  # misc
  
  ggplot(excr.verts %>% filter(Season == 'F',
                               Species.code == 'YP'), 
         aes(x = Time.elapsed..min.,
             y = Log10.massnorm.N.excr)) +
    geom_point()
  
  anova(lm(Log10.massnorm.N.excr ~ Time.elapsed..min., 
           data = excr.verts %>%  filter(Season == 'F',
                                         Species.code == 'LP')))
  
  