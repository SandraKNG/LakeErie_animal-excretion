  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(lindia) # to look at model diagnostics
  library(performance) # to compare models
  library(car) # for Anova() function
  library(emmeans) # for posthoc
  library(lmerTest) # to add random effect to lm + F-test/p-value
  library(writexl)
  library(rstatix) # kruskal_test
  library(ARTool) # for ART ANOVA

  # ANOVA ----
  # ..Figure 1 - Species ----
  kkN.sp <- excr %>% kruskal_test(log10(masscorr.N.excr) ~ Species.code)
  kkN.sp
  dunnN.sp <- excr %>% dunn_test(log10.masscorr.N.excr ~ Species.code)
  dunnN.sp
  
  kkP.sp <- excr %>% kruskal_test(log10(masscorr.P.excr) ~ Species.code)
  kkP.sp
  dunnP.sp <- excr %>% dunn_test(log10.masscorr.P.excr ~ Species.code)
  dunnP.sp
  
  kkNP.sp <- excr %>% kruskal_test(log10(masscorr.NP.excr) ~ Species.code)
  kkNP.sp
  dunnNP.sp <- excr %>% dunn_test(log10.masscorr.NP.excr ~ Species.code)
  dunnNP.sp
 
  # ..Figure 2 - Season  ----
  kkN.seas <- excr %>%  kruskal_test(log10(masscorr.N.excr) ~ Season)
  kkN.seas
  
  kkP.seas <- excr %>%  kruskal_test(log10(masscorr.P.excr) ~ Season)
  kkP.seas

  kkNP.seas <- excr %>%  kruskal_test(log10(masscorr.NP.excr) ~ Season)
  kkNP.seas
  
  # ..Figure S1 - Season sub ----
  excr.seas.sub <- excr %>%  filter(Species.code %in% c('GS', 'LB'))
  
  kkN.seas.sub <- excr.seas.sub %>%  kruskal_test(log10(masscorr.N.excr) ~ Season)
  kkN.seas.sub
  
  kkP.seas.sub <- excr.seas.sub %>%  kruskal_test(log10(masscorr.P.excr) ~ Season)
  kkP.seas.sub
  
  kkNP.seas.sub <- excr.seas.sub %>%  kruskal_test(log10(masscorr.NP.excr) ~ Season)
  kkNP.seas.sub
 
  # make kruskal table based on all sp models ----
  kruskal_sp_models <- list(
    "Mass-specific N excretion" = kkN.sp,
    "Mass-specific P excretion" = kkP.sp,
    "Mass-specific N:P excretion" = kkNP.sp
  )
  combined_kruskal_sp <- bind_rows(lapply(names(kruskal_sp_models), function(model_name) {
    model <- kruskal_sp_models[[model_name]]
    kruskal_result <- rownames_to_column(model, 
                                       var = "Predictors") %>% as_tibble
    kruskal_result$model <- model_name
    kruskal_result <- kruskal_result %>% 
      mutate(Predictors = ifelse(
        Predictors == 1,
        'Species', Predictors)) %>% 
      select(model, Predictors, n, statistic, df, p)
    return(kruskal_result)
  }))
 
  # make kruskal table based on all season models ----
  kruskal_seas_models <- list(
    "Mass-specific N excretion" = kkN.seas,
    "Mass-specific P excretion" = kkP.seas,
    "Mass-specific N:P excretion" = kkNP.seas
  )
  combined_kruskal_seas <- bind_rows(lapply(names(kruskal_seas_models), function(model_name) {
    model <- kruskal_seas_models[[model_name]]
    kruskal_result <- rownames_to_column(model, 
                                         var = "Predictors") %>% as_tibble
    kruskal_result$model <- model_name
    kruskal_result <- kruskal_result %>% 
      mutate(Predictors = ifelse(
        Predictors == 1,
        'Sampling', Predictors)) %>% 
      select(model, Predictors, n, statistic, df, p)
    return(kruskal_result)
  }))
  
  # make kruskal table based on all season sub models ----
  kruskal_seas_sub_models <- list(
    "Mass-specific N excretion" = kkN.seas.sub,
    "Mass-specific P excretion" = kkP.seas.sub,
    "Mass-specific N:P excretion" = kkNP.seas.sub
  )
  combined_kruskal_seas_sub <- bind_rows(lapply(names(kruskal_seas_sub_models), function(model_name) {
    model <- kruskal_seas_sub_models[[model_name]]
    kruskal_result <- rownames_to_column(model, 
                                         var = "Predictors") %>% as_tibble
    kruskal_result$model <- model_name
    kruskal_result <- kruskal_result %>% 
      mutate(Predictors = ifelse(
        Predictors == 1,
        'Sampling', Predictors)) %>% 
      select(model, Predictors, n, statistic, df, p)
    return(kruskal_result)
  }))
  
  # modify posthoc dunn table ----
  dunn_sp_models <- list(
    "Mass-specific N excretion" = dunnN.sp,
    "Mass-specific P excretion" = dunnP.sp,
    "Mass-specific N:P excretion" = dunnNP.sp
  )
  combined_dunn_sp <- bind_rows(lapply(names(dunn_sp_models), function(model_name) {
    model <- dunn_sp_models[[model_name]]
    dunn_result <- rownames_to_column(model,
                                         var = "Predictors") %>% as_tibble
    dunn_result$model <- model_name
    dunn_result <- dunn_result %>% 
      select(c(model, group1, group2, n1, n2, statistic, p.adj))
    return(dunn_result)
  }))
  
  # LMER ----
  # data distribution
  hist(excr$masscorr.N.excr)
  
  # ..Figure 2 - Temperature ----
  lmN.temp <- lmer(log10(masscorr.N.excr) ~ Temp + (1|Species.code), data = excr)
  lmN.temp2 <- lm(log10(masscorr.N.excr) ~ Temp, data = excr)
  lmN.temp <- lm(log10(masscorr.N.excr) ~ Temp * Species.code, data = excr)
  check_model(lmN.temp)
  AIC(lmN.temp, lmN.temp2)
  anova(lmN.temp)
  summary(lmN.temp)
  
  lmP.temp <- lmer(log10(masscorr.P.excr) ~ Temp + (1|Species.code), data = excr)
  lmP.temp2 <- lm(log10(masscorr.P.excr) ~ Temp, data = excr)
  check_model(lmP.temp)
  AIC(lmP.temp, lmP.temp2)
  anova(lmP.temp)
  
  lmNP.temp <- lmer(log10(masscorr.NP.excr) ~ Temp + (1|Species.code), data = excr)
  lmNP.temp2 <- lm(log10(masscorr.NP.excr) ~ Temp, data = excr)
  check_model(lmNP.temp)
  AIC(lmNP.temp, lmNP.temp2)
  anova(lmNP.temp)
  
  # Define your list of models
  anova_temp_models <- list(
    "Mass-specific N excretion" = lmN.temp,
    "Mass-specific P excretion" = lmP.temp,
    "Mass-specific N:P excretion" = lmNP.temp
  )
  
  # Combine ANOVA results into a single data frame
  combined_anova_temp <- bind_rows(lapply(names(anova_temp_models), function(model_name) {
    model <- anova_temp_models[[model_name]]
    anova_result <- rownames_to_column(anova(model), var = "Predictors") %>% as_tibble
    anova_result$groupname <- model_name
    return(anova_result)
  }))
  
  # ..Figure 2 - stable isotopes ----
  
  # make lm and lmer + model performance function
  perf_si <- function(x, y, data) {
    df <- data %>%  filter(!is.na(data[[y]]))
    df$log10.y <- log10(df[[y]])
    
    m1 <- lm(log10.y ~ df[[x]], data = df)
    m2 <- lmer(log10.y ~ df[[x]] + (1|Species.code), data = df)
    m3 <- lmer(log10.y ~ df[[x]] + (1 | Season) + (1|Species.code), data = df)
    
    perf <- compare_performance(m1, m2, m3, rank = T)
    return(perf)
  }
  
  # ...N excretion ----
  # N excretion vs body N
  perf_si("BodyN", "masscorr.N.excr", excr)
  Nexcr.bN <- lmer(log10(masscorr.N.excr) ~ BodyN + (1|Species.code), 
                       data = excr)
  anova(Nexcr.bN)
  check_model(Nexcr.bN)
  # N excretion vs body CN
  perf_si("BodyCN", "masscorr.N.excr", excr)
  Nexcr.bCN <- lmer(log10(masscorr.N.excr) ~ BodyCN + (1|Species.code),
                 data = excr)
  anova(Nexcr.bCN)
  check_model(Nexcr.bCN)
  AIC(Nexcr.bCN)
  
  # N excretion vs d15N
  perf_si("d15N", "masscorr.N.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  # NexcrSI.15N <- lmer(log10(masscorr.N.excr) ~ d15N + (1|Species.code), 
  #                     data = excr)
  NexcrSI.15N <- lmer(log10(masscorr.N.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  summary(NexcrSI.15N)
  anova(NexcrSI.15N)
  NexcrSI.15N <- lm(log10(masscorr.N.excr) ~ d15N * Species.code, 
                      data = excr)
  check_model(NexcrSI.15N)
  Anova(NexcrSI.15N)
  NexcrSI.15N.ss <- summary(NexcrSI.15N)
  NexcrSI.15N.ss$coefficients
  
  # N excretion vs d13C
  perf_si("d13C", "masscorr.N.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  NexcrSI.13C <- lmer(log10(masscorr.N.excr) ~ d13C + (1|Species.code), 
                      data = excr)
  NexcrSI.13C
  check_model(NexcrSI.13C)
  Anova(NexcrSI.13C, type = 'III')
  summary(NexcrSI.13C)

  NexcrSI.null <- lmer(log10(masscorr.N.excr) ~ (1|Species.code), 
                       data = excr %>% filter(!is.na(d15N)))
  AIC(NexcrSI.15N, NexcrSI.13C, NexcrSI.null)
  
  # ...P excretion ----
  # P excretion vs d15N
  perf_si("d15N", "masscorr.P.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  PexcrSI.15N <- lmer(log10(masscorr.P.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  check_model(PexcrSI.15N)
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
  
  # ...N:P excretion ----
  # N:P excretion vs d15N
  perf_si("d15N", "masscorr.NP.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  NPexcrSI.15N <- lmer(log10(masscorr.NP.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  check_model(PexcrSI.15N)
  Anova(PexcrSI.15N, type = 'III')
  summary(PexcrSI.15N)
  
  # N:P excretion vs d13C
  perf_si("d13C", "masscorr.NP.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  NPexcrSI.13C <- lmer(log10(masscorr.P.excr) ~ d13C + (1|Species.code), 
                      data = excr)
  check_model(PexcrSI.13C)
  Anova(PexcrSI.13C, type = 'III')
  summary(PexcrSI.13C)
  
  NPexcrSI.null <- lmer(log10(masscorr.NP.excr) ~ (1|Species.code), 
                       data = excr %>% filter(!is.na(d15N)))
  Anova(PexcrSI.null, type = 'III')
  AIC(NPexcrSI.15N, NPexcrSI.13C, NPexcrSI.null)
  
  
  # make anova table ----
  # Define your list of models
  anova_SI_models <- list(
    "Mass-specific N excretion (d15N)" = NexcrSI.15N,
    "Mass-specific N excretion (d13C)" = NexcrSI.13C,
    "Mass-specific N excretion (N)" = Nexcr.bN,
    "Mass-specific N excretion (CN)" = Nexcr.bCN,
    "Mass-specific P excretion (d15N)" = PexcrSI.15N,
    "Mass-specific P excretion (d13C)" = PexcrSI.13C,
    "Mass-specific N:P excretion (d15N)" = NPexcrSI.15N,
    "Mass-specific N:P excretion (d13C)" = NPexcrSI.13C
  )
  
  # Combine ANOVA results into a single data frame
  combined_anova_SI <- bind_rows(lapply(names(anova_SI_models), function(model_name) {
    model <- anova_SI_models[[model_name]]
    anova_result <- rownames_to_column(anova(model), var = "Predictors") %>% as_tibble
    anova_result$groupname <- model_name
    anova_result <- anova_result %>%  
      mutate(Predictors = if_else(Predictors == 'BodyN', 'Tissue N',
                                          if_else(Predictors == 'BodyCN', 'Tissue C:N',
                                                  Predictors)))
    return(anova_result)
  }))
  
  # make AIC table ----
  AIC.tbl <- AIC(NexcrSI.15N, NexcrSI.13C, Nexcr.bN, Nexcr.bCN, NexcrSI.null,
                 PexcrSI.15N, PexcrSI.13C, PexcrSI.null,
                 NPexcrSI.15N, NPexcrSI.13C, NPexcrSI.null)
  
  AIC.table <- AIC.tbl %>%  rownames_to_column(var = "Model") %>%
    mutate(df = round(df, digits = 0),
           AIC = round(AIC, digits = 0))
  write_xlsx(AIC.table, 'output/AIC_table.xlsx')
  
  
  