  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(lindia) # to look at model diagnostics
  library(performance) # to compare models
  library(car) # for Anova() function
  library(emmeans) # for posthoc
  library(lme4) # to add random effect to lm
  library(afex) # to add random effect to lm + F-test/p-values
  library(writexl)

  # ANOVA ----
  # ..Figure 1 - Species ----
  aovN.sp <- lm(log10(masscorr.N.excr) ~ Species.code, data = excr)
  anova(aovN.sp)
  aovP.sp <- lm(log10(masscorr.P.excr) ~ Species.code, data = excr)
  anova(aovP.sp)
  aovNP.sp <- lm(log10(masscorr.NP.excr) ~ Species.code, data = excr)
  Anova(aovNP.sp)
  
  # # Get column indices between "massnorm.SUVA.excr" and "massnorm.C7.excr"
  # start_col <- which(names(excr.var) == "massnorm.SUVA.excr")
  # end_col <- which(names(excr.var) == "massnorm.C7.excr")
  # selected_cols <- names(excr.var)[(start_col):(end_col)]
  # 
  # excr.ttest <- excr
  # t_test_results <- list()
  # 
  # # Loop through selected columns
  # for (col in selected_cols) {
  #   result <- excr.ttest %>%
  #     t_test(as.formula(paste(col, "~ 1")), mu = 0, alternative = "greater")
  #   
  #   # Store the result in the list
  #   t_test_results[[col]] <- result
  #   
  #   # Print progress
  #   cat("Processed column:", col, "\n")
  # }
  # 
  # # Convert the list of results to a tibble
  # t.test.results <- bind_rows(t_test_results) 
  # t.test.results <- t.test.results %>% 
  #   arrange(p) %>% 
  #   rename(response = .y.)
  # t.test.results
  # 
  # ..Figure 2 - Season  ----
  excr.seas.sub <- excr %>%  filter(Species.code %in% c('GS', 'LB'))
  
  aovN.seas <- lm(log10(masscorr.N.excr) ~ Season * Species.code, data = excr)
  Anova(aovN.seas)
  check_model(aovN.seas)
  aovP.seas <- lm(log10(masscorr.P.excr) ~ Season * Species.code, data = excr)
  Anova(aovP.seas)
  check_model(aovP.seas)
  aovNP.seas <- lm(log10(masscorr.NP.excr) ~ Season * Species.code, data = excr)
  Anova(aovNP.seas)
  
  ols_vif_tol(aovP.seas)
  vif(aovP.seas)
  
  # ..Figure S1 - Season sub ----
  aovN.seas.sub <- lm(log10(masscorr.N.excr) ~ Season * Species.code, 
                      data = excr.seas.sub)
  Anova(aovN.seas.sub)
  check_model(aovN.seas.sub)
  aovP.seas.sub <- lm(log10(masscorr.P.excr) ~ Season * Species.code, 
                      data = excr.seas.sub)
  Anova(aovP.seas.sub)
  check_model(aovP.seas.sub)
  aovNP.seas.sub <- lm(log10(masscorr.NP.excr) ~ Season * Species.code, 
                   data = excr.seas.sub)
  Anova(aovNP.seas.sub)
  check_model(aovNP.seas.sub)
  
  # make anova table based on all sp models ----
  anova_sp_models <- list(
    "Mass-specific N excretion" = aovN.sp,
    "Mass-specific P excretion" = aovP.sp,
    "Mass-specific N:P excretion" = aovNP.sp
  )
  combined_anova_sp <- bind_rows(lapply(names(anova_sp_models), function(model_name) {
    model <- anova_sp_models[[model_name]]
    anova_result <- rownames_to_column(anova(model), 
                                       var = "Predictors") %>% as_tibble
    anova_result$groupname <- model_name
    anova_result <- anova_result %>% 
      mutate(Predictors = ifelse(
        Predictors == 'Species.code',
        'Species', Predictors))
    return(anova_result)
  }))
  
  # make anova table based on all season models ----
  anova_seas_models <- list(
    "Mass-specific N excretion" = aovN.seas,
    "Mass-specific P excretion" = aovP.seas,
    "Mass-specific N:P excretion" = aovNP.seas
  )
  combined_anova_seas <- bind_rows(lapply(names(anova_seas_models),
                                          function(model_name) {
    model <- anova_seas_models[[model_name]]
    anova_result <- rownames_to_column(anova(model),
                                       var = "Predictors") %>% as_tibble
    anova_result$groupname <- model_name
    anova_result <- anova_result %>%
      mutate(
        Predictors = ifelse(
          Predictors == 'Species.code',
          'Species',
          ifelse(Predictors == 'Season:Species.code', 'Season:Species', Predictors
          )))
    return(anova_result)
  }))
  
  # make anova table based on all season sub models ----
  anova_seas_sub_models <- list(
    "Mass-specific N excretion" = aovN.seas.sub,
    "Mass-specific P excretion" = aovP.seas.sub,
    "Mass-specific N:P excretion" = aovNP.seas.sub
  )
  combined_anova_sub_seas <-
    bind_rows(lapply(names(anova_seas_sub_models),
                     function(model_name) {
                       model <- anova_seas_models[[model_name]]
                       anova_result <-
                         rownames_to_column(anova(model),
                                            var = "Predictors") %>% as_tibble
                       anova_result$groupname <-
                         model_name
                       anova_result <-
                         anova_result %>%
                         mutate(Predictors = ifelse(
                           Predictors == 'Species.code',
                           'Species',
                           ifelse(
                             Predictors == 'Season:Species.code',
                             'Season:Species',
                             Predictors
                           )
                         ))
                       return(anova_result)
                     }))
  
  # ANOVA posthoc - emmeans ----
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
  pwcP <- emm(aovP.sp)
  pwcNP <- emm(aovNP.sp)
  
  # make contrast table based on all models
  contrasts <- bind_rows(pwcN[['contrasts']], pwcP[['contrasts']], 
                         pwcNP[['contrasts']], .id = 'column_label')
  
  contrasts <- contrasts %>% 
    mutate(test = if_else(column_label == 1,  "Mass-normalized N excretion",
                                  if_else(column_label == 2,  "Mass-normalized P excretion", 
                                          "Mass-normalized N:P excretion"))) %>% 
    select(-column_label)
  
  
  # LMER ----
  # data distribution
  hist(excr$masscorr.N.excr)
  
  # ..Figure 2 - Temperature ----
  lmN.temp <- lmer(log10(masscorr.N.excr) ~ Temp + (1|Species.code), data = excr)
  lmN.temp2 <- lm(log10(masscorr.N.excr) ~ Temp, data = excr)
  lmN.temp <- lm(log10(masscorr.N.excr) ~ Temp * Species.code, data = excr)
  check_model(lmN.temp)
  AIC(lmN.temp, lmN.temp2)
  Anova(lmN.temp)
  summary(lmN.temp)
  
  lmP.temp <- lmer(log10(masscorr.P.excr) ~ Temp + (1|Species.code), data = excr)
  lmP.temp2 <- lm(log10(masscorr.P.excr) ~ Temp, data = excr)
  check_model(lmP.temp)
  AIC(lmP.temp, lmP.temp2)
  Anova(lmP.temp)
  
  lmNP.temp <- lmer(log10(masscorr.NP.excr) ~ Temp + (1|Species.code), data = excr)
  lmNP.temp2 <- lm(log10(masscorr.NP.excr) ~ Temp, data = excr)
  check_model(lmNP.temp)
  AIC(lmNP.temp, lmNP.temp2)
  Anova(lmNP.temp)
  
  # ..Figure 2 - stable isotopes ----
  
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
  
  # ...N excretion ----
  # N excretion vs d15N
  perf_si("d15N", "masscorr.N.excr", excr)
  # chose m2 instead of m3 because of collinearity between d15N and Season
  # NexcrSI.15N <- lmer(log10(masscorr.N.excr) ~ d15N + (1|Species.code), 
  #                     data = excr)
  NexcrSI.15N <- mixed(log10(masscorr.N.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  NexcrSI.15N
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
  NexcrSI.13C <- mixed(log10(masscorr.N.excr) ~ d13C + (1|Species.code), 
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
  PexcrSI.15N <- mixed(log10(masscorr.P.excr) ~ d15N + (1|Species.code), 
                      data = excr)
  PexcrSI.15N 
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
  
  # make AIC table
  AIC.tbl <- AIC(NexcrSI.15N, NexcrSI.13C, NexcrSI.null,
                 PexcrSI.15N, PexcrSI.13C, PexcrSI.null,
                 NPexcrSI.15N, NPexcrSI.13C, NPexcrSI.null)
  
  AIC.table <- AIC.tbl %>%  rownames_to_column(var = "Model") %>%
    mutate(df = round(df, digits = 0),
           AIC = round(AIC, digits = 0))
  write_xlsx(AIC.table, 'output/AIC_table.xlsx')
  
  
  