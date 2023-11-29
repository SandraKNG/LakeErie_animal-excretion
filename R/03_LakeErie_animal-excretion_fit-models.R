  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(lindia) # to look at model diagnostics
  library(performance) # to compare models
  library(car) # for Anova() function
  library(boot) # for boostrapping
  
  # ANOVA ----
  # ..Figure 1 - Season effect ----
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

  
  # misc
  
  ggplot(excr.verts %>% filter(Season == 'F',
                               Species.code == 'YP'), 
         aes(x = Time.elapsed..min.,
             y = Log10.massnorm.N.excr)) +
    geom_point()
  
  anova(lm(Log10.massnorm.N.excr ~ Time.elapsed..min., 
           data = excr.verts %>%  filter(Season == 'F',
                                         Species.code == 'LP')))
  
  