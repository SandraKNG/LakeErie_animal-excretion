  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries and read datasets ----
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  library(datawizard) # to do summary statistics
  
  # load dataset ----
  er <- read_csv('data/2022-03-03_LakeErie_Mastersheet.csv')
  bms <- read_csv('data/2022-03-03_LakeErie_Biomass-estimates.csv')
  
  str(er) 
  head(er)
  
  # clean dataset ----
  excr <- er %>%
    rename(Mass = `Ind. wet.dry mass (g)`,
           Numb.ind = `# indiv.`,
           Species.code = `Species code`,
           P.excretion.rate = `P excretion rate (ug/h/ind)`,
           N.excretion.rate = `N excretion rate (ug/h/ind)`,
           P.excretion.rate.t = `P excretion rate 2 (ug/h/ind)`,
           N.excretion.rate.t = `N excretion rate 2 (ug/h/ind)`,
           C.tissue = `%C tissue`,
           N.tissue = `%N tissue`,
           P.tissue = `%P tissue`) %>% 
    dplyr::mutate(
      Season = fct_relevel(Season, 'S', 'F'),
      Species.code = if_else(Species.code == 'QM', 'DM', Species.code)) %>%  
    filter(
           !(Species.code %in% c("CTL1","CTL2","CTL3","CTL4","CTL5","CTL6"))
    )
  
  biomass <- bms %>% 
    rename(Biomass = `Biomass (kg/ha)`,
           Species.code = 'Species code')
  
  # plot log10 P excretion rate vs log10 mass ----
  ggplot(excr, 
         aes(x = log10(Mass), y = log10(N.excretion.rate))) +
    geom_point()
  ggplot(excr, 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  # vert
  ggplot(excr %>% filter(Species.code != 'DM'), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  ggplot(excr %>% filter(Species.code == 'LP'), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  ggplot(excr %>% filter(Species.code == 'LB'), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  ggplot(excr %>% filter(Species.code == 'YB'), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  ggplot(excr %>% filter(Species.code != 'QM'), 
         aes(x = Log10.mass, y = Log10.N.excretion.rate)) +
    geom_point()
  
  # get coeff of variation ----
  # verts animals without DM - SRP and NH4
  verts.Nm <- lm(log10(N.excretion.rate) ~ log10(Mass), 
               data = excr %>% filter(Species.code != 'DM'))
  verts.Ncoeff <- verts.Nm$coefficients["log10(Mass)"]
  verts.Pm <- lm(log10(P.excretion.rate) ~ log10(Mass), 
               data = excr %>% filter(Species.code != 'DM'))
  verts.Pcoeff <- verts.Pm$coefficients["log10(Mass)"]
  
  # with TDN and TDP
  verts.Pm <- lm(Log10.P.excretion.rate.t ~ Log10.mass, 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Pcoeff <- verts.Pm$coefficients["Log10.mass"]
  verts.Nm <- lm(Log10.N.excretion.rate.t ~ Log10.mass, 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Ncoeff <- verts.Nm$coefficients["Log10.mass"]
  
  # invert
  ggplot(excr %>% filter(Species.code == 'QM'),
         #Log10.mass > -2.328827), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate.t)) +
    geom_point()
  ggplot(excr %>% filter(Species.code == 'QM'), 
         aes(x = Log10.mass, y = Log10.N.excretion.rate.t)) +
    geom_point()
  
  inverts.Pm <- lm(Log10.P.excretion.rate.t ~ Log10.mass, 
                   data = excr %>% filter(Species.code == 'QM'))
  #Log10.P.excretion.rate.t < -0.6))
  inverts.Pcoeff <- inverts.Pm$coefficients["Log10.mass"]
  inverts.Nm <- lm(Log10.N.excretion.rate.t ~ Log10.mass, 
                   data = excr %>% filter(Species.code == 'QM'))
  #Log10.P.excretion.rate < -0.6))
  inverts.Ncoeff <- inverts.Nm$coefficients["Log10.mass"]
  
  # ..SP: Sort observations by species ----
  obs.spsummary <- excr %>% 
    group_by(Species.code) %>% 
    tally() %>% 
    # Now arrange by smallest number of observations to largest
    arrange(n)
  
  head(obs.spsummary)
  
  # let's take only species with 3 or more observations and carry on.
  # Here I create a new data.frame that only has species with 10 or more observations. 
  newdf.sp <- obs.spsummary %>% 
    filter(n > 3) %>% 
    left_join(excr, by = "Species.code") %>% 
    select(-10)
  
  # Now get unique species in this new df
  
  # ....scaling exponent b for each species for N/P excretions ----
  # what are the unique species
  species <- unique(newdf.sp$Species.code)
  nb.species <- length(species)
  
  results.spdf <- data.frame() # Erica: I made an empty dataframe to put results into
  
  for (i in 1:nb.species) {
    subdf <- newdf.sp %>% 
      filter(Species.code == species[i]) # equivalent to 'Species X'
    subdf
    
    subdf %>% 
      select(N.excretion.rate, 
             P.excretion.rate, Mass) 
    
    
    modelN <- lm(log10(N.excretion.rate)~log10(Mass), data = subdf)
    modelP <- lm(log10(P.excretion.rate)~log10(Mass), data = subdf)
    result <- data.frame(Species.code = species[i],
                         b.coeff.N.excr.sp = modelN$coefficients["log10(Mass)"],
                         b.coeff.P.excr.sp = modelP$coefficients["log10(Mass)"],
                         stringsAsFactors = FALSE)
    results.spdf <- bind_rows(results.spdf, result)
    cat(species[i], '\n') # to check where are in loop
  }
  results.spdf # Here are all of your results in one data.frame. You can use "left_join" to join it to another data.frame if you want to further down in the code.
  excr <- left_join(excr, results.spdf, by = "Species.code")
  
  # ..do mass-normalized excretion rates calculations ----
  excr <- excr %>% mutate(
    massnorm.N.excr =
      ifelse(
        Species.code != "DM",
        N.excretion.rate / (Mass ^ verts.Ncoeff),
        N.excretion.rate / Mass
      ),
    massnorm.P.excr =
      ifelse(
        Species.code != "DM",
        P.excretion.rate / (Mass ^ verts.Pcoeff),
        P.excretion.rate / Mass
      ),
    massnorm.NP.excr = (massnorm.N.excr / massnorm.P.excr) /
      (31 / 14)
  )
  
  # ..make excr dataset with one entry for each excretion average ----
  # for a seasonal dataset
  excr.seas <- excr %>% 
    group_by(Season, Species.code) %>% 
    summarise(
      across(c(
        ends_with('excr'),
        d13C:d15N
      ),
        \(x) mean(x, na.rm = TRUE),
        .names = "{.col}.sp"
      ),
      n = n()
    )
  
  # for a yearly dataset
  excr.yr <- excr %>% 
    group_by(Species.code) %>% 
    summarise(
      across(
        ends_with('excr'),
        function(x) mean(x, na.rm = TRUE),
        .names = "{.col}.sp"
      ),
      n = n()
    ) %>% 
    mutate(
      Pop.N.excr.sp = massnorm.N.excr.sp*Biomass*10^3, # need to convert kg/ha to g/ha
      Pop.P.excr.sp = massnorm.P.excr.sp*Biomass*10^3
    )
  
  # excr.sp <- excr %>% group_by(Season, Species.code) %>% 
  #   summarise(massnorm.N.excr.sp = mean(massnorm.N.excr, na.rm = T),
  #             massnorm.P.excr.sp = mean(massnorm.P.excr, na.rm = T),
  #             massnorm.NP.excr.sp = mean(massnorm.NP.excr, na.rm = T),
  #             d13C.sp = mean(d13C, na.rm = T),
  #             d15N.sp = mean(d15N, na.rm = T)) %>% 
  #   filter(!is.na(d15N.sp))
  # 
  # excr.yr <- excr %>% group_by(Species.code) %>% 
  #   summarise(massnorm.N.excr.sp = mean(massnorm.N.excr, na.rm = T),
  #             massnorm.P.excr.sp = mean(massnorm.P.excr, na.rm = T),
  #             massnorm.NP.excr.sp = mean(massnorm.NP.excr, na.rm = T)) %>% 
  #   left_join(biomass, by = 'Species.code') %>% 
  #   filter(!is.na(Biomass)) %>% 
  #   mutate(Pop.N.excr.sp = massnorm.N.excr.sp*Biomass*10^3, # need to convert kg/ha to g/ha
  #          Pop.P.excr.sp = massnorm.P.excr.sp*Biomass*10^3,
  #          Log10.Pop.N.excr.sp = log10(Pop.N.excr.sp),
  #          Log10.Pop.P.excr.sp = log10(Pop.P.excr.sp))
  
  # ..summary statistics ----
  excr.ss <- excr.yr %>% 
    select(c('Pop.N.excr.sp', 'Pop.P.excr.sp')) %>% 
    describe_distribution()
  