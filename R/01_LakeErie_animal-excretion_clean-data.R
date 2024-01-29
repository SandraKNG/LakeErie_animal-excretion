  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries and read datasets ----
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  library(datawizard) # to do summary statistics
  
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
           BodyC = `%C tissue`,
           BodyN = `%N tissue`,
           BodyP = `%P tissue`,
           BodyCN = `C:N`,
           BodyCP = `C:P`,
           BodyNP = `N:P`,
           Temp = `Incub. Temperature`,
           AmTDN = `TDN (ug/L)...40`,
           AmTDP = `TDP (ug/L)...39`) %>% 
    dplyr::mutate(
      Season = fct_relevel(Season, 'S', 'F'),
      Species.code = if_else(Species.code == 'QM', 'DM', Species.code)) %>%  
    filter(
           !(Species.code %in% c("CTL1","CTL2","CTL3","CTL4","CTL5","CTL6"))
    )
  
  biomass <- bms %>% 
    rename(Biomass = `Biomass (kg/ha)`,
           Species.code = 'Species code') %>% 
    mutate(Species.code = if_else(Species.code == 'QM', 'DM', Species.code))
  
  # plot log10 P excretion rate vs log10 mass ----
  ggplot(excr, 
         aes(x = log10(Mass), y = log10(N.excretion.rate))) +
    geom_point()
  ggplot(excr, 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  # vert
  # ggplot(excr %>% filter(Species.code != 'DM'), 
  #        aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
  #   geom_point()
  # ggplot(excr %>% filter(Species.code == 'LP'), 
  #        aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
  #   geom_point()
  # ggplot(excr %>% filter(Species.code == 'LB'), 
  #        aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
  #   geom_point()
  # ggplot(excr %>% filter(Species.code == 'YB'), 
  #        aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
  #   geom_point()
  # ggplot(excr %>% filter(Species.code != 'QM'), 
  #        aes(x = Log10.mass, y = Log10.N.excretion.rate)) +
  #   geom_point()
  
  # get coeff of variation ----
  # verts animals without DM - SRP and NH4
  verts.Nm <- lm(log10(N.excretion.rate) ~ log10(Mass), 
               data = excr %>% filter(Species.code != 'DM'))
  verts.Ncoeff <- verts.Nm$coefficients["log10(Mass)"]
  verts.Pm <- lm(log10(P.excretion.rate) ~ log10(Mass), 
               data = excr %>% filter(Species.code != 'DM'))
  verts.Pcoeff <- verts.Pm$coefficients["log10(Mass)"]
  
  # with TDN and TDP
  verts.Pm.t <- lm(log10(P.excretion.rate.t) ~ log10(Mass), 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Pcoeff.t <- verts.Pm$coefficients["log10(Mass)"]
  verts.Nm.t <- lm(log10(N.excretion.rate.t) ~ log10(Mass), 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Ncoeff.t <- verts.Nm$coefficients["log10(Mass)"]
  
  # invert
  ggplot(excr %>% filter(Species.code == 'QM'),
         #Log10.mass > -2.328827), 
         aes(x = log10(Mass), y = log10(P.excretion.rate))) +
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
  
  # # ..SP: Sort observations by species ----
  # obs.spsummary <- excr %>% 
  #   group_by(Species.code) %>% 
  #   tally() %>% 
  #   # Now arrange by smallest number of observations to largest
  #   arrange(n)
  # 
  # head(obs.spsummary)
  # 
  # # let's take only species with 3 or more observations and carry on.
  # # Here I create a new data.frame that only has species with 10 or more observations. 
  # newdf.sp <- obs.spsummary %>% 
  #   filter(n > 3) %>% 
  #   left_join(excr, by = "Species.code") %>% 
  #   select(-10)
  # 
  # # Now get unique species in this new df
  # 
  # # ....scaling exponent b for each species for N/P excretions ----
  # # what are the unique species
  # species <- unique(newdf.sp$Species.code)
  # nb.species <- length(species)
  # 
  # results.spdf <- data.frame() # Erica: I made an empty dataframe to put results into
  # 
  # for (i in 1:nb.species) {
  #   subdf <- newdf.sp %>% 
  #     filter(Species.code == species[i]) # equivalent to 'Species X'
  #   subdf
  #   
  #   subdf %>% 
  #     select(N.excretion.rate, 
  #            P.excretion.rate, Mass) 
  # 
  #   modelN <- lm(log10(N.excretion.rate)~log10(Mass), data = subdf)
  #   modelP <- lm(log10(P.excretion.rate)~log10(Mass), data = subdf)
  #   result <- data.frame(Species.code = species[i],
  #                        b.coeff.N.excr.sp = modelN$coefficients["log10(Mass)"],
  #                        b.coeff.P.excr.sp = modelP$coefficients["log10(Mass)"],
  #                        stringsAsFactors = FALSE)
  #   results.spdf <- bind_rows(results.spdf, result)
  #   cat(species[i], '\n') # to check where are in loop
  # }
  # results.spdf # Here are all of your results in one data.frame. You can use "left_join" to join it to another data.frame if you want to further down in the code.
  # excr <- left_join(excr, results.spdf, by = "Species.code")
  
  # ..do mass-normalized excretion rates calculations ----
  excr <- excr %>% mutate(
    massnorm.N.excr = N.excretion.rate / Mass ^ verts.Ncoeff,
    massnorm.P.excr = P.excretion.rate / Mass ^ verts.Pcoeff,
    massnorm.NP.excr = (massnorm.N.excr / massnorm.P.excr) / (31 / 14),
    masscorr.N.excr = N.excretion.rate / Mass,
    masscorr.P.excr = P.excretion.rate / Mass,
    masscorr.NP.excr = (masscorr.N.excr / masscorr.P.excr) / (31 / 14),
    massnorm.N.excr.t = N.excretion.rate.t / Mass ^ verts.Ncoeff.t,
    massnorm.P.excr.t = P.excretion.rate.t / Mass ^ verts.Pcoeff.t,
    massnorm.NP.excr.t = (massnorm.N.excr.t / massnorm.P.excr.t) / (31 / 14),
    masscorr.N.excr.t = N.excretion.rate.t / Mass,
    masscorr.P.excr.t = P.excretion.rate.t / Mass,
    masscorr.NP.excr.t = (masscorr.N.excr.t / masscorr.P.excr.t) / (31 / 14)
  )
  
  # make stable isotopes dataset
  excr.SI <- excr %>% filter(d13C < 0)
  
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
  # need to convert biomass from kg/ha to g/m2 (/10^4)
  excr.yr <- excr %>% 
    group_by(Species.code) %>% 
    reframe(
      across(
        ends_with('excr'),
        \(x) mean(x, na.rm = TRUE)
      ),
      n = n(),
      across(
        ends_with('excr.t'),
        \(x) mean(x, na.rm = TRUE)
      ),
      n = n()
     ) %>% 
    left_join(biomass, by = 'Species.code') %>%
    mutate(
      Biomass.g.m2 = Biomass * 10 ^ 3 / 10 ^ 4,
      Pop.N.excr = masscorr.N.excr * Biomass.g.m2,
      Pop.P.excr = masscorr.P.excr * Biomass.g.m2,
      Pop.N.excr.t = masscorr.N.excr.t * Biomass.g.m2,
      Pop.P.excr.t = masscorr.P.excr.t * Biomass.g.m2
    ) %>%
    filter(!is.na(Biomass))

  # make volumetric excretion dataset
  # convert Lake Erie water retention time from yr to h (x 24h x 325d = 8760h)
  # convert Lake Erie surface area from km2 to m2 (x 10^6)
  # convert Lake Erie water volume in km3 to L (x 10^12)
  # for excretion load, convert ug/m2/h to metric ton per annum
  wat.ret.time.h <- 2.6 * 8760
  Area <- 25700 * 10^6
  lake.vol.L <- 480 * 10^12
  
  excr.vol <- excr.yr %>%  filter(Year == 2021, Species.code != 'DM') %>%
    reframe(
      Agg.N.excr.t = sum(Pop.N.excr.t, na.rm = TRUE),
      Agg.P.excr.t = sum(Pop.P.excr.t, na.rm = TRUE)
    ) %>%  
    mutate(
      volN = Agg.N.excr.t * Area * wat.ret.time.h/lake.vol.L,
      volP = Agg.P.excr.t * Area * wat.ret.time.h/lake.vol.L,
      Source = 'Fish'
    )
  
  ambient.vol <- excr %>%
    reframe(
      volN = mean(AmTDN, na.rm = T),
      volP = mean(AmTDP, na.rm = T)
    ) %>% 
    mutate(
      Source = 'TD'
    )
  
  excr.vol <- excr.vol %>% 
    bind_rows(ambient.vol)
  
  # combine load estimates
  excr.load <- excr.yr %>%  filter(Year == 2019, Species.code != 'DM') %>%
    reframe(
      Agg.N.excr = sum(Pop.N.excr, na.rm = TRUE),
      Agg.P.excr = sum(Pop.P.excr, na.rm = TRUE)
    ) %>% 
    mutate(
    Nload = Agg.N.excr * 8760 * Area / 10 ^ 12,
    Pload = Agg.P.excr * 8760 * Area / 10 ^ 12,
    Source = 'Fish'
    )
  
  excr.DM.load <- excr.yr %>%  filter(Year == 2019, Species.code == 'DM') %>%
    reframe(
      Agg.N.excr = sum(Pop.N.excr, na.rm = TRUE),
      Agg.P.excr = sum(Pop.P.excr, na.rm = TRUE),
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area / 10 ^ 12,
      Pload = Agg.P.excr * 8760 * Area / 10 ^ 12,
      Nflux = Agg.N.excr * 8760 * Area * 10^-15,
      Pflux = Agg.P.excr * 8760 * Area * 10^-15,
      Source = 'Dreissenid'
    )
  
  # ambient load based on the tributary calc + 51% method (Maccoux et al. 2016)
  ambient.load <- tibble(Pload = c(13544, 4577), 
                         Source = c('TP', 'SRP'))
  
  excr.load <- excr.load %>% 
    bind_rows(excr.DM.load) %>% 
    bind_rows(ambient.load) #%>% 
    #mutate(Source = c('TP', 'SRP', 'Fish', 'Dreissenid'))
  
  # Pop N excr for mussel in my study =  ug/m2/h
  # vs. excretion + egesta Li et al (2021) = 0.0468 - 9 mg/m2/d
  Li.Pflux <- mean(0.55, 1.87, 0.768, 0.698, 1.6, 9, 0.188, 0.0468)
  
  Li.Pflux.ugm2h <- Li.Pflux * 10^3 / 24
  LiPflux.Ggyr <- Li.Pflux.ugm2h * 8760 * 10^(-15)

  # ..summary statistics ----
  excr.ss <- excr %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr', 'Mass', 
             'Temp')) %>% 
    describe_distribution()
  
  excr.seas.ss <- excr %>% 
    group_by(Season) %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'massnorm.NP.excr', 'Mass', 
             'Temp')) %>% 
    describe_distribution()
  
  # excr.verts.ss <- excr.verts %>% 
  #   group_by(Season) %>% 
  #   select(c('massnorm.N.excr','massnorm.P.excr', 'massnorm.NP.excr', 'Temperature')) %>% 
  #   describe_distribution()
  
  excr.sp.ss <- excr.yr %>% 
    select(c('Pop.N.excr.sp', 'Pop.P.excr.sp')) %>% 
    describe_distribution()
  