  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries and read datasets ----
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  library(datawizard) # to do summary statistics
  
  er <- read_csv('data/2022-03-03_LakeErie_Mastersheet.csv')
  bms_f <- read_csv('data/2022-03-03_LakeErie_Fish_biomass_estimates.csv')
  bms_WB <- read_csv('data/2022-03-03_LakeErie_WB_Fish_Dreissenid_biomass_estimates.csv')
  bms_dm <- read_csv('data/LakeErie_Dreissenid_biomass_estimates.csv')
  
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
    filter(P.excretion.rate > 0,
           P.excretion.rate.t > 0,
           !Species.code %in% c('NP', 'WE')) %>% 
    dplyr::mutate(
      Season = fct_relevel(Season, 'S', 'F'),
      Species.code = if_else(Species.code == 'QM', 'DM', Species.code),
      Taxo.rank = if_else(Species.code == 'DM', 'Dreissenid', 'Fish')) %>%  
    filter(
      !(Species.code %in% c("CTL1","CTL2","CTL3","CTL4","CTL5","CTL6"))
    )
  
  biomass_f <- bms_f %>% 
    rename(Biomass = `Biomass (kg/ha)`,
           Species.code = 'Species code') #%>% 
    # mutate(Species.code = if_else(Species.code == 'QM', 'DM', Species.code))
  
  biomass_WB <- bms_WB %>% 
    rename(Biomass_kg_ha = `Biomass (kg/ha)`,
           Biomass_g_m2 = `Biomass (g/m2)`) 
  
  biomass_dm <- bms_dm %>% 
    rename(Biomass.g.m2 = `Biomass (g/m2)`) %>% 
    select(-`Ref: 500 g/m2 = 3.6cm`)
  
  # plot log10 P excretion rate vs log10 mass ----
  ggplot(excr, 
         aes(x = log10(Mass), y = log10(N.excretion.rate))) +
    geom_point(aes(colour = Species.code), size = 2) +
    theme_classic(base_size = 17)
  ggplot(excr, 
         aes(x = log10(Mass), y = log10(P.excretion.rate))) +
    geom_point(aes(colour = Species.code), size = 2) +
    theme_classic(base_size = 17)
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
  # ggplot(excr %>% filter(Species.code == 'QM'),
  #        #Log10.mass > -2.328827), 
  #        aes(x = log10(Mass), y = log10(P.excretion.rate))) +
  #   geom_point()
  # ggplot(excr %>% filter(Species.code == 'QM'), 
  #        aes(x = Log10.mass, y = Log10.N.excretion.rate.t)) +
  #   geom_point()
  # 
  # inverts.Pm <- lm(Log10.P.excretion.rate.t ~ Log10.mass, 
  #                  data = excr %>% filter(Species.code == 'QM'))
  # #Log10.P.excretion.rate.t < -0.6))
  # inverts.Pcoeff <- inverts.Pm$coefficients["Log10.mass"]
  # inverts.Nm <- lm(Log10.N.excretion.rate.t ~ Log10.mass, 
  #                  data = excr %>% filter(Species.code == 'QM'))
  # #Log10.P.excretion.rate < -0.6))
  # inverts.Ncoeff <- inverts.Nm$coefficients["Log10.mass"]
  
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
  
  # make excr dataset with one entry for each excretion average ----
  # ..for a seasonal dataset ----
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
  
  # ..for a stable isotopes dataset ----
  excr.SI <- excr %>% filter(!is.na(d15N))
  
  # ..for a yearly dataset ----
  # lakewide fish
  # need to convert biomass from kg/ha to g/m2 (/10^4)
  # need to convert fish wet biomass to dry biomass using 0.25 by Vanni et al. (2017)
  # need to convert dreissenid total wet biomass to ash free dry biomass using 
  # 0.025 by Karatayev et al. (2022)
  excr.f.yr <- excr %>% 
    filter(Species.code != 'DM') %>% 
    group_by(Species.code, Taxo.rank) %>% 
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
    left_join(biomass_f, by = 'Species.code') %>%
    mutate(
      Biomass.g.m2 = Biomass * 10 ^ 3 / 10 ^ 4 * 0.25
    ) %>% 
    mutate(
      Pop.N.excr = masscorr.N.excr * Biomass.g.m2,
      Pop.P.excr = masscorr.P.excr * Biomass.g.m2,
      Pop.NP.excr = masscorr.NP.excr * Biomass.g.m2,
      Pop.N.excr.t = masscorr.N.excr.t * Biomass.g.m2,
      Pop.P.excr.t = masscorr.P.excr.t * Biomass.g.m2,
      Pop.NP.excr.t = masscorr.NP.excr.t * Biomass.g.m2
    ) %>%
    filter(!is.na(Biomass)) 
  
  # lakewide dreissenids
  excr.dm.yr <- excr %>% 
    filter(Species.code == 'DM') %>% 
    group_by(Species.code, Taxo.rank) %>% 
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
    left_join(biomass_dm, by = 'Species.code') %>%
    mutate(
      Biomass.g.m2 = Biomass.g.m2 * 0.025,
      Pop.N.excr = masscorr.N.excr * Biomass.g.m2,
      Pop.P.excr = masscorr.P.excr * Biomass.g.m2,
      Pop.NP.excr = masscorr.NP.excr * Biomass.g.m2,
      Pop.N.excr.t = masscorr.N.excr.t * Biomass.g.m2,
      Pop.P.excr.t = masscorr.P.excr.t * Biomass.g.m2,
      Pop.NP.excr.t = masscorr.NP.excr.t * Biomass.g.m2
    ) 
  
  # combine
  excr.yr <- excr.f.yr %>% bind_rows(excr.dm.yr)
  
  # summary statistic
  # by taxonomic group (fish vs dreissenid)
  excr.taxo.ss <- excr %>% 
    group_by(Taxo.rank) %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr', 
             'massnorm.N.excr','massnorm.P.excr', 'massnorm.NP.excr', 
             'masscorr.N.excr.t','masscorr.P.excr.t', 'masscorr.NP.excr.t',
             'Mass', 'Temp')) %>% 
    describe_distribution()
    
  # Western basin only
  excr_yr_WB <- biomass_WB %>% 
    mutate(
      Biomass_g_m2 = Biomass_g_m2 * 0.25,
      Biomass_kg_ha = Biomass_kg_ha * 0.25,
      Pop.N.excr = if_else(
        Source == 'Dreissenid SRP', 
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Dreissenid", Variable == "masscorr.N.excr") %>% 
          pull(Mean) * Biomass_g_m2,
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Fish", Variable == "masscorr.N.excr") %>% 
          pull(Mean) * Biomass_kg_ha,
        NA_real_
      ),
      Pop.N.excr.t = if_else(
        Source == 'Dreissenid SRP', 
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Dreissenid", Variable == "masscorr.N.excr.t") %>% 
          pull(Mean) * Biomass_g_m2,
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Fish", Variable == "masscorr.N.excr.t") %>% 
          pull(Mean) * Biomass_kg_ha,
        NA_real_
      ),
      Pop.P.excr = if_else(
        Source == 'Dreissenid SRP', 
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Dreissenid", Variable == "masscorr.P.excr") %>% 
          pull(Mean) * Biomass_g_m2,
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Fish", Variable == "masscorr.P.excr") %>% 
          pull(Mean) * Biomass_kg_ha,
        NA_real_
      ),
      Pop.P.excr.t = if_else(
        Source == 'Dreissenid SRP', 
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Dreissenid", Variable == "masscorr.P.excr.t") %>% 
          pull(Mean) * Biomass_g_m2,
        excr.taxo.ss %>% 
          filter(.group == "Taxo.rank=Fish", Variable == "masscorr.P.excr.t") %>% 
          pull(Mean) * Biomass_kg_ha,
        NA_real_
      )
    )
  
  # make volumetric excretion dataset ----
  # convert Lake Erie water retention time from yr to h (x 24h x 325d = 8760h)
  # convert Lake Erie surface area from km2 to m2 (x 10^6)
  # convert Lake Erie water volume in km3 to L (x 10^12)
  # for excretion load, convert ug/m2/h to metric ton per annum
  wat.ret.time.h <- 2.6 * 8760
  Area <- 25657 * 10^6
  Area.WB <- 3284 * 10^6
  lake.vol.L <- 480 * 10^12
  
  excr.WB.tt <- excr_yr_WB %>% 
    group_by(Source) %>% 
    reframe(
      Agg.N.excr.t = mean(Pop.N.excr.t, na.rm = TRUE),
      Agg.P.excr.t = mean(Pop.P.excr.t, na.rm = TRUE)
    ) 
  
  ambient.WB.tt <- excr %>%
    reframe(
      AmTDN = mean(AmTDN, na.rm = T),
      AmTDP = mean(AmTDP, na.rm = T)
    ) 
  
  excr.WB.tt <- excr.WB.tt %>%
    cross_join(ambient.WB.tt) %>%
    mutate(
      N.turnover.time.h = AmTDN / Agg.N.excr.t,
      P.turnover.time.h = AmTDP / Agg.P.excr.t,
      N.turnover.time.min = N.turnover.time.h * 60,
      P.turnover.time.min = P.turnover.time.h * 60
    )
  
  
  # combine load estimates ----
  excr.load <- excr.f.yr %>%  filter(Year == 2019) %>%
    reframe(
      Agg.N.excr = sum(Pop.N.excr, na.rm = TRUE),
      Agg.P.excr = sum(Pop.P.excr, na.rm = TRUE)
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area / 10 ^ 12,
      Pload = Agg.P.excr * 8760 * Area / 10 ^ 12,
      Source = 'Fish SRP'
    )
  
  excr.DM.load <- excr.dm.yr %>%  filter(Year == 2019) %>%
    reframe(
      Agg.N.excr = sum(Pop.N.excr, na.rm = TRUE),
      Agg.P.excr = sum(Pop.P.excr, na.rm = TRUE),
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area / 10 ^ 12,
      Pload = Agg.P.excr * 8760 * Area / 10 ^ 12,
      Nflux = Agg.N.excr * 8760 * Area * 10^-15,
      Pflux = Agg.P.excr * 8760 * Area * 10^-15,
      Source = 'Dreissenid SRP'
    )
  
  excr.WB.load <- excr_yr_WB %>% 
    group_by(Source) %>% 
    reframe(
      Agg.N.excr = mean(Pop.N.excr, na.rm = TRUE),
      Agg.P.excr = mean(Pop.P.excr, na.rm = TRUE),
      # Agg.N.excr.t = mean(Pop.N.excr.t, na.rm = TRUE),
      # Agg.P.excr.t = mean(Pop.P.excr.t, na.rm = TRUE),
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area.WB / 10 ^ 12,
      Pload = Agg.P.excr * 8760 * Area.WB / 10 ^ 12
    )
  
  # ambient load based on main tributaries calc from US + data from Can
  # TP = 11386 (US) + 1205 (CAN), SRP = 3030 (US) + 351 (CAN)
  # and lakewide TP + TP*33% SRP method (Maccoux et al. 2016)
  ambient.load <- tibble(Pload = c(13544, 4470, 12591, 3381), 
                         Source = c('Total TP', 'Total SRP', 
                                    'Tributary TP', 'Tributary SRP'))
  
  excr.load <- excr.load %>% 
    bind_rows(excr.DM.load) %>% 
    bind_rows(ambient.load) %>% 
    mutate(Source = factor(
      Source,
      levels = c(
        'Dreissenid SRP',
        'Fish SRP',
        'Tributary SRP',
        'Tributary TP',
        'Total SRP',
        'Total TP'
      )
    ))
  
  # Western basin loads
  # average TP 2011-2020 + TP*33% SRP method (Maccoux et al. 2016)
  ambient.WB.load <- tibble(Pload = c(3440, 860, 3099, 713), 
                         Source = factor(c('Total TP', 'Total SRP',
                                    'Tributary TP', 'Tributary SRP'))) 
  excr.WB.load <- excr.WB.load %>% 
    bind_rows(ambient.WB.load) %>% 
    mutate(Source = factor(
      Source,
      levels = c(
        'Dreissenid SRP',
        'Fish SRP',
        'Tributary SRP',
        'Tributary TP',
        'Total SRP',
        'Total TP'
      )
    ))
  
  # Pop N excr for mussel in my study =  ug/m2/h
  # vs. excretion + egesta Li et al (2021) = 0.0468 - 9 mg/m2/d
  Li.Pflux <- mean(0.55, 1.87, 0.768, 0.698, 1.6, 9, 0.188, 0.0468)
  
  Li.Pflux.ugm2h <- Li.Pflux * 10^3 / 24
  LiPflux.Ggyr <- Li.Pflux.ugm2h * 8760 * 10^(-15)
  
  # ..summary statistics ----
  # overall summary
  excr.ss <- excr %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr', 
             'massnorm.N.excr','massnorm.P.excr', 'massnorm.NP.excr', 
             'masscorr.N.excr.t','masscorr.P.excr.t', 'masscorr.NP.excr.t',
             'Mass', 'Temp')) %>% 
    describe_distribution()
  
  # by season only
  excr.seas.ss <- excr %>% 
    group_by(Season) %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr', 
             'masscorr.N.excr.t','masscorr.P.excr.t', 'masscorr.NP.excr.t',
             'd15N', 'd13C', 'Mass', 'BodyC', 'BodyN', 'BodyP', 'BodyCN','Temp')) %>% 
    describe_distribution()
  
  # by taxonomic group (fish vs dreissenid) and season
  excr.taxo.seas.ss <- excr %>% 
    group_by(Taxo.rank, Season) %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr', 
             'masscorr.N.excr.t','masscorr.P.excr.t', 'masscorr.NP.excr.t',
             'd15N', 'd13C', 'Mass', 'BodyC', 'BodyN', 'BodyP', 'BodyCN',
             'Temp', 'AmTDN', 'AmTDP')) %>% 
    describe_distribution()

  # individual rates by species
  excr.sp.ss <- excr  %>% 
    group_by(Species.code) %>% 
    select(c('masscorr.N.excr','masscorr.P.excr', 'masscorr.NP.excr',
             'masscorr.N.excr.t','masscorr.P.excr.t', 'masscorr.NP.excr.t',
             'd15N', 'd13C', 'Mass', 'BodyC', 'BodyN', 'BodyP', 'BodyCN')) %>% 
    describe_distribution()
  
  # population rates by species
  excr.pop.ss <- excr.yr %>% 
    group_by(Taxo.rank) %>% 
    select(c('Pop.N.excr', 'Pop.P.excr', 'Pop.NP.excr',
             'Biomass.g.m2')) %>% 
    describe_distribution()
  
  CTL.av <- er %>% filter(`Species code` %in% c("CTL1","CTL2","CTL3",
                                                "CTL4","CTL5","CTL6")) %>% 
    select(c(`SRP (ug/L)`,  `NH4 (ug/L)`)) %>% 
    describe_distribution()