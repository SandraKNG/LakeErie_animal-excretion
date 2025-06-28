  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries and read datasets ----
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  library(datawizard) # to do summary statistics
  library(metaDigitise) # to digitise figure for dreissenid biomass
  library(writexl)
  
  # only do it once
  # retrieve data from figure
  # bms_dm_dig <- metaDigitise(dir = "data/dataToExtract/")
  # write_xlsx(bms_dm_dig, 'data/LakeErie_dreissenid_biomass_digestimates.xlsx')
  
  er <- read_csv('data/2022-03-03_LakeErie_Mastersheet.csv')
  bms_f <- read_csv('data/2022-03-03_LakeErie_fish_species_biomass_estimates.csv')
  bms_WB <- read_csv('data/2022-03-03_LakeErie_WB_total_fish_dreissenid_biomass_estimates.csv')
  bms_dm <- read_csv('data/LakeErie_dreissenid_biomass_estimates.csv')
  
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
      Species.code = factor(if_else(Species.code == 'QM', 'DM', Species.code)),
      Taxo.rank = if_else(Species.code == 'DM', 'Dreissenid', 'Fish')) %>%  
    filter(
      !(Species.code %in% c("CTL1","CTL2","CTL3","CTL4","CTL5","CTL6"))
    )
  
  biomass_f <- bms_f %>% 
    rename(Species.code = 'Species code',
           biomass.lw.kg.ha = 'Biomass_lakewide (kg/ha)',
           biomass.wb.kg.ha = 'Biomass_WB (kg/ha)') 
  
  biomass_WB <- bms_WB %>% filter(Year >= 2011)
  
  biomass_dm <- bms_dm %>% 
    rename(n.b = n)
  
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
    masscorr.NP.excr.t = (masscorr.N.excr.t / masscorr.P.excr.t) / (31 / 14),
    log10.masscorr.N.excr = log10(masscorr.N.excr),
    log10.masscorr.P.excr = log10(masscorr.P.excr),
    log10.masscorr.NP.excr = log10(masscorr.NP.excr)
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
  # .....lakewide fish ----
  # need to convert biomass from kg/ha to g/m2 (/10^4)
  # need to convert fish wet biomass to dry biomass using 0.25 by Vanni et al. (2017)
  # need to convert dreissenid total wet biomass to ash free dry biomass using 
  # 0.025 by Karatayev et al. (2022)
  excr.f.yr <- excr %>% 
    filter(Species.code %in% c('GS','LP','RG','WP','YP')) %>% 
    group_by(Species.code, Taxo.rank) %>% 
    reframe(
      across(
        starts_with('masscorr'),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
        .names = "{.col}{ifelse(.fn == 'sd', '.sd', '')}"
      )
    ) %>% 
    select(-c(ends_with('.t.sd'), ends_with('.t'))) %>% 
    left_join(biomass_f, by = 'Species.code') %>%
    mutate(
      biomass.g.m2 = biomass.lw.kg.ha * 10 ^ 3 / 10 ^ 4 * 0.25,
      Pop.N.excr = masscorr.N.excr * biomass.g.m2,
      Pop.N.excr.sd = masscorr.N.excr.sd * biomass.g.m2,
      Pop.P.excr = masscorr.P.excr * biomass.g.m2,
      Pop.P.excr.sd = biomass.g.m2 * masscorr.P.excr.sd
    ) %>%
    filter(!is.na(biomass.lw.kg.ha)) 
  
  # .....lakewide dreissenids ----
  excr.dm.yr <- excr %>% 
    filter(Species.code == 'DM') %>% 
    group_by(Species.code, Taxo.rank) %>% 
    reframe(
      across(
        starts_with('masscorr'),
        list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
        .names = "{.col}{ifelse(.fn == 'sd', '.sd', '')}"
      )
    ) %>% 
    select(-c(ends_with('.t.sd'), ends_with('.t'))) %>% 
    left_join(biomass_dm, by = 'Species.code') %>%
    mutate(
      biomass.g.m2 = biomass.g.m2 * 0.0265,
      biomass.g.m2.sd = biomass.g.m2.sd * 0.0265,
      Pop.N.excr = masscorr.N.excr * biomass.g.m2,
      Pop.N.excr.sd = Pop.N.excr * sqrt((masscorr.N.excr.sd / masscorr.N.excr)^2 +
                                          (biomass.g.m2.sd / biomass.g.m2)^2),
      Pop.P.excr = masscorr.P.excr * biomass.g.m2,
      Pop.P.excr.sd = Pop.P.excr * sqrt((masscorr.P.excr.sd / masscorr.P.excr)^2 +
                                          (biomass.g.m2.sd / biomass.g.m2)^2)
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
    
  # .....Western basin only ----
  # define function
  get_excr_value <- function(source, variable, stat = "Mean") {
    group_value <- if (source == "Dreissenid") "Taxo.rank=Dreissenid" else "Taxo.rank=Fish"
    value <- excr.taxo.ss %>%
      filter(.group == group_value, Variable == variable) %>%
      slice(1) %>%
      pull({{ stat }})
    return(value)
  }
  
  # make dataset
  excr.yr.WB <- biomass_WB %>%
    mutate(
      biomass.g.m2 = if_else(
        Source == 'Fish',
        biomass.kg.ha * 10 ^ 3 / 10 ^ 4 * 0.25,
        biomass.g.m2 * 0.0265
      ),
      biomass.g.m2.sd = if_else(
        Source == 'Fish',
        biomass.kg.ha.sd * 10 ^ 3 / 10 ^ 4 * 0.25,
        biomass.g.m2.sd * 0.0265
      ),
      # Lookup excretion means and SDs safely using map_dbl
      masscorr.N.excr = map_dbl(Source, ~ get_excr_value(.x, "masscorr.N.excr", "Mean")),
      masscorr.N.excr.sd = map_dbl(Source, ~ get_excr_value(.x, "masscorr.N.excr", "SD")),
      masscorr.P.excr = map_dbl(Source, ~ get_excr_value(.x, "masscorr.P.excr", "Mean")),
      masscorr.P.excr.sd = map_dbl(Source, ~ get_excr_value(.x, "masscorr.P.excr", "SD")),
      
      # Scaled population-level rates + uncertainty propagation
      Pop.N.excr = masscorr.N.excr * biomass.g.m2,
      Pop.N.excr.sd = Pop.N.excr * sqrt(
        (masscorr.N.excr.sd / masscorr.N.excr)^2 +
          (biomass.g.m2.sd / biomass.g.m2)^2
      ),
      Pop.P.excr = masscorr.P.excr * biomass.g.m2,
      Pop.P.excr.sd = Pop.P.excr * sqrt(
        (masscorr.P.excr.sd / masscorr.P.excr)^2 +
          (biomass.g.m2.sd / biomass.g.m2)^2
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
  
  # combine load estimates ----
  # convert loads in ug/h to tonnes/yr by:
  # converting yr to h (x 24h x 325d = 8760h) and multiplying loads by it
  # converting ug to tonnes and dividing loads by it
  excr.load <- excr.f.yr %>%  filter(Year == 2019) %>%
    reframe(
      Agg.biomass.g.m2 = sum(biomass.g.m2, na.rm = TRUE),
      Agg.N.excr = sum(Pop.N.excr, na.rm = TRUE),
      Agg.biomass.g.m2.sd = sqrt(mean(biomass.g.m2^2, na.rm = TRUE)),
      Agg.biomass.g.m2.se = sd(biomass.g.m2, na.rm = TRUE) / sqrt(n()),
      # Propagate measurement uncertainty across years
      Agg.N.excr.sd = sqrt(mean(Pop.N.excr.sd^2, na.rm = TRUE)),
      Agg.N.excr.se = sd(Pop.N.excr, na.rm = TRUE) / sqrt(n()),
      Agg.P.excr = sum(Pop.P.excr, na.rm = TRUE),
      Agg.P.excr.sd = sqrt(mean(Pop.P.excr.sd^2, na.rm = TRUE)),
      Agg.P.excr.se = sd(Pop.P.excr, na.rm = TRUE) / sqrt(n()),
      n.load = n()
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area / 1e12,
      Nload.sd = Agg.N.excr.sd * 8760 * Area / 1e12,
      Nload.se = Agg.N.excr.se * 8760 * Area / 1e12,
      Pload = Agg.P.excr * 8760 * Area / 1e12,
      Pload.sd = Agg.P.excr.sd * 8760 * Area / 1e12,
      Pload.se = Agg.P.excr.se * 8760 * Area / 1e12,
      Source = 'Fish'
    )
  
  excr.DM.load <- excr.dm.yr %>%  filter(Year == 2019) %>%
    reframe(
      Agg.biomass.g.m2 = biomass.g.m2,
      Agg.biomass.g.m2.sd = biomass.g.m2.sd,
      Agg.biomass.g.m2.se = biomass.g.m2.se,
      Agg.N.excr = Pop.N.excr,
      Agg.N.excr.sd = Pop.N.excr.sd,
      Agg.P.excr = Pop.P.excr,
      Agg.P.excr.sd = Pop.P.excr.sd,
      n.load = n()
    ) %>% 
    mutate(
      Nload = Agg.N.excr * 8760 * Area / 1e12,
      Nload.sd = Agg.N.excr.sd * 8760 * Area / 1e12,
      Pload = Agg.P.excr * 8760 * Area / 1e12,
      Pload.sd = Agg.P.excr.sd * 8760 * Area / 1e12,
      Source = 'Dreissenid'
    )
  
  excr.WB.load <- excr.yr.WB %>% 
    group_by(Source) %>% 
    reframe(
      Agg.biomass.g.m2 = mean(biomass.g.m2, na.rm = TRUE),
      Agg.biomass.g.sd = sqrt(mean(biomass.g.m2.sd^2, na.rm = TRUE)),
      Agg.biomass.g.se = sd(biomass.g.m2, na.rm = TRUE) / sqrt(n()),
      # Mean of population-level excretion
      Agg.N.excr = mean(Pop.N.excr, na.rm = TRUE),
      # Propagate measurement uncertainty across years
      Agg.N.excr.sd = sqrt(mean(Pop.N.excr.sd^2, na.rm = TRUE)),
      Agg.N.excr.se = sd(Pop.N.excr, na.rm = TRUE) / sqrt(n()),
      Agg.P.excr = mean(Pop.P.excr, na.rm = TRUE),
      Agg.P.excr.sd = sqrt(mean(Pop.P.excr.sd^2, na.rm = TRUE)),
      Agg.P.excr.se = sd(Pop.P.excr, na.rm = TRUE) / sqrt(n()),
      n.load = n()
    ) %>%
    mutate(
      Nload = Agg.N.excr * 8760 * Area.WB / 1e12,
      Nload.sd = Agg.N.excr.sd * 8760 * Area.WB / 1e12,
      Nload.se = Agg.N.excr.se * 8760 * Area.WB / 1e12,
      Pload = Agg.P.excr * 8760 * Area.WB / 1e12,
      Pload.sd = Agg.P.excr.sd * 8760 * Area.WB / 1e12,
      Pload.se = Agg.P.excr.se * 8760 * Area.WB / 1e12
    )
  
  # ambient load based on main tributaries calc from US + data from Can
  # TP = 11386 (US) + 1205 (CAN), SRP = 3030 (US) + 351 (CAN)
  # and lakewide TP + TP*33% SRP method (Maccoux et al. 2016)
  ambient.load <- tibble(Pload = c(13544, 4470, 12591, 3381), 
                         Nload = c(NA, NA, NA, 41900),
                         Source = c('Total TP', 'Total SRP', 
                                    'Tributary TP', 'Tributary TKN or SRP'))
  
  excr.load <- excr.load %>% 
    bind_rows(excr.DM.load) %>% 
    bind_rows(ambient.load) %>% 
    mutate(Source = factor(
      Source,
      levels = c(
        'Dreissenid',
        'Fish',
        'Tributary TKN or SRP',
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
        'Dreissenid',
        'Fish',
        'Tributary SRP',
        'Tributary TP',
        'Total SRP',
        'Total TP'
      )
    ))
  
  # add loads to excr.yr datasets ----
  excr.lw.final <- excr.yr %>%
    rename(Source = Taxo.rank) %>% 
    left_join(excr.load, by = 'Source')
  
  excr.WB.final <- excr.yr.WB %>%
    left_join(excr.WB.load, by = 'Source')

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
    select(c('Pop.N.excr', 'Pop.P.excr','biomass.g.m2')) %>% 
    describe_distribution()
  
  CTL.av <- er %>% filter(`Species code` %in% c("CTL1","CTL2","CTL3",
                                                "CTL4","CTL5","CTL6")) %>% 
    select(c(`SRP (ug/L)`,  `NH4 (ug/L)`)) %>% 
    describe_distribution()