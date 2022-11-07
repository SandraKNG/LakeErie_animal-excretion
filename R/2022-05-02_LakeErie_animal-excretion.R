  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.1.0
  
  # load libraries and read datasets ----
  
  library(RColorBrewer)
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  library(ggdist) # for stat_halfeye
  library(ggpubr) # for ggarrange
  library(lindia) # to look at model diagnostics
  library(performance) # to compare models
  
  # load dataset ----
  er <- read.csv('data/2022-03-03_LakeErie_Mastersheet.csv',
                 stringsAsFactors = F, na.strings = c("", "NA", "."), 
                 strip.white = TRUE, sep = ",")
  bms <- read.csv('data/2022-03-03_LakeErie_Biomass-estimates.csv',
                      stringsAsFactors = F, na.strings = c("", "NA", "."), 
                      strip.white = TRUE, sep = ",")
  
  str(er) 
  head(er)
  
  # clean dataset ----
  excr <- er %>%
    rename(Mass = Ind..dry.mass..g.,
           Numb.ind = X..indiv.,
           P.excretion.rate = P.excretion.rate..ug.h.ind.,
           N.excretion.rate = N.excretion.rate..ug.h.ind.,
           C.tissue = X.C.tissue,
           N.tissue = X.N.tissue,
           P.tissue = X.P.tissue) %>% 
    mutate(Log10.mass = log10(Mass),
           Log10.P.excretion.rate = log10(P.excretion.rate),
           Log10.N.excretion.rate = log10(N.excretion.rate),
           Season.bin = if_else(Season == 'S', "1", "2")) %>%  
    filter(!is.na(Log10.P.excretion.rate),
           !is.na(Log10.N.excretion.rate),
           Species.code != "CTL1",
           Species.code != "CTL2",
           Species.code != "CTL3",
           Species.code != "CTL4",
           Species.code != "CTL5",
           Species.code != "CTL6")
  
  biomass <- bms %>% 
    rename(Species.code = ï..Species.code,
           Biomass = Biomass..kg.ha.)
  
  # plot log10 P excretion rate vs log10 mass ----
  # vert
  ggplot(excr %>% filter(Species.code != 'QM'), 
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
  
  verts.Pm <- lm(Log10.P.excretion.rate ~ Log10.mass, 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Pcoeff <- verts.Pm$coefficients["Log10.mass"]
  verts.Nm <- lm(Log10.N.excretion.rate ~ Log10.mass, 
                 data = excr %>% filter(Species.code != 'QM'))
  verts.Ncoeff <- verts.Nm$coefficients["Log10.mass"]
  
  # invert
  ggplot(excr %>% filter(Species.code == 'QM',
                         Log10.mass > -2.328827), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  ggplot(excr %>% filter(Species.code == 'QM'), 
         aes(x = Log10.mass, y = Log10.N.excretion.rate)) +
    geom_point()
  
  inverts.Pm <- lm(Log10.P.excretion.rate ~ Log10.mass, 
                  data = excr %>% filter(Species.code == 'QM',
                                         Log10.P.excretion.rate < -0.6))
  inverts.Pcoeff <- inverts.Pm$coefficients["Log10.mass"]
  inverts.Nm <- lm(Log10.N.excretion.rate ~ Log10.mass, 
                   data = excr %>% filter(Species.code == 'QM',
                                          Log10.P.excretion.rate < -0.6))
  inverts.Ncoeff <- inverts.Nm$coefficients["Log10.mass"]
  
  # ..SP: Sort observations by species ----
  obs.spsummary <- excr %>% 
    group_by(Species.code, Season) %>% 
    # Count number of observations by group, put the count in a new column, "n" (that's what tally does")
    tally() %>% 
    # Now arrange by smallest number of observations to largest
    arrange(n)
  
  # Notice that for some species, you only have 1 or 2 observations! You should have at least 10 observations for a model with one predictor (some people will argue you need fewer or more, but that's my "go to" minimum.) 
  head(obs.spsummary)
  #You can't build a model with only one observation, so that's why you get the error in "lm". 
  
  # let's take only species with 10 or more observations and carry on.
  # Here I create a new data.frame that only has species with 10 or more observations. 
  newdf.sp <- obs.spsummary %>% 
    filter(n >3) %>% 
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
      select(Log10.N.excretion.rate, 
             Log10.P.excretion.rate, Log10.mass) 
    
    
    modelN <- lm(Log10.N.excretion.rate~Log10.mass, data = subdf)
    modelP <- lm(Log10.P.excretion.rate~Log10.mass, data = subdf)
    result <- data.frame(Species.code = species[i],
                         b.coeff.N.excr.sp = modelN$coefficients["Log10.mass"],
                         b.coeff.P.excr.sp = modelP$coefficients["Log10.mass"],
                         stringsAsFactors = FALSE)
    results.spdf <- bind_rows(results.spdf, result)
    cat(species[i], '\n') # to check where are in loop
  }
  results.spdf # Here are all of your results in one data.frame. You can use "left_join" to join it to another data.frame if you want to further down in the code.
  excr <- left_join(excr, results.spdf, by = "Species.code")
  
  # ..do mass-corrected excretion rates calculations ----
  excr <- excr %>% mutate(massnorm.P.excr = 
                            ifelse(Species.code != "QM",
                                   P.excretion.rate/(Mass^verts.Pcoeff),
                                   P.excretion.rate/Mass),
                          massnorm.N.excr = 
                            ifelse(Species.code != "QM",
                                   N.excretion.rate/(Mass^verts.Ncoeff),
                                   N.excretion.rate/Mass),
                          massnorm.NP.excr = (massnorm.N.excr/massnorm.P.excr)/(31/14),
                          Log10.massnorm.N.excr = log10(massnorm.N.excr),
                          Log10.massnorm.P.excr = log10(massnorm.P.excr))
  
  # ..make excr dataset with one entry for each excretion average ----
  excr.sp <- excr %>% group_by(Season.bin, Species.code) %>% 
    summarise(massnorm.N.excr.sp = mean(massnorm.N.excr, na.rm = T),
              massnorm.P.excr.sp = mean(massnorm.P.excr, na.rm = T),
              massnorm.NP.excr.sp = mean(massnorm.NP.excr, na.rm = T),
              d13C.sp = mean(d13C, na.rm = T),
              d15N.sp = mean(d15N, na.rm = T),
              Log10.massnorm.N.excr.sp = log10(massnorm.N.excr.sp),
              Log10.massnorm.P.excr.sp = log10(massnorm.P.excr.sp),
              Log10.massnorm.NP.excr.sp = log10(massnorm.NP.excr.sp)) %>% 
    filter(!is.na(d15N.sp))
  
  excr.yr <- excr %>% group_by(Species.code) %>% 
    summarise(massnorm.N.excr.sp = mean(massnorm.N.excr, na.rm = T),
              massnorm.P.excr.sp = mean(massnorm.P.excr, na.rm = T),
              massnorm.NP.excr.sp = mean(massnorm.NP.excr, na.rm = T),
              Log10.massnorm.N.excr.sp = log10(massnorm.N.excr.sp),
              Log10.massnorm.P.excr.sp = log10(massnorm.P.excr.sp),
              Log10.massnorm.NP.excr.sp = log10(massnorm.NP.excr.sp)) %>% 
    left_join(biomass, by = 'Species.code') %>% 
    filter(!is.na(Biomass)) %>% 
    mutate(Pop.N.excr.sp = massnorm.N.excr.sp*Biomass,
           Pop.P.excr.sp = massnorm.P.excr.sp*Biomass,
           Log10.Pop.N.excr.sp = log10(Pop.N.excr.sp),
           Log10.Pop.P.excr.sp = log10(Pop.P.excr.sp))
  
  # TotPop.Nexcr.2016 <- excr.yr %>% filter(Year == 2016) %>%
  #   sum(Pop.N.excr.sp)
  # 
  #  excr.yr <- excr.yr %>% mutate(pourc.Pop.N.excr = Pop.N.excr/(sum(Pop.N.excr))*100,
  #                                pourc.Pop.P.excr = Pop.P.excr/(sum(Pop.P.excr))*100)
              
              
  ########## PLOT #######
  # Individual excretion rate ----
  species <- c('Brown Bullhead', 'Goldfish', 'Gizzard shad', 'Largemouth bass', 
               'Logperch', 'Northern Pike', 'Quagga mussel', 'Round goby', 
               'Walleye', 'White perch', 'Yellow bullhead', 'Yellow perch')
  # ..N excretion rate vs species ----
  NexcrSp.p <- ggplot(excr%>%  filter(Log10.mass > -2.328827),
                      aes(x = Species.code, y = Log10.massnorm.N.excr)) +
    geom_jitter(size = 3, alpha = .5, 
                position = position_jitterdodge(jitter.width = 0.2),
                aes(color = Season.bin)) +
    geom_boxplot(alpha = 0, aes(color = Season.bin), size = 0.8) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(N~excretion~(μg~N/g/h))))) +
    # geom_jitter(data = excr.inverts %>%  filter(Log10.mass > -2.328827),
    #             position = position_jitterdodge(jitter.width = 0.2),
    #             aes(x = Species.code, y = Log10.massnorm.N.excr,
    #                 color = Season.bin), size = 3, alpha = .5) +
    # geom_boxplot(data = excr.inverts %>%  filter(Log10.mass > -2.328827),
    #              size = 0.8,
    #              aes(x = Species.code, y = Log10.massnorm.N.excr,
    #                  color = Season.bin), alpha = 0) +
    #scale_x_discrete(labels = species) +
    theme_classic(base_size = 26) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  NexcrSp.p
  
  # ..N excretion vs d15N ----
  Nexcr15N.p <- ggplot(excr.sp,
                      aes(x = d15N.sp, y = Log10.massnorm.N.excr.sp)) +
    geom_point(size = 4, aes(color = Season.bin,
                                         shape = Species.code)) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(N~excretion~(μg~N/g/h))))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    scale_shape_manual(name = 'Species',
                       values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_classic(base_size = 26) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  Nexcr15N.p
  
  # ..N excretion vs d13C ----
  Nexcr13C.p <- ggplot(excr.sp,
                       aes(x = d13C.sp, y = Log10.massnorm.N.excr.sp)) +
    geom_point(size = 4, aes(color = Season.bin,
                                         shape = Species.code)) +
    labs(x = '',
         y = '') +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    scale_shape_manual(name = 'Species',
                       values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_classic(base_size = 26) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'none')
  Nexcr13C.p
  
  # ..P excretion rate vs species ----
  windows(width = 14, height = 7)
  PexcrSp.p <- ggplot(excr %>% filter(Log10.mass > -2.328827),
         aes(x = Species.code, y = Log10.massnorm.P.excr)) +
    geom_jitter(size = 3, alpha = .5, aes(color = Season.bin),
               position = position_jitterdodge(jitter.width = 0.2)) +
    geom_boxplot(alpha = 0, size = 0.8, aes(color = Season.bin)) +
    # geom_jitter(data = excr.inverts %>%  filter(Log10.mass > -2.328827),
    #             position = position_jitterdodge(jitter.width = 0.2),
    #            aes(x = Species.code, y = Log10.massnorm.P.excr,
    #                color = Season.bin), size = 3, alpha = .5) +
    # geom_boxplot(data = excr.inverts %>%  filter(Log10.mass > -2.328827),
    #              size = 0.8,
    #              aes(x = Species.code, y = Log10.massnorm.P.excr,
    #                                color = Season.bin), alpha = 0) +
    labs(x = 'Species',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(P~excretion~(μg~P/g/h))))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    theme_classic(base_size = 26) +
    scale_x_discrete(labels = species) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'top')
  PexcrSp.p
  
  # ..P excretion vs d15N ----
  Pexcr15N.p <- ggplot(excr.sp,
                      aes(x = d15N.sp, y = Log10.massnorm.P.excr.sp)) +
    geom_point(size = 4, aes(color = Season.bin,
                                         shape = Species.code)) +
    labs(x = expression(δ^15~'N (‰)'),
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme_classic(base_size = 26) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    scale_shape_manual(name = 'Species',
                       values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  Pexcr15N.p
  
  # ..P excretion vs d13C ----
  Pexcr13C.p <- ggplot(excr.sp,
                       aes(x = d13C.sp, y = Log10.massnorm.P.excr.sp)) +
    geom_point(size = 4,  aes(color = Season.bin,
                                         shape = Species.code)) +
    labs(x = expression(δ^13~'C (‰)'),
         y = '') +
    theme_classic(base_size = 26) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    scale_shape_manual(name = 'Species',
                       values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  Pexcr13C.p
  
  # combine plots ----
  ggarrange(NexcrSp.p, PexcrSp.p, nrow = 2, heights = c(1,1),
            labels = c("(a)", "(b)"),
            font.label = list(size = 26), label.x = 0.1, label.y = 1,
            legend = 'top', common.legend = T, align = 'v')
  ggsave('figures/preliminary-figures/N_P_excretion_sp.png', 
         width = 20, height = 14, 
         units = 'in', dpi = 600)
  
  ggarrange(Nexcr15N.p, Nexcr13C.p, Pexcr15N.p, Pexcr13C.p,
            nrow = 2, ncol = 2,
            heights = c(1,1),
            labels = c("(a)", "(b)", "(c)", "(d)"),
            font.label = list(size = 26), legend = 'right',
            label.x = 0.25, label.y = 1, common.legend = T, 
            align = 'hv')
  ggsave('figures/preliminary-figures/N_P_excretion_SI_species_av.png', 
         width = 20, height = 14, 
         units = 'in', dpi = 600)
  
  # ..N excretion rate vs season ----
  NexcrSea.p <- ggplot(excr,
                    aes(x = Season.bin, y = Log10.massnorm.N.excr, 
                        color = Season.bin)) +
    stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
      alpha = .3, aes(fill = Season.bin)) + 
    geom_boxplot(width = .25, size = 0.8, outlier.shape = NA) +
    geom_point(size = 3, alpha = .3, 
               position = position_jitter(seed = 1, width = .1)) +
    stat_compare_means(label.x = 0.7, label = 'p.format', size = 8) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(N~excretion~(μg~N/g/h))))) +
    theme_classic(base_size = 26) +
    scale_x_discrete(labels = c("Summer", "Fall")) +
    scale_colour_manual(values = c("goldenrod2", "#D16103")) +
    scale_fill_manual(values = c("goldenrod2", "#D16103")) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'none')
  NexcrSea.p
  
  # ..P excretion rate vs season ----
  PexcrSea.p <- ggplot(excr,
                       aes(x = Season.bin, y = Log10.massnorm.P.excr, 
                           color = Season.bin)) +
    stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
                 alpha = .3, aes(fill = Season.bin)) + 
    geom_boxplot(width = .25, size = 0.8, outlier.shape = NA) +
    geom_point(size = 3, alpha = .3, 
               position = position_jitter(seed = 1, width = .1)) +
    stat_compare_means(label.x = 0.7, label = 'p.format', size = 8) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme_classic(base_size = 26) +
    scale_x_discrete(labels = c("Summer", "Fall")) +
    scale_colour_manual(values = c("goldenrod2", "#D16103")) +
    scale_fill_manual(values = c("goldenrod2", "#D16103")) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'none')
  PexcrSea.p
  
  # combine plots ----
  ggarrange(NexcrSea.p, PexcrSea.p, nrow = 2, heights = c(1,1),
            labels = c("(a)", "(b)"),
            font.label = list(size = 26), label.x = 0.12, label.y = 1,
            legend = 'none', align = 'v')
  ggsave('figures/preliminary-figures/N_Pexcretion_season.png', 
         width = 16, height = 14, 
         units = 'in', dpi = 600)
  
  # Population excretion rate ----
  Species.pop <- c('Gizzard shad', 'Logperch', 'Round goby', 
                   'White perch','Yellow perch')
  
  # ..Pop N excretion vs year ----
  PopNexcr.yr.p <- ggplot(excr.yr,
                             aes(x = Year, y = Log10.Pop.N.excr.sp,
                                 color = Species.code)) + 
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~Population, 
                             paste(N~excretion~(μg~P/kg/ha))))) +
    theme_classic(base_size = 26) +
    scale_color_brewer(palette = "Set1",
                       name = 'Species',
                       labels = Species.pop) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  PopNexcr.yr.p 
  
  # ..Pop P excretion vs year ----
  PopPexcr.yr.p <- ggplot(excr.yr,
                          aes(x = Year, y = Log10.Pop.P.excr.sp,
                              color = Species.code)) + 
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~Population, 
                             paste(P~excretion~(μg~P/kg/ha))))) +
    theme_classic(base_size = 26) +
    scale_color_brewer(palette = "Set1",
                       name = 'Species',
                       labels = Species.pop) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  PopPexcr.yr.p 
  
  # combine plots ----
  ggarrange(PopNexcr.yr.p, PopPexcr.yr.p, nrow = 2, heights = c(1,1),
            labels = c("(a)", "(b)"),
            font.label = list(size = 26), label.x = 0.12, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  ggsave('figures/preliminary-figures/Pop_N_Pexcretion_year.png', 
         width = 16, height = 14, 
         units = 'in', dpi = 600)
  
  # ..Pop N excretion vs year ----
  PopNexcr.yr.p <- ggplot(excr.yr,
                          aes(x = Year, y = Log10.Pop.N.excr.sp,
                              color = Species.code)) + 
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~Population, 
                             paste(N~excretion~(μg~P/kg/ha))))) +
    theme_classic(base_size = 26) +
    # scale_x_discrete(labels = c("Summer", "Fall")) +
    # scale_colour_manual(values = c("goldenrod2", "#D16103")) +
    # scale_fill_manual(values = c("goldenrod2", "#D16103")) +
    scale_color_brewer(palette = "Set1",
                       name = 'Species',
                       labels = Species.pop) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  PopNexcr.yr.p 
  
  # Pop P excretion vs year
  PopPexcr.yr.p <- ggplot(excr.yr,
                          aes(x = Year, y = Log10.Pop.P.excr.sp,
                              color = Species.code)) + 
    geom_point(size = 3) +
    geom_line(size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~Population, 
                             paste(P~excretion~(μg~P/kg/ha))))) +
    theme_classic(base_size = 26) +
    # scale_x_discrete(labels = c("Summer", "Fall")) +
    # scale_colour_manual(values = c("goldenrod2", "#D16103")) +
    # scale_fill_manual(values = c("goldenrod2", "#D16103")) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  PopPexcr.yr.p 
  
  # ..GLM ----
  # N excretion vs d15N
  NexcrSI <- lm(Log10.massnorm.N.excr ~ d15N, 
                data = excr.verts %>% filter(Season == 'F'))
  anova(NexcrSI)
  gg_diagnose(NexcrSI)
  NexcrSI.null <- lm(Log10.massnorm.N.excr ~ 1,
                     data = excr.verts %>% filter(Season == 'F'))
  anova(NexcrSI.null)
  check_model(NexcrSI, NexcrSI.null)
  
  # P excretion vs d15N
  PexcrSI <- lm(Log10.massnorm.P.excr ~ d15N, 
                data = excr.verts %>% filter(Season == 'F'))
  anova(PexcrSI)
  gg_diagnose(PexcrSI)
  
  
  anova(lm(log10(massnorm.N.excr) ~ Incub..Temperature, data = excr.verts))
  anovaN <- lm(log10(massnorm.N.excr) ~ Season*Species.code, data = excr.verts)
  anova(anovaN)
  
  
  
  ggplot(excr.verts %>% filter(Season == 'F',
                               Species.code == 'YP'), 
         aes(x = Time.elapsed..min.,
         y = Log10.massnorm.N.excr)) +
    geom_point()
  
  anova(lm(Log10.massnorm.N.excr ~ Time.elapsed..min., 
     data = excr.verts %>%  filter(Season == 'F',
                                   Species.code == 'LP')))
  
  