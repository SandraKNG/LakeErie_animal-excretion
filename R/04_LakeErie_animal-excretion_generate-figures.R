  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(RColorBrewer)
  library(viridisLite)
  library(viridis) # for beautiful colours
  library(ggdist) # for stat_halfeye
  library(ggpubr) # for ggarrange
  
  # Set up prediction data ----
  lmN.temp.pred <- excr %>% tidyr::expand(Species.code,
                                        Temp = c(seq(min(Temp), max(Temp),
                                                      length = 100),
                                                  rep(median(Temp), 100)))
  
  # prediction model
  lmN.temp.pred <- cbind(lmN.temp.pred, 
                         predict(lmN.temp, lmN.temp.pred, re.form = NA,
                                 type = "response")) 
  gamNDOC.pred <- gamNDOC.pred %>% mutate(upper = fit + (1.96 * se.fit),
                                          lower = fit - (1.96 * se.fit))
  
  # set up plotting parameters and functions ----
  point.size = 1.5
  line.width = .5
  stat.size = 3
  fill.alpha = .3
  Season.labels = c("Summer", "Fall")
  Season.colors = c("goldenrod2", "#D16103")
  Species.pop.labels <- c('Gizzard shad', 'Logperch', 'Dreissenid', 
                          'Round goby','White perch','Yellow perch')
  Species.labels <- c('Brown Bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
               'Largemouth bass', 'Logperch', 'Round goby',
               'White perch', 'Yellow bullhead', 'Yellow perch')
  
  excr.sp.sub <- excr %>% filter(!Species.code %in% c('NP', 'WE'))
  
  plot_sp <- function(y) {
    ggplot(excr.sp.sub, aes(x = Species.code, y = log10(y),
                            color = Season, fill = Season)) +
      geom_jitter(size = point.size, alpha = fill.alpha, 
                  position = position_jitterdodge(jitter.width = 0.3),
                  aes(color = Season)) +
      geom_boxplot(width = .8, size = line.width, outlier.shape = NA, alpha = .2) +
      labs(x = 'Species',
           y = expression(atop(Log[10]~mass-corrected, 
                               paste(N~excretion~(μg~N/g/h))))) +
      scale_x_discrete(labels = Species.labels) +
      theme_classic(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.1)) +
      scale_colour_manual(name = 'Season',
                          labels = Season.labels,
                          values = Season.colors) +
      scale_fill_manual(name = 'Season',
                          labels = Season.labels,
                          values = Season.colors)
  }
  
  plot_season <- function(y) {
    ggplot(excr,
          aes(x = Season, y = log10(y), 
              color = Season, fill = Season)) +
      stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
                   alpha = fill.alpha) + 
      geom_boxplot(width = .25, size = line.width, outlier.shape = NA, alpha = .2) +
      geom_point(size = point.size, alpha = fill.alpha, 
                 position = position_jitter(seed = 1, width = .1)) +
      theme_classic(base_size = 10) +
      scale_x_discrete(labels = c("Summer", "Fall")) +
      scale_colour_manual(values = Season.colors) +
      scale_fill_manual(values = Season.colors)
  }
  
  plot_si <- function(x, y) {
    ggplot(excr, aes(x = x, y = log10(y), color = Season)) +
      geom_point(size = point.size) +
      scale_colour_manual(values = Season.colors) +
      theme_classic(base_size = 10) 
    
  }
  
  plot_pop <- function(y) {
    ggplot(excr.yr, aes(x = Year, y = log10(y),
                                color = Species.code)) + 
      geom_point(size = point.size) +
      geom_line(linewidth = line.width) +
      theme_classic(base_size = 10) +
      scale_color_viridis(option = 'D',
                          name = 'Species',
                          labels = Species.pop.labels,
                          discrete = T) 
  }
  
  # Figure 1 ----
  # Season
  NexcrSeas.p <- plot_season(excr$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(N~excretion~(μg~N/g/h))))) +
    theme(axis.text.x = element_blank()) +
    annotate("text", x = 1.5, y = 2.65, label = '*', size = stat.size) +
    geom_segment(x = 1, xend = 2, y = 2.6, yend = 2.6, 
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 1, xend = 1, y = 2.6, yend = 2.55, 
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 2, xend = 2, y = 2.6, yend = 2.55, 
                 linewidth = line.width, colour = 'black')
  NexcrSeas.p
  
  PexcrSeas.p <- plot_season(excr$masscorr.P.excr) +
    labs(x = 'Season',
         y = expression(atop(Log[10]~mass-corrected, 
                             paste(P~excretion~(μg~P/g/h)))))
  PexcrSeas.p
  
  # Temperature
  NexcrTemp.p <- ggplot(excr, aes(x = Temp, y = log10(masscorr.N.excr), 
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    labs(x = '',
         y = '') +
    scale_x_continuous(n.breaks = 8) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    scale_colour_manual(values = Season.colors) +
    theme_classic(base_size = 10) 
    #geom_ribbon(lmN.temp.pred, aes(ymin = lower, ymax = upper), alpha = .2) +
    #geom_line(lmN.temp.pred, linewidth = line.width)
  NexcrTemp.p
  
  PexcrTemp.p <- ggplot(excr, aes(x = Temp, y = log10(masscorr.P.excr), 
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    labs(x = 'Temperature (°C)',
         y = '') +
    scale_x_continuous(n.breaks = 8) +
    theme(axis.text.y = element_blank()) +
    scale_colour_manual(values = Season.colors) +
    theme_classic(base_size = 10) 
  PexcrTemp.p

  # combine plots ----
  ggarrange(NexcrSeas.p, NexcrTemp.p, PexcrSeas.p, PexcrTemp.p,
            nrow = 2, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)"),
            font.label = list(size = 10), label.x = 0.23, label.y = 1,
            legend = 'none', align = 'v')
  ggsave('figures/final-figures/Fig1.tiff', 
         width = 17, height = 13, 
         units = 'cm', dpi = 600, compression = 'lzw')  
  
  # Figure 2 ----
  # N excretion vs d15N
  Nexcr15N.p <- plot_si(excr$d15N, excr$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-corrected", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Nexcr15N.p
  
  # N excretion vs d13C
  Nexcr13C.p <- plot_si(excr$d13C, excr$masscorr.N.excr) +
    labs(x = '',
         y = '') +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Nexcr13C.p
  
  # P excretion vs d15N
  Pexcr15N.p <- plot_si(excr$d15N, excr$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-corrected", 
                             paste(P~excretion~"(μg P/g/h)")))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Pexcr15N.p
  
  # P excretion vs d13C
  Pexcr13C.p <- plot_si(excr$d13C, excr$masscorr.P.excr) +
    labs(x = '',
         y = '') +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Pexcr13C.p
  
  # N:P excretion vs d15N
  NPexcr15N.p <- plot_si(excr$d15N, excr$masscorr.NP.excr) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    labs(x = expression(δ^{15} * 'N (‰)'),
         y = expression(atop(Log[10]~"mass-corrected", 
                             paste(N:P~excretion~"(molar)")))) 
  NPexcr15N.p
  
  # N:P excretion vs d13C
  NPexcr13C.p <- plot_si(excr$d13C, excr$masscorr.NP.excr) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    labs(x = expression(δ^{13} * "C (‰)"),
         y = '') +
    theme(axis.text.y = element_blank())
  NPexcr13C.p
  
  # combine plots ----
  ggarrange(Nexcr15N.p, Nexcr13C.p, 
            Pexcr15N.p, Pexcr13C.p,
            NPexcr15N.p, NPexcr13C.p,
            nrow = 3, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
            font.label = list(size = 10), legend = 'none',
            label.x = 0.22, label.y = 1.02, common.legend = T,
            align = 'hv')
  ggsave('figures/final-figures/Fig2.tiff', 
         width = 17, height = 20, 
         units = 'cm', dpi = 600)
  
  # Figure 3 ----
  PopNexcr.yr.p <- plot_pop(excr.yr$Pop.N.excr.sp)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N~excretion~(μg~N/m^2/h)))))
  PopNexcr.yr.p
  
  PopPexcr.yr.p <- plot_pop(excr.yr$Pop.P.excr.sp)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(P~excretion~μg~P/m^2/h))))
  PopPexcr.yr.p
  
  # combine plots ----
  ggarrange(PopNexcr.yr.p, PopPexcr.yr.p, nrow = 2, 
            labels = c("(a)", "(b)"),
            font.label = list(size = 10), label.x = 0.22, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  ggsave('figures/final-figures/Fig3.tiff', 
         width = 12, height = 14, 
         units = 'cm', dpi = 600)  
  
  # Figure 4 ----
  # bargraph

  
  # Figure S1 ----
  Nexcr.sp.p <- plot_sp(excr.sp.sub$masscorr.N.excr) +
    xlab('') +
    theme(axis.text.x = element_blank())
  Nexcr.sp.p
  
  Pexcr.sp.p <- plot_sp(excr.sp.sub$masscorr.P.excr)
  Pexcr.sp.p
  
  # combine plots ----
  ggarrange(Nexcr.sp.p, Pexcr.sp.p, nrow = 2, 
            labels = c("(a)", "(b)"),
            font.label = list(size = 10), label.x = 0.12, label.y = 1,
            legend = 'none', align = 'v')
  ggsave('figures/final-figures/Fig4.tiff', 
         width = 17, height = 14, 
         units = 'cm', dpi = 600)  
  