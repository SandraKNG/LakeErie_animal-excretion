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
  
  # set up plotting parameters and functions ----
  point.size = 1.5
  line.width = .5
  stat.size = 2.5
  fill.alpha = .3
  Season.labels = c("Summer", "Fall")
  Season.colors = c("goldenrod2", "#D16103")
  Species.pop.labels <- c('Gizzard shad', 'Logperch', 'Dreissenid', 
                          'Round goby','White perch','Yellow perch')
  
  plot_sp <- function(df) {
    ggplot(excr, #%>%  filter(log10(mass) > -2.328827),
           aes(x = Species.code, y = Log10.massnorm.N.excr)) +
      geom_jitter(size = 3, alpha = .5, 
                  position = position_jitterdodge(jitter.width = 0.2),
                  aes(color = Season.bin)) +
      geom_boxplot(alpha = 0, aes(color = Season), size = 0.8) +
      labs(x = '',
           y = expression(atop(Log[10]~mass-corrected, 
                               paste(N~excretion~(μg~N/g/h))))) +
      theme_classic(base_size = 10) +
      scale_colour_manual(name = 'Season',
                          labels = Season.labels,
                          values = Season.colors)
  }
  
  plot_season <- function(y) {
    ggplot(excr.verts,
          aes(x = Season, y = log10(y), 
              color = Season, fill = Season)) +
      stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
                   alpha = fill.alpha) + 
      geom_boxplot(width = .25, size = line.width, outlier.shape = NA, alpha = .2) +
      geom_point(size = point.size, alpha = fill.alpha, 
                 position = position_jitter(seed = 1, width = .1)) +
      stat_compare_means(comparisons = list(c(1, 2)),
                         label = 'p.signif', size = stat.size,
                         bracket.size = line.width) +
      theme_classic(base_size = 10) +
      scale_x_discrete(labels = c("Summer", "Fall")) +
      scale_colour_manual(values = Season.colors) +
      scale_fill_manual(values = Season.colors)
  }
  
  plot_si <- function(x, y) {
    ggplot(excr, aes(x = x, y = log10(y), color = Season)) +
      geom_point(size = point.size) +
      geom_smooth(method = lm, formula = y ~ x, 
                  color = 'black', alpha = fill.alpha, linewidth = line.width) +
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
  NexcrSeas.p <- plot_season(excr.verts$massnorm.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(N~excretion~(μg~N/g/h))))) +
    theme(axis.text.x = element_blank())
  NexcrSeas.p
  PexcrSeas.p <- plot_season(excr.verts$massnorm.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(P~excretion~(μg~P/g/h)))))
  PexcrSeas.p

  # combine plots ----
  ggarrange(NexcrSeas.p, PexcrSeas.p, nrow = 2,
            labels = c("(a)", "(b)"),
            font.label = list(size = 10), label.x = 0.2, label.y = 1,
            legend = 'none', align = 'v')
  ggsave('figures/final-figures/Fig1.tiff', 
         width = 10, height = 13, 
         units = 'cm', dpi = 600, compression = 'lzw')  
  
  # Figure 2 ----
  # N excretion vs d15N
  Nexcr15N.p <- plot_si(excr$d15N, excr$massnorm.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    theme(axis.text.x = element_blank())
  Nexcr15N.p
  
  # N excretion vs d13C
  Nexcr13C.p <- plot_si(excr$d13C, excr$massnorm.N.excr) +
    labs(x = '',
         y = '') +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Nexcr13C.p
  
  # P excretion vs d15N
  Pexcr15N.p <- plot_si(excr$d15N, excr$massnorm.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(P~excretion~"(μg P/g/h)")))) +
    theme(axis.text.x = element_blank())
  Pexcr15N.p
  
  # P excretion vs d13C
  Pexcr13C.p <- plot_si(excr$d13C, excr$massnorm.P.excr) +
    labs(x = '',
         y = '') +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Pexcr13C.p
  
  # N:P excretion vs d15N
  NPexcr15N.p <- plot_si(excr$d15N, excr$massnorm.NP.excr) +
    labs(x = expression(δ^{15} * 'N (‰)'),
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(N:P~excretion~"(molar)")))) 
  NPexcr15N.p
  
  # N:P excretion vs d13C
  NPexcr13C.p <- plot_si(excr$d13C, excr$massnorm.NP.excr) +
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
            label.x = 0.25, label.y = 1.02, common.legend = T,
            align = 'hv')
  ggsave('figures/final-figures/Fig2.tiff', 
         width = 17, height = 20, 
         units = 'cm', dpi = 600)
  
  # Figure 3 ----
  PopNexcr.yr.p <- plot_pop(excr.yr$Pop.N.excr.sp)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N~excretion~"(μg N/ha/h)"))))
  PopNexcr.yr.p
  
  PopPexcr.yr.p <- plot_pop(excr.yr$Pop.P.excr.sp)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(P~excretion~"(μg P/ha/h)"))))
  PopPexcr.yr.p
  
  # combine plots ----
  ggarrange(PopNexcr.yr.p, PopPexcr.yr.p, nrow = 2, 
            labels = c("(a)", "(b)"),
            font.label = list(size = 10), label.x = 0.18, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  ggsave('figures/final-figures/Fig3.tiff', 
         width = 12, height = 14, 
         units = 'cm', dpi = 600)  
  
  # Figure 4 ----
  
  # Figure S1 ----
  