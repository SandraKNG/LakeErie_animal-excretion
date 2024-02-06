  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(gt) # to make tables
  library(RColorBrewer)
  library(viridisLite)
  library(viridis) # for beautiful colours
  library(ggdist) # for stat_halfeye
  library(ggpubr) # for ggarrange
  library(patchwork) # to arrange multiple plots on one page
  library(ciTools) # for lmer CI bootstrapp
  library(scales) # for trans_breaks
  
  # make tables 
  # Table S1 ----
  combined_anova_sp %>%  
    gt(groupname_col = "groupname") %>% 
    cols_label(
      Predictors = "",
      Df = md("**df**"),
      'Sum Sq' = md("**SS**"),
      'Mean Sq' = md("**MS**"),
      'F value' = md("**F**"),
      'Pr(>F)' = md("***p***")
    ) %>% 
    cols_align(
      align = "center",
      columns = c(Df, 'Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)')
    ) %>% 
    fmt_number(
      columns = c('Sum Sq', 'Mean Sq', 'F value'),
      decimals = 3
    ) %>% 
    fmt_number(
      columns = c('Pr(>F)'),
      decimals = 2
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        style = 'hidden'
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    gtsave("tables_figures/final-tables_figures/tableS1.rtf")
  
  # Table S2 ----
  contrasts %>% 
    gt(groupname_col = "test") %>% 
    fmt_number(
      columns = everything(),
      decimals = 3
    ) %>% 
    cols_align(
      align = "center"
    ) %>% 
    cols_align(
      align = "left",
      columns = contrast
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>% 
    cols_label(
      lower.CL = 'lower CI',
      upper.CL = "upper CI",
      t.ratio = "t ratio",
      p.value = md("*p*")
    ) %>% 
    gtsave("tables_figures/final-tables_figures/tableS2.rtf")
  
  # Table S1 ----
  combined_anova_seas %>%  
    gt(groupname_col = "groupname") %>% 
    cols_label(
      Predictors = "",
      Df = md("**df**"),
      'Sum Sq' = md("**SS**"),
      'Mean Sq' = md("**MS**"),
      'F value' = md("**F**"),
      'Pr(>F)' = md("***p***")
    ) %>% 
    cols_align(
      align = "center",
      columns = c(Df, 'Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)')
    ) %>% 
    fmt_number(
      columns = c('Sum Sq', 'Mean Sq', 'F value'),
      decimals = 3
    ) %>% 
    fmt_number(
      columns = c('Pr(>F)'),
      decimals = 2
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        style = 'hidden'
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>% 
    gtsave("tables_figures/final-tables_figures/tableS3.rtf")
  
  # Set up prediction data ----
  lmN.temp.pred <- excr %>% tidyr::expand(nesting(Species.code, Season),
                                        Temp = c(seq(min(Temp), max(Temp),
                                                      length = 100),
                                                  rep(median(Temp), 100)))
  
  # prediction model
  # lmN.temp.pred <- cbind(lmN.temp.pred, 
  #                        predict(lmN.temp, lmN.temp.pred, re.form = NA,
  #                                type = "response")) 
  
  # Try the parametric bootstrap method, and make prediction at the population level
  lmN.temp.pred <- add_ci(lmN.temp.pred, lmN.temp, alpha = 0.5, 
                          type = "boot", includeRanef = FALSE, nSims = 100) %>% 
    rename(lower = LCB0.25,
           upper = UCB0.75)
  
  # set up plotting parameters and functions ----
  point.size = 1.5
  line.width = .5
  stat.size = 3
  fill.alpha = .3
  Season.labels = c("Summer", "Fall")
  Season.colors = c("goldenrod2", "#D16103")
  Species.pop.labels <- c('Dreissenid', 'Gizzard shad', 'Logperch',  
                          'Round goby','White perch','Yellow perch')
  Species.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
               'Largemouth bass', 'Logperch', 'Round goby',
               'White perch', 'Yellow bullhead', 'Yellow perch')
  Species.SI.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
                         'Logperch', 'Round goby','White perch', 'Yellow perch')
  
  excr.sp.sub <- excr %>% filter(!Species.code %in% c('NP', 'WE'))
  
  plot_sp <- function(y) {
    ggplot(excr.sp.sub, aes(x = Species.code, y = log10(y),
                            color = Season, fill = Season)) +
      geom_jitter(size = point.size, alpha = fill.alpha, 
                  position = position_jitterdodge(jitter.width = 0.3),
                  aes(color = Season)) +
      geom_boxplot(width = .8, size = line.width, outlier.shape = NA, alpha = .2) +
      labs(x = 'Species',
           y = expression(atop(Log[10]~mass-specific, 
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
      scale_colour_manual(name = 'Season',
                          labels = Season.labels,
                          values = Season.colors) +
      scale_fill_manual(name = 'Season',
                        labels = Season.labels,
                        values = Season.colors)
  }
  
  plot_si <- function(x, y) {
    ggplot(excr.SI, aes(x = x, y = log10(y), color = Season)) +
      geom_point(aes(shape = Species.code), size = point.size) +
      scale_colour_manual(name = 'Season',
                          labels = Season.labels,
                          values = Season.colors) +
      scale_shape_manual(name = 'Species',
                         labels = Species.SI.labels,
                         values = c(3, 13, 8, 23, 11, 15, 17, 16)) +
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
  Nexcr.sp.p <- plot_sp(excr.sp.sub$masscorr.N.excr) +
    xlab('') +
    theme(axis.text.x = element_blank()) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width)
  Nexcr.sp.p
  
  Pexcr.sp.p <- plot_sp(excr.sp.sub$masscorr.P.excr) +
    xlab('') +
    theme(axis.text.x = element_blank()) +
    ylab(expression(atop(Log[10]~mass-specific, 
                         paste(P~excretion~(μg~P/g/h))))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width)
  Pexcr.sp.p
  
  NPexcr.sp.p <- plot_sp(excr.sp.sub$masscorr.NP.excr) +
    ylab(expression(atop(Log[10]~mass-specific, 
                         paste(N:P~excretion~(molar))))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    geom_hline(aes(yintercept = log10(16)), color = 'darkred')
  NPexcr.sp.p
  
  # combine plots ----
  ggarrange(Nexcr.sp.p, Pexcr.sp.p, NPexcr.sp.p, nrow = 3, 
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.13, label.y = 1,
            legend = 'right', common.legend = T, align = 'v')
  ggsave('figures/final-figures/Fig1.tiff', 
         width = 17, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # Figure 2 ----
  # ..Season ----
  # N excretion
  NexcrSeas.p <- plot_season(excr$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
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
  
  # P excretion
  PexcrSeas.p <- plot_season(excr$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme(axis.text.x = element_blank())
  PexcrSeas.p
  
  # N:P excretion
  NPexcrSeas.p <- plot_season(excr$masscorr.NP.excr) +
    labs(x = 'Season',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N:P~excretion~(molar)))))
  NPexcrSeas.p
  
  # ..Temperature ----
  # N excretion
  NexcrTemp.p <- ggplot(lmN.temp.pred, aes(x = Temp, y = pred, 
                                  color = Season)) +
    geom_point(data = excr, aes(x = Temp, y = log10(masscorr.N.excr)),
                                size = point.size, alpha = fill.alpha) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                colour = NA, alpha = .2) +
    geom_line(linewidth = line.width, colour = 'black') +
    labs(x = '',
         y = '') +
    scale_x_continuous(n.breaks = 8) +
    scale_colour_manual(values = Season.colors) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  NexcrTemp.p
  
  # P excretion
  PexcrTemp.p <- ggplot(excr, aes(x = Temp, y = log10(masscorr.P.excr), 
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    labs(x = '',
         y = '') +
    scale_x_continuous(n.breaks = 8) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    scale_colour_manual(values = Season.colors)
  PexcrTemp.p
  
  # N:P excretion
  NPexcrTemp.p <- ggplot(excr, aes(x = Temp, y = log10(masscorr.NP.excr), 
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    labs(x = 'Temperature (°C)',
         y = '') +
    scale_x_continuous(n.breaks = 8) +
    theme_classic(base_size = 10) +
    theme(axis.text.y = element_blank()) +
    scale_colour_manual(values = Season.colors) 
  NPexcrTemp.p

  # combine plots ----
  ggarrange(NexcrSeas.p, NexcrTemp.p, 
            PexcrSeas.p, PexcrTemp.p,
            NPexcrSeas.p, NPexcrTemp.p,
            nrow = 3, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
            font.label = list(size = 10), label.x = 0.25, label.y = 1,
            common.legend = T, legend = 'right', align = 'hv')
  ggsave('figures/final-figures/Fig2.tiff', 
         width = 17, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  
  # Figure 3 ----
  # N excretion vs d15N
  Nexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Nexcr15N.p
  
  # N excretion vs d13C
  Nexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = '') +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Nexcr13C.p
  
  # P excretion vs d15N
  Pexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(P~excretion~"(μg P/g/h)")))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Pexcr15N.p
  
  # P excretion vs d13C
  Pexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = '') +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Pexcr13C.p
  
  # N:P excretion vs d15N
  NPexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.NP.excr) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) +
    labs(x = expression(δ^{15} * 'N (‰)'),
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(N:P~excretion~"(molar)")))) 
  NPexcr15N.p
  
  # N:P excretion vs d13C
  NPexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.NP.excr) +
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
            font.label = list(size = 10), 
            label.x = 0.27, label.y = 1.02, common.legend = T,
            legend = 'right', align = 'hv')
  ggsave('figures/final-figures/Fig3.tiff', 
         width = 17, height = 17, 
         units = 'cm', dpi = 600, compression = 'lzw', bg = 'white')
  
  # Figure 4 ----
  PopNexcr.yr.p <- plot_pop(excr.yr$Pop.N.excr)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N~excretion~(μg~N/m^2/h)))))
  PopNexcr.yr.p
  
  PopPexcr.yr.p <- plot_pop(excr.yr$Pop.P.excr)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(P~excretion~μg~P/m^2/h))))
  PopPexcr.yr.p
  
  PopNPexcr.yr.p <- plot_pop(excr.yr$Pop.NP.excr)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N:P~excretion~(molar)))))
  PopNPexcr.yr.p
  
  # combine plots ----
  ggarrange(PopNexcr.yr.p, PopPexcr.yr.p, PopNPexcr.yr.p, nrow = 3, 
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.22, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  ggsave('figures/final-figures/Fig4.tiff', 
         width = 12, height = 17, units = 'cm', dpi = 600,
        compression = 'lzw', bg = 'white')  
  
  # Figure 5 ----
  # P load
  Pload.p <- ggplot(excr.load, aes(x = Source, y = Pload, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(c) 2019",
         x = "",
         y = expression(Log[10] ~ P ~ load ~ (tonnes/yr))) +
    scale_fill_grey(start = 0.8, end = 0) +
    scale_x_discrete(labels = c('Dreissenid SRP', 'Fish SRP', 
                                'External SRP', 'External TP')) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip() +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  Pload.p
  
  # Volumetric excretion vs ambient cocnentration
  Nvol.p <- ggplot(excr.vol, aes(x = Source, y = volN, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(a) 2021", 
         x = "",
         y = "N concentration (µg N/L)") +
    scale_x_discrete(labels = c('Fish TDN', 'Ambient TDN')) +
    scale_fill_grey(start = 0.8, end = 0) +
    coord_flip() +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold"))
  Nvol.p
  
  Pvol.p <- ggplot(excr.vol, aes(x = Source, y = volP, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(b) 2021", 
         x = "",
         y = "P concentration (µg P/L)") +
    scale_x_discrete(labels = c('Fish TDP', 'Ambient TDP')) +
    scale_fill_grey(start = 0.8, end = 0) +
    coord_flip() +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  Pvol.p
  
  # combine plots ----
  fig5 <- (Nvol.p / Pvol.p) | Pload.p
  fig5
  
  ggsave('figures/final-figures/Fig5.tiff', 
         width = 17, height = 8, units = 'cm', dpi = 600, 
         scaling = 0.7, compression = 'lzw', bg = 'white')   
  
  # export final tables ----
  write_csv(excr.vol, "output/excr_vol.csv")
  write_csv(excr.load, "output/excr_load.csv")
  write_csv(excr.ss, "output/excr_summary.csv")
  write_csv(excr.seas.f.ss, "output/excr_summary_fish_season.csv")
  write_csv(excr.seas.dm.ss, "output/excr_summary_dm_season.csv")
  
