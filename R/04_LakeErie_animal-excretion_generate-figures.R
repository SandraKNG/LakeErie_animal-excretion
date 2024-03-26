  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021-22
  # R version 4.3.0
  
  # load libraries ----
  library(gt) # to make tables
  library(broom.mixed) # to tidy tables
  library(RColorBrewer)
  library(viridisLite)
  library(viridis) # for beautiful colours
  library(ggdist) # for stat_halfeye
  library(ggpubr) # for ggarrange
  library(patchwork) # to arrange multiple plots on one page
  library(ciTools) # for lmer CI bootstrapp
  library(scales) # for trans_breaks
  
  # make tables 
  # Define a custom function to format p-values
  format_p_value <- function(x) {
    ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))
  }
  
  # Apply custom formatting function to Pr(>F) column
  combined_anova_sp <- combined_anova_sp %>%
    mutate(`Pr(>F)` = format_p_value(`Pr(>F)`)) 
  
  # Table S3 ----
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
    )  %>% 
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
  
  # Table S4 ----
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
    gtsave("tables_figures/final-tables_figures/tableS4.rtf")
  
  # Table S5 ----
  # Apply custom formatting function to Pr(>F) column
  combined_anova_seas <- combined_anova_seas %>%
    mutate(`Pr(>F)` = format_p_value(`Pr(>F)`)) 
  
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
    gtsave("tables_figures/final-tables_figures/tableS5.rtf")
  
  # Table S6 ----
  # Apply custom formatting function to Pr(>F) column
  combined_anova_sub_seas <- combined_anova_sub_seas %>%
    mutate(`Pr(>F)` = format_p_value(`Pr(>F)`)) 
  
  combined_anova_sub_seas %>%  
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
    gtsave("tables_figures/final-tables_figures/tableS6.rtf")
  
  # Table S8 ----
  # Apply custom formatting function to Pr(>F) column
  combined_anova_SI <- combined_anova_SI %>%
    mutate(`Pr(>F)` = format_p_value(`Pr(>F)`))
  
  combined_anova_SI %>%  
    gt(groupname_col = "groupname") %>% 
    cols_label(
      Predictors = "",
      'NumDF' = md("**Num df**"),
      'DenDF' = md("**Den df**"),
      'Sum Sq' = md("**SS**"),
      'Mean Sq' = md("**MS**"),
      'F value' = md("**F**"),
      'Pr(>F)' = md("***p***")
    ) %>% 
    cols_align(
      align = "center",
      columns = c('NumDF', 'DenDF', 'Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)')
    ) %>% 
    fmt_number(
      columns = c('Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)'),
      decimals = 3
    ) %>% 
    fmt_number(
      columns = c('DenDF'),
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
    gtsave("tables_figures/final-tables_figures/tableS8.rtf")
  
  # Set up prediction data ----
  lmN.temp.pred <- excr %>% tidyr::expand(nesting(Species.code, Season),
                                        Temp = c(seq(min(Temp), max(Temp),
                                                      length = 100),
                                                  rep(median(Temp), 100)))
  # lmN.bCN.pred <- excr.SI %>% tidyr::expand(nesting(Species.code),
  #                                          BodyCN = c(seq(min(BodyCN), max(BodyCN),
  #                                                        length = 100),
  #                                                    rep(median(BodyCN), 100)))
  
  # Try the parametric bootstrap method, and make predictions with CI
  lmN.temp.pred <- add_ci(lmN.temp.pred, lmN.temp, alpha = 0.5,
                          type = "boot", includeRanef = FALSE, nSims = 100) %>%
    rename(lower = LCB0.25,
           upper = UCB0.75)
  
  # lmN.bCN.pred <- add_ci(lmN.bCN.pred, Nexcr.bCN, alpha = 0.5, 
  #                       type = "boot", includeRanef = FALSE, nSims = 100) %>% 
  #   rename(lower = LCB0.25,
  #          upper = UCB0.75)
  
  # set up plotting parameters and functions ----
  point.size = 1.5
  line.width = .5
  stat.size = 3
  fill.alpha = .3
  Season.labels = c("Summer", "Fall")
  Season.colors = c("goldenrod2", "#D16103")
  Species.pop.labels <- c('Gizzard shad', 'Logperch',  
                          'Round goby','White perch','Yellow perch')
  Species.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
               'Largemouth bass', 'Logperch', 'Round goby',
               'White perch', 'Yellow bullhead', 'Yellow perch')
  Species.SI.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
                         'Logperch', 'Round goby','White perch', 'Yellow perch')
  
  # excr.sp.sub <- excr %>% filter(!Species.code %in% c('NP', 'WE'))
  
  plot_sp <- function(y) {
    ggplot(excr, aes(x = Species.code, y = log10(y),
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
  
  
  plot_season <- function(df, y) {
    ggplot(df,
          aes(x = Season, y = log10(y), 
              color = Season, fill = Season)) +
      # stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
      #              alpha = fill.alpha) + 
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
  
  
  plot_pop <- function(df, y) {
    ggplot(df, aes(x = Year, y = log10(y), colour = Species.code)) + 
      geom_point(size = point.size) +
      geom_line(linewidth = line.width) +
      theme_classic(base_size = 10) +
      scale_color_viridis(option = 'D',
                          name = 'Species',
                          labels = Species.pop.labels,
                          discrete = T)
  }
  
  # Figure 1 ----
  Nexcr.sp.p <- plot_sp(excr$masscorr.N.excr) +
    xlab('') +
    #theme(axis.text.x = element_blank()) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width)
  Nexcr.sp.p
  
  Pexcr.sp.p <- plot_sp(excr$masscorr.P.excr) +
    xlab('') +
    #theme(axis.text.x = element_blank()) +
    ylab(expression(atop(Log[10]~mass-specific, 
                         paste(P~excretion~(μg~P/g/h))))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width)
  Pexcr.sp.p
  
  NPexcr.sp.p <- plot_sp(excr$masscorr.NP.excr) +
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
  ggsave('tables_figures/final-tables_figures/Fig1.tiff', 
         width = 17, height = 20, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # Figure 2 ----
  # N excretion
  NexcrSeas.p <- plot_season(excr.seas, excr$masscorr.N.excr.sp) +
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
  PexcrSeas.p <- plot_season(excr, excr$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme(axis.text.x = element_blank())
  PexcrSeas.p
  
  # N:P excretion
  NPexcrSeas.p <- plot_season(excr, excr$masscorr.NP.excr) +
    labs(x = 'Season',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N:P~excretion~(molar)))))
  NPexcrSeas.p
  
  
  # combine plots ----
  ggarrange(NexcrSeas.p,
            PexcrSeas.p, 
            NPexcrSeas.p,
            nrow = 3, 
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.25, label.y = 1,
            common.legend = T, legend = 'none', align = 'hv')
  ggsave('tables_figures/final-tables_figures/Fig2.tiff', 
         width = 10, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  
  # Figure 3 ----
  # N excretion vs d15N
  Nexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Nexcr15N.p
  
  # N excretion vs d13C
  Nexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = '') +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Nexcr13C.p
  
  # P excretion vs d15N
  Pexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(P~excretion~"(μg P/g/h)")))) +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
    theme(axis.text.x = element_blank())
  Pexcr15N.p
  
  # P excretion vs d13C
  Pexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = '') +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Pexcr13C.p
  
  # N:P excretion vs d15N
  NPexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.NP.excr) +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
    labs(x = expression(δ^{15} * 'N (‰)'),
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(N:P~excretion~"(molar)")))) 
  NPexcr15N.p
  
  # N:P excretion vs d13C
  NPexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.NP.excr) +
    # geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
    #            aes(yintercept = log10(Mean)), linetype = 'dashed', 
    #            linewidth = line.width) +
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
  ggsave('tables_figures/final-tables_figures/Fig3.tiff', 
         width = 17, height = 17, 
         units = 'cm', dpi = 600, compression = 'lzw', bg = 'white')
  
  # Figure 4 ----
  # Fish
  PopNexcr.f.yr.p <- plot_pop(excr.f.yr, excr.f.yr$Pop.N.excr)  +
    labs(title = "      Fish",
         x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N~excretion~(μg~N/m^2/h))))) +
    theme(plot.title = element_text(face = "bold"))
  PopNexcr.f.yr.p
  
  PopPexcr.f.yr.p <- plot_pop(excr.f.yr, excr.f.yr$Pop.P.excr)  +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(P~excretion~μg~P/m^2/h)))) 
  PopPexcr.f.yr.p
  
  # PopNPexcr.f.yr.p <- plot_pop(excr.f.yr, excr.f.yr$Pop.NP.excr)  +
  #   labs(x = '',
  #        y = expression(atop(Log[10]~population, 
  #                            paste(N:P~excretion~(molar))))) 
  # PopNPexcr.f.yr.p
  
  # Dreissenids
  PopNexcr.dm.yr.p <- plot_pop(excr.dm.yr, excr.dm.yr$Pop.N.excr)  +
    labs(title = "      Dreissenids",
         x = '',
         y = '') +
    scale_color_manual(values = 'black') +
    scale_x_continuous(n.breaks = 6) +
    theme(plot.title = element_text(face = "bold")) 
  PopNexcr.dm.yr.p
  
  PopPexcr.dm.yr.p <- plot_pop(excr.dm.yr, excr.dm.yr$Pop.P.excr)  +
    labs(x = '',
         y = '') +
    scale_color_manual(values = 'black') +
    scale_x_continuous(n.breaks = 6) 
  PopPexcr.dm.yr.p
  
  # PopNPexcr.dm.yr.p <- plot_pop(excr.dm.yr, excr.dm.yr$Pop.NP.excr)  +
  #   labs(x = '',
  #        y = '') +
  #   scale_color_manual(values = 'black') +
  #   scale_x_continuous(n.breaks = 6) 
  # PopNPexcr.dm.yr.p
  
  # combine plots ----
  fig4 <- ggarrange(PopNexcr.f.yr.p, PopNexcr.dm.yr.p, 
            PopPexcr.f.yr.p, PopPexcr.dm.yr.p,
            #PopNPexcr.f.yr.p, PopNPexcr.dm.yr.p,
            nrow = 2, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)"),#, "(e)", "(f)"),
            font.label = list(size = 10), label.x = 0.25, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  annotate_figure(fig4, 
                  bottom = text_grob('Year', size = 10, y = 1))
  
  ggsave('tables_figures/final-tables_figures/Fig4.tiff', 
         width = 17, height = 12, units = 'cm', dpi = 600,
        compression = 'lzw', bg = 'white')  
  
  # Figure 5 ----
  # Nutrient turnover time
  Ntt.p <- ggplot(excr.WB.tt, aes(x = Source, y = N.turnover.time.d)) +
    geom_bar(stat = "identity", fill = 'grey80', width = .7) +
    labs(title = "(a) Western basin mean (2011-2020)",
         x = "",
         y = "Turnover time (days)") +
    scale_x_discrete(labels = c('Dreissenid TDN', 'Fish TDN')) +
    #scale_fill_grey(start = 0.8, end = 0) +
    coord_flip() +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold"))
  Ntt.p
  
  Ptt.p <- ggplot(excr.WB.tt, aes(x = Source, y = P.turnover.time.d)) +
    geom_bar(stat = "identity", fill = 'grey80', width = .7) +
    labs(title = "(b) Western basin mean (2011-2020)",
         x = "",
         y = "Turnover time (days)") +
    scale_x_discrete(labels = c('Dreissenid TDP', 'Fish TDP')) +
    #scale_fill_grey(start = 0.8, end = 0) +
    coord_flip() +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold"))
  Ptt.p
  
  # Lakewide N load
  Nload.p <- ggplot(excr.load %>% filter(!is.na(Nload)), aes(x = Source, y = Nload, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(c) Lake wide (2019)",
         x = "",
         y = expression(Log[10] ~ N ~ load ~ (tonnes/yr))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip(ylim = c(1, 1*10^6)) +
    scale_fill_manual(values = c('grey80', 'grey80',
                                 'grey40', 'grey40',  
                                 'grey10', 'grey10')) +
    scale_x_discrete(labels = c('Dreissenid NH4+', 'Fish NH4+',
                                'Tributary TKN')) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          #axis.text.x = element_blank(),
          plot.title = element_text(face = "bold")) 
  Nload.p
  
  # Lakewide P load
  Pload.p <- ggplot(excr.load, aes(x = Source, y = Pload, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(d) Lake wide (2019)",
         x = "",
         y = expression(Log[10] ~ P ~ load ~ (tonnes/yr))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip(ylim = c(1, 1*10^5)) +
    scale_fill_manual(values = c('grey80', 'grey80',
                                 'grey40', 'grey40',  
                                 'grey10', 'grey10')) +
    scale_x_discrete(labels = c('Dreissenid SRP',
                                'Fish SRP',
                                'Tributary SRP',
                                'Tributary TP',
                                'Total SRP',
                                'Total TP')) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          #axis.text.x = element_blank(),
          plot.title = element_text(face = "bold")) 
  Pload.p
  
  # WB P load 2011-2020 average
  PloadWB.p <- ggplot(excr.WB.load, aes(x = Source, y = Pload, fill = Source)) +
    geom_bar(stat = "identity") +
    labs(title = "(e) Western basin mean (2011-2020)",
         x = "",
         y = expression(Log[10] ~ P ~ load ~ (tonnes/yr))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip(ylim = c(1, 1*10^5)) +
    scale_fill_manual(values = c('grey80', 'grey80',
                                 'grey40', 'grey40',  
                                   'grey10', 'grey10')) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  PloadWB.p
  
  # combine plots ----
  #fig5 <- (Nvol.p / Pvol.p) | Pload.p
  fig5 <- Ntt.p / Nload.p | Ptt.p / Pload.p 
  fig5
  fig5 <-  fig5 / PloadWB.p
  fig5
  
  ggarrange(Ntt.p, Ptt.p, 
            Nload.p, Pload.p,
            NA, PloadWB.p,
            nrow = 3, ncol = 2,
            #labels = c("(a)", "(b)", "(c)", "(d)"),#, "(e)", "(f)"),
            font.label = list(size = 10), label.x = 0.25, label.y = 1,
            legend = 'none', align = 'v')
  
  ggsave('tables_figures/final-tables_figures/Fig5.tiff', 
         width = 15, height = 15, units = 'cm', dpi = 600, 
        scaling = 0.7, compression = 'lzw', bg = 'white')   
  
  # Figure S1 ----
  # N excretion
  NexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N~excretion~(μg~N/g/h))))) +
    theme(axis.text.x = element_blank()) +
    annotate("text", x = 1.5, y = 1.52, label = '*', size = stat.size) +
    geom_segment(x = 1, xend = 2, y = 1.5, yend = 1.5,
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 1, xend = 1, y = 1.5, yend = 1.48,
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 2, xend = 2, y = 1.5, yend = 1.48,
                 linewidth = line.width, colour = 'black')
  
  NexcrSeas.sub.p
  
  # P excretion
  PexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme(axis.text.x = element_blank()) #+
    # annotate("text", x = 1.5, y = 1.55, label = '**', size = stat.size) +
    # geom_segment(x = 1, xend = 2, y = 1.5, yend = 1.5,
    #              linewidth = line.width, colour = 'black') +
    # geom_segment(x = 1, xend = 1, y = 1.5, yend = 1.45,
    #              linewidth = line.width, colour = 'black') +
    # geom_segment(x = 2, xend = 2, y = 1.5, yend = 1.45,
    #              linewidth = line.width, colour = 'black')
  PexcrSeas.sub.p
  
  # N:P excretion
  NPexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.NP.excr) +
    labs(x = 'Season',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N:P~excretion~(molar)))))
  NPexcrSeas.sub.p
 
  # combine plots ----
  ggarrange(NexcrSeas.sub.p, 
            PexcrSeas.sub.p, 
            NPexcrSeas.sub.p, 
            nrow = 3,
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.23, label.y = 1,
            common.legend = T, legend = 'none', align = 'hv')
  ggsave('tables_figures/final-tables_figures/FigS1.tiff', 
         width = 10, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # Figure S2 ----
  # N excretion vs tissue N
  NexcrbN.p <- plot_si(excr.SI$BodyN, excr.SI$masscorr.N.excr) +
    labs(x = 'Tissue N (%)',
         y = expression(atop(Log[10]~"mass-specific", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = log10(Mean)), linetype = 'dashed', 
               linewidth = line.width) 
  NexcrbN.p
  
  # N excretion vs tissue C:N
  NexcrbCN.p <- ggplot(lmN.bCN.pred, aes(x = BodyCN, y = pred)) +
    theme_classic(base_size = 10) +
    labs(x = 'Tissue C:N (molar)',
         y = '') +
    geom_point(data = excr.SI, aes(x = BodyCN, y = log10(masscorr.N.excr), 
                                   colour = Season, shape = Species.code),
               size = point.size) +
    geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = .2) +
    geom_line(linewidth = line.width, colour = 'black') +
    scale_colour_manual(name = 'Season',
                        labels = Season.labels,
                        values = Season.colors) +
    scale_shape_manual(
      name = 'Species',
      labels = Species.SI.labels,
      values = c(3, 13, 8, 23, 11, 15, 17, 16)
    ) +
    theme(axis.text.y = element_blank())
  NexcrbCN.p
  
  # combine plots ----
  ggarrange(NexcrbN.p,
            NexcrbCN.p,
            ncol = 2,
            labels = c("(a)", "(b)"),
            font.label = list(size = 10), label.x = 0.23, label.y = 1,
            common.legend = T, legend = 'right', align = 'hv')
  ggsave('tables_figures/final-tables_figures/FigS2.tiff', 
         width = 20, height = 8, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # Figure S3 ----
  # N excretion
  NexcrTemp.p <- ggplot(lmN.temp.pred, aes(x = Temp, y = pred,
                                           color = Season)) +
    geom_point(data = excr, aes(x = Temp, y = log10(masscorr.N.excr)),
               size = point.size, alpha = fill.alpha) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                colour = NA, alpha = .2) +
    geom_line(linewidth = line.width, colour = 'black') +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N~excretion~(μg~N/g/h))))) +
    scale_x_continuous(n.breaks = 8) +
    scale_colour_manual(values = Season.colors) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank())
  NexcrTemp.p
  
  # P excretion
  PexcrTemp.p <- ggplot(excr, aes(x = Temp, y = log10(masscorr.P.excr),
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'),
               aes(yintercept = log10(Mean)), linetype = 'dashed',
               linewidth = line.width) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-specific, 
                             paste(P~excretion~(μg~P/g/h))))) +
    scale_x_continuous(n.breaks = 8) +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank()) +
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
         y = expression(atop(Log[10]~mass-specific, 
                             paste(N:P~excretion~(molar))))) +
    scale_x_continuous(n.breaks = 8) +
    theme_classic(base_size = 10) +
    scale_colour_manual(values = Season.colors)
  NPexcrTemp.p
  
  # combine plots ----
  ggarrange(NexcrTemp.p, 
            PexcrTemp.p,
            NPexcrTemp.p,
            nrow = 3,
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.2, label.y = 1,
            common.legend = T, legend = 'right', align = 'hv')
  ggsave('tables_figures/final-tables_figures/FigS3.tiff', 
         width = 11, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # export final tables ----
  write_csv(excr.ss, "output/excr_summary.csv")
  write_csv(excr.seas.ss, "output/excr_summary_season.csv")
  write_csv(excr.taxo.seas.ss, "output/excr_summary_taxo_season.csv")
  write_csv(excr.sp.ss, "output/excr_summary_sp.csv")
  write_csv(excr.pop.ss, "output/excr_summary_pop.csv")
  write_csv(excr.load, "output/excr_load.csv")
  write_csv(excr.WB.load, "output/excr_WB_load.csv")
  
