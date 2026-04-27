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
  
  # Table S3 ----
  combined_kruskal_sp <- combined_kruskal_sp %>% 
    mutate(p = format_p_value(p))
  
  combined_kruskal_sp %>%
    gt(groupname_col = "model") %>%
    cols_label(
      Predictors = "",
      n = md("**n**"),
      df = md("**df**"),
      statistic = md("**H**"),
      p = md("***p***")
    ) %>%
    cols_align(
      align = "center",
      columns = -Predictors
    ) %>%
    fmt_number(
      columns = c('statistic'),
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
  combined_dunn_sp <- combined_dunn_sp %>% 
    mutate(p.adj = format_p_value(p.adj))
  
  combined_dunn_sp %>%
    gt(groupname_col = "model") %>%
    cols_label(
      group1 = md("**group 1**"),
      group2 = md("**group 2**"),
      n1 = md("**n1**"),
      n2 = md("**n2**"),
      statistic = md("**Z**"),
      p.adj = md("***p.adj***")
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    fmt_number(
      columns = c('statistic'),
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
    gtsave("tables_figures/final-tables_figures/tableS4.rtf")
  
  # Table S5 ----
  combined_kruskal_seas <- combined_kruskal_seas %>%
    mutate(p = format_p_value(p))
  
  combined_kruskal_seas %>%
    gt(groupname_col = "model") %>%
    cols_label(
      Predictors = "",
      n = md("**n**"),
      df = md("**df**"),
      statistic = md("**H**"),
      p = md("***p***")
    ) %>%
    cols_align(
      align = "center",
      columns = -Predictors
    ) %>%
    fmt_number(
      columns = c('statistic'),
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
    gtsave("tables_figures/final-tables_figures/tableS5.rtf")
  
  # Table S6 ----
  combined_kruskal_seas_sub <- combined_kruskal_seas_sub %>%
    mutate(p = format_p_value(p))
  
  combined_kruskal_seas_sub %>%
    gt(groupname_col = "model") %>%
    cols_label(
      Predictors = "",
      n = md("**n**"),
      df = md("**df**"),
      statistic = md("**H**"),
      p = md("***p***")
    ) %>%
    cols_align(
      align = "center",
      columns = -Predictors
    ) %>%
    fmt_number(
      columns = c('statistic'),
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
    gtsave("tables_figures/final-tables_figures/tableS6.rtf")
  
  # Table S7 ----
  combined_anova_temp <- combined_anova_temp %>%
    mutate(`Pr(>F)` = format_p_value(`Pr(>F)`)) 
  
  combined_anova_temp %>%  
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
    gtsave("tables_figures/final-tables_figures/tableS7.rtf")
  
  # Table S8 ----
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
  
  # Try the parametric bootstrap method, and make predictions with CI
  lmN.temp.pred <- add_ci(lmN.temp.pred, lmN.temp, alpha = 0.5,
                          type = "boot", includeRanef = FALSE, nSims = 100) %>%
    mutate(fit = 10^pred,
           lower = 10^LCB0.25,
           upper = 10^UCB0.75) 
 
  # set up plotting parameters and functions ----
  point.size = 1.5
  line.width = .5
  stat.size = 3
  fill.alpha = .3
  Sampling.labels = c("First", "Second")
  Sampling.colors = c("goldenrod2", "#D16103")
  Species.pop.labels <- c('Gizzard shad', 'Logperch',  
                          'Round goby','White perch','Yellow perch')
  Species.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
               'Largemouth bass', 'Logperch', 'Round goby',
               'White perch', 'Yellow bullhead', 'Yellow perch')
  Species.SI.labels <- c('Brown bullhead', 'Dreissenid', 'Goldfish', 'Gizzard shad', 
                         'Logperch', 'Round goby','White perch', 'Yellow perch')
  Psource.labels <- c('Dreissenid SRP','Fish SRP','Tributary SRP',
                      'Tributary TP','Total SRP','Total TP')
  
  # excr.sp.sub <- excr %>% filter(!Species.code %in% c('NP', 'WE'))
  
  plot_sp <- function(y) {
    ggplot(excr, aes(x = Species.code, y = y)) +#,
                            #color = Season, fill = Season)) +
      geom_jitter(size = point.size, alpha = fill.alpha) +#, 
                  #position = position_jitterdodge(jitter.width = 0.3)),
                  #aes(color = Season)) +
      geom_boxplot(width = .8, size = line.width, outlier.shape = NA, alpha = .2) +
      labs(x = 'Species',
           y = expression(atop("Mass-specific", 
                               paste(N~excretion~(μg~N~g^-1~h^-1))))) +
      scale_x_discrete(labels = Species.labels) +
      scale_y_continuous(trans = 'log10') +
      theme_classic(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.1)) #+
      # scale_colour_manual(name = 'Sampling',
      #                     labels = Sampling.labels,
      #                     values = Sampling.colors) +
      # scale_fill_manual(name = 'Sampling',
      #                   labels = Sampling.labels,
      #                   values = Sampling.colors)
  }
  
  plot_season <- function(df, y) {
    ggplot(df,
          aes(x = Season, y = y, 
              color = Season, fill = Season)) +
      stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
                   alpha = fill.alpha) +
      geom_boxplot(width = .25, size = line.width, outlier.shape = NA, alpha = .2) +
      geom_point(size = point.size, alpha = fill.alpha, 
                 position = position_jitter(seed = 1, width = .1)) +
      theme_classic(base_size = 10) +
      scale_x_discrete(labels = c("First", "Second")) +
      scale_y_continuous(trans = 'log10') +
      scale_colour_manual(name = 'Sampling',
                          labels = Sampling.labels,
                          values = Sampling.colors) +
      scale_fill_manual(name = 'Sampling',
                        labels = Sampling.labels,
                        values = Sampling.colors)
  }
  
  plot_si <- function(x, y) {
    ggplot(excr.SI, aes(x = x, y = y, color = Season)) +
      geom_point(aes(shape = Species.code), size = point.size) +
      scale_y_continuous(trans = 'log10') +
      scale_colour_manual(name = 'Sampling',
                          labels = Sampling.labels,
                          values = Sampling.colors) +
      scale_shape_manual(name = 'Species',
                         labels = Species.SI.labels,
                         values = c(3, 13, 8, 23, 11, 15, 17, 16)) +
      theme_classic(base_size = 10) 
  }
  
  
  plot_pop <- function(df, y, y.sd) {
    ggplot(df, aes(x = Year, y = y, colour = Species.code)) + 
      geom_point(size = point.size) +
      geom_line(linewidth = line.width) +
      geom_errorbar(aes(ymin = pmax(y - y.sd, 0), 
                        ymax = y + y.sd, colour = Species.code),
                    width = .1) +
      theme_classic(base_size = 10) +
      scale_color_viridis(option = 'D',
                          name = 'Species',
                          labels = Species.pop.labels,
                          discrete = T)
  }
  
  # Figure 2 ----
  Nexcr.sp.p <- plot_sp(excr$masscorr.N.excr) +
    xlab('') +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.N.excr'), 
               aes(yintercept = Mean), linetype = 'dashed', 
               linewidth = line.width) +
    annotate("text", x = 1, y = 340, label = 'a', size = stat.size) +
    annotate("text", x = 2, y = 340, label = 'b', size = stat.size) +
    annotate("text", x = 3, y = 340, label = 'abcd', size = stat.size) +
    annotate("text", x = 4, y = 340, label = 'bcd', size = stat.size) +
    annotate("text", x = 5, y = 340, label = 'ad', size = stat.size) +
    annotate("text", x = 6, y = 340, label = 'bcd', size = stat.size) +
    annotate("text", x = 7, y = 340, label = 'acd', size = stat.size) +
    annotate("text", x = 8, y = 340, label = 'bc', size = stat.size) +
    annotate("text", x = 9, y = 340, label = 'abcd', size = stat.size) +
    annotate("text", x = 10, y = 340, label = 'ad', size = stat.size)
  
  Nexcr.sp.p
  
  Pexcr.sp.p <- plot_sp(excr$masscorr.P.excr) +
    xlab('') +
    ylab(expression(atop("Mass-specific", 
                         paste(P~excretion~(μg~P~g^-1~h^-1))))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'), 
               aes(yintercept = Mean), linetype = 'dashed', 
               linewidth = line.width) +
    annotate("text", x = 1, y = 120, label = 'adf', size = stat.size) +
    annotate("text", x = 2, y = 120, label = 'bc', size = stat.size) +
    annotate("text", x = 3, y = 120, label = 'abcdef', size = stat.size) +
    annotate("text", x = 4, y = 120, label = 'b', size = stat.size) +
    annotate("text", x = 5, y = 120, label = 'af', size = stat.size) +
    annotate("text", x = 6, y = 120, label = 'acdf', size = stat.size) +
    annotate("text", x = 7, y = 120, label = 'acdf', size = stat.size) +
    annotate("text", x = 8, y = 120, label = 'bce', size = stat.size) +
    annotate("text", x = 9, y = 120, label = 'adf', size = stat.size) +
    annotate("text", x = 10, y = 120, label = 'f', size = stat.size)
  Pexcr.sp.p
  
  NPexcr.sp.p <- plot_sp(excr$masscorr.NP.excr) +
    ylab(expression(atop("Mass-specific", 
                         paste(N:P~excretion~(molar))))) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'), 
               aes(yintercept = Mean), linetype = 'dashed', 
               linewidth = line.width) +
    geom_hline(aes(yintercept = 16), color = 'darkred') +
    annotate("text", x = 1, y = 70, label = 'ac', size = stat.size) +
    annotate("text", x = 2, y = 70, label = 'abc', size = stat.size) +
    annotate("text", x = 3, y = 70, label = 'abc', size = stat.size) +
    annotate("text", x = 4, y = 70, label = 'b', size = stat.size) +
    annotate("text", x = 5, y = 70, label = 'ac', size = stat.size) +
    annotate("text", x = 6, y = 70, label = 'ac', size = stat.size) +
    annotate("text", x = 7, y = 70, label = 'a', size = stat.size) +
    annotate("text", x = 8, y = 70, label = 'ab', size = stat.size) +
    annotate("text", x = 9, y = 70, label = 'ac', size = stat.size) +
    annotate("text", x = 10, y = 70, label = 'c', size = stat.size)
  NPexcr.sp.p
  
  # combine plots ----
  ggarrange(Nexcr.sp.p, Pexcr.sp.p, NPexcr.sp.p, nrow = 3, 
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.11, label.y = 1.02,
            legend = 'right', common.legend = T, align = 'v')
  ggsave('tables_figures/final-tables_figures/Fig2.tiff', 
         width = 17, height = 20, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  # Figure 3 ----
  # N excretion
  NexcrSeas.p <- plot_season(excr, excr$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(N~excretion~(μg~N~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank()) +
    annotate("text", x = 1.5, y = 450, label = '*', size = stat.size) +
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
         y = expression(atop("Mass-specific", 
                             paste(P~excretion~(μg~P~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank()) +
    annotate("text", x = 1.5, y = 150, label = '*', size = stat.size) +
    geom_segment(x = 1, xend = 2, y = 2.1, yend = 2.1,
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 1, xend = 1, y = 2.1, yend = 2.05,
                 linewidth = line.width, colour = 'black') +
    geom_segment(x = 2, xend = 2, y = 2.1, yend = 2.05,
                 linewidth = line.width, colour = 'black')
  PexcrSeas.p
  
  # N:P excretion
  NPexcrSeas.p <- plot_season(excr, excr$masscorr.NP.excr) +
    labs(x = 'Sampling',
         y = expression(atop("Mass-specific", 
                             paste(N:P~excretion~(molar)))))
  NPexcrSeas.p
  
  
  # combine plots ----
  ggarrange(NexcrSeas.p,
            PexcrSeas.p, 
            NPexcrSeas.p,
            nrow = 3, 
            labels = c("(a)", "(b)", "(c)"),
            font.label = list(size = 10), label.x = 0.22, label.y = 1,
            common.legend = T, legend = 'none', align = 'hv')
  ggsave('tables_figures/final-tables_figures/Fig3.tiff', 
         width = 10, height = 17, units = 'cm', dpi = 600,
         compression = 'lzw', bg = 'white')  
  
  
  # Figure 4 ----
  # N excretion vs d15N
  Nexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(N~excretion~(μg~N~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank())
  Nexcr15N.p
  
  # N excretion vs d13C
  Nexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.N.excr) +
    labs(x = '',
         y = '') +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Nexcr13C.p
  
  # P excretion vs d15N
  Pexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(P~excretion~(μg~P~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank())
  Pexcr15N.p
  
  # P excretion vs d13C
  Pexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.P.excr) +
    labs(x = '',
         y = '') +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())
  Pexcr13C.p
  
  # N:P excretion vs d15N
  NPexcr15N.p <- plot_si(excr.SI$d15N, excr.SI$masscorr.NP.excr) +
    labs(x = expression(δ^{15} * 'N (‰)'),
         y = expression(atop("Mass-specific", 
                             paste(N:P~excretion~"(molar)")))) 
  NPexcr15N.p
  
  # N:P excretion vs d13C
  NPexcr13C.p <- plot_si(excr.SI$d13C, excr.SI$masscorr.NP.excr) +
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
            label.x = 0.28, label.y = 1.02, common.legend = T,
            legend = 'right', align = 'hv')
  ggsave('tables_figures/final-tables_figures/Fig4.tiff', 
         width = 17, height = 17, 
         units = 'cm', dpi = 600, compression = 'lzw', bg = 'white')
  
  # Figure 5 ----
  # Fish
  PopNexcr.f.yr.p <- plot_pop(excr.f.yr, excr.f.yr$Pop.N.excr, excr.f.yr$Pop.N.excr.sd)  +
    labs(title = "      Fish",
         x = '',
         y = expression(atop(Population, 
                             paste(N~excretion~(μg~N~m^-2~h^-1))))) +
    theme(plot.title = element_text(face = "bold")) 
  PopNexcr.f.yr.p
  
  PopPexcr.f.yr.p <- plot_pop(excr.f.yr, excr.f.yr$Pop.P.excr, excr.f.yr$Pop.P.excr.sd)  +
    labs(x = '',
         y = expression(atop(Population, 
                             paste(P~excretion~(μg~P~m^-2~h^-1)))))
  PopPexcr.f.yr.p
  
  # Dreissenids
  PopNexcr.dm.yr.p <- plot_pop(excr.dm.yr, excr.dm.yr$Pop.N.excr, excr.dm.yr$Pop.N.excr.sd)  +
    geom_errorbar(aes(ymin = pmax(Pop.N.excr - Pop.N.excr.sd, 0),
                      ymax = Pop.N.excr + Pop.N.excr.sd, width = 1)) +
    labs(title = "      Dreissenids",
         x = '',
         y = '') +
    scale_color_manual(values = 'black') +
    scale_x_continuous(n.breaks = 6) +
    theme(plot.title = element_text(face = "bold")) 
  PopNexcr.dm.yr.p
  
  PopPexcr.dm.yr.p <- plot_pop(excr.dm.yr, excr.dm.yr$Pop.P.excr, excr.dm.yr$Pop.P.excr.sd)  +
    geom_errorbar(aes(ymin = pmax(Pop.P.excr - Pop.P.excr.sd, 0), 
                      ymax = Pop.P.excr + Pop.P.excr.sd, width = 1)) +
    labs(x = '',
         y = '') +
    scale_color_manual(values = 'black') +
    scale_x_continuous(n.breaks = 6)
  PopPexcr.dm.yr.p
 
  # combine plots ----
  fig4 <- ggarrange(PopNexcr.f.yr.p, PopNexcr.dm.yr.p, 
            PopPexcr.f.yr.p, PopPexcr.dm.yr.p,
            nrow = 2, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)"),
            font.label = list(size = 10), label.x = 0.28, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  annotate_figure(fig4, 
                  bottom = text_grob('Year', size = 10, y = 1))
  
  ggsave('tables_figures/final-tables_figures/Fig5.tiff', 
         width = 17, height = 12, units = 'cm', dpi = 600,
        compression = 'lzw', bg = 'white')  
  
  # Figure 6 ----
  # Lakewide N load
  Nload.p <- ggplot(excr.load %>% filter(!is.na(Nload)), aes(x = Source, y = Nload, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = pmax(Nload - Nload.se, 1), ymax = Nload + Nload.se),
                  width = 0.2, color = "grey50") +
    labs(title = "(a) Lake-wide (2019)",
         x = "",
         y = expression(N ~ load ~ (Mg~yr^-1))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip(ylim = c(1, 1*10^6)) +
    scale_fill_manual(values = c('grey10', 'grey10',
                                 'grey40', 'grey40',  
                                 'grey80', 'grey80')) +
    scale_x_discrete(labels = c('Dreissenid NH4+', 'Fish NH4+',
                                'Tributary TKN')) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  Nload.p
  
  # Lakewide P load
  Pload.p <- ggplot(excr.load, aes(x = Source, y = Pload, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = pmax(Pload - Pload.se, 1), ymax = Pload + Pload.se),
                  width = 0.2, color = "grey50") +
    labs(title = "(b) Lake-wide (2019)",
         x = "",
         y = expression(P ~ load ~ (Mg~yr^-1))) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(1, 1e6)
    ) +
    coord_flip() +
    scale_fill_manual(values = c('grey10', 'grey10',
                                 'grey40', 'grey40',  
                                 'grey80', 'grey80')) +
    scale_x_discrete(labels = Psource.labels) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  Pload.p
  
  # WB N load 2011-2020 average
  NloadWB.p <- ggplot(excr.WB.load %>% filter(!is.na(Nload)), aes(x = Source, y = Nload, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = pmax(Nload - Nload.se, 1), ymax = Nload + Nload.se),
                  width = 0.2, color = "grey50") +
    labs(title = "(c) Western basin mean (2011-2020)",
         x = "",
<<<<<<< HEAD
         y = expression(N ~ load ~ (Mg~yr^-1))) +
=======
         y = expression(N ~ load ~ (tonnes~yr^-1))) +
>>>>>>> 5b03cbe5a5a16efb29b134c14b1f557a084623dd
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    coord_flip(ylim = c(1, 1*10^6)) +
    scale_fill_manual(values = c('grey10', 'grey10',
                                 'grey40', 'grey40',  
                                 'grey80', 'grey80')) +
    scale_x_discrete(labels = c('Dreissenid NH4+', 'Fish NH4+',
                                'Tributary TKN')) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  NloadWB.p
  
  # WB P load 2011-2020 average
  PloadWB.p <- ggplot(excr.WB.load, aes(x = Source, y = Pload, fill = Source)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = pmax(Pload - Pload.se, 1), ymax = Pload + Pload.se),
                  width = 0.2, color = "grey50") +
    labs(title = "(d) Western basin mean (2011-2020)",
         x = "",
         y = expression(P ~ load ~ (Mg~yr^-1))) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      limits = c(1, 1e6)
    ) +
    coord_flip() +
    scale_fill_manual(values = c('grey10', 'grey10',
                                 'grey40', 'grey40',  
                                 'grey80', 'grey80')) +
    scale_x_discrete(labels = Psource.labels) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold")) 
  PloadWB.p
  
  ggarrange(Nload.p, Pload.p,
            NloadWB.p, PloadWB.p,
            nrow = 2, ncol = 2,
            font.label = list(size = 10), label.x = 0.25, label.y = 1,
            legend = 'none', align = 'v')
  
  # combine plots ----
  
  ggsave('tables_figures/final-tables_figures/Fig6.tiff', 
         width = 17, height = 12, units = 'cm', dpi = 600, 
        scaling = 0.8, compression = 'lzw', bg = 'white')   
  
  # Figure S1 ----
  # N excretion
  NexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.N.excr) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(N~excretion~(μg~N~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank()) 
  
  NexcrSeas.sub.p
  
  # P excretion
  PexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.P.excr) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(P~excretion~(μg~P~g^-1~h^-1))))) +
    theme(axis.text.x = element_blank()) 
  PexcrSeas.sub.p
  
  # N:P excretion
  NPexcrSeas.sub.p <- plot_season(excr.seas.sub, excr.seas.sub$masscorr.NP.excr) +
    labs(x = 'Sampling',
         y = expression(atop("Mass-specific", 
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
         y = expression(atop("Mass-specific", 
                             paste(N~excretion~(μg~N~g^-1~h^-1))))) 
  NexcrbN.p
  
  # N excretion vs tissue C:N
  NexcrbCN.p <- plot_si(excr.SI$BodyCN, excr.SI$masscorr.N.excr) +
    labs(x = 'Tissue C:N (molar)',
         y = '') +
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
  NexcrTemp.p <- ggplot(lmN.temp.pred, aes(x = Temp, y = fit,
                                  color = Season)) +
    geom_point(data = excr, aes(x = Temp, y = masscorr.N.excr),
               size = point.size, alpha = fill.alpha) +
    geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = .2) +
    geom_line(linewidth = line.width, colour = 'black') +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(N~excretion~(μg~N~g^-1~h^-1))))) +
    scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(trans = 'log10') +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank()) + 
    scale_colour_manual(name = 'Sampling',
                        labels = Sampling.labels,
                        values = Sampling.colors)
  NexcrTemp.p
  
  # P excretion
  PexcrTemp.p <- ggplot(excr, aes(x = Temp, y = masscorr.P.excr,
                                  color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.P.excr'),
               aes(yintercept = Mean), linetype = 'dashed',
               linewidth = line.width) +
    labs(x = '',
         y = expression(atop("Mass-specific", 
                             paste(P~excretion~(μg~P~g^-1~h^-1))))) +
    scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(trans = 'log10') +
    theme_classic(base_size = 10) +
    theme(axis.text.x = element_blank()) + 
    scale_colour_manual(name = 'Sampling',
                        labels = Sampling.labels,
                        values = Sampling.colors)
  PexcrTemp.p
  
  # N:P excretion
  NPexcrTemp.p <- ggplot(excr, aes(x = Temp, y = masscorr.NP.excr,
                                   color = Season)) +
    geom_point(size = point.size, alpha = fill.alpha) +
    geom_hline(data = excr.ss %>% filter(Variable == 'masscorr.NP.excr'),
               aes(yintercept = Mean), linetype = 'dashed',
               linewidth = line.width) +
    labs(x = 'Temperature (°C)',
         y = expression(atop("Mass-specific", 
                             paste(N:P~excretion~(molar))))) +
    scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(trans = 'log10') +
    theme_classic(base_size = 10) + 
    scale_colour_manual(name = 'Sampling',
                        labels = Sampling.labels,
                        values = Sampling.colors)
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
  write_csv(excr.lw.final, "output/excr_lw_final.csv")
  write_csv(excr.WB.final, "output/excr_WB_final.csv")
  
