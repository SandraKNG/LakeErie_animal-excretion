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
  library(rstatix)
  
  # check for outliers ----
<<<<<<< HEAD
<<<<<<< HEAD
  ggboxplot(excr, "Season", "masscorr.N.excr", color = 'Species.code',
            palette = 'viridis') 
  outliers <- excr %>% 
    select(c(masscorr.N.excr, Season, Species.code)) %>% 
    group_by(Season) %>% 
    identify_outliers(masscorr.N.excr)
=======
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
  ggboxplot(excr, "Season", "massnorm.N.excr", color = 'Species.code',
            palette = 'viridis') 
  excr %>% 
    select(c(massnorm.N.excr, Season, Species.code)) %>% 
    group_by(Season) %>% 
    identify_outliers(massnorm.N.excr)
  
  # normality check
  # Build the linear model
  model  <- lm(log10(massnorm.C.excr) ~ DOC.level, data = excr.aov)
  # Create a QQ plot of residuals
  ggqqplot(residuals(model))
  
  # Compute Shapiro-Wilk test of normality
  shapiro_test(residuals(model))
  
  # QQ plot for each group level
  ggqqplot(excr.aov, 'massnorm.C.excr', facet.by = 'DOC.level')
  
  # check homogeneity of variances
  excr.aov %>% levene_test(massnorm.C.excr ~ DOC.level)
<<<<<<< HEAD
>>>>>>> 561e07aec1796006abeee7626848271440840a25
=======
>>>>>>> 561e07aec1796006abeee7626848271440840a25
  
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
  Nexcr15N.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                       aes(x = d15N, y = Log10.massnorm.N.excr)) +
    geom_point(size = 6, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    # geom_smooth() +
    labs(x = '',
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(N~excretion~"(μg N/g/h)")))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_grey(base_size = 34) +
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
  Nexcr13C.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                       aes(x = d13C, y = Log10.massnorm.N.excr)) +
    geom_point(size = 6, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    labs(x = '',
         y = '') +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_grey(base_size = 34) +
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
  
  #..N excretion vs C:N ----
  NexcrCN.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                      aes(x = C.N, y = Log10.massnorm.N.excr)) +
    geom_point(size = 4, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    # geom_smooth() +
    labs(x = '',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(N~excretion~(μg~N/g/h))))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_classic(base_size = 34) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  NexcrCN.p
  
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
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(P~excretion~(μg~P/g/h))))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    theme_classic(base_size = 34) +
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
  Pexcr15N.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                       aes(x = d15N, y = Log10.massnorm.P.excr)) +
    geom_point(size = 6, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    # geom_smooth() +
    labs(x = expression(δ^15~'N (‰)'),
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(P~excretion~"(μg P/g/h)")))) +
    theme_grey(base_size = 34) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          #axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  Pexcr15N.p
  
  # ..P excretion vs d13C ----
  Pexcr13C.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                       aes(x = d13C, y = Log10.massnorm.P.excr)) +
    geom_point(size = 6,  aes(color = Season.bin)) + 
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    labs(x = expression(δ^13~'C (‰)'),
         y = '') +
    theme_grey(base_size = 34) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    scale_shape_manual(name = 'Species',
                       values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          #axis.text.x = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  Pexcr13C.p
  
  # ..P excretion vs C:N ----
  PexcrCN.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                      aes(x = C.N, y = Log10.massnorm.P.excr)) +
    geom_point(size = 4, aes(color = Season.bin)) +
    #shape = Species.code)) +
    # geom_smooth(method = lm, formula = y ~ x) +
    # geom_smooth() +
    labs(x = 'C:P',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme_classic(base_size = 34) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  PexcrCN.p
  
  # ..N:P excretion vs d15N ----
  NPexcr15N.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                        aes(x = d15N, y = Log10.massnorm.NP.excr)) +
    geom_point(size = 6, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    # geom_smooth() +
    labs(x = expression(δ^15~'N (‰)'),
         y = expression(atop(Log[10]~"mass-normalized", 
                             paste(N:P~excretion~"(molar)")))) +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_grey(base_size = 34) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  NPexcr15N.p
  
  # ..N:P excretion vs d13C ----
  NPexcr13C.p <- ggplot(excr, #%>% filter(Log10.massnorm.N.excr.sp < 2.5),
                        aes(x = d13C, y = Log10.massnorm.NP.excr)) +
    geom_point(size = 6, aes(color = Season.bin)) +
    #shape = Species.code)) +
    geom_smooth(method = lm, formula = y ~ x, color = 'black') +
    # geom_smooth() +
    labs(x = expression(δ^13~'C (‰)'),
         y = "") +
    scale_colour_manual(name = 'Season',
                        labels = c("Summer", "Fall"),
                        values = c("goldenrod2", "#D16103")) +
    # scale_shape_manual(name = 'Species',
    #                    values = c(3, 7, 15, 9, 8, 17, 19)) +
    theme_grey(base_size = 34) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'right')
  NPexcr13C.p
  
  # combine plots ----
  ggarrange(NexcrSp.p, PexcrSp.p, nrow = 2, heights = c(1,1),
            labels = c("(a)", "(b)"),
            font.label = list(size = 26), label.x = 0.1, label.y = 1,
            legend = 'top', common.legend = T, align = 'v')
  ggsave('figures/preliminary-figures/N_P_excretion_sp.png', 
         width = 20, height = 14, 
         units = 'in', dpi = 600)
  
  ggarrange(Nexcr15N.p, Nexcr13C.p, Pexcr15N.p, Pexcr13C.p,
            #NPexcr15N.p, NPexcr13C.p,
            nrow = 2, ncol = 2,
            labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
            font.label = list(size = 34), legend = 'none',
            label.x = 0.25, label.y = 1.02, common.legend = T,
            align = 'v')
  ggsave('figures/preliminary-figures/N_P_excretion_SI.png', 
         width = 16, height = 14, 
         units = 'in', dpi = 600)
  
  # mussel only ----
  # ..P excretion vs %P
  PexcrPm.p <- ggplot(excr %>%  filter(Species.code == 'QM'),
                      aes(x = P.tissue, y = Log10.massnorm.P.excr)) +
    geom_point()
  
  PexcrPm.p
  
  # ..N excretion rate vs season ----
  NexcrSea.p <- ggplot(excr,
                       aes(x = Season.bin, y = Log10.massnorm.N.excr, 
                           color = Season.bin)) +
    stat_halfeye(adjust = .5, width = .6, .width = 0,justification = -.3,
                 alpha = .3, aes(fill = Season.bin)) + 
    geom_boxplot(width = .25, size = 1, outlier.shape = NA, 
                 aes(fill = Season.bin), alpha = .2) +
    geom_point(size = 5, alpha = .3, 
               position = position_jitter(seed = 1, width = .1)) +
    # stat_compare_means(label.x = 0.7, label.y = 2.74,
    #                    label = 'p.format', size = 10) +
    stat_compare_means(comparisons = list(c(1, 2)),
                       label = 'p.signif', size = 5.5,
                       bracket.size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(N~excretion~(μg~N/g/h))))) +
    theme_grey(base_size = 34) +
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
    geom_boxplot(width = .25, size = 0.8, outlier.shape = NA,
                 aes(fill = Season.bin), alpha = .2) +
    geom_point(size = 5, alpha = .3, 
               position = position_jitter(seed = 1, width = .1)) +
    # stat_compare_means(label.x = 0.7, label.y = 2.74,
    #                    label = 'p.format', size = 10) +
    stat_compare_means(comparisons = list(c(1, 2)),
                       label = 'p.signif', size = 5.5,
                       bracket.size = 1) +
    labs(x = '',
         y = expression(atop(Log[10]~mass-normalized, 
                             paste(P~excretion~(μg~P/g/h))))) +
    theme_grey(base_size = 34) +
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
            font.label = list(size = 34), label.x = 0.14, label.y = 1,
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
    geom_point(size = 6) +
    geom_line(size = 2) +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(N~excretion~"(μg N/ha/h)")))) +
    theme_grey(base_size = 34) +
    scale_color_viridis(option = 'D',
                        name = 'Species',
                        labels = Species.pop,
                        discrete = T) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          axis.text.x = element_blank(),
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
    geom_point(size = 6) +
    geom_line(size = 2) +
    labs(x = '',
         y = expression(atop(Log[10]~population, 
                             paste(P~excretion~"(μg P/ha/h)")))) +
    theme_grey(base_size = 34) +
    scale_color_viridis(option = 'D',
                        name = 'Species',
                        labels = Species.pop,
                        discrete = T) +
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
            font.label = list(size = 34), label.x = 0.18, label.y = 1,
            legend = 'right', align = 'v', common.legend = T)
  ggsave('figures/preliminary-figures/Pop_N_Pexcretion_year.png', 
         width = 14, height = 14, 
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