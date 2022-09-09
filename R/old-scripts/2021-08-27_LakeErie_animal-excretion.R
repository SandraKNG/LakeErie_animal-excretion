  ######## Aquatic animals are an important source of nutrients ####
  ######## in the western basin of Lake Erie
  # This code was created by S. Klemet-N'Guessan in 2021
  # R version 4.1.0
  
  # load libraries and read datasets ----
  
  library(RColorBrewer)
  library(tidyverse)
  library(rfishbase) # to get fish trophic position using fishbase.org database
  
  # load dataset ----
  er <- read.csv('21 08 26 Lake Erie Mastersheet.csv',
                 stringsAsFactors = F, na.strings = c("", "NA", "."), 
                 strip.white = TRUE, sep = ",")
  
  str(er) 
  head(er)
  
  # clean dataset
  excr <- er %>%
    rename(ID = ï..ID,
           Mass = Ind..dry.mass..g.,
           Numb.ind = X..indiv.,
           P.excretion.rate = P.excretion.rate..ug.h.ind.) %>% 
    mutate(Log10.mass = log10(Mass),
           Log10.P.excretion.rate = log10(P.excretion.rate)) 
  
  # plot log10 P excretion rate vs log10 mass
  ggplot(excr %>% filter(Species.code == 'QM'), 
         aes(x = Log10.mass, y = Log10.P.excretion.rate)) +
    geom_point()
  verts.m <- lm(Log10.P.excretion.rate ~ Log10.mass, 
                data = excr %>% filter(Species.code != 'QM'))
  verts.coeff <- verts.m$coefficients["Log10.mass"]
  inverts.m <- lm(Log10.P.excretion.rate ~ Log10.mass, 
                  data = excr %>% filter(Species.code == 'QM',
                                         Log10.P.excretion.rate < -0.6))
  inverts.coeff <- inverts.m$coefficients["Log10.mass"]
  
  # ..SP: Sort observations by species ----
  obs.spsummary <- excr %>% 
    group_by(Species.code) %>% 
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
    filter(n >11) %>% 
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
  excr1 <- excr %>% filter(Species.code != "QM") %>% 
    mutate(massnorm.P.excr = P.excretion.rate/(Mass^verts.coeff))
  
  excr2 <- excr %>% filter(Species.code == "QM",
                          Log10.P.excretion.rate < -0.6) %>% 
    mutate(massnorm.P.excr = P.excretion.rate/(Mass^inverts.coeff))
  
  # P excretion rate vs species
  species <- c('Goldfish', 'Gizzard shad', 'Largemouth bass', 'Walleye', 'White perch',
               'Yellow bullhead', 'Yellow perch')
  Pexcr.p <- ggplot(excr %>% filter(Species.code != "CTL1", 
                         Species.code != "CTL2",
                         Species.code != "CTL3", 
                         Species.code != "CTL4",
                         Species.code != "CTL5", 
                         Species.code != "CTL6"),
         aes(x = Species.code, y = massnorm.P.excr)) +
    geom_point(excr1, size = 3, alpha = .5) +
    geom_boxplot(excr1, alpha = 0) +
    geom_point(excr2, size = 3, alpha = .5) +
    geom_boxplot(excr2, alpha = 0) +
    labs(x = 'Species',
         y = expression(atop(Mass-corrected~P~excretion, 
                             paste((μg~P/g/h))))) +
    theme_classic(base_size = 20) +
    scale_x_discrete(labels = species ) +
    theme(axis.text = element_text(face = 'bold'),
          axis.line = element_line(size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
          legend.title = element_text(face = 'bold'),
          legend.margin = margin(.15, .15, .15, .15, 'cm'),
          legend.key.height = unit(2, 'lines'),
          legend.key.width = unit(3, 'lines'),
          legend.position = 'none')
  Pexcr.p
  
  ggsave('preliminary figures for trends/P excretion vs species.png', 
         width = 20, height = 14, 
         units = 'cm', dpi = 300)
  
  anova(lm(massnorm.P.excr ~ Incub..Temperature, data = excr))
  
  
  