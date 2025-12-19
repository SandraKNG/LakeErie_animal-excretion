################################################################################
# Calculating and graphing southern tributary nutrient loads in Lake Erie
# This script uses daily nutrient concentration and discharge data downloaded 
# from NCWQR at Heidelberg University: https://ncwqr-data.org/HTLP/Portal
# R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"
################################################################################

# load packages
library(readxl)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(viridis)

# read in Heidelberg excel xlsx file with rivers in separate sheets 
excel_file <- "data/HTLP_WB_LakeErie_US_TP_SRP_TKN_2025-12-11_070823_noREADME.xlsx"

# read in ECCC file
can_WB_load <- read_csv('data/ECC_Erie_P_loading/Annual_TP_SRP_Tributary_Loads_PT_PRS_charges_affluent_calculation.csv')
  
# pivot table
can_l <- can_WB_load %>% 
  pivot_wider(
    names_from = parameter,
    values_from = load
  ) %>% 
  rename(TP_mt = `TP/PT`,
         SRP_mt = `SRP/PRS`)


# get the list of sheet names
sheet_names <- excel_sheets(excel_file)

# initialize an empty list to store data frames
tributary_data <- list()

# loop through each sheet and read its contents
for (i in sheet_names) {
  data <- read_excel(excel_file, sheet = i)
  
  # extract stream name from the sheet name
  stream_name <- gsub("_samples$", "", i, perl = TRUE)
  
  # add a new column for stream
  data$stream <- stream_name
  
  # add modified data frame to the list
  tributary_data[[i]] <- data
}

# combine all sheet data frames into one
tributary_df <- do.call(rbind, tributary_data)

# adjust row and column names
rownames(tributary_df) <- NULL

names(tributary_df)

colnames <- c("datetime", "discharge_qual", "discharge_cfs","TP_qual", 
              "TP_mgl", "SRP_qual", "SRP_mgl", "TKN_qual", "TKN_mgl", "stream")
colnames(tributary_df) <- colnames

# tributary_df %>% select(-c("datetime", "qual1", "qual2", "qual3"))

daily_summary_stats <- function(data, datetime, value) {
  result <- data %>%
    mutate(date = as.Date({{ datetime }})) %>%  
    group_by(stream, date) %>%
    summarise(
      date = first(date),
      mean = mean({{ value }}),
      sd = sd({{ value }}),
      n = n()
    ) %>%
    mutate(
      se_value = sd / sqrt(n),
      ci_lower = mean - 1.96 * se_value,
      ci_upper = mean + 1.96 * se_value
    )
  
  return(result)
}

# create variables vector without date or quality flag columns
exclude_columns <- c("datetime", grep("_qual", colnames, value = TRUE))
variables <- setdiff(colnames, exclude_columns)

# initialize list
df_names <- list()

# summary stats loop
for (j in variables) {
  df_name <- (j)
  assign(df_name, daily_summary_stats(tributary_df, datetime = datetime, value = !!sym(j)))
  df_names[[df_name]] <- get(df_name)  # store data frame in the list
}

# create new df of daily means
mean_tributary_df <- data.frame(
  stream = discharge_cfs$stream,
  date = discharge_cfs$date,
  discharge_cfs = discharge_cfs$mean,
  TP_mgl = TP_mgl$mean,
  SRP_mgl = SRP_mgl$mean,
  TKN_mgl = TKN_mgl$mean
) 

# convert cfs to total litres per day
convert_fact_cfs_ls = 28.3168466 # liters per cubic foot
convert_fact_spd = 86400 # number of seconds per day
convert_fact_mg_kg <- 1000000 # 1000000 mg per kg
convert_fact_mg_mt <- 1000000000 # 1000000000 mg per metric ton

mean_tributary_df$discharge_lpd <- mean_tributary_df$discharge_cfs * convert_fact_cfs_ls * convert_fact_spd

tributary_daily_loads_mt <- data.frame(
  stream = mean_tributary_df$stream,
  date = mean_tributary_df$date,
  year = year(mean_tributary_df$date),
  TP_mt = mean_tributary_df$discharge_lpd * mean_tributary_df$TP_mgl / convert_fact_mg_mt,
  SRP_mt = mean_tributary_df$discharge_lpd * mean_tributary_df$SRP_mgl / convert_fact_mg_mt,
  TKN_mt = mean_tributary_df$discharge_lpd * mean_tributary_df$TKN_mgl / convert_fact_mg_mt
)

tributary_daily_loads_mt <- tributary_daily_loads_mt %>%
  mutate(
    water_year = year(date) + if_else(month(date) >= 10, 1, 0)
  )

write.csv(tributary_daily_loads_mt, "output/tributary_WB_us_daily_loads_mt.csv", row.names = F)

# get yearly loads
tributary_yearly_loads <- tributary_daily_loads_mt %>% 
  group_by(water_year) %>% 
  reframe(TP_mt = sum(TP_mt, na.rm = T),
          SRP_mt = sum(SRP_mt, na.rm = T),
          TKN_mt = sum(TKN_mt, na.rm = T))

write_csv(tributary_yearly_loads, "output/tributary_WB_us_yearly_loads_mt.csv")

# get average from 2011 to 2020
tributary_mean_yearly_loads <- tributary_yearly_loads %>% 
  reframe(TP_mt = mean(TP_mt, na.rm = T),
          SRP_mt = mean(SRP_mt, na.rm = T),
          TKN_mt = mean(TKN_mt, na.rm = T))

write_csv(tributary_mean_yearly_loads, "output/tributary_WB_us_mean_2011_2020_loads_mt.csv")

# calculate mean loads from Canada
can_yearly_l <- can_l %>% 
  filter(stream %in% c('Sydenham', 'Thames', 'Canard', 'Turkey')) %>% 
  group_by(water_year) %>% 
  reframe(
    TP_mt  = sum(TP_mt,  na.rm = TRUE),
    SRP_mt = sum(SRP_mt, na.rm = TRUE)
  ) 

# get average from 2011 to 2020
can_mean_yearly_l <- can_yearly_l %>% 
  reframe(TP_mt = mean(TP_mt, na.rm = T),
          SRP_mt = mean(SRP_mt, na.rm = T))

write_csv(can_mean_yearly_l, "output/tributary_WB_can_mean_2011_2020_loads_mt.csv")
  
