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
excel_file <- "data/HTLP_LakeErie_US_TP_SRP_2024-01-26_105214.xlsx"

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
              "TP_mgl", "SRP_qual", "SRP_mgl", "stream")
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
  SRP_mgl = SRP_mgl$mean
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
  SRP_mt = mean_tributary_df$discharge_lpd * mean_tributary_df$SRP_mgl / convert_fact_mg_mt
)

write.csv(tributary_daily_loads_mt, "output/tributary_daily_loads_mt.csv", row.names = F)

# get yearly loads
tributary_yearly_loads <- tributary_daily_loads_mt %>% 
  reframe(TP_mt = sum(TP_mt, na.rm = T),
          SRP_mt = sum(SRP_mt, na.rm = T))

write_csv(tributary_yearly_loads, "output/tributary_yearly_loads_mt.csv")

# # daily load plot loop vectors
# colnames <- names(tributary_daily_loads_mt)
# exclude_col <- c("stream", "date", "year")
# 
# variables_mean <- setdiff(colnames, exclude_col)
# streams <- unique(tributary_daily_loads_mt$stream)
# 
# # generate a random vector of colors
# num_colors <- length(variables_mean)
# random_colors <- viridis(num_colors, option = "D")
# variable_colors <- setNames(random_colors, variables_mean) # consistent col/var matching across streams
# # trying to get subscript numbers in titles for NO3 and SO4 here... not working yet
# var_name <- c(expression(TP), expression(SRP), expression(NO[3]), expression(TKN), expression(SO[4]), expression(Cl))
# variable_names <- setNames(var_name, variables_mean)
# 
# # initialize list to store grid of variable load plots
# plot_grid_list <- list()
# 
# # loop plot for each stream
# for (s in streams){
#   plot_list <- list()
#   
#   for (v in variables_mean) {
#     plot_data <- tributary_daily_loads_mt %>% 
#       filter(year == 2022, stream == s)
#     
#    # var_name <- v
#     stream_name <- s
#     
#     p <- ggplot(plot_data, aes(x = as.Date(date), y = !!sym(v))) +
#       geom_line(color = variable_colors[v], alpha = 0.6, linewidth = 1) +
#       labs(title = paste0("Daily load ", variable_names[v], " in ", stream_name), 
#            x = "Date", 
#            y = paste0(variable_names[v], " mt")) +
#       theme_bw() +
#       theme(text = element_text(size = 14, face = "bold"))
#     
#     plot_list[[v]] <- p
#   }
#   
#   # create a grid arrangement for the current stream
#   stream_grid <- grid.arrange(grobs = plot_list, ncol = 2)
#   plot_grid_list[[s]] <- stream_grid
# }
# 
# # can access a specific grid of plots for a particular stream using name or index
# plot(plot_grid_list[["Maumee"]])
# 
# # write plots to figures folder in directory
# if (!file.exists("figures")) {
#   dir.create("figures")
# }
# 
# for (a in seq_along(plot_grid_list)) {
#   stream_name <- streams[a]
#   filename <- paste0("figures/", stream_name, "_variable_plots.png")
#   
#   ggsave(filename, 
#          plot_grid_list[[a]], 
#          width = 16, 
#          height = 10,
#          dpi = 500,
#          bg = "white")
# }
