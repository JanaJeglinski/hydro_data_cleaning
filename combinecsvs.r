## Combine all csv files in folder into one

# Load library
library(data.table)

# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("C:/Data/Hydrology/cleaned_site_logger/", pattern="*.csv", full.names=TRUE)

# read and row bind all data sets
data <- rbindlist(lapply(filenames,fread))

write.csv(data,"C:/Data/Hydrology/cleaned_site_logger/combined/combined_hydrology_daily_means_all_sites.csv")

