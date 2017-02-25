library(readxl)
library(ggplot2)
library(ggthemes)

#########################################################################
# Load the data

######################
# Formal
formal <- read_excel("consultation_data.xlsx",
                     sheet = 1,
                     na = "NA")
formal <- formal[1:365, ]

# fix up the dates
names(formal)[11] <- "date_inf_start"
names(formal)[12] <- "date_form_start"
names(formal)[14] <- "date_form_end"
formal$date_form_end <- as.Date(as.numeric(formal$date_form_end),
                                format = "%Y-%m-%d",
                                origin = "1899-12-30")
formal$date_form_start <- as.Date(formal$date_form_start)
formal$date_inf_start <- as.Date(formal$date_inf_start)
formal$formal_duration <- as.numeric(formal$date_form_end - formal$date_form_start)
formal$total_duration <- as.numeric(formal$date_form_end - formal$date_inf_start)

# calculate pages
formal$total_len <- formal$len_proj_desc + 
                    formal$bkgrn_len +
                    formal$status_len +
                    formal$baseline_len +
                    formal$eff_len +
                    formal$CE_len

######################
# Informal
informal <- read_excel("consultation_data.xlsx",
                       sheet = 2,
                       na = "NA")
informal <- informal[1:275, ]
informal$Programmatic <- rep(0, length(informal[[1]]))
informal$LAA <- rep(0, length(informal[[1]]))

# fix up the dates
names(informal)[11:12] <- c("actual_start", "actual_end")
informal$actual_start <- as.Date(informal$actual_start)
informal$actual_end <- as.Date(informal$actual_end)
informal$total_duration <- as.numeric(informal$actual_end - informal$actual_start)
summary(informal$total_duration)

######################
# make the combined consults df
formal_sub <- formal[, c(3:8, 57:61, 67)]
names(formal_sub)
dim(formal_sub)
informal_sub <- informal[, c(3:8, 23:28)]
names(informal_sub)
dim(informal_sub)
combo <- rbind(formal_sub, informal_sub)
dim(combo)

# make the no programmatics df (useful for plots)
no_prog <- formal[formal$Programmatic == "0", ]
dim(no_prog)
all_no_prog <- combo[combo$Programmatic == "0", ]
dim(all_no_prog)

