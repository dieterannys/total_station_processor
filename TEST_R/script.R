###############################################################################
#                                                                  STARTER CODE

# Config vars
fn <- "170724-TP20.txt"

# Libraries
library(ggplot2)
library(gridExtra)

# Sources
source("func.R")
source("func_acadcom.R")
source("func_easy.R")

# PTS loading
pts <- read.csv(fn, sep = ",")
names(pts) <- c("id", "x", "y", "z")

# Plot entire PTS
plot_data_3D(xyz_filt(pts, ""))

# Initial script commands
scr_data <- scr_init()


###############################################################################
#                                                                        BLOCKS

# SIF ROERMOND - 01 - Overall straightness & total length

circle_ids <- c("10", "20", "30", "40", "50", "60", "70", "80", "90")

for (crc in circle_ids) {
    with_ucs(best_fit_ucs(xyz_filt(pts, crc)), xyz_filt(pts, crc), plot_data_3D)
    scr_data <- paste(scr_data, easy_bf_circle(xyz_filt(pts, crc)), sep = "")
}


###############################################################################
#                                                                    ENDER CODE

scr_export(scr_data)
