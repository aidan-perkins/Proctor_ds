rm(list = ls())

#last updated, 8/1/2024, 

install.packages("tidyverse") 
install.packages("data.table")
install.packages("lubridate")
install.packages("splines")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2") 
install.packages("cowplot")
install.packages("lfe")
install.packages("Hmisc")
install.packages("raster")
install.packages("sf")
install.packages("terra")
install.packages("stringr")
install.packages("terra")
install.packages("rms")
install.packages("ggmap")
install.packages("leaflet")
install.packages("Rcpp")
install.packages("rasterVis")
install.packages("lattice")
install.packages("tmap")
install.packages("viridis")
install.packages("fields")
install.packages("maps")
install.packages("tigris")
install.packages("stargazer")
install.packages("dply")

library(tidyverse)
library(data.table)
library(lubridate)
library(splines)
library(stargazer)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(lfe)
library(Hmisc)
library(raster)
library(sf)
library(terra)
library(stringr)
library(bigmemory)
library(exactextractr)
library(dplyr)
library(tidyr)
library(data.table)
library(raster)
library(rms)
library(ggmap)
library(leaflet)
library(Rcpp)
library(rasterVis)
library(lattice)
library(tmap)
library(viridis)
library(fields)
library(maps)
library(tigris)
library(stargazer)
library(dplyr)

###########################################
### --- Merge in all data --- ###
###########################################
### data paths
maize <- "D:/R_temp/Maize_1999_2019_NASS.csv"
weather <- "D:/R_temp/weather_2020-09-15.csv"
AWC <- "D:/R_temp/awc_gNATSGO_US.tif"
MaizeCover <- "D:/R_temp/crop_frequency_corn_2008-2023.tif"
AWC_reprojected_path <- "D:/R_temp/AWC_reprojected.tif"
MaizeCover_aggregated_path <- "D:/R_temp/MaizeCover_aggregated.tif"
County_path <- "D:/R_temp/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"
AWC_aggregated_path <- "D:/R_temp/AWC_aggregated.tif"
AWC_WM_path <- "D:/R_temp/weighted_mean_df.csv"
Weighted_AWC_Maize_path <- "D:/R_temp/Weighted_AWC_Maize.tif"
maize_gs_path <- "D:/R_temp/maize_gs.csv"

### datasets
weather <- fread(weather)
maize = fread(maize)
AWC <- rast(AWC)
MaizeCover <- rast(MaizeCover)
AWC_reprojected <- rast(AWC_reprojected_path)
MaizeCover_aggregated <- rast(MaizeCover_aggregated_path)
county <- st_read(County_path)
AWC_aggregated <- rast(AWC_aggregated_path)
AWC_WM <- fread(AWC_WM_path)
WM_Rast <- rast(Weighted_AWC_Maize_path)
maize_gs <- fread(maize_gs_path)

###########################################
########Coarsened the Resolution of Maize to Match AWC
#####AWC and Maize, no need to run, think I used the aggregate tool shown below##########

# Load the terra package

if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
library(terra)

# Paths to the raster files
AWC_path <- "/Users/aidanperkins/Downloads/awc_gNATSGO/awc_gNATSGO_US.tif"
MaizeCover_path <- "/Users/aidanperkins/Downloads/Proctor/Proctor_1/Data/Crop_Frequency_2008-2023/crop_frequency_corn_2008-2023.tif"

# Read the raster files
AWC <- rast(AWC_path)
MaizeCover <- rast(MaizeCover_path)

# Resample MaizeCover to match the resolution and extent of AWC
MaizeCover_resampled <- resample(MaizeCover, AWC)

# Print the resampled raster
print(MaizeCover_resampled)

# Print the extent and CRS of both rasters
extent_AWC <- ext(AWC)
extent_MaizeCover_resampled <- ext(MaizeCover_resampled)

crs_AWC <- crs(AWC)
crs_MaizeCover_resampled <- crs(MaizeCover_resampled)

print(paste("Extent of AWC: ", extent_AWC))
print(paste("Extent of MaizeCover_resampled: ", extent_MaizeCover_resampled))

print(paste("CRS of AWC: ", crs_AWC))
print(paste("CRS of MaizeCover_resampled: ", crs_MaizeCover_resampled))

# Get the resolution of AWC
AWC_resolution <- res(AWC)

# Get the resolution of the resampled MaizeCover
MaizeCover_resampled_resolution <- res(MaizeCover_resampled)

# Print the resolution of both rasters
print(paste("Resolution of AWC (in degrees): ", AWC_resolution[1], "x", AWC_resolution[2]))
print(paste("Resolution of MaizeCover_resampled (in meters): ", MaizeCover_resampled_resolution[1], "x", MaizeCover_resampled_resolution[2]))

###########################################
#########Reproject AWC to match Maize CRS, no run##############

# Reproject the AWC dataset to NAD83 Conus Albers
AWC_reprojected <- project(AWC, Maize_Cover)

# Print the CRS of the reprojected dataset
print(crs(AWC_reprojected))

# Print the resolutions to verify they match
AWC_reprojected_res <- res(AWC_reprojected)
Maize_res <- res(Maize_Cover)

print(AWC_reprojected_res)
print(Maize_res)

# Save the reprojected raster
output_path <- "D:/R_temp/AWC_reprojected.tif"
writeRaster(AWC_reprojected, output_path, overwrite = TRUE)
output_path <- "D:/R_temp/MaizeCover_aggregated.tif"
writeRaster(AWC_reprojected, output_path, overwrite = TRUE)

########################################
##########AGGREGATE, no run#############
########################################

# Decrease the resolution by a factor of 5
factor <- 5
MaizeCover_aggregated <- aggregate(Maize_Cover, fact=factor, fun=mean)

# Stop the timer
end_time <- Sys.time()

# Print the new resolution
print(res(MaizeCover_aggregated))


# Install and load data.table package if not already installed
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}



###########################################
### --- Clean Data, did not add soy data --- ###
###########################################

# Convert data frames to data tables
maize <- as.data.table(maize)
#soy <- as.data.table(soy)

#############################################

# Convert data frames to data tables
maize <- as.data.table(maize)
#soy <- as.data.table(soy)

# Create GEOID by padding the State and County ANSI codes
maize[, GEOID := paste0(
  str_pad(`State ANSI`, 2, pad = "0"),
  str_pad(`County ANSI`, 3, pad = "0")
)]

#soy[, GEOID := paste0(
#  str_pad(`State ANSI`, 2, pad = "0"),
#  str_pad(`County ANSI`, 3, pad = "0")
#)]

# Get rid of NAs
maize <- maize[!is.na(`County ANSI`),]
#soy <- soy[!is.na(`County ANSI`),]

#soy[, GEOID := paste0(
#  str_pad(`State ANSI`, 2, pad = "0"),
#  str_pad(`County ANSI`, 3, pad = "0")
#)]

# Get rid of NAs
maize <- maize[!is.na(`County ANSI`),]
#soy <- soy[!is.na(`County ANSI`),]


# Remove unnecessary columns
maize[, `:=`(
  `Week Ending` = NULL,
  Period = NULL,
  `Geo Level` = NULL,
  `Zip Code` = NULL,
  Region = NULL,
  watershed_code = NULL,
  Watershed = NULL,
  `Domain Category` = NULL,
  `CV (%)` = NULL
)]

#soy[, `:=`(
#  `Week Ending` = NULL,
#  Period = NULL,
#  `Geo Level` = NULL,
#  `Zip Code` = NULL,
#  Region = NULL,
#  watershed_code = NULL,
#  Watershed = NULL,
#  `Domain Category` = NULL,
#  `CV (%)` = NULL
#)]

# Check for duplicates
duplicated_maize <- maize[duplicated(maize[, .(GEOID, Year)]),]
#duplicated_soy <- soy[duplicated(soy[, .(GEOID, Year)]),]

# Print the duplicates
print(duplicated_maize)
#print(duplicated_soy)


###########################################
### --- Continue with Maize for now, will process soy later --- ###
###########################################


#drop all rows in the weather data that are not GEOIDs in the yield data

# Subset weather to create weather_merged
weather_merged <- weather %>% 
  filter(GEOID %in% unique(maize$GEOID))

# Convert the date column to Date format
weather_merged$date <- as.Date(weather_merged$date)

# Extract the year from the date column
weather_merged$Year <- year(weather_merged$date)


#In this first case we're going for a long format to try out dlnm
#set the date of the yield observation to be the harvest date
#currently using May, June, July as in Rigden2020
plantdoy = yday(as.Date("2013-05-15"))
harvestdoy = yday(as.Date("2013-08-15"))

#plantdoy = yday(as.Date("2013-03-01"))
#harvestdoy = yday(as.Date("2013-09-01"))

maize$doy = harvestdoy

#Get variables to merge onto
weather_merged$Year = year(weather_merged$date)
weather_merged$doy = yday(weather_merged$date)


weather_merged = weather_merged[weather_merged$doy > (plantdoy - 1),]
weather_merged = weather_merged[weather_merged$doy <= harvestdoy]

weather_merged$INTPTLAT = as.numeric(as.character(weather_merged$INTPTLAT))
weather_merged$INTPTLON = as.numeric(as.character(weather_merged$INTPTLON))

weather_merged = weather_merged[Year<2020]

#mulitplying by 3 and dividing by 1000 makes it so the unit of CERESS_swdown is kWh/m2/day
weather_merged$CERES_swdown = weather_merged$CERES_swdown * 3 / 1000

###########################
#####PRISM TEMP DATA ######
###########################
write.table(weather_merged$PRISM_tmean, "daily_PRISM_tmean_temperature_vals.tab")

###########################################
### --- Define Functions --- ###
###########################################

namean = function(x) {mean(x,na.rm=T)}
namin = function(x) {min(x,na.rm=T)}
namax = function(x) {max(x,na.rm=T)}

# These functions are used for the restricted cubic spline values, same processing done for global ag
genRecenteredXVals_RCS = function(x, xRef, k) {
  
  xVals_reCentered = rcspline.eval(x, knots = k, inclx = T) - rcspline.eval(x = rep(xRef, length(x)), knots = k, inclx = T)
  
  return(xVals_reCentered)
  
}

RCSagg = function(x, col, xRef=20, k =  c(10,20,27,35)){
  
  return (genRecenteredXVals_RCS(x=x, xRef=xRef, k=k)[,col])
}




# gdd = function(x, t_low, t_high, returnMean=T) {
#   x = x * (x>t_low)
#   x[x >= t_low] = x[x >= t_low] - t_low
#   x[x >= (t_high - t_low)] = t_high - t_low
#   if(returnMean) {
#     return(namean(x))
#   } else {return(x)}
# }

# edd = function(x, t_high, returnMean=T) {
#   x = x * (x>t_high)
#   x[x >= t_high] = x[x >= t_high] - t_high
#   if(returnMean) {
#     return(namean(x))
#   } else {return(x)}
# }

#Calculates the exceedance degree days above a threshold using a sinusoidal approximation, as in S&R. 
#To calculate gdd, subtract the edd_sin of the kdd knot from the edd_sin of the gdd knot. 
# e.g. gdd = edd_10 - edd_30
#To calculate kdd, just use the edd_sin with the cutVal set at the kdd knot
# e.g. kdd = edd_30
edd_sin = function(tmin, tmax, cutVal, returnMean=T) {
  
  ### Let's do this with matrix algebra:
  #mean
  M = (tmax + tmin)/2
  #1/2 amp
  W = (tmax - tmin)/2
  
  #indicators for which case we're in
  #if your min temp is greater than the cutval it's easy
  I1 = tmin >= cutVal
  #If your min temp is below the cutval but max temp is above it this is the tricky one
  I2 = (tmin < cutVal) & (tmax > cutVal)
  #If you're too cold, you just get a zero
  I3 = (tmin < cutVal) & (tmax <= cutVal)
  
  ### Calculate the dd in each case
  dd1 = M - cutVal                           
  #theta
  inside = (cutVal - M)/W
  
  
  ## Calling asin and creating NAs is very slow. Set the NAs ahead of time #LS change
  inside[!I2] = NA  
  
  theta = asin(inside)
  
  
  dd2 = ((M-cutVal)*(pi/2 - theta) + W*cos(theta)) / pi
  dd2[is.na(dd2)] = 0 #NAs are set back to 0. asin never tries to deal with them.
  
  ### Sum them all together
  dd = I1*dd1 + I2*dd2 + I3*0
  
  if(returnMean) {
    return(namean(dd))
  } else {return(dd)}
  
}

###########################################
### --- Generate basis functions: summarize over growing season--- ###
###########################################


### --- Calculate growing season means of transformed daily values --- ###
#Calculate some useful mean values: 
gs_vals_singleVars = weather_merged[,.(lat = namean(INTPTLAT),
                                       lon = namean(INTPTLON),
                                       tmean = namean(PRISM_tmean),
                                       ppt = namean(PRISM_ppt),
                                       swdown = namean(CERES_swdown),
                                       smrz = namean(smrz),
                                       smrz_2 = namean(smrz^2),
                                       tmax = namean(PRISM_tmax),
                                       tmin_observed = namin(PRISM_tmean),
                                       tmax_observed = namax(PRISM_tmean),
                                       tmax_rcs_1 = namean(RCSagg(PRISM_tmax,2)),
                                       tmax_rcs_2 = namean(RCSagg(PRISM_tmax,3)),
                                       #tmean_2 = namean(PRISM_tmean^2),
                                       #tmean_3 = namean(PRISM_tmean^3),
                                       #tmean_4 = namean(PRISM_tmean^4),
                                       #tmean_5 = namean(PRISM_tmean^5),
                                       #tmean_6 = namean(PRISM_tmean^6),
                                       #tmean_7 = namean(PRISM_tmean^7),
                                       #tmean_8 = namean(PRISM_tmean^8),
                                       #edd_5 = edd_sin(PRISM_tmin, PRISM_tmax,5),
                                       edd_10 = edd_sin(PRISM_tmin, PRISM_tmax,10),
                                       #edd_15 = edd_sin(PRISM_tmin, PRISM_tmax,15),
                                       #edd_20 = edd_sin(PRISM_tmin, PRISM_tmax,20),
                                       #edd_25 = edd_sin(PRISM_tmin, PRISM_tmax,25),
                                       #edd_26 = edd_sin(PRISM_tmin, PRISM_tmax,26),
                                       #edd_27 = edd_sin(PRISM_tmin, PRISM_tmax,27),
                                       edd_28 = edd_sin(PRISM_tmin, PRISM_tmax,28),
                                       #edd_29 = edd_sin(PRISM_tmin, PRISM_tmax,29),
                                       #edd_30 = edd_sin(PRISM_tmin, PRISM_tmax,30),
                                       #edd_35 = edd_sin(PRISM_tmin, PRISM_tmax,35),
                                       ppt_2 = namean(PRISM_ppt^2),
                                       swdown_2 = namean(CERES_swdown^2)),
                                    by = .(GEOID,Year)]


### Merge together any of the features that we want to merge with the yield data:

gs_vals =  gs_vals_singleVars


###########################################
### --- SOY merge with yield data, no soy data--- ###
###########################################
soy$doy = harvestdoy

soy_gs = merge(soy, gs_vals, by = c("GEOID","Year"), all.y = T)
num = soy_gs[!is.na(Value),.N,GEOID]
soy_gs = merge(soy_gs, num, by = "GEOID", all.x = T)

#Drop counties with fewer than 10 yield observations
soy_gs = soy_gs[N >= 10,]

#Drop counties west of the 100th meridian, following S&R. Also drop florida (already gone). 
soy_gs = soy_gs[lon > -100]

#Make useful variables:
soy_gs$Year = soy_gs$Year
soy_gs$Year_2 = soy_gs$Year^2
soy_gs$ln_yield = log(soy_gs$Value)

### Clean up: 
soy_gs$Program = NULL
soy_gs$`Ag District` = NULL
soy_gs$`Ag District Code` = NULL
soy_gs$Domain = NULL
soy_gs$doy = NULL
soy_gs$N = NULL

colnames(soy_gs)[colnames(soy_gs) == "Value"] = "yield"

head(maize_gs)

soy_gs = soy_gs[!is.na(edd_10) & !is.na(edd_28) & !is.na(ln_yield),]
soy_gs = soy_gs[!is.nan(edd_10) & !is.nan(edd_28) & !is.nan(ln_yield) & ln_yield > 0,]


###########################################
### --- estimate regressions --- ###
###########################################
# Calculate the mean
mean_awc <- mean(maize_gs$weighted_mean_AWC, na.rm = TRUE)

# Calculate the standard deviation
sd_awc <- sd(maize_gs$weighted_mean_AWC, na.rm = TRUE)

# Print the results
cat("Mean of weighted_mean_AWC:", mean_awc, "\n")
cat("Standard Deviation of weighted_mean_AWC:", sd_awc, "\n")

str(maize_gs)
head(maize_gs)
str(maize_gs$weighted_mean_AWC)


# Filter out rows with NA values in edd_10, edd_28, and ln_yield
maize_gs <- maize_gs[!is.na(maize_gs$edd_10) & !is.na(maize_gs$edd_28) & !is.na(maize_gs$ln_yield), ]

# Remove rows with -Inf values in ln_yield
maize_gs <- maize_gs[!is.infinite(maize_gs$ln_yield), ]

# Recalculate the variance of ln_yield
ln_yield_var <- var(maize_gs$ln_yield)

maize_gs = maize_gs[!is.na(edd_10) & !is.na(edd_28) & !is.na(ln_yield),]
maize_gs = maize_gs[!is.nan(edd_10) & !is.nan(edd_28) & !is.nan(ln_yield) & ln_yield > 0,]

#original models 
#mod0 = felm(ln_yield ~ edd_10 + edd_28 | as.factor(GEOID) + as.factor(State):as.numeric(Year), data = maize_gs, keepCX = T, keepX = T)
#mod1 = felm(ln_yield ~ edd_10 + edd_28 + ppt + ppt_2  | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod2 = felm(ln_yield ~ edd_10 + edd_28 + swdown + swdown_2  | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod3 = felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2  | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod4 = felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2 + swdown + swdown_2  | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod5 = felm(ln_yield ~ edd_10 + edd_28 | as.factor(GEOID), data = maize_gs, keepCX = T, keepX = T)
#mod6 = felm(ln_yield ~ edd_10 + edd_28 | as.factor(State): as.numeric(Year), data = maize_gs)

# Updated models with interactions

#mod0 <- felm(ln_yield ~ edd_10 + edd_28 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | as.factor(GEOID) + as.factor(State):as.numeric(Year), data = maize_gs, keepCX = TRUE, keepX = TRUE)
#mod1 <- felm(ln_yield ~ edd_10 + edd_28 + ppt + ppt_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod2 <- felm(ln_yield ~ edd_10 + edd_28 + swdown + swdown_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod3 <- felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod4 <- felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2 + swdown + swdown_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
#mod5 <- felm(ln_yield ~ edd_10 + edd_28 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | as.factor(GEOID), data = maize_gs, keepCX = TRUE, keepX = TRUE)
#mod6 <- felm(ln_yield ~ edd_10 + edd_28 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | as.factor(State):as.numeric(Year), data = maize_gs)

#now with updated mod0 and 1 to include precipitation
mod0 <- felm(ln_yield ~ edd_10 + edd_28 + ppt + ppt_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC + ppt:weighted_mean_AWC + ppt_2:weighted_mean_AWC | as.factor(GEOID) + as.factor(State):as.numeric(Year), data = maize_gs, keepCX = TRUE, keepX = TRUE)
mod1 <- felm(ln_yield ~ edd_10 + edd_28 + ppt + ppt_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC + ppt:weighted_mean_AWC + ppt_2:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
mod2 <- felm(ln_yield ~ edd_10 + edd_28 + swdown + swdown_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
mod3 <- felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
mod4 <- felm(ln_yield ~ edd_10 + edd_28 + smrz + smrz_2 + swdown + swdown_2 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | GEOID + as.factor(State):as.numeric(Year), data = maize_gs)
mod5 <- felm(ln_yield ~ edd_10 + edd_28 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | as.factor(GEOID), data = maize_gs, keepCX = TRUE, keepX = TRUE)
mod6 <- felm(ln_yield ~ edd_10 + edd_28 + edd_10:weighted_mean_AWC + edd_28:weighted_mean_AWC | as.factor(State):as.numeric(Year), data = maize_gs)



stargazer(mod0, mod1, mod2, mod3, mod4, mod5, mod6, type = "text")

text_output <- capture.output(
  stargazer(mod0, mod1, mod2, mod3, mod4, mod5, mod6, type = "text",
            title = "Regression Results",
            dep.var.labels = c("Dependent Variable Name"),
            column.labels = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
            align = TRUE,
            no.space = TRUE)
)

# Write the text output to a file
regression_file_path <- "D:/R_temp/Plots/regression_results_precip.txt"
cat(text_output, sep = "\n", file = regression_file_path)


#######RESPONSE FUNCTIONS########
#################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

##### PPT vs yield response curve###

# Model results (insert the coefficients from your model here)
coefficients <- list(
  ppt = 0.049,
  ppt_2 = -0.001
)

# Generate a sequence of values for ppt
ppt_seq <- seq(min(maize_gs$ppt, na.rm = TRUE),
               max(maize_gs$ppt, na.rm = TRUE),
               length.out = 100)

# Define a function to predict yield based on ppt and ppt_2
predict_yield <- function(ppt, coefficients) {
  yield <- coefficients$ppt * ppt +
    coefficients$ppt_2 * (ppt^2)
  return(yield)
}

# Create a dataframe for plotting
plot_data <- data.frame(
  ppt = ppt_seq,
  predicted_yield = sapply(ppt_seq, predict_yield, coefficients = coefficients)
)

# Plot the response curve for ppt
ggplot(plot_data, aes(x = ppt, y = predicted_yield)) +
  geom_line(color = "blue") +
  labs(title = "Response Curve of Yield to ppt",
       x = "Precipitation (ppt)",
       y = "Predicted Yield (ln_yield)") +
  theme_minimal()


##################################################
#### GGD and KDD temperature curve###############

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Model results (insert the coefficients from your model here)
coefficients_gdd <- list(
  gdd = 0.015
)

coefficients_kdd <- list(
  kdd = -0.363
)

# Define a function to predict yield based on GDD (edd_10)
predict_yield_gdd <- function(gdd, coefficients) {
  yield <- coefficients$gdd * gdd
  return(yield)
}

# Define a function to predict yield based on KDD (edd_28)
predict_yield_kdd <- function(kdd, coefficients) {
  yield <- coefficients$kdd * kdd
  return(yield)
}

# Create dataframes for plotting
plot_data_gdd <- data.frame(
  gdd = gdd_seq,
  predicted_yield = sapply(gdd_seq, predict_yield_gdd, coefficients = coefficients_gdd)
)

plot_data_kdd <- data.frame(
  kdd = kdd_seq,
  predicted_yield = sapply(kdd_seq, predict_yield_kdd, coefficients = coefficients_kdd)
)

# Plot the response curve for GDD
gdd_plot <- ggplot(plot_data_gdd, aes(x = gdd, y = predicted_yield)) +
  geom_line(color = "blue") +
  labs(title = "Response Curve of Yield to GDD (edd_10)",
       x = "Growing Degree Days (GDD)",
       y = "Predicted Yield (ln_yield)") +
  theme_minimal()

# Plot the response curve for KDD
kdd_plot <- ggplot(plot_data_kdd, aes(x = kdd, y = predicted_yield)) +
  geom_line(color = "blue") +
  labs(title = "Response Curve of Yield to KDD (edd_28)",
       x = "Killing Degree Days (KDD)",
       y = "Predicted Yield (ln_yield)") +
  theme_minimal()

# Display the plots
print(gdd_plot)
print(kdd_plot)


#####################################################
#########plotting KDD and GDD together##############

# Load necessary libraries
library(ggplot2)

# Define temperature range (e.g., from 0°C to 40°C)
temperature_seq <- seq(0, 40, by = 0.01)

# Calculate GDD and KDD for each temperature
calculate_gdd_kdd <- function(temp) {
  gdd <- ifelse(temp >= 10 & temp < 28, temp - 10, 0)
  kdd <- ifelse(temp >= 28, temp - 28, 0)
  return(c(gdd, kdd))
}

gdd_kdd_values <- t(sapply(temperature_seq, calculate_gdd_kdd))
colnames(gdd_kdd_values) <- c("GDD", "KDD")

# Apply the model coefficients
coefficients_gdd <- 0.015  # From Model 0
coefficients_kdd <- -0.363 # From Model 0

predicted_yield <- gdd_kdd_values[, "GDD"] * coefficients_gdd +
  gdd_kdd_values[, "KDD"] * coefficients_kdd

# Create dataframe for plotting
plot_data_kdd_gdd <- data.frame(
  temperature = temperature_seq,
  predicted_yield = predicted_yield
)

# Plot the response function
ggplot(plot_data, aes(x = temperature, y = predicted_yield)) +
  geom_line(color = "blue") +
  labs(title = "Response Curve of Yield to Temperature",
       x = "Temperature (°C)",
       y = "Predicted Yield (ln_yield)") +
  theme_minimal()

##################################
### response curves takes growing season in account#####
### changes from log scale to percentage###############

# Load necessary libraries
library(ggplot2)

# Define temperature range (e.g., from 0°C to 40°C)
temperature_seq <- seq(0, 40, by = 0.01)

# Calculate GDD and KDD for each temperature
calculate_gdd_kdd <- function(temp) {
  gdd <- ifelse(temp >= 10 & temp < 28, temp - 10, 
                ifelse(temp >= 28, 18, 0))
  kdd <- ifelse(temp >= 28, temp - 28, 0)
  return(c(gdd, kdd))
}

gdd_kdd_values <- t(sapply(temperature_seq, calculate_gdd_kdd))
colnames(gdd_kdd_values) <- c("GDD", "KDD")

# Define the growing season length (e.g., 120 days)
growing_season_length <- 120

# Model results (insert the coefficients from your model here)
coefficients_gdd <- 0.015  # From Model 0
coefficients_kdd <- -0.363 # From Model 0

# Apply the model coefficients and adjust for daily effects
predicted_yield_log <- (gdd_kdd_values[, "GDD"] * coefficients_gdd +
                          gdd_kdd_values[, "KDD"] * coefficients_kdd) / growing_season_length

# Exponentiate the log values and convert to percentage change
predicted_yield_percent <- (exp(predicted_yield_log) - 1) * 100

# Create dataframe for plotting
plot_data <- data.frame(
  temperature = temperature_seq,
  predicted_yield = predicted_yield_percent
)

# Plot the response function
ggplot(plot_data, aes(x = temperature, y = predicted_yield)) +
  geom_line(color = "blue") +
  labs(title = "Response Curve of Yield to Temperature",
       x = "Temperature (°C)",
       y = "Predicted Yield Change (%)") +
  theme_minimal()

###################################################################
############response curve with AWC ###############################

# Load necessary libraries
library(ggplot2)
maize_gs <- fread(maize_gs_path)
# Define temperature range (e.g., from 0°C to 40°C)
temperature_seq <- seq(0, 40, by = 0.01)

# Calculate GDD and KDD for each temperature
calculate_gdd_kdd <- function(temp) {
  gdd <- ifelse(temp >= 10 & temp < 28, temp - 10, 
                ifelse(temp >= 28, 18, 0))
  kdd <- ifelse(temp >= 28, temp - 28, 0)
  return(c(gdd, kdd))
}

gdd_kdd_values <- t(sapply(temperature_seq, calculate_gdd_kdd))
colnames(gdd_kdd_values) <- c("GDD", "KDD")

# Define the growing season length (e.g., 120 days)
growing_season_length <- 120

# Model results from Model 0
coefficients_gdd <- 0.015              # edd_10
coefficients_kdd <- -0.363             # edd_28
coefficients_gdd_awc <- 0.0002         # edd_10:weighted_mean_AWC
coefficients_kdd_awc <- -0.001         # edd_28:weighted_mean_AWC

# Define AWC levels based on real range (10th percentile, median, 90th percentile)
awc_levels <- c(low = 80, medium = 140, high = 200)

# Calculate predicted yield changes for each AWC level
predicted_yields <- lapply(awc_levels, function(awc) {
  predicted_yield_log <- (gdd_kdd_values[, "GDD"] * coefficients_gdd +
                            gdd_kdd_values[, "KDD"] * coefficients_kdd +
                            gdd_kdd_values[, "GDD"] * awc * coefficients_gdd_awc +
                            gdd_kdd_values[, "KDD"] * awc * coefficients_kdd_awc) / growing_season_length
  predicted_yield_percent <- (exp(predicted_yield_log) - 1) * 100
  return(predicted_yield_percent)
})

# Create dataframe for plotting
plot_data <- data.frame(
  temperature = rep(temperature_seq, times = length(awc_levels)),
  predicted_yield = unlist(predicted_yields),
  awc_level = rep(names(awc_levels), each = length(temperature_seq))
)

# Plot the response functions
ggplot(plot_data, aes(x = temperature, y = predicted_yield, color = awc_level)) +
  geom_line() +
  labs(title = "Response Curve of Yield to Temperature with Different AWC Levels",
       x = "Temperature (°C)",
       y = "Predicted Yield Change (%)",
       color = "AWC Level") +
  theme_minimal()

###################################
#####NOW PERCENTILES###############
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(data.table)

# Load the dataset
maize_gs <- fread(maize_gs_path)

# Calculate the 25th and 75th percentiles, removing any NA values
percentiles <- quantile(maize_gs$weighted_mean_AWC, probs = c(0.05, 0.95), na.rm = TRUE)

# Define a function to categorize AWC based on the percentiles
categorize_awc <- function(awc) {
  if (is.na(awc)) {
    return(NA)
  } else if (awc <= percentiles[1]) {
    return("bottom 5%")
  } else if (awc <= percentiles[2]) {
    return("middle 90%")
  } else {
    return("upper 5%")
  }
}

# Apply the function to categorize the dataset
maize_gs <- maize_gs %>%
  mutate(awc_category = sapply(weighted_mean_AWC, categorize_awc))

# Check the results
print(head(maize_gs))

# Define temperature range (e.g., from 0°C to 40°C)
temperature_seq <- seq(0, 40, by = 0.01)

# Calculate GDD and KDD for each temperature
calculate_gdd_kdd <- function(temp) {
  gdd <- ifelse(temp >= 10 & temp < 28, temp - 10, 
                ifelse(temp >= 28, 18, 0))
  kdd <- ifelse(temp >= 28, temp - 28, 0)
  return(c(gdd, kdd))
}

gdd_kdd_values <- t(sapply(temperature_seq, calculate_gdd_kdd))
colnames(gdd_kdd_values) <- c("GDD", "KDD")

# Define the growing season length (e.g., 120 days)
growing_season_length <- 120

# Model results from Model 0
coefficients_gdd <- 0.015              # edd_10
coefficients_kdd <- -0.363             # edd_28
coefficients_gdd_awc <- 0.0002         # edd_10:weighted_mean_AWC
coefficients_kdd_awc <- -0.001         # edd_28:weighted_mean_AWC

# Define AWC levels based on calculated percentiles
awc_levels <- c("bottom 5%" = percentiles[1], 
                "middle 90%" = (percentiles[1] + percentiles[2]) / 2, 
                "upper 5%" = percentiles[2])

# Print AWC levels to ensure they are correct
print(awc_levels)

# Calculate predicted yield changes for each AWC level
predicted_yields <- lapply(awc_levels, function(awc) {
  predicted_yield_log <- (gdd_kdd_values[, "GDD"] * coefficients_gdd +
                            gdd_kdd_values[, "KDD"] * coefficients_kdd +
                            gdd_kdd_values[, "GDD"] * awc * coefficients_gdd_awc +
                            gdd_kdd_values[, "KDD"] * awc * coefficients_kdd_awc) / growing_season_length
  predicted_yield_percent <- (exp(predicted_yield_log) - 1) * 100
  return(predicted_yield_percent)
})

# Print a sample of the predicted yields to verify calculations
print(head(predicted_yields))

# Create dataframe for plotting
plot_data <- data.frame(
  temperature = rep(temperature_seq, times = length(awc_levels)),
  predicted_yield = unlist(predicted_yields),
  awc_level = rep(names(awc_levels), each = length(temperature_seq))
)

# Plot the response functions
ggplot(plot_data, aes(x = temperature, y = predicted_yield, color = awc_level)) +
  geom_line() +
  labs(title = "Response Curve of Yield to Temperature with Different AWC Levels",
       x = "Temperature (°C)",
       y = "Predicted Yield Change (%)",
       color = "AWC Level") +
  theme_minimal()



library(ggplot2)
library(data.table)

# Assuming 'maize_gs_path' is the path to your dataset
maize_gs <- fread(maize_gs_path)

# Define temperature range (e.g., from 0°C to 40°C)
temperature_seq <- seq(0, 40, by = 0.01)

# Calculate GDD and KDD for each temperature
calculate_gdd_kdd <- function(temp) {
  gdd <- ifelse(temp >= 10 & temp < 28, temp - 10, 
                ifelse(temp >= 28, 18, 0))
  kdd <- ifelse(temp >= 28, temp - 28, 0)
  return(c(gdd, kdd))
}

gdd_kdd_values <- t(sapply(temperature_seq, calculate_gdd_kdd))
colnames(gdd_kdd_values) <- c("GDD", "KDD")

# Define the growing season length (e.g., 120 days)
growing_season_length <- 120

# Model results from Model 0
coefficients_gdd <- 0.015              # edd_10
coefficients_kdd <- -0.363             # edd_28
coefficients_gdd_awc <- 0.0002         # edd_10:weighted_mean_AWC
coefficients_kdd_awc <- -0.001         # edd_28:weighted_mean_AWC

# Calculate percentiles for AWC values
awc_quantiles <- quantile(maize_gs$weighted_mean_AWC, probs = c(0.05, 0.50, 0.95), na.rm = TRUE)

# Define AWC levels based on the quantiles
awc_levels <- c(low = awc_quantiles[1], medium = awc_quantiles[2], high = awc_quantiles[3])

# Calculate predicted yield changes for each AWC level
predicted_yields <- lapply(awc_levels, function(awc) {
  predicted_yield_log <- (gdd_kdd_values[, "GDD"] * coefficients_gdd +
                            gdd_kdd_values[, "KDD"] * coefficients_kdd +
                            gdd_kdd_values[, "GDD"] * awc * coefficients_gdd_awc +
                            gdd_kdd_values[, "KDD"] * awc * coefficients_kdd_awc) / growing_season_length
  predicted_yield_percent <- (exp(predicted_yield_log) - 1) * 100
  return(predicted_yield_percent)
})

# Create dataframe for plotting
plot_data <- data.frame(
  temperature = rep(temperature_seq, times = length(awc_levels)),
  predicted_yield = unlist(predicted_yields),
  awc_level = rep(names(awc_levels), each = length(temperature_seq))
)

# Plot the response functions
ggplot(plot_data, aes(x = temperature, y = predicted_yield, color = awc_level)) +
  geom_line() +
  labs(title = "Response Curve of Yield to Temperature with Different AWC Levels",
       x = "Temperature (°C)",
       y = "Predicted Yield Change (%)",
       color = "AWC Level") +
  theme_minimal()





###################################################
######### MAPS.none of these look all that great rn       
##################################################
# Create the plot without border and axis numbers
plot(AWC_aggregated, main="Available Water Capacity, 150m Resolution",
     xlab="", ylab="", xaxt='n', yaxt='n', bty='n', axes=FALSE)

# Add a simple legend that displays 'mm' as the unit
legend("bottomright",       # Position the legend in the top right corner
       legend="mm",      # Display 'mm' as the unit
       bty="n",          # No border around the legend
       col = "black",
       x.intersp=0.2,    # Adjust horizontal spacing to tighten legend
       y.intersp=0.5,    # Adjust vertical spacing to tighten legend
       cex=0.8)          # Adjust text size, if needed


#Reclassify the raster values based on the provided rules
# Define the custom color palette with grey for zeros and viridis for the rest
# Define the custom color palette with white for zeros, viridis for the rest, and grey for 255
library(terra)
library(viridis)
# Create the reclassification matrix
reclass_matrix <- matrix(c(0, 0, -1,    # 0 to -1 (No Data/Background)
                           1, 1, 1,     # 1 to 1
                           2, 2, 2,     # 2 to 2
                           3, 3, 3,     # 3 to 3
                           4, 4, 4,     # 4 to 4
                           5, 5, 5,     # 5 to 5
                           6, 6, 6,     # 6 to 6
                           7, 7, 7,     # 7 to 7
                           8, 8, 8,     # 8 to 8
                           9, 9, 9,     # 9 to 9
                           10, 10, 10,  # 10 to 10
                           11, 11, 11,  # 11 to 11
                           12, 12, 12,  # 12 to 12
                           13, 13, 13,  # 13 to 13
                           14, 14, 14,  # 14 to 14
                           15, 15, 15,  # 15 to 15
                           16, 16, 16,  # 16 to 16
                           255, 255, 0  # 255 to 0 (Planted 0 times in 16 years)
), ncol=3, byrow=TRUE)

# Reclassify the raster
r_reclass <- classify(MaizeCover_aggregated, reclass_matrix)

# Define the custom color palette with white for -1, reversed viridis for the rest, and grey for 0
custom_colors <- c("white", viridis(16), "grey")

# Set up layout: 1 for the map, 2 for the legend
layout(matrix(c(1, 2), ncol=2), widths=c(6, 1))  # Increase the width for the map

# Plot the raster in the first layout section, suppressing the automatic legend
par(mar=c(5, 2, 4, 1) + 0.1, xpd=NA)  # Adjust margins to make the map larger
plot(r_reclass, main="Maize Cover, 150m Resolution",
     col = custom_colors,
     xlab="", ylab="", xaxt='n', yaxt='n', bty='n', axes=FALSE,
     colNA = "white", legend = FALSE)  # Ensuring NA values are plotted as white

# Plot the color scale legend in the second layout section using image.plot
par(mar=c(5, 2, 4, 1) + 0.1)  # Adjust margins for the legend
image.plot(legend.only=TRUE, 
           zlim=c(0, 16), 
           col=rev(viridis(16)), 
           legend.mar=0,  # Adjust margin for the legend labels
           legend.width=1.5)  # Adjust width for better readability

# Ensure the object is a data.frame or tibble
average_tmean_per_geo <- as_tibble(average_tmean_per_geo)

# Select only the required columns
average_tmean_per_geo <- average_tmean_per_geo %>%
  dplyr::select(GEOID, avg_PRISM_tmean)

# Retrieve US counties shapefile and convert to sf object
options(tigris_use_cache = TRUE)
counties_sf <- counties(cb = TRUE, resolution = "20m")

# Ensure GEOID in counties_sf is character
counties_sf$GEOID <- as.character(counties_sf$GEOID)

# Merge the average PRISM_tmean with the geographical data
map_data <- left_join(counties_sf, average_tmean_per_geo, by = "GEOID")

average_tmean_per_geo_sf <- st_transform(average_tmean_per_geo_sf, 4326)

average_tmean_per_geo_sf_simplified <- st_simplify(average_tmean_per_geo_sf, dTolerance = 0.01)
ggplot(data = average_tmean_per_geo_sf_simplified) +
  geom_sf(aes(fill = avg_PRISM_tmean)) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Temperature") +
  theme_minimal() +
  labs(title = "Average Mean Temperature per GEOID",
       subtitle = "Data Source: PRISM",
       caption = "Plot created using ggplot2 and sf in R")


ggplot(data = average_tmean_per_geo_sf) +
  geom_sf(aes(fill = avg_PRISM_tmean)) +
  scale_fill_viridis_c(option = "plasma", name = "Mean Temperature") +
  theme_minimal() +
  labs(title = "Average Mean Temperature per GEOID",
       subtitle = "Data Source: PRISM",
       caption = "Plot created using ggplot2 and sf in R")

###maps are not working properly###
#might need to avergae values

# Ensure necessary libraries are loaded
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)

# Convert GEOID in maize_gs to character to match county data
maize_gs <- maize_gs %>%
  mutate(GEOID = as.character(GEOID))

# Merge maize_gs with county shapefile data to expand the county dataset
expanded_county <- maize_gs %>%
  left_join(county, by = "GEOID")

# Check the structure of the expanded dataset
str(expanded_county)

# Ensure the geometry column is correctly assigned
expanded_county <- st_as_sf(expanded_county)

# Plot for AWC
# Filter out rows with NA values in weighted_mean_AWC
filtered_data <- expanded_county %>%
  filter(!is.na(weighted_mean_AWC))


ggplot(data = filtered_data) +
  geom_sf(data = county, fill = "white", color = "grey") +
  geom_sf(aes(fill = weighted_mean_AWC)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "AWC by Year", fill = "AWC")

# Plot for Yield
ggplot(data = expanded_county) +
  geom_sf(data = county, fill = "white", color = "grey") +
  geom_sf(aes(fill = yield)) +
  facet_wrap(~ Year) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Yield by Year", fill = "Yield")

# Plot for Temperature
ggplot(data = expanded_county) +
  geom_sf(data = county, fill = "white", color = "grey") +
  geom_sf(aes(fill = tmean)) +
  facet_wrap(~ Year) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Temperature by Year", fill = "Temperature")

# Plot for Rainfall
ggplot(data = expanded_county) +
  geom_sf(data = county, fill = "white", color = "grey") +
  geom_sf(aes(fill = ppt)) +
  facet_wrap(~ Year) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Rainfall by Year", fill = "Rainfall")
