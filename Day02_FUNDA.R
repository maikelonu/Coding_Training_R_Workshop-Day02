# //////////////////////////////////////////////////////////////////////////////////
# INSTITUTO TECNOLOGICO DE COSTA RICA
# Escuela de Ingenieria en Construccion
# Day 02. Graphic Analysis with ggplot2

# M.Sc. Eng. Maikel Mendez M
# Water Resources + GIS + DataScience
# Instituto Tecnologico de Costa Rica
# https://www.tec.ac.cr
# https://orcid.org/0000-0003-1919-141X
# https://www.scopus.com/authid/detail.uri?authorId=51665581300
# https://scholar.google.com/citations?user=JnmSVFYAAAAJ&hl=en
# https://www.youtube.com/c/maikelmendez
# https://twitter.com/MaikelMendezM
# https://github.com/maikelonu
# //////////////////////////////////////////////////////////////////////////////////

# Workspace is cleared
rm(list = ls())

# Working directory is selected
setwd("C:/DATOS/FUNDA_R/R_Workshop/Day02")

# CRAN libraries are installed
# install.packages("pastecs")

# CRAN libraries are requested
require(pastecs)
require(ggplot2)
require(DescTools)
require(ggthemes)

# /////////////////////////////////////////////////
# BLOCK: CREATING AND ORGANIZING INPUT DATA.FRAMES
# /////////////////////////////////////////////////

# Input files are loaded and data.frames are created

# HOBO-U20 sensor temperature and water-level observations
df.hobo <- read.table("hobo.txt",header=T,sep="\t",quote="")

# Compressive strength of the same element at different dates for different labs
df.lab <- read.table("lab_comp.txt",header=T,sep="\t",quote="")

# Compressive strength for various elements at different ages
df.element <- read.table("element_comp.txt",header=T,sep="\t",quote="")

# Performance of a 2009-Nissan Tiida 1.6L
df.tiida <- read.table("tiida.txt",header=T,sep="\t",quote="")

#////////////////////////
# BLOCK: HOBO-Sensor
#////////////////////////

ggplot() +
  geom_boxplot(aes(y = water_temp_celsius,x = sensor_number,colour = sensor_number),data=df.hobo,size = 0.6,alpha = 0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
  geom_point(aes(x = sensor_number,y = water_temp_celsius,colour = sensor_number),data=df.hobo,size = 2.2,alpha = 0.9,position = position_jitter(width = 0.1))

ggplot() +
  geom_boxplot(aes(y = water_level_m,x = sensor_number,colour = sensor_number),data=df.hobo,size = 0.6,alpha = 0.6) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
  geom_point(aes(x = sensor_number,y = water_level_m,colour = sensor_number),data=df.hobo,size = 2.2,alpha = 0.9,position = position_jitter(width = 0.1))

ggplot() +
 geom_point(aes(x = water_level_m,y = water_temp_celsius,colour = sensor_number),data=df.hobo,position = position_jitter(width = 0.03)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0))

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_temp_celsius,shape = sensor_number,colour = sensor_number),data=df.hobo,size = 3.50,alpha = 0.90,position = position_jitter(width = 0.1)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_temp_celsius,shape = sensor_number,colour = sensor_number),data=df.hobo,size = 2.75,alpha = 0.9145,position = position_jitter(width = 0.1)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_smooth(aes(x = timelapse_min,y = water_temp_celsius),data=df.hobo,size = 0.75,alpha = 0.3162,method = lm,fullrange = TRUE,na.rm = TRUE)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_temp_celsius,shape = sensor_number,colour = sensor_number),data=df.hobo,size = 2.75,alpha = 0.9145,position = position_jitter(width = 0.1)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_smooth(aes(x = timelapse_min,y = water_temp_celsius,colour = sensor_number),data=df.hobo,size = 0.75,alpha = 0.3162,method = lm,fullrange = TRUE,na.rm = TRUE)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_level_m,shape = sensor_number,colour = sensor_number),data=df.hobo,size = 2.75,alpha = 0.9145,position = position_jitter(width = 0.1)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_smooth(aes(x = timelapse_min,y = water_level_m),data=df.hobo,method = lm)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_level_m,shape = sensor_number,colour = sensor_number),data=df.hobo,size = 2.75,alpha = 0.9,position = position_jitter(width = 0.1)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_smooth(aes(x = timelapse_min,y = water_level_m,colour = sensor_number),data=df.hobo,method = lm)

ggplot() +
 stat_identity(aes(x = sensor_number,y = water_temp_celsius,fill = sensor_number),data=df.hobo,size = 0.75,alpha = 0.906,geom = 'bar',position = position_identity()) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 stat_identity(aes(x = sensor_number,y = water_level_m,fill = sensor_number),data=df.hobo,size = 0.75,alpha = 0.906,geom = 'bar',position = position_identity()) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 stat_identity(aes(x = sensor_number,y = water_level_m,fill = sensor_number),data=df.hobo,size = 0.75,alpha = 0.906,geom = 'bar',position = position_identity()) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_boxplot(aes(y = water_level_m,x = sensor_number),data=df.hobo,size = 0.4,outlier.colour = '#0000ff')

ggplot() +
 geom_density(aes(x = water_level_m,y = ..density..,colour = sensor_number),data=df.hobo)

ggplot() +
 geom_density(aes(x = water_temp_celsius,y = ..density..,colour = sensor_number),data=df.hobo)

ggplot() +
 geom_density(aes(x = water_level_m,y = ..density..,colour = sensor_number),data=df.hobo) +
 facet_wrap(facets = ~sensor_number)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_temp_celsius,colour = sensor_number),data=df.hobo) +
 geom_smooth(aes(x = timelapse_min,y = water_temp_celsius,colour = sensor_number),data=df.hobo,method = lm) +
 facet_wrap(facets = ~sensor_number)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_level_m,colour = sensor_number),data=df.hobo) +
 geom_smooth(aes(x = timelapse_min,y = water_level_m,colour = sensor_number),data=df.hobo,method = lm) +
 facet_wrap(facets = ~sensor_number)

ggplot() +
 geom_point(aes(x = timelapse_min,y = water_temp_celsius,colour = sensor_number),data=df.hobo) +
 geom_smooth(aes(x = timelapse_min,y = water_temp_celsius,colour = sensor_number),data=df.hobo,method = lm) +
 facet_wrap(facets = ~sensor_number) +
 geom_area(aes(x = timelapse_min,y = water_temp_celsius,fill = sensor_number),data=df.hobo,alpha = 0.20)

ggplot() +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_boxplot(aes(y = water_temp_celsius,x = sensor_number),data=df.hobo) +
 geom_violin(aes(x = sensor_number,y = water_temp_celsius,fill = sensor_number),data=df.hobo,alpha = 0.20) +
 geom_point(aes(x = sensor_number,y = water_temp_celsius),data=df.hobo,position = position_jitter(width = 0.05))

ggplot() +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_violin(aes(x = sensor_number,y = water_level_m,fill = sensor_number),data=df.hobo,alpha = 0.1966) +
 geom_point(aes(x = sensor_number,y = water_level_m),data=df.hobo,position = position_jitter(width = 0.05)) +
 facet_wrap(facets = ~sensor_number)

#////////////////////////
# BLOCK: LAB
#////////////////////////

ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = company,colour = company),data=df.lab) +
 geom_point(aes(x = company,y = compressive_strength_Mpa),data=df.lab,position = position_jitter(width = 0.075)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))


ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = date,colour = company),data=df.lab,outlier.colour = 'green') +
 geom_point(aes(x = date,y = compressive_strength_Mpa),data=df.lab,position = position_jitter(width = 0.075)) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_density(aes(x = compressive_strength_Mpa),data=df.lab)

ggplot() +
 geom_density(aes(x = compressive_strength_Mpa,colour = company),data=df.lab)

ggplot() +
 geom_density(aes(x = compressive_strength_Mpa,colour = date,linetype = company),data=df.lab,size = 0.95)

# summery function is requested
summary(df.lab)

# stat.desc {pastecs} function is requested
df.lab.desc <- as.data.frame(stat.desc(df.lab$compressive_strength_Mpa))

# data.frame is rounded to 3 significant digits
df.lab.desc <- round(df.lab.desc,3)

# subsetting data.frame
df.lab01.subset <- subset(df.lab, company == "LAB1")
df.lab02.subset <- subset(df.lab, company == "LAB2")

# stat.desc {pastecs} function is requested
df.lab.01.desc <- round((as.data.frame(stat.desc(df.lab01.subset$compressive_strength_Mpa))),3)
df.lab.02.desc <- round((as.data.frame(stat.desc(df.lab02.subset$compressive_strength_Mpa))),3)

# cbind function is called
df.lab.total.desc <- cbind(df.lab.desc,
                           df.lab.01.desc,
                           df.lab.02.desc)

# colnames function is requested
colnames(df.lab.total.desc) <- c("lab_total_MPa", "lab_01_MPa", "lab_02_MPa")

#////////////////////////
# BLOCK: ELEMENT
#////////////////////////

# data.frame variable is transformed to character class
df.element$time_lapse_days <- as.character(df.element$time_lapse_days)

ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = element_type,colour = element_type),
              data=df.element) +
 geom_point(aes(x = element_type,y = compressive_strength_Mpa,colour = time_lapse_days,
                group = time_lapse_days),
                data=df.element,position = position_jitter(width = 0.1), size = 3.5) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0))


ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = time_lapse_days,colour = element_type),
              data=df.element) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))


ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = time_lapse_days,colour = element_type),
              data=df.element) +
 geom_point(aes(x = time_lapse_days,y = compressive_strength_Mpa,colour = element_type),
            data=df.element,position = position_identity(), size = 3.5)+
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) 


ggplot() +
 geom_boxplot(aes(y = compressive_strength_Mpa,x = time_lapse_days,colour = element_type),
              data=df.element) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 facet_wrap(facets = ~ element_type) +
 geom_point(aes(x = time_lapse_days,y = compressive_strength_Mpa,colour = time_lapse_days),
            data=df.element,position = position_jitter(width = 0.1), size = 3.5)

# subsetting data.frame
 df.ele01.day7 <- subset(df.element, element_type == "element01" & time_lapse_days == "7")
df.ele01.day28 <- subset(df.element, element_type == "element01" & time_lapse_days == "28")
 df.ele02.day7 <- subset(df.element, element_type == "element02" & time_lapse_days == "7")
df.ele02.day28 <- subset(df.element, element_type == "element02" & time_lapse_days == "28")

# stat.desc {pastecs} function is requested
 df.ele01.day7.desc <- as.data.frame(stat.desc(df.ele01.day7$compressive_strength_Mpa))
df.ele01.day28.desc <- as.data.frame(stat.desc(df.ele01.day28$compressive_strength_Mpa))
 df.ele02.day7.desc <- as.data.frame(stat.desc(df.ele02.day7$compressive_strength_Mpa))
df.ele02.day28.desc <- as.data.frame(stat.desc(df.ele02.day28$compressive_strength_Mpa))

# cbind function is called
df.desc.ele.total <- cbind(df.ele01.day7.desc,
                           df.ele01.day28.desc,
                           df.ele02.day7.desc,
                           df.ele02.day28.desc)

# data.frame is rounded to 3 significant digits
df.desc.ele.total <- round(df.desc.ele.total, 3)

# colnames function is requested
colnames(df.desc.ele.total) <- c("ele01_day7_MPa",
                                 "ele01_day28_MPa",
                                 "ele02_day7_MPa",
                                 "ele02_day28_MPa")

#////////////////////////
# BLOCK: NISAN TIIDA
#////////////////////////

# as.Date function is requested
df.tiida$date <- as.Date(df.tiida$date, "%m/%d/%Y")

# stat.desc {pastecs} function is requested
df.tiida.desc <- as.data.frame(stat.desc(df.tiida[, 1:8]))

# data.frame is rounded to 3 significant digits
df.tiida.desc <- round(df.tiida.desc, 3)

ggplot() +
 geom_point(aes(x = vehicle,y = performance_km_Gal),data=df.tiida,colour = '#3300ff',
            position = position_jitter(width = 0.05)) +
 geom_boxplot(aes(y = performance_km_Gal,x = vehicle),data=df.tiida,fill = '#cccc00',
              size = 0.75,alpha = 0.15,outlier.colour = '#ff0000',outlier.size = 3) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_point(aes(x = vehicle,y = dist_km),data=df.tiida,colour = '#3300ff',
            position = position_jitter(width = 0.05)) +
 geom_boxplot(aes(y = dist_km,x = vehicle),data=df.tiida,fill = '#cccc00',
              size = 0.75,alpha = 0.1538,outlier.colour = '#ff0000',outlier.size = 3) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_point(aes(x = vol_GAL,y = dist_km),data=df.tiida,alpha = 0.50) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_smooth(aes(x = vol_GAL,y = dist_km,fill = vehicle),data=df.tiida,
             method = lm,fullrange = TRUE)

ggplot() +
 geom_point(aes(x = date,y = performance_km_Gal),data=df.tiida,alpha = 0.80) +
 geom_line(aes(x = date,y = performance_km_Gal,colour = vehicle),data=df.tiida)

ggplot() +
 geom_point(aes(x = date,y = performance_km_Gal),data=df.tiida,alpha = 0.812) +
 geom_line(aes(x = date,y = performance_km_Gal,colour = vehicle),data=df.tiida) +
 geom_smooth(aes(x = date,y = performance_km_Gal),data=df.tiida,method = lm)

ggplot() +
 geom_point(aes(x = date,y = cost_per_L),data=df.tiida,alpha = 0.4188) +
 geom_line(aes(x = date,y = cost_per_L,colour = cost_per_L),
           data=df.tiida,size = 0.75) +
 scale_y_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 scale_colour_gradient(guide = guide_legend(),breaks = scales::pretty_breaks(min.n = 5.0),
                       low = '#0000ff',high = '#ff0033') +
 scale_x_date(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_density(aes(x = performance_km_Gal),data=df.tiida,colour = '#000099',
              weight = 2.0,size = 0.75) +
 geom_histogram(aes(y = ..density..,x = performance_km_Gal,fill = vehicle),
                data=df.tiida,alpha = 0.40) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0))

ggplot() +
 geom_density(aes(x = performance_km_Gal,y = ..count..),data=df.tiida,
              colour = '#000099',weight = 2.0,size = 0.75) +
 scale_x_continuous(breaks = scales::pretty_breaks(n = 20.0,min.n = 20.0)) +
 geom_dotplot(aes(x = performance_km_Gal,fill = vehicle),
              data=df.tiida,alpha = 0.641)

#////////////////////////
# BLOCK: ggplot - MANUAL
#////////////////////////

# mpg {ggplot2} data set is loaded
data = mpg

# A data.frame is created
df.mpg <- data

# Desc {DescTools} function is requested
Desc(df.mpg, plotit = T)

# ggplot(aes(x = X, y = Y),data = DATA)
f <- ggplot(aes(x = cty, y = hwy), data = df.mpg) +
       geom_point(aes(color = cyl)) +
       geom_smooth(method ="lm") +
       scale_color_gradient() +
       theme_gray()

ggplot(aes(x = cty, y = hwy), data = df.mpg) +
  geom_point(aes(color = cyl)) +
  geom_smooth(method ="lm") +
  scale_color_gradient() +
  ggtitle("Prueba 01") +
  xlab("Redimiento en ciudad (MPG)") +
  ylab("Redimiento en carretera (MPG)") +
  theme_gray()
  








