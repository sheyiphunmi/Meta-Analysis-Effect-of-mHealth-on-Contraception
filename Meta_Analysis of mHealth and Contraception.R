library("tidyverse")
library("dmetar")
library("metafor")
library("readxl")
library("meta")
library("ggplot2")
library("esc")
#library("dmetar")

setwd("C:/Users/sleem/Desktop/UC_FALL_23/Meta Analysis/Project/")

util_data <- read_excel("Data_Analysis.xlsx", sheet="Utilization")

glimpse(util_data)

util_data$Year <- as.numeric(util_data$Year)
util_data$Location <- as.factor(util_data$Location)
util_data$Economic_Status <- as.factor(util_data$Economic_Status)
util_data$Study_Group <- as.factor(util_data$Study_Group)


## Overall Meta-Analaysis.
m.bin <- metabin(
            event.e = event_trt,
            n.e = n_trt,
            event.c = event_ctrl,
            n.c = n_ctrl,
            studlab = Author,
            data = util_data,
            sm = "OR",
            method = "MH",
            MH.exact = TRUE,
            fixed = FALSE,
            random = TRUE,
            method.tau = "REML",  # or "ML", "EB", "PM"
            hakn = TRUE
)

m.bin <- update.meta(m.bin, prediction = TRUE)
summary(m.bin)


# Forest Plot
png(file = "forestplot_overall.png", width = 2800, height = 2400, res = 300)
forest.meta(m.bin, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author"),
            layout = "RevMan5")
dev.off()



## Subgroup Analysis: Economic Status

m.subgroup_economic_status <- metabin(
  event.e = event_trt,
  n.e = n_trt,
  event.c = event_ctrl,
  n.c = n_ctrl,
  studlab = Author,
  data = util_data,
  method = "MH",
  MH.exact = TRUE,
  sm = "OR",
  fixed = FALSE,
  random = TRUE,
  prediction = TRUE,
  byvar = Economic_Status,  # Subgroup variable
  method.tau = "REML",
  hakn = TRUE
)

# Summary of subgroup analysis
summary(m.subgroup_economic_status)

#Forest Plot
png(file = "forestplot_economic_stat.png", width = 2800, height = 2400, res = 300)
forest.meta(m.subgroup_economic_status, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author"),
            layout = "RevMan5")
dev.off()


## Subgroup Analysis: Study Group

m.subgroup_study_group <- metabin(
  event.e = event_trt,
  n.e = n_trt,
  event.c = event_ctrl,
  n.c = n_ctrl,
  studlab = Author,
  data = util_data,
  method = "MH",
  MH.exact = TRUE,
  sm = "OR",
  fixed = FALSE,
  random = TRUE,
  prediction = TRUE,
  byvar = Study_Group,  # Subgroup variable
  method.tau = "REML",
  hakn = TRUE
)

# Summary of subgroup analysis
summary(m.subgroup_study_group)

#Forest Plot
png(file = "forestplot_study_group.png", width = 2800, height = 2400, res = 300)
forest.meta(m.subgroup_study_group, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author"),
            layout = "RevMan5")
dev.off()



### meta regression.

# Year
m.reg.year <- metareg(m.bin, ~util_data$Year)
m.reg.year


## Economic_Status
m.reg.eco <- metareg(m.bin,  ~util_data$Economic_Status)
m.reg.eco


## Eco + Year
m.reg.ecoyear <- metareg(m.bin, ~util_data$Economic_Status + util_data$Year )
m.reg.ecoyear