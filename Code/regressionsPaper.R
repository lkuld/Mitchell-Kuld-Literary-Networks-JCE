# creates regression tables for the paper and Figure 7 (event study)

rm(list=ls())

# load libraries

library(ggplot2)
library(dplyr)
library(stringr)
library(fixest)
library(texreg)

load("Data/regressionData.RData")

data$not.alone<- data$authors_number != 0
data$kindler_author <- data$name %in% data$name[data$kindler_pub>0]

# standardise variables and set NAs to 0
for (i in names(data)[str_detect(names(data), 'wordcount')]) {
  data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) / sd(data[[i]], na.rm = TRUE)
  
}
for (i in names(data)[str_detect(names(data), 'wordcount|output.lagged')]) {
  data[[i]][is.na(data[[i]])] <- 0
  
}

data$circle <- data$buddies.number > 3 

#  main regressions--------------------------

## regs Table 1 ---

cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('not.alone+location.number+location.wordcount', 'buddies.number+buddies.wordcount + not.alone',
         'location.number+location.wordcount+buddies.number+buddies.wordcount + not.alone' ,
         'location.number+location.wordcount+buddies.number+buddies.wordcount + london + not.alone',
         'london*location.number + buddies.number*london  + not.alone')

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feols(as.formula(paste('output ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond,], cluster = 'name')
}

texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))

## regs Table 2 ---
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('not.alone', 'location.number+location.wordcount+ not.alone', 'buddies.number+buddies.wordcount + not.alone',
         'location.number+location.wordcount+buddies.number+buddies.wordcount + not.alone' ,
         'location.number+location.wordcount+buddies.number+buddies.wordcount + london + not.alone',
         'buddies.number',"buddies.number+buddies.wordcount")

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feglm(as.formula(paste('kindler_pub ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond &data$london%in%(0:1) ,], cluster = 'name', family = 'poisson')
}

texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))


## regs Table 3 ---
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('circle','circle+not.alone','circle+not.alone+london')

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feglm(as.formula(paste('output ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond &data$london%in%(0:1) ,], cluster = 'name',
                        family = 'poisson')
}

for (i in seq_along(rhs)) {
  regList[[i+3]] <- feglm(as.formula(paste('kindler_pub ~', rhs[i],'|age+year+name') ),
                          , data =  data[cond &data$london%in%(0:1) ,], cluster = 'name',
                          family = 'poisson')
}

texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))


## Appendix -----------------
## regs Table 1 ---

cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('location.number', 'location.wordcount', 'location.output.lagged',
         'location.number+location.wordcount+location.output.lagged')

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feols(as.formula(paste('output ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond &data$london%in%(0:1),], cluster = 'name')
}

for (i in seq_along(rhs)) {
  regList[[i+4]] <- feols(as.formula(paste('output ~', rhs[i],'+london|age+year+name') ),
                          , data =  data[cond &data$london%in%(0:1),], cluster = 'name')
}
rhs2 <- c('log1p(location.number)','log1p(location.number)+london')
for (i in seq_along(rhs2)) {
  regList[[i+8]] <- feols(as.formula(paste('output ~', rhs2[i],'|age+year+name') ),
                          , data =  data[cond &data$london%in%(0:1),], cluster = 'name')
}

library(texreg)
texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))

## regs Table 2 ---

cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('buddies.number', 'buddies.wordcount', 'buddies.output.lagged',
         'buddies.number+buddies.wordcount+buddies.output.lagged')

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feols(as.formula(paste('output ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond &data$london%in%(0:1),], cluster = 'name')
}

texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))

## regs Table 3 ---
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs_2 <- c('not.alone+location.gender.number+ london','location.gender.number+location.gender.wordcount+location.gender.output.lagged + london +
          not.alone',
           'not.alone+ location.age.number+ london','location.age.number+location.age.wordcount+location.age.output.lagged + london +
          not.alone')
regList <- list()

for (i in seq_along(rhs_2)) {
  regList[[i]] <- feols(as.formula(paste('output ~', rhs_2[i],'|age+year+name') ),
                        , data =  data[cond &data$london%in%(0:1),], cluster = 'name')
}

library(texreg)
texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))


## lagged peers ------------------------------

library(data.table)
setDT(data)
data[, year := as.integer(year)]
setorder(data, name, year)

vars <- c("location.number", "buddies.number",
          "location.wordcount", "buddies.wordcount",
          "not.alone", "london")


data[, (paste0(vars, "_lag_3yr_avg")) := lapply(.SD, function(x) {
  shift(frollmean(as.numeric(x), n = 3, align = "right", na.rm = TRUE), 1L)
}), by = name, .SDcols = vars]

data <- as.data.frame(data)

cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

rhs <- c('not.alone_lag_3yr_avg', 'location.number_lag_3yr_avg+location.wordcount_lag_3yr_avg+ not.alone_lag_3yr_avg', 
         'buddies.number_lag_3yr_avg+buddies.wordcount_lag_3yr_avg + not.alone_lag_3yr_avg',
         'location.number_lag_3yr_avg+location.wordcount_lag_3yr_avg+buddies.number_lag_3yr_avg+buddies.wordcount_lag_3yr_avg + not.alone_lag_3yr_avg' ,
         'london+location.number_lag_3yr_avg+location.wordcount_lag_3yr_avg+buddies.number_lag_3yr_avg+buddies.wordcount_lag_3yr_avg + london_lag_3yr_avg + not.alone_lag_3yr_avg')

regList <- list()

for (i in seq_along(rhs)) {
  regList[[i]] <- feols(as.formula(paste('kindler_pub ~', rhs[i],'|age+year+name') ),
                        , data =  data[cond,], cluster = 'name')
}

texreg(regList, digits=3, stars = c(0.001, 0.01, 0.05, 0.1))


#  event study ------------------------------

library(data.table)

cc_var <- "circle"

setDT(data)
data[, year := as.integer(year)]
setorder(data, name, year)

# First adoption (first year the author is in a creative circle)
data[, first_cc_year := {
  idx <- which(get(cc_var) == 1L)
  if (length(idx)) year[idx[1]] else NA_integer_
}, by = name]

# Event-study time: years relative to first entry; NA for never-treated authors
data[, es_time := ifelse(is.na(first_cc_year), NA_integer_, year - first_cc_year)]

# (Optional) Bin extreme leads/lags into a tidy window, e.g. [-10, +10]
lower <- -5
upper <-  20
data[, es_time_binned := fifelse(is.na(es_time), NA_integer_,
                                 pmax(pmin(es_time, upper), lower))]


library(did2s)

data$circle_past <- data$es_time >= 0 & is.finite(data$es_time)
data$circle_past[is.na(data$es_time)] <- FALSE

data$circle_gone <- data$circle_past & !data$circle
data$es_time_binned[is.na(data$es_time)] <- Inf

cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

es <- did2s(data[cond&is.finite(data$es_time_binned)&data$kindler_author,],
            yname = "kindler_pub", first_stage = ~ 0 |  year+name,
            second_stage = ~ poly(age,3)+i(es_time_binned, ref = c(-1)), treatment = "circle",
            cluster_var = "name"
)
summary(es)
fixest::iplot(es, ci_level=.90,ylab = "Effect on Kindler publications",
              xlab = "Relative time to first co-location with creative circle", 
              col = "grey20", ref.line = -0.5, main='')
