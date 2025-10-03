############################################################
## Multilevel Workshop 
############################################################

## 0) packages + theme ----
c("tidyverse","lubridate","lme4","glmmTMB","DHARMa",
  "psych","sjPlot","ggeffects","performance","janitor","broom.mixed", "lattice") |>
  setdiff(rownames(installed.packages())) |>
  (\(pk) if (length(pk)) install.packages(pk, quiet = TRUE) )()

library(tidyverse); library(lubridate); library(lme4); library(glmmTMB)
library(DHARMa); library(psych); library(sjPlot); library(ggeffects)
library(performance); library(janitor); library(broom.mixed); library(lattice)

theme_set(theme_minimal(base_size = 12))
ok <- \(x) cli::cli_alert_success(x) |> invisible()

## ========================================================
## PART 1 — Built-in datasets (null → predictors → LR test)
## ========================================================

dat <- lme4::sleepstudy

psych::describe(sleepstudy)

### A) LMM: sleepstudy (Reaction ~ Days, random by Subject)

    m0 <- lmer(Reaction ~ 1 + (1|Subject), data = dat, REML = FALSE) ## RI null model
    m1 <- lmer(Reaction ~ Days + (1|Subject), data = dat, REML = FALSE) ## RI + predictor 
    m2 <- lmer(Reaction ~ Days + (Days|Subject), data = dat, REML = FALSE) ## RI + RS for days | subject 
    m2u<- lmer(Reaction ~ Days + (Days||Subject), data = dat, REML = FALSE) ## 
    
    ##summary 
    summary(m0)
    summary(m1)
    summary(m2)
    summary(m2u)
    
    ## tab models (sjplot)
    tab_model(m0) ## can add ,show.BIC = "TRUE"
    tab_model(m1)
    tab_model(m2)
    tab_model(m2u)
    
    
    ## for each model we can check fit and residuals
    
    
    ## can also use the performance package
    # Check model assumptions visually
    performance::check_model(m0)
    
    # Calculate R-squared
    performance::r2(m0)
    
    # Calculate Intraclass Correlation Coefficient
    performance::icc(m0)
    
    # Check for multicollinearity
    performance::check_collinearity(m0)
    
    ## can Simulate residuals using DHARMa's simulateResiduals function
    

simulationOutput <- simulateResiduals(fittedModel = m0, plot = FALSE) ## note replace m0 with other models and compare below

# 3. Plot the DHARMa residuals
# This generates the standard DHARMa residual plots for visual inspection.
plot(simulationOutput)

# 4. Run specific DHARMa tests for model misspecification
# You can perform various tests to check for issues like overdispersion, zero-inflation, and uniformity.
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
testUniformity(simulationOutput)

# You can also check for outliers
testOutliers(simulationOutput)
   
   ## plot models 
   
  
  ## model comparison w/ anova
   ## m0 vs m1 
   anova(m0, m1) ## adds value 
   ## m1 vs m2 
   anova(m1, m2) ## adds value 
   
   ## comparing correlated and uncorrelated random slopes 
   anova(m2, m2u)
   
    
### B) Counts: Salamanders (Poisson vs NB vs ZI-NB)
   
   dat_2 <- glmmTMB::Salamanders  
   
   ## describe data and plot 
   
   psych::describe(dat_2)
   
   ## some GGPlots 
   
   dat_2 %>% 
     ggplot(aes(x=count)) +
     geom_histogram()
   
   dat_2 %>% 
     ggplot(aes(x=count)) +
     geom_ra()
   
   
   dat_2 %>% 
     ggplot(aes(x=mined, y=count)) +
     geom_boxplot()+
     geom_jitter() 
    
   
  ## 
   

    mP  <- glmmTMB(count ~ spp + mined + (1|site), family = poisson,  data = dat_2)
    mNB <- glmmTMB(count ~ spp + mined + (1|site), family = nbinom2, data = dat_2)
    mZI <- glmmTMB(count ~ spp + mined + (1|site), ziformula = ~1,
                   family = nbinom2, data = dat_2)
    
    ##NB we could also try modelling a hurdle model where a separate count model 
    # is estimated on the truncated count data and a binomial model predicts 0's 
    
##e.g., (MAY NOT RUN, just FYI)
    
    mhurdle <- glmmTMB(count ~ spp + mined + (1|site), ziformula = ~ spp + mined + (1|site),
                   family = truncated_nbinom2(), data = dat_2)
    
    tab_model(mhurdle)
    
    summary(mhurdle)
    
    
    simulationOutput <- simulateResiduals(fittedModel = mP, plot = TRUE) ## note replace m0 with other models and compare below
    
    # 3. Plot the DHARMa residuals
    # This generates the standard DHARMa residual plots for visual inspection.
    plot(simulationOutput)
    
    # 4. Run specific DHARMa tests for model misspecification
    # You can perform various tests to check for issues like overdispersion, zero-inflation, and uniformity.
    testDispersion(simulationOutput)
    testZeroInflation(simulationOutput)
    testUniformity(simulationOutput)
    
    # You can also check for outliers
    testOutliers(simulationOutput)
    

## ========================================================
## PART 2 — StudentLife from GitHub (descriptives, models)
## ========================================================



day_basic <- read_csv("https://raw.githubusercontent.com/dandersonluxford/MLMworkshop/refs/heads/main/data/person_day_basic.csv")
day_plus  <- read_csv("https://raw.githubusercontent.com/dandersonluxford/MLMworkshop/refs/heads/main/data/person_day_plus.csv")

head(day_basic)

## Descriptives (psych) - overall summary stats 
day_basic %>%
  psych::describe() ## what looks dodgy (I deliberately left some dodgy vars - so have a think and run some plots)


## can use psych::describe.by() to look at group specific interests 
## basic approach: # describeBy(data$score, group = data$group)

describeBy(day_basic$n_convo, group = day_basic$deadline_day)

  psych::describeBy(day_basic$deadline_day)

## same for day_plus 
  


## e.g., We might have a hypothesis that stress, sleep, socialising etc influence 
##mobile phone activity 


## Models: NB unlocks (REWB) + NB rate (offset) + LMM continuous
  
  ## Note: to create within and between person decomposition we decompose within person (deviation from person mean) and between person (person mean)

  ## e.g. creating person mean: data$PersonMeanVar <- ave(data$var, data$id_var, FUN = mean)
  ## e.g., create person (within) deviation: data$WithinDeviation <- Data$var - data$PersonTemp
  
  
  ## example - model number of times unlocking phone daily ()
  
  
  ## plot the model 
  
  ## distribution
  day_plus %>% 
    ggplot(aes(x = unlocks)) +
    geom_histogram()
  
  ## relationship w/ sleep? 
  
  
    ggplot(aes(x = sleep, y = unlocks)) +
    geom_jitter() 
  + 
    geom_smooth(method = "loess") ## or "lm" for linear fit 
  
  
  
  
  ## null model
mod_unlock_null <- glmmTMB(
  n_unlocks ~ 1 + (1|uid),
  family = nbinom2, data = day_plus
)

summary(mod_unlock_null)

mod_unlock <- glmmTMB(
  n_unlocks ~ sleep_cwc + sleep_pm + stress_cwc + stress_pm +
    weekend + deadline_day + (1 + sleep_cwc | uid),
  family = nbinom2, data = day_plus
)

anova(mod_unlock_null, mod_unlock) ## does adding predictors improve fit, make sense?

sjPlot::tab_model(mod_unlock_null, mod_unlock, transform = "exp",
                  dv.labels = c("Unlocks null (IRR)","Unlocks + REWB (IRR)")) |> invisible()
DHARMa::simulateResiduals(mod_unlock) |> (\(r){ plot(r); testDispersion(r); testZeroInflation(r); r }) |> invisible()



    m0 <- glmmTMB(n_bt_unique ~ 1 + (1|uid),
                  offset = log(n_scans), family = nbinom2, data = bt)
    m1 <- glmmTMB(n_bt_unique ~ weekend + deadline_day + (1|uid),
                  offset = log(n_scans), family = nbinom2, data = bt)
    anova(m0, m1) |> print()
    sjPlot::tab_model(m0, m1, transform="exp",
                      dv.labels = c("BT rate null (IRR)","BT rate + covars (IRR)")) |> invisible()
    DHARMa::simulateResiduals(m1) |> (\(r){ plot(r); testDispersion(r); r }) |> invisible()
    m1
  }) |> invisible()

  m0c <- lmer(minutes_convo ~ 1 + (1|uid), data = day_plus, REML = FALSE)
  m1c <- lmer(minutes_convo ~ sleep_cwc + sleep_pm + weekend + deadline_day +
                (1 + sleep_cwc | uid), data = day_plus, REML = FALSE)
  anova(m0c, m1c) |> print()
  sjPlot::tab_model(m0c, m1c, dv.labels = c("Convo null","Convo + REWB")) |> invisible()
  performance::check_model(m1c)
}

## ========================================================
## PART 3 — Choose-your-own adventure (subset + toggles)
## ========================================================

## you can install and load the studentlife package (if not already): see https://cran.r-project.org/web/packages/studentlife/readme/README.html

## or explore other datasets I included here: https://github.com/dandersonluxford/MLMworkshop/tree/main/data 


## e,g, load in studentlife 

## Uncomment to install the package from CRAN
# install.packages("studentlife")

## Or, uncomment to install the package from GitHub
# install.packages("devtools")
# devtools::install_github("frycast/studentlife")

# can download test data: 
## can use getwd() to set location to current directory 
download_studentlife(location = #replace with your directory, url = "testdata") 
                       
# e.g., download_studentlife(location = "C:/foldername" , url = "testdata")

#if having issues can include: unzip = TRUE, untar = TRUE

#

