# LING716 Advanced methods in sociophonetics
# Tutorial on generalized additive mixed modelling (GAMM)
# Ping Hei YEUNG
# May 11, 2023
# Compare the pitch production between speakers of American English and Hong Kong English
# Based on Wieling (2018)

# Load the packages
library(tidyverse)
library(mgcv)
library(itsadug)
library(tidymv)

# Import data
data <- read_csv("gamm_tutorial.csv", col_names = T)
data <- data %>%
  mutate_at(c("speaker", "variety", "gender", "word", "token", "cat","adjacent"), as.factor) %>%
  mutate_at(c("age", "duration", "repetition", "point", "F0", "semitone.norm"), as.numeric)
summary(data)

# speaker: individual speakers
# variety: American English (AME) or Hong Kong English (HKE)
# age: age of individual speakers
# gender: gender of individual speakers
# word: individual words
# duration: duration (in seconds) of the target word
# repetition: each target word was repeated three times (1-3)
# point: 9 equidistant F0 measurements were made at the 10%-90% intervals of target words (1-9)
# F0: raw F0 measurements from Praat
# pos: parts of speech of the target words (determiner, auxiliary, etc.)
# semitone: pitch converted from F0 to semitone
# token: speaker + word + repetition
# semitone.norm: semitones z-score normalized by speaker
# cat: content word or function word
# adjacent: preceding segment and following segment

### Visualize the original data
# Normalized F0 trajectory of each token by individual speakers
data %>%
  ggplot(aes(x = point, y = semitone.norm, color = cat, group = token)) + 
  geom_line() +
  facet_wrap(~speaker)

# Normalized F0 trajectory by syntactic category and English variety
data %>%
  group_by(speaker,cat,point,variety) %>%
  summarise(mean = mean(semitone.norm)) %>%
  ggplot(aes(x = point, y = mean, group = cat, color = cat)) +
  geom_smooth(method="loess") +
  facet_wrap(~variety)

# Now we can start analyzing the data using GAMM! #

#### STEP 1: The most basic linear model ####
m1 <- bam(semitone.norm ~ cat, data=data, method="fREML")
summary(m1)

#### STEP 2: Include a smooth for change in F0 over time ####
m2 <- bam(semitone.norm ~ cat + s(point, by=cat,bs="tp", k=9), data=data)
summary(m2)
# Question: How do we interpret the results?
# Ref.df: the reference number of degrees of freedom used for hypothesis testing
# edf: the number of effective degrees of freedom, the amount of non-linearity of the smooth. Greater value indicates more complex pattern.
# R-sq: the amount of variance explained by the regression
# Deviance explained:a generalization of R-sq, basically the same as R-sq
# fREML: no meaning by itself, used for comparing models
# Scale est.: the variance of the residuals
# n: the number of data points

## Check the model
gam.check(m2)
# Low p-value (k-index<1) may indicate that k is too low, especially if edf is close to k'.

## Visualize GAMM results
# More basic graphs
plot_smooth(m2, view="point", plot_all= "cat", rug=FALSE)
plot_diff(m2, view="point", comp=list(cat=c("content","function")))

# If you want fancier graphs
m2 %>%
  get_gam_predictions(point, series_length = 150, exclude_random = TRUE) -> m2.predictions
m2.predictions %>%
  ggplot(aes(point * 10, semitone.norm)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = cat, group = cat), alpha = 0.2) +
  geom_line(aes(colour = cat)) +
  scale_x_continuous(name = "Normalized Time (%)", breaks = seq(0, 100, by=20)) + 
  scale_y_continuous(name = "F0 (normalized)") +
  scale_color_discrete(name = "Syntactic category", labels = c("Content words","Function words")) +
  scale_fill_discrete(name = "Syntactic category", labels = c("Content words","Function words")) +
  theme_minimal(base_size = 14)

m2 %>%
  get_smooths_difference(point, list(cat = c("content", "function"))) -> m2.diff
m2.diff %>%
  ggplot(aes(point*10, difference, group = group)) +
  geom_hline(aes(yintercept = 0), colour = "darkred") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = sig_diff), alpha = 0.3) +
  geom_line(aes(colour = sig_diff), size = 1) +
  scale_x_continuous(name = "Normalized Time (%)", breaks = seq(0, 100, by=20)) +
  labs(colour = "Significant", fill = "Significant") +
  theme_minimal(base_size = 14)

#### STEP 3: Include random intercepts for speakers and words ####
# Question: What is the difference between random intercepts and random slopes?
# Some speakers or words will on average have a higher F0 than others, and this structural variability is captured by a by-speaker or by-word random intercepts.
# On the other hand, the exact difference in F0 between content and function words may vary per speaker. 
# Random slopes allow the influence of a predictor to vary for each level of the random-effect factor.

m3 <- bam(semitone.norm ~ cat +
                s(point, by=cat, k=9) +
                s(speaker,bs="re") +
                s(word, bs="re"),
              data=data)
summary(m3)
# Model comparison
m2.ml <- bam(semitone.norm ~ cat + s(point, by=cat,bs="tp", k=9), data=data, method="ML")
m3.ml <- bam(semitone.norm ~ cat + s(point, by=cat, k=9) + s(speaker,bs="re") + s(word, bs="re"), data=data, method="ML")
compareML(m2.ml,m3.ml)
# The model with a lower AIC is better

#### STEP 4: Include by-speaker random slopes ####
m4 <- bam(semitone.norm ~ cat +
                s(point, by=cat, k=9) +
                s(word, bs="re") +
                s(speaker, cat, bs="re"),
              data=data)
summary(m4)

#### STEP 5: Include non-linear random effects ####
# The smooth specification s(point, speaker, by=cat, bs="fs",m=1) replaces the random intercept s(speaker, bs="re").
# The factor smooth ("fs") models non-linear difference over time (the first parameter) with respect to the general time pattern for each of the speakers (the second parameter: the random-effect factor)
# The final parameter, m, indicates the order of the non-linearity penalty.
# by=cat allows for individual variability in the effect of syntactic category.
# Random intercepts for speakers and words are dropped because the difference is incorporated by the non-centered factor smooth

m5 <- bam(semitone.norm ~ cat +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data)
summary(m5)

#### STEP 6: Account for autocorrelation in the residuals ####
m5.acf <- acf_resid(m5)
data <- data %>%
  arrange(speaker, word, repetition, point) %>%
  group_by(speaker, word, repetition) %>%
  mutate(start.event = case_when(point == min(point) ~ TRUE, TRUE ~ FALSE), .after = point)

m6 <- bam(semitone.norm ~ cat +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data,
              rho=m5.acf[2], AR.start=data$start.event)
summary(m6)
compareML(m5,m6)

#### STEP 7: Include two-dimensional interaction ####
# Interaction of two numerical predictors: time and repetition
# Since the predictors are not on the same scale, a tensor product smooth interaction is used.
m7 <- bam(semitone.norm ~ cat +
                te(point, repetition, k=3) +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data)
summary(m7)

#### STEP 8: Compare HKE and AME ####
# Combine the two variables
data$cat.variety <- interaction(data$cat, data$variety)
# It takes a while to load...
m8 <- bam(semitone.norm ~ cat.variety +
            te(point, repetition, k=3) +
            s(point, speaker, by=cat, bs="fs", m=1) +
            s(point, word, by=variety, bs="fs", m=1),
          data=data)
summary(m8)

#### STEP 9: Final model ####
# Load the saved GAMM results to save your time
m.full <- readRDS("gamm_result.rds")

##### It may take a while to run #####
m.full.noar <- bam(semitone.norm ~ cat.variety +
                     s(point, k=9) +
                     s(duration) +
                     s(repetition, k=3) +
                     s(point, by = cat.variety, k=9) +
                     ti(point, duration, k=9) + # Fixed interaction of duration x time
                     ti(point, repetition, k=3) + # Fixed interaction of repetition x time            
                     s(point, by = adjacent, k=9) + # Difference smooth for syllable structure
                     # Random Reference/Difference smooths for Speaker/Word
                     s(point, speaker, bs = 'fs', m = 1, k=9) +
                     s(point, speaker, by = cat, bs = 'fs', m = 1, k=9) +
                     s(point, word, bs = 'fs', m = 1, k=9) +
                     s(point, word, by = variety, bs = 'fs', m = 1, k=9),
                   discrete = TRUE, nthreads = parallel::detectCores(logical = FALSE) - 1,
                   data = data)
summary(m.full.noar)

# discrete and nthreads helps you to speed up the calculation
# discrete reduces computation time by taking advantage of the fact that numerical predictors often only have a modest number of unique (rounded) values.
# nthreads speeds up the computation by using multiple processors in parallel to obtain the model fit.

m.full.acf <- acf_resid(m.full.noar)
m.full <- bam(semitone.norm ~ cat.variety +
                s(point, k=9) +
                s(duration) +
                s(repetition, k=3) +
                s(point, by = cat.variety, k=9) +
                ti(point, duration, k=9) +
                ti(point, repetition, k=3) +                
                s(point, by = adjacent, k=9) +
                s(point, speaker, bs = 'fs', m = 1, k=9) +
                s(point, speaker, by = cat, bs = 'fs', m = 1, k=9) +
                s(point, word, bs = 'fs', m = 1, k=9) +
                s(point, word, by = variety, bs = 'fs', m = 1, k=9),
              rho = m.full.acf[2], AR.start = data$start.event,
              discrete = TRUE, nthreads = parallel::detectCores(logical = FALSE) - 1,
              data = data)

# Save the full model because we don't want to wait every time we run the script
saveRDS(m.full, file = "gamm_result.rds")
###########
# Model summary (it also takes a while to load)
summary(m.full)

# Visualization
m.full.predictions <- m.full %>%
  get_predictions(cond = list(cat.variety = c("content.AE","function.AE","content.HKE","function.HKE"), point=seq(1,9,0.1))) %>%
  mutate(lower = fit - CI, upper = fit + CI)
m.full.predictions <- separate(m.full.predictions, cat.variety, c("cat","variety"))
m.full.predictions %>%
  ggplot(aes(point * 10, fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = cat, group = cat), alpha = 0.2) +
  geom_line(aes(colour = cat)) +
  scale_x_continuous(name = "Normalized Time (%)", breaks = seq(0, 100, by=20)) + 
  scale_y_continuous(name = "Predicted F0 (z-score)") +
  scale_color_discrete(name = "Syntactic category", breaks = c("content","function"), labels = c("Content word","Function word")) +
  scale_fill_discrete(name = "Syntactic category", breaks = c("content","function"), labels = c("Content word","Function word")) +
  theme_minimal(base_size = 18) +
  facet_wrap(~variety)

m.full %>%
  get_smooths_difference(point, list(cat.variety = c("function.HKE","content.HKE"))) -> hke.diff
hke.diff <- hke.diff %>%
  mutate(cat="function word - content word")
m.full %>%
  get_smooths_difference(point, list(cat.variety = c("function.AE","content.AE"))) -> ame.diff
ame.diff <- ame.diff %>%
  mutate(cat="function word - content word",variety = "AE")
diff <- rbind(hke.diff,ame.diff)
diff$variety = factor(diff$variety, levels=c('AE','HKE'))
diff %>%
  ggplot(aes(point*10, difference, group = group)) +
  geom_hline(aes(yintercept = 0), colour = "darkred") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = sig_diff), alpha = 0.3) +
  geom_line(aes(colour = sig_diff), size = 1) +
  scale_x_continuous(name = "Normalized Time (%)", breaks = seq(0, 100, by=20)) +
  scale_y_continuous(name = "Predicted F0 difference (z-score)") +
  scale_color_discrete(breaks = c("TRUE","FALSE"), labels = c("Significant","Insignificant")) +
  scale_fill_discrete(breaks = c("TRUE","FALSE"), labels = c("Significant","Insignificant")) +
  labs(colour = "", fill = "") +
  theme_minimal(base_size = 18) +
  facet_grid(cat~variety)
