# An introduction to Generalised Additive Mixed Models (GAMMs)
<p style="font-size: 24px;">Author: Ping Hei Yeung
  <br>
Georgetown University
  <br>
May 11, 2023</p>

## What is GAMM?

This tutorial is based on Wieling (2018).
In this tutorial, you will learn the basics of generalized additive mixed modelling (GAMM).

It is a very useful tatistical tool to analyze dynamic data in sociophonetics.
Plenty of recent studies have made use of GAMM to analyze vowel formants, pitch, articulatory measurements, etc.
In this tutorial, I will show you a step-by-step guide on how GAMM works using my data on Hong Kong English as an example.

## Our sample data: tones in Hong Kong English
In this study, I want to find out if Hong Kong English is a tone language.
According to recent studies on Hong Kong English like Wee <a href="https://muse.jhu.edu/article/621181">(2016)</a> and Gussenhoven <a href="https://doi.org/10.1093/oxfordhb/9780199777716.013.29">(2014)</a>, monosyllabic content words may have a **high** tone while monosyllabic function words may have a **low** tone.
I want to know if that is really the case, so I asked speakers of Hong Kong English and American English to read those words, and I extracted the F0 contours they produced using Praat.
As you can see in **Figure 1**, speakers of Hong Kong English tend to produce content words like *four* with a higher pitch than function words like *for*.

**Figure 1**. The F0 contour of a Hong Kong English speaker saying ***Say four again*** and ***Say for again***:
![HKE example](/docs/HKE_example.png)
Listen to it:
<audio controls>
  <source src="https://raw.githubusercontent.com/yeungpinghei/yeungpinghei.github.io/docs/HKE_example.wav" type="audio/wav">
</audio>
<br>
On the other hand, speakers of American English like the one in **Figure 2** seem to produce both the ceontent words and the function words with the same pitch.
You may compare the two figures and see how they differ.

**Figure 2**: The F0 contour of a American English speaker saying ***Say four again*** and ***Say for again***:
![AE example](/docs/AME_example.png)
Listen to it:
<audio controls>
  <source src="https://raw.githubusercontent.com/yeungpinghei/yeungpinghei.github.io/docs/AME_example.wav" type="audio/wav">
</audio>
<br>
However, just by looking at the raw F0 contour alone is not enough to answer my research question.
How do I know if the difference is statistically significant?
In the next section, I will provide a step-by-step guide on how I used GAMM to analyze me data.

---
I will present my findings at <a href="https://www.icphs2023.org/">ICPhS 2023</a> this August, so please come if you want to know more about my study!
My paper is titled *"Contact-induced tonogenesis in Hong Kong English"* and it should be available soon on the conference website.

---

## A brief introduction of the data
First, download the R script **gamm_tutorial.R** and the csv file **gamm_tutorial.csv** from the Github repository.

Load the packages we need for the R script.
```r
# Load the packages
library(tidyverse)
library(mgcv)
library(itsadug)
library(tidymv)
```
Import the csv file and define the data type of each column.

```r
data <- read_csv("gamm_tutorial.csv", col_names = T)
data <- data %>%
  mutate_at(c("speaker", "variety", "gender", "word", "token", "cat","adjacent"), as.factor) %>%
  mutate_at(c("age", "duration", "repetition", "point", "F0", "semitone.norm"), as.numeric)
summary(data)
```

In in csv file, each row represents an F0 measurement, with columns:

- `speaker`:  a unique code for each speaker
- `variety`: the English variety spoken by the participant, American English (AE) or Hong Kong English (HKE)
- `age`: age of the speaker
- `gender`: gender of the speaker
- `word`: the word from which the measurement was taken
- `duration`: duration (in seconds) of the target word
- `repetition`: each target word was repeated three times (1-3)
- `point`: 9 equidistant F0 measurements were made at the 10%-90% intervals of the target words (1-9)
- `F0`: the raw F0 measurements from Praat
- `token`: the three columns `speaker`, `word`, and `repetition` combined into one
- `semitone.norm`: the F0 measurements converted to semitones and z-score normalized by speaker
-  `cat`: the syntactic category of the target word, content word (content) or function word (function)
- `adjacent`: the onset and coda consonants of the target word

First, we visualize the original data to check how it looks like before applying any statistical models.

```r
# Normalized F0 trajectory of each token by individual speakers
data %>%
  ggplot(aes(x = point, y = semitone.norm, color = cat, group = token)) + 
  geom_line() +
  facet_wrap(~speaker)
```

<img src="/docs/line_all.png" alt="Many lines" width="70%">

Here we have the normalized F0 contours of each speaker, but there's not much we can get from the graph since individual lines are messy.

Let's make a `geom_smooth` plot to see what speakers of Hong Kong English and American English did in general.

```r
# Normalized F0 trajectory by syntactic category and English variety
data %>%
  group_by(speaker,cat,point,variety) %>%
  summarise(mean = mean(semitone.norm)) %>%
  ggplot(aes(x = point, y = mean, group = cat, color = cat)) +
  geom_smooth(method="loess") +
  facet_wrap(~variety)
```
<img src="/docs/smooth_variety.png" alt="geom_smooth">

We can observe that speakers of Hong Kong English and American English produced the content words and function words with differnt pitch contours.
As indicated by the 95% confidence interval (the shaded area), there is a complete overlap of pitch contour for American English speakers.
Speakers of Hong Kong English on the other hand produced the content words with a much higher F0 than the function words, and there is no overlap of the confidence interval.
However, this graph does not the consider the effect of other factors on pitch production like adjacent segments, number of repetion and duration, which may all contribute to the F0 difference.
Thus, the F0 difference by syntactic category may be exaggerated.

## Step 1: The most basic linear model
```r
m1 <- bam(semitone.norm ~ cat, data=data, method="fREML")
summary(m1)
```

<img src="/docs/m1_summary.png" alt="m1_summary" width="50%">

## Step 2: Include a smooth for change in F0 over time
```r
m2 <- bam(semitone.norm ~ cat + s(point, by=cat,bs="tp", k=9), data=data)
summary(m2)
```

<img src="/docs/m2_summary.png" alt="m2_summary" width="50%">

**Question**: How do we interpret the results?

- `Ref.df`: the reference number of degrees of freedom used for hypothesis testing
- `edf`: the number of effective degrees of freedom, the amount of non-linearity of the smooth. Greater value indicates more complex pattern.
- `R-sq`: the amount of variance explained by the regression
- `Deviance explained`:a generalization of R-sq, basically the same as R-sq
- `fREML`: no meaning by itself, used for comparing models
- `Scale est.`: the variance of the residuals
- `n`: the number of data points

```r
## Check the model
gam.check(m2)
```

Low p-value (k-index<1) may indicate that k is too low, especially if edf is close to k'.

We may visualize the results of GAMM using `plot_smooth()` and `plot_diff()`

```r
plot_smooth(m2, view="point", plot_all= "cat", rug=FALSE)
plot_diff(m2, view="point", comp=list(cat=c("content","function")))
```
<head>
<style>
      .image-container {
        display: flex;
      }
      .image-container img {
        width: 50%;
        padding: 5px;
      }
</style>
</head>
    
<div class="image-container">
  <img src="/docs/plot_smooth_m2.png" alt="plot_smooth">
  <img src="/docs/plot_diff_m2.png" alt="plot_diff">
</div>

```r
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
```
![m2.predictions](/docs/m2.predictions.png)


```r
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
```
![m2.diff](/docs/m2.diff.png)

## Step 3: Include random intercepts for speakers and words
```r
m3 <- bam(semitone.norm ~ cat +
                s(point, by=cat, k=9) +
                s(speaker,bs="re") +
                s(word, bs="re"),
              data=data)
summary(m3)
```
<img src="/docs/m3_summary.png" alt="m3_summary" width="50%">

```r
# Model comparison
compareML(m2,m3)
```
<img src="/docs/m2_m3_compare.png" alt="compare" width="50%">

The model with a lower AIC is better.

**Question**: What is the difference between random intercepts and random slopes?

Some speakers or words will on average have a higher F0 than others, and this structural variability is captured by a by-speaker or by-word random intercepts.
On the other hand, the exact difference in F0 between content and function words may vary per speaker. 
Random slopes allow the influence of a predictor to vary for each level of the random-effect factor.

## Step 4: Include by-speaker random slopes
```r
m4 <- bam(semitone.norm ~ cat +
                s(point, by=cat, k=9) +
                s(word, bs="re") +
                s(speaker, cat, bs="re"),
              data=data)
summary(m4)
```

<img src="/docs/m4_summary.png" alt="m4_summary" width="55%">


## Step 5: Include non-linear random effects
```r
m5 <- bam(semitone.norm ~ cat +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data)
summary(m5)
```
<img src="/docs/m5_summary.png" alt="m5_summary" width="50%">

It may start to take more time to run the code since the model is getting more complex.

The smooth specification `s(point, speaker, by=cat, bs="fs",m=1)` replaces the random intercept `s(speaker, bs="re")`.
The factor smooth `("fs")` models non-linear difference over time (the first parameter) with respect to the general time pattern for each of the speakers (the second parameter: the random-effect factor)
The final parameter, `m`, indicates the order of the non-linearity penalty.
`by=cat` allows for individual variability in the effect of syntactic category.
Random intercepts for speakers and words are dropped because the difference is incorporated by the non-centered factor smooth

## Step 6: Account for autocorrelation in the residuals
```r
m5.acf <- acf_resid(m5)
data <- data %>%
  arrange(speaker, word, repetition, point) %>%
  group_by(speaker, word, repetition) %>%
  mutate(start.event = case_when(point == min(point) ~ TRUE, TRUE ~ FALSE), .after = point)
```
<img src="/docs/acf_resid.png" alt="acf_resid">

```r
m6 <- bam(semitone.norm ~ cat +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data,
              rho=m5.acf[2], AR.start=data$start.event)
summary(m6)
```
<img src="/docs/m6_summary.png" alt="m6_summary" width="50%">

```r
compareML(m5,m6)
```
<img src="/docs/m5_m6_compare.png" alt="compare" width="60%">

## Step 7: Include two-dimensional interaction
Interaction of two numerical predictors: time and repetition
Since the predictors are not on the same scale, a tensor product smooth interaction is used.

```r
m7 <- bam(semitone.norm ~ cat +
                te(point, repetition, k=3) +
                s(word, bs="re") +
                s(point, speaker, by=cat, bs="fs", m=1),
              data=data)
summary(m7)
```

<img src="/docs/m7_summary.png" alt="m7_summary" width="50%">

## Step 8: Compare Hong Kong English and American English
```r
# Combine the two variables
data$cat.variety <- interaction(data$cat, data$variety)
# It takes a while to load...
m8 <- bam(semitone.norm ~ cat.variety +
            te(point, repetition, k=3) +
            s(point, speaker, by=cat, bs="fs", m=1) +
            s(point, word, by=variety, bs="fs", m=1),
          data=data)
summary(m8)
```

<img src="/docs/m8_summary.png" alt="m8_summary" width="50%">

## Step 9: Final model
`discrete` and `nthreads` helps you to speed up the calculation
`discrete` reduces computation time by taking advantage of the fact that numerical predictors often only have a modest number of unique (rounded) values.
`nthreads` speeds up the computation by using multiple processors in parallel to obtain the model fit.

Remeber to save the full model because we don't want to wait every time we run the script.
```r
# Save the full model because we don't want to wait every time we run the script
saveRDS(m.full, file = "gamm_result.rds")
```

```r
# Load the saved GAMM results to save your time
m.full <- readRDS("gamm_result.rds")
```


```r
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
```

```r
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
              data = data
summary(m.full)
```
<img src="/docs/m9_summary.png" alt="m9_summary" width="50%">

## Step 10: Visualize the output of the final GAMM
```r
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
```

<img src="/docs/gamm_result.png" alt="gamm_result">

```r
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
```

<img src="/docs/gamm_diff.png" alt="gamm_diff">
