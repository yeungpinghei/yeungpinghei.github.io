# An introduction to Generalised Additive Mixed Models (GAMMs)
<p style="font-size: 24px;">Author: Ping Hei Yeung
  <br>
Georgetown University
  <br>
May 11, 2023</p>

## What is GAMM?
Generalized Additive Mixed Models (<a href="https://www.taylorfrancis.com/books/mono/10.1201/9781315370279/generalized-additive-models-simon-wood">GAMMs; Wood 2017</a>) are an extension of Generalized Linear Mixed Models that allow for more flexible modeling of nonlinear relationships between the depdendent and independent variables.
A relatively new approach, GAMMs have received increased attention in recent sociophonetic studies that involve the analysis of vowel formants (<a href="https://doi.org/10.1121/1.5089886">Kirkham et al., 2019</a>; <a href="https://doi.org/10.1177/00754242211043163">Stanley et al., 2021</a>) and articulatory measurements (<a href="https://doi.org/10.5334/labphon.214">Carignan et al., 2020</a>).
GAMMs are comparable to linear mixed effect modeling, but they differ in how they model the relationships between depdendent and independent variables.
In linear mixed effect modeling, the relationships between the variables are typically modeled using linear functions.
In GAMMs on the other hand, the relationships between the variables are modeled using smooth functions, such as splines or smoothing splines.
These smooth functions can capture more complex and nonlinear relationships between the variables, which allows for the modeling of nonlinear differences like vowel formant trajectories.
Therefore, GAMMs have a major advantage in modeling dynamic acoustic and articulatory data like vowel formant trajectories and F0 contours which are nonlinear in nature.

In this tutorial, you will learn the basics of generalized additive mixed modelling (GAMM) using my data on Hong Kong English as an example.
This tutorial is inspired by Wieling (<a href="https://doi.org/10.1016/j.wocn.2018.03.002">2018</a>) and modifications have been made to suit my own data.

---
Check out the tutorial by Winter & Wieling (<a href="https://doi.org/10.1093/jole/lzv003">2016</a>), Sóskuthy (<a href="
https://doi.org/10.48550/arXiv.1703.05339">2017</a>, <a href="https://doi.org/10.1016/j.wocn.2020.101017">2021</a>), Wieling (<a href="https://doi.org/10.1016/j.wocn.2018.03.002">2018</a>) for more sophisticated explanations on the mechanisms of GAMMs.

---

## Our sample data: tones in Hong Kong English
In this study, I want to find out if Hong Kong English is a tone language.
According to recent studies on Hong Kong English like Wee (<a href="https://muse.jhu.edu/article/621181">2016</a>) and Gussenhoven (<a href="https://doi.org/10.1093/oxfordhb/9780199777716.013.29">2014</a>), monosyllabic content words may have a **high** tone while monosyllabic function words may have a **low** tone.
I want to know if that is really the case, so I asked speakers of Hong Kong English and American English to read those words, and I extracted the F0 contours they produced using Praat.
As you can see in **Figure 1**, speakers of Hong Kong English tend to produce content words like *four* with a higher pitch than function words like *for*.

**Figure 1**. The F0 contour of a Hong Kong English speaker saying ***Say four again*** and ***Say for again***:
![HKE example](/docs/HKE_example.png)
Listen to it:
<audio controls>
  <source src="https://raw.githubusercontent.com/yeungpinghei/yeungpinghei.github.io/docs/HKE_example.wav" type="audio/wav">
</audio>
<br>
On the other hand, speakers of American English like the one in **Figure 2** seem to produce both the content words and the function words with the same pitch.
You may compare the two figures and see how they differ.

**Figure 2**: The F0 contour of a American English speaker saying ***Say four again*** and ***Say for again***:
![AE example](/docs/AME_example.png)
Listen to it:
<audio controls>
  <source src="https://raw.githubusercontent.com/yeungpinghei/yeungpinghei.github.io/docs/AME_example.wav" type="audio/wav">
</audio>
<br>
However, just by looking at the raw F0 contour alone is not enough to answer my research question.
How do I know if the difference between Hong Kong English and American English is statistically significant?
In the next section, I will provide a step-by-step guide on how I used GAMM to analyze the data.

---
I will present my findings at <a href="https://www.icphs2023.org/">ICPhS 2023</a> this August, so please come if you want to know more about my study!
My paper is titled *"Contact-induced tonogenesis in Hong Kong English"* and it should be available soon on the conference website.

---

## A brief introduction of the data
First, download the R script **gamm_tutorial.R** and the csv file **gamm_tutorial.csv** from the <a href="https://github.com/yeungpinghei/yeungpinghei.github.io/tree/main">Github repository</a>.

Open the R script on RStudio and load the packages we need.
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

| word | adjacent | speaker | variety | age | gender | duration | repetition | point | F0 | token | semitone.norm | cat |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| an | null.son | AME_007 | AE | 19 | F | 0.429388633 | 3 | 8 | 200.8319991 | AE_007-an-3 | 0.634121036 | function |
| an | null.son | HKE_024 | HKE | 28 | F | 0.277847326 | 1 | 1 | 211.5430769 | HKE_024-an-1 | 1.173468235 | function |
| an | null.son | AME_013 | AE | 21 | M | 0.363250934 | 1 | 5 | 139.3838609 | AE_013-an-1 | -0.285611094 | function |
| an | null.son | HKE_006 | HKE | 19 | F | 0.354345514 | 1 | 2 | 242.4686589 | HKE_006-an-1 | -0.003444452 | function |
| an | null.son | HKE_012 | HKE | 24 | M | 0.164787882 | 2 | 4 | 97.84136779 | HKE_012-an-2 | -0.064295015 | function |
| an | null.son | HKE_023 | HKE | 30 | M | 0.227550751 | 1 | 5 | 116.2275949 | HKE_023-an-1 | -0.517019108 | function |
| an | null.son | AME_023 | AE | 20 | F | 0.390254703 | 1 | 6 | 201.8185885 | AE_023-an-1 | -0.289436547 | function |
| an | null.son | HKE_010 | HKE | 38 | F | 0.247996446 | 1 | 9 | 140.0242878 | HKE_010-an-1 | -1.458272919 | function |
| an | null.son | AME_033 | AE | 18 | M | 0.207696414 | 2 | 2 | 93.90499073 | AE_033-an-2 | -1.735087432 | function |
| an | null.son | HKE_038 | HKE | 35 | F | 0.22003899 | 2 | 5 | 180.9764375 | HKE_038-an-2 | -0.992133573 | function |


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

Before applying any statistical models, we may first visualize the original data to check how they look like.

```r
# Normalized F0 trajectory of each token by individual speakers
data %>%
  ggplot(aes(x = point, y = semitone.norm, color = cat, group = token)) + 
  geom_line() +
  facet_wrap(~speaker)
```

<img src="/docs/line_all.png" alt="Many lines" width="70%">

Here we have the normalized F0 contours of each speaker, but it's hard for us to pick up any patterns from the graph since the individual lines are messy.

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

We can see that speakers of Hong Kong English and American English produced the content words and function words with differnt pitch contours.
As indicated by the overlap of the 95% confidence interval (the shaded area), the pitch contour of content words and function words did not differ significantly for American English speakers.
Speakers of Hong Kong English on the other hand produced the content words with a much higher F0 than the function words, and there was no overlap of the confidence intervals.
However, this graph does not the consider the effect of other factors on pitch production like adjacent segments, number of repetion and duration, which may all contribute to the F0 difference.
Thus, the effect of syntactic category on F0 may be overestimated.

## Step 1: The most basic linear model
In this step, we construct a very basic linear regression model using the `bam()` function with the normalized F0 `semitone.norm` as the dependent variable and the syntactic category `cat` as the independent variable. The argument `data` refers to the data frame containing the model response variable and covariates required by the formula, which is named as `data`. The argument `method` refers to the estimation method we use for thesmoothing parameter. Here we may use the default method which is `"fREML"`. We will start from here and expand our model bit by bit.
```r
m1 <- bam(semitone.norm ~ cat, data=data, method="fREML")
summary(m1)
```

<img src="/docs/m1_summary.png" alt="m1_summary" width="50%">
The model summary shows that

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
<img src="/docs/m9_summary.png" alt="m9_summary" width="60%">

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
