# An introduction to Generalised Additive Mixed Models (GAMMs)

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

<img src="/docs/line_all.png" alt="Many lines" width="750" height="700">

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

---
Family: gaussian

Link function: identity 

Formula:

semitone.norm ~ cat

Parametric coefficients:

             Estimate Std. Error t value Pr(>|t|)  
             
(Intercept)  0.227742   0.007747   29.40   <2e-16 ***

catfunction -0.478826   0.011004  -43.51   <2e-16 ***

R-sq.(adj) =  0.0581   Deviance explained = 5.81%

-REML =  42441  Scale est. = 0.92928   n = 30699

---

## Step 2: Include a smooth for change in F0 over time

## Step 3: Include random intercepts for speakers and words
**Question**: What is the difference between random intercepts and random slopes?

Some speakers or words will on average have a higher F0 than others, and this structural variability is captured by a by-speaker or by-word random intercepts.
On the other hand, the exact difference in F0 between content and function words may vary per speaker. 
Random slopes allow the influence of a predictor to vary for each level of the random-effect factor.

## Step 4: Include by-speaker random slopes

## Step 5: Include non-linear random effects
The smooth specification `s(point, speaker, by=cat, bs="fs",m=1)` replaces the random intercept s(speaker, bs="re").
The factor smooth `("fs")` models non-linear difference over time (the first parameter) with respect to the general time pattern for each of the speakers (the second parameter: the random-effect factor)
The final parameter, `m`, indicates the order of the non-linearity penalty.
`by=cat` allows for individual variability in the effect of syntactic category.
Random intercepts for speakers and words are dropped because the difference is incorporated by the non-centered factor smooth

## Step 6: Account for autocorrelation in the residuals

## Step 7: Include two-dimensional interaction
Interaction of two numerical predictors: time and repetition
Since the predictors are not on the same scale, a tensor product smooth interaction is used.

## Step 8: Compare Hong Kong English and American English

## Step 9: Final model
`discrete` and `nthreads` helps you to speed up the calculation
`discrete` reduces computation time by taking advantage of the fact that numerical predictors often only have a modest number of unique (rounded) values.
`nthreads` speeds up the computation by using multiple processors in parallel to obtain the model fit.

Remeber to save the full model because we don't want to wait every time we run the script.

## Step 10: Visualize the output of the final GAMM
```r
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

