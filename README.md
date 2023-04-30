# An introduction to Generalised Additive Mixed Models (GAMMs)

In this tutorial, you will learn the basics of generalized additive mixed modelling (GAMM).

It is a very useful tatistical tool to analyze dynamic data in sociophonetics.
Plenty of recent studies have made use of GAMM to analyze vowel formants, pitch, articulatory measurements, etc.
In this tutorial, I will show you a step-by-step guide on how GAMM works using my data on Hong Kong English as an example.
# I will present my findings at ICPhS 2023.
# If you want to know more about my study, click here to read the conference paper.
# (Yeung, 2023)
Compare the pitch production between speakers of American English and Hong Kong English.
This tutorial is based on Wieling (2018).

## A brief introduction of the data


## h2 Heading
### h3 Heading

## Horizontal Rules

**This is bold text**

__This is bold text__

*This is italic text*

_This is italic text_

~~Strikethrough~~

## Blockquotes

> Blockquotes can also be nested...
>> ...by using additional greater-than signs right next to each other...
> > > ...or with spaces between arrows.




## Introduction

Multiple recent projects at
[NZILBB](https://www.canterbury.ac.nz/nzilbb/) have used [Generalised
Mixed Models
(GAMMs)](https://en.wikipedia.org/wiki/Generalized_additive_model) to
investigate changes in vowel spaces both across multiples speakers and
within single speakers.

In such projects, it is useful to visualise changes to vowel spaces over
time with both static plots and animations.

This post sets out a structure for fitting models of the first and
second formants of a series of vowels and for visualising them together
within vowel space diagrams.

This general structure, and some specific code for visualisation, was
originally developed by James Brand for @brand2021.

I'll assume the reader knows something about vowels and vowel spaces,
the basics of data manipulation with `dplyr`, and setting up models in
R.

## Fitting Multiple Models with `purrr` and `mgcv`

### Setup

We're going to fit these models with a small subset of the data from the
[Origins of New Zealand English
(ONZE)](https://www.canterbury.ac.nz/nzilbb/research/onze/) corpus. This
dataset contains first and second formant data for 100 speakers of New
Zealand English (for details see [supplementaries for Brand et al.
2021](https://osf.io/q4j29/)). The data can be found
[here](anon_ONZE_mean_sample.rds){target="_blank"}.

For the purposes of this post any similar data set would be fine. We
need:

-   first and second formant data,
-   a range of vowels (we'll only look at monophthongs here),
-   a time variables (whether year of birth, age category, or time
    through recording), and
-   any variables you wish to control for.

Let's [load the libraries](renv.lock){target="_blank"} we will use and
have a look at the data.

``` r
# Load renv environment
renv::use(lockfile = "renv.lock")
```

```{r}
#| include: false
renv::use(lockfile = "renv.lock")
```

```{r}
#| message: false
# Load tidyverse and friends.
library(tidyverse)
library(gganimate)

# mgcv will be used for fitting gamms later and itsadug for visualisation
library(mgcv)
library(itsadug)

# kable for displaying the dataset.
library(kableExtra)

vowels <- read_rds('anon_ONZE_mean_sample.rds')

vowels %>%
  head(10) %>%
  kable() %>%
  kable_styling(font_size = 11) %>%
  scroll_box(width = "100%")
```

In this dataset each row is a vowel token, with columns:

-   `F1_50` and `F2_50`: F1 and F2, taken at the midpoint measured in
    Hz,
-   `Vowel`: Wells lexical set labels for New Zealand English
    monophthongs,
-   `yob`: participant year of birth (our time variable),
-   `Speech_rate`: the average speech rate of the participant across the
    recording (a control variable),
-   `Speaker`: a code indicating which speaker the token comes from
    (sometimes useful as a random effect), and
-   `Gender`: the gender of the speaker (in this case, an `M`/`F`
    binary).

In any real research project, you will need to engage in a lot of data
exploration here. Do you have good data coverage? Is there evidence of
outliers in the data? Does the data need to be normalised? This is the
time to ask this kind of question. The answers will, of course, depend
on your research questions. For this post, the only point of this data
is to illustrate a method for modelling and visualising. We can skip
these questions!

We will now fit separate models for the F1 and F2 of each vowel. Rather
than using a big `for` loop, or fitting each model with a separate line
of code, we will use the `purrr` method of *nesting* our data so that we
have a row for each of the models we want to fit, fit the models, and
then *unnest* to produce data which can be used to visualise our model.
**We nest, we mutate, and we unnest.** This is a common pattern with
`purrr`.

Before we nest, we need to slightly modify our data. Rather than having
a column for our F1 data and a column for our F2 data, we want to
capture there in *rows*. That is, we need our table to be *longer*.
There will then be two rows for each token, one for the F1 and one for
the F2.

To do this, we use the trusty `dplyr` function `pivot_longer()`:
