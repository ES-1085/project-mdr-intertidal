Project proposal
================
MDR Intertidal

``` r
library(tidyverse)
library(broom)
library(readxl)
```

## 1. Introduction

Our general research question is “How has the Mount Desert Rock
intertidal zone changed over time?”. We will be using indicator species
such as periwinkles to measure changes in their population. We will also
look at changes in seaweed cover throughout the years.

We will be using intertidal data from the Northeastern Coastal Station
Alliance, a collection of field stations in the Gulf of Maine. We will
specifically be analysing data collected on Mount Desert Rock. It
consists of vertical and horizontal transects collected through quadrats
in the intertidal zone. For this project we will be using the horizontal
transects. This data was collected annually from 2017 to 2023. In the
early years, the data was collected by high school students who were
trained on the rock. The project was continued by Marina in 2022 and
2023.

## 2. Data

``` r
horizontal_MDR_NeCSA_2017  <- read_excel("/cloud/project/data/2017_horizontal_MDR_NeCSA.xlsx")

horizontal_L_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low")
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
horizontal_M_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid")
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
horizontal_H_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High")
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`
    ## • `` -> `...9`
    ## • `` -> `...10`
    ## • `` -> `...11`
    ## • `` -> `...12`
    ## • `` -> `...13`
    ## • `` -> `...14`
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`

``` r
# horizontal_MDR_NeCSA_2019

# horizontal_MDR_NeCSA_2020


horizontal_MDR_NeCSA_2021  <- read_excel("/cloud/project/data/2021_horizontal_MDR_NeCSA.xlsx")

horizontal_MDR_NeCSA_2022  <- read_excel("/cloud/project/data/2022_horizontal_MDR_NeCSA.xlsx")

horizontal_MDR_NeCSA_2023  <- read_excel("/cloud/project/data/2023_horizontal_MDR_NeCSA.xlsx")
```

``` r
MDR_temp_20170812_20180811 <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20170812-20180811")

MDR_temp_20180830_20180929 <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20180830-20180929")
```

    ## Warning: Expecting logical in G43438 / R43438C7: got 'Logged'

``` r
MDR_temp_20180812_20190905 <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20180812-20190905")
```

    ## Warning: Expecting logical in G9328 / R9328C7: got 'Logged'

    ## Warning: Expecting logical in G9330 / R9330C7: got 'Logged'

    ## Warning: Expecting logical in G9333 / R9333C7: got 'Logged'

    ## Warning: Expecting logical in G9335 / R9335C7: got 'Logged'

    ## Warning: Expecting logical in G9338 / R9338C7: got 'Logged'

``` r
WC_MDR_temp_20210602_20211008 <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20210602-20211008WC")
```

    ## Warning: Expecting logical in G3086 / R3086C7: got 'Logged'

    ## Warning: Expecting logical in H3087 / R3087C8: got 'Logged'

    ## Warning: Expecting logical in I3088 / R3088C9: got 'Logged'

    ## Warning: Expecting logical in J3088 / R3088C10: got 'Logged'

Place your data in the /data folder, and add dimensions and codebook to
the README in that folder. Then print out the output of glimpse() or
skim() of your data frame.

## 3. Data analysis plan

Very preliminary exploratory data analysis, including some summary
statistics and visualizations, along with some explanation on how they
help you learn more about your data. (You can add to these later as you
work on your project.) The data visualization(s) that you believe will
be useful in exploring your question(s). (You can update these later as
you work on your project.)

The variables that we will be using to explore our data are tide height,
year, species, sea water temperature, and ascophyllum length. We do not
anticipate needing to find any more data to help with this research
question.

For the preliminary data analysis, we will look at the mean tide height
for certain indicator species. We also plan on mutating the data to be
able to compare the ratio of crabs to snails over time. In addition to
that, we will look at how the percentage of canopy cover affects the
presence of snails and crabs across the quadrats. Finally, we will
create a bar graph of indicator species abundance faceted by year.
