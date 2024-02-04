Project tidying
================
MDR Intertidal

``` r
library(tidyverse)
library(broom)
library(readxl)
```

``` r
horizontal_MDR_NeCSA_2017  <- read_excel("/cloud/project/data/2017_horizontal_MDR_NeCSA.xlsx", na = c("*all rock", "*skipped because of tide pool"))

seaweed_mdr_2017 <- horizontal_MDR_NeCSA_2017 %>%
  subset(select = -c(Site, Name, Llitt:SembalA2 )) %>%
  add_column(year = "2017") %>%
  pivot_longer(
    cols = AscoCC:FucSppA,
    names_to = "seaweed_species",
    values_to = "proportion")
  
 inverts_mdr_2017 <- horizontal_MDR_NeCSA_2017 %>%
   subset(select = -c(Site, Name, AscoCC:AscoBladders)) %>%
   add_column(year = "2017") %>%
   pivot_longer(
     cols = Llitt:SembalA2,
     names_to = "invert_species",
     values_to = "count"
   )
```

``` r
# not sure if this spreadsheet is complete - omit?

# low quadrats

HL00_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "A4:C49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 0)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL03_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "D4:F49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 3)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL06_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "G4:I49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 6)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL09_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "J4:L49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 9)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL12_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "M4:O49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 12)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL15_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "P4:R49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 15)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL18_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "S4:U49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 18)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL21_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "V4:X49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 21)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL24_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "Y4:AA49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 24)
```

    ## New names:
    ## • `` -> `...3`

``` r
HL27_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Low", range = "AB4:AD49") %>%
  mutate(tide_height = "low",
         year = "2018",
         quadrat = 27)
```

    ## New names:
    ## • `` -> `...3`

``` r
# mid quadrats

HM00_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "A4:C49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 0)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM03_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "D4:F49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 3)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM06_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "G4:I49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 6)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM09_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "J4:L49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 9)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM12_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "M4:O49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 12)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM15_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "P4:R49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 15)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM18_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "S4:U49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 18)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM21_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "V4:X49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 21)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM24_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "Y4:AA49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 24)
```

    ## New names:
    ## • `` -> `...3`

``` r
HM27_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT Mid", range = "AB4:AD49") %>%
  mutate(tide_height = "mid",
         year = "2018",
         quadrat = 27)
```

    ## New names:
    ## • `` -> `...3`

``` r
# high quadrats

HH00_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "A4:C49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 0)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH03_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "D4:F49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 3)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH06_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "G4:I49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 6)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH09_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "J4:L49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 9)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH12_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "M4:O49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 12)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH15_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "P4:R49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 15)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH18_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "S4:U49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 18)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH21_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "V4:X49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 21)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH24_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "Y4:AA49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 24)
```

    ## New names:
    ## • `` -> `...3`

``` r
HH27_MDR_NeCSA_2018 <- read_excel("/cloud/project/data/2018_full_NeCSA.xlsx", sheet = "West HT High", range = "AB4:AD49") %>%
  mutate(tide_height = "high",
         year = "2018",
         quadrat = 27)
```

    ## New names:
    ## • `` -> `...3`

``` r
# horizontal_MDR_NeCSA_2019
```

``` r
# horizontal_MDR_NeCSA_2020
```

``` r
horizontal_MDR_NeCSA_2021  <- read_excel("/cloud/project/data/2021_horizontal_MDR_NeCSA.xlsx")
```

``` r
horizontal_MDR_NeCSA_2022  <- read_excel("/cloud/project/data/2022_horizontal_MDR_NeCSA.xlsx")
```

``` r
horizontal_MDR_NeCSA_2023  <- read_excel("/cloud/project/data/2023_horizontal_MDR_NeCSA.xlsx")
```
