Project tidying
================
MDR Intertidal

``` r
library(tidyverse)
library(broom)
library(readxl)
library(lubridate)
library(tidyr)
library(janitor)
library(anytime)
```

``` r
horizontal_MDR_NeCSA_2017  <- read_excel("/cloud/project/data/2017_horizontal_MDR_NeCSA.xlsx", na = c("*all rock", "*skipped because of tide pool"))
```

``` r
seaweeds_mdr_2017 <- horizontal_MDR_NeCSA_2017 %>%
  select(-c(Site, Name, Llitt:SembalA2 )) %>%
  add_column(year = "2017") %>%
  rename(date = Date,
         tide_ht = Tide,
         quadrat_m = `Quadrat (m)`,
         Asco_nCC = AscoCC,
         Fucu_vCC = FucVCC,
         Fucu_sCC = FucSCC,
         Fucu_dCC = FucDCC,
         Fucu_spp = FucSpp.CC,
         Asco_nSC = AscoSC,
         Fucu_vSC = FucVSC,
         Fucu_sSC = FucSSC,
         Fucu_dSC = FucDSC,
         Mast_sSC = MstellSC,
         Chon_cSC = ChonCrSC,
         Ulva_iSC = UlvaIntesSC,
         Ulva_lSC = UlvaLacSC,
         Cora_oSC = CorOSC,
         Vert_lSC = VertLanSC,
         Cera_rSC = CerSC,
         Lami_spp = `Laminaria spp.`,
         Porp_sp = `Porphyra sp.`,
         Asco_recruits = AscoR,
         Asco_holdfasts = AscoA,
         Fucu_recruits = FucR,
         Fucu_v_holdfasts = FucVA,
         Fucu_s_holdfasts = FucSA,
         Fucu_d_holdfasts = FucDA,
         Asco_n_ht = `AscoHeight (cm)`,
         Asco_n_bladders = AscoBladders,
         Fucu_spp_holdfasts = FucSppA) %>%
  relocate(year, .after = date) %>%
  fill(c(date, tide_ht), .direction = "down") %>%
  pivot_longer(
    cols = Asco_nCC:Fucu_spp_holdfasts,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = `squares_out_of_25`/25) %>%
  filter(!(seaweed_species %in% c("Asco_recruits",
                                  "Asco_holdfasts",
                                  "Fucu_recruits",
                                  "Fucu_v_holdfasts",
                                  "Fucu_s_holdfasts",
                                  "Fucu_d_holdfasts",
                                  "Fucu_spp_holdfasts"
                                  )))
```

``` r
inverts_mdr_2017 <- horizontal_MDR_NeCSA_2017 %>%
  select(-c(Site,Name, AscoCC:AscoBladders, CarmaeMale, CarmaeFem, CarmaeEgg, HemiMale, HemiFem, HemiEgg, CanirrMale:CanirrEgg, CanborMale:SembalA2)) %>%
  add_column(year = "2017") %>%
  rename(date = Date,
         tide_ht = Tide,
         quadrat_m = `Quadrat (m)`,
         Litt_l = Llitt,
         Litt_o = Liobtu,
         Litt_s = Lsax,
         Litt_spp = `Litt. Spp.`,
         Nuce_l = Nlap,
         Lacu_v = Lacvinc,
         Hiat_a = Hiatarct,
         Ostr_e = Ostredu,
         Myti_e = Myted,
         Modi_m = Modmod,
         Tect_t = Testtest,
         Crep_f = Crepfor,
         Urti_f = Urtfel,
         Metr_s = Metrsen,
         Aste_f = Astfor,
         Aste_v = Astvul,
         Stro_d = Strongdro,
         Lepi_s = Lepsqua,
         Dide_v = Didvex,
         Botr_s = Botrysch,
         Cion_i = Ciointes,
         Gamm_spp = Gamarus,
         Carc_m = CarmaeTot,
         Hemi_s = HemiTot,
         Canc_i = CanirrTot,
         Canc_b = CanborTot) %>%
  pivot_longer(
     cols = Litt_l:Canc_b,
     names_to = "invert_species",
     values_to = "count"
     ) %>%
  relocate(year, .after = date)
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
horizontal_MDR_NeCSA_2019  <- read_excel("/cloud/project/data/2019_horizontal_MDR_NeCSA.xlsx")
```

``` r
seaweeds_mdr_2019 <- horizontal_MDR_NeCSA_2019 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Urti_f:Semi_bA2)) %>%
  add_column(year = "2019") %>%
  pivot_longer(
    cols = Asco_nCC:Cera_rSC,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = squares_out_of_25/25) %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Asco_n_ht = Asco_nHt,
         Asco_n_bladders = Asco_nBladders) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
inverts_mdr_2019 <- horizontal_MDR_NeCSA_2019 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Asco_nCC:Fucu_dCC, Asco_nSC:Cera_rSC, Asco_nHt, Asco_nBladders, Myti_eMethod)) %>%
  add_column(year = "2019") %>%
  rename(date = Date,
      tide_ht = TideHt,
      quadrat_number = Quadrat,
      Nuce_l = Nuci_l) %>%
   pivot_longer(
     cols = Urti_f:Semi_bA2,
     names_to = "invert_species",
     values_to = "count"
   ) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
horizontal_MDR_NeCSA_2020  <- read_excel("/cloud/project/data/2020_horizontal_MDR_NeCSA.xlsx")
```

``` r
seaweeds_mdr_2020 <- horizontal_MDR_NeCSA_2020 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Urti_f:Notes ))  %>%
  add_column(year = "2020") %>%
  pivot_longer(
    cols = Asco_nCC:Cera_rSC,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = squares_out_of_25/25) %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Asco_n_ht = Asco_nHt,
         Asco_n_bladders = Asco_nBladders) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
inverts_mdr_2020 <- horizontal_MDR_NeCSA_2020 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Asco_nCC:Fucu_dCC, Asco_nSC:Cera_rSC, Asco_nHt, Asco_nBladders, Myti_eMethod)) %>%
  add_column(year = "2020") %>%
  rename(date = Date,
      tide_ht = TideHt,
      quadrat_number = Quadrat,
      Nuce_l = Nuci_l) %>%
   pivot_longer(
     cols = Urti_f:Semi_bA2,
     names_to = "invert_species",
     values_to = "count"
   ) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
horizontal_MDR_NeCSA_2021  <- read_excel("/cloud/project/data/2021_horizontal_MDR_NeCSA.xlsx")
```

``` r
seaweeds_mdr_2021 <- horizontal_MDR_NeCSA_2021 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Urti_f:Notes ))  %>%
  add_column(year = "2021") %>%
  pivot_longer(
    cols = Asco_nCC:Cera_rSC,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = squares_out_of_25/25) %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Asco_n_ht = Asco_nHt,
         Asco_n_bladders = Asco_nBladders) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
inverts_mdr_2021 <- horizontal_MDR_NeCSA_2021 %>%
  select(-c(Site, Name, Notes, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Asco_nCC:Fucu_dCC, Asco_nSC:Cera_rSC, Asco_nHt, Asco_nBladders, Myti_eMethod)) %>%
  add_column(year = "2021") %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Nuce_l = Nuci_l) %>%
   pivot_longer(cols = Urti_f:Semi_bA2,
                names_to = "invert_species",
                values_to = "count"
                ) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)

# need to specify value/NA for quadrat_m if value in quadrat_number doesn't match list?
```

``` r
horizontal_MDR_NeCSA_2022  <- read_excel("/cloud/project/data/2022_horizontal_MDR_NeCSA.xlsx")
```

``` r
seaweeds_mdr_2022 <- horizontal_MDR_NeCSA_2022 %>%
  select(-c(Site, Name, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Urti_f:Notes)) %>%
  add_column(year = "2022") %>%
  pivot_longer(
    cols = Asco_nCC:Cera_rSC,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = squares_out_of_25/25) %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
inverts_mdr_2022 <- horizontal_MDR_NeCSA_2022 %>%
  select(-c(Site, Name, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Asco_nCC:Fucu_dCC, Asco_nSC:Cera_rSC, Asco_nHt, Asco_nBladders, Myti_eMethod, Notes)) %>%
  add_column(year = "2022") %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Nuce_l = Nuci_l) %>%
  pivot_longer(
    cols = Urti_f:Semi_bA2,
    names_to = "invert_species",
    values_to = "count") %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
horizontal_MDR_NeCSA_2023  <- read_excel("/cloud/project/data/2023_horizontal_MDR_NeCSA.xlsx")
```

``` r
seaweeds_mdr_2023 <- horizontal_MDR_NeCSA_2023 %>%
  select(-c(Site, Name, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Urti_f:Notes)) %>%
  add_column(year = "2023") %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat) %>%
  pivot_longer(
    cols = Asco_nCC:Cera_rSC,
    names_to = "seaweed_species",
    values_to = "squares_out_of_25") %>%
  mutate(proportion = squares_out_of_25/25) %>%
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
inverts_mdr_2023 <- horizontal_MDR_NeCSA_2023 %>%
  select(-c(Site, Name, Semi_bCC, Myti_eCC, Semi_bSC, Myti_eSC, Asco_nCC:Fucu_dCC, Asco_nSC:Cera_rSC, Asco_nHt, Asco_nBladders, Myti_eMethod, Notes)) %>%
  add_column(year = "2023") %>%
  rename(date = Date,
         tide_ht = TideHt,
         quadrat_number = Quadrat,
         Nuce_l = Nuci_l) %>%
  pivot_longer(
    cols = Urti_f:Semi_bA2,
    names_to = "invert_species",
    values_to = "count") %>% 
  relocate(year, .after = date) %>%
  mutate(quadrat_m = case_when(quadrat_number == 1 ~ 0,
                               quadrat_number == 2 ~ 3,
                               quadrat_number == 3 ~ 6,
                               quadrat_number == 4 ~ 9,
                               quadrat_number == 5 ~ 12,
                               quadrat_number == 6 ~ 15,
                               quadrat_number == 7 ~ 18,
                               quadrat_number == 8 ~ 21,
                               quadrat_number == 9 ~ 24,
                               quadrat_number == 10 ~ 27),
         .after = quadrat_number)
```

``` r
# missing 2018

all_seaweeds_mdr <- full_join(seaweeds_mdr_2017, seaweeds_mdr_2019) %>%
  full_join(seaweeds_mdr_2020) %>%
  full_join(seaweeds_mdr_2021) %>%
  full_join(seaweeds_mdr_2022) %>%
  full_join(seaweeds_mdr_2023) %>%
  mutate(tide_ht = as.factor(tide_ht)) %>%
  mutate(tide_ht = fct_relevel(tide_ht, c("H", "M", "L")))
```

    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, Asco_n_ht,
    ## Asco_n_bladders, seaweed_species, squares_out_of_25, proportion)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, Asco_n_ht,
    ## Asco_n_bladders, seaweed_species, squares_out_of_25, proportion,
    ## quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, Asco_n_ht,
    ## Asco_n_bladders, seaweed_species, squares_out_of_25, proportion,
    ## quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, seaweed_species,
    ## squares_out_of_25, proportion, quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, seaweed_species,
    ## squares_out_of_25, proportion, quadrat_number, Asco_nHt, Asco_nBladders)`

``` r
all_seaweeds_mdr <- all_seaweeds_mdr %>%
  mutate(seaweed_species = case_when(seaweed_species == "Coro_oSC" ~ "Cora_oSC",
                                     TRUE ~ seaweed_species))

distinct(all_seaweeds_mdr, seaweed_species) %>%
  arrange(seaweed_species)
```

    ## # A tibble: 18 × 1
    ##    seaweed_species
    ##    <chr>          
    ##  1 Asco_nCC       
    ##  2 Asco_nSC       
    ##  3 Cera_rSC       
    ##  4 Chon_cSC       
    ##  5 Cora_oSC       
    ##  6 Fucu_dCC       
    ##  7 Fucu_dSC       
    ##  8 Fucu_sCC       
    ##  9 Fucu_sSC       
    ## 10 Fucu_spp       
    ## 11 Fucu_vCC       
    ## 12 Fucu_vSC       
    ## 13 Lami_spp       
    ## 14 Mast_sSC       
    ## 15 Porp_sp        
    ## 16 Ulva_iSC       
    ## 17 Ulva_lSC       
    ## 18 Vert_lSC

``` r
expanded_seaweeds_mdr <- expand(all_seaweeds_mdr, year, tide_ht, quadrat_m, seaweed_species) %>%
  left_join(all_seaweeds_mdr,
            by = join_by(year, tide_ht, quadrat_m, seaweed_species)) %>%
  mutate(squares_out_of_25 = replace_na(squares_out_of_25, 0)) %>%
  mutate(proportion = replace_na(proportion, 0))

write_csv(expanded_seaweeds_mdr, "/cloud/project/analysis/expanded_seaweeds_mdr.csv")
```

``` r
# missing 2018

all_inverts_mdr <- full_join(inverts_mdr_2017, inverts_mdr_2019) %>%
  full_join(inverts_mdr_2020) %>%
  full_join(inverts_mdr_2021) %>%
  full_join(inverts_mdr_2022) %>%
  full_join(inverts_mdr_2023) %>%
  filter(!(invert_species %in% c("Semi_bA1",
                                 "Semi_bA2",
                                 "Semi_bR1",
                                 "Semi_bR2"))) %>%
  mutate(tide_ht = as.factor(tide_ht)) %>%
  mutate(tide_ht = fct_relevel(tide_ht, c("H", "M", "L")))
```

    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, invert_species,
    ## count)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, invert_species,
    ## count, quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, invert_species,
    ## count, quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, invert_species,
    ## count, quadrat_number)`
    ## Joining with `by = join_by(date, year, tide_ht, quadrat_m, invert_species,
    ## count, quadrat_number)`

``` r
glimpse(all_inverts_mdr)
```

    ## Rows: 4,666
    ## Columns: 7
    ## $ date           <dttm> 2017-08-12, 2017-08-12, 2017-08-12, 2017-08-12, 2017-0…
    ## $ year           <chr> "2017", "2017", "2017", "2017", "2017", "2017", "2017",…
    ## $ tide_ht        <fct> L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L…
    ## $ quadrat_m      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ invert_species <chr> "Litt_l", "Litt_o", "Litt_s", "Litt_spp", "Nuce_l", "La…
    ## $ count          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ quadrat_number <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

``` r
distinct(all_inverts_mdr, invert_species) %>%
  arrange(invert_species)
```

    ## # A tibble: 28 × 1
    ##    invert_species
    ##    <chr>         
    ##  1 Aste_f        
    ##  2 Aste_r        
    ##  3 Aste_v        
    ##  4 Botr_s        
    ##  5 Canc_b        
    ##  6 Canc_i        
    ##  7 Carc_m        
    ##  8 Cion_i        
    ##  9 Crep_f        
    ## 10 Dide_sp       
    ## # ℹ 18 more rows

``` r
expanded_inverts_mdr <- expand(all_inverts_mdr,
                               year,
                               tide_ht,
                               quadrat_m,
                               invert_species) %>%
  left_join(all_inverts_mdr, by = join_by(year,
                                          tide_ht,
                                          quadrat_m,
                                          invert_species)) %>%
  mutate(count = replace_na(count, 0))

write_csv(expanded_inverts_mdr, "/cloud/project/analysis/expanded_inverts_mdr.csv")
```

``` r
MDR_temperature_20160615_20170812  <- read_excel("/cloud/project/data/MDR_temperature_20160615_20170812.xlsx")

MDR_temperature_20170812_20180811  <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20170812-20180811")

MDR_temperature_20180812_20190905  <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20180812-20190905")
```

    ## Warning: Expecting logical in G9328 / R9328C7: got 'Logged'

    ## Warning: Expecting logical in G9330 / R9330C7: got 'Logged'

    ## Warning: Expecting logical in G9333 / R9333C7: got 'Logged'

    ## Warning: Expecting logical in G9335 / R9335C7: got 'Logged'

    ## Warning: Expecting logical in G9338 / R9338C7: got 'Logged'

``` r
MDR_temperature_20210602_20211008  <- read_excel("/cloud/project/data/MDR_temperature_2017_2021.xlsx", sheet = "20210602-20211008WC")
```

    ## Warning: Expecting logical in G3086 / R3086C7: got 'Logged'

    ## Warning: Expecting logical in H3087 / R3087C8: got 'Logged'

    ## Warning: Expecting logical in I3088 / R3088C9: got 'Logged'

    ## Warning: Expecting logical in J3088 / R3088C10: got 'Logged'

``` r
MDR_temperature_20211008_20220806 <-
  read_excel("/cloud/project/data/MDR_temperature_WC_2022.xlsx")
```

    ## New names:
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...7`
    ## • `` -> `...8`

``` r
#tidy MDR_temperature_2016_2017
MDR_temperature_2016_2017 <- MDR_temperature_20160615_20170812 %>%
  rename(Date_Time_GMT400 = "Date Time, GMT-04:00",
         "temp" = "Temp, 蚌 (LGR S/N: 10930407, SEN S/N: 10930407)") %>%
  mutate("Date_Time_GMT400" = as.character(Date_Time_GMT400)) %>%
  separate_wider_delim("Date_Time_GMT400", delim = " ", names = c("date", "time"),
    too_few = "debug",
    too_many = "debug") %>%
   mutate(year = year(date)) %>%
  mutate(time = replace_na(time, "00:00:00")) %>%
  select(c(date, time, temp, year))
```

    ## Warning: Debug mode activated: adding variables `Date_Time_GMT400_ok`,
    ## `Date_Time_GMT400_pieces`, and `Date_Time_GMT400_remainder`.

``` r
#tidy MDR_temperature_2017_2018
MDR_temperature_2017_2018 <- MDR_temperature_20170812_20180811 %>%
  rename(time = "Time, GMT-04:00",
         temp = '"Temp °F (LGR S/N: 10930407 SEN S/N: 10930407)"',
         date = Date) %>%
  select(c(date, 
            time,
            "AM/PM",
            temp)) %>%
  mutate(year = year(date)) %>%
  mutate(date = as.character(date)) %>%
    separate_wider_delim(time, delim = " ", names = c("delete", "time1"),
    too_few = "debug",
    too_many = "debug") %>%
  select(-delete, -time, -time_ok, -time_pieces, -time_remainder, time = time1, test = 'AM/PM') %>%
  mutate(time_mil = case_when(time == "12:00:00" & test == "AM" ~ "00:00:00",
                              time == "01:00:00" & test == "AM" ~ "01:00:00",
                              time == "02:00:00" & test == "AM" ~ "02:00:00",
                              time == "03:00:00" & test == "AM" ~ "03:00:00",
                              time == "04:00:00" & test == "AM" ~ "04:00:00",
                              time == "05:00:00" & test == "AM" ~ "05:00:00",
                              time == "06:00:00" & test == "AM" ~ "06:00:00",
                              time == "07:00:00" & test == "AM" ~ "07:00:00",
                              time == "08:00:00" & test == "AM" ~ "08:00:00",
                              time == "09:00:00" & test == "AM" ~ "09:00:00",
                              time == "10:00:00" & test == "AM" ~ "10:00:00",
                              time == "11:00:00" & test == "AM" ~ "11:00:00",
                              time == "12:00:00" & test == "PM" ~ "12:00:00",
                              time == "01:00:00" & test == "PM" ~ "13:00:00",
                              time == "02:00:00" & test == "PM" ~ "14:00:00",
                              time == "03:00:00" & test == "PM" ~ "15:00:00",
                              time == "04:00:00" & test == "PM" ~ "16:00:00",
                              time == "05:00:00" & test == "PM" ~ "17:00:00",
                              time == "06:00:00" & test == "PM" ~ "18:00:00",
                              time == "07:00:00" & test == "PM" ~ "19:00:00",
                              time == "08:00:00" & test == "PM" ~ "20:00:00",
                              time == "09:00:00" & test == "PM" ~ "21:00:00",
                              time == "10:00:00" & test == "PM" ~ "22:00:00",
                              time == "11:00:00" & test == "PM" ~ "23:00:00")) %>%
  select(-time, -test) %>%
  rename(time = time_mil)
```

    ## Warning: Debug mode activated: adding variables `time_ok`, `time_pieces`, and
    ## `time_remainder`.

``` r
#tidy MDR_temperature_2018_2019
MDR_temperature_2018_2019 <- MDR_temperature_20180812_20190905 %>%
  rename(time = "Time, GMT-04:00",
         temp = "Temp, °F (LGR S/N: 10930407, SEN S/N: 10930407)",
         date = Date) %>%
  select(c(date, 
            time,
            "AM/PM",
            temp)) %>%
  mutate(year = year(date)) %>%
  mutate(date = as.character(date)) %>%
    separate_wider_delim(time, delim = " ", names = c("delete", "time1"),
    too_few = "debug",
    too_many = "debug") %>%
  select(-delete, -time, -time_ok, -time_pieces, -time_remainder, time = time1, test = 'AM/PM') %>%
  mutate(time_mil = case_when(time == "12:00:00" & test == "AM" ~ "00:00:00",
                              time == "01:00:00" & test == "AM" ~ "01:00:00",
                              time == "02:00:00" & test == "AM" ~ "02:00:00",
                              time == "03:00:00" & test == "AM" ~ "03:00:00",
                              time == "04:00:00" & test == "AM" ~ "04:00:00",
                              time == "05:00:00" & test == "AM" ~ "05:00:00",
                              time == "06:00:00" & test == "AM" ~ "06:00:00",
                              time == "07:00:00" & test == "AM" ~ "07:00:00",
                              time == "08:00:00" & test == "AM" ~ "08:00:00",
                              time == "09:00:00" & test == "AM" ~ "09:00:00",
                              time == "10:00:00" & test == "AM" ~ "10:00:00",
                              time == "11:00:00" & test == "AM" ~ "11:00:00",
                              time == "12:00:00" & test == "PM" ~ "12:00:00",
                              time == "01:00:00" & test == "PM" ~ "13:00:00",
                              time == "02:00:00" & test == "PM" ~ "14:00:00",
                              time == "03:00:00" & test == "PM" ~ "15:00:00",
                              time == "04:00:00" & test == "PM" ~ "16:00:00",
                              time == "05:00:00" & test == "PM" ~ "17:00:00",
                              time == "06:00:00" & test == "PM" ~ "18:00:00",
                              time == "07:00:00" & test == "PM" ~ "19:00:00",
                              time == "08:00:00" & test == "PM" ~ "20:00:00",
                              time == "09:00:00" & test == "PM" ~ "21:00:00",
                              time == "10:00:00" & test == "PM" ~ "22:00:00",
                              time == "11:00:00" & test == "PM" ~ "23:00:00")) %>%
  select(-time, -test) %>%
  rename(time = time_mil)
```

    ## Warning: Debug mode activated: adding variables `time_ok`, `time_pieces`, and
    ## `time_remainder`.

``` r
#tidy MDR_temperature_2021
MDR_temperature_2021 <- MDR_temperature_20210602_20211008 %>%
  rename(time = "Time, GMT-04:00",
         temp = "Temp, °F (LGR S/N: 20911608, SEN S/N: 20911608)",
         date = Date) %>%
  select(c(date, 
            time,
            "AM/PM",
            temp)) %>%
  mutate(year = year(date)) %>%
  mutate(date = as.character(date)) %>%
    separate_wider_delim(time, delim = " ", names = c("delete", "time1"),
    too_few = "debug",
    too_many = "debug") %>%
  select(-delete, -time, -time_ok, -time_pieces, -time_remainder, time = time1, test = 'AM/PM') %>%
  mutate(time_mil = case_when(time == "12:00:00" & test == "AM" ~ "00:00:00",
                              time == "01:00:00" & test == "AM" ~ "01:00:00",
                              time == "02:00:00" & test == "AM" ~ "02:00:00",
                              time == "03:00:00" & test == "AM" ~ "03:00:00",
                              time == "04:00:00" & test == "AM" ~ "04:00:00",
                              time == "05:00:00" & test == "AM" ~ "05:00:00",
                              time == "06:00:00" & test == "AM" ~ "06:00:00",
                              time == "07:00:00" & test == "AM" ~ "07:00:00",
                              time == "08:00:00" & test == "AM" ~ "08:00:00",
                              time == "09:00:00" & test == "AM" ~ "09:00:00",
                              time == "10:00:00" & test == "AM" ~ "10:00:00",
                              time == "11:00:00" & test == "AM" ~ "11:00:00",
                              time == "12:00:00" & test == "PM" ~ "12:00:00",
                              time == "01:00:00" & test == "PM" ~ "13:00:00",
                              time == "02:00:00" & test == "PM" ~ "14:00:00",
                              time == "03:00:00" & test == "PM" ~ "15:00:00",
                              time == "04:00:00" & test == "PM" ~ "16:00:00",
                              time == "05:00:00" & test == "PM" ~ "17:00:00",
                              time == "06:00:00" & test == "PM" ~ "18:00:00",
                              time == "07:00:00" & test == "PM" ~ "19:00:00",
                              time == "08:00:00" & test == "PM" ~ "20:00:00",
                              time == "09:00:00" & test == "PM" ~ "21:00:00",
                              time == "10:00:00" & test == "PM" ~ "22:00:00",
                              time == "11:00:00" & test == "PM" ~ "23:00:00")) %>%
  select(-time, -test) %>%
  rename(time = time_mil)
```

    ## Warning: Debug mode activated: adding variables `time_ok`, `time_pieces`, and
    ## `time_remainder`.

``` r
#tidy MDR_temperature_20211008_20220806
#no date column... ?? (date and time are combined into 1 column)

MDR_temperature_2022 <- MDR_temperature_20211008_20220806 %>%
  row_to_names(row_number = 1) %>%
  mutate(datetime = convert_to_datetime(`Date Time, GMT-04:00`)) %>%
  rename(temp = "Temp, °F (LGR S/N: 20911608, SEN S/N: 20911608)") %>%
  select(c(datetime,
            temp)) %>%
  mutate(year = year(datetime)) %>%
  separate(datetime, c("date", "time"), sep = " ") %>%
  mutate(date = as.character(date)) %>%
  mutate(temp = as.numeric(temp)) %>%
  drop_na(temp)
  
mdr_temp_allyears <- rbind(MDR_temperature_2016_2017,
                           MDR_temperature_2017_2018,
                           MDR_temperature_2018_2019,
                           MDR_temperature_2021,
                           MDR_temperature_2022)

mdr_temp_allyears <- mdr_temp_allyears %>%
  mutate(date = as_date(ymd(date))) %>%
  unite(date_time, c(date, time), sep = "", remove = FALSE) %>%
  mutate(date_time = as_datetime(ymd_hms(date_time)), tz = "EST") %>%
  relocate(date_time, .after = time) %>%
  relocate(year, .before = date) %>%
  mutate(temp_C = (5/9) * (temp - 32)) %>%
  mutate(date = as.character(date))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `date_time = as_datetime(ymd_hms(date_time))`.
    ## Caused by warning:
    ## !  17 failed to parse.

``` r
write_csv(mdr_temp_allyears, "/cloud/project/analysis/mdr_temp_allyears.csv")
```

``` r
tide_verified_2017 <-read.csv("/cloud/project/data/2017_tide_times.csv")

tide_verified_2018 <-read.csv("/cloud/project/data/2018_tide_times.csv")

tide_verified_2019 <-read.csv("/cloud/project/data/2019_tide_times.csv")

tide_verified_2020 <-read.csv("/cloud/project/data/2020_tide_times.csv")

tide_verified_2021 <-read.csv("/cloud/project/data/2021_tide_times.csv")

tide_verified_2022 <-read.csv("/cloud/project/data/2022_tide_times.csv")

tide_verified_2023 <-read.csv("/cloud/project/data/2023_tide_times.csv")
```

``` r
tide_time_2017 <- tide_verified_2017 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")


tide_time_2018 <- tide_verified_2018 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")

tide_time_2019 <- tide_verified_2019 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")

tide_time_2020 <- tide_verified_2020 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")

tide_time_2021 <- tide_verified_2021 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")

tide_time_2022 <- tide_verified_2022 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")

tide_time_2023 <- tide_verified_2023 %>%
  select(Date, Time..GMT., Verified..m.) %>%
  filter(Verified..m. != "-") %>%
  rename(date = "Date",
         time = "Time..GMT.",
         tide_height = "Verified..m.")
```

``` r
all_tide_time <- full_join(tide_time_2017, tide_time_2018) %>%
  full_join(tide_time_2019) %>%
  full_join(tide_time_2020) %>%
  full_join(tide_time_2021) %>%
  full_join(tide_time_2022) %>%
  full_join(tide_time_2023) %>%
  mutate(tide_ht = case_when(tide_height < 1 ~ "L",
                             tide_height > 1 ~ "H"),
         .after = tide_height) %>%
  mutate(date = str_replace_all(date, "/", "-")) %>%
  mutate(date_time = paste(date, time, sep = " "))
```

    ## Joining with `by = join_by(date, time, tide_height)`
    ## Joining with `by = join_by(date, time, tide_height)`
    ## Joining with `by = join_by(date, time, tide_height)`
    ## Joining with `by = join_by(date, time, tide_height)`
    ## Joining with `by = join_by(date, time, tide_height)`
    ## Joining with `by = join_by(date, time, tide_height)`

``` r
write_csv(all_tide_time, "/cloud/project/analysis/all_tide_time.csv")
```

``` r
#by <- mdr_temp_allyears
  #join_by(closest(date_time >= date_time)) %>%
 # left_join(all_tide_time)
```
