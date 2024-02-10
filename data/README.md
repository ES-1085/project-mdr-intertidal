# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## Note: the following codebooks apply to the tidied data (see "tidymdrdata.Rmd" file in Cloud > project > extra)

## all_seaweeds_mdr

- `date`: date survey was conducted
- `year`: year survey was conducted
- `tide_ht`: tide height at which quadrat was placed; options are as follows:
      H: high
      M: medium
      L: low
- `quadrat_number`: integer assigned to each 1-meter quadrat; indicates order
- `quadrat_m`: distance of the left edge of each quadrat from the start of the horizontal transect, measured in meters (the left edge of each 1-meter quadrat was placed starting 3 meters from the left of the previous quadrat)
- `Asco_n_ht`: length of longest blade of Ascophyllum nodosum in the quadrat, measured in centimeters
- `Asco_n_bladders`: number of air bladders on longest blade of Ascophyllum nodosum in the quadrat (one bladder represents one year of growth)
- `seaweed_species`: species of seaweed (Latin name); suffix indicates canopy cover (CC) or subcanopy (SC); species codes are as follows:
      Asco_n: Ascophyllum nodosum
      Cera_r: Ceramium rubrum
      Chon_c: Chondrus crispus
      Cora_o: Corallina officinalis
      Fucu_d: Fucus distichus
      Fucu_s: Fucus spiralis
      Fucu_spp: Fucus spp. (unidentified Fucus)
      Fucu_v: Fucus vesiculosus
      Lami_spp: Laminaria spp. (unidentified kelp)
      Mast_s: Mastocarpus stellatus
      Porp_sp: Porphyra sp. (unidentified laver)
      Ulva_i: Ulva intestinalis
      Ulva_l: Ulva lactuca
      Vert_l: Vertebrata lanosa
- `proportion`: number of squares within quadrat where species was present, divided by 25

## all_inverts_mdr

- `date`: date survey was conducted
- `year`: year survey was conducted
- `tide_ht`: tide height at which quadrat was placed; options are as follows:
      H: high
      M: medium
      L: low
- `quadrat_number`: integer assigned to each 1-meter quadrat; indicates order
- `quadrat_m`: distance of the left edge of each quadrat from the start of the horizontal transect, measured in meters (the left edge of each 1-meter quadrat was placed starting 3 meters from the left of the previous quadrat)
- `invert_species`: species of invertebrate (Latin name); species codes are as follows:
     Aste_f: Asterias forbesi (Forbes sea star)
     Aste_r: Asterias rubens (common sea star)
     Aste_v: SYNONYM FOR Aste_r; Asterias vulgaris (common sea star)
     Botr_s: Botryllus schlosseri (golden star colonial tunicate)
     Canc_b: Cancer borealis (Jonah crab)
     Canc_i: Cancer irroratus (rock crab)
     Carc_m: Carcinus maenas (European green crab)
     Cion_i: Ciona intestinalis (solitary vase tunicate)
     Crep_f: Crepidula fornicata (slipper shell)
     Dide_sp: Didemnum sp. (colonial tunicate)
     Dide_v: Didemnum vexillum (colonial tunicate)
     Gamm_spp: Gammarus spp. (unidentified gammarid amphipod)
     Hemi_s: Hemigrapsus sanguineus (Asian shore crab)
     Hiat_a: Hiatella arctica (arctic rock borer clam)
     Lacu_v: Lacuna vincta (northern lacuna snail)
     Lepi_s: Lepidonotus squamatus (twelve-scaled worm)
     Litt_l: Littorina littorea (common periwinkle)
     Litt_o: Littorina obtusata (smooth periwinkle)
     Litt_s: Littorina saxatilis (rough periwinkle)
     Litt_spp: Littorina spp. (unidentified periwinkle)
     Metr_s: Metridium senile (frilled anemone)
     Modi_m: Modiolus modiolus (horse mussel)
     Myti_e: Mytilus edulis (blue mussel)
     Nuce_l: Nucella lapillus (dog whelk)
     Ostr_e: Ostrea edulis (European flat oyster)
     Stro_d: Strongylocentrotus droebachiensis (green sea urchin)
     Tect_t: Tectura testudinalis, now Testundinalia testudinalis (tortoiseshell limpet)
     Urti_f: Urticina felina (northern red anemone)
-`count`: number of individuals of each species counted in the quadrat
