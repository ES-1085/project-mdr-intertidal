# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## inverts_mdr_20??.xlsx

- `Date`: date survey was conducted
- `TideHt`: tide height at which quadrat was placed; options are as follows:
      H: high
      M: medium
      L: low
- `invert_species`: species of invertebrate (Latin name); species codes are as follows:
      Urti_f: Urticina felina (northern red anemone)
      Metr_s: Metridium senile (frilled anemone)
      Ostr_e: Ostrea edulis (European flat oyster)
      Dide_sp: Didemnum sp. (colonial tunicate)
      Myti_e: Mytilus edulis (blue mussel)
      Botr_s: Botryllus schlosseri (golden star colonial tunicate)
      Nuce_l: Nucella lapillus (dog whelk)
      Litt_l: Littorina littorea (common periwinkle)
      Litt_o: Littorina obtusata (smooth periwinkle)
      Litt_s: Littorina saxatilis (rough periwinkle)
      Lacu_v: Lacuna vincta (northern lacuna snail)
      Tect_t: Tectura testudinalis, now Testundinalia testudinalis (tortoiseshell limpet)
      Stro_d: Strongylocentrotus droebachiensis (green sea urchin)
      Aste_f: Asterias forbesi (Forbes sea star)
      Aste_r: Asterias rubens (common sea star)
      Lepi_s: Lepidonotus squamatus (twelve-scaled worm)
      Carc_m: Carcinus maenas (European green crab)
      Hemi_s: Hemigrapsus sanguineus (Asian shore crab)
      Canc_i: Cancer irroratus (rock crab)
      Canc_b: Cancer borealis (Jonah crab)
-`count`: number of individuals of each species counted in the quadrat

## inverts_mdr_20??.xlsx

- `Date`: date survey was conducted
- `TideHt`: tide height at which quadrat was placed; options are as follows:
      H: high
      M: medium
      L: low
- `Quadrat_m`: quadrat's starting point in transect (measured in meters from start of transect)
- `Asco_nHt`: length of longest blade of Ascophyllum nodosum in the quadrat, measured in centimeters
- `Asco_nBladders`: number of air bladders on longest blade of Ascophyllum nodosum in the quadrat (one bladder represents one year of growth)
- `year`: year in which survey was conducted
- `seaweed_species`: species of seaweed (Latin name); suffix indicates canopy cover (CC) or subcanopy (SC); species codes are as follows:
      Asco_n: Ascophyllum nodosum
      Fucu_v: Fucus vesiculosus
      Fucu_s: Fucus spiralis
      Fucu_d: Fucus distichus
      Fuc_spp: Fucus spp.
      Mast_s: Mastocarpus stellatus
      Chon_c: Chondrus crispus
      Ulva_i: Ulva intestinalis
      Ulva_l: Ulva lactuca
      Cora_o: Corallina officinalis
      Vert_l: Vertebrata lanosa
      Cera_r: Ceramium rubrum
      Lam_spp: Laminaria spp. (unknown kelp)
      Por_sp: Porphyra sp. (unknown laver)
- `proportion`: number of squares within quadrat where species was present, divided by 25