# Input data

The course dataset is not redistributed in this repository. The scripts expect these files in `data/`:

| File | Content |
|---|---|
| `Assignment.csv` | One row per policyholder (163,657 rows) with columns `AGEPH, CODPOSS, duree, lnexpo, nbrtotc, nbrtotan, chargtot, agecar, sexp, fuelc, split, usec, fleetc, sportc, coverp, powerc` |
| `inspost.xls` | Belgian postal codes with columns `INS, COMMUNE, LAT, LONG, CODPOSS` (first sheet) |
| `shapefile/npc96_region_Project1.*` | Shapefile of Belgian postal areas (`.shp`, `.shx`, `.dbf`, `.prj` and companions) with fields `POSTCODE` and `Shape_Area` |

Column meaning: `duree` is the exposure in years, `lnexpo` its log, `nbrtotc` the number of claims, `chargtot` the total claim amount, and the remaining columns are the rating factors described in the main README.
