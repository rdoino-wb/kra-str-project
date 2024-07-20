***Pilot randomization***
***5 Feb 2024 - Chris Hoy***

global root "/Users/ruggerodoino/Desktop"
global myProject "${root}/Reproducible_package_kenya/Data"

**Import data**

import delimited "${myProject}/Intermediate/dt_pilot.csv", clear

**Additional coding of variables**

encode station_name, gen(station_name_num)

encode major_profession, gen(major_profession_num)

gen taxregion_mean_monthly_tot_rd = round(taxregion_mean_monthly_tot_paid) 

**Randomization**

randtreat, generate(randomized_group) setseed(123456789) strata(station_name_num decile_within_taxregion_pay always_filer) unequal(4/5 1/20 1/20 1/20 1/20)  misfits(wglobal)

**Export file**

export delimited tax_payer_id randomized_group using "${myProject}/Intermediate/pilot.csv", replace



















