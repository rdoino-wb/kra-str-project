***Pilot randomization***
***5 Feb 2024 - Chris Hoy***

**Import data**

global root "/Users/ruggerodoino/Desktop"
global myProject "${root}/Reproducible_package_kenya/Data"

import delimited "$myProject/Final/Pilot/matched_data_wide_pilot.csv", clear

***ONLY KEEP CONTROL GROUP FROM PILOT, WE'RE NOT INTERVENING WITH FIRMS THT ALREADY RECEIVED SMS
keep if group == "Group 0"

**Additional coding of variables**
encode station_name, gen(station_name_num)

*****DROP FIRMS IN TOP DECILE WITHIN REGION, THEY WILL NOT BE INCLUDED IN THE EXPERIMENT
drop if decile_within_taxregion_pay == 10

**Randomization**

randtreat, generate(randomized_group) setseed(12345678) strata(station_name_num decile_within_taxregion_pay always_filer) ///
	unequal(3/10 2/10 3/10  2/10)  misfits(wglobal)

export delimited using "$myProject/Final/Experiment/scale_up.csv" , replace
