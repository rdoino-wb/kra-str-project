# ---------------------------------------------------------------------------- #
#                              4 Kenya Tax Project: pilot data                 #
#                                   World Bank - DIME                          #
#                                 .                                             #
# ---------------------------------------------------------------------------- #

# 1. Load Data ----
{
  
  # Load filing tax data
  sample        = fread(file.path(fin_data, "dt_month_tot_pay.csv"))
  
  # Load the list of randomized groups from pilot
  randomized    = fread(file.path(int_data, "pilot.csv"))
  
  # Load payments dataset
  data_payments = fread(file.path(fin_data, "dt_payments_collapsed.csv"))
  
  # Load pilot data
  pilot         = fread(file.path(int_data, "dt_pilot.csv"))
  
}

# 2. Clean the data base for the pilot ----

{
  
  # We need to do one correction for the way we compute the payments in the end of 2023
  setnames(pilot, "taxpayer_mean_monthly_ToT_paid", "taxpayer_mean_monthly_tot_paid")
  pilot[is.na(taxpayer_mean_monthly_tot_paid), taxpayer_mean_monthly_tot_paid := 0]
  
  # We want to compute by geographical area in which of the 10th percentiles the individual taxpayer is placed based on the
  # amount paid for tot in 2023 after June 
  pilot[, 
        decile_within_taxregion_pay := ntile(taxpayer_mean_monthly_tot_paid, 10), 
        by = station_name]
  
  pilot = pilot[, 
                -c(
                  "station_name", 
                  "sector", 
                  "divison", 
                  "group", 
                  "major_profession", 
                  "sub_profession", 
                  "minor_profession")]
  dt_taxpayers = sample[dt_year >= 2022 & dt_year < 2024 & !is.na(tax_due)]
  
  # This will create a dataset with unique identifier monthly level dataset
  data = CJ(tax_payer_id = unique(pilot$tax_payer_id),
            period = c(
              "2023-10", 
              "2023-11",
              "2023-12", 
              "2024-1", 
              "2024-2",
              "2024-3"))
  
  setorder(sample, dt_year)
  info = copy(sample[dt_year >= 2022])[, 
                                       period := paste0(dt_year, "-", dt_month)
  ][, .(dummy_filing = .N,
        sum_filing   = sum(net_tax)), 
    by = c("tax_payer_id", "period")]
  data = left_join(data, info[, c("tax_payer_id", "dummy_filing", "sum_filing", "period")]  , by = c("tax_payer_id", "period"))
  data[is.na(dummy_filing), dummy_filing := 0]
  data[is.na(sum_filing)  , sum_filing   := 0]
  
  info_2 = copy(dt_taxpayers)[, 
                              .(brackets_tot = last(brackets_tot)), 
                              by = c("tax_payer_id", "station_name")]
  data = left_join(data, info_2[, c("tax_payer_id", "station_name", "brackets_tot")], by = c("tax_payer_id"))
  
  # Assign 'Control' to the 'group' column where 'group' is NA.
  # If there is not match with the pre_pilot it means that it is control group
  # This step categorizes unmatched data as 'Control' group in the experiment/study.
  data = left_join(data, randomized)
  data = copy(data)
  data[, group := ifelse(!is.na(randomized_group), paste0("Group ", randomized_group), NA)]
  data[is.na(group), group := "Control"]
  data <- data[, -c("randomized_group")]
  
  # Filter 'data_payments' for the year 2023 and months 10, 11, and 12.
  data_payments = data_payments[
    (dt_year == 2023 &
       (dt_month == 10 | dt_month == 11 | dt_month == 12)) | 
      (dt_year == 2024 & dt_month != 4)
  ]
  
  # Merge 'data' with 'data_payments' on 'tax_payer_id' and 'dt_month'.
  data <- copy(left_join(
    data, 
    data_payments[, period := paste0(dt_year, "-", dt_month)], 
    by = c("tax_payer_id" = "fake_id"  , "period")))
  
  # Create a dummy variable 'dummy_payment' to indicate the presence of payments.# It assigns 1 if 'tot_payments' is not NA  , otherwise 0.
  data[, dummy_payment := fifelse(is.na(tot_payments), 0, 1)]
  
  # Transform the 'data' from long to wide format.
  data_long <- melt(data, 
                    id.vars = c("tax_payer_id", "group", "brackets_tot", "station_name", "period"), 
                    measure.vars = c("tot_payments", "dummy_payment", "dummy_filing", "sum_filing"))
  
  # Cast the data back to wide format
  data_wide <- dcast(data_long, 
                     tax_payer_id + group + brackets_tot + station_name ~ period + variable, 
                     value.var = "value")
  
  # Replace NA values in 'tot_payments' columns for each month with 0.
  # This step handles missing data by assuming no payments were made where data is missing.
  # Assuming 'dt' is your data.table
  columns_to_replace <- grep("(2023-(10|11|12)|2024-(1|2|3))_tot_payments", names(data_wide), value = TRUE)
  
  setDT(data_wide)
  
  # Replace NA values with zero in the specified columns
  data_wide[, (columns_to_replace) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = columns_to_replace]
  setnames(data_wide, names(data_wide), gsub("-", "_", names(data_wide)))
  
  data_wide = left_join(data_wide, pilot)
  
}

{
  
  # Save 
  fwrite(data_wide, file.path(fin_data, "pilot/matched_data_wide_pilot.csv"))
  
}

