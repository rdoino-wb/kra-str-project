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
  scale_up    = fread(file.path(fin_data, "Experiment/scale_up.csv"))
  
  # Load payments dataset
  data_payments = fread(file.path(fin_data, "dt_payments_collapsed.csv"))
  
  # Load reports sms
  sms_report = fread(file.path(raw_data, "sms_delivery_report/sms_delivery_15_04_2024.csv"))
  
  scale_up = haven::read_dta("/Users/ruggerodoino/Library/CloudStorage/Dropbox/Kenya/Data/Final/Experiment/ScaleUp_RandomizedSample.dta")
  
}

# 2. Clean the data base for the pilot ----

{
  
  # This will create a dataset with unique identifier monthly level dataset
  data = CJ(tax_payer_id = unique(scale_up$tax_payer_id),
            period = c(
              "2023-10", 
              "2023-11",
              "2023-12", 
              "2024-1", 
              "2024-2",
              "2024-3",
              "2024-4"))
  
  setorder(sample, dt_year)
  info = copy(sample[dt_year >= 2022])[, 
                                       period := paste0(dt_year, "-", dt_month)
  ][, .(dummy_filing = .N,
        sum_filing   = sum(net_tax)), 
    by = c("tax_payer_id", "period")]
  data = left_join(data, info[, c("tax_payer_id", "dummy_filing", "sum_filing", "period")]  , by = c("tax_payer_id", "period"))
  data[is.na(dummy_filing), dummy_filing := 0]
  data[is.na(sum_filing)  , sum_filing   := 0]
  dt_taxpayers = sample[dt_year >= 2022 & dt_year < 2024 & !is.na(tax_due)]
  
  info_2 = copy(dt_taxpayers)[, 
                              .(brackets_tot = last(brackets_tot)), 
                              by = c("tax_payer_id", "station_name")]
  data = left_join(data, info_2[, c("tax_payer_id", "station_name", "brackets_tot")], by = c("tax_payer_id"))
  data = left_join(data, scale_up[, c("tax_payer_id", "randomized_group")])
  
  # Filter 'data_payments' for the year 2023 and months 10, 11, and 12.
  data_payments = data_payments[
    (dt_year == 2023 &
       (dt_month == 10 | dt_month == 11 | dt_month == 12)) | 
      (dt_year == 2024 & dt_month != 5)
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
                    id.vars = c("tax_payer_id", "randomized_group", "brackets_tot", "station_name", "period"), 
                    measure.vars = c("tot_payments", "dummy_payment", "dummy_filing", "sum_filing"))
  
  # Cast the data back to wide format
  data_wide <- dcast(data_long, 
                     tax_payer_id + randomized_group + brackets_tot + station_name ~ period + variable, 
                     value.var = "value")
  
  setnames(data_wide, 
           old = names(data_wide)[5:ncol(data_wide)], 
           new = sub("^(\\d{4}-\\d{1,2})_(.*)$", "\\2_\\1", names(data_wide)[5:ncol(data_wide)]))
  
  setDT(data_wide)
  
  # Replace NA values in 'tot_payments' columns for each month with 0.
  # This step handles missing data by assuming no payments were made where data is missing.
  # Assuming 'dt' is your data.table
  columns_to_replace <- grep("tot_payments_(2023-(10|11|12)|2024-(1|2|3|4))", names(data_wide), value = TRUE)
  
  # Replace NA values with zero in the specified columns
  data_wide[, (columns_to_replace) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = columns_to_replace]
  setnames(data_wide, names(data_wide), gsub("-", "_", names(data_wide)))
  
  data_wide = left_join(data_wide, scale_up[,
                                            c(
                                              "tax_payer_id",                    
                                              "taxpayer_mean_monthly_tot_paid",  
                                              "taxregion_mean_monthly_tot_paid",
                                              "decile_within_taxregion_pay",     
                                              "always_filer_2023",               
                                              "zero_filer_2023",                
                                              "tot_obligation_began_2023",      
                                              "paid_atleast_once_2023",         
                                              "months_since_registration",      
                                              "n_files",                         
                                              "always_filer" 
                                            )])
  
  # We need to do one correction for the way we compute the payments in the end of 2023
  data_wide[is.na(taxpayer_mean_monthly_tot_paid), taxpayer_mean_monthly_tot_paid := 0]
  
  sms_report = sms_report[Status == "Delivered"]
  data_wide[, sms_delivery := tax_payer_id %in% sms_report$`tax payer ID`]
  
}

# 3. Clean for analysis regressions ----

{
  
  # Copy the former dataset to modify it for the analysis
  matched_data <- copy(data_wide)
  
  # Ensure 'group_num' exists
  if (!"group_num" %in% colnames(matched_data)) {
    matched_data[, group_num := as.integer(factor(randomized_group))]
  }
  matched_data[, randomized_group := NULL]  # Remove 'randomized_group' column
  
  # Filter data
  matched_data <- matched_data[decile_within_taxregion_pay != 10 | is.na(decile_within_taxregion_pay)]
  
  # Define variables for log transformation
  vars_to_log <- c(paste0("tot_payments_2023_", 10:12), paste0("tot_payments_2024_", 1:4),
                   paste0("sum_filing_2023_", 10:12), paste0("sum_filing_2024_", 1:4))
  log_vars <- paste0("log_", vars_to_log)
  
  # Perform log transformation and additional calculations
  matched_data[, (log_vars) := lapply(.SD, asinh), .SDcols = vars_to_log]
  matched_data[, l_taxpayer_mean_monthly_tot_paid := asinh(taxpayer_mean_monthly_tot_paid)]
  
  # Create treatment-related variables
  matched_data[, `:=`(
    basic = fcase(group_num == 2, 1, group_num == 1, 0, default = NA),
    anchor = fcase(group_num == 3, 1, group_num == 1, 0, default = NA),
    anchor_app = fcase(group_num == 4, 1, group_num == 1, 0, default = NA),
    treated = between(group_num, 2, 4),
    anchoring_interventions = fcase(between(group_num, 3, 4), 1, group_num == 1, 0, default = NA))
  ][,
    group := fcase(
      basic == 1, "Reminder", 
      anchor == 1, "Anchor",
      anchor_app == 1, "Anchor + App", 
      default = "Control")]
  
  # Calculate deviations and Winsorize
  matched_data[, `:=`(
    deviation_anchor = (tot_payments_2024_4 - round(taxregion_mean_monthly_tot_paid)) / round(taxregion_mean_monthly_tot_paid),
    deviation_anchor_file = (sum_filing_2024_4 - round(taxregion_mean_monthly_tot_paid)) / round(taxregion_mean_monthly_tot_paid))
  ][,
    `:=`(
      deviation_anchor_w = Winsorize(deviation_anchor, probs = c(0.01, 0.99)),
      deviation_anchor_file_w = Winsorize(deviation_anchor_file, probs = c(0.01, 0.99)),
      below_median = decile_within_taxregion_pay < 5)]
  
  # Remove "Anchor + App" group if necessary
  matched_data <- matched_data[is.na(anchor_app) | anchor_app != 1]
  
}

# 4. Save data ----

{
  
  # Save 
  fwrite(data_wide, file.path(fin_data, "Experiment/matched_data_wide_scale_up.csv"))
  
  # save
  fwrite(matched_data, file.path(fin_data, "Experiment/data_reg_wide_scale_up.csv"))
  
}


