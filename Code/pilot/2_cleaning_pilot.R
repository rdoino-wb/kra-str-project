# ---------------------------------------------------------------------------- #
#                              4 Kenya Tax Project: pilot data                 #
#                                   World Bank - DIME                          #
#                                                                              #
# ---------------------------------------------------------------------------- #

# 1. Load Data ----
{
  
  # Load the TOT and Payments data
  data = fread(file.path(fin_data, "Kenya_tot_pay_data.csv"), na.strings = c(NA_character_, ""))
  
}

# 2. Clean the data base for the pilot ----

{
  
  # First, we only select filer (and their files) who filed at least once between 2022 and 2024
  data = data[dt_year >= 2022 & dt_year < 2024]
  
  # We want to create a dummy for cases where firms file zero turnover in all their files in 2023
  data[dt_year == 2023, 
       zero_filer_2023 := fifelse((min(tot_turnover) == 0 & max(tot_turnover) == 0), 1, 0), 
       by = .(tax_payer_id)]
  
  # We want to convert each value of tax due which is equal or lower than zero as missing which will simplify future operations
  data = data[net_tax <= 0, net_tax := NA]
  
  # compute the average monthly payment in 2023 (after the new tax rate) at the geographical level
  data[dt_year == 2023 & dt_month >= 7, 
       taxregion_mean_monthly_ToT_paid := mean(tot_payments, na.rm = TRUE), 
       by = station_name
  ][,
    taxregion_mean_monthly_ToT_paid := mean(taxregion_mean_monthly_ToT_paid, na.rm = TRUE),
    by = station_name]
  
  # compute the average monthly payment in 2023 (after the new tax rate) at the payer level
  data[dt_year == 2023 & dt_month >= 7, 
       taxpayer_mean_monthly_ToT_paid := mean(tot_payments, na.rm = TRUE), 
       by = tax_payer_id
  ][,
    taxpayer_mean_monthly_ToT_paid := mean(taxpayer_mean_monthly_ToT_paid, na.rm = TRUE),
    by = tax_payer_id]
  
  # compute a dummy for filers that file every month in 2023
  data[dt_year == 2023, 
       always_filer_2023 := fifelse(.N == 12, 1, 0),
       by = tax_payer_id][,
                          always_filer_2023 := mean(always_filer_2023, na.rm = TRUE),
                          by = tax_payer_id][is.na(always_filer_2023),
                                             always_filer_2023 := 0]
  
  # compute a dummy for filers that never file in 2023
  data[dt_year == 2023, 
       zero_filer_2023 := fifelse(.N == 0, 1, 0),
       by = tax_payer_id][,
                          zero_filer_2023 := mean(zero_filer_2023, na.rm = TRUE),
                          by = tax_payer_id][
                            is.na(zero_filer_2023),
                            zero_filer_2023 := 1]
  
  # compute a dummy for filers that pay at least once in 2023
  data[dt_year == 2023,
       paid_atleast_once_2023 := fifelse(sum(tot_payments, na.rm = TRUE) != 0, 1, 0),
       by = tax_payer_id][,
                          paid_atleast_once_2023 := mean(paid_atleast_once_2023, na.rm = TRUE),
                          by = tax_payer_id]
  
  # compute a dummy for filers who registered in 2023
  data[,
       ToT_obligation_began_2023 := fifelse(year(obligation_reg_date) == 2023, 1, 0),
       by = tax_payer_id]
  
  # compute the months since the registration 
  data[, months_since_registration := as.integer(
    interval(
      start = obligation_reg_date, 
      end = as.Date("2023-12-31")) / months(1)
  ) + 1]
  
}

# 3. Collapse data ----

{
  
  # We want to move from a file-level dataset to payer-level dataset
  data_collapsed = copy(data)
  data_collapsed = data_collapsed[,
                                  .(
                                    paid_atleast_once_2023          = mean(paid_atleast_once_2023         , na.rm = TRUE),
                                    taxpayer_mean_monthly_ToT_paid  = mean(taxpayer_mean_monthly_ToT_paid , na.rm = TRUE),
                                    taxregion_mean_monthly_ToT_paid = mean(taxregion_mean_monthly_ToT_paid, na.rm = TRUE),
                                    n_files                         = .N
                                  ),
                                  by = .(
                                    tax_payer_id, 
                                    station_name, 
                                    sector, 
                                    divison, 
                                    group, 
                                    major_profession, 
                                    minor_profession, 
                                    sub_profession, 
                                    ToT_obligation_began_2023,
                                    months_since_registration,
                                    zero_filer_2023,
                                    always_filer_2023)]
  
  # We want to create a dummy for whether filers filed always  
  data_collapsed[, always_filer := fcase(
    n_files == 24                                                        , 1,  
    is.na(months_since_registration) & n_files != 24                     , 0,
    months_since_registration == n_files & months_since_registration < 24, 1,  
    default = 0  
  )]
  
  # We want to compute by geograhpical area in which of the 10th perrcentiles the individual taxpayer is placed basend on the
  # amount paid for ToT in 2023 after June 
  data_collapsed[, 
                 decile_within_taxregion_pay := as.integer(
                   cut(taxpayer_mean_monthly_ToT_paid, 
                       breaks = quantile(
                         taxpayer_mean_monthly_ToT_paid, 
                         probs = 0:10/10, na.rm = TRUE), 
                       include.lowest = TRUE, labels = FALSE)), 
                 by = station_name]
  
  # reorder the columns
  setcolorder(data_collapsed, 
              c(
                "tax_payer_id", 
                "station_name", 
                "sector", 
                "divison", 
                "group", 
                "major_profession", 
                "sub_profession",
                "minor_profession",
                "taxpayer_mean_monthly_ToT_paid",
                "taxregion_mean_monthly_ToT_paid", 
                "decile_within_taxregion_pay",
                "always_filer_2023",
                "zero_filer_2023",
                "ToT_obligation_began_2023", 
                "paid_atleast_once_2023", 
                "months_since_registration"))
  
}

# 4. Save the data ---- 

{
  
  # save the data for the experiment 
  fwrite(data_collapsed, file.path(int_data, "dt_pilot.csv"))
  
}
