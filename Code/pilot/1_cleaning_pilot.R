{
  
  # Load the raw TOT filing data
  {
    
    list_files_in_directory <- function(directory, pattern) {
      main_directory <- file.path(raw_data, directory)
      files <- list.files(
        path       = main_directory, # path to directory
        pattern    = pattern,        # pattern to match files
        full.names = TRUE            # return full file names
      )
      return(files)
    }
    
    # Get files from January 24, 2024
    files_jan24 <- list_files_in_directory("KRA_Jan24_v3/", "^TOT Returns")
    
    # Get files from March 28, 2024 (JANUARY AND FEBRUARY FILING MONTHS)
    files_mar28 <- list_files_in_directory("KRA_Mar28/", "^TOT Returns")
    
    # Combine the lists of files from both directories
    combined_files <- c(files_jan24, files_mar28)
    
    data_list <- lapply(combined_files, function(file) {
      
      # set reading options to optimize memory
      data = data.table::fread(
        file,
        na.strings = c(
          "NULL", "-", "unknown"  # Treat these values as NA
        ), 
        encoding = "UTF-8"  # Handle files with UTF-8 encoding
      )
      
      data[, 
           ':='(
             return_period_to   = as.character(return_period_to),
             return_period_from = as.character(return_period_from)
           )]
      
      return(data)
      
    })
    
    tot_data = data.table::rbindlist(data_list, fill = TRUE)
    
  }
  
  payments_to_23 = fread(file.path(raw_data, "KRA_Jan24_v3/payments.csv"), 
                         showProgress = TRUE,
                         encoding     = "Latin-1",
                         na.strings   = c("NULL", ""))
  
  payments_24 = fread(file.path(raw_data, "KRA_Mar28/payments.csv"), 
                      showProgress = TRUE,
                      encoding     = "Latin-1",
                      na.strings   = c("NULL", ""))
  
  payments = rbind(payments_to_23, payments_24, fill = TRUE)
  
  profession = fread(file.path(raw_data, "KRA_Jan24_v3/TOT Professions.csv"), 
                     showProgress = TRUE,
                     encoding     = "Latin-1",
                     na.strings   = c("NULL", ""))[, -c("station_name")]
  
  sectors = fread(file.path(raw_data, "KRA_Jan24_v3/TP Reg for TOT with sectors_div_group   dupl 25-01-2024.csv"), 
                  showProgress = TRUE,
                  encoding     = "Latin-1",
                  na.strings   = c("NULL", "")) |> 
    distinct() |> 
    arrange(obligation_reg_date) |> 
    group_by(fake_id, reg_status, business_type, business_subtype, business_commenced_dt, buss_reg_date, obligation_reg_date) |> 
    dplyr::summarise(
      sector = last(sector), 
      divison = last(division),
      group = last(group)
    ) |> 
    mutate(obligation_reg_date = dmy(obligation_reg_date))
  
}

# 2: clean data ----

{
  
  # 2.1: We drop some constant variables
  tot_data = tot_data[, -c("obligation_name", "obligation_id", "is_nil_return", "month_", "year_", "is_nil_return", "tot_adv_tax_paid")]
  
  # 2.2: We clean the name of the cities
  tot_data[, station_name := str_to_title(station_name)][,
                                                         station_name_graphs := fifelse(
                                                           grepl("Nairobi", station_name), "Nairobi",
                                                           fifelse(
                                                             grepl("Mombasa", station_name), "Mombasa", station_name
                                                           )
                                                         )
  ]
  
  # 2.3: format dates
  # format dates
  tot_data = tot_data[, ':='(
    trp_from_dt  = dmy(return_period_from),
    trp_to_dt    = dmy(return_period_to)  ,
    filing_date  = dmy(substr(filing_date, 0, 10))
  )][, - c("return_period_to", "return_period_from")]
  
  # extract year and month
  tot_data[, ':='(
    dt_month = fifelse(is.na(trp_from_dt), month(trp_to_dt), month(trp_from_dt)),
    dt_year  = fifelse(is.na(trp_from_dt),  year(trp_to_dt), year(trp_from_dt)))
  ]
  
  {# work on payments
    
    payments = payments[, c("fake_id", "tax_period_from_dt", "tax_period_to_dt", "payment_amount")]
    
    payments = payments[, ':='(
      trp_from_dt  = dmy(tax_period_from_dt),
      trp_to_dt    = dmy(tax_period_to_dt)
    )][, -c("tax_period_from_dt", "tax_period_to_dt")]
    
    # extract year and month
    payments[, ':='(
      dt_month = fifelse(is.na(trp_from_dt), month(trp_to_dt), month(trp_from_dt)),
      dt_year  = fifelse(is.na(trp_from_dt),  year(trp_to_dt), year(trp_from_dt)))
    ]
    
    payments_collapsed = copy(payments)
    # collapse data at the "fake_id", "tax_period_from_dt", "tax_period_to_dt" level
    payments_collapsed = payments_collapsed[,
                                            .(
                                              n_payments   = .N, 
                                              tot_payments = sum(payment_amount, na.rm = TRUE),
                                              avg_payment  = mean(payment_amount, na.rm = TRUE)
                                            ),
                                            by = c("fake_id", "dt_month", "dt_year")]
    
  }
  
  { # 2.4 Create a Turnover variable
    
    # 2.4.2 We first drop perfect unique rows across all columns
    # 202 obs nrow(data) - nrow(unique(data))
    test = unique(tot_data)
    
    # we also drop 1003 observations that are duplicated across the following key variables
    sum(duplicated(tot_data, by = c("tax_payer_id", "trp_from_dt", "trp_to_dt", "net_tax")))
    
    # Step 1: Order the data.table by 'ID', 'Value', and count the number of missing values ('Num_Missing')
    setkey(tot_data, tax_payer_id, trp_from_dt, trp_to_dt, net_tax)
    tot_data[, Num_Missing := rowSums(is.na(.SD))]
    
    # Step 2: Drop duplicated observations based on "fake_id", "tax_payer_id", "trp_from_dt", "trp_to_dt", "net_tax", keeping the ones with the least number of missing values
    result_dt <- tot_data[order(Num_Missing), .SD[1], by = .(tax_payer_id, trp_from_dt, trp_to_dt, net_tax)]
    
    # Remove the 'Num_Missing'
    tot_data = result_dt[, Num_Missing := NULL]
    
    # We add a variable to distinguish before and after April 2019
    tot_data[, before_after := fcase(
      dt_year %in% seq(2015, 2018), "Before",
      dt_year %in% seq(2020, 2023), "After",
      default = NA
    )][,
       n_months_filed := month(trp_to_dt) - month(trp_from_dt) + 1
    ]
    
    # We label brackets_
    tot_data = tot_data[,
                        `:=` (turnover_tax_yearly = sum(tot_turnover, na.rm = TRUE)), by = .(dt_year, tax_payer_id)] %>% 
      .[,
        brackets_tot := cut(
          turnover_tax_yearly,
          c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,
          labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "Over 50m")
        )]
    
  }
  
  {
    
    # We extract all the firms that filed a larger amount than zero for every month in 2022
    stable_id = unique(
      unique(tot_data, by = c("tax_payer_id", "dt_month", "dt_year"))[
        dt_year %in% 2022, if(.N >= 12) .SD, .(tax_payer_id)], 
      by = c("tax_payer_id"))$tax_payer_id
    
    # We extract all the tax declarations filed by the just above sub-sample of firms
    tot_data[, stable_dummy := fifelse(tax_payer_id %in% stable_id, 1, 0)]
    
  }
  
  {
    
    # We extract all the frims that filed a larger amount than zero for every month in 2022
    balanced_id = unique(
      unique(tot_data, by = c("tax_payer_id", "dt_year"))[dt_year %in% c(2017, 2018, 2020, 2021, 2022), if(.N >= 5) .SD, .(tax_payer_id)], 
      by = c("tax_payer_id"))$tax_payer_id
    
    # We extract all the tax declarations filed by the just above sub-sample of firms
    tot_data[, balanced_dummy := fifelse(tax_payer_id %in% balanced_id, 1, 0)]
    
  }
  
}

{ # test the match between payments and taxes
  
  tot_data = left_join(
    tot_data, 
    payments_collapsed,
    by = c("tax_payer_id" = "fake_id", "dt_month", "dt_year")
  )
  
  tot_data = left_join(
    tot_data, 
    full_join(sectors, profession, by = c("fake_id")),
    by = c("tax_payer_id" = "fake_id")
  )
  
}

{
  
  data_yearly = copy(tot_data)
  
  data_yearly = data_yearly[,
                            n_months_filed := month(trp_to_dt) - month(trp_from_dt) + 1
  ][,
    .(tot_turnover        = sum(tot_turnover, na.rm = TRUE),
      tot_local_purchase  = sum(tot_local_purchase, na.rm = TRUE),
      turnover_tax_rate   = sum(turnover_tax_rate, na.rm = TRUE),
      gross_tax           = sum(gross_tax, na.rm = TRUE),
      net_tax             = sum(net_tax, na.rm = TRUE),
      tot_local_purchase  = sum(tot_local_purchase, na.rm = TRUE),
      net_tax_payable     = sum(net_tax_payable, na.rm = TRUE),
      crdt_sect_12a       = sum(crdt_sect_12a, na.rm = TRUE),
      net_tax_payable     = sum(net_tax_payable, na.rm = TRUE),
      n_declarations      = .N,
      n_months_filed      = sum(n_months_filed, na.rm = TRUE),
      n_payments          = sum(n_payments, na.rm = TRUE), 
      tot_payments        = sum(tot_payments, na.rm = TRUE),
      avg_payment         = mean(avg_payment, na.rm = TRUE))
    , by = .(
      tax_payer_id,
      tax_payer_type,
      dt_year,
      station_name,
      station_name_graphs, 
      stable_dummy,
      balanced_dummy,
      before_after)]
  
}

# 4: Save the data ----

{
  
  # Save the raw cleaned data
  fwrite(tot_data, file = file.path(fin_data, "Kenya_tot_pay_data.csv"))
  
  fwrite(data_yearly, file = file.path(fin_data, "Kenya_tot_pay_yearly.csv"))
  
  fwrite(payments, file = file.path(fin_data, "payments.csv"))
  
  fwrite(payments_collapsed, file = file.path(fin_data, "payments_collapsed.csv"))
  
  # free unused memory usage
  gc()
  
}
