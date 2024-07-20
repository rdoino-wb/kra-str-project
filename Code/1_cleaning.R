# ---------------------------------------------------------------------------- #
#                              1 Kenya Tax Project: cleaning                   #
#                                   World Bank - DIME                          #
#                                                                              #
# ---------------------------------------------------------------------------- #

# This code will reproduce most of the first cleaning operations for the datasets
# both at the filing and payments level. All the files will be stored in the 
# final folder "Data/Final".

# 0. Functions ----

{
  
  # Function to extract all files from a directory
  list_files_in_directory <- function(directory, pattern) {
    
    main_directory <- file.path(directory)
    
    files <- list.files(
      path       = main_directory, # path to directory
      pattern    = pattern,        # pattern to match files
      full.names = TRUE            # return full file names
    )
    
    return(files)
    
  }
  
  # Function to read csv files
  read_csv_files <- function(file) {
    
    # set reading options to optimize memory
    data = data.table::fread(
      file,
      na.strings = c(
        "NULL", "-", "unknown"  # Treat these values as NA
      ), 
      encoding = "UTF-8"  # Handle files with UTF-8 encoding
    )
    
    # convert in string
    data[, 
         ':='(
           return_period_to   = as.character(return_period_to),
           return_period_from = as.character(return_period_from)
         )]
    
    return(data)
    
  }
  
}

# 1. Load Data ----
{
  
  
  { # SMS DELIVERY DATA 
    
    # Load the SmS delivery report data
    sms_report = fread(
      file.path(
        raw_data, 
        "sms_delivery_report/SMS Delivery Report - Short Survey on Experience Paying ToT (22.05.2024).csv"
        )
      )
    
  }
    
  { # TOT FILING DATA
    
    # Extract names of files for each patch of data received by KRA
    files_jan24 <- list_files_in_directory(paste0(raw_data, "/KRA_Jan24_v3/"), "^TOT Returns")
    files_may24 <- list_files_in_directory(paste0(raw_data, "/KRA_May24/"), "^TOT Filing")
    
    # Combine the lists of files from both directories
    combined_files <- c(files_jan24)
    
    # We read all files and store them in a list
    data_list <- lapply(combined_files, read_csv_files)
    
    # Transform the list in a data table
    tot_data_15_23    = data.table::rbindlist(data_list, fill = TRUE)
    
    # We read the latest batch of data seperately
    tot_data_24 = fread(files_may24)
    
  }
    
  { # PAYMENTS DATA
    
    # Load the first batch of data
    payments_to_23 = fread(file.path(raw_data, "KRA_Jan24_v3/payments.csv"), 
                           showProgress = TRUE,
                           encoding     = "Latin-1",
                           na.strings   = c("NULL", ""))
    
    # Load the second batch of data
    payments_24 = fread(file.path(raw_data, "KRA_May24/TOT Payments 2024_deid 27.05.2024.csv"), 
                        showProgress = TRUE,
                        encoding     = "Latin-1",
                        na.strings   = c("NULL", ""))
    
  }
  
  { # COMPLEMENTARY DATA
    
    # Load VAT data
    data_vat = fread(file.path(raw_data, "VAT.csv"), 
                       showProgress = TRUE,
                       encoding     = "Latin-1",
                       na.strings   = c("NULL", ""))
    
    # Load Sector records
    sectors = fread(file.path(raw_data, "KRA_Jan24_v3/TP Reg for TOT with sectors_div_group   dupl 25-01-2024.csv"), 
                    showProgress = TRUE,
                    encoding     = "Latin-1",
                    na.strings   = c("NULL", ""))
    
  }
    
}

# 2: Clean Main data ----

{

  { # TOT DATA
    
    # We drop some constant variables
    tot_data_15_23 = tot_data_15_23[, -c("obligation_name", "obligation_id", "is_nil_return", "month_", "year_", "is_nil_return", "tot_adv_tax_paid")]
    tot_data_24    = tot_data_24[, -c("trp_period_mnth", "trp_period_year", "obligation_name", "gross_tax", "Tax Due", "net tax payable")]
    
    # We clean the name of the cities
    tot_data_15_23[, station_name := str_to_title(station_name)][,
                                                           station_name_graphs := fifelse(
                                                             grepl("Nairobi", station_name), "Nairobi",
                                                             fifelse(
                                                               grepl("Mombasa", station_name), "Mombasa", station_name
                                                             )
                                                           )
    ]
    
    # format dates
    tot_data_15_23 = tot_data_15_23[, ':='(
      trp_from_dt  = dmy(return_period_from),
      trp_to_dt    = dmy(return_period_to)  ,
      filing_date  = dmy(substr(filing_date, 0, 10))
    )][, - c("return_period_to", "return_period_from")]
    
    # extract year and month
    tot_data_15_23[, ':='(
      dt_month = fifelse(is.na(trp_from_dt), month(trp_to_dt), month(trp_from_dt)),
      dt_year  = fifelse(is.na(trp_from_dt),  year(trp_to_dt), year(trp_from_dt)))
    ]
    
    # Replicate the same for the 2024 batch of TOT data
    tot_data_24 <- tot_data_24[, ':='(
      trp_from_dt  = mdy(trp_from_dt),
      trp_to_dt    = mdy(trp_to_dt))
    ][, 
      station_name := str_to_title(station_name)
    ][,
      station_name_graphs := fifelse(
        grepl("Nairobi", station_name), "Nairobi",
        fifelse(
          grepl("Mombasa", station_name), "Mombasa", station_name
        )
      )][, ':='( # these following operations were not done to the previous batch since the vars were already there
        dt_month = fifelse(is.na(trp_from_dt), month(trp_to_dt), month(trp_from_dt)),
        dt_year  = fifelse(is.na(trp_from_dt),  year(trp_to_dt), year(trp_from_dt)) ,
        tot_turnover = as.numeric(gsub(",", "", tot_turnover)),
        net_tax = as.numeric(gsub(",", "", net_tax))
      )][,
         net_tax := fifelse(!is.na(tot_turnover) & is.na(net_tax), tot_turnover*turnover_tax_rate/100, net_tax)
      ][, -c("turnover_tax_rate")]
    
    # Now, we can merge them together
    tot_data = rbindlist(list(tot_data_15_23, tot_data_24), fill = TRUE)
    
    # There are 4 cases that are exact duplicates, we drop them 
    # nrow(tot_data) - nrow(unique(tot_data))
    tot_data = unique(tot_data)
    
    # we also drop 58 observations that are duplicated across the following key variables
    sum(duplicated(tot_data, by = c("tax_payer_id", "trp_from_dt", "trp_to_dt", "net_tax")))
    
    # We want to keep the rows that have the least number of missing values
    setkey(tot_data, tax_payer_id, trp_from_dt, trp_to_dt, net_tax)
    tot_data[, Num_Missing := rowSums(is.na(.SD))]
    
    # Drop duplicated observations based on "fake_id", "tax_payer_id", "trp_from_dt", "trp_to_dt", "net_tax", keeping the ones with the least number of missing values
    result_dt <- tot_data[order(Num_Missing), .SD[1], by = .(tax_payer_id, trp_from_dt, trp_to_dt, net_tax)]
    
    # Remove the 'Num_Missing'
    tot_data = result_dt[, Num_Missing := NULL]
    
    # There are 99 cases where we have more than one filing for a month, we collapse them at the monthly level
    # If you want to look at these cases then you can run the following code
    #View(tot_data[, .N, by = .(
    #       tax_payer_id, tax_payer_type, trp_from_dt, trp_to_dt, station_name, 
    #       filing_date, type_of_return, tot_local_purchase, 
    #       turnover_tax_rate, crdt_sect_12a, 
    #       station_name_graphs, dt_month, dt_year)][N == 2])
    tot_data = tot_data[, 
                        .(
                          net_tax         = sum(net_tax, na.rm = TRUE), 
                          tax_due         = sum(tax_due, na.rm = TRUE), 
                          gross_tax       = sum(gross_tax, na.rm = TRUE),
                          tot_turnover    = sum(tot_turnover, na.rm = TRUE),
                          net_tax_payable = sum(net_tax_payable, na.rm = TRUE)), 
                        by = .(
                          tax_payer_id, tax_payer_type, trp_from_dt, trp_to_dt, station_name, 
                          filing_date, type_of_return, tot_local_purchase, 
                          turnover_tax_rate, crdt_sect_12a, 
                          station_name_graphs, dt_month, dt_year)]
    
    # We add a variable to distinguish before and after April 2019
    tot_data[, before_after := fcase(
      dt_year %in% seq(2015, 2018), "Before",
      dt_year %in% seq(2020, 2023), "After",
      default = NA)
      ][,
        # we compute the number of filings for each row. Sometimes, taxpayers file once for multiple months
        # we want to make sure that we capture that
        n_months_filed := month(trp_to_dt) - month(trp_from_dt) + 1
        ][,
          # we compute year turnover at the taxpayer-level
          `:=` (turnover_tax_yearly = sum(tot_turnover, na.rm = TRUE)), by = .(dt_year, tax_payer_id)
          ][,
            brackets_tot := cut(
              turnover_tax_yearly,
              c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,
              labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "Over 50m")
            )][,
              # here, we want to compute the number of months within a year the taxpayer filed taxes
              n_months := uniqueN(dt_month),
              by = .(dt_year, tax_payer_id)
              ][,
                # create a dummy to identify who files consistently every month 
                stable_dummy := fcase(
                  dt_year < 2019 & n_months >= 4 , 1,
                  dt_year > 2019 & n_months >= 12, 1,
                  default = 0)]
    
    # We want to identify the sample of balanced taxpayers who filed at least once per year in the following years
    balanced_id = unique(
      unique(tot_data, by = c("tax_payer_id", "dt_year"))[dt_year %in% c(2017, 2018, 2020, 2021, 2022), if(.N >= 5) .SD, .(tax_payer_id)], 
      by = c("tax_payer_id"))$tax_payer_id
    
    # We extract all the tax declarations filed by the just above sub-sample of firms
    tot_data[, balanced_dummy := fifelse(tax_payer_id %in% balanced_id, 1, 0)]
    
    # exclude taxes inferior than 0
    tot_data = tot_data[net_tax < 0 & net_tax != 0 & dt_year != 2019, net_tax := NA]
    
    # Create a data at the firm level
    firm = copy(tot_data)[, .(
      declarations = .N, 
      total_tax = sum(net_tax, na.rm = TRUE)),
      by = .(tax_payer_id, station_name_graphs)
      ]
    
    # Finally, we append the registration date
    # Select distinct rows based on `fake_id` and `obligation_reg_date`, and arrange by `obligation_reg_date`
    sectors <- sectors[, .SD[order(obligation_reg_date)], by = .(fake_id, reg_status, business_type, business_subtype, business_commenced_dt, buss_reg_date, obligation_reg_date)]
    sectors <- unique(sectors, by = c("fake_id", "obligation_reg_date"))
    
    # Summarize to get the last values of `sector`, `division`, and `group`
    sectors <- sectors[, .(
      sector = last(sector),
      division = last(division),
      group = last(group)
    ), by = .(fake_id, reg_status, business_type, business_subtype, business_commenced_dt, buss_reg_date, obligation_reg_date)]
    
    # Convert `obligation_reg_date` to Date type
    sectors[, obligation_reg_date := dmy(obligation_reg_date)]
    
    tot_data = left_join(tot_data, sectors, by = c("tax_payer_id" = "fake_id"))
    
  }
  
  { # PAYMENTS
      
      # We select the variables of interest
      payments_to_23 = payments_to_23[, c("fake_id", "tax_period_from_dt", "tax_period_to_dt", "payment_amount")]
      
      # format dates
      payments_to_23 = payments_to_23[,
                                      ':='(
                                        trp_from_dt  = dmy(tax_period_from_dt),
                                        trp_to_dt    = dmy(tax_period_to_dt))
                                      ][, 
                                        -c("tax_period_from_dt", "tax_period_to_dt")
                                        ]
      
      # extract year and month
      payments_to_23[, ':='(
        dt_month = fifelse(is.na(trp_from_dt), month(trp_to_dt), month(trp_from_dt)),
        dt_year  = fifelse(is.na(trp_from_dt),  year(trp_to_dt), year(trp_from_dt)))
      ]
      
      # collapse data at the "fake_id", "tax_period_from_dt", "tax_period_to_dt" level
      payments_collapsed = copy(payments_to_23)[,
                          .(
                            n_payments   = .N, 
                            tot_payments = sum(payment_amount, na.rm = TRUE),
                            avg_payment  = mean(payment_amount, na.rm = TRUE)
                          ),
                          by = c("fake_id", "dt_month", "dt_year")]
      
      # change names to homogenize to the previous batch of data
      setnames(payments_24, c("tax_payer_id", "tax_period_to_dt", "Payment_Amount"), c("fake_id", "trp_from_dt", "payment_amount"))
      
      # extract dates
      payments_24[, 
                  ":="(
                    reconciliation_month = dmy(paste("01", gsub("\\s+", "", sub("/", "-", reconciliation_month)), sep = "-")), 
                    payment_amount = as.numeric(gsub(",", "", payment_amount)),
                    trp_from_dt = gsub("/", "-", trp_from_dt)
                  )][,
                     ":="(
                       dt_month = month(reconciliation_month),
                       dt_year  = year(reconciliation_month)
                     )]
      
      # collapse data ad the fake_id, dt_month, dt_year level
      payments_24_collapsed = payments_24[,
                                          .(
                                            n_payments   = .N, 
                                            tot_payments =  sum(payment_amount, na.rm = TRUE),
                                            avg_payment  = mean(payment_amount, na.rm = TRUE)
                                            ),
                                          by = c("fake_id", "dt_month", "dt_year")]
      
      # we merge the two payment batches (no collapsed)
      payments = rbindlist(list(
        payments_to_23,
        payments_24[, c("fake_id", "payment_amount", "dt_month", "dt_year")]
      ), fill = TRUE)
      
      # we merge the two payment batches (collapsed)
      payments_collapsed = rbindlist(list(
        payments_collapsed,
        payments_24_collapsed
      ), fill = TRUE)
      
  }
  
  { # TOT + PAYMENTS
    
    # We add payments collapsed data to the TOT data
    tot_data = full_join(
      tot_data, 
      payments_collapsed,
      by = c("tax_payer_id" = "fake_id", "dt_month", "dt_year")
    )
    
    # We collapse TOT-Payments data at the year level
    data_yearly = copy(tot_data)[,
                                 .(tot_turnover        = sum(tot_turnover, na.rm = TRUE),
                                   tot_local_purchase  = sum(tot_local_purchase, na.rm = TRUE),
                                   turnover_tax_rate   = sum(turnover_tax_rate, na.rm = TRUE),
                                   gross_tax           = sum(gross_tax, na.rm = TRUE),
                                   net_tax             = sum(net_tax, na.rm = TRUE),
                                   tot_local_purchase  = sum(tot_local_purchase, na.rm = TRUE),
                                   net_tax_payable     = sum(net_tax_payable, na.rm = TRUE),
                                   crdt_sect_12a       = sum(crdt_sect_12a, na.rm = TRUE),
                                   n_declarations      = .N,
                                   n_months_filed      = sum(n_months_filed, na.rm = TRUE),
                                   n_payments          = sum(n_payments, na.rm = TRUE), 
                                   tot_payments        = sum(tot_payments, na.rm = TRUE),
                                   avg_payment         = mean(avg_payment, na.rm = TRUE)), 
                                 by = .(
                                   tax_payer_id,
                                   tax_payer_type,
                                   dt_year,
                                   station_name,
                                   station_name_graphs, 
                                   stable_dummy,
                                   balanced_dummy,
                                   before_after)][, 
                                                  brackets_tot_tax := cut(
                                                    tot_turnover,
                                                    c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "Over 50m")
                                                  )][,
                                                     bracket_highlight_tax := fcase(
                                                       tot_turnover < 5000000  & tot_turnover > 500000  & dt_year %in% seq(2015, 2019), "Taxable",
                                                       tot_turnover < 50000000 & tot_turnover > 1000000 & dt_year %in% seq(2020, 2023), "Taxable",
                                                       default = "Non Taxable"
                                                     )
                                                  ]
    
  }
  
}

# 3: Clean Complemntary data ----

{
  
  { # VAT
    
    # Convert columns and clean data
    data_vat[, 
             `:=`(
               reconciliation_month = dmy(paste("01", gsub("\\s+", "", sub("/", "-", reconciliation_month)), sep = "-")), 
               Payment_Amount = as.numeric(gsub(",", "", Payment_Amount))
             )]
    
    # Summarize payment amounts by reconciliation month
    data_vat <- data_vat[, .(Payment_Amount = sum(Payment_Amount, na.rm = TRUE)), by = reconciliation_month]
    
    # Ensure data is in data.table format
    setDT(data_vat)
    
    # Create dt_month, dt_year, and YearMonth columns
    data_vat[, `:=`(
      dt_month = as.numeric(substr(reconciliation_month, 6, 7)),
      dt_year  = as.numeric(substr(reconciliation_month, 1, 4)),
      YearMonth = ymd(paste0("2023", fifelse(nchar(substr(reconciliation_month, 6, 7)) == 1, paste0("0", substr(reconciliation_month, 6, 7)), substr(reconciliation_month, 6, 7)), "01"))
    )]
    
  }
  
}

# 4: Save the data ----

{
  
  # Save the TOT data at the month level
  fwrite(tot_data, file = file.path(fin_data, "dt_month_tot_pay.csv"))
  
  # Save the TOT data at the year level
  fwrite(data_yearly, file = file.path(fin_data, "dt_year_tot_pay.csv"))
  
  # Save the payments transactions
  fwrite(payments, file = file.path(fin_data, "dt_payments.csv"))
  
  # Save the Payments Data collapsed at the month level
  fwrite(payments_collapsed, file = file.path(fin_data, "dt_payments_collapsed.csv"))
  
  # Save the Payments Data collapsed at the month level
  fwrite(firm, file = file.path(fin_data, "dt_firm_tot_pay.csv"))

  # SAve VAT Data
  fwrite(data_vat, file = file.path(fin_data, "dt_vat.csv"))
  
  # free unused memory usage
  gc()
  
}
