# ---------------------------------------------------------------------------- #
#                               3 tax data:  output creation                   #
#                                   World Bank - DIME                          #
#                                                                              #
# ---------------------------------------------------------------------------- #

# 0: Load the data ----

{
  
  # Read monthly payment-tot data
  data <- fread(file = file.path(fin_data, "dt_month_tot_pay.csv")  , na.strings = c("NA", ""))
  
  # Read yearly payment-tot data
  data_year <- fread(file = file.path(fin_data, "dt_year_tot_pay.csv"), na.strings = c("NA", ""))
  
  # Read the table prepared by the authors
  table <- read_excel(file.path(raw_data, "str_overview.xlsx"), sheet = "Table")
  
}


# Table 1: ----

{
  
  # Filter and summarize data
  tab_summ <- copy(data)[dt_year %in% c(2016, 2018, 2020, 2023),
                  .(
                    "N of Firms" = uniqueN(tax_payer_id), 
                    "Total Turnover (\\textit{KSh} in \\textit{millions})" = sum(tot_turnover, na.rm = TRUE) / 1e6,
                    "Tax Due (\\textit{KSh} in \\textit{millions})" = sum(net_tax, na.rm = TRUE) / 1e6,
                    "Total Payments (\\textit{KSh} in \\textit{millions})" = sum(tot_payments, na.rm = TRUE) / 1e6,
                    "Total Turnover (\\textit{KSh})" = median(tot_turnover, na.rm = TRUE), 
                    "Tax Due (\\textit{KSh})" = median(net_tax, na.rm = TRUE),
                    "Total Payment (\\textit{KSh})" = median(as.double(tot_payments), na.rm = TRUE),
                    "Shr of Individuals" = mean(tax_payer_type == "INDI", na.rm = TRUE),
                    "Shr in Nairobi" = mean(station_name_graphs == "Nairobi", na.rm = TRUE),
                    "Shr in Nyeri" = mean(station_name_graphs == "Nyeri", na.rm = TRUE),
                    "Shr in Mombasa" = mean(station_name_graphs == "Mombasa", na.rm = TRUE),
                    "Shr in Meru" = mean(station_name_graphs == "Meru", na.rm = TRUE),
                    "Shr in Eldoret" = mean(station_name_graphs == "Eldoret", na.rm = TRUE),
                    "Shr in Nakuru" = mean(station_name_graphs == "Nakuru", na.rm = TRUE)
                  ),
                  by = .(dt_year)
  ]
  
  # Reshape data for presentation
  reshaped_data <- melt(tab_summ, id.vars = "dt_year")
  reshaped_data <- dcast(reshaped_data, variable ~ dt_year, value.var = "value")
  
  # Define the formatting function
  format_num_vec <- function(x) {
    sapply(x, function(y) {
      y <- as.numeric(y)
      if (y < 1) {
        format(y, decimal.mark = ".", nsmall = 2, digits = 1, scientific = FALSE)
      } else {
        format(round(y), big.mark = ",", decimal.mark = ".", scientific = FALSE, nsmall = 0)
      }
    })
  }
  
  # convert to data table
  setDT(reshaped_data)
  
  # Apply the formatting function to all columns except the first one
  reshaped_data[, (names(reshaped_data)[-1]) := lapply(.SD, format_num_vec), .SDcols = names(reshaped_data)[-1]]
  
  # Add rows for specific categories
  tab <- reshaped_data %>% 
    add_row(
      variable = "\\textit{Aggregates}",
      `2016` = "", 
      `2018` = "", 
      `2020` = "",
      `2023` = "",
      .before = 1
    ) %>% 
    add_row(
      variable = "\\textit{Medians}",
      `2016` = "", 
      `2018` = "", 
      `2020` = "",
      `2023` = "",
      .before = 6
    ) %>% 
    add_row(
      variable = "\\textit{Firm Characteristics}",
      `2016` = "", 
      `2018` = "", 
      `2020` = "",
      `2023` = "",
      .before = 10
    )
  
  # Rename the first column
  setnames(tab, c("variable"), c(""))
  
  # Generate the LaTeX table
  tab_latex <- tab %>%
    kable(format = "latex", booktabs = TRUE, longtable = FALSE, digits = 2, 
          align = c("l", "r", "r", "r", "r"), format.args = list(big.mark = ","), 
          escape = FALSE, linesep = "") %>%
    kable_styling(latex_options = c("HOLD_position"), full_width = FALSE) %>%
    row_spec(5, extra_latex_after = "\\cmidrule(lr){3-6}") %>%
    row_spec(9, extra_latex_after = "\\cmidrule(lr){3-6}") %>% 
    row_spec(17, extra_latex_after = "\\cmidrule(lr){3-6}") 
  
  # Extract the lines of the LaTeX code
  lines <- unlist(strsplit(tab_latex, "\n"))
  
  # Remove the first, second, and last lines
  lines_modified <- lines[-c(1, 2, length(lines))]
  
  # Write the modified lines to a file
  writeLines(lines_modified, file.path(table_output, "tab_summary.tex"), sep = "\n")
  
}

# Table 2: ----

{
  
  # Step 1: Filter and aggregate data
  data_firm_month <- copy(data)[tot_turnover > 0 & inrange(dt_year, 2016, 2022) & dt_year != 2019, 
                          keyby = .(dt_month, dt_year, tax_payer_id), 
                          .(tot_turnover = sum(tot_turnover, na.rm = TRUE))
  ]
  setkey(data_firm_month, tax_payer_id, dt_year, dt_month)
  
  # Step 2: Create lagged values and new columns
  data_with_lag <- copy(data_firm_month[, TaxesPaid_PrevMonth := shift(tot_turnover, 1, type = "lag"), by = tax_payer_id])
  data_with_flags <- copy(data_with_lag)[, `:=`(
    SameTaxesAsPrevMonth = tot_turnover == TaxesPaid_PrevMonth,
    TaxesPaid_PrevMonth_Within_5Percent = abs(tot_turnover - TaxesPaid_PrevMonth) / TaxesPaid_PrevMonth <= 0.05
  )]
  
  # Step 3: Summarize yearly data
  summary_yearly <- copy(data_with_flags)[, 
                                    .(`Files with the Same Turnover` = paste0(format(mean(SameTaxesAsPrevMonth, na.rm = TRUE) * 100, digits = 3), "%"), 
                                      `Files with Turnover within ±5%` = paste0(format(mean(TaxesPaid_PrevMonth_Within_5Percent, na.rm = TRUE) * 100, digits = 3), "%")), 
                                    by = .(dt_year)
  ][order(dt_year)]
  
  # Step 4: Calculate share of firms with consistent turnover
  summary_yearly_firms <- copy(data_with_flags)[, 
                                          .(share_same = mean(SameTaxesAsPrevMonth, na.rm = TRUE),
                                            share_within_5percent = mean(TaxesPaid_PrevMonth_Within_5Percent, na.rm = TRUE)), 
                                          by = .(dt_year, tax_payer_id)
  ] %>% 
    .[, `:=`(share_same = share_same == 1, share_within_5percent = share_within_5percent == 1)] %>% 
    .[, 
      .(`Firms with always the Same Turnover` = paste0(format(mean(share_same, na.rm = TRUE) * 100, digits = 3), "%"), 
        `Firms with Turnover always within ±5%` = paste0(format(mean(share_within_5percent, na.rm = TRUE) * 100, digits = 3), "%")), 
      by = .(dt_year)
    ]
  
  # Merge summaries and create LaTeX table
  table_combined <- summary_yearly %>% 
    left_join(summary_yearly_firms, by = c("dt_year")) %>% 
    rename(Year = dt_year) %>% 
    mutate(Year = as.character(Year))
  
  table_latex <- kbl(
    table_combined, 
    format = "latex", 
    booktabs = TRUE, 
    longtable = FALSE,
    linesep = "",
    align = c("lcc"), 
    format.args = list(digits = 2, big.mark = ",")
  ) %>% 
    kable_classic_2(full_width = FALSE)
  
  # Additional formatting for LaTeX table
  table_lines <- unlist(strsplit(table_latex, "\n"))
  table_lines <- table_lines[-c(1, 2, length(table_lines))]
  table_lines <- gsub("NA", "0", table_lines)
  table_lines <- gsub("Year", "", table_lines)
  
  table_lines[3] <- "& \\multicolumn{2}{c}{Share of TOT declarations with} & \\multicolumn{2}{c}{Share of Firms declaring} \\\\"
  table_lines <- c(
    table_lines[0:3], 
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "& \\multicolumn{1}{c}{Same Turnover} & \\multicolumn{1}{c}{Turnover within ±5\\%} & \\multicolumn{1}{c}{Same Turnover} & \\multicolumn{1}{c}{Turnover within ±5\\%} \\\\", 
    table_lines[4:length(table_lines)]
  )
  
  # Save the LaTeX table to a file
  write_lines(table_lines, file.path(table_output, "tbl_patterns_filing.tex"))
  
}

# Table 4: ----

{
  
  setDT(table)
  
  # convert percentages
  table[, `Corporate Tax Rate` := paste0(`Corporate Tax Rate`*100, "%")]
  table[, Proportional := fifelse(!is.na(Proportional), paste0(Proportional*100, "%"), NA)]
  table[, `Minimum Threshold (USD)` := format(round(`Minimum Threshold (USD)`, digits = 0), big.mark = ",")]
  
  
  setorder(table, Country)
  
  # Create the LaTeX table
  latex_table <- kable(
    table, 
    format = "latex", 
    booktabs = TRUE, 
    longtable = TRUE, 
    escape = TRUE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c(" " = 7)) 
  latex_table <- unlist(strsplit(latex_table, "\n"))
  
  latex_table <- c(
    "\\begin{tabular}{lcccccc}", 
    "\\toprule",
    "\\multicolumn{1}{c}{Country} & \\multicolumn{1}{c}{Minimum Threshold} & \\multicolumn{1}{c}{Proportional} & \\multicolumn{1}{c}{Differentiated} & \\multicolumn{1}{c}{Progressive} & \\multicolumn{1}{c}{Set Fee} & \\multicolumn{1}{c}{Corporate Tax} \\\\",
    "\\multicolumn{1}{c}{} & \\multicolumn{1}{c}{(USD)} & \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{(sector/area)} & \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{(USD)} & \\multicolumn{1}{c}{Rate} \\\\",
    latex_table[7:44], 
    "\\bottomrule",
    "\\multicolumn{7}{l}{\\rule{0pt}{1em}\\textit{Note: }} \\\\",
    "\\multicolumn{7}{l}{\\rule{0pt}{1em}\\textsuperscript{*} Set fee at lower levels of turnover, percentage otherwise} \\\\",
    "\\multicolumn{7}{l}{\\rule{0pt}{1em}\\textsuperscript{**} Set fee if no records, percentage otherwise} \\\\",
    "\\end{tabular}")
  
  latex_table <- gsub("NA", "", latex_table)
  
  
  # Save the table to a .tex file
  write_lines(latex_table, file.path(table_output, "str_overview.tex"))
  
}

