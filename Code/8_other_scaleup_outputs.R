
# Load the data ----

{
  
  matched_data <- fread(file.path(fin_data, "Experiment/data_reg_wide_scale_up.csv"))
  
}


# Table 5 ----

{
  
  any = copy(matched_data)[group != "Control"]
  any[, group := "Any Treatment"]
  
  data_balance = rbind(matched_data, any)
  
  data_balance[, capital_city := grepl("Nairobi", station_name)]
  
  compute_stats <- function(data, group_col, value_cols) {
    
    # Calculate means and standard errors
    means <- data[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = group_col, .SDcols = value_cols]
    ses <- data[, lapply(.SD, function(x) sd(x, na.rm = TRUE) / sqrt(.N)), by = group_col, .SDcols = value_cols]
    
    # Rename columns
    setnames(means, old = value_cols, new = paste0(value_cols, "_mean"))
    setnames(ses, old = value_cols, new = paste0(value_cols, "_se"))
    
    # Merge the two datasets
    stats <- merge(means, ses, by = group_col)
    
    # Reorder columns to alternate _mean and _se
    mean_cols <- paste0(value_cols, "_mean")
    se_cols <- paste0(value_cols, "_se")
    ordered_cols <- c(group_col, as.vector(rbind(mean_cols, se_cols)))
    stats <- stats[, ..ordered_cols]
    
    return(stats)
  }
  
  # Compute statistics for the whole sample
  value_cols <- c("taxpayer_mean_monthly_tot_paid", "zero_filer_2023", 
                  "months_since_registration", "tot_obligation_began_2023", 
                  "n_files")
  
  whole_sample_stats <- compute_stats(data_balance, "group", value_cols)
  
  # Segment data based on the median value of avg_tot_paid
  below_median_data <- data_balance[below_median == TRUE]
  above_median_data <- data_balance[below_median == FALSE]
  
  # Compute statistics for below median
  below_median_stats <- compute_stats(below_median_data, "group", value_cols)
  
  # Compute statistics for above median
  above_median_stats <- compute_stats(above_median_data, "group", value_cols)
  
  # Format the statistics with mean and standard error in parentheses
  format_stats <- function(stats, data) {
    
    dt = copy(stats)
    
    for (col in value_cols) {
      
      mean_col <- paste0(col, "_mean")
      se_col <- paste0(col, "_se")
      
      dt[, 
         (mean_col) := round(get(mean_col), 3)
      ][,
        (se_col) := paste0("[", round(get(se_col), 3), "]")
      ]
      
    }
    
    # Reshape the dt
    long_data <- melt(dt, id.vars = "group", variable.name = "variable", value.name = "value")
    
    # Cast the data to wide format
    wide_data <- dcast(long_data, variable ~ group, value.var = "value")
    
    # Conver to data.table
    setDT(wide_data)
    
    # Reorder columns
    wide_data = wide_data[, c("variable", "Control", "Any Treatment", "Anchor", "Reminder")]
    
    # Adding the number of observations row
    obs_row <- data.table(
      variable = "Observations",
      Control = c(format(nrow(data[group == "Control"]), big.mark = ",")),
      `Any Treatment` = c(format(nrow(data[group == "Any Treatment"]), big.mark = ",")),
      Anchor = c(format(nrow(data[group == "Anchor"]), big.mark = ",")),
      Reminder = c(format(nrow(data[group == "Reminder"]), big.mark = ","))
    )
    
    # Add the observation row to the data
    wide_data <- rbind(wide_data, obs_row, fill = TRUE)
    
    return(wide_data)
    
  }
  
  # Apply formatting
  formatted_whole_sample <- format_stats(whole_sample_stats, data_balance)
  formatted_below_median <- format_stats(below_median_stats, below_median_data)
  formatted_above_median <- format_stats(above_median_stats, above_median_data)
  
  # Define a function to compute differences and significance levels for all value columns
  compute_differences <- function(stats, value_cols, data) {
    
    diff_list <- list()
    
    for (col in value_cols) {
      
      mean_col <- paste0(col, "_mean")
      se_col <- paste0(col, "_se")
      
      diffs <- stats[, .(
        `Control - Any Treatment` = round(get(mean_col)[group == "Control"] - get(mean_col)[group == "Any Treatment"], 3),
        `Control - Reminder` = round(get(mean_col)[group == "Control"] - get(mean_col)[group == "Reminder"], 3),
        `Control - Anchor` = round(get(mean_col)[group == "Control"] - get(mean_col)[group == "Anchor"], 3),
        `Reminder - Anchor` = round(get(mean_col)[group == "Reminder"] - get(mean_col)[group == "Anchor"], 3)
      )]
      
      se_diffs <- stats[, .(
        `Control - Any Treatment` = sqrt(get(se_col)[group == "Control"]^2 + get(se_col)[group == "Any Treatment"]^2),
        `Control - Reminder` = sqrt(get(se_col)[group == "Control"]^2 + get(se_col)[group == "Reminder"]^2),
        `Control - Anchor` = sqrt(get(se_col)[group == "Control"]^2 + get(se_col)[group == "Anchor"]^2),
        `Reminder - Anchor` = sqrt(get(se_col)[group == "Reminder"]^2 + get(se_col)[group == "Anchor"]^2)
      )]
      
      n_diffs <- stats[, .(
        `Control - Any Treatment` = min(nrow(data[group == "Control"]), nrow(data[group == "Any Treatment"])),
        `Control - Reminder` = min(nrow(data[group == "Control"]), nrow(data[group == "Reminder"])),
        `Control - Anchor` = min(nrow(data[group == "Control"]), nrow(data[group == "Anchor"])),
        `Reminder - Anchor` = min(nrow(data[group == "Reminder"]), nrow(data[group == "Anchor"]))
      )]
      
      p_values <- diffs / se_diffs
      
      dt_p_values <- as.data.table(rbind(p_values, n_diffs))
      
      # Add an ID column for reshaping
      dt_p_values[, id := c("p_values", "n")]
      
      # Reshape the data from wide to long format
      long_data <- melt(dt_p_values, id.vars = "id", variable.name = "Comparison", value.name = "Value")
      
      # Split the rows into two separate columns
      reshaped_data <- dcast(long_data, Comparison ~ id, value.var = "Value")
      
      setDT(reshaped_data)
      
      reshaped_data[, p_values := as.numeric(p_values)] # Ensure p_values is numeric
      reshaped_data[, p_values := 2 * pt(-abs(p_values), df = n - 1)]
      
      significance <- reshaped_data[, lapply(p_values, function(p) {
        if (is.na(p)) {
          return("-")
        } else if (p < 0.01) {
          return("***")
        }else if (p < 0.05) {
          return("**")
        } else if (p < 0.1) {
          return("*")
        } else {
          return("")
        }
      })]
      
      setnames(significance, new = names(diffs))
      
      diffs_with_significance <- as.data.table(diffs)
      diffs_with_significance[, `:=` (
        `Control - Any Treatment` = paste0(diffs[, `Control - Any Treatment`], significance[, `Control - Any Treatment`]),
        `Control - Reminder` = paste0(diffs[, `Control - Reminder`], significance[, `Control - Reminder`]),
        `Control - Anchor` = paste0(diffs[, `Control - Anchor`], significance[, `Control - Anchor`]),
        `Reminder - Anchor` = paste0(diffs[, `Reminder - Anchor`], significance[, `Reminder - Anchor`])
      )]
      
      diff_list[[col]] <- diffs_with_significance
      
    }
    
    diff_table <- rbindlist(diff_list, use.names = TRUE, fill = TRUE)
    
    # Function to interleave missing rows
    interleave_missing <- function(dt) {
      # Create a data.table with missing values
      dt_missing <- dt[1, ][, lapply(.SD, function(x) NA)]
      # Replicate the missing row to match the original data.table's length
      dt_missing <- dt_missing[rep(1, nrow(dt))]
      # Combine the original data.table and the missing rows
      dt_combined <- rbindlist(list(dt, dt_missing), use.names = TRUE)
      # Create an index to interleave the rows
      idx <- order(c(seq_len(nrow(dt)), seq_len(nrow(dt))))
      # Reorder the combined data.table by the interleaved index
      dt_interleaved <- dt_combined[idx]
      # Add an additional row of missing values at the end
      dt_interleaved <- rbind(dt_interleaved, dt_missing[1])
      return(dt_interleaved)
    }
    
    # Apply the function to the sample data.table
    diff_table <- interleave_missing(diff_table)
    
    return(diff_table)
    
  }
  
  # Apply the function to compute differences with significance for all value columns
  diffs_whole_sample <- compute_differences(whole_sample_stats, value_cols, data_balance)
  diffs_below_median <- compute_differences(below_median_stats, value_cols, below_median_data)
  diffs_above_median <- compute_differences(above_median_stats, value_cols, above_median_data)
  
  # Compute the F-Stats
  compute_f_stats <- function(data, value_cols) {
    
    f_stats = data.table(
      `variable` = c("F-Stat"),
      `Control` = c(""), 
      `Any Treatment` = c(""), 
      `Anchor` = c(""),
      `Reminder` = c(""),
      `Control - Any Treatment` = round(fitstat(feols(data, as.formula(paste0("group != 'Control'", " ~ ", paste0(value_cols, collapse = " + "))), subset = data$group %in% c("Control", "Any Treatment")), type = "f")$f$stat, 3),
      `Control - Reminder` = round(fitstat(feols(data, as.formula(paste0("group != 'Control'", " ~ ", paste0(value_cols, collapse = " + "))), subset = data$group %in% c("Control", "Reminder")), type = "f")$f$stat, 3),
      `Control - Anchor`   = round(fitstat(feols(data, as.formula(paste0("group != 'Control'", " ~ ", paste0(value_cols, collapse = " + "))), subset = data$group %in% c("Control", "Anchor")), type = "f")$f$stat, 3),
      `Reminder - Anchor`  = round(fitstat(feols(data, as.formula(paste0("group != 'Anchor'", " ~ ", paste0(value_cols, collapse = " + "))), subset = data$group %in% c("Anchor", "Reminder")), type = "f")$f$stat, 3)
    )
    
    return(f_stats)
    
  }
  
  # Compute F-Stats for the differences
  f_stats_wole_sample = compute_f_stats(data_balance, value_cols)
  f_stats_below_median = compute_f_stats(below_median_data, value_cols)
  f_stats_above_median = compute_f_stats(above_median_data, value_cols)
  
  # Combine formatted results and differences into one final table
  final_table <- cbind(
    rbindlist(list(
      Whole_Sample = formatted_whole_sample,
      Below_Median = formatted_below_median,
      Above_Median = formatted_above_median)),
    rbindlist(list(
      Differences_Whole_Sample = diffs_whole_sample,
      Differences_Below_Median = diffs_below_median,
      Differences_Above_Median = diffs_above_median
    )))
  
  table = rbind(final_table[1:11],  f_stats_wole_sample, final_table[12:22], f_stats_below_median, final_table[23:33], f_stats_above_median)
  
  
  # Output the table
  kable_output <- table %>%
    kable("latex", booktabs = TRUE, escape = FALSE, linesep = "", align = "lcccccccc") %>%
    kable_styling(latex_options = "hold_position", full_width = FALSE)
  
  # Convert the output to a single character vector split by newline
  kable_output <- unlist(strsplit(as.character(kable_output), "\n"))
  
  kable_output = gsub("NA", "", kable_output)
  kable_output = gsub("Observations", "\\\\textit{Observations}", kable_output)
  kable_output = gsub("F-Stat", "\\\\textit{F-Stat}", kable_output)
  kable_output = gsub("zero_filer_2023_se|months_since_registration_se|tot_obligation_began_2023_se|n_files_se|taxpayer_mean_monthly_tot_paid_se", "", kable_output)
  kable_output = gsub("zero_filer_2023_mean", "Zero Filing, 2023", kable_output)
  kable_output = gsub("months_since_registration_mean", "N of Months since registration", kable_output)
  kable_output = gsub("tot_obligation_began_2023_mean", "ToT Obligation began in 2023", kable_output)
  kable_output = gsub("n_files_mean", "N of Filings", kable_output)
  kable_output = gsub("taxpayer_mean_monthly_tot_paid_mean", "Avg ToT Paid, 2023", kable_output)
  kable_output[33:34] = gsub("0|0-|\\[0\\]", "-", kable_output[33:34])
  
  test = c(
    kable_output[3],
    "\\tabularnewline \\midrule \\midrule",
    "& (1) & (2) & (3) & (4) & Difference & Difference & Difference & Difference\\\\",
    "& Control & Any Treatment & Anchor & Reminder & (1) - (2) & (1) - (3) & (1) - (4) & (3) - (4) \\\\ \n",
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Whole Sample}}} & & & & & & & & \\\\ \n \\addlinespace ",
    kable_output[7:16], 
    "\\midrule", 
    kable_output[17:18],
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Below Median}}} & & & & & & & & \\\\ \n \\addlinespace ",
    kable_output[19:28], 
    "\\midrule", 
    kable_output[29:30],
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Median and Above}}} & & & & & & & & \\\\ \n \\addlinespace ",
    kable_output[31:40], 
    "\\midrule", 
    kable_output[41:44]
  )
  
  write_lines(test, file.path(table_output, "tbl_balance.tex"))
  
}

# Table 6 ----

{
  
  data_balance_sms = copy(matched_data)[group != "Control"]
  
  # Compute statistics for the whole sample
  value_cols <- c("taxpayer_mean_monthly_tot_paid", "zero_filer_2023", 
                  "months_since_registration", "tot_obligation_began_2023", 
                  "n_files")
  
  whole_sample_stats <- compute_stats(data_balance_sms, "sms_delivery", value_cols)
  
  # Segment data based on the median value of avg_tot_paid
  below_median_data <- data_balance_sms[below_median == TRUE]
  above_median_data <- data_balance_sms[below_median == FALSE]
  
  # Compute statistics for below median
  below_median_stats <- compute_stats(below_median_data, "sms_delivery", value_cols)
  
  # Compute statistics for above median
  above_median_stats <- compute_stats(above_median_data, "sms_delivery", value_cols)
  
  # Format the statistics with mean and standard error in parentheses
  format_stats <- function(stats, data) {
    
    dt = copy(stats)
    
    for (col in value_cols) {
      
      mean_col <- paste0(col, "_mean")
      se_col <- paste0(col, "_se")
      
      dt[, 
         (mean_col) := round(get(mean_col), 3)
      ][,
        (se_col) := paste0("[", round(get(se_col), 3), "]")
      ]
      
    }
    
    # Reshape the dt
    long_data <- melt(dt, id.vars = "sms_delivery", variable.name = "variable", value.name = "value")
    
    # Cast the data to wide format
    wide_data <- dcast(long_data, variable ~ sms_delivery, value.var = "value")
    
    # Conver to data.table
    setDT(wide_data)
    
    # Reorder columns
    wide_data = wide_data[, c("variable", "FALSE", "TRUE")]
    
    # Adding the number of observations row
    obs_row <- data.table(
      variable = "Observations",
      `FALSE` = c(format(nrow(data[sms_delivery == FALSE]), big.mark = ",")),
      `TRUE` = c(format(nrow(data[sms_delivery == TRUE]), big.mark = ","))
    )
    
    # Add the observation row to the data
    wide_data <- rbind(wide_data, obs_row, fill = TRUE)
    
    return(wide_data)
    
  }
  
  # Apply formatting
  formatted_whole_sample <- format_stats(whole_sample_stats, data_balance_sms)
  formatted_below_median <- format_stats(below_median_stats, below_median_data)
  formatted_above_median <- format_stats(above_median_stats, above_median_data)
  
  # Define a function to compute differences and significance levels for all value columns
  compute_differences <- function(stats, value_cols) {
    
    diff_list <- list()
    
    for (col in value_cols) {
      
      mean_col <- paste0(col, "_mean")
      se_col <- paste0(col, "_se")
      
      diffs <- stats[, .(
        `TRUE - FALSE` = round(get(mean_col)[sms_delivery == TRUE] - get(mean_col)[sms_delivery == FALSE], 3)
      )]
      
      se_diffs <- stats[, .(
        `TRUE - FALSE` = sqrt(get(se_col)[sms_delivery == TRUE]^2 + get(se_col)[sms_delivery == FALSE]^2)
      )]
      
      n_diffs <- stats[, .(
        `TRUE - FALSE` = min(nrow(data_balance_sms[sms_delivery == TRUE]), nrow(data_balance_sms[sms_delivery == FALSE]))
      )]
      
      p_values <- diffs / se_diffs
      
      dt_p_values <- as.data.table(rbind(p_values, n_diffs))
      
      # Add an ID column for reshaping
      dt_p_values[, id := c("p_values", "n")]
      
      # Reshape the data from wide to long format
      long_data <- melt(dt_p_values, id.vars = "id", variable.name = "Comparison", value.name = "Value")
      
      # Split the rows into two separate columns
      reshaped_data <- dcast(long_data, Comparison ~ id, value.var = "Value")
      
      setDT(reshaped_data)
      
      reshaped_data[, p_values := as.numeric(p_values)] # Ensure p_values is numeric
      reshaped_data[, p_values := 2 * pt(-abs(p_values), df = n - 1)]
      
      significance <- reshaped_data[, lapply(p_values, function(p) {
        if (is.na(p)) {
          return("-")
        } else if (p < 0.01) {
          return("***")
        }else if (p < 0.05) {
          return("**")
        } else if (p < 0.1) {
          return("*")
        } else {
          return("")
        }
      })]
      
      setnames(significance, new = names(diffs))
      
      diffs_with_significance <- as.data.table(diffs)
      diffs_with_significance[, `:=` (
        `TRUE - FALSE` = paste0(diffs[, `TRUE - FALSE`], significance[, `TRUE - FALSE`])
      )]
      
      diff_list[[col]] <- diffs_with_significance
      
    }
    
    diff_table <- rbindlist(diff_list, use.names = TRUE, fill = TRUE)
    
    # Function to interleave missing rows
    interleave_missing <- function(dt) {
      # Create a data.table with missing values
      dt_missing <- dt[1, ][, lapply(.SD, function(x) NA)]
      # Replicate the missing row to match the original data.table's length
      dt_missing <- dt_missing[rep(1, nrow(dt))]
      # Combine the original data.table and the missing rows
      dt_combined <- rbindlist(list(dt, dt_missing), use.names = TRUE)
      # Create an index to interleave the rows
      idx <- order(c(seq_len(nrow(dt)), seq_len(nrow(dt))))
      # Reorder the combined data.table by the interleaved index
      dt_interleaved <- dt_combined[idx]
      # Add an additional row of missing values at the end
      dt_interleaved <- rbind(dt_interleaved, dt_missing[1])
      return(dt_interleaved)
    }
    
    # Apply the function to the sample data.table
    diff_table <- interleave_missing(diff_table)
    
    return(diff_table)
    
  }
  
  # Apply the function to compute differences with significance for all value columns
  diffs_whole_sample <- compute_differences(whole_sample_stats, value_cols)
  diffs_below_median <- compute_differences(below_median_stats, value_cols)
  diffs_above_median <- compute_differences(above_median_stats, value_cols)
  
  # Compute the F-Stats
  compute_f_stats <- function(data, value_cols) {
    
    f_stats = data.table(
      `variable` = c("F-Stat"),
      `TRUE` = c(""), 
      `FALSE` = c(""), 
      `TRUE - FALSE` = round(fitstat(feols(data, as.formula(paste0("sms_delivery != TRUE", " ~ ", paste0(value_cols, collapse = " + ")))), type = "f")$f$stat, 3)
    )
    
    return(f_stats)
    
  }
  
  # Compute F-Stats for the differences
  f_stats_wole_sample = compute_f_stats(data_balance_sms, value_cols)
  f_stats_below_median = compute_f_stats(below_median_data, value_cols)
  f_stats_above_median = compute_f_stats(above_median_data, value_cols)
  
  # Combine formatted results and differences into one final table
  final_table <- cbind(
    rbindlist(list(
      Whole_Sample = formatted_whole_sample,
      Below_Median = formatted_below_median,
      Above_Median = formatted_above_median)),
    rbindlist(list(
      Differences_Whole_Sample = diffs_whole_sample,
      Differences_Below_Median = diffs_below_median,
      Differences_Above_Median = diffs_above_median
    )))
  
  table = rbind(final_table[1:11],  f_stats_wole_sample, final_table[12:22], f_stats_below_median, final_table[23:33], f_stats_above_median)
  
  
  # Output the table
  kable_output <- table %>%
    kable("latex", booktabs = TRUE, escape = FALSE, linesep = "", align = "lcccccccc") %>%
    kable_styling(latex_options = "hold_position", full_width = FALSE)
  
  # Convert the output to a single character vector split by newline
  kable_output <- unlist(strsplit(as.character(kable_output), "\n"))
  
  kable_output = gsub("NA", "", kable_output)
  kable_output = gsub("Observations", "\\\\textit{Observations}", kable_output)
  kable_output = gsub("F-Stat", "\\\\textit{F-Stat}", kable_output)
  kable_output = gsub("zero_filer_2023_se|months_since_registration_se|tot_obligation_began_2023_se|n_files_se|taxpayer_mean_monthly_tot_paid_se", "", kable_output)
  kable_output = gsub("zero_filer_2023_mean", "Zero Filing, 2023", kable_output)
  kable_output = gsub("months_since_registration_mean", "N of Months since registration", kable_output)
  kable_output = gsub("tot_obligation_began_2023_mean", "ToT Obligation began in 2023", kable_output)
  kable_output = gsub("n_files_mean", "N of Filings", kable_output)
  kable_output = gsub("taxpayer_mean_monthly_tot_paid_mean", "Avg ToT Paid, 2023", kable_output)
  
  test = c(
    kable_output[3],
    "\\tabularnewline \\midrule \\midrule",
    "& (1) & (2) & Difference \\\\",
    "& Sms Delivered & Sms Not Delivered & (1) - (2) \\\\ \n",
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Whole Sample}}} & & & \\\\ \n \\addlinespace ",
    kable_output[7:16], 
    "\\midrule", 
    kable_output[17:18],
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Below Median}}} & & & \\\\ \n \\addlinespace ",
    kable_output[19:28], 
    "\\midrule", 
    kable_output[29:30],
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Median and Above}}} & & & \\\\ \n \\addlinespace ",
    kable_output[31:40], 
    "\\midrule", 
    kable_output[41:44]
  )
  
  write_lines(test, file.path(table_output, "tbl_balance_sms.tex"))
  
}

{
  
  theme_aeqj <- function() {
    theme_minimal(base_family = "LM Roman 10", base_size = 15) +
      theme(
        
        # Text Size
        text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"), 
        plot.title = element_text(size = 14, colour = "black", face = "italic"), 
        plot.title.position = 'plot',
        
        # Grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        # Axis lines
        axis.line = element_line(color = "black", size = 0.4),
        axis.ticks = element_line(color = "black", size = 0.4),
        
        # Legend
        legend.position = "none",
        
        # Background
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        
        # Margins
        plot.margin = margin(25, 25, 10, 25)
      )
    
  }
  
  # Define the controls
  controls <- c(
    "l_taxpayer_mean_monthly_tot_paid", 
    "taxregion_mean_monthly_tot_paid", 
    "always_filer_2023", 
    "zero_filer_2023", 
    "tot_obligation_began_2023"
  )
  
  run_regression_iv <- function(outcome_var, data = matched_data, control_vars = controls, vcov_type = "hetero") {
    # Base formula for the fixed effects and controls
    base_formula <- paste(outcome_var, "~", paste(control_vars, collapse = " + "))
    
    # Instrument and its respective label
    instrument <- "sms_delivery ~ anchor"
    
    # Initialize a list to store models
    models <- list()
    
    # Define groups to run models for each quantile
    groups <- list(
      quantile_1 = data[inrange(decile_within_taxregion_pay, 1, 1)], 
      quantile_2 = data[inrange(decile_within_taxregion_pay, 2, 2)], 
      quantile_3 = data[inrange(decile_within_taxregion_pay, 3, 3)], 
      quantile_4 = data[inrange(decile_within_taxregion_pay, 4, 4)], 
      quantile_5 = data[inrange(decile_within_taxregion_pay, 5, 5)], 
      quantile_6 = data[inrange(decile_within_taxregion_pay, 6, 6)], 
      quantile_7 = data[inrange(decile_within_taxregion_pay, 7, 7)], 
      quantile_8 = data[inrange(decile_within_taxregion_pay, 8, 8)], 
      quantile_9 = data[inrange(decile_within_taxregion_pay, 9, 9)]
    )
    
    # Loop through each group
    for (grp in names(groups)) {
      formula_iv <- as.formula(paste(base_formula, "|", instrument))
      model_key <- paste("anchor", grp, sep = "_")
      models[[model_key]] <- feols(formula_iv, data = groups[[grp]], vcov = vcov_type)
    }
    
    return(models)
  }
  
  test <- run_regression_iv("log_tot_payments_2024_4")
  
  # Use lapply to tidy each model in the list and add the model name as a column
  tidy_results <- lapply(names(test), function(model_name) {
    tidy(test[[model_name]]) %>%
      mutate(model = model_name) %>%
      filter(grepl("fit", term)) %>%
      mutate(model_group = gsub(".*_", "", model))
  })
  
  # Combine the results into a single data frame
  combined_results <- bind_rows(tidy_results)
  
  critical_value <- qnorm(0.9)  
  
  ggplot(combined_results, aes(x = estimate, y = model_group, color = term, group = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#E53E3E", size = 1) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(xmin = estimate - critical_value * std.error, xmax = estimate + critical_value * std.error), 
                  width = 0.2, position = position_dodge(width = 0.5), size = 0.8) +
    theme_aeqj() +
    labs(
      x = "Estimated Effects",
      y = "",
      color = "Treatments",
      title = "Quantiles"
    ) +
    scale_color_manual(
      values = c("#13294B"),
      labels = c("Anchor")) +
    scale_y_discrete(labels = parse(text = c(
      "1" = "1^{st}",
      "2" = "2^{nd}",
      "3" = "3^{rd}",
      "4" = "4^{th}",
      "5" = "5^{th}",
      "6" = "6^{th}",
      "7" = "7^{th}",
      "8" = "8^{th}",
      "9" = "9^{th}"
    ))) 
  
  ggsave(
    filename = file.path(graph_output, paste0("quantile_effects.jpg")),
    plot = last_plot()                                                ,
    width    = 8                                            ,
    height   = 6                                            
  )
}

# Figure 16 ----

{
  
  
  # Define bin size
  bin_size <- 0.01
  
  # Create bins
  matched_data[, bin := cut(deviation_anchor_w, breaks = seq(-0.02, 0.02, by = bin_size), include.lowest = TRUE, right = FALSE)]
  
  # Compute number of observations per bin
  bin_counts <- matched_data[group != "Reminder", .N, by = .(bin, group)]
  
  # Plotting
  # For deviation_anchor_w with anchoring_interventions
  plot = ggplot() +
    geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1) +
    geom_histogram(data = matched_data[group == "Anchor" & inrange(deviation_anchor_w, -0.5, 0.5)], 
                   aes(x = deviation_anchor_w, y = ..count../sum(..count..)*100, fill = "Anchoring arms"), 
                   bins = 100, position = "identity", color = "black", alpha = .8) + 
    geom_histogram(data = matched_data[group == "Control" & inrange(deviation_anchor_w, -0.5, 0.5)], 
                   aes(x = deviation_anchor_w, y = ..count../sum(..count..)*100, fill = "Control"), 
                   bins = 100, position = "identity", color = "black", alpha = .8) +
    scale_fill_manual(values = c("Control" = "#32936F", "Anchoring arms" = "#E53E3E")) +
    labs(fill = "", y = "", x = "Deviation from Anchor") +
    theme_classic() +
    ggtitle("Share of Observations (%)") +
    theme_aeqj() +
    scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5.3)) +
    annotate(geom = "point"  , color = "#32936F", x = - 0.414, y = 2.31) +
    annotate(geom = "segment", color = "#32936F", x = - 0.414,  xend = - 0.414, y = 2.31, yend = 4.2) +
    annotate("text", x = - 0.3, y = 3.9, label = "Anchoring arms", size = 5, color = "#E53E3E") +
    annotate(geom = "point"  , color = "#E53E3E", x = - 0.3028, y = 2.65) +
    annotate(geom = "segment", color = "#E53E3E", x = - 0.3028,  xend = - 0.3028, y = 2.65, yend = 3.8) +
    annotate("text", x = - 0.405, y = 4.3, label = "Control", size = 5, color = "#32936F") 
  
  library(ggplot2)
  library(data.table)
  
  # Define the percentage range for deviation from the anchor
  percentage_range <- 0.02 # 2%
  
  # Sort the deviations
  sorted_deviations <- sort(abs(matched_data$deviation_anchor_w))
  
  # Calculate the index corresponding to the 2nd percentile
  percentile_index <- ceiling(0.02 * length(sorted_deviations))
  
  # Get the deviation value at the 2nd percentile
  deviation_threshold <- sorted_deviations[percentile_index]
  
  # Calculate the number of firms within the specified range of deviation
  within_range <- matched_data[abs(deviation_anchor_w) <= 0.02]
  
  # Calculate the percentage of firms within the range
  percentage_within_range <- nrow(within_range) / nrow(data) * 100
  
  # Print the result
  cat(sprintf("%.2f%% of firms declared revenue within %.2f%% of the anchor.\n", percentage_within_range, percentage_range * 100))
  
  # Filter the data based on the conditions used in the ggplot
  anchor_data <- matched_data[group == "Anchor" & inrange(deviation_anchor_w, -0.5, 0.5)]
  control_data <- matched_data[group == "Control" & inrange(deviation_anchor_w, -0.5, 0.5)]
  
  # Create a histogram and extract the data points
  anchor_hist <- hist(anchor_data$deviation_anchor_w, breaks = 100, plot = FALSE)
  control_hist <- hist(control_data$deviation_anchor_w, breaks = 100, plot = FALSE)
  
  # Convert histogram data to data frames
  anchor_df <- data.table(
    x = anchor_hist$mids,
    y = anchor_hist$counts,
    group = "Anchor"
  )
  
  control_df <- data.table(
    x = control_hist$mids,
    y = control_hist$counts ,
    group = "Control"
  )
  
  # Combine both data frames
  combined_df <- rbind(anchor_df, control_df)
  
  # Show the combined data frame
  combined_df
  
  ggsave(
    filename = file.path(graph_output, paste0("deviation_anchor.jpg")),
    plot = last_plot()                                                ,
    width    = 8                                            ,
    height   = 6                                            
  )
  
  
  
}