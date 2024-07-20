# ---------------------------------------------------------------------------- #
#                              7 Kenya Tax Project: pilot data                 #
#                                   World Bank - DIME                          #
#                                 .                                            #
# ---------------------------------------------------------------------------- #

# 0. Functions ----
{
  
  # Regression function for ITT
  run_regression_ols <- function(outcome_var, data = matched_data, control_vars = controls, heteroscedastic = TRUE) {
    
    # Initialize a list to store models
    models <- list()
    
    # Define the base formula for controls
    base_formula <- reformulate(control_vars, response = outcome_var)
    
    # Define groups to run models for
    groups <- list(
      pooled = data,
      below_median = data[below_median == TRUE],
      above_median = data[below_median == FALSE]
    )
    
    # Run models for each group
    for (group_name in names(groups)) {
      
      # Add 'treated' term to the formula
      formula_with_treatment <- update(base_formula, . ~ . + treated)
      models[[paste0("pooled_", group_name)]] <- feols(formula_with_treatment, data = groups[[group_name]], 
                                                       vcov = if (heteroscedastic) "hetero" else NULL)
      
      # Run regression by arm for the same group
      formula_with_group <- update(base_formula, . ~ . + i(group, ref = "Control"))
      models[[paste0("by_arm_", group_name)]] <- feols(formula_with_group, data = groups[[group_name]], 
                                                       vcov = if (heteroscedastic) "hetero" else NULL)
      
    }
    
    return(models)
  }
  
  # list of controls
  controls <- c(
    "l_taxpayer_mean_monthly_tot_paid", 
    "taxregion_mean_monthly_tot_paid", 
    "always_filer_2023", 
    "zero_filer_2023", 
    "tot_obligation_began_2023")
  
  # Function for regressions LATE IV 
  control_vars = controls
  run_regression_iv <- function(outcome_var, data = iv_data, control_vars = controls, vcov_type = "hetero") {
    # Base formula for the fixed effects and controls
    base_formula <- paste(outcome_var, "~", paste(control_vars, collapse = " + "))
    
    # Instruments and their respective labels
    instruments <- c("sms_delivery ~ treated", "basic_delivery + anchor_delivery ~ basic + anchor")
    labels <- c("pooled", "basic", "anchor")
    
    # Initialize a list to store models
    models <- list()
    
    # Define groups to run models for each instrument
    groups <- list(all = data, below_median = data[below_median == TRUE], above_median = data[below_median == FALSE])
    
    # Loop through each instrument and group
    for (inst in instruments) {
      for (grp in names(groups)) {
        formula_iv <- as.formula(paste(base_formula, "|", inst))
        model_key <- paste(sub(" ~ .*", "", inst), grp, sep = "_")
        models[[model_key]] <- feols(formula_iv, data = groups[[grp]], vcov = "hetero")
      }
    }
    
    return(models)
    
  }
  
  # Function to extract coefficients, ses, and p-valueso
  extract_fixest_results <- function(model_list, coefficient_name, coefficient_to_avoid = "") {
    
    result_list <- lapply(model_list, function(model) {
      
      # Define the names of coefficients to exclude
      exclude_coef <- c("(Intercept)", "l_taxpayer_mean_monthly_tot_paid", 
                        "taxregion_mean_monthly_tot_paid", "always_filer_2023", 
                        "zero_filer_2023", "tot_obligation_began_2023", coefficient_to_avoid)
      
      # Extract coefficients, standard errors, and p-values using broom::tidy
      model_tidy <- broom::tidy(model)
      
      # Round coefficients and p-values
      model_tidy <- model_tidy %>%
        mutate(
          Coefficient = coefficient_name,
          estimate = round(estimate, 3),
          std.error = paste0("(", round(std.error, 2), ")"),
          p.value = round(p.value, 3)
        )
      
      # Filter out the excluded coefficients
      model_filtered <- model_tidy %>%
        filter(!term %in% exclude_coef)
      
      # Extract the filtered coefficients, standard errors, and p-values
      coef_filtered <- model_filtered$estimate
      se_filtered <- model_filtered$std.error
      pvalues_filtered <- model_filtered$p.value
      
      tbl <- data.table(
        Coefficient = coefficient_name, 
        Estimate = coef_filtered,
        Std.Error = se_filtered,
        P.Value = pvalues_filtered
      )
      
      # Add asterisks based on p-values
      tbl[, Significance := fcase(
        P.Value <= 0.01, "***",
        P.Value <= 0.05, "**",
        P.Value <= 0.1 , "*",
        default = ""
      )]
      
      tbl[, Estimate := paste0(Estimate, Significance)]
      
      tbl[, Significance := NULL]  # Remove temporary column
      
      return(tbl)
      
    })
    
    result_table <- rbindlist(result_list, idcol = "Model")
    
    return(result_table)
    
  }
  
  # Create a custom function to format the table for LaTeX
  format_results_for_latex <- function(result_table) {
    
    
    # Create a new formatted_table
    formatted_table <- result_table[, {
      # Create a list of labels
      labels <- c("\\scriptsize $\\hat{\\beta}$", "\\scriptsize{\\textit{se}}", "\\scriptsize{\\textit{p-value}}")
      
      # Create a list for each row containing the label and the corresponding value
      # Ensuring that each value aligns with the right label
      list(
        Row = rep(labels, each = 1),  # Repeat each label once
        Value = as.character(c(Estimate, Std.Error, P.Value))  # Convert to character to ensure uniformity
      )
    }, by = .(Model, Coefficient)]
    
    
    formatted_table_wide <- formatted_table %>%
      
      pivot_wider(names_from = Model, values_from = Value, values_fn = list(Value = first))
    
    return(formatted_table_wide)
    
  }
  
  # Function to extract p-values from t-tests
  perform_t_test <- function(model) {
    
    # Convert the model call to a character string
    model_name <- deparse(substitute(model))
    
    # Initialize t_test_result
    t_test_result <- NA
    
    # Check for "ivs" or "ols" in the model name
    if (grepl("ivs", model_name, fixed = TRUE)) {
      t_test_result <- linearHypothesis(model, "fit_basic_deliveryTRUE - fit_anchor_deliveryTRUE = 0", test = "F")$`Pr(>F)`[2]
    } else if (grepl("ols", model_name, fixed = TRUE)) {
      t_test_result <- linearHypothesis(model, "group::Anchor - group::Reminder = 0", test = "F")$`Pr(>F)`[2]
    }
    
    # Format the t_test_result
    sprintf("& \\multicolumn{1}{c}{%s}", sprintf("%0.3f", round(t_test_result, 3)))
    
  }
  
  # Function to generate the kable output for a given table
  generate_kable_output <- function(latex_table) {
    kable_output <- latex_table %>%
      kable("latex", booktabs = TRUE, escape = FALSE, linesep = "", align = "c") %>%
      kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
      row_spec(0, bold = TRUE) %>% 
      row_spec(0, bold = TRUE) 
    
    # Convert the output to a single character vector split by newline
    kable_output <- unlist(strsplit(as.character(kable_output), "\n"))
    
    return(kable_output)
    
  }
  
}
# 1. Load Data ----

{
  
  matched_data <- fread(file.path(fin_data, "Experiment/data_reg_wide_scale_up.csv"))
  
}

# 2. Clean Data for Analysis ---- 
# 
{
  
  # Clean data for IV regressions
  iv_data = copy(matched_data)[,
                               ":="(
                                 basic_delivery   = fcase(basic == 1 & sms_delivery == TRUE,  TRUE,
                                                          basic == 0, FALSE, 
                                                          basic == 1 & sms_delivery == FALSE, FALSE,
                                                          is.na(basic), FALSE),
                                 anchor_delivery  = fcase(anchor == 1 & sms_delivery == TRUE,  TRUE,
                                                          anchor == 0, FALSE,
                                                          anchor == 1 & sms_delivery == FALSE, FALSE,
                                                          is.na(anchor), FALSE))][,
                                                                                  ":="(
                                                                                    basic  = ifelse(is.na(basic) , 1, 0),
                                                                                    anchor = ifelse(is.na(anchor), 1, 0))
                                                          ]
  
}

# 3. Perform Analysis: OLS, IV ----

{
  
  # Esimate OLS - ITT 
  model_results_ols_dummy <- lapply("dummy_payment_2024_4", run_regression_ols)
  model_results_ols_log   <- lapply("log_tot_payments_2024_4", run_regression_ols)
  
  # Esimate IV - LATE
  model_results_ivs_dummy <- lapply("dummy_payment_2024_4", run_regression_iv)
  model_results_ivs_log   <- lapply("log_tot_payments_2024_4", run_regression_iv)
  
}

# 5. Panel A: Whole Sample ----

{
  
  # Define model results lists
  model_lists <- list(
    ols_dummy = model_results_ols_dummy[[1]],
    ivs_dummy = model_results_ivs_dummy[[1]],
    ols_log = model_results_ols_log[[1]],
    ivs_log = model_results_ivs_log[[1]]
  )
  
  # Any Treatment vs Reminder (p-values)
  t_basic_anchor <- paste0(
    "\\multicolumn{2}{l}{\\textit{p-value} (Reminder vs Anchor)}",
    perform_t_test(model_lists$ols_dummy$by_arm_pooled),
    perform_t_test(model_lists$ivs_dummy$`basic_delivery + anchor_delivery_all`),
    perform_t_test(model_lists$ols_log$by_arm_pooled), 
    perform_t_test(model_lists$ivs_log$`basic_delivery + anchor_delivery_all`),
    "\\\\"
  )
  
  # Extract and format results
  results_1 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$pooled_pooled,
      model_results_ivs_dummy[[1]]$sms_delivery_all,
      model_results_ols_log[[1]]$pooled_pooled,
      model_results_ivs_log[[1]]$sms_delivery_all
    ),
    coefficient_name = "Any Treatment"
  )
  
  results_2 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_pooled,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_all`,
      model_results_ols_log[[1]]$by_arm_pooled,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_all`
    ),
    coefficient_name = "Reminder",
    coefficient_to_avoid = c("group::Anchor",  "fit_anchor_deliveryTRUE")
  )
  
  results_3 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_pooled,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_all`,
      model_results_ols_log[[1]]$by_arm_pooled,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_all`
    ),
    coefficient_name = "Anchor",
    coefficient_to_avoid = c("group::Reminder", "fit_basic_deliveryTRUE")
  )
  
  latex_table_1 <- format_results_for_latex(results_1)
  latex_table_2 <- format_results_for_latex(results_2)
  latex_table_3 <- format_results_for_latex(results_3)
  
}

# 6. Panel A: Below Median ----

{
  
  # "H0: Reminder - Anchor = 0" (p-values)
  t_basic_anchor_below <- paste0(
    "\\multicolumn{2}{l}{\\textit{p-value} (Reminder vs Anchor)}",
    perform_t_test(model_lists$ols_dummy$by_arm_below_median),
    perform_t_test(model_lists$ivs_dummy$`basic_delivery + anchor_delivery_below_median`),
    perform_t_test(model_lists$ols_log$by_arm_below_median), 
    perform_t_test(model_lists$ivs_log$`basic_delivery + anchor_delivery_below_median`),
    "\\\\"
  )
  
  # Extract and format results
  results_4 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$pooled_below_median,
      model_results_ivs_dummy[[1]]$sms_delivery_below_median,
      model_results_ols_log[[1]]$pooled_below_median,
      model_results_ivs_log[[1]]$sms_delivery_below_median
    ),
    coefficient_name = "Any Treatment"
  )
  
  results_5 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_below_median,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_below_median`,
      model_results_ols_log[[1]]$by_arm_below_median,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_below_median`
    ),
    coefficient_name = "Reminder",
    coefficient_to_avoid = c("group::Anchor", "fit_anchor_deliveryTRUE")
  )
  
  results_6 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_below_median,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_below_median`,
      model_results_ols_log[[1]]$by_arm_below_median,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_below_median`
    ),
    coefficient_name = "Anchor",
    coefficient_to_avoid = c("group::Reminder", "fit_basic_deliveryTRUE")
  )
  
  latex_table_4 <- format_results_for_latex(results_4)
  latex_table_5 <- format_results_for_latex(results_5)
  latex_table_6 <- format_results_for_latex(results_6)
  
}

# 7. Panel B: Median and Above ----

{
  
  # "H0: Reminder - Anchor = 0" (p-values)
  t_basic_anchor_above <- paste0(
    "\\multicolumn{2}{l}{\\textit{p-value} (Reminder vs Anchor)}",
    perform_t_test(model_lists$ols_log$by_arm_above_median),
    perform_t_test(model_lists$ivs_log$`basic_delivery + anchor_delivery_above_median`),
    perform_t_test(model_lists$ols_dummy$by_arm_above_median), 
    perform_t_test(model_lists$ivs_dummy$`basic_delivery + anchor_delivery_above_median`),
    "\\\\"
  )
  
  # Extract and format results
  results_7 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$pooled_above_median,
      model_results_ivs_dummy[[1]]$sms_delivery_above_median,
      model_results_ols_log[[1]]$pooled_above_median,
      model_results_ivs_log[[1]]$sms_delivery_above_median
    ),
    coefficient_name = "Any Treatment"
  )
  
  results_8 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_above_median,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_above_median`,
      model_results_ols_log[[1]]$by_arm_above_median,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_above_median`
    ),
    coefficient_name = "Reminder",
    coefficient_to_avoid = c("group::Anchor", "fit_anchor_deliveryTRUE")
  )
  
  results_9 <- extract_fixest_results(
    list(
      model_results_ols_dummy[[1]]$by_arm_above_median,
      model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_above_median`,
      model_results_ols_log[[1]]$by_arm_above_median,
      model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_above_median`
    ),
    coefficient_name = "Anchor",
    coefficient_to_avoid = c("group::Reminder", "fit_basic_deliveryTRUE")
  )
  
  latex_table_7 <- format_results_for_latex(results_7)
  latex_table_8 <- format_results_for_latex(results_8)
  latex_table_9 <- format_results_for_latex(results_9)
  
}

# 8. Observations and Control Mean ---- 

{
  
  # We compute the mean of control for each model
  mean_whole_sample = paste0(
    "\\multicolumn{2}{l}{Control Mean} &",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "group")) %>% na.omit() %>% filter(group == "Control"))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "basic", "group")) %>% na.omit() %>% filter(group == "Control"))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "group")) %>% na.omit() %>% filter(group == "Control"))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "basic", "group")) %>% na.omit() %>% filter(group == "Control"))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "\\\\"
  )
  
  mean_below_median = paste0(
    "\\multicolumn{2}{l}{Control Mean} &",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == TRUE))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "basic", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == TRUE))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == TRUE))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "basic", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == TRUE))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "\\\\"
  )
  
  mean_above_median = paste0(
    "\\multicolumn{2}{l}{Control Mean} &",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == FALSE))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "dummy_payment_2024_4", "basic", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == FALSE))$dummy_payment_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == FALSE))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "&",
    sprintf("%0.3f", round(mean((matched_data %>% select(c(control_vars, "log_tot_payments_2024_4", "basic", "group", "below_median")) %>% na.omit() %>% filter(group == "Control" & below_median == FALSE))$log_tot_payments_2024_4, na.rm = TRUE), 3)),
    "\\\\"
  )
  
  # We count the number of obeservations for each model
  nobs_whole_sample = paste0(
    "\\multicolumn{2}{l}{Observations} &",
    format(model_results_ols_dummy[[1]]$pooled_pooled$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_all`$nobs, big.mark = ","),
    "&",
    format(model_results_ols_log[[1]]$pooled_pooled$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_all`$nobs, big.mark = ","),
    "\\\\"
  )
  
  nobs_below_median = paste0(
    "\\multicolumn{2}{l}{Observations} &",
    format(model_results_ols_dummy[[1]]$by_arm_below_median$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_below_median`$nobs, big.mark = ","),
    "&",
    format(model_results_ols_log[[1]]$by_arm_below_median$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_below_median`$nobs, big.mark = ","),
    "\\\\"
  )
  
  nobs_above_median = paste0(
    "\\multicolumn{2}{l}{Observations} &",
    format(model_results_ols_dummy[[1]]$by_arm_above_median$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_dummy[[1]]$`basic_delivery + anchor_delivery_above_median`$nobs, big.mark = ","),
    "&",
    format(model_results_ols_log[[1]]$by_arm_above_median$nobs, big.mark = ","),
    "&",
    format(model_results_ivs_log[[1]]$`basic_delivery + anchor_delivery_above_median`$nobs, big.mark = ","),
    "\\\\"
  )
  
}

# 9. Table With Main Results ----

{
  
  # List all latex tables
  latex_tables <- list(latex_table_1, latex_table_2, latex_table_3,
                       latex_table_4, latex_table_5, latex_table_6,
                       latex_table_7, latex_table_8, latex_table_9)
  
  # Apply the function to each table
  kable_outputs <- lapply(latex_tables, generate_kable_output)
  
  # To access them individually later:
  list2env(setNames(kable_outputs, paste0("kable_output_", 1:9)), envir = .GlobalEnv)
  
  # Combine all parts
  kable_output_1[4] <- "\\tabularnewline \\midrule \\midrule \n \\addlinespace \n \\addlinespace \n"
  kable_output <- c(
    kable_output_1[1:4], 
    "\\multicolumn{1}{c}{ } & \\multicolumn{1}{c}{ } & \\multicolumn{2}{c}{$\\mathbbm{1}$\\{\\textit{Payments} $>$ 0\\}} & \\multicolumn{2}{c}{\\textit{Log}({Payments})} \\\\\n", 
    "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}", 
    " &  & \\multicolumn{1}{c}{\\textit{OLS}} & \\multicolumn{1}{c}{\\textit{IV}} & \\multicolumn{1}{c}{\\textit{OLS}} & \\multicolumn{1}{c}{\\textit{IV}} \\\\\n",
    kable_output_1[6],
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel A: Whole Sample}}} &  &  &  & &\\\\ \n \\addlinespace ",
    kable_output_1[7: 9],
    kable_output_2[7: 9],
    kable_output_3[7: 9], 
    "\\midrule", 
    t_basic_anchor,
    mean_whole_sample,
    nobs_whole_sample,
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel B: Below Median}}} &  &  &  & &\\\\ \n \\addlinespace",
    kable_output_4[7: 9],
    kable_output_5[7: 9],
    kable_output_6[7: 9],
    "\\midrule", 
    t_basic_anchor_below,
    mean_below_median,
    nobs_below_median,
    "\\midrule", 
    "\\multicolumn{1}{l}{\\underline{\\textit{Panel C: Median and Above}}} &  &  &  & &\\\\ \n \\addlinespace",
    kable_output_7[7: 9],
    kable_output_8[7: 9],
    kable_output_9[7: 9],
    "\\midrule", 
    t_basic_anchor_above,
    mean_above_median,
    nobs_above_median,
    "\\midrule",
    kable_output_1[10:length(kable_output_1)]
  )
  kable_output[length(kable_output) - 2] <- "\\midrule \\midrule\n\\multicolumn{6}{l}{\\emph{Heteroskedasticity-robust standard-errors in parentheses}}\\\\\n\\multicolumn{6}{l}{\\emph{Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}}\\\\\n"
  
  for (i in c(10:18, 25:33, 40:48)) { # Adjust for specific rows
    kable_output[i] <- gsub(
      "([^&]+) & ([^&]+) & ([^&]+) & ([^&]+) & ([^&]+) & ([^\\\\]+)\\\\\\\\",
      "\\\\multicolumn{1}{l}{\\1} & \\\\multicolumn{1}{c}{\\2} & \\\\multicolumn{1}{c}{\\3} & \\\\multicolumn{1}{c}{\\4} & \\\\multicolumn{1}{c}{\\5} & \\\\multicolumn{1}{c}{\\6} \\\\\\\\",
      kable_output[i]
    )
    kable_output[i] <- gsub(
      "Panel A: Whole Sample",
      "",
      kable_output[i]
    )
    if (!i %in% c(10, 13, 16, 25, 28, 31, 40, 43, 46)) {
      kable_output[i] <- gsub(
        "Anchor",
        "",
        kable_output[i]
      )
      kable_output[i] <- gsub(
        "Any Treatment",
        "",
        kable_output[i]
      )
      kable_output[i] <- gsub(
        "Reminder",
        "",
        kable_output[i]
      )}
  }
  
  kable_output = kable_output[4:length(kable_output)-1]
  
  write_lines(kable_output, file.path(table_output, "tbl_main_results.tex"))
  
  
  # Print the complete table
  cat(kable_output)
  
}

