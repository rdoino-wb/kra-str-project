# ---------------------------------------------------------------------------- #
#                               2 tax data:  visualizations                   #
#                                   World Bank - DIME                          #
#                                                                              #
# ---------------------------------------------------------------------------- #

# 0: Load the data ----

{
  
  # Read monthly payment-tot data
  data      = fread(file = file.path(fin_data, "dt_month_tot_pay.csv")  , na.strings = c("NA", ""))
  
  # Read yearly payment-tot data
  data_year = fread(file = file.path(fin_data, "dt_year_tot_pay.csv"), na.strings = c("NA", ""))
  
  # Read firm-level darta
  firm = fread(file = file.path(fin_data, "dt_firm_tot_pay.csv"), na.strings = c("NA", ""))
  
  # Read json map data for Africa
  african_countries <- st_read(file.path(raw_data, "custom.geo.json"))
  
  # Read VAT data
  data_vat = fread(file = file.path(fin_data, "dt_vat.csv"), na.strings = c("NA", ""))
  
}

# Figure 1: Spatial Distribution of Simplified Tax Regimes in Sub-Saharan Africa ---- 

{
  
  # We first need to do some cleaning 
  
  # We add labels to each country based on Author Computations
  setDT(african_countries)[, 
                           tax_category := fifelse(
                             geounit %in% c("Ethiopia",
                                            "Kenya",
                                            "Madagascar",
                                            "Malawi",
                                            "Mauritius",
                                            "Mozambique",
                                            "Rwanda",
                                            "Seychelles",
                                            "Tanzania",
                                            "Uganda",
                                            "Zambia",
                                            "Zimbabwe",
                                            "Eswatini",
                                            "South Africa",
                                            "São Tomé and Principe",
                                            "Angola",
                                            "Cameroon",
                                            "Central African Republic",
                                            "Chad",
                                            "Gabon",
                                            "São Tomé and Príncipe",
                                            "Benin",
                                            "Burkina Faso",
                                            "Cabo Verde",
                                            "Côte d'Ivoire",
                                            "Ghana",
                                            "Liberia",
                                            "Mali",
                                            "Niger",
                                            "Senegal",
                                            "Sierra Leone",
                                            "Togo"), "STR (without minimum threshold)", "No STR")
  ][,
    tax_category := fifelse(
      tax_category == "STR (without minimum threshold)", 
      fifelse(
        geounit %in% c(
          "Angola",
          "Benin",
          "Burkina Faso",
          "Cabo Verde",
          "Chad",
          "Côte d'Ivoire",
          "Ethiopia",
          "Gabon",
          "Liberia",
          "Madagascar",
          "Mali",
          "Mauritius",
          "Mozambique",
          "Niger",
          "São Tomé and Príncipe",
          "Senegal",
          "Seychelles",
          "Zimbabwe"
        ), "STR (without minimum threshold)", "STR (with minimum threshold)"
      ), "No STR"
    )][
      geounit %in% c("Morocco", "Western Sahara", "Algeria", "Libya", "Tunisia", "Egypt"),
      no_ssa := TRUE
    ]
  
  african_countries$tax_category <- factor(african_countries$tax_category,
                                           levels = c("STR (with minimum threshold)",
                                                      "STR (without minimum threshold)",
                                                      "No STR"))
  
  african_countries <- st_as_sf(african_countries, sf_column_name = "geometry")
  
  # Then we can plot the map
  ggplot() +
    theme_void() +  # Set the theme to void for a clean map without axes
    geom_sf(
      data = african_countries %>% filter(no_ssa == TRUE),  # Filter non-SSA countries
      colour = "gray90",  # Set the border color for non-SSA countries
      fill = "gray99"     # Set the fill color for non-SSA countries
    ) + 
    geom_sf(
      data = african_countries %>% filter(is.na(no_ssa)),  # Filter SSA countries
      aes(fill = tax_category),  # Fill SSA countries based on tax category
      color = "#555555",  # Set the border color for SSA countries
      size = 0.01         # Set the border size for SSA countries
    ) +
    scale_fill_manual(
      values = c(
        "No STR" = "gray92",
        "STR (without minimum threshold)" = "#9ecae1",
        "STR (with minimum threshold)" = "#3182bd"
      ),
      name = ""  # Remove legend title
    ) +
    coord_sf(
      xlim = c(-20, 60),  # Set longitude limits
      ylim = c(-40, 40),  # Set latitude limits
      expand = FALSE      # Disable coordinate expansion
    ) +
    theme_void() +  # Apply the void theme again to ensure no axes are shown
    theme(
      plot.background = element_blank(),  # Remove plot background
      panel.border = element_blank(),     # Remove border around the plot
      axis.text = element_blank(),        # Remove axis text
      axis.ticks = element_blank(),       # Remove axis ticks
      axis.title = element_blank(),       # Remove axis titles
      plot.margin = margin(0, 0, 10, 0),  # Set plot margin
      legend.position = "bottom",         # Position legend at the bottom
      legend.text = element_text(size = 15, family = "LM Roman 10")  # Set legend text properties
    )
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "map.png"),  # Set file path and name
    plot = last_plot(),                             # Save the last plotted graph
    width = 8,                                      # Set width of the saved plot
    height = 10                                     # Set height of the saved plot
  )
  
}

# Figure 2: ----

{
  
  # Create a data frame based on Author's computations
  plot_data <- data.frame(
    Country = c("Ethiopia", "Kenya", "Madagascar", "Mauritius", "Mozambique", "Rwanda", "Seychelles", 
                "Tanzania", "Uganda", "Zambia", "Eswatini", "South Africa", "Angola", "Cameroon", 
                "Central Africa", "Chad", "Gabon", "Benin", "Cabo Verde", "Côte d'Ivoire", "Ghana", 
                "Liberia", "Mali", "Niger", "Senegal", "Sierra Leone", "Togo"),
    Percentage = c(6.67, 10, 18, 7, 9.38, 10, 6, 10, 1.33, 13, 5, 3.7, 8, 10.71, 6, 4, 3, 4, 19, 8, 
                   12, 16, 10, 10.65, 6.67, 8, 9.26),
    stringsAsFactors = FALSE
  )
  
  # Calculate the average percentage
  average_percentage <- mean(plot_data$Percentage)
  
  # Add a row for the average
  plot_data <- rbind(plot_data, data.frame(Country = "Average", Percentage = average_percentage))
  
  # Plot the data
  ggplot(plot_data, aes(x = reorder(Country, Percentage), y = Percentage, fill = Country == "Average")) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Create bars without legend
    scale_fill_manual(values = c("#32936F", "#FFBE0B")) +  # Define colors for bars
    labs(title = "Break-even Profit (%)", x = NULL, y = "") +  # Add plot title and remove axis labels
    theme_aeqj() +  # Apply a custom theme
    theme(
      axis.title.y = element_blank(),  # Remove Y-axis title
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, face = "italic"),  # Customize X-axis text
      axis.ticks.length = unit(0.25, "cm"),  # Set length of axis ticks
      axis.ticks.y = element_line(size = 0.5),  # Set size of Y-axis ticks
      axis.line.y = element_line(size = 0.5),  # Set size of Y-axis line
      panel.grid.major.x = element_blank(),  # Remove major grid lines on X-axis
      panel.grid.minor.x = element_blank()   # Remove minor grid lines on X-axis
    ) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, max(plot_data$Percentage), by = 5)) +  # Customize Y-axis
    scale_x_discrete(expand = c(0.05, 0.05))  # Ensure ticks are between bars
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "profit_rate_hist.png"),  # Set file path and name
    plot = last_plot(),                             # Save the last plotted graph
    width = 8,                                      # Set width of the saved plot
    height = 5                                     # Set height of the saved plot
  )
  
}

# Figure 3: ----

{
  
  # Create a data table of unique filers from 2020 to 2023
  filers = unique(data[dt_year %in% seq(2020, 2023)], by = c("tax_payer_id", "dt_year", "dt_month"))[, .N, by = .(dt_month, dt_year)]
  
  # Prepare the plot data with YearMonth column
  plot_data = copy(filers)[, 
                           YearMonth := ymd(
                             paste0(
                               "2023", fifelse(nchar(dt_month) == 1, paste0("0", dt_month), paste0(dt_month)), "01"))
  ]
  
  # Plot the data
  plot_data %>%
    ggplot(aes(x = YearMonth, 
               y = N / 1000,  # Scale the number of filers to thousands
               colour = as.factor(dt_year))) +  # Use year as color factor
    geom_line(size = 1) +  # Add line graph
    geom_point(size = 3, shape = 19) +  # Add points to the line graph
    scale_colour_manual(values = c("#13294B", "#E53E3E", "#FFBE0B", "#32936F")) +  # Define custom colors for lines
    scale_y_continuous(breaks = seq(3, 15, 3), expand = c(0, 0), limits = c(0, 17)) +  # Customize Y-axis
    scale_x_date(date_breaks = "2 month", labels = function(x) format(x, "%b")) +  # Customize X-axis with month labels
    labs(x = "", y = "", title = "Firms filing ToT (thousands)") +  # Add plot title and remove axis labels
    # Adding labels to the end of each line
    geom_text(data = plot_data %>% filter(YearMonth == min(YearMonth)), 
              aes(label = dt_year, hjust = 0.4, vjust = -.6),  # Adjust hjust for label positioning
              size = 5.4,  # Adjust size of the text as needed
              show.legend = FALSE, family = "LM Roman 10") +  # Set text properties
    theme_aeqj() +  # Apply custom theme
    coord_cartesian(expand = TRUE, clip = 'off')  # Allow text to extend beyond plot limits
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "filers_months.png"),  # Set file path and name
    plot = last_plot(),                                       # Save the last plotted graph
    width = 6,                                                # Set width of the saved plot
    height = 4                                                # Set height of the saved plot
  )
  
}

# Figure 4: ----

{
  
  # Prepare the plot data
  plot_data = copy(data[dt_year %in% c(2016, 2018, 2020, 2023)])
  
  # Compute the turnover at the year-firm-level
  data_firm_year = plot_data[, .(
    tot_turnover = sum(tot_turnover, na.rm = TRUE), 
    net_tax      = sum(net_tax     , na.rm = TRUE)
  ), by = .(dt_year, tax_payer_id, stable_dummy, balanced_dummy)]
  
  # Define tax brackets and highlight taxable brackets
  data_firm_year = data_firm_year[, 
                                  brackets_tot_tax := cut(
                                    tot_turnover,
                                    c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,
                                    labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "> 50m")
                                  )][, 
                                     bracket_highlight_tax := fcase(
                                       tot_turnover < 5000000  & tot_turnover > 500000  & dt_year %in% seq(2015, 2019), "Taxable",
                                       tot_turnover < 50000000 & tot_turnover > 1000000 & dt_year %in% seq(2020, 2023), "Taxable",
                                       default = "Non Taxable"
                                     )
                                  ]
  
  # Calculate proportions for the plot data
  plot_data = data_firm_year[dt_year %in% c(2016, 2018, 2020, 2023)][, 
                                                                     N := .N, by = .(dt_year)][, 
                                                                                               .(prop = .N / N), by = .(brackets_tot_tax, N, dt_year, bracket_highlight_tax)]
  
  # Plot the data
  plot_data %>% 
    ggplot(aes(y = prop, x = brackets_tot_tax, fill = bracket_highlight_tax)) + 
    ggtitle("") +  # Add a title to the plot
    geom_bar(stat = "identity", colour = "black", size = 0.2) +  # Create bars with borders
    facet_wrap(dt_year ~ .) +  # Create facets for each year
    scale_y_continuous(
      breaks = seq(0.2, 0.8, 0.2),  # Set Y-axis breaks
      limits = c(0, 0.85),          # Set Y-axis limits
      expand = c(0, 0)              # Remove space between axis and data
    ) +
    scale_x_discrete(expand = c(0.2, -0.2)) +  # Adjust space around X-axis labels
    xlab("") +  # Remove X-axis label
    ylab("") +  # Remove Y-axis label
    theme_aeqj() +  # Apply custom theme
    theme(
      legend.position = "top",  # Position legend at the top
      axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2, size = 14),  # Customize X-axis text
      strip.background = element_rect(fill = "gray90", colour = "black", size = 0.5),  # Customize facet strip background
      strip.text = element_text(size = 14, face = "bold", margin = margin(t = 1, b = 1, l = 4, r = 4))  # Customize facet strip text
    ) +
    scale_fill_manual(name = "", values = c("Taxable" = "#FFBE0B", "Non Taxable" = "white")) +  # Define colors for fill
    labs("")  # Remove any additional labels
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "brackets_tot_tax.png"),  # Set file path and name
    plot = last_plot(),                                          # Save the last plotted graph
    width = 6,                                                   # Set width of the saved plot
    height = 5                                                   # Set height of the saved plot
  )
  
}

# Figure 5 ----

{

  # Copy the original data.table and compute total net_tax and count by year and tax bracket
  data_firm_year_processed <- copy(data_year)[, .(
    tot = sum(net_tax, na.rm = TRUE),   # Sum of net_tax with NA values removed
    N_sum = .N                          # Count of rows in each group
  ), by = .(dt_year, bracket_highlight_tax)]
  
  # Calculate the share of total tax and the share of count for each year
  data_firm_year_processed[, `:=` (
    share_tax = tot / sum(tot),          # Share of tax calculated within each year
    share_N = N_sum / sum(N_sum)         # Share of counts calculated within each year
  ), by = dt_year]
  
  # Arrange the data by dt_year and convert back to data.table if needed
  data_firm_year_processed <- data_firm_year_processed[order(dt_year)]
  
  # Plot the data
  ggplot(data_firm_year_processed[bracket_highlight_tax == "Non Taxable" & dt_year != 2019], 
         aes(x = dt_year, y = share_tax)) +
    geom_line(size = 1, colour = "#13294B") +  # Line plot
    geom_point(size = 3, shape = 19, colour = "#13294B") +  # Points on the line
    scale_y_continuous(
      breaks = seq(0.1, 0.6, 0.1),  # Set Y-axis breaks
      expand = c(0, 0),             # Remove space between axis and data
      limits = c(0, 0.62)           # Set Y-axis limits
    ) +
    scale_x_continuous(
      breaks = seq(2015, 2024, 2),  # Set X-axis breaks
      limits = c(2014, 2024)        # Set X-axis limits
    ) +
    labs(x = "", y = "", title = "Share of Tax Below Exemption Level") +  # Add plot title
    theme_aeqj()  # Apply custom theme
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "tax_exemption_line.png"),  # Set file path and name
    plot = last_plot(),                                            # Save the last plotted graph
    width = 6,                                                     # Set width of the saved plot
    height = 4                                                     # Set height of the saved plot
  )
  
}

# Figure 6: ----

{
  
  # Copy the original data.table and adjust the total turnover for firms with less than 12 months filed
  data_firm_year_processed <- copy(data_year[!is.na(n_months_filed)])[
    n_months_filed != 12, 
    tot_turnover := (tot_turnover / n_months_filed) * 12
  ]
  
  # Define tax brackets and highlight taxable brackets
  data_firm_year_processed[, brackets_tot_tax := cut(
    tot_turnover,
    c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,
    labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "Over 50m")
  )][,
     bracket_highlight_tax := fcase(
       tot_turnover < 5000000  & tot_turnover > 500000  & dt_year %in% seq(2015, 2019), "Taxable",
       tot_turnover < 50000000 & tot_turnover > 1000000 & dt_year %in% seq(2020, 2023), "Taxable",
       default = "Non Taxable"
     )
  ]
  
  # Process data for firms with 12 months filed
  data_firm_year_processed_1 <- data_firm_year_processed[, .(
    tot = sum(net_tax, na.rm = TRUE),   # Sum of net_tax with NA values removed
    N_sum = .N                          # Count of rows in each group
  ), by = .(dt_year, bracket_highlight_tax)]
  
  # Calculate the share of total tax and the share of count for each year
  data_firm_year_processed_1[, `:=` (
    share_tax = tot / sum(tot),          # Share of tax calculated within each year
    share_N = N_sum / sum(N_sum)         # Share of counts calculated within each year
  ), by = dt_year]
  
  # Arrange the data by dt_year
  data_firm_year_processed_1 <- data_firm_year_processed_1[order(dt_year)]
  
  # Process data for firms with less than 12 months filed
  data_firm_year_processed_2 <- copy(data_year)[, .(
    tot = sum(net_tax, na.rm = TRUE),   # Sum of net_tax with NA values removed
    N_sum = .N                          # Count of rows in each group
  ), by = .(dt_year, bracket_highlight_tax)]
  
  # Calculate the share of total tax and the share of count for each year
  data_firm_year_processed_2[, `:=` (
    share_tax = tot / sum(tot),          # Share of tax calculated within each year
    share_N = N_sum / sum(N_sum)         # Share of counts calculated within each year
  ), by = dt_year]
  
  # Arrange the data by dt_year
  data_firm_year_processed_2 <- data_firm_year_processed_2[order(dt_year)]
  
  # Plot the data
  ggplot() +
    geom_line(data = data_firm_year_processed_1[bracket_highlight_tax == "Non Taxable" & dt_year != 2019], 
              aes(x = dt_year, y = share_tax),
              size = 1, colour = "#32936F") +
    geom_point(data = data_firm_year_processed_1[bracket_highlight_tax == "Non Taxable" & dt_year != 2019], 
               aes(x = dt_year, y = share_tax),
               size = 3, shape = 19, colour = "#32936F") +
    geom_line(data = data_firm_year_processed_2[bracket_highlight_tax == "Non Taxable" & dt_year != 2019], 
              aes(x = dt_year, y = share_tax),
              size = 1, colour = "#FFBE0B") +
    geom_point(data = data_firm_year_processed_2[bracket_highlight_tax == "Non Taxable" & dt_year != 2019], 
               aes(x = dt_year, y = share_tax),
               size = 3, shape = 19, colour = "#FFBE0B") + 
    geom_text(data = data_firm_year_processed_1 %>% filter(bracket_highlight_tax == "Non Taxable" & dt_year == min(dt_year)), 
              aes(x = dt_year, y = share_tax, label = "Balanced", hjust = .7, vjust = 2.5),  # Adjust hjust for label positioning
              size = 5,  # Adjust size of the text as needed
              colour = "#32936F",
              show.legend = FALSE, family = "LM Roman 10") + 
    geom_text(data = data_firm_year_processed_2 %>% filter(bracket_highlight_tax == "Non Taxable" & dt_year == min(dt_year)), 
              aes(x = dt_year, y = share_tax, label = "Annualized", hjust = .6, vjust = -2.5),  # Adjust hjust for label positioning
              size = 5,  # Adjust size of the text as needed
              colour = "#FFBE0B",
              show.legend = FALSE, family = "LM Roman 10") + 
    scale_y_continuous(breaks = seq(0.1, 0.6, 0.1),
                       expand = c(0, 0),
                       limits = c(0, 0.62)) +
    scale_x_continuous(breaks = seq(2015, 2024, 2), 
                       limits = c(2014, 2024)) +
    labs(x = "", y = "", title = "Share of Tax Below Exemption Level") +
    theme_aeqj()
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "tax_exemption_line_two.png"),  # Set file path and name
    plot = last_plot(),                                                # Save the last plotted graph
    width = 6,                                                         # Set width of the saved plot
    height = 4                                                         # Set height of the saved plot
  )
  
}

# Figure 7: ----

{
  
  # Copy the original data and filter for the years of interest
  plot_data = copy(data[dt_year %in% c(2016, 2018, 2020, 2023)])
  
  # Compute the turnover at the year-firm level
  data_firm_year = plot_data[, .(
    tot_turnover = sum(tot_turnover, na.rm = TRUE), 
    net_tax      = sum(net_tax     , na.rm = TRUE)
  ), keyby = .(dt_year, tax_payer_id, stable_dummy, balanced_dummy)]
  
  # Define tax brackets and highlight taxable brackets
  data_firm_year = data_firm_year[, 
                                  brackets_tot_tax := cut(
                                    tot_turnover,
                                    c(0, 50000, 500000, 1000000, 5000000, 50000000, Inf), include.lowest = TRUE,
                                    labels = c("0 - 50k", "50k - 500k", "500k - 1m", "1m - 5m", "5m - 50m", "> 50m")
                                  )][,
                                     bracket_highlight_tax := fcase(
                                       tot_turnover < 5000000  & tot_turnover > 500000  & dt_year %in% seq(2015, 2019), "Taxable",
                                       tot_turnover < 50000000 & tot_turnover > 1000000 & dt_year %in% seq(2020, 2023), "Taxable",
                                       default = "Non Taxable"
                                     )
                                  ]
  
  # Create a copy of the data for plotting
  data_plot = copy(data_firm_year[, dt_year_2 := dt_year])
  
  # Calculate density for each year
  data_plot <- data_plot[, list(x = density(log(tot_turnover))$x, y = density(log(tot_turnover))$y), by = "dt_year"]
  
  # Plot the data
  data_plot %>% 
    ggplot() +
    geom_ribbon(data = subset(data_plot, ifelse(dt_year == 2016, x < log(500000), x < log(1000000))),
                aes(x = x, ymax = y), ymin = 0, fill = "#FFBE0B") +  # Highlight regions under density curves
    geom_line(aes(x = x, y = y), colour = "black", size = 0.2) +  # Add density lines
    ggtitle("") +  # Add a title to the plot
    facet_wrap(~ dt_year) +  # Create facets for each year
    scale_y_continuous(
      breaks = seq(0.1, 0.4, 0.1),  # Set Y-axis breaks
      limits = c(0, 0.45),          # Set Y-axis limits
      expand = c(0, 0)              # Remove space between axis and data
    ) +
    scale_x_continuous(
      limits = c(5, 18),            # Set X-axis limits
      breaks = seq(6, 21, 3)        # Set X-axis breaks
    ) +
    xlab("") +  # Remove X-axis label
    ylab("") +  # Remove Y-axis label
    theme_aeqj() +  # Apply custom theme
    theme(
      strip.background = element_rect(fill = "gray90", colour = "black", size = 0.5),  # Customize facet strip background
      strip.text = element_text(size = 14, face = "bold", margin = margin(t = 1, b = 1, l = 4, r = 4))  # Customize facet strip text
    )
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "density_log_tot_tax.png"),  # Set file path and name
    plot = last_plot(),                                              # Save the last plotted graph
    width = 6,                                                       # Set width of the saved plot
    height = 4                                                       # Set height of the saved plot
  )
  
}

# Figure 8 ----

{
  
  # Filter and compute the total net tax by month and year for the years 2020 to 2023
  plot_data = copy(data[dt_year >= 2020 & dt_year <= 2023, .(total = sum(as.numeric(net_tax), na.rm = TRUE)), by = .(dt_month, dt_year)])
  
  # Create a YearMonth column for plotting
  plot_data = plot_data[, 
                        YearMonth := ymd(
                          paste0(
                            "2023", fifelse(nchar(dt_month) == 1, paste0("0", dt_month), paste0(dt_month)), "01"))
  ]
  
  # Plot the data
  plot_data %>%
    ggplot(aes(x = YearMonth, 
               y = total / 1e6,  # Convert total tax to millions
               colour = as.factor(dt_year))) +  # Use year as color factor
    geom_line(size = 1) +  # Add line graph
    geom_point(size = 3, shape = 19) +  # Add points to the line graph
    scale_colour_manual(values = c("#13294B", "#E53E3E", "#FFBE0B", "#32936F")) +  # Define custom colors for lines
    scale_y_continuous(
      breaks = seq(10, 40, 10),  # Set Y-axis breaks
      expand = c(0, 0),          # Remove space between axis and data
      limits = c(-1, 41)         # Set Y-axis limits
    ) +
    scale_x_date(
      date_breaks = "2 month",  # Set X-axis breaks
      labels = function(x) format(x, "%b"),  # Format X-axis labels as month abbreviations
      expand = c(0.09, 10)  # Adjust space around X-axis labels
    ) +
    labs(x = "", y = "", title = "Total Tax Due - ToT (millions)") +  # Add plot title and remove axis labels
    # Adding labels to the end of each line
    geom_text(data = plot_data %>% filter(YearMonth == min(YearMonth)), 
              aes(label = dt_year, hjust = 1.2, vjust = -.15),  # Adjust hjust for label positioning
              size = 5,  # Adjust size of the text as needed
              show.legend = FALSE, family = "LM Roman 10") +  # Set text properties
    theme_aeqj() +  # Apply custom theme
    coord_cartesian(expand = TRUE, clip = 'off')  # Allow text to extend beyond plot limits
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "net_tax_months.png"),  # Set file path and name
    plot = last_plot(),                                        # Save the last plotted graph
    width = 6,                                                 # Set width of the saved plot
    height = 4                                                 # Set height of the saved plot
  )
  
}

# Figure 9 ----

{
  
  # Filter and compute the total turnover by month and year for the years 2020 to 2023
  plot_data = copy(data)[dt_year %in% seq(2020, 2023), .(
    total = sum(as.numeric(tot_turnover), na.rm = TRUE)),
    by = .(dt_month, dt_year)
  ]
  
  # Create a YearMonth column for plotting
  plot_data = plot_data[, 
                        YearMonth := ymd(
                          paste0(
                            "2023", fifelse(nchar(dt_month) == 1, paste0("0", dt_month), paste0(dt_month)), "01"))
  ]
  
  # Plot the data
  plot_data %>% ggplot(aes(x = YearMonth, 
                           y = total / 1e6,  # Convert total turnover to millions
                           colour = as.factor(dt_year))) +  # Use year as color factor
    geom_line(size = 1) +  # Add line graph
    geom_point(size = 3, shape = 19) +  # Add points to the line graph
    scale_colour_manual(values = c("#13294B", "#E53E3E", "#FFBE0B", "#32936F")) +  # Define custom colors for lines
    scale_y_continuous(
      breaks = seq(400, 1200, 400),  # Set Y-axis breaks
      expand = c(0, 0),              # Remove space between axis and data
      limits = c(-1, 1300)           # Set Y-axis limits
    ) +
    scale_x_date(
      date_breaks = "2 month",  # Set X-axis breaks
      labels = function(x) format(x, "%b"),  # Format X-axis labels as month abbreviations
      expand = c(0.09, 10)  # Adjust space around X-axis labels
    ) +
    labs(x = "", y = "", title = "Total Turnover (millions)") +  # Add plot title and remove axis labels
    # Adding labels to the end of each line
    geom_text(data = plot_data %>% filter(YearMonth == min(YearMonth)), 
              aes(label = dt_year, hjust = 1, vjust = -1),  # Adjust hjust for label positioning
              size = 5,  # Adjust size of the text as needed
              show.legend = FALSE, family = "LM Roman 10") +  # Set text properties
    theme_aeqj() +  # Apply custom theme
    coord_cartesian(expand = TRUE, clip = 'off')  # Allow text to extend beyond plot limits
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "turnover_months.png"),  # Set file path and name
    plot = last_plot(),                                        # Save the last plotted graph
    width = 6,                                                 # Set width of the saved plot
    height = 4                                                 # Set height of the saved plot
  )
  
}

# Figure 10 an 12 ----

{
  
  # Filter data for the years 2022 and 2023
  plot_data = copy(data)[dt_year %in% c(2022, 2023)]
  
  # Filter data for the 12 months before the reform
  dt_filtered <- copy(plot_data)[(dt_month %in% 0:6 & dt_year == 2023) | (dt_month %in% 6:12 & dt_year == 2022)]
  
  # Calculate the average turnover for each taxpayer
  average_turnover <- dt_filtered[, .(average_turnover = mean(tot_turnover, na.rm = TRUE)), by = tax_payer_id]
  
  # Calculate the median of the average turnovers
  median_avg_turnover <- median(average_turnover$average_turnover, na.rm = TRUE)
  
  # Merge average turnover back to the original data
  plot_data <- merge(plot_data, average_turnover, by = "tax_payer_id", all.x = TRUE)
  
  # Divide the sample into two groups based on the median of the average turnovers
  plot_data[, turnover_group := ifelse(average_turnover < median_avg_turnover, "below_median", "above_median")]
  
  # Generate log of total turnover
  plot_data[, log_turnover := log(tot_turnover)]
  plot_data[, dt_month := as.factor(dt_month)]
  plot_data[, dt_year := as.factor(dt_year)]
  
  # Setting reference levels
  plot_data$dt_year <- relevel(plot_data$dt_year, ref = "2022")
  plot_data$dt_month <- relevel(plot_data$dt_month, ref = "6")
  
  # Fit the model with fixed effects and clustering
  model <- feols(log_turnover ~ dt_year * dt_month | tax_payer_id, data = plot_data, cluster = "tax_payer_id")
  model_below <- feols(log_turnover ~ dt_year * dt_month | tax_payer_id, data = plot_data[turnover_group == "below_median"], cluster = "tax_payer_id")
  model_above <- feols(log_turnover ~ dt_year * dt_month | tax_payer_id, data = plot_data[turnover_group == "above_median"], cluster = "tax_payer_id")

  # Extract coefficients, standard errors, and confidence intervals
  coefs  <- coef(model)[grepl("dt_year2023:", names(coef(model)))]
  se     <- sqrt(diag(vcov(model)))[grepl("dt_year2023:", names(coef(model_above)))]
  ci_low <- confint(model, level = 0.95)[, 1][grepl("dt_year2023:", names(coef(model_above)))]
  ci_up  <- confint(model, level = 0.95)[, 2][grepl("dt_year2023:", names(coef(model_above)))]
  
  # Extract coefficients, standard errors, and confidence intervals
  coefs_below  <- coef(model_below)[grepl("dt_year2023:", names(coef(model_below)))]
  se_below     <- sqrt(diag(vcov(model_below)))[grepl("dt_year2023:", names(coef(model_above)))]
  ci_low_below <- confint(model_below, level = 0.95)[, 1][grepl("dt_year2023:", names(coef(model_above)))]
  ci_up_below  <- confint(model_below, level = 0.95)[, 2][grepl("dt_year2023:", names(coef(model_above)))]
  
  # Extract coefficients, standard errors, and confidence intervals
  coefs_above  <- coef(model_above)[grepl("dt_year2023:", names(coef(model_above)))]
  se_above     <- sqrt(diag(vcov(model_above)))[grepl("dt_year2023:", names(coef(model_above)))]
  ci_low_above <- confint(model_above, level = 0.95)[, 1][grepl("dt_year2023:", names(coef(model_above)))]
  ci_up_above  <- confint(model_above, level = 0.95)[, 2][grepl("dt_year2023:", names(coef(model_above)))]
  
  # Create a dataframe for plotting
  df = data.frame(cbind(c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12), coefs, se, ci_low, ci_up))
  names(df) <- c("Month", "Coef", "SE", "CI_Low", "CI_Up")
  
  # Add a zero for month == 6
  zero_row <- data.frame(Month = 6, Coef = 0, SE = 0, CI_Low = 0, CI_Up = 0)
  df <- bind_rows(df, zero_row) %>%
    arrange(Month)
  
  # Plot the data
  ggplot(df, aes(x = Month, y = Coef)) +
    geom_vline(xintercept = 7, color = "red", size = 0.5, linetype = "dashed") +  # Vertical line for July
    geom_hline(yintercept = 0, color = "black", size = 0.4) +  # Horizontal line at y=0
    geom_ribbon(aes(ymin = CI_Low, ymax = CI_Up), fill = "#E69F00", alpha = 0.5) +  # Confidence interval ribbon
    geom_line(colour = "black", size = 0.4) +  # Line plot for coefficients
    geom_point(size = 3, shape = 23, fill = "#E69F00") +  # Points for coefficients
    scale_x_continuous("",  # X-axis labels and breaks
                       breaks = seq(1, 12, 2),
                       labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
    coord_cartesian(expand = TRUE, clip = 'off') +  # Allow text to extend beyond plot limits
    scale_y_continuous("", limits = c(-0.5, 0.1)) +  # Y-axis limits
    theme_aeqj()  # Apply custom theme
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "did_firms.png"),  # Set file path and name
    plot = last_plot(),                                   # Save the last plotted graph
    width = 6,                                            # Set width of the saved plot
    height = 3.5                                          # Set height of the saved plot
  )
  
  # Create a dataframe for plotting for below median group
  df_below <- data.frame(
    Month = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12),
    Coef = coefs_below,
    SE = se_below,
    CI_Low = ci_low_below,
    CI_Up = ci_up_below
  )
  
  # Create a dataframe for plotting for above median group
  df_above <- data.frame(
    Month = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12),
    Coef = coefs_above,
    SE = se_above,
    CI_Low = ci_low_above,
    CI_Up = ci_up_above
  )
  
  df_below <- bind_rows(df_below, zero_row) %>%
    arrange(Month)
  
  df_above <- bind_rows(df_above, zero_row) %>%
    arrange(Month)
  
  # Example for adding labels at the first month of each group
  first_month_below <- df_below %>% filter(Month == min(Month))
  first_month_above <- df_above %>% filter(Month == min(Month))
  
  # Plot heterogeneity effects
  ggplot() +
    geom_vline(xintercept = 7, color = "red", size = 0.5, linetype = "dashed") +  # Vertical line for July
    geom_hline(yintercept = 0, color = "black", size = 0.4) +  # Horizontal line at y=0
    geom_ribbon(data = df_below, aes(x = Month, ymin = CI_Low, ymax = CI_Up), fill = "#E53E3E", alpha = 0.5) +  # Confidence interval ribbon
    geom_line(data = df_below, aes(x = Month, y = Coef), color = "black", size = 0.4) +  # Line plot for coefficients
    geom_point(data = df_below, aes(x = Month, y = Coef), size = 3, shape = 23, fill = "#E53E3E") +  # Points for coefficients
    geom_ribbon(data = df_above, aes(x = Month, ymin = CI_Low, ymax = CI_Up), fill = "#32936F", alpha = 0.5) +  # Confidence interval ribbon
    geom_line(data = df_above, aes(x = Month, y = Coef), color = "black", size = 0.4) +  # Line plot for coefficients
    geom_point(data = df_above, aes(x = Month, y = Coef), size = 3, shape = 23, fill = "#32936F") +  # Points for coefficients
    scale_x_continuous("",  # X-axis labels and breaks
                       breaks = seq(1, 12, 2),
                       labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
    coord_cartesian(expand = TRUE, clip = 'off') +  # Allow text to extend beyond plot limits
    scale_y_continuous("", limits = c(-0.55, 0.2)) +  # Y-axis limits
    theme_aeqj() +  # Apply custom theme
    ggtitle("Turnover Group Comparison: Below vs Above Median") +
    geom_text(data = first_month_below, aes(x = Month, y = Coef, label = "Below Median", hjust = 0, vjust = 3),
              size = 5, show.legend = FALSE, family = "LM Roman 10", color = "#E53E3E") +  # Label for Below Median
    geom_text(data = first_month_above, aes(x = Month, y = Coef, label = "Median and Above", hjust = 0, vjust = -1.5),
              size = 5, show.legend = FALSE, family = "LM Roman 10", color = "#32936F")  # Label for Median and Above
  
  ggsave(
    filename = file.path(graph_output, "did_firms_h.png"),  # Set file path and name
    plot = last_plot(),                                   # Save the last plotted graph
    width = 6,                                            # Set width of the saved plot
    height = 3.5                                          # Set height of the saved plot
  )
  
}

# Figure 13 ----

{
  
  # Filter and compute the total net tax by month and year for the years 2020 to 2023
  data_tot = copy(data)[dt_year >= 2020 & dt_year <= 2023, .(total = sum(as.numeric(net_tax), na.rm = TRUE)), by = .(dt_month, dt_year)]
  
  # Create a YearMonth column for plotting
  data_tot[, 
           YearMonth := ymd(
             paste0(
               "2023", fifelse(nchar(dt_month) == 1, paste0("0", dt_month), paste0(dt_month)), "01"))
  ]
  
  # Ensure data_vat is in data.table format
  setDT(data_vat)
  
  # Filter for the year 2023 and exclude January
  data_vat_2023 = data_vat[dt_year == 2023 & dt_month != 1]
  data_tot_2023 = data_tot[dt_year == 2023 & dt_month != 1]
  
  # Merge the datasets
  plot = merge(data_vat_2023, data_tot_2023, by = c("dt_month", "dt_year", "YearMonth"))
  
  # Normalize the data
  plot[, `:=`(
    total = total / 11159557 * 100, 
    Payment_Amount = Payment_Amount / 18977204910 * 100
  )]
  
  # Plot the data
  ggplot(data = plot) +
    geom_line(aes(x = YearMonth, y = Payment_Amount), size = 1, color = "#E53E3E") +
    geom_point(aes(x = YearMonth, y = Payment_Amount), size = 2, color = "#E53E3E") +
    geom_line(aes(x = YearMonth, y = total), linetype = "dashed", size = 1, color = "#13294B") +
    geom_point(aes(x = YearMonth, y = total), size = 2, color = "#13294B") +
    geom_text(aes(x = min(YearMonth), y = 100, label = "TOT"), hjust = .6, vjust = -1, size = 5, colour = "#13294B", show.legend = FALSE, family = "LM Roman 10") + 
    geom_text(aes(x = min(YearMonth), y = 100, label = "VAT"), hjust = .6, vjust = 1.9, size = 5, colour = "#E53E3E", show.legend = FALSE, family = "LM Roman 10") +
    theme_aeqj() +
    scale_y_continuous("", limits = c(0, 410), breaks = seq(100, 400, 100)) +
    scale_x_date("", date_breaks = "2 month", labels = function(x) format(x, "%b"), expand = c(0.1, 0)) + 
    coord_cartesian(expand = TRUE, clip = 'off') +
    ggtitle("Total Taxes in 2023 - relative to February 2023")
  
  # Save the plot
  ggsave(
    filename = file.path(graph_output, "vat_tot_trends.png"),  # Set file path and name
    plot = last_plot(),                                        # Save the last plotted graph
    width = 6,                                                 # Set width of the saved plot
    height = 4                                                 # Set height of the saved plot
  )
  
}