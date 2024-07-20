# README for Reproducing Results
# Reproducible Package: "Trade-offs in the Design of Simplified Tax Regimes: Evidence from Sub-Saharan Africa"

## Overview

This README file provides step-by-step instructions for reproducing the results presented in our paper. The folder structure is organized to streamline the process, with specific directories for outputs, data, and code. Please follow the instructions carefully to ensure the accurate replication of our results.

## Folder Structure

1. **Outputs**:
    - **Figures**: Contains all the figures generated for the paper.
    - **Tables**: Contains all the tables generated for the paper.

2. **Data**:
    - **Raw**: Contains the original, unprocessed data.
    - **Intermediate**: Contains data that has been partially processed.
    - **Final**: Contains fully processed data ready for analysis.
        - **Experiment**: Sub-folder with data from:
            - **Scale Up**: Data from the scaled-up experiment.
            - **Pilot**: Data from the pilot study. Note: The pilot data underwent multiple iterations and slight modifications in collaboration with the Kenya Revenue Authority.

3. **Code**:
    - **Master Script**: The main R script that runs all other scripts. Adjust directories in this script before running.
    - **Randomization Scripts**: Written in STATA. These scripts are numbered and can be run through R or manually if issues arise.

## Prerequisites

To successfully run this reproducible package, the following software must be installed on your computer:

- **R**
- **STATA**

## Steps to Reproduce Results

1. **Setup Directories**:
    - Open the **Master Script** in the **Code** folder.
    - Modify the directory paths in the script to reflect the location of the data and output folders on your computer.
    - Similarly, open the `.do` files in the STATA scripts and update the directory paths accordingly.

2. **Running the Master Script**:
    - Run the Master Script in R. This script will sequentially execute all necessary scripts, including data processing and analysis.
    - If the script encounters issues running the STATA randomization scripts through R, follow the numbering and run these scripts manually in STATA.

3. **Manual Execution of STATA Scripts**:
    - Navigate to the **Code** folder.
    - Identify the numbered STATA scripts.
    - Open each script in STATA and run them in the specified order.

## Special Considerations

- **Pilot Data Randomization**:
    - The randomization process for the pilot data slightly varied due to iterations and changes in collaboration with the Kenya Revenue Authority. Ensure that the steps in the STATA scripts reflect these modifications accurately.

- **Scale Up Randomization**:
    - The scale-up data was randomized using the updated dataset. Follow the scripts for precise steps.

## Troubleshooting

If you encounter any issues, please ensure that:

- Both R and STATA are correctly installed and operational on your machine.
- Directory paths are correctly specified in both the R and STATA scripts.
- STATA scripts are executed in the correct order if run manually.

## Conclusion

By following these steps, you should be able to accurately reproduce the results from our paper. If you have any questions or require further assistance, please contact [your contact information or support details].

---

