WSL Setup Guide for R, cmdstanr, RWeka, and PostgreSQL
-------------------------------------------------------

This guide sets up a complete computational environment in WSL (Ubuntu) to support R-based workflows, including spatial operations, statistical modeling, and PostgreSQL integration.

1. System Prerequisites
   - Windows 10 or 11 with WSL2 enabled
   - Ubuntu distribution installed under WSL
   - Visual Studio Code with the "Remote - WSL" extension (optional but recommended)

2. Setup Instructions

   a. Run the WSL setup script:

      ./setup_wsl_env.sh

      This script performs the following:
      - Updates system packages
      - Installs development libraries and compilers
      - Installs Java (required for RWeka)
      - Installs PostgreSQL 14 and PostGIS 3
      - Installs Miniconda
      - Creates the Conda environment using environment.yml

   b. After installation, activate the Conda environment:

      conda activate remar_autom

   c. Run the R setup script to configure dependencies:

      Rscript setup.R

      This script will:
      - Install R packages from CRAN (e.g., cmdstanr, RWeka, data.table, sf, posterior, etc.)
      - Install eRTG3D from GitHub
      - Verify the CmdStan toolchain (C++ compiler and build tools)
      - Install CmdStan if it is not already present

3. Notes

   - Windows files can be accessed from WSL via: /mnt/c/Users/...
   - WSL files can be accessed from Windows via: \\wsl$\Ubuntu\home\...
   - To simulate remote execution, use SSH from Windows: ssh <user>@<WSL_IP>
   - For database operations in R, use packages like DBI and RPostgres
   - Tools like mapview or QGIS can be used for spatial data verification

4. File Structure

   - environment.yml: Conda environment definition (R and Python dependencies)
   - setup.R: R script to install R packages and configure CmdStan
   - setup_wsl_env.sh: Bash script to configure the WSL system
   - README.txt: This setup guide

5. Useful Commands

   - Start PostgreSQL:
        sudo service postgresql start

   - Check your WSL IP address:
        ip addr show eth0 | grep inet

   - From Windows, connect to WSL over SSH:
        ssh user@<WSL_IP>

6. Final R Environment Setup

   Once the Conda environment is active:

       conda activate remar_autom

   Run the following to complete the R configuration:

       Rscript setup.R

   This step will:
   - Install all required CRAN packages
   - Install eRTG3D from GitHub
   - Ensure the C++ toolchain is available for CmdStan
   - Build CmdStan locally if not yet installed

After this process, your R environment is ready for statistical modeling, geospatial analysis, and integration with CmdStan.

End of README

