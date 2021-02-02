# Zooplankton-EMP-Reporting
This project is intended to fufill the annual reporting requirements for the Zooplankton EMP survey.

In order to update the markdown document follow you must run the scripts in this order:

1)data_cleaning.R

    -this script will download the latest version of the zooplankton catch matrices from EDI and prepare them for use.
    
2)indices_calcs.R

    -this script will calculate catch indices to be used by the next script.
    
3)figures.R

    -this script uses the calculated indices to create the necessary figures.
    
4) annual-report.Rmd

    -located in the main project folder, this script will contains the text for the report, and pulls the necessary figures from the "Figures" folder.
