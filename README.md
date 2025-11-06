# Job Ad Data Collector (R Shiny App)

This R Shiny app is a secure tool for collecting job advertisement data related to Data Science and Analytics roles. It provides a structured interface for entering job details, ensuring consistency in recording skills, qualifications, and other attributes. All data is securely stored in a connected Google Sheet.

This app was built as part of the **STA 3152.0 – Essential Skills in Statistics** assignment.

## Key Features

- Secure Google Sheets Integration: Utilizes the googlesheets4 package and a Service Account for read/write access to a private Google Sheet. 

- Structured Data Entry: Uses dedicated input tabs for core details, programming languages, ML concepts, databases/tools, and other binary qualifications.

- Real-time Data View: Displays a live, sortable data table of the collected data directly within the app.

- Data Export: Allows users to download the complete dataset as a CSV file.

## Prerequisites

To run this application, you must have R installed, along with the following R packages:

shiny

tidyverse

DT

googlesheets4

