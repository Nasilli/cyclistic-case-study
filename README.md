##  View the Full Report

[**Click here to view the interactive HTML report**](https://nasilli.github.io/cyclistic-case-study/)

---

# Cyclistic Case Study

This project is my version of the **Google Data Analytics Capstone**, exploring how Cyclistic (Divvy) bike-share customers differ between *casual riders* and *annual members*.

## Contents
- `capstone_project_cleaning_script.R` - full R Script detailing all this case study's code prior to formal RMD formatting
- `Capstone-Project-Write-Up.Rmd` — full R Markdown script (data cleaning, analysis, visualisation, and mapping)
- `index.html` — rendered report (open in browser)
- `chicago_hotspots.csv` — reference data for tourist hotspot proximity analysis

## Summary
The analysis shows a clear segmentation between rider types:
- **Casual riders** behave like tourists or leisure users.
- **Members** behave like commuters or frequent locals.

Casual top stations are physically closer to major Chicago tourist attractions, while members’ trips cluster around business and residential areas.

## Key Insights
- Casual riders take longer trips (recreational use).
- Members ride more during weekday peak hours (commuting use).
- 47% of casual riders’ top stations are within 250 m of a tourist hotspot, compared to 20% of members’.

## Recommendations
- Offer short-term or tourist-oriented memberships providing bike access in multiple cities/locations.
- Provide commuter incentives for weekday users.
- Re-position bikes between business and leisure districts based on time of day.

## Notes
Raw Divvy data files are not included due to GitHub’s 25 MB limit but are available publicly through the [Divvy data portal](https://divvy-tripdata.s3.amazonaws.com/index.html)

---

*Author: Luca Nasillo*  
*Tools used: R, tidyverse, lubridate, ggplot2, leaflet, geosphere, htmltools*
