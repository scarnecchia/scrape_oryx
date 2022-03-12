# Scrape Oryx
## About
This is a simple R script designed to scrape data from Oryx' excellent post detailing materiel lost by all sides in the [Russian invasion of Ukraine](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html) and to output it into csv format.

## To-Do
- [ ] Explore XPATH's ability to extract data between HTML tags: The Country column is currently blank, making attribution impossible. I'm not super familiar with xpath, so I've yet to work out how to use HTML pseudo-elements to extract only the list items under the "Russia" header, for example.
- [ ] Data Quality Assurance using the Totals provided at the start of each list item.
