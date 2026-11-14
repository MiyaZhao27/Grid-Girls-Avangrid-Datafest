This Repo contains all work for 2026 Innovation Hackathon for Grid Girls. 

All of our commit history is in our private Repo, which also includes the raw data. Because of the security concern of including the data directly on this Repo, right before submission we copied the contents of our private Repo without the data so that this Repo can be public and the raw data will not be accessible in the commit history. To use this code, first put all of the given datasets (UNZIPPED) into our "data" folder. Then run master.R , sources all below code in the correct order to produce this Repo. This way, the code is only reproducible for those who have access to the Challenge data. 


Part 00: Data Cleaning
- merge_data_frames.qmd (all code to get files in usable format, clean, and join temp data)
- Data folder: givens (likely EVs, etc), csvs with identified CUSTOMER_NUMBERs for various categories
- cache_clean folder: all rows of AMI data, stored in parquet and used in analysis

Part 01:
- EVchargerclassification.R (classifies EVs)
- heatpumpclassification.R (classifies heat pumps/AC)

Part 02:
- summarize.R (calculates our top peak load contribution customers)
- characteristics_all.R: takes in all lists of CUSTOMER_NUMBERS associated with characteristics, and makes
- customer_characteristics.csv (one row per customer, one column per characteristic. 1 if has characteristic, 0 if does not)
- analysis.R (forecasting model and residual comparison after classification)

Part 03:
- price.qmd (BTM simulations and price analysis)

Miscellaneous
- explore100customers.R (old version of what became heat pump classification)
- m_exploraion.qmd (early exploration)
- equationformatting.R (latex/knitr for equation in appendix)
