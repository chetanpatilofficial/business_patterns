  library(dplyr)
  
  # SLURP ALL THE DATA INTO R AND DO SOME INITIAL CLEANING:
  
  # Slurp all of the csv files into R:
  bp16 <- read.csv("~/projects/county_business_patterns_ny/cbp16co.txt")
  bp15 <- read.csv("~/projects/county_business_patterns_ny/cbp15co.txt")
  bp14 <- read.csv("~/projects/county_business_patterns_ny/cbp14co.txt")
  bp13 <- read.csv("~/projects/county_business_patterns_ny/cbp13co.txt")
  bp12 <- read.csv("~/projects/county_business_patterns_ny/cbp12co.txt")
  
  
  # Looks like bp15's colnames are in all caps; change this: 
  colnames(bp15) <- tolower(colnames(bp15))
  
  
  # To make datasizes manageable, filter out rows that don't concern 
  # NY State (fipstate #36):
  bp16 <- bp16 %>% filter(fipstate == "36")
  bp15 <- bp15 %>% filter(fipstate == "36")
  bp14 <- bp14 %>% filter(fipstate == "36")
  bp13 <- bp13 %>% filter(fipstate == "36")
  bp12 <- bp12 %>% filter(fipstate == "36")
  
  
  # Slurp in the tables for converting NAICS and GEO_IDs to intelligible names:
  
  geo_id_to_name <- read.csv("~/projects/county_business_patterns_ny/georef12.txt")
  
  # For NAICS we're using a table that associates the first 2 digits of the NAICS
  # with a "broad" industry category (e.g., Hunting, Fishing, & Forestry
  # rather than Deep Sea Fishing):
  naics_to_industry <- read.csv("~/projects/county_business_patterns_ny/naics_broad")
  
  
  # Add a variable for year so that we can combine all the data into one big dataframe
  # for efficient processing and analysis: 
  bp16$year <- rep(2016, count(bp16))
  bp15$year <- rep(2015, count(bp15))
  bp14$year <- rep(2014, count(bp14))
  bp13$year <- rep(2013, count(bp13))
  bp12$year <- rep(2012, count(bp12))
  
  # Combine all the data:
  bp <- rbind(bp16, bp15, bp14, bp13, bp12)
  
  
  # GIVE INTELLIGIBLE NAMES TO THE COLUMNS & CREATE NEEDED COLUMNS: 
  
  # Add a column for the broad industry code:
  bp$industry_code <- as.integer(substr(as.character(bp$naics), 1,2))
  
  # Left join the business pattern tables with the NAICS and Geo ID tables,
  # so we can have recognizable place names and business types.
  bp <- left_join(bp, naics_to_industry, by = "industry_code")
  
  bp <- left_join(bp, geo_id_to_name, by = c("fipscty", "fipstate"))
  
  # Replace 'ap' with 'annual_payroll':
  colnames(bp)[colnames(bp) == "ap"] <- "annual_payroll"
  
  # Replace 'est' with 'num_establishments':
  colnames(bp)[colnames(bp) == "est"] <- "num_establishments"
  
  # Replace 'emp' with 'num_employees':
  colnames(bp)[colnames(bp) == "emp"] <- "num_employees"
  
  
  # MAKE VALUES MORE INTELLIGIBLE: 
  
  # Eliminate summary rows: 
  bp <- bp %>% filter(naics != "------")
  
  
  # FOR SIMPLICITY, SELECT ONLY THE COLUMNS WE ARE GOING TO CONSIDER: 
  # This line can be modified to broaden the scope of our study.
  bp <- bp %>% select(ctyname, industry, num_employees, num_establishments, annual_payroll, year)
  
  
  # CREATE TABLES OF THE TOP 2 IN EACH CATEGORY FOR EACH YEAR: 
  
  # Employers (in number of paid employees): 
  top2_employers <- bp %>% group_by(year, ctyname) %>% top_n(2, num_employees)
  
  # Number of establishments:
  top2_estab <- bp %>% group_by(year, ctyname) %>% top_n(2, num_establishments)
  
  # Annual payroll:
  top2_payroll <- bp %>% group_by(year, ctyname) %>% top_n(2, annual_payroll)
  
  
  # CREATE SUBSETS FOR THE 5 MAJOR METROPOLITAN AREAS:
  nyc <- bp %>% filter(grepl("New York|Kings|Bronx|Richmond|Queens", ctyname))
  buffalo <- bp %>% filter(grepl("Erie", ctyname))
  rochester <- bp %>% filter(grepl("Monroe", ctyname))
  syracuse <- bp %>% filter(grepl("Onondaga", ctyname))
  albany <- bp %>% filter(grepl("Albany", ctyname))
  westchester <- bp %>% filter(grepl("Westchester", ctyname))
  
  # CREATE TABLES FOR THE MEDIAN NUM OF EMPLOYEES BY INDUSTRY:
  nyc_med_empl <- nyc %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees))
  buffalo_med_empl <- buffalo %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
  rochester_med_empl <- rochester %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
  syracuse_med_empl <- syracuse %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
  westchester_med_empl <- westchester %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees))
  
  # CREATE TABLES FOR MEDIAN ANNUAL PAYROLL BY INDUSTRY:
  nyc_med_payroll <- nyc %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
  buffalo_med_payroll <- buffalo %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
  rochester_med_payroll <- rochester %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
  syracuse_med_payroll <- syracuse %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll)) 
  westchester_med_payroll <- westchester %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
  
  # CREATE TABLES FOR MEDIAN NUM OF ESTABLISHMENTS BY INDUSTRY:
  nyc_med_estab <- nyc %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
  buffalo_med_estab <- buffalo %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
  rochester_med_estab <- rochester %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
  syracuse_med_estab <- syracuse %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
  westchester_med_estab <- westchester %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))