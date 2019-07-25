library(ggplot2)

# Graph the median number of employees by industry:
nyc_med_empl <- nyc %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees))
ggplot(nyc_med_empl, aes(x = year, y = median_num_employees)) + geom_point(color = "blue") + geom_line(color = "blue") + ggtitle("NYC Median Number of Employees by Industry") + facet_wrap("industry", scales = "free")

buffalo_med_empl <- buffalo %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
ggplot(buffalo_med_empl, aes(x = year, y = median_num_employees)) + geom_point(color = "seagreen") + geom_line(color = "seagreen") + ggtitle("Buffalo Median Number of Employees by Industry") + facet_wrap("industry", scales = "free")

rochester_med_empl <- rochester %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
ggplot(rochester_med_empl, aes(x = year, y = median_num_employees)) + geom_point(color = "red") + geom_line(color = "red") + ggtitle("Rochester Median Number of Employees by Industry") + facet_wrap("industry", scales = "free")

syracuse_med_empl <- syracuse %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees)) 
ggplot(syracuse_med_empl, aes(x = year, y = median_num_employees)) + geom_point(color = "orange") + geom_line(color = "orange") + ggtitle("Syracuse Median Number of Employees by Industry") + facet_wrap("industry", scales = "free")

westchester_med_empl <- westchester %>% group_by(industry, year) %>% summarise(median_num_employees = median(num_employees))
ggplot(westchester_med_empl, aes(x = year, y = median_num_employees)) + geom_point(color = "purple") + geom_line(color = "purple") + ggtitle("Westchester Median Number of Employees by Industry") + facet_wrap("industry", scales = "free")


# Graph the coefficient of variation of the number of employees, by industry:
nyc %>% group_by(industry, year) %>% summarise(coeff_of_variation = sd(num_employees)/mean(num_employees)) %>% ggplot(aes(x = year, y = coeff_of_variation)) + geom_point(color = "blue") + geom_line(color = "blue") + ggtitle("NYC Coefficient of Variation for Number of Employees by Industry") + facet_wrap("industry")
buffalo %>% group_by(industry, year) %>% summarise(coeff_of_variation = sd(num_employees)/mean(num_employees)) %>% ggplot(aes(x = year, y = coeff_of_variation)) + geom_point(color = "seagreen") + geom_line(color = "seagreen") + ggtitle("Buffalo Coefficient of Variation for Number of Employees by Industry") + facet_wrap("industry")
rochester %>% group_by(industry, year) %>% summarise(coeff_of_variation = sd(num_employees)/mean(num_employees)) %>% ggplot(aes(x = year, y = coeff_of_variation)) + geom_point(color = "red") + geom_line(color = "red") + ggtitle("Rochester Coefficient of Variation for Number of Employees by Industry") + facet_wrap("industry")
syracuse %>% group_by(industry, year) %>% summarise(coeff_of_variation = sd(num_employees)/mean(num_employees)) %>% ggplot(aes(x = year, y = coeff_of_variation)) + geom_point(color = "orange") + geom_line(color = "orange") + ggtitle("Syracuse Coefficient of Variation for Number of Employees by Industry") + facet_wrap("industry")
westchester %>% group_by(industry, year) %>% summarise(coeff_of_variation = sd(num_employees)/mean(num_employees)) %>% ggplot(aes(x = year, y = coeff_of_variation)) + geom_point(color = "purple") + geom_line(color = "purple") + ggtitle("Westchester Coefficient of Variation for Number of Employees by Industry") + facet_wrap("industry")


# Graph the median annual payroll by industry:
nyc_med_payroll <- nyc %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
ggplot(nyc_med_payroll, aes(x = year, y = median_annual_payroll)) + geom_point(color = "blue") + geom_line(color = "blue") + ggtitle("NYC Median Annual Payroll by Industry") + facet_wrap("industry", scales = "free")

buffalo_med_payroll <- buffalo %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
ggplot(buffalo_med_payroll, aes(x = year, y = median_annual_payroll)) + geom_point(color = "seagreen") + geom_line(color = "seagreen") + ggtitle("Buffalo Median Annual Payroll by Industry") + facet_wrap("industry", scales = "free")

rochester_med_payroll <- rochester %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
ggplot(rochester_med_payroll, aes(x = year, y = median_annual_payroll)) + geom_point(color = "red") + geom_line(color = "red") + ggtitle("Rochester Median Annual Payroll by Industry") + facet_wrap("industry", scales = "free")

syracuse_med_payroll <- syracuse %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll)) 
ggplot(syracuse_med_payroll, aes(x = year, y = median_annual_payroll)) + geom_point(color = "orange") + geom_line(color = "orange") + ggtitle("Syracuse Median Annual Payroll by Industry") + facet_wrap("industry", scales = "free")

westchester_med_payroll <- westchester %>% group_by(industry, year) %>% summarise(median_annual_payroll = median(annual_payroll))
ggplot(westchester_med_payroll, aes(x = year, y = median_annual_payroll)) + geom_point(color = "purple") + geom_line(color = "purple") + ggtitle("Westchester Median Annual Payroll by Industry") + facet_wrap("industry", scales = "free")


# Graph the median number of establishments by industry:
nyc_med_estab <- nyc %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
ggplot(nyc_med_estab, aes(x = year, y = median_num_establishments)) + geom_point(color = "blue") + geom_line(color = "blue") + ggtitle("NYC Median Number of Establishments by Industry") + facet_wrap("industry", scales = "free")

buffalo_med_estab <- buffalo %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
ggplot(buffalo_med_estab, aes(x = year, y = median_num_establishments)) + geom_point(color = "seagreen") + geom_line(color = "seagreen") + ggtitle("Buffalo Median Number of Establishments by Industry") + facet_wrap("industry", scales = "free")

rochester_med_estab <- rochester %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
ggplot(rochester_med_estab, aes(x = year, y = median_num_establishments)) + geom_point(color = "red") + geom_line(color = "red") + ggtitle("Rochester Median Number of Establishments by Industry") + facet_wrap("industry", scales = "free")

syracuse_med_estab <- syracuse %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
ggplot(syracuse_med_estab, aes(x = year, y = median_num_establishments)) + geom_point(color = "orange") + geom_line(color = "orange") + ggtitle("Syracuse Median Number of Establishments by Industry") + facet_wrap("industry", scales = "free")

westchester_med_estab <- westchester %>% group_by(industry, year) %>% summarise(median_num_establishments = median(num_establishments))
ggplot(westchester_med_estab, aes(x = year, y = median_num_establishments)) + geom_point(color = "purple") + geom_line(color = "purple") + ggtitle("Westchester Median Number of Establishments by Industry") + facet_wrap("industry", scales = "free")