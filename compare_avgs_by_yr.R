avgs <- bp %>% 
  group_by(industry, year) %>%
    summarise(avg_ap = mean(ap), avg_emp = mean(emp, na.rm = TRUE), avg_est = mean(est))

naics <- read_csv("naics_broad")

comp <- avgs %>% filter(industry %in% naics$industry)
  
# Compare trends by industry in avg num of establishments:
ggplot(comp, aes(x = year, y = avg_est)) + geom_line() + facet_wrap("industry", scales = "free") +
  ggtitle("Mean Number of Establishments") + xlab("Year")

# Compare trends by industry in avg annual payroll:
ggplot(comp, aes(x = year, y = avg_ap)) + geom_line() + facet_wrap("industry", scales = "free") +
  ggtitle("Mean Annual Payroll") + xlab("Year")

# Compare trends by industry in avg num of employees:
ggplot(comp, aes(x = year, y = avg_emp)) + geom_line() + facet_wrap("industry", scales = "free") +
  ggtitle("Mean Number of Employees") + xlab("Year")