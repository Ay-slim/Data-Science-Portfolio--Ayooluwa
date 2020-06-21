#create my datascience profile

criteria <- c("reading", "critical_thinking", "time_management", "mathematics", "computer_programming", "system_design", "report_writing", "listening", "team_work", "curiosity")
score <- c(9, 7, 6, 8, 7, 5, 7, 8, 8, 6)

data_science_profile <- data.frame(criteria, score)

data_science_profile

library(ggplot2)

ggplot(data_science_profile, aes(x=criteria, y=score)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 90))


