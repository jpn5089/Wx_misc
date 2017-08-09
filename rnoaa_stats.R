library(cranlogs)
library(ggplot2)

rnoaa <- cran_downloads(
  packages = "rnoaa", 
  from     = "2014-01-01", 
  to       = "2017-07-31")

ggplot(rnoaa, aes(x = date, y = count, label = date)) +
  # Data
  geom_point(alpha = 0.5) +
  geom_point(data = rnoaa[rnoaa$count > 250, ], color = "red", size = 2) +
  geom_text(data = rnoaa[rnoaa$count > 250, ], hjust = 0.5, vjust = 1.8, size = 2.5) +
  labs(title = "rnoaa package: Daily downloads", x = "", y = "Downloads",
       subtitle = "2014-01-01 through 2017-07-31",
       caption = "Downloads data courtesy of cranlogs package") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
