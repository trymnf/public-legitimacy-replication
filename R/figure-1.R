# Loading packages --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

# Theming -----------------------------------------------------------------

# You need the font IBM Plex Sans. Available on google fonts. 
# remove the 'text' argument if it gives you trouble. 

theme_set(theme_minimal() + 
            theme(text = element_text(family = "IBM Plex Sans"),
                  plot.title = element_text(face = "bold")))


# Figure 1 ----------------------------------------------------------------

monthly_frequencies <- read_rds("data/monthly-frequencies.Rds")


anno_text <- tibble(agency = "EBA", month = ymd("20110101"), proportion = 0.05)

together %>% 
  mutate(proportion = 
           agency_in_context/context) %>% 
  ggplot(aes(month, proportion)) + 
  geom_point(
    size = 2,
    alpha = 0.5) + 
  geom_smooth(
    span = 0.6, 
    se =  FALSE,
    color = "tomato",
    show.legend = FALSE) +
  facet_wrap(~agency) +
  scale_y_continuous(labels = percent_format(1)) + 
  scale_x_date(breaks = scales::date_breaks("4 years"),
               labels = date_format("%Y"), 
               limits = c(ymd("20050101"), 
                          ymd("20200101"))) + 
  labs(y = "Proportion", 
       x = "Date") + 
  theme(text = element_text(size = 14), 
        strip.text = element_text(face = "bold")) +
  geom_vline(data = anno_text, 
             aes(xintercept = month), 
             lty = 2, 
             alpha = 0.5) +
  geom_text(data = anno_text, 
            label = "January 2011:\nEBA established",
            aes(x = month,
                y = proportion),
            hjust = 1.05,
            family = "IBM Plex Sans", # Comment out if needed
            size = 3) + 
  scale_color_viridis_d()

# Saving: 

ggsave("plots/leg-dimensions-facet.png", width = 8, height = 3, dpi = "print")


