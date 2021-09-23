
# Loading packages --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)

# Theming -----------------------------------------------------------------

# You need the font IBM Plex Sans. Available on Google fonts. 
# remove the 'text' argument if it gives you trouble. 

theme_set(theme_minimal() + 
            theme(text = element_text(family = "IBM Plex Sans"),
                  plot.title = element_text(face = "bold")))



# ----------------------------------------------------

# This is a dataset with the full text removed. If you have a 
# subscription to Retriever or Mediearkivet, you may access the 
# full texts via the 'url' column.

dt_hits <- read_csv("data/dt_hits.csv")



# Calculating TF-IDFs -----------------------------------------------------

dt_tfidfs <- dt_hits %>%
  group_by(term) %>%
  
  # Calculate IDF. Varies pr term---not document
  # Furthermore: We don't want to divide by 0. So add 1 to numerator & denominator
  # (Alternative IDF specification for robustness check)
  
  mutate(
    idf = log((1 + n()) /
                (1 + sum(term_counts > 0))),
    idf_alt = log(n() /
                    (1 + sum(term_counts > 0)))
  ) %>%
  ungroup() %>%
  
  # tf-idf: unweighted in this case. so tf = hitcounts
  
  mutate(tf_idf = term_counts * idf) %>%
  arrange(desc(tf_idf))

# summarizing TF-IDFs and TFs per article and legitimacy dimension ------------

sum_dt <- dt_tfidfs %>%
  group_by(id, dimension) %>%
  summarise(
    tf_sum = sum(tf_idf),
    art_sum = sum(term_counts)
  )


# Joining back to articles ------------------------------------------------

# The commented-out code works on full-text data -- not available in this repository. 
# Kept here for transparency purposes. 


# dt <- read_rds("data/data.Rds")

# dt_tf <- dt %>%
#   left_join(sum_dt, by = "id")

# Check that the dimensions are correct. Each article gets 4 rows; 
# one for each legitimacy dimension

# stopifnot(nrow(dt) * length(unique(dt$leg_dim)) == nrow(dt_tf))
# stopifnot(all(table(dt_tf$id) == 4))


# Filtering ---------------------------------------------------------------

# Due to the search process, there will be duplicates between the legitimacy dimensions.
# We need to remove these. 

# dt_filtered <- dt_tf %>%
#   mutate(keep = case_when(
#     leg_dim == "expertise" & dimension == "ev_based" ~ TRUE,
#     leg_dim == "leg-command" & dimension == "leg_command" ~ TRUE,
#     leg_dim == "leg-command" & dimension == "leg_no_control" ~ TRUE,
#     dimension == "fundamental_rights" & art_sum > 0 ~ TRUE,
#     leg_dim == dimension ~ TRUE, # participation == participation
#     TRUE ~ FALSE # I.e. all the rest == FALSE
#   )) %>%
#   filter(keep)

# Removing duplicates *within* each legitimacy dimension --------------------

# dt_no_dupes <- dt_filtered %>%
#   group_by(
#     agency, title, body_strip, source,
#     publishDate, dimension,
#     tf_sum, art_sum, month
#   ) %>%
#   summarise(
#     n = n(),
#     ids = list(id)
#   ) %>%
#   arrange(publishDate)


# Creating monthly term frequencies ---------------------------------------

# monthlies <- dt_no_dupes %>%
#   group_by(agency, dimension, month) %>%
#   summarize(
#     wordcount = sum(art_sum),
#     tfcount = sum(tf_sum)
#   ) %>%
#   ungroup()

monthlies <- read_rds("data/monthlies.rds")

# Joining with total monthly frequencies (also for articles not downloaded) ----

source("R/read_retriever_xlsx.R")


ls <- list.files("final-searches", pattern = ".xlsx", full.names = TRUE)
ls

lw <- tibble(
  agency = str_extract(ls, "/(.+?)-") %>%
    str_remove("-") %>%
    str_remove("/"),
  data = map(ls, read_retriever_xlsx)
)

lw <- lw %>%
  unnest(data) %>%
  mutate(term = str_remove(term, "^.+ AND "))

monthly_totals <- lw %>%
  filter(term == "D") %>%
  select(-term) %>%
  rename(monthly_totals = articles)

# Create dataset for all months 2005--2019

monthlies %>%
  complete(agency, dimension,
           month = seq.Date(min(month),
                            max(month),
                            by = "month"
           ),
           fill = list(
             wordcount = 0,
             tfcount = 0
           )
  ) %>%
  full_join(monthly_totals) -> joined_monthlies


# Plotting: Figure 2 ------------------------------------------------------

names_lookup <- c(
  eba = "EBA",
  eea = "EEA",
  frontex = "Frontex"
)

a <- joined_monthlies %>%
  group_by(agency, dimension) %>%
  summarise(
    tot_arts = sum(monthly_totals),
    tot_words = sum(wordcount)
  ) %>%
  mutate(mean_words_pr_art = tot_words / tot_arts) %>%
  filter(dimension != "leg_command") %>%
  ggplot(aes(agency, mean_words_pr_art, fill = dimension)) +
  geom_col(position = "dodge") + 
  labs(
    title = "Mean no. of legitimacy words pr article",
    subtitle = "Mean of all words/articles",
    x = "Agency",
    y = "Mean words per article",
    fill = "Dimension"
  ) +
  scale_fill_viridis_d(
    # Disclaimer: Test that your version of ggplot gives the same sorting as mine
    # (comment out the 'labels' argument).
    # Or else the labels might be wrong. 
    labels = c( 
      "Evidence-based",
      "Fundamental rights",
      "Legislator's command",
      "Participation"),
    option = "D") +
  scale_x_discrete(labels = names_lookup) +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())

a

# Disaggregating by year 

yearlies <- joined_monthlies %>%
  mutate(year = year(month)) %>%
  group_by(agency, dimension, year) %>%
  summarise(
    tot_arts = sum(monthly_totals),
    tot_words = sum(wordcount)
  ) %>%
  mutate(words_pr_year = tot_words / tot_arts) %>%
  filter(
    !(words_pr_year %in% c(NaN, Inf)),
    dimension != "leg_command"
  )


yearlies %>%
  ggplot(aes(year, words_pr_year, color = dimension)) +
  geom_line(size = 1.2, alpha = 0.9) +
  facet_wrap(~agency, ncol = 1, labeller = labeller(agency = names_lookup)) + 
  scale_color_viridis_d(
    labels = c(
      "Evidence-based",
      "Fundamental rights",
      "Legislator's command",
      "Participation"),
    option = "D") + 
  labs(y = "Mean words per article", 
       x = "Year", 
       color = "Dimension") + 
  theme(legend.position = "bottom", 
        strip.text = element_text(face = "bold")) + 
  scale_x_continuous(breaks = c(2007, 2010, 2013, 2016, 2019)) -> yearly_plot


# Combining and saving ----------------------------------------------------

a_notitle <- a + ggtitle(NULL, subtitle = NULL)

all_and_year <- (a_notitle + yearly_plot + theme(legend.position = "none")) / guide_area() + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect", heights = c(9, 1))

ggsave(all_and_year, filename = "plots/all-and-year-w-fundrights.png", width = 8, height = 4)
