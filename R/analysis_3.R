
# Analysis 3: All agencies ------------------------------------------------


## ----packages and data------------------------------------------------------------------------

# Packages

library(tidyverse)
library(patchwork)
library(googlesheets4)
library(tidytext)
library(ggtext)
library(gt)


# Theming

theme_set(theme_minimal(base_family = "IBM Plex Sans") +
  theme(
    axis.text.x = element_markdown(),
    axis.text.y = element_markdown()
  ))

# Read data

ags <- read_rds("data/ags.Rds")

# A vector with full names of dimensions

recode_lookup <- c(
  evidence = "Evidence-based",
  hr = "Fundamental\nRights",
  leg_command = "Legislator's\nCommand",
  participation = "Participation",
  none = "All articles"
)


# Extract a helper dataset with all hits (dimension == "none") and 
# join back to full data

to_join <- ags %>%
  filter(dimension == "none") %>%
  select(forkortning, total_hits) %>%
  rename(all_hits = total_hits)

ags %>%
  left_join(to_join) %>%
  mutate(frac_hits = total_hits / all_hits) %>%
  filter(dimension != "none") %>%
  filter(forkortning != "-") -> ags_frac



# Read yearly hits ---------------------------------------

library(readxl)
library(clock)

yearly_ags <- read_rds("data/yearly_ags.rds")

to_join_yearly <- yearly_ags %>%
  filter(dimension == "none") %>%
  select(-c(dimension, total_hits, id)) %>%
  rename(all_arts = count)

ags_frac_yearly <- yearly_ags %>%
  left_join(to_join_yearly, by = c("year", "agency_excel")) %>%
  select(-total_hits) %>%
  mutate(frac_hits = count / all_arts) %>%
  filter(dimension != "none")


## ----prepare-dataset-salience-----------------------------------------------------------------------------------------------------------------------------------------------------------------

ags_frac_yearly %>%
  dplyr::select(-c(count)) %>%
  pivot_wider(names_from = dimension, 
              values_from = frac_hits, 
              id_cols = c(year, agency_excel, all_arts)) %>%
  mutate(leg_prevalence = leg_command - evidence) %>%
  na.exclude() -> for_fe_no_drop

ags_frac_yearly %>%
  dplyr::select(-c(frac_hits)) %>%
  pivot_wider(names_from = dimension, 
              values_from = count, 
              id_cols = c(year, agency_excel, all_arts)) %>%
  mutate(leg_prevalence = leg_command - evidence) -> for_mod_counts_no_drop

to_modeling <- ags_frac_yearly %>%
  group_by(agency_excel, dimension) %>%
  summarise(
    dim_sum = sum(count),
    all_arts = sum(all_arts)
  ) %>%
  ungroup() %>%
  mutate(prop_dim = dim_sum / all_arts)

count_mod <- to_modeling %>%
  dplyr::select(-prop_dim) %>%
  pivot_wider(names_from = dimension, values_from = dim_sum) %>%
  mutate(leg_prevalence = leg_command - evidence)


parlq <- read_tsv(file = "data/all-parliament-questions.txt") %>%
  # Remove false EEA hits
  filter(!(agency == "EEA" & !(str_detect(title, "European Environment Agency") |
    str_detect(text, "European Environment Agency")))) %>%
  # remove false ERA hits
  filter(!(agency == "ERA" & !(str_detect(title, "Agency for Railways|European Railway Agency|(r|R)ail") |
    str_detect(text, "Agency for Railways|European Railway Agency|(r|R)ail"))))

# Add 2019 data (only counts)

parlq_2019 <- read_tsv(file = "data/all-parliament-questions_2019.txt") %>%
  rename("parl_questions" = quest_counts) %>%
  mutate(agency = recode(agency,
    "European Environment Agency" = "EEA",
    "Agency for Railways" = "ERA"
  ))


parlq <- parlq %>%
  mutate(
    document_reference = str_trim(document_reference),
    year = get_year(year)
  )

parlq %>%
  group_by(agency, year) %>%
  count(name = "parl_questions") %>%
  ungroup() %>%
  bind_rows(parlq_2019) -> yearly_parlqs


for_fe_no_drop %>%
  left_join(yearly_parlqs, by = c("agency_excel" = "agency", 
                                  "year")) %>%
  filter(agency_excel != "ERA") %>%
  # We know that agencies with NA on parl_questions have no questions in that year.
  mutate(parl_questions = ifelse(is.na(parl_questions), 
                                 0, parl_questions)) -> for_fe_parl_no_drop

for_mod_counts_no_drop %>%
  left_join(yearly_parlqs, by = c("agency_excel" = "agency", 
                                  "year")) %>%
  filter(agency_excel != "ERA") %>%
  # We know that agencies with NA on parl_questions have no questions in that year.
  mutate(parl_questions = ifelse(is.na(parl_questions), 
                                 0, parl_questions)) -> for_mod_parl_counts_no_drop


## ----prepare-dataset-hardness-----------------------------------------------------------------------------------------------------------------------------------------------------------------


hardness <- read_rds("data/hardness.Rds")

to_boot <- to_modeling %>%
  left_join(hardness, c("agency_excel" = "agency")) %>%
  group_by(hardness, dimension)


## ---- Figure 1: Total number of articles mentioning each agency ----------------------------------------------------------------------------------------------------------------

highlights <- c("Frontex", "EBA", "EEA")

to_plot_top <- yearly_ags %>%
  filter(dimension == "none") %>%
  group_by(agency_excel) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate(agency_excel = fct_reorder(agency_excel, 
                                    count))

to_plot_top %>%
  ggplot(aes(count, agency_excel, 
             color = agency_excel %in% highlights)) +
  geom_linerange(aes(xmin = 0, xmax = count)) +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_color_manual(values = c("grey70", "black")) +
  labs(
    # title = "Total number of articles mentioning an agency",
    x = "Log(10) scale",
    y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

ggsave("plots/fig1.pdf", width = 13.2, height = 10.2, units = "cm", dpi = 300, device = cairo_pdf)


## ----Figure 3: Regression coefficients from four OLS models. All variables are log-transformed. 95 % confidence intervals."--------------------------------------------------


mod_all_counts2 <- parlq %>%
  group_by(agency) %>%
  count(name = "n_parlq") %>%
  ungroup() %>%
  # Add 2019 parlq data
  left_join(parlq_2019) %>%
  mutate(n_parlq = n_parlq + parl_questions) %>%
  select(-c(parl_questions, year)) %>%
  right_join(count_mod, by = c("agency" = "agency_excel")) %>%
  # add 1 b/c of log-tranformation
  mutate(across(c("evidence", "participation", "hr", "leg_command"), function(x) x + 1)) 

lc_m <- lm(log(leg_command) ~ log(n_parlq) + log(all_arts), 
           data = mod_all_counts2)

lc <- lc_m %>%
  tidy() %>%
  mutate(model = "Legislator's command")

ev_m <- lm(log(evidence) ~ log(n_parlq) + log(all_arts), 
           data = mod_all_counts2)

ev <- ev_m %>%
  tidy() %>%
  mutate(model = "Evidence-based")

part_m <- lm(log(participation) ~ log(n_parlq) + log(all_arts), 
             data = mod_all_counts2)

part <- part_m %>%
  tidy() %>%
  mutate(model = "Participation")

hr_m <- lm(log(hr) ~ log(n_parlq) + log(all_arts), data = mod_all_counts2)

hr <- hr_m %>%
  tidy() %>%
  mutate(model = "Fundamental rights")

bind_rows(lc, ev, part, hr) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(term, estimate, fill = model)) +
  geom_errorbar(
    aes(
      ymin = estimate - (1.96 * std.error),
      ymax = estimate + (1.96 * std.error)
    ),
    width = .1,
    position = position_dodge(width = .3)
  ) +
  geom_point(
    position = position_dodge(width = .3),
    shape = 21,
    color = "black",
    size = 3
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  scale_fill_viridis_d() +
  labs(
    x = "Term",
    y = "Estimate (*β*)",
    fill = "Dependent variable"
  ) +
  scale_x_discrete(labels = c(
    `log(all_arts + 1)` = "ln(All articles)",
    `log(n_parlq)` = "ln(Parliamentary questions)"
  )) +
  theme(
    axis.text.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  )

# ggsave("plots/salience-between.png", width = 6, height = 4, units = "in", dpi = 300)
ggsave("plots/fig3.pdf", width = 13.2, height = 9, units = "cm", dpi = 300, device = cairo_pdf)


# Figure 4: Bootstrapped hardness models ----------------------------------


library(tidymodels)
tidymodels_prefer(quiet = FALSE)

set.seed(09072021)

# Clustering by agency

to_boot %>%
  ungroup() %>%
  select(-prop_dim) %>%
  pivot_wider(
    names_from = dimension,
    values_from = dim_sum
  ) -> to_boot_agency

# lm(evidence ~ as_factor(hardness) + all_arts, data = to_boot_agency) %>%
#   summary() # Equivalent result


boots_agency <- bootstraps(to_boot_agency, times = 2000)

get_mean_boots_agency <- function(split) {
  analysis(split) %>%
    pivot_longer(
      cols = -c(agency_excel, all_arts, hardness, external_operational, jha),
      names_to = "dimension",
      values_to = "dim_sum"
    ) %>%
    group_by(hardness, dimension) %>%
    summarise(
      dim = sum(dim_sum),
      tot = sum(all_arts),
      n = n(), .groups = "drop"
    ) %>%
    mutate(prop_dim = dim / tot)
}


# This will create the bootstrapped dataset when you run the script for the first time,
# and save it to disk. The file will be read from disk at subsequent runs

if (file.exists("data/boot_mod_ungr.Rds")) {
  boot_mod_ungr <- read_rds("data/boot_mod_ungr.Rds")
} else {
  boot_mod_ungr <- boots_agency %>%
    mutate(
      model =
        map(splits, get_mean_boots_agency)
    ) %>%
    unnest(model)

  write_rds(boot_mod_ungr, "data/boot_mod_ungr.Rds")
}

summary_bootmod <- boot_mod_ungr %>%
  mutate(hardness = as_factor(hardness)) %>%
  group_by(hardness, dimension) %>%
  summarise(
    estimate = mean(prop_dim),
    ci_lower = quantile(prop_dim, 0.025), # "Two-tailed" CI
    ci_upper = quantile(prop_dim, 0.975),
    .groups = "drop"
  )

library(ggdist)

boot_mod_ungr %>%
  mutate(hardness = as_factor(hardness)) %>%
  ggplot(aes(hardness,
    fill = hardness,
    y = prop_dim
  )) +
  # Comment out this stat_slab if you don't have or don't want to install the ggdist package.
  stat_slab(
    shape = 21,
    show.legend = FALSE, size = 2,
    scale = 3,
    alpha = 0.5
  ) +
  geom_errorbar(
    data = summary_bootmod,
    aes(hardness, estimate,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.1
  ) +
  geom_point(
    data = summary_bootmod,
    aes(hardness, estimate,
      fill = hardness
    ),
    shape = 21,
    size = 2, show.legend = FALSE
  ) +
  facet_wrap(~dimension,
    labeller =
      labeller(dimension = recode_lookup),
    scales = "free_x"
  ) +
  scale_fill_viridis_d() +
  labs(
    x = "Hardness",
    y = "Proportion"
  ) +
  scale_fill_viridis_d() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

# ggsave(width = 15, height = 10, dpi = 300, filename = "plots/hardness_halfeye.png", units = "cm")
ggsave(width = 13.2, height = 9, dpi = 300, filename = "plots/fig4.pdf", units = "cm", device = cairo_pdf)



# Figure 5: Migration and Home Affairs agencies ---------------------------


to_boot_agency %>%
  pivot_longer(
    cols = c(evidence:participation),
    names_to = "dimension",
    values_to = "count"
  ) %>%
  group_by(jha, dimension) %>%
  summarise(
    m_count = mean(count),
    s_count = sum(count),
    all_arts = sum(all_arts),
    n = n()
  ) %>%
  mutate(
    w_mean = m_count * all_arts / sum(all_arts),
    prop = s_count / all_arts,
    se = (prop * (1 - prop)) / n,
    ci_lower = prop - 2 * se,
    ci_upper = prop + 2 * se,
    jha = as_factor(jha)
  ) %>%
  filter(dimension == "hr") %>%
  ggplot(aes(jha, prop)) +
  geom_errorbar(aes(
    ymin = ci_lower,
    ymax = ci_upper
  ),
  width = .3
  ) +
  geom_point(
    shape = 21,
    size = 2,
    aes(fill = jha),
    show.legend = FALSE
  ) + 
  scale_x_discrete(labels = c(
    "0" = "**Not** MHA",
    "1" = "MHA"
  )) +
  labs(
    x = NULL,
    y = "Proportion of articles"
  ) +
  scale_fill_brewer(palette = "Set1") + 
  ylim(0, NA)

# ggsave(width = 10, height = 10, units = "cm", dpi = 300, filename = "plots/MHAplot.png")
ggsave(width = 6.6, height = 6.6, units = "cm", dpi = 300, filename = "plots/fig5.pdf", device = cairo_pdf)
