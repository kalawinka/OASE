# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(janitor)
library(ggpubr)


# Themes -----------------------------------------------------------------------
t <- theme(text = element_text(size = 20),
           plot.title = element_text(size = 15, face = "bold"),
           axis.title.x = element_text(size = 15, margin = margin(6, 0, 0, 0)),
           axis.title.y = element_text(size = 15, margin = margin(0, 6, 0, 0)),
           axis.text.x = element_text(size = 11),
           axis.text.y = element_text(size = 11),
           plot.margin = margin(6, 6, 6, 6))
theme_set(theme_minimal() + t)

# Helpers ----------------------------------------------------------------------
likert_factorize <- function(d) {
  d %>%
    mutate(response = factor(response, 
                             levels = c("Strongly agree",
                                        "Agree",
                                        "Neutral",
                                        "Disagree",
                                        "Strongly disagree")))
}
plot_likert <- function(data, codes, title = NULL) {
  
  plot_labels <- codes %>%
    filter(code %in% colnames(data)) %>%
    mutate(code = fct_reorder(code, colnames(data)),
           label = as.character(label)) %>%
    pull(label)
  
  plot <- data %>%
    pivot_longer(., 
                 cols = starts_with("A"), 
                 names_to = "code", 
                 values_to = "response") %>%
    likert_factorize() %>%
    inner_join(codes, by="code") %>%
    droplevels() %>%
    group_by(label, response) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(x = label, y = freq, fill = response), 
             color = "grey50", width = 0.8, size = 0.25, 
             stat = "identity", position = "stack") +
    coord_flip() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 60),
                     limits = rev(plot_labels)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colorspace::diverging_hcl(n = 5, palette = "Blue-Red", alpha = 0.8)) +
    labs(x = "", y = "", fill = "", title = str_c(title, " (N = ", nrow(data), ")")) +
    guides(fill = guide_legend(reverse = T)) +
    theme(axis.title.x = element_blank(),
          legend.position="bottom")
  
  return(plot)
  
}

# Load full response data ------------------------------------------------------
eco  <- read_csv("eco_cleaned.csv")
eco_n = eco[eco$Region == 'Global North',]
eco_s = eco[eco$Region == 'Global South',]

soc <- read_csv("soc_cleaned.csv")
soc_n = soc[soc$Region == 'Global North',]
soc_s = soc[soc$Region == 'Global South',]

ocean <- read_csv("ocean_cleaned.csv")
ocean_n = ocean[ocean$Region == 'Global North',]
ocean_s = ocean[ocean$Region == 'Global South',]



# Preprint motivations ---------------------------------------------------------

## Data
deposited_codes <- read_csv("deposited.csv", col_types = "ff")

all_deposited_eco <- eco_n %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_eco <- eco_n %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_eco <- bind_rows(all_deposited_eco, some_deposited_eco)

all_deposited_soc <- soc_n %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_soc <- soc_n %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_soc <- bind_rows(all_deposited_soc, some_deposited_soc)

all_deposited_ocean <- ocean_n %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_ocean <- ocean_n %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_ocean <- bind_rows(all_deposited_ocean, some_deposited_ocean)



all_deposited_eco_s <- eco_s %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_eco_s <- eco_s %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_eco_s <- bind_rows(all_deposited_eco_s, some_deposited_eco_s)

all_deposited_soc_s <- soc_s %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_soc_s <- soc_s %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_soc_s <- bind_rows(all_deposited_soc_s, some_deposited_soc_s)

all_deposited_ocean_s <- ocean_s %>% 
  select(A = starts_with("CorAllDeposited")) %>%
  na.omit()
some_deposited_ocean_s <- ocean_s %>% 
  select(A = starts_with("CorSomeDeposited")) %>%
  na.omit()
deposited_ocean_s <- bind_rows(all_deposited_ocean_s, some_deposited_ocean_s)


## Decision making

# This refers to items A1, A4, A5, A2 and A3
# eco
p_eco_deposited_decision <- plot_likert(
  deposited_eco %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "D:    Economics")
#soc
p_soc_deposited_decision <- plot_likert(
  deposited_soc %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "E:    Social Sciences")
#ocean
p_ocean_deposited_decision <- plot_likert(
  deposited_ocean %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "F:    Oceanography")



p_eco_deposited_decision_s <- plot_likert(
  deposited_eco_s %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "A:    Economics")
#soc
p_soc_deposited_decision_s <- plot_likert(
  deposited_soc_s %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "B:    Social Sciences")
#ocean
p_ocean_deposited_decision_s <- plot_likert(
  deposited_ocean_s %>% select(A1, A4, A5, A2, A3), 
  deposited_codes,
  "C:    Oceanography")

p_deposited_decision <- p_eco_deposited_decision + 
  p_soc_deposited_decision + theme(axis.text.y = element_blank()) + p_ocean_deposited_decision + theme(axis.text.y = element_blank()) +
  p_eco_deposited_decision_s + 
  p_soc_deposited_decision_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_decision_s + theme(axis.text.y = element_blank()) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")

p_deposited_decision 
  ggsave("DepositedDecisionCombined.png", width = 12, height = 6)
  
p1 <- p_eco_deposited_decision_s + 
  p_soc_deposited_decision_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_decision_s + theme(axis.text.y = element_blank()) +
  plot_annotation("Global South") +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "none") 

p2 <- p_deposited_decision <- p_eco_deposited_decision + 
  p_soc_deposited_decision + theme(axis.text.y = element_blank()) + p_ocean_deposited_decision + theme(axis.text.y = element_blank()) +
  plot_annotation("Global North") +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom") 
  

figure <- ggarrange(p1, p2, ncol = 1, nrow = 2, heights  = c(1, 1.1))
figure
ggsave("DepositedDecisionCombined.png", width = 14, height = 7)

## Motivations

# Items A6, A7, A8, A9, A10
# What factors motivate preprint deposition (A6, A7, A8, A9, A10)
# eco
p_eco_deposited_motivation <- plot_likert(
  deposited_eco %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "D:    Economics")
#soc
p_soc_deposited_motivation <- plot_likert(
  deposited_soc %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "E:    Social Sciences")
#ocean
p_ocean_deposited_motivation <- plot_likert(
  deposited_ocean %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "F:    Oceanography")



# eco
p_eco_deposited_motivation_s <- plot_likert(
  deposited_eco_s %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "A:    Economics")
#soc
p_soc_deposited_motivation_s <- plot_likert(
  deposited_soc_s %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "B:    Social Sciences")
#ocean
p_ocean_deposited_motivation_s <- plot_likert(
  deposited_ocean_s %>% select(A6, A7, A8, A9, A10), 
  deposited_codes,
  "C:    Oceanography")

p_deposited_motivation <- p_eco_deposited_motivation + 
  p_soc_deposited_motivation + theme(axis.text.y = element_blank()) + p_ocean_deposited_motivation + theme(axis.text.y = element_blank())+
  p_eco_deposited_motivation_s + 
  p_soc_deposited_motivation_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_motivation_s + theme(axis.text.y = element_blank()) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
p_deposited_motivation 
  ggsave("DepositedMotivationCombined.png", width = 12, height = 6)
  
  
  
  p1 <- p_eco_deposited_motivation_s + 
    p_soc_deposited_motivation_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_motivation_s + theme(axis.text.y = element_blank()) +
    plot_annotation("Global South") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "none") 
  
  p2 <- p_eco_deposited_motivation + 
    p_soc_deposited_motivation + theme(axis.text.y = element_blank()) + p_ocean_deposited_motivation + theme(axis.text.y = element_blank())+
    plot_annotation("Global North") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom") 
  
  
  figure <- ggarrange(p1, p2, ncol = 1, nrow = 2, heights  = c(1, 1.1))
  figure
  ggsave("DepositedMotivationCombined.png", width = 14, height = 7)
  
## Benefits

# Items A11, A12, A13
# eco
p_eco_deposited_benefits <- plot_likert(
  deposited_eco %>% select(A11, A12, A13), 
  deposited_codes,
  "D:    Economics")
#soc
p_soc_deposited_benefits <- plot_likert(
  deposited_soc %>% select(A11, A12, A13), 
  deposited_codes,
  "E:    Social Sciences")
#ocean
p_ocean_deposited_benefits <- plot_likert(
  deposited_ocean %>% select(A11, A12, A13), 
  deposited_codes,
  "F:    Oceanography")


p_eco_deposited_benefits_s <- plot_likert(
  deposited_eco_s %>% select(A11, A12, A13), 
  deposited_codes,
  "A:    Economics")
#soc
p_soc_deposited_benefits_s <- plot_likert(
  deposited_soc_s %>% select(A11, A12, A13), 
  deposited_codes,
  "B:    Social Sciences")
#ocean
p_ocean_deposited_benefits_s <- plot_likert(
  deposited_ocean_s %>% select(A11, A12, A13), 
  deposited_codes,
  "C:    Oceanography")

p_deposited_benefits <- p_eco_deposited_benefits + 
  p_soc_deposited_benefits + theme(axis.text.y = element_blank()) + p_ocean_deposited_benefits + theme(axis.text.y = element_blank()) +
  p_eco_deposited_benefits_s + 
  p_soc_deposited_benefits_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_benefits_s + theme(axis.text.y = element_blank()) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
p_deposited_benefits 
  ggsave("DepositedBenefitsCombined.png", width = 12, height = 6)
  

  p1 <- p_eco_deposited_benefits_s + 
    p_soc_deposited_benefits_s + theme(axis.text.y = element_blank()) + p_ocean_deposited_benefits_s + theme(axis.text.y = element_blank()) +
    plot_annotation("Global South") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "none") 
  
  p2 <- p_eco_deposited_benefits + 
    p_soc_deposited_benefits + theme(axis.text.y = element_blank()) + p_ocean_deposited_benefits + theme(axis.text.y = element_blank()) +
    plot_annotation("Global North") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom") 
  
  
  figure <- ggarrange(p1, p2, ncol = 1, nrow = 2, heights  = c(0.6, 0.7))
  figure
  ggsave("DepositedBenefitsCombined.png", width = 14, height = 7)

# Preprint demotivations -------------------------------------------------------

## Data

not_deposited_codes <- read_csv("not_deposited.csv", col_types = "ff")
# eco
eco_none_not_deposited <- eco_n %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
eco_some_not_deposited <- eco_n %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
eco_not_deposited <- bind_rows(eco_none_not_deposited, eco_some_not_deposited)

#soc 
soc_none_not_deposited <- soc_n %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
soc_some_not_deposited <- soc_n %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
soc_not_deposited <- bind_rows(soc_none_not_deposited, soc_some_not_deposited)

#ocean
ocean_none_not_deposited <- ocean_n %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
ocean_some_not_deposited <- ocean_n %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
ocean_not_deposited <- bind_rows(ocean_none_not_deposited, ocean_some_not_deposited)



eco_none_not_deposited_s <- eco_s %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
eco_some_not_deposited_s <- eco_s %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
eco_not_deposited_s <- bind_rows(eco_none_not_deposited_s, eco_some_not_deposited_s)

#soc 
soc_none_not_deposited_s <- soc_s %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
soc_some_not_deposited_s <- soc_s %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
soc_not_deposited_s <- bind_rows(soc_none_not_deposited_s, soc_some_not_deposited_s)

#ocean
ocean_none_not_deposited_s <- ocean_s %>% 
  select(A = starts_with("CorNoneNotDeposited")) %>%
  na.omit()
ocean_some_not_deposited_s <- ocean_s %>% 
  select(A = starts_with("CorSomeNotDeposited")) %>%
  na.omit()
ocean_not_deposited_s <- bind_rows(ocean_none_not_deposited_s, ocean_some_not_deposited_s)


p_eco_not_deposited_demotivations <- plot_likert(
  eco_not_deposited,
  not_deposited_codes,
  "D:    Economics")

p_soc_not_deposited_demotivations <- plot_likert(
  soc_not_deposited,
  not_deposited_codes,
  "E:    Social Sciences")

p_ocean_not_deposited_demotivations <- plot_likert(
  ocean_not_deposited,
  not_deposited_codes,
  "F:    Oceanography")


p_eco_not_deposited_demotivations_s <- plot_likert(
  eco_not_deposited_s,
  not_deposited_codes,
  "A:    Economics")

p_soc_not_deposited_demotivations_s <- plot_likert(
  soc_not_deposited_s,
  not_deposited_codes,
  "B:    Social Sciences")

p_ocean_not_deposited_demotivations_s <- plot_likert(
  ocean_not_deposited_s,
  not_deposited_codes,
  "C:    Oceanography")


p_not_deposited_demotivations <- p_eco_not_deposited_demotivations + 
  p_soc_not_deposited_demotivations + theme(axis.text.y = element_blank()) + p_ocean_not_deposited_demotivations + theme(axis.text.y = element_blank()) +
  p_eco_not_deposited_demotivations_s + 
  p_soc_not_deposited_demotivations_s + theme(axis.text.y = element_blank()) + p_ocean_not_deposited_demotivations_s + theme(axis.text.y = element_blank()) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
p_not_deposited_demotivations 
  ggsave("NotDepositedDemotivationsCombined.png", width = 12, height = 8)
  
  
  p1 <-  p_eco_not_deposited_demotivations_s + 
    p_soc_not_deposited_demotivations_s + theme(axis.text.y = element_blank()) + p_ocean_not_deposited_demotivations_s + theme(axis.text.y = element_blank()) +
    plot_annotation("Global South") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "none") 
  
  p2 <- p_eco_not_deposited_demotivations + 
    p_soc_not_deposited_demotivations + theme(axis.text.y = element_blank()) + p_ocean_not_deposited_demotivations + theme(axis.text.y = element_blank()) +
    plot_annotation("Global North") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom") 
  
  
  figure <- ggarrange(p1, p2, ncol = 1, nrow = 2, heights  = c(0.6, 0.7))
  figure
  ggsave("NotDepositedDemotivationsCombined.png", width = 14, height = 9)
  
# Preprint comparisons ---------------------------------------------------------

## Data

comparison_codes <- read_csv("comparison.csv", col_types = "ff")
# eco
eco_comparison <- eco_n %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()

# soc
soc_comparison <- soc_n %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()

# ocean
ocean_comparison <- ocean_n %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()


# eco
eco_comparison_s <- eco_s %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()

# soc
soc_comparison_s <- soc_s %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()

# ocean
ocean_comparison_s <- ocean_s %>% 
  select(A = starts_with("CorSomeComparison")) %>%
  na.omit()



p_eco_comparison <- plot_likert(
  eco_comparison,
  comparison_codes,
  "D:    Economics")

p_soc_comparison <- plot_likert(
  soc_comparison,
  comparison_codes,
  "E:    Social Sciences")

p_ocean_comparison <- plot_likert(
  ocean_comparison,
  comparison_codes,
  "F:    Oceanography")

p_eco_comparison_s <- plot_likert(
  eco_comparison_s,
  comparison_codes,
  "A:    Economics")

p_soc_comparison_s <- plot_likert(
  soc_comparison_s,
  comparison_codes,
  "B:    Social Sciences")

p_ocean_comparison_s <- plot_likert(
  ocean_comparison_s,
  comparison_codes,
  "C:    Oceanography")

p_comparison <- p_eco_comparison + 
  p_soc_comparison + theme(axis.text.y = element_blank()) + p_ocean_comparison + theme(axis.text.y = element_blank()) +
  p_eco_comparison_s + 
  p_soc_comparison_s + theme(axis.text.y = element_blank()) + p_ocean_comparison_s + theme(axis.text.y = element_blank()) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom")
p_not_deposited_demotivations 
  ggsave("ComparisonsCombined.png", width = 12, height = 7.5)
  
  p1 <-  p_eco_comparison_s + 
    p_soc_comparison_s + theme(axis.text.y = element_blank()) + p_ocean_comparison_s + theme(axis.text.y = element_blank()) +
    plot_annotation("Global South") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "none") 
  
  p2 <- p_eco_comparison + 
    p_soc_comparison + theme(axis.text.y = element_blank()) + p_ocean_comparison + theme(axis.text.y = element_blank()) +
    plot_annotation("Global North") +
    plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "bottom") 
  
  
  figure <- ggarrange(p1, p2, ncol = 1, nrow = 2, heights  = c(0.6, 0.7))
  figure
  ggsave("ComparisonsCombined.png", width = 14, height = 8.5)

# Demographics -----------------------------------------------------------------

## Countries

country_eco <- eco_n %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title =  str_c("D:    Economics", " (N = ", nrow(eco_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 90))
  coord_flip(ylim = c(0, 150))
  
  
country_soc <- soc_n %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("E:    Social Sciences", " (N = ", nrow(soc_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 200))
  coord_flip(ylim = c(0, 150))


country_ocean <- ocean_n %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("F:    Oceanography", " (N = ", nrow(ocean_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 75))
  coord_flip(ylim = c(0, 150))


country_eco_s <- eco_s %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("A:    Economics", " (N = ", nrow(eco_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 40))
  coord_flip(ylim = c(0, 150))


country_soc_s <- soc_s %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("B:    Social Sciences", " (N = ", nrow(soc_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 30))
  coord_flip(ylim = c(0, 150))

country_ocean_s <- ocean_s %>%
  select(country = "Country") %>%
  mutate(country = case_when(
    country == "the United Kingdom of Great Britain and Northern Ireland" ~ "UK",
    country == "United States of America" ~ "USA",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    T ~ country
  )) %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(country, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("C:    Oceanography", " (N = ", nrow(ocean_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_flip(ylim = c(0, 30))
  coord_flip(ylim = c(0, 150))
## Composite Countries 


p1 <- country_eco_s + country_soc_s + country_ocean_s +
      plot_annotation("Countries Global South (Top 10)")
p2 <- country_eco + country_soc + country_ocean +
      plot_annotation("Countries Global North (Top 10)")
figure <- ggarrange(p1, p2, ncol = 1, nrow = 2)
figure
  
ggsave("countiresCombined2.png", width = 14, height = 7)

  
  
## Discipline

discipline_eco <- eco_n %>%
  select(discipline_eco = starts_with("Discipline"))
d1_eco <- data.frame(discipline1_eco=unlist(discipline_eco, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_eco <- count(d1_eco,discipline1_eco)
d1_eco <- d1_eco[order(d1_eco$n, decreasing = TRUE),] 
d1_eco <- d1_eco %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_eco, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "D:    Economics") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 350))


discipline_soc <- soc_n %>%
  select(discipline_soc = starts_with("Discipline"))
d1_soc <- data.frame(discipline1_soc=unlist(discipline_soc, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_soc <- count(d1_soc,discipline1_soc)
d1_soc <- d1_soc[order(d1_soc$n, decreasing = TRUE),] 
d1_soc <- d1_soc %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_soc, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "E:    Social Sciences") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0,500))


discipline_ocean <- ocean_n %>%
  select(discipline_ocean = starts_with("Discipline"))
d1_ocean <- data.frame(discipline1_ocean=unlist(discipline_ocean, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_ocean <- count(d1_ocean,discipline1_ocean)
d1_ocean <- d1_ocean[order(d1_ocean$n, decreasing = TRUE),] 
d1_ocean <- d1_ocean %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_ocean, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "Oceanography  Global North") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 150))



discipline_eco <- eco_s %>%
  select(discipline_eco = starts_with("Discipline"))
d1_eco_s <- data.frame(discipline1_eco=unlist(discipline_eco, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_eco_s <- count(d1_eco_s,discipline1_eco)
d1_eco_s <- d1_eco_s[order(d1_eco_s$n, decreasing = TRUE),] 
d1_eco_s <- d1_eco_s %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_eco, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "A:    Economics") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 350))


discipline_soc <- soc_s %>%
  select(discipline_soc = starts_with("Discipline"))
d1_soc_s <- data.frame(discipline1_soc=unlist(discipline_soc, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_soc_s <- count(d1_soc_s,discipline1_soc)
d1_soc_s <- d1_soc_s[order(d1_soc_s$n, decreasing = TRUE),] 
d1_soc_s <- d1_soc_s %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_soc, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "B:    Social Sciences") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0,500))


discipline_ocean <- ocean_s %>%
  select(discipline_ocean = starts_with("Discipline"))
d1_ocean_s <- data.frame(discipline1_ocean=unlist(discipline_ocean, use.names = FALSE))%>%
  remove_empty(which = c("rows"), quiet = TRUE) 
d1_ocean_s <- count(d1_ocean_s,discipline1_ocean)
d1_ocean_s <- d1_ocean_s[order(d1_ocean_s$n, decreasing = TRUE),] 
d1_ocean_s <- d1_ocean_s %>%
  slice(1:10)%>%
  ggplot() +
  geom_col(aes(x = reorder(discipline1_ocean, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "Oceanography  Global South") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 150))

## Composite Disciplines 

d1_eco + d1_soc + d1_ocean + 
  d1_eco_s + d1_soc_s + d1_ocean_s + 
  plot_annotation("Disciplines (Top 10)") +
  plot_annotation(tag_levels = 'A') 
  ggsave("disciplinesCombined.png", width = 14, height = 5)


## Gender


gender_eco <- eco_n %>%
  select(gender_eco = "Gender") %>% 
  count(gender_eco)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_eco, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "D:    Economics") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 600)) 

gender_soc <- soc_n %>%
  select(gender_soc = "Gender") %>% 
  count(gender_soc)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_soc, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "E:    Social Sciences") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 500)) 

gender_ocean <- ocean_n %>%
  select(gender_ocean = "Gender") %>% 
  count(gender_ocean)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_ocean, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "F:    Oceanography") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 300))


gender_eco_s <- eco_s %>%
  select(gender_eco_s = "Gender") %>% 
  count(gender_eco_s)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_eco_s, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "A:    Economics") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 600)) 

gender_soc_s <- soc_s %>%
  select(gender_soc_s = "Gender") %>% 
  count(gender_soc_s)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_soc_s, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "B:    Social Sciences") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 500)) 

gender_ocean_s <- ocean_s %>%
  select(gender_ocean_s = "Gender") %>% 
  count(gender_ocean_s)%>% 
  ggplot() +
  geom_col(aes(x = reorder(gender_ocean_s, n), y = n), na.rm = TRUE, 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "C:    Oceanography") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 300))

#composite gender
gender_eco + gender_ocean + gender_soc + 
  gender_eco_s + gender_ocean_s + gender_soc_s + 
  plot_annotation("Gender") +
  plot_annotation(tag_levels = 'A')
  ggsave("genderCombined.png", width = 12, height = 4)


## Career Status

career_status_eco <- eco_n %>%
  select(career_status_eco = "CareerStatus") %>%
  count(career_status_eco) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_eco, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("D:    Economics", " (N = ", nrow(eco_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 300)) 
  #coord_flip(ylim = c(0, 250))


career_status_soc <- soc_n %>%
  select(career_status_soc = "CareerStatus") %>%
  count(career_status_soc) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_soc, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("E:    Social Sciences", " (N = ", nrow(soc_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 250))

career_status_ocean <- ocean_n %>%
  select(career_status_ocean = "CareerStatus") %>%
  count(career_status_ocean) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_ocean, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("F:    Oceanography", " (N = ", nrow(ocean_n), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 100))
  #coord_flip(ylim = c(0, 250))


career_status_eco_s <- eco_s %>%
  select(career_status_eco_s = "CareerStatus") %>%
  count(career_status_eco_s) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_eco_s, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("A:    Economics", " (N = ", nrow(eco_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 60))
  #coord_flip(ylim = c(0, 250))

career_status_soc_s <- soc_s %>%
  select(career_status_soc_s = "CareerStatus") %>%
  count(career_status_soc_s) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_soc_s, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = str_c("B:    Social Sciences", " (N = ", nrow(soc_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 60))
  #coord_flip(ylim = c(0, 250))

career_status_ocean_s <- ocean_s %>%
  select(career_status_ocean_s = "CareerStatus") %>%
  count(career_status_ocean_s) %>%
  top_n(10, n) %>%
  ggplot() +
  geom_col(aes(x = reorder(career_status_ocean_s, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title =  str_c("C:    Oceanography", " (N = ", nrow(ocean_s), ")")) +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip(ylim = c(0, 40))
  #coord_flip(ylim = c(0, 250))

#composite career status
career_status_eco + career_status_soc + career_status_ocean + 
  career_status_eco_s + career_status_soc_s + career_status_ocean_s +
  plot_annotation(tag_levels = 'A') +
  plot_annotation("Career Status (Top 10)") 
  ggsave("C:/gesis/oase/surveys/results/analyse_23/plots/careerstatusCombined.png", width = 12, height = 5.5)


  p1 <- career_status_eco_s + career_status_soc_s + career_status_ocean_s +
    plot_annotation("Career Status Global South (Top 10)")
  p2 <- career_status_eco + career_status_soc + career_status_ocean + 
    plot_annotation("Career Status Global North (Top 10)")
  figure <- ggarrange(p1, p2, ncol = 1, nrow = 2)
  figure
  ggsave("careerstatusCombined.png", width = 16, height = 8)
## Institution Type


institution_eco <- eco_n %>%
  select(institution_type_eco = "Institution")%>%
  count(institution_type_eco) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_eco, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "D:  Economics") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 650)) 

institution_soc <- soc_n %>%
  select(institution_type_soc = "Institution")%>%
  count(institution_type_soc) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_soc, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "E:  Social Sciences") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 700)) 

institution_ocean <- ocean_n %>%
  select(institution_type_ocean = "Institution")%>%
  count(institution_type_ocean) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_ocean, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "F:    Oceanography") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 250))



institution_eco_s <- eco_s %>%
  select(institution_type_eco = "Institution")%>%
  count(institution_type_eco) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_eco, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "A:    Economics") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 650)) 

institution_soc_s <- soc_s %>%
  select(institution_type_soc = "Institution")%>%
  count(institution_type_soc) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_soc, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "B:    Social Sciences") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 700)) 

institution_ocean_s <- ocean_s %>%
  select(institution_type_ocean = "Institution")%>%
  count(institution_type_ocean) %>%
  ggplot() +
  geom_col(aes(x = reorder(institution_type_ocean, n), y = n), 
           fill = "grey75", color = "grey25", width = 0.8, size = 0.25) +
  labs(y = "Participants", x = "", title = "C:    Oceanography") +
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 35)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip(ylim = c(0, 250))
## Composite institution 

p1 <- institution_eco_s + institution_soc_s + institution_ocean_s + 
    plot_annotation("Institution types Global South")
p2 <- institution_eco + institution_soc + institution_ocean +
    plot_annotation("Institution types Global North")
figure <- ggarrange(p1, p2, ncol = 1, nrow = 2)
ggsave("institutionCombined.png", width = 12, height = 5.5)  

# Publishing behaviour ---------------------------------------------------------

# Number of articles published
pub_levels = c("0", "1-2", "3-5", "6-10", "11-20", "21+")
plot_n_pub <- function(d, type) {
  
  d %>%
    na.omit() %>%
    mutate(n_pub = factor(n_pub, levels = pub_levels)) %>%
    count(n_pub) %>%
    ggplot() +
    geom_col(aes(x = n_pub, y = n), 
             fill = "grey75", color = "grey25", width = 0.8, size = 0.25)  +
    labs(y = "Participants", x = "", 
         title = str_c(type, " (N = ", nrow(d), ")")) +
    #coord_flip()
    coord_flip(ylim = c(0, 200))
}


n_pub_eco <- eco_n %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"D:    Economics")

n_pub_soc <- soc_n %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"E:    Social Sciences")

n_pub_ocean <- ocean_n %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"F:    Oceanography")


n_pub_eco_s <- eco_s %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"A:    Economics")

n_pub_soc_s <- soc_s %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"B:    Social Sciences")

n_pub_ocean_s <- ocean_s %>%
  select(n_pub = "NPublishedCor") %>%
  plot_n_pub(.,"C:    Oceanography")


# Proportion of articles deposited as preprints
dep_levels = c("None of these articles were also deposited as preprints*",
               "Some of these articles were also deposited as preprints*",
               "All of these articles were also deposited as preprints*")
dep_labels = c("None of these articles were also deposited as preprints",
               "Some of these articles were also deposited as preprints",
               "All of these articles were also deposited as preprints")


plot_p_dep <- function(d, type) {
  d %>%
    na.omit() %>%
    mutate(p_dep = factor(p_dep, levels = dep_levels, labels = dep_labels)) %>%
    count(p_dep) %>%
    ggplot() +
    geom_col(aes(x = reorder(p_dep, -p_dep), y = n),
             fill = "grey75", color = "grey25", width = 0.8, size = 0.25)  +
    labs(y = "Participants", x = "", 
         title = str_c(type, " (N = ", nrow(d), ")")) +
    scale_x_discrete(label = function(x) str_wrap(x, 40)) +
    #coord_flip()
    coord_flip(ylim = c(0, 350))
}

dep_levels_eco = c("None of these articles were also deposited as working paper*",
               "Some of these articles were also deposited as working paper*",
               "All of these articles were also deposited as working paper*")

plot_p_dep_eco <- function(d, type) {
  d %>%
    na.omit() %>%
    mutate(p_dep = factor(p_dep, levels = dep_levels_eco, labels = dep_labels)) %>%
    count(p_dep) %>%
    ggplot() +
    geom_col(aes(x = reorder(p_dep, -p_dep), y = n),
             fill = "grey75", color = "grey25", width = 0.8, size = 0.25)  +
    labs(y = "Participants", x = "", 
         title = str_c(type, " (N = ", nrow(d), ")")) +
    scale_x_discrete(label = function(x) str_wrap(x, 40)) +
    #coord_flip()
    coord_flip(ylim = c(0, 350))
}


p_dep_eco <- eco_n %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep_eco(., "J:    Economics")
p_dep_soc <- soc_n %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep(., "K:    Social Sciences")
p_dep_ocean <- ocean_n %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep(., "L:    Oceanography")


p_dep_eco_s <- eco_s %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep_eco(., "G:    Economics")
p_dep_soc_s <- soc_s %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep(., "H:    Social Sciences")
p_dep_ocean_s <- ocean_s %>%
  select(p_dep = matches("PDepositedCor")) %>%
  plot_p_dep(., "I:    Oceanography")


# Patchwork

p1 <-  n_pub_eco_s + n_pub_soc_s + n_pub_ocean_s +
  plot_annotation("Global South: journal articles published in past 5 years")
p2 <- n_pub_eco + n_pub_soc + n_pub_ocean +
  plot_annotation("Global North: journal articles published in past 5 years")
p3 <-  p_dep_eco_s + theme(plot.tag.position	 = "topleft") +
  (p_dep_soc_s + theme(axis.text.y = element_blank(), plot.tag.position	 = "topleft")) + (p_dep_ocean_s + theme(axis.text.y = element_blank(), plot.tag.position	 = "topleft"))+
  plot_annotation("Global South: proportion of articles deposited as preprints")
p4 <-  p_dep_eco + theme(plot.tag.position	 = "topleft") +
  (p_dep_soc + theme(axis.text.y = element_blank(), plot.tag.position	 = "topleft")) + (p_dep_ocean + theme(axis.text.y = element_blank(), plot.tag.position	 = "topleft"))+
  plot_annotation("Global North: proportion of articles deposited as preprints")

figure <- ggarrange(p1, p2, p3, p4,  ncol = 1, nrow = 4, heights = c(1,1,0.7,0.7))
figure
ggsave("publishing_behaviourCombined_tot.png",
       width = 15,
       height = 12)


```