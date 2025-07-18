library(here)
library(dplyr)
library(bbmle)
# library(MuMIn)
library(performance)
library(ggeffects)
library(DHARMa)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(patchwork)
library(mgcv)

incidence_data <- read.csv(here("data", "incidence_data.csv"), header = T)

fire_filtered <- incidence_data %>%
   filter(fire.cases >0)

# Modelling

# Model Fire: number of caes of fire related diseases ~ PM 2.5 ####
m_fire <- gam(fire.cases ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = fire_filtered, family= nb(), method = 'REML')

summary(m_fire)

# r2
summary(m_fire)$r.sq

# Plot predictions
(pred_fire <- ggpredict(m_fire, terms= "pm25_SUM"))

(p_fire <- pred_fire %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
    theme(
      text = element_text(size = 16), 
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)) + 
  labs(title= "", x = expression(PM[2.5]),
       y = expression(N^o * " of Fire-related diseases cases"))+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Angled x-axis labels
    ))


# Model respiratory: number of cases ~ PM 2.5 ####
# dataset filtering out zeroes
resp_filtered <- incidence_data %>% filter(respiratory.cases > 0)
m_resp <- gam(respiratory.cases ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = resp_filtered, family= nb(), method = 'REML')

summary(m_resp)

# calculate r2
summary(m_resp)$r.sq

# Plot predictions
(pred_resp <- ggpredict(m_resp, terms= "pm25_SUM"))
(p_resp <- pred_resp %>% plot() + 
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
      theme_cowplot() +
    theme(
      text = element_text(size = 16), 
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)) + 
    labs(title= "", x = expression(PM[2.5]),
         y = expression(N^o * " of Respiratory diseases cases")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Angled x-axis labels
    ))


# Model cardiovascular: number of cases ~ PM 2.5 ####
# dataset filtering out zeroes
card_filtered <- incidence_data %>% filter(cardiovascular.cases > 0)
m_card <- gam(cardiovascular.cases ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = card_filtered, family= nb(), method = 'REML')

summary(m_card)

# calculate r2
summary(m_card)$r.sq

# Plot predictions
(pred_card <- ggpredict(m_card, terms= "pm25_SUM"))
(p_card <- pred_card %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
    theme(
      text = element_text(size = 16), 
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)) + 
  labs(title= "", x = expression(PM[2.5]),
       y = expression(N^o * " of Cardiovascular diseases cases")))

## Cardiovascular timelag pm ####

card_filtered_lag <- card_filtered %>%
  group_by(COD) %>%
  arrange(year) %>%
  mutate(
    pm25_2yr = zoo::rollapply(pm25_SUM, width = 2, FUN = sum, align = "right", fill = NA, partial = FALSE),
    pm25_5yr = zoo::rollapply(pm25_SUM, width = 5, FUN = sum, align = "right", fill = NA, partial = FALSE)
  ) %>%
  ungroup()


# Remove NA values that result from lag operations
card_filtered_lag_2 <- card_filtered_lag %>%
  filter(!is.na(pm25_2yr))


# Fit the GAM model with 2-year lag
m_card_2yr <- gam(cardiovascular.cases ~ pm25_2yr + s(X, Y) +
                    s(year, bs = 're') +
                    offset(IDH),
                  data = card_filtered_lag_2, family = nb(), method = 'REML')

# Summary of the models
summary(m_card_2yr)

# R²
(r2_2yr <- summary(m_card_2yr)$r.sq)


# Plot Predictions for 2-year lag model
(pred_card_2yr <- ggpredict(m_card_2yr, terms = "pm25_2yr"))
(p_card_2yr <- pred_card_2yr %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16)) +
  labs(title = "", x = expression("2-yr accumulated PM"[2.5]),
       y = expression(N^o * " of Cardiovascular Disease Cases")) +
         theme(
           axis.text.x = element_text(angle = 45, hjust = 1)  # Angled x-axis labels
         ))



# Fit the GAM model with 5-year lag
card_filtered_lag_5 <- card_filtered_lag %>%
  filter(!is.na(pm25_5yr))

m_card_5yr <- gam(cardiovascular.cases ~ pm25_5yr + s(X, Y) +
                    s(year, bs = 're') +
                    offset(IDH),
                  data = card_filtered_lag_5, family = nb(), method = 'REML')

# Summary of the models
summary(m_card_5yr)

# R²
(r2_5yr <- summary(m_card_5yr)$r.sq)


# Plot Predictions for 5
(pred_card_5yr <- ggpredict(m_card_5yr, terms = "pm25_5yr"))
(p_card_5yr <- pred_card_5yr %>% plot() +
    geom_line(color = "darkgreen") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
    theme_cowplot() +
    theme(
      text = element_text(size = 16), 
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)) +
    labs(title = "", x = expression("5-yr accumulated PM"[2.5]),
         y = expression(N^o * " of Cardiovascular Disease Cases")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Angled x-axis labels
    ))




AICtab(m_card, m_card_2yr, m_card_5yr)

# > summary(m_card)$r.sq
# [1] 0.06023809
# > summary(m_card_2yr)$r.sq
# [1] 0.07460868
# > summary(m_card_5yr)$r.sq
# [1] 0.07506598

# 5-year lag has an adjusted R-squared that is ~0.0148 higher than yr model
# This improvement, though smaller compared to the fire-related models,
  # still supports the idea that cumulative exposure to PM2.5 over multiple years
  # provides a better explanation for variations in cardiovascular disease cases than yearly exposure



# # Plot panel ####
# # Combine the plots using patchwork
(p_fire + xlab("")) + p_resp + (p_card_5yr  + xlab(""))+
    plot_annotation(tag_levels = "a")

ggsave(here("outputs", "figures", "firecases_pollutants.png"),
         width = 13, height = 5)

# Interpretation ####  
  
  # Coefficients from model summaries
  estimate_fire <- summary(m_fire)$p.coef[2]
  estimate_resp <- summary(m_resp)$p.coef[2]
  estimate_card_5yr <- summary(m_card)$p.coef[2]
  
  # Average PM2.5 released per hectare in grams
  pm25_per_hectare <- 760500  # kg to grams conversion is already done
  
  # Convert coefficients based on the log link function
  # Calculate the increase in the number of cases per hectare burned
  increase_per_hectare_fire <- exp(pm25_per_hectare * estimate_fire)
  increase_per_hectare_resp <- exp(pm25_per_hectare * estimate_resp)
  increase_per_hectare_card_5yr <- exp(pm25_per_hectare * estimate_card_5yr)
  
  # Print the results
  cat("Increase per hectare (fire-related):", increase_per_hectare_fire - 1, "cases per hectare per year\n")
  cat("Increase per hectare (respiratory):", increase_per_hectare_resp - 1, "cases per hectare per year\n")
  cat("Increase per hectare (cardiovascular after 5 years):", increase_per_hectare_card_5yr - 1, "cases per hectare\n")
