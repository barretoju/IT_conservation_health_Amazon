library(here)
library(dplyr)
# library(fitdistrplus)
# library(lme4)
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

# july 17th meeting points

#  # no incidence/population data, work with no cases instead
#  # remove 0 number of cases per municipality/year, PP argued diseases are way too common
#  # fit models with negative binomial, apparently better than poisson or gaussian
#  # structure IDH as offset, check if year variation too low to stay as RE, besides country
#  # gam spatial models dispense country, already account for lower spatial level
#  # spatial autocorrelation mantel or Moran's I if not spatial model
  # other predictors, ask PP about _pland, and ITs,_ AND **timelag (5yrs)**

# Loading the data and data prep ####
combined_data <- read.csv(here("data", "combined_data.csv")) %>%
  # X & Y data for spatial models
  left_join(read.csv(here("data", "amazon_districts", "Coordinates.csv")), 
            by= "COD", relationship = "many-to-many") # XY repeat through year

str(combined_data)


# no incidence/population data, work with no cases instead
# remove 0 number of cases per municipality/year, PP argued diseases are way too common
fire_filtered <- combined_data %>%
   filter(fire_related >0)

# # test models ####
# 
# # better transformed in log10 + 1
# # better without year, adds little explanation
# 
# # worse it can get # two REs, rawest predictor which is scaled
# mm1 <- glmer(fire_related~ scale(pm25_SUM) + (1|country) + (1|year), family = poisson, data= fire_filtered)
# summary(mm1)
# testDispersion(mm1)
# testResiduals(mm1, plot = TRUE) # terrrrrrible
# ggpredict(mm1, terms= "pm25_SUM") %>% plot() + theme_cowplot() # não faz sentido
# 
# mm2 <- glmer(fire_related~ log10(pm25_SUM + 1) + (1|country) + (1|year), family = poisson, data= fire_filtered)
# summary(mm2)
# testDispersion(mm2)
# testResiduals(mm2, plot = TRUE) # baaad
# ggpredict(mm2, terms= "pm25_SUM") %>% plot() + theme_cowplot() # pouco sentido
# 
# mm3 <- glmer(fire_related~ log10(pm25_SUM + 1) + (1|country) + (1|year), family = poisson, data= fire_filtered)
# summary(mm3)
# testDispersion(mm3)
# testResiduals(mm3, plot = TRUE) # better, but still baaad
# ggpredict(mm3, terms= "pm25_SUM") %>% plot() + theme_cowplot() # better, but still pouco sentido
# 
# mm4 <- glmer(fire_related~ log10(pm25_SUM + 1) + (1|country), family = poisson, data= fire_filtered)
# summary(mm4)
# testDispersion(mm4)
# testResiduals(mm4, plot = TRUE) # biiit better, but still no
# ggpredict(mm4, terms= "pm25_SUM") %>% plot() + theme_cowplot() # bit better, but naah
# 
# mm5 <- glmer(fire_related~ log10(pm25_SUM + 1) + (1|country) + offset(IDH), family = poisson, data= fire_filtered)
# summary(mm5)
# testDispersion(mm5)
# testResiduals(mm5, plot = TRUE)
# ggpredict(mm5, terms= "pm25_SUM") %>% plot() + theme_cowplot() # nope
# 
# mm6 <- glmer.nb(fire_related~ log10(pm25_SUM+1) +(1|country),data= fire_filtered)
# summary(mm6)
# testDispersion(mm6)
# testResiduals(mm6, plot = TRUE)
# ggpredict(mm6, terms= "pm25_SUM") %>% plot() + theme_cowplot()
# 
# mm7 <- glmer.nb(fire_related~ scale(pm25_SUM) +(1|country),data= fire_filtered)
# 
# mm8 <- glmer.nb(fire_related~ log10(pm25_SUM+1) + (1|country) + offset(IDH),data= fire_filtered)
# summary(mm8)
# 
# # mm9 <- glmer.nb(fire_related~ log10(pm25_SUM+1) +(1|year),data= fire_filtered)
# 
# # mm10 <- glmer.nb(fire_related~ scale(pm25_SUM) +(1|year),data= fire_filtered)
# 
# mm11 <- glmer.nb(fire_related~ log10(pm25_SUM+1) +(1|country) + offset(IDH), data= fire_filtered)
# 
# AICtab(mm1, mm2, mm3, mm4, mm5, mm6, mm7, mm8, mm9, mm10, mm11)
# # despite not top, we keep idh as offset for its reasoning
# 
# # # does that work simplifying? lm and gaussian # NOPE, too worse
# # m1 <- glm(fire_related~ pm25_SUM, data= fire_filtered)
# # summary(m1)
# # testResiduals(m1, plot= TRUE)
# # testDispersion(m1)
# # 
# # ggpredict(m1, terms= "pm25_SUM") %>% plot() + theme_cowplot()
# # 
# # m2 <- glm(fire_related~ log10(pm25_SUM+1), data= fire_filtered)
# # summary(m2)
# # testDispersion(m2)
# # testResiduals(m2, plot = TRUE)
# # 
# # ggpredict(m2, terms= "pm25_SUM") %>% plot() + theme_cowplot() # não faz sentido
# # 
# # AICtab(m1, m2)
# # 
# # 
# # ## negative binomial glm # no geral efeito bem mais baixo, mas melhor fit
# # m3 <- glm.nb(fire_related~ pm25_SUM, data= fire_filtered)
# # summary(m3)
# # testDispersion(m3)
# # testResiduals(m3, plot = TRUE)
# # 
# # ggpredict(m3, terms= "pm25_SUM") %>% plot() + theme_cowplot()
# # 
# # AICtab(m1, m2, m3, mm1, mm2, mm3, mm4, mm5, mm6, mm7, mm8, mm9, mm10, mm11)
# 
# # GAM testar com log10 para ver se melhora
# 
# 
# # GAMS
# m1_gam <- gam(fire_related ~ pm25_SUM + s(X,Y), # falta offset
#               data = fire_filtered, family= nb(), method = 'REML')
# summary(m1_gam)
# plot(ggpredict(m1_gam, terms= "pm25_SUM")) + cowplot::theme_cowplot()
# 
# m2_gam <- gam(fire_related ~ pm25_SUM + s(X,Y)+ 
#                 offset(IDH),
#               data = fire_filtered, family= nb(), method = 'REML')
# summary(m2_gam)
# plot(ggpredict(m2_gam, terms= "pm25_SUM")) + cowplot::theme_cowplot()
# 
# m3_gam <- gam(fire_related ~ pm25_SUM + s(X,Y)+
#                 s(year, bs = 're') +
#                 offset(IDH),
#               data = fire_filtered, family= nb(), method = 'REML')
# summary(m3_gam)
# plot(ggpredict(m3_gam, terms= "pm25_SUM")) + cowplot::theme_cowplot()
# 
# AICtab(m1_gam, m2_gam, m3_gam)
# 
# # AICtab(m1, m2, m3, mm1, mm2, mm3, mm4, mm5, mm6, mm7, mm8, mm9, mm10, mm11, m1_gam, m2_gam, m3_gam)

# Modelling ####
# ELEITO NUMERO GAM CASOS ˜ PM25 + XY + reYEAR + OFFSET IDH, nb()


# Model Fire: number of caes of fire related diseases ~ PM 2.5 ####
m_fire <- gam(fire_related ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = fire_filtered, family= nb(), method = 'REML')

summary(m_fire)
# # test GAM with curvature for predictor pm2.5 limited to 1 knot
# m_fire1 <- gam(fire_related ~ s(pm25_SUM, k= 3) + s(X,Y)+
#                 s(year, bs = 're') +
#                 offset(IDH),
#               data = fire_filtered, family= nb(), method = 'REML')
# 
# summary(m_fire1)

# r2
summary(m_fire)$r.sq

# run model diagnostics # thats bad
# DHARMa::testDispersion(m_fire)
# testResiduals(m_fire, plot = TRUE)

# Plot predictions
(pred_fire <- ggpredict(m_fire, terms= "pm25_SUM"))

(p_fire <- pred_fire %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
  labs(title= "", x = expression(PM[2.5]),
       y = expression(N^o * " of Fire-related Disease Cases")))


# Model respiratory: number of cases ~ PM 2.5 ####
# dataset filtering out zeroes
resp_filtered <- combined_data %>% filter(respiratory > 0)
m_resp <- gam(respiratory ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = resp_filtered, family= nb(), method = 'REML')

summary(m_resp)

# calculate r2
summary(m_resp)$r.sq

# run model diagnostics # thats bad
# testDispersion(m_resp)
# testResiduals(m_resp, plot = TRUE)

# Plot predictions
(pred_resp <- ggpredict(m_resp, terms= "pm25_SUM"))
(p_resp <- pred_resp %>% plot() + 
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
      theme_cowplot() +
    labs(title= "", x = expression(PM[2.5]),
         y = expression(N^o * " of Respiratory Disease Cases")))

# Model cardiovascular: number of cases ~ PM 2.5 ####
# dataset filtering out zeroes
card_filtered <- combined_data %>% filter(cardiovascular > 0)
m_card <- gam(cardiovascular ~ pm25_SUM + s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = card_filtered, family= nb(), method = 'REML')

summary(m_card)

# calculate r2
summary(m_card)$r.sq

# run model diagnostics # thats bad
# testDispersion(m_card)
# testResiduals(m_card, plot = TRUE)

# Plot predictions
(pred_card <- ggpredict(m_card, terms= "pm25_SUM"))
(p_card <- pred_card %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
  labs(title= "", x = expression(PM[2.5]),
       y = expression(N^o * " of Cardiovascular Disease Cases")))

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
m_card_2yr <- gam(cardiovascular ~ pm25_2yr + s(X, Y) +
                    s(year, bs = 're') +
                    offset(IDH),
                  data = card_filtered_lag_2, family = nb(), method = 'REML')

# Summary of the models
summary(m_card_2yr)

# R²
(r2_2yr <- summary(m_card_2yr)$r.sq)

# Run model diagnostics using DHARMa
# testDispersion(m_card_2yr)
# testResiduals(m_card_2yr, plot = TRUE)

# Plot Predictions for 2-year lag model
(pred_card_2yr <- ggpredict(m_card_2yr, terms = "pm25_2yr"))
p_card_2yr <- pred_card_2yr %>% plot() +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  theme_cowplot() +
  labs(title = "", x = expression("2-yr accumulated PM"[2.5]),
       y = expression(N^o * " of Cardiovascular Disease Cases"))


# Fit the GAM model with 5-year lag
card_filtered_lag_5 <- card_filtered_lag %>%
  filter(!is.na(pm25_5yr))

m_card_5yr <- gam(cardiovascular ~ pm25_5yr + s(X, Y) +
                    s(year, bs = 're') +
                    offset(IDH),
                  data = card_filtered_lag_5, family = nb(), method = 'REML')

# Summary of the models
summary(m_card_5yr)

# R²
(r2_5yr <- summary(m_card_5yr)$r.sq)

# Run model diagnostics using DHARMa
# testDispersion(m_card_5yr)
# testResiduals(m_card_5yr, plot = TRUE)

# Plot Predictions for 5
(pred_card_5yr <- ggpredict(m_card_5yr, terms = "pm25_5yr"))
(p_card_5yr <- pred_card_5yr %>% plot() +
    geom_line(color = "darkgreen") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
    theme_cowplot() +
    labs(title = "", x = expression("5-yr accumulated PM"[2.5]),
         y = expression(N^o * " of Cardiovascular Disease Cases")))


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
    plot_annotation(tag_levels = "A")

  ggsave(here("data", "descriptive_figs", "GAM_fire_PM25.png"),
         width = 15, height = 5, units = "in", dpi = 300)

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
  increase_per_hectare_card_5yr <- exp(pm25_per_hecÿare * estimate_card_5yr)
  
  # Print the results
  cat("Increase per hectare (fire-related):", increase_per_hectare_fire - 1, "cases per hectare per year\n")
  cat("Increase per hectare (respiratory):", increase_per_hectare_resp - 1, "cases per hectare per year\n")
  cat("Increase per hectare (cardiovascular after 5 years):", increase_per_hectare_card_5yr - 1, "cases per hectare\n")

# # Code graveyard ####
# # ## this below gone afterr meeting july 17th ####
  # # to do:
  #   # unscale (we're not using more than one predictor at a time)
  #   # fit gama models as filipa is doing
  #   # remove IDH, it matters less for fire-related
  # 
  # # Loading the data and data prep ####
  # ## load
  # data_inc <- read.csv(here("data", "incidence_year_COD.csv")) %>%
  #   ## add predictors
  #   left_join(combined_data %>% dplyr::select(1:3, 25:42), 
  #             by = c("country", "COD", "year"))
  # 
  # str(data_inc)
  # 
  # 
  # # ## Scale predictors
  # # ### to ensure they are on comparable scales, might help with model convergence
  # # manual_scale <- function(x) {
  # #   return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  # # }
  # # 
  # # data_inc <- data_inc %>%
  # #   mutate(for_PLAND_scaled = manual_scale(for_PLAND),
  # #          for_PD_scaled= manual_scale(for_PD),
  # #          for_ED_scaled= manual_scale(for_ED),
  # #          for_AI_scaled= manual_scale(for_AI),
  # #          savanna_PLAND_scaled = manual_scale(savanna_PLAND),
  # #          pm25_SUM_scaled = manual_scale(pm25_SUM))
  # 
  # ## Filter out rows with missing values only for the required columns for each model
  # data_filtered_m1 <- data_inc %>% filter(pm25_SUM >0 ,!is.na(pm25_SUM) & !is.na(for_PLAND))
  # data_filtered_m2 <- data_inc %>% filter(!is.na(wfire_inc) & !is.na(pm25_SUM))
  # data_filtered_m3 <- data_inc %>% filter(!is.na(wrespiratory_inc) & !is.na(pm25_SUM))
  # data_filtered_m4 <- data_inc %>% filter(!is.na(wcardiovascular_inc) & !is.na(pm25_SUM))
  # 
  # 
  # # Select models structure (testing the best model fit (glm or glmer) ####
  # m2 <- lmer(wfire_inc ~ 1 + (1 | year) + (1 | country), 
  #            data = data_filtered_m2)
  # 
  # 
  # lm1 <- glm(log10(wfire_inc+ 1) + 1 ~ 1, data=data_filtered_m2, family=Gamma(link = "log"))
  # plot(simulateResiduals(fittedModel = lm1))
  # 
  # mm1 <- glmer(log10(wfire_inc+ 1) +1 ~ 1 + (1|year), 
  #              data=data_filtered_m2, family=Gamma(link = "log") #, 
  #              # control=glmerControl(check.conv.singular="warning",optCtrl=list(maxfun=10000))) 
  # )
  # plot(simulateResiduals(fittedModel = mm1))
  # 
  # mm2 <-  glmer(log10(wfire_inc+1) +1 ~ 1 + (1|year) + (1|country), 
  #               data=data_filtered_m2, family=Gamma(link = "log")#,
  #               # control=glmerControl(check.conv.singular="warning",optCtrl=list(maxfun=10000))
  # )
  # 
  # AICtab(lm1, mm1, mm2, m2)
  # 
  # plot(simulateResiduals(fittedModel = m2))
  # 
  # # MODELLING ####
  # 
  # ## Model set 1: PM2.5 ~ for_PLAND
  # m1 <- lmer(pm25_SUM ~ for_PLAND + (1 | year), 
  #            data = data_filtered_m1) # RE year; fit ok
  # summary(m1)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m1 <- r.squaredGLMM(m1))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m1))
  # testDispersion(m1)
  # 
  # # Plot predictions
  # (pred_m1 <- ggpredict(m1, terms = "for_PLAND"))
  # (m1_plot <- pred_m1 %>% plot() +
  #   geom_line(color = "darkgreen") +
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #   theme_cowplot() +
  #   labs(title = " ",
  #        x = "Forest cover percent",
  #        y = "pred Fire pollutant (PM2.5)"))
  # 
  # 
  # ## add models for fragmentation ####
  # 
  # ## Model set 1: PM2.5 ~ for_PD
  # m12 <- lmer(pm25_SUM ~ for_PD + (1 | year), 
  #            data = data_filtered_m1) # RE year; fit ok
  # summary(m12)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m12 <- r.squaredGLMM(m12))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m12))
  # testDispersion(m12)
  # 
  # # Plot predictions
  # (pred_m12 <- ggpredict(m12, terms = "for_PD"))
  # (pd <- pred_m12 %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = " ",
  #          x = "Patch Density",
  #          y = "pred PM2.5"))
  # 
  # ## Model set 1: PM2.5 ~ for_ED
  # m13 <- lmer(pm25_SUM ~ for_ED + (1 | year),
  #             data = data_filtered_m1) # RE year; fit ok
  # summary(m13)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m13 <- r.squaredGLMM(m13))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m13))
  # testDispersion(m13)
  # 
  # # Plot predictions
  # (pred_m13 <- ggpredict(m13, terms = "for_ED"))
  # (ed <- pred_m13 %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = " ",
  #          x = "Edge Density",
  #          y = "pred PM2.5"))
  # 
  # ## Model set 1: PM2.5 ~ for_AI
  # m14 <- lmer(pm25_SUM ~ for_AI + (1 | year),
  #             data = data_filtered_m1) # RE year; fit ok
  # summary(m14)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m14 <- r.squaredGLMM(m14))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m14))
  # testDispersion(m14)
  # 
  # # Plot predictions
  # (pred_m14 <- ggpredict(m14, terms = "for_AI"))
  # (ai <- pred_m14 %>% plot() +
  #   geom_line(color = "darkgreen") +
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #   theme_cowplot() +
  #   labs(title = " ",
  #        x = "Aggregation Index",
  #        y = "pred PM2.5"))
  # 
  # 
  # ## Model set 1: PM2.5 ~ savanna_PLAND
  # m15 <- lmer(pm25_SUM ~ savanna_PLAND + (1 | year),
  #             data = data_filtered_m1) # RE year; fit ok
  # summary(m15)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m15 <- r.squaredGLMM(m15))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m15))
  # testDispersion(m15)
  # 
  # # Plot predictions
  # (pred_m15 <- ggpredict(m15, terms = "savanna_PLAND"))
  # (sav <- pred_m15 %>% plot() +
  #   geom_line(color = "darkgreen") +
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #   theme_cowplot() +
  #   labs(title = " ",
  #        x = "Savanna Cover Percent",
  #        y = "pred PM2.5"))
  # 
  # m1_plot/ (pd + ed + ai + sav)
  # 
  # 
  # ## Fire-related regular models ####
  # # Model 2: wfire_inc ~ pm25_SUM + HDI + (1 | year) + (1 | country)
  # m2 <- lmer(wfire_inc ~ scale(pm25_SUM) + (1 | year) + (1 | country),
  #            data = data_filtered_m2)
  # anova(m2)
  # 
  # summary(m2)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m2 <- r.squaredGLMM(m2))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m2))
  # testDispersion(m2) # non-sig
  # 
  # # Plot predictions
  # (pred_m2 <- ggpredict(m2, terms = "pm25_SUM"))
  # 
  # # Define the y-axis limits
  # y_limits <- c(min(pred_m3$predicted, pred_m3$conf.low), max(pred_m3$predicted, pred_m3$conf.high))
  # 
  # 
  # (fire_pm <- ggplot(pred_m2, aes(x = x, y = predicted)) +
  #     geom_line(color = "darkgreen") +
  #     theme_cowplot() +
  #     labs(title = " ",
  #          x = "Fire pollutant (PM2.5)",
  #          y = "pred Fire-related diseases/100k people"))
  # 
  # # Model 3: wrespiratory_inc ~ pm25_SUM + (1|country) + (1 | year)
  # m3 <- lmer(wrespiratory_inc ~ scale(pm25_SUM) + (1 | year) + (1 | country),
  #            data = data_filtered_m3)
  # summary(m3)
  # anova(m3)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m3 <- r.squaredGLMM(m3))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m3))
  # testDispersion(m3) # non-sig
  # 
  # # Plot predictions
  # (pred_m3 <- ggpredict(m3, terms = "pm25_SUM"))
  # # Define the y-axis limits
  # y_limits <- c(0, max(pred_m3$predicted, pred_m3$conf.high))
  # 
  # (resp_pm <- ggplot(pred_m3, aes(x = x, y = predicted)) +
  #     geom_line(color = "darkgreen") +
  #     theme_cowplot() +
  #   labs(title = " ",
  #        x = "Fire pollutant (PM2.5)",
  #        y = "pred Respiratory diseases/100k") + ylim(y_limits))
  # 
  # # Model 4: wcardiovascular_inc ~ pm25_SUM + HDI + (1 | year) + (1 | country)
  # m4 <- lmer(wcardiovascular_inc ~ scale(pm25_SUM) + (1 | country),
  #            data = data_filtered_m4) # absent variance for year as RE
  # summary(m4)
  # anova(m4)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m4 <- r.squaredGLMM(m4))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m4))
  # testDispersion(m4)
  # 
  # # Plot predictions
  # (pred_m4 <- ggpredict(m4, terms = "pm25_SUM"))
  # 
  # (card_pm <- ggplot(pred_m2, aes(x = x, y = predicted)) +
  #     geom_line(color = "darkgreen") +
  #     theme_cowplot() +
  #   labs(title = " ",
  #        x = "Fire pollutant (PM2.5)",
  #        y = "pred Cardiovascular diseases/100k") + 
  #     ylim(y_limits)) # standardize the y-axis limits across plots
  # 
  # 
  # m1_plot + (fire_pm + ylim(y_limits)) + (resp_pm / card_pm)  +
  #   plot_annotation(tag_levels = "A")
  # 
  # ggsave(here("data", "descriptive_figs", "fire_PM25.png"),
  #        width = 19, height = 8, units = "in", dpi = 300)
  # 
  # (fire_pm + ylim(y_limits) + xlab("")) + resp_pm + (card_pm  + xlab(""))+
  #   plot_annotation(tag_levels = "A")
  # 
  # ggsave(here("data", "descriptive_figs", "fire_PM252.png"),
  #        width = 12, height = 4, units = "in", dpi = 300)
  # 
  # 
  # # RESID PM ####
  # # Model 5: Residuals of pm25_SUM ~ wfire_inc
  # data_filtered_m1 <- data_filtered_m1 %>%
  #   mutate(residuals_m1 = residuals(m1, type = "response"))
  # 
  # m5 <- lmer(wfire_inc ~ scale(residuals_m1) + (1 | year) + (1 | country),
  #            data = data_filtered_m1 %>% filter(!is.na(wfire_inc) & !is.na(residuals_m1)))
  # summary(m5)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m5 <- r.squaredGLMM(m5))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m5))
  # testDispersion(m5) # non-sig
  # 
  # # Plot predictions
  # (pred_m5 <- ggpredict(m5, terms = "residuals_m1"))
  # 
  # (res_fire <- pred_m5 %>% plot() +
  #     theme_cowplot() +
  #     # geom_line(color = "darkgrey") +
  #     # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgrey", alpha = 0.05) +
  #     labs(title = " ",
  #          x = "Residuals PM2.5 ~ % Forest",
  #          y = "Fire-related diseases/100,000 people"))
  # 
  # # Model 6: Residuals of pm25_SUM ~ wrespiratory_inc
  # m6 <- lmer(wrespiratory_inc ~ scale(residuals_m1) + (1 | year) + (1 | country),
  #            data = data_filtered_m1 %>% filter(!is.na(wrespiratory_inc) & !is.na(residuals_m1)))
  # summary(m6)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m6 <- r.squaredGLMM(m6))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m6))
  # testDispersion(m6)
  # 
  # # Plot predictions
  # (pred_m6 <- ggpredict(m5, terms = "residuals_m1"))
  # 
  # (res_resp <- pred_m6 %>% plot() +
  #     theme_cowplot() +
  #     # geom_line(color = "darkgrey") +
  #     # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgrey", alpha = 0.05) +
  #     labs(title = " ",
  #          x = "Residuals PM2.5 ~ % Forest",
  #          y = "Respiratory diseases/100k people"))
  # 
  # # Model 7: Residuals of pm25_SUM ~ wcardiovascular_inc
  # m7 <- lmer(wcardiovascular_inc ~ scale(residuals_m1) + (1 | year) + (1 | country),
  #            data = data_filtered_m1 %>% filter(!is.na(wcardiovascular_inc) & !is.na(residuals_m1)))
  # summary(m7)
  # 
  # ## calculate the marginal and conditional r2
  # (r2_m7 <- r.squaredGLMM(m7))
  # 
  # ## run model diagnostics # thats AWFUL
  # plot(simulateResiduals(fittedModel = m7))
  # testDispersion(m7)
  # 
  # # Plot predictions
  # (pred_m7 <- ggpredict(m7, terms = "residuals_m1"))
  # 
  # (res_card <- pred_m7 %>% plot() +
  #     theme_cowplot() +
  #     # geom_line(color = "darkgrey") +
  #     # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgrey", alpha = 0.05) +
  #     labs(title = " ",
  #          x = "Residuals PM2.5 ~ % Forest",
  #          y = "Cardiovascular diseases/100k"))
  # 
  # 
  # # PLOT PANEL ####
  # # Combine the plots using patchwork
  # m1_plot + res_resp + (res_card / res_fire) +
  #   plot_annotation(tag_levels = "A")
  # 
  # # ggsave(here("data", "descriptive_figs", "fire_PM25.png"),
  # #        width = 20, height = 8, units = "in", dpi = 300)
  # 
  # ## Interpretation ####
  # 
  # # While higher forest cover positively associated with increased fire pollutants (Estimate = 0.32, R²c = 0.9; Figure Xa), the amount of fire pollutant levels not explained by forest cover (PM2.5 residuals) was positively associated with the incidence of respiratory diseases (Estimate = 105,230, p = 0.002, R²M = 0.0007, R²C = 0.98; Figure 1b). Which indicates that, for every unit increase in residual PM2.5, there are approximately 1,052.3 new cases of respiratory diseases per 10,000 population. For cardiovascular diseases incidence, this relationship was negative (Estimate = -998, R²M = 0.23, R²C = 0.72; Figure Xc), this contrast resulted in no significant effect of the amount of PM2.5 not explained by forest cover on overall fire-related disease incidence (R²M = 0.0001, R²C = 0.11; Figure 1d).
  # 
  # # To translate the fixed effects into meaningful effect sizes, we need to understand the units of the predictor and outcome variables. Given that wrespiratory_inc is likely the incidence of respiratory diseases and residuals_m1 represents the residuals from the PM2.5 ~ forest cover model, we can interpret the coefficients in terms of the incidence rates.
  # # 
  # # For every unit increase in the residual PM2.5 not explained by forest cover, the incidence of respiratory diseases increases by approximately 105,230 cases per unit of PM2.5. To standardize this to a population of 10,000, we need to scale this effect size accordingly.
  # # 
  # # Here is the interpretation in terms of number of new cases per 10,000 population for every unit increase in PM2.5:
  # #   
  # #   Given that the estimate is 105,230 cases for the total population, we first need to convert this to a per capita rate. If the population size represented by wrespiratory_inc is known, the effect size can be adjusted. However, in the absence of specific population data, the simplest approach is to standardize the effect size directly:
  # #   
  # #   Calculate the per capita increase in cases.
  # # Multiply by 10,000 to get the effect size per 10,000 population.
  # # Let's assume the total population represented in the dataset is 
  # # N
  # # N. Then:
  # # 
  # # Effect per capita
  # # =
  # # 105
  # # ,
  # # 230
  # # N
  # # Effect per capita= 
  # # N
  # # 105,230
  # # ​	
  # #  
  # # 
  # # For a population of 10,000:
  # # 
  # # Effect per 10,000
  # # =
  # # 105
  # # ,
  # # 230
  # # N
  # # ×
  # # 10
  # # ,
  # # 000
  # # Effect per 10,000= 
  # # N
  # # 105,230
  # # ​	
  # #  ×10,000
  # # 
  # # Without the exact population size 
  # # N
  # # N, it's challenging to provide an accurate effect size per 10,000 population. However, we can state the effect size relative to the observed data.
  # 
  # 
  # 
# # # tried Filipas fit of log 10 ####
# # # but super underdispersed and fain to converge with country and or year...
# # # Model 2: wfire_inc ~ pm25_SUM + HDI + (1 | year) + (1 | country)
# # m2 <-  glmer(log10(wfire_inc+1) +1 ~ scale(pm25_SUM) + (1|year), 
# #              data=data_filtered_m2, family=Gamma(link = "log")#,
# #              # control=glmerControl(check.conv.singular="warning",optCtrl=list(maxfun=10000))
# # )
# # 
# # summary(m2)
# # 
# # ## calculate the marginal and conditional r2
# # (r2_m2 <- r.squaredGLMM(m2))
# # 
# # ## run model diagnostics # thats AWFUL
# # plot(simulateResiduals(fittedModel = m2))
# # testDispersion(m2) # super under dispersed
# # 
# # # Plot predictions
# # (pred_m2 <- ggpredict(m2, terms = "pm25_SUM"))
# # 
# # pred_m2 %>% plot() +
# #   theme_cowplot() +
# #   # geom_line(color = "darkgrey") +
# #   # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgrey", alpha = 0.05) +
# #   labs(title = " ",
# #        x = "Fire pollutant (PM2.5)",
# #        y = "pred Fire-related diseases incidence")
# 
# 
# # testing models : #### 
# # M3  wrespiriratory_inc ~ pm25_SUM + HDI + (1 | year) + (1 | country)
# # m3 <- lmer(wrespiratory_inc ~ pm25_SUM + (1 | year) + (1 | country), 
# #            data = data_filtered_m3)
# # summary(m3)
# # 
# # ## warning indicates that the model may not have fully converged, which can occur due to numerical issues, model complexity, or data characteristics
# # 
# # ## increase of iterations for the optimizer using the control argument in lmer
# # m3 <- lmer(wrespiratory_inc ~ pm25_SUM + (1 | year) + (1 | country), 
# #            data = data_filtered_m3, 
# #            control = lmerControl(optCtrl = list(maxfun = 100000)))
# # ## still NO
# # 
# # ## sttempt 2) Check for Influential Points
# # library(influence.ME)
# # 
# # # Fit the model again to use it with influence.ME
# # m3 <- lmer(wrespiratory_inc ~ pm25_SUM + (1 | year) + (1 | country), 
# #            data = data_filtered_m3)
# # 
# # # Calculate the influence measures
# # infl <- influence(m3, obs = TRUE) ## took ages and it didnt run
# # 
# # # Plot the influence measures
# # plot(infl)
# # 
# # # Get the most influential points
# # summary(infl)
# # 
# # # simplify the random effects structure
# # m3_simplified <- lmer(wrespiratory_inc ~ pm25_SUM + (1 | year), 
# #                       data = data_filtered_m3)
# # summary(m3_simplified)
  
  
  
  
  # ARCHIVE ####
  
  # # Timelag ####
  # 
  # ## Fire-related timelag pm ####
  #   fire_filtered_lag <- fire_filtered %>%
  #     group_by(COD) %>%
  #     arrange(year) %>%
  #     mutate(
  #       pm25_2yr = zoo::rollapply(pm25_SUM, width = 2, FUN = sum, align = "right", fill = NA, partial = FALSE),
  #       pm25_5yr = zoo::rollapply(pm25_SUM, width = 5, FUN = sum, align = "right", fill = NA, partial = FALSE)
  #     ) %>%
  #     ungroup()
  #   
  #   
  #   # Remove NA values that result from lag operations
  #   fire_filtered_lag_2 <- fire_filtered_lag %>%
  #     filter(!is.na(pm25_2yr))
  #   
  #   
  #   # Fit the GAM model with 2-year lag
  #   m_fire_2yr <- gam(fire_related ~ pm25_2yr + s(X, Y) +
  #                       s(year, bs = 're') +
  #                       offset(IDH),
  #                     data = fire_filtered_lag_2, family = nb(), method = 'REML')
  #   
  #   # Summary of the models
  #   summary(m_fire_2yr)
  #   
  #   # R²
  #   (r2_2yr <- summary(m_fire_2yr)$r.sq)
  #   
  #   # Run model diagnostics using DHARMa
  #   # testDispersion(m_fire_2yr)
  #   # testResiduals(m_fire_2yr, plot = TRUE)
  #   
  #   # Plot Predictions for 2-year lag model
  #   (pred_fire_2yr <- ggpredict(m_fire_2yr, terms = "pm25_2yr"))
  #   p_fire_2yr <- pred_fire_2yr %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = "", x = expression("2-yr accumulated PM"[2.5]),
  #          y = expression(N^o * " of Fire-related Disease Cases"))
  #   
  # 
  # # Fit the GAM model with 5-year lag
  #   fire_filtered_lag_5 <- fire_filtered_lag %>%
  #     filter(!is.na(pm25_5yr))
  #   
  #   m_fire_5yr <- gam(fire_related ~ pm25_5yr + s(X, Y) +
  #                       s(year, bs = 're') +
  #                       offset(IDH),
  #                     data = fire_filtered_lag_5, family = nb(), method = 'REML')
  #   
  #   # Summary of the models
  #   summary(m_fire_5yr)
  #   
  #   # R²
  #   r2_5yr <- summary(m_fire_5yr)$r.sq
  #   
  #   # Run model diagnostics using DHARMa
  #   testDispersion(m_fire_5yr)
  #   testResiduals(m_fire_5yr, plot = TRUE)
  #   
  #   # Plot Predictions for 5
  #   (pred_fire_5yr <- ggpredict(m_fire_5yr, terms = "pm25_5yr"))
  #   (p_fire_5yr <- pred_fire_5yr %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = "", x = expression("5-yr accumulated PM"[2.5]),
  #          y = expression(N^o * " of Fire-related Disease Cases")))
  #   
  # 
  # AICtab(m_fire, m_fire_2yr, m_fire_5yr)
  # 
  # 
  # # summary(m_fire)$r.sq
  # # [1] 0.8138609
  # # > summary(m_fire_2yr)$r.sq
  # # [1] 0.8234057
  # # > summary(m_fire_5yr)$r.sq
  # # [1] 0.8351074
  # 
  # # The 5-year lag model has an adjusted R-squared that is 0.022 higher than yr
  # 
  # ## Resp timelag pm ####
  # 
  # resp_filtered_lag <- resp_filtered %>%
  #   group_by(COD) %>%
  #   arrange(year) %>%
  #   mutate(
  #     pm25_2yr = zoo::rollapply(pm25_SUM, width = 2, FUN = sum, align = "right", fill = NA, partial = FALSE),
  #     pm25_5yr = zoo::rollapply(pm25_SUM, width = 5, FUN = sum, align = "right", fill = NA, partial = FALSE)
  #   ) %>%
  #   ungroup()
  # 
  # # Remove NA values that result from lag operations
  # resp_filtered_lag_2 <- resp_filtered_lag %>%
  #   filter(!is.na(pm25_2yr))
  # 
  # 
  # # Fit the GAM model with 2-year lag
  # m_resp_2yr <- gam(respiratory ~ pm25_2yr + s(X, Y) +
  #                     s(year, bs = 're') +
  #                     offset(IDH),
  #                   data = resp_filtered_lag_2, family = nb(), method = 'REML')
  # 
  # # Summary of the models
  # summary(m_resp_2yr)
  # 
  # # R²
  # (r2_2yr <- summary(m_resp_2yr)$r.sq)
  # 
  # # Run model diagnostics using DHARMa
  # # testDispersion(m_resp_2yr)
  # # testResiduals(m_resp_2yr, plot = TRUE)
  # 
  # # Plot Predictions for 2-year lag model
  # (pred_resp_2yr <- ggpredict(m_resp_2yr, terms = "pm25_2yr"))
  # p_resp_2yr <- pred_resp_2yr %>% plot() +
  #   geom_line(color = "darkgreen") +
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #   theme_cowplot() +
  #   labs(title = "", x = expression("2-yr accumulated PM"[2.5]),
  #        y = expression(N^o * " of Respiratory Disease Cases"))
  # 
  # 
  # # Fit the GAM model with 5-year lag
  # resp_filtered_lag_5 <- resp_filtered_lag %>%
  #   filter(!is.na(pm25_5yr))
  # 
  # m_resp_5yr <- gam(respiratory ~ pm25_5yr + s(X, Y) +
  #                     s(year, bs = 're') +
  #                     offset(IDH),
  #                   data = resp_filtered_lag_5, family = nb(), method = 'REML')
  # 
  # # Summary of the models
  # summary(m_resp_5yr)
  # 
  # # R²
  # r2_5yr <- summary(m_resp_5yr)$r.sq
  # 
  # # Run model diagnostics using DHARMa
  # testDispersion(m_resp_5yr)
  # testResiduals(m_resp_5yr, plot = TRUE)
  # 
  # # Plot Predictions for 5
  # (pred_resp_5yr <- ggpredict(m_resp_5yr, terms = "pm25_5yr"))
  # (p_resp_5yr <- pred_resp_5yr %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = "", x = expression("5-yr accumulated PM"[2.5]),
  #          y = expression(N^o * " of Respiratory Disease Cases")))
  # 
  # 
  # AICtab(m_resp, m_resp_2yr, m_resp_5yr)
  # 
  # # summary(m_resp)$r.sq
  # # [1] 0.8136975
  # # > summary(m_resp_2yr)$r.sq
  # # [1] 0.8230642
  # # > summary(m_resp_5yr)$r.sq
  # # [1] 0.834321
  # # 5-year lag model has an adjusted R-squared that is approximately 0.0206 higher 
  # 
  # ## Cardiovascular timelag pm ####
  # 
  # card_filtered_lag <- card_filtered %>%
  #   group_by(COD) %>%
  #   arrange(year) %>%
  #   mutate(
  #     pm25_2yr = zoo::rollapply(pm25_SUM, width = 2, FUN = sum, align = "right", fill = NA, partial = FALSE),
  #     pm25_5yr = zoo::rollapply(pm25_SUM, width = 5, FUN = sum, align = "right", fill = NA, partial = FALSE)
  #   ) %>%
  #   ungroup()
  # 
  # 
  # # Remove NA values that result from lag operations
  # card_filtered_lag_2 <- card_filtered_lag %>%
  #   filter(!is.na(pm25_2yr))
  # 
  # 
  # # Fit the GAM model with 2-year lag
  # m_card_2yr <- gam(cardiovascular ~ pm25_2yr + s(X, Y) +
  #                     s(year, bs = 're') +
  #                     offset(IDH),
  #                   data = card_filtered_lag_2, family = nb(), method = 'REML')
  # 
  # # Summary of the models
  # summary(m_card_2yr)
  # 
  # # R²
  # (r2_2yr <- summary(m_card_2yr)$r.sq)
  # 
  # # Run model diagnostics using DHARMa
  # # testDispersion(m_card_2yr)
  # # testResiduals(m_card_2yr, plot = TRUE)
  # 
  # # Plot Predictions for 2-year lag model
  # (pred_card_2yr <- ggpredict(m_card_2yr, terms = "pm25_2yr"))
  # p_card_2yr <- pred_card_2yr %>% plot() +
  #   geom_line(color = "darkgreen") +
  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #   theme_cowplot() +
  #   labs(title = "", x = expression("2-yr accumulated PM"[2.5]),
  #        y = expression(N^o * " of Cardiovascular Disease Cases"))
  # 
  # 
  # # Fit the GAM model with 5-year lag
  # card_filtered_lag_5 <- card_filtered_lag %>%
  #   filter(!is.na(pm25_5yr))
  # 
  # m_card_5yr <- gam(cardiovascular ~ pm25_5yr + s(X, Y) +
  #                     s(year, bs = 're') +
  #                     offset(IDH),
  #                   data = card_filtered_lag_5, family = nb(), method = 'REML')
  # 
  # # Summary of the models
  # summary(m_card_5yr)
  # 
  # # R²
  # r2_5yr <- summary(m_card_5yr)$r.sq
  # 
  # # Run model diagnostics using DHARMa
  # testDispersion(m_card_5yr)
  # testResiduals(m_card_5yr, plot = TRUE)
  # 
  # # Plot Predictions for 5
  # (pred_card_5yr <- ggpredict(m_card_5yr, terms = "pm25_5yr"))
  # (p_card_5yr <- pred_card_5yr %>% plot() +
  #     geom_line(color = "darkgreen") +
  #     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.05) +
  #     theme_cowplot() +
  #     labs(title = "", x = expression("5-yr accumulated PM"[2.5]),
  #          y = expression(N^o * " of Cardiovascular Disease Cases")))
  # 
  # 
  # AICtab(m_card, m_card_2yr, m_card_5yr)
  # 
  # # > summary(m_card)$r.sq
  # # [1] 0.06023809
  # # > summary(m_card_2yr)$r.sq
  # # [1] 0.07460868
  # # > summary(m_card_5yr)$r.sq
  # # [1] 0.07506598
  # 
  # # 5-year lag has an adjusted R-squared that is ~0.0148 higher than yr model
  # # This improvement, though smaller compared to the fire-related models, 
  #   # still supports the idea that cumulative exposure to PM2.5 over multiple years 
  #   # provides a better explanation for variations in cardiovascular disease cases than yearly exposure
  # 
  # 
  # # # Combine the plots using patchwork
  # (p_fire_5yr + xlab("")) + p_resp_5yr + (p_card_5yr  + xlab(""))+
  #   plot_annotation(tag_levels = "A")
  # 
  # ggsave(here("data", "descriptive_figs", "GAM_fire_PM25.png"),
  #        width = 14, height = 4, units = "in", dpi = 300)