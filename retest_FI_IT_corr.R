# Correlation forest and TI

data0<-read.csv(here("data", "combined_data.csv"), header = T)
str(data0)

# Forest/savanna PLAND vs total IT
round(cor(data0$FS_PLAND, data0$tot_IT, method = "spearman"),3) # -0.127

# tbm outros proxys de floresta
round(cor(data0$for_PD, data0$tot_IT, method = "spearman"),3) # -0.21
round(cor(data0$for_ED, data0$tot_IT, method = "spearman"),3) # -0.314
round(cor(data0$for_AI, data0$tot_IT, method = "spearman"),3) # -0.122


# Forest/savanna PLAND vs total IT (removing negative values of FS_noIT)
data_sem0 <- data0 %>% filter(FS_noIT >= 0)

round(cor(data_sem0$FS_PLAND, data_sem0$tot_IT, method = "spearman"),3) # 0.529

# tbm outros proxys de floresta
round(cor(data_sem0$for_PD, data_sem0$tot_IT, method = "spearman"),3) # -0.365
round(cor(data_sem0$for_ED, data_sem0$tot_IT, method = "spearman"),3) # -0.189
round(cor(data_sem0$for_AI, data_sem0$tot_IT, method = "spearman"),3) # 0.51

## Model set 1: for_PLAND ~ IT
m1 <- lmer(FS_PLAND ~ tot_IT+ (1|country),
           data = data_sem0)
summary(m1)

## calculate the marginal and conditional r2
(r.squaredGLMM(m1))

## run model diagnostics # thats AWFUL
plot(simulateResiduals(fittedModel = m1))
testDispersion(m1)

# Plot predictions
(pred_m1 <- ggeffects::ggpredict(m1, terms = "tot_IT"))
(m1_plot <- pred_m1 %>% 
  plot() +
  # geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.05) +
  theme_cowplot() +
  labs(title = " ",
       x = "Indigenous territories percent",
       y = "Forest-savanna native cover percent") +
  annotate("text", x = 1, y = 140, 
           label = bquote(beta == .(round(summary(m1)$coefficients[2,1], 2)) ~
                            "±sd" ~ .(round(summary(m1)$coefficients[2,2], 3)) ~ "; p= 0.00"),
           color = "black", hjust = 0))







###### testing models including both
m_fire2 <- gam(fire_related ~ tot_IT + FS_PLAND + 
                 s(X,Y)+
                s(year, bs = 're') +
                offset(IDH),
              data = fire_filtered, family= nb(), method = 'REML')

summary(m_fire2)

## calculate the marginal and conditional r2
summary(m_fire2)$r.sq

## run model diagnostics # thats AWFUL
plot(simulateResiduals(fittedModel = m_fire2))
testDispersion(m_fire2)

# Plot predictions
(pred_m2 <- ggeffects::ggpredict(m1, terms = c("tot_IT", "FS_PLAND")))
(m1_plot <- pred_m1 %>% 
    plot() +
    # geom_line(color = "darkgreen") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.05) +
    theme_cowplot() +
    labs(title = " ",
         x = "Indigenous territories percent",
         y = "Forest-savanna native cover percent") +
    annotate("text", x = 1, y = 140, 
             label = bquote(beta == .(round(summary(m1)$coefficients[2,1], 2)) ~
                              "±sd" ~ .(round(summary(m1)$coefficients[2,2], 3)) ~ "; p= 0.00"),
             color = "black", hjust = 0))


zoo <- glmer(zoonotic ~ scale(tot_IT) + scale(FS_PLAND) + offset(IDH) + (1|year) + (1|country),
               binomial(link = "logit"), data = binomial.zoo.data,
               control=glmerControl(check.conv.singular="warning",optCtrl=list(maxfun=10000)))
summary(zoo.2); AIC(zoo.2); r.squaredGLMM(zoo.2)

cor(fire.data2$FS_PLAND, fire.data2$FS_IT) # 0.42
fire.x1 <- glmer(log10(fire.incidence.std + 1) ~ scale(FS_PLAND) + scale(I(FS_PLAND^2)) +
                   scale(FS_IT) + scale(I(FS_IT^2)) +
                   scale(for_PD) + scale(for_AI) + offset(IDH) + (1|year) + (1| country),
                 family= Gamma(link = "log"), data = fire.data2,
                 control=glmerControl(check.conv.singular="warning",optCtrl=list(maxfun=10000)))
summary(fire.x1); AIC(fire.x1) # 29698.5
