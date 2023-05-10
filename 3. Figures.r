########################
##FINAL THESIS FIGURES##
########################

##Figure 1##
############

final_tract %>%
    filter(!(tract_majority %in% NA)) %>%
    ggplot( aes(x=tract_majority, y=log1p(deepsolar_res_installs), fill=tract_majority)) +
    geom_violin() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="D") +
    theme_ipsum() +
    theme(
        legend.position="none",
        plot.title = element_text(size=40),
        axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
    ) +
    ggtitle("Project Sunroof Residential Installations by Racial/Ethnic Majority") +
    xlab("Racial/Ethnic Majority Classification") +
    ylab("Ln(DeepSolar Residential Installations + 1)")


##Figure 2##
############

final_tract %>%
    filter(
        !(tract_majority %in% NA),
        !(deepsolar_logit) %in% NA
        ) %>%
    ggplot(aes(fill=tract_majority, x=factor(deepsolar_logit))) + 
    geom_bar(position="fill", stat="count") +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="D") +
    theme_ipsum() +
    theme(
        plot.title = element_text(size=40),
        axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
        ) +
    scale_x_discrete(labels=c('0', '>0')) +
    ggtitle("Racial/Ethnic Composition of Tracts with 0 or >0 Installation(s), DeepSolar") +
    xlab("Number of Installations") +
    ylab("Proportion of Total Tracts")+
    labs(fill = "Racial/Ethnic Majority Classification")


##Figure 4##
############
##Create a new dataframe with simplified geometries.  This will make the mapping go faster.

final_simple <- st_simplify(final_tract, preserveTopology = FALSE, dTolerance = 1000)

final_simple %>% 
    filter(statefips != "02",
    statefips != "15",
    !(tract_majority %in% NA)
    ) %>% 
ggplot(aes(fill = tract_majority)) +
facet_wrap(~ tract_majority) +
geom_sf(lwd = 0) +
borders(database = "usa", size=0.5) +
coord_sf(datum = NA) +
scale_fill_viridis(discrete = TRUE, alpha=0.6, option="D") +
theme_map() +
theme_ipsum() +
theme(legend.position = "none")


##Figure 5##
############
dwplot(list(Model_3 = dslog4, Model_4 = dslog5, Model_5 = dslog6), 
       vars_order = c("black_maj","asian_maj","hispanic_maj", "no_maj"), 
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2),
       model_order = c("Model_3", "Model_4", "Model_5")
       ) %>%
       relabel_predictors(
           c(black_maj = "Majority-Black", 
             asian_maj = "Majority-Asian", 
             hispanic_maj = "Majority-Hispanic", 
             no_maj = "No Majority")
           ) + 
        coord_cartesian(xlim = c(-1, 1)) +
        xlab("Coefficient Estimate") + 
        theme_ipsum() + 
        theme(legend.position = "right")

dwplot(list(Model_3 = srlog4, Model_4 = srlog5, Model_5 = srlog6), 
       vars_order = c("black_maj","asian_maj","hispanic_maj", "no_maj"), 
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2),
       model_order = c("Model_3", "Model_4", "Model_5")
       ) %>%
       relabel_predictors(
           c(black_maj = "Majority-Black", 
             asian_maj = "Majority-Asian", 
             hispanic_maj = "Majority-Hispanic", 
             no_maj = "No Majority")
           ) + 
        coord_cartesian(xlim = c(-1, 1)) +
        xlab("Coefficient Estimate") + 
        theme_ipsum() + 
        theme(legend.position = "right")

##Figure 6##
############
dwplot(list(Model_3 = dsols4, Model_4 = dsols5, Model_5 = dsols6), 
       vars_order = c("black_maj","asian_maj","hispanic_maj", "no_maj"), 
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2),
       model_order = c("Model_3", "Model_4", "Model_5")
       ) %>%
       relabel_predictors(
           c(black_maj = "Majority-Black", 
             asian_maj = "Majority-Asian", 
             hispanic_maj = "Majority-Hispanic", 
             no_maj = "No Majority")
           ) + 
        coord_cartesian(xlim = c(-1, 1)) +
        xlab("Coefficient Estimate") + 
        theme_ipsum() + 
        theme(legend.position = "right")

dwplot(list(Model_3 = srols4, Model_4 = srols5, Model_5 = srols6), 
       vars_order = c("black_maj","asian_maj","hispanic_maj", "no_maj"), 
       vline = geom_vline(
           xintercept = 0,
           colour = "grey60",
           linetype = 2),
       model_order = c("Model_3", "Model_4", "Model_5")
       ) %>%
       relabel_predictors(
           c(black_maj = "Majority-Black", 
             asian_maj = "Majority-Asian", 
             hispanic_maj = "Majority-Hispanic", 
             no_maj = "No Majority")
           ) + 
        coord_cartesian(xlim = c(-1, 1)) +
        xlab("Coefficient Estimate") + 
        theme_ipsum() + 
        theme(legend.position = "right")

####################
##APPENDIX FIGURES##
####################


##CORRELATION MATRICES##
########################

##Select all model variables from the dataframe
final_vars <- final_tract %>%
    dplyr::select(c(deepsolar_res_installs, sunroof_res_installs, n_units, black_maj, asian_maj, hispanic_maj, no_maj, daily_solar_radiation, res_electricity_price, tot_state_res_incentive, med_age, pct_educ, med_income, pct_own, pct_sfh, pct_lep))

##Get rid of geometry column
final_vars <- st_set_geometry(final_vars, NULL)

##DeepSolar
dscorr <- cor(final_vars[, -2], use = "complete.obs")
corrplot(dscorr, order = 'FPC', type = 'lower', diag = FALSE, addCoef.col = 'black')

##Sunroof
srcorr <- cor(final_vars[, -1], use = "complete.obs")
corrplot(dscorr, order = 'FPC', type = 'lower', diag = FALSE, addCoef.col = 'black')


##VIF OUTPUTS##
###############

##DeepSolar
allvars_ds <- lm(deepsolar_res_installs ~ n_units + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + med_income + pct_own + pct_sfh + pct_lep, data=final_tract)

vif(allvars_ds)

allvars_sr <- lm(sunroof_res_installs ~ n_units + black_maj + asian_maj + hispanic_maj + no_maj + daily_solar_radiation + res_electricity_price + tot_state_res_incentive + med_age + pct_educ + med_income + pct_own + pct_sfh + pct_lep, data=final_tract)

vif(allvars_sr)

##SUNROOF DESCRIPTIVE STATISTICS AND GRAPHICS##
###############################################

##Figure 1##
############
final_tract %>%
    filter(!(tract_majority %in% NA)) %>%
    ggplot( aes(x=tract_majority, y=log1p(sunroof_res_installs), fill=tract_majority)) +
    geom_violin() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="D") +
    theme_ipsum() +
    theme(
        legend.position="none",
        plot.title = element_text(size=40),
        axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
    ) +
    ggtitle("Project Sunroof Residential Installations by Racial/Ethnic Majority") +
    xlab("Racial/Ethnic Majority Classification") +
    ylab("Ln(Sunroof Residential Installations + 1)")


##Figure 2##
############

final_tract %>%
    filter(
        !(tract_majority %in% NA),
        !(sunroof_logit) %in% NA
        ) %>%
    ggplot(aes(fill=tract_majority, x=factor(sunroof_logit))) + 
    geom_bar(position="fill", stat="count") +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="D") +
    theme_ipsum() +
    theme(
        plot.title = element_text(size=40),
        axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)
        ) +
    scale_x_discrete(labels=c('0', '>0')) +
    ggtitle("Racial/Ethnic Composition of Tracts with 0 or >0 Installation(s), Project Sunroof") +
    xlab("Number of Installations") +
    ylab("Proportion of Total Tracts")+
    labs(fill = "Racial/Ethnic Majority Classification")