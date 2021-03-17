#################### Linear Model ####################
lockdown.spatial <- subset(lockdown, select=c("Longitude", "Latitude", "adj.ATT", "coal_contribution", "agriculture", "dust", "industrial_boilers", "industrial_processes", "residential", "mobile", "relative_mobility_change", "age_pct_65_plus", "white_pct", "population_density", "poverty", "urban_rural_2", "urban_rural_3", "urban_rural_4", "Region_Northeast", "Region_South", "Region_West"))
lockdown.spatial[c(4:15)] <- lapply(lockdown.spatial[c(4:15)], function(x) scale(x))

## Model without interactions
eq_noint <- adj.ATT ~ agriculture + dust + industrial_boilers + industrial_processes + residential + mobile + relative_mobility_change + age_pct_65_plus + white_pct + population_density + poverty + urban_rural_2 + urban_rural_3 + urban_rural_4 + Region_Northeast + Region_South + Region_West - 1

r.lm <- lm(eq_noint, data = lockdown.spatial)
res <- summary(r.lm)
conf <- confint(r.lm)

lgm_confint.noint <- data.frame("Variable" = c("Agriculture Emissions", "Dust Emissions", "Industrial Boiler Emissions", "Industrial Processes Emissions", "Residential Emissions", "Mobile Emissions", "Relative Mobility Decrease", "% Age 65+", "% White", "Population Density", "Poverty Rate", "Large Fringe Metro", "Medium Metro", "Other","Northeast", "South", "West"), "estimate" = res$coefficients[,1], "ci0.025" = conf[,1], "ci0.975" = conf[,2])
lgm_confint.noint$Variable <- factor(lgm_confint.noint$Variable, 
                                     levels = lgm_confint.noint$Variable)
lgm_confint.noint$VarType <- c(rep("Air Pollution",6), "Mobility", 
                               rep("Socioeconomic",4), rep("Urbanization", 3),
                               rep("Region", 3))

ggplot() +
  geom_pointrange(lgm_confint.noint, mapping = aes(x = Variable, y = estimate, 
                                                   ymin = ci0.025, ymax = ci0.975,
                                                   color = VarType)) + coord_flip() +
  ggtitle("Coefficient Estimates") + ylab("Scaled Estimate") + labs(color="Variable Type") +
  scale_x_discrete(limits = rev(levels(lgm_confint.noint$Variable))) + 
  scale_color_manual(values = c("Air Pollution" = "brown",
                                "Mobility" = "orange", "Region" = "purple", 
                                "Socioeconomic" = "forestgreen", 
                                "Urbanization" = "gray40")) + geom_hline(yintercept = 0)


## Model with interactions
eq_reg <- adj.ATT ~ agriculture + dust + industrial_boilers + industrial_processes + residential + mobile + relative_mobility_change*age_pct_65_plus + white_pct + population_density + poverty + urban_rural_2 + urban_rural_3 + urban_rural_4 + Region_Northeast + Region_South + Region_West - 1

r.lm.int <- lm(eq_reg, data = lockdown.spatial)
res.int <- summary(r.lm.int)
conf.int <- confint(r.lm.int)

lgm_confint.int <- data.frame("Variable" = c("Agriculture Emissions", "Dust Emissions", "Industrial Boiler Emissions", "Industrial Processes Emissions", "Residential Emissions", "Mobile Emissions", "Relative Mobility Decrease", "% Age 65+", "% White", "Population Density", "Poverty Rate", "Large Fringe Metro", "Medium Metro", "Other","Northeast", "South", "West", "Rel. Mobility Change*% Age 65+"), "estimate" = res.int$coefficients[,1], "ci0.025" = conf.int[,1], "ci0.975" = conf.int[,2])
lgm_confint.int$Variable <- factor(lgm_confint.int$Variable, 
                                   levels = lgm_confint.int$Variable)
lgm_confint.int$VarType <- c(rep("Air Pollution",6), "Mobility", 
                             rep("Socioeconomic",4), rep("Urbanization", 3),
                             rep("Region", 3), rep("Interactions",1))

ggplot() +
  geom_pointrange(lgm_confint.int, mapping = aes(x = Variable, y = estimate, 
                                                 ymin = ci0.025, ymax = ci0.975,
                                                 color = VarType)) + coord_flip() +
  ggtitle("Coefficient Estimates") + ylab("Scaled Estimate") + labs(color="Variable Type") +
  scale_x_discrete(limits = rev(levels(lgm_confint.int$Variable))) + 
  scale_color_manual(values = c("Air Pollution" = "brown",
                                "Mobility" = "orange", "Region" = "purple", 
                                "Socioeconomic" = "forestgreen", 
                                "Urbanization" = "gray40", "Interactions" = "magenta")) + 
  geom_hline(yintercept = 0)


## Check correlation matrix
op <- par(mfrow=c(2,2)); plot(r.lm); par(op)
corr <- cor(as.data.frame(lockdown[c("agriculture", "dust", "industrial_boilers", "industrial_processes", "residential", "mobile", "relative_mobility_change", "age_pct_65_plus", "white_pct", "population_density", "poverty", "urban_rural_2", "urban_rural_3", "urban_rural_4", "Region_Northeast", "Region_South", "Region_West")]), use = "pairwise.complete.obs")



