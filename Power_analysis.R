team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data,col_name = "Condition", value = "A") # without none condition

# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Alone") # without none condition
# 
# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Together")


# Factor the columns ----#
# Factors columns - general, the columns that have a specific set of values should be facors. Factoring a columns has shown to work better with ANOVA function

columns_to_refactor <- c("SessionOrder", "Team", "Player_ID", "Condition", "Dominate.Strategy", "Condition", "Target")
team_aggregate_data_stats <- re_factor_columns(team_aggregate_data_stats, columns_to_refactor)


# Team ------
fit_rand_teamscore <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Currnt power
powerSim(fit_rand_teamscore, nsim = 200, seed = 300)

# test <- extend(fit_rand_teamscore, along = "Team")
test_100 <- extend(fit_rand_teamscore, along = "Team", n = 100)
powerSim(test_100, nsim = 100, seed = 300)
powerCurve(test_100, seed = 300, along = "Team")


# powerSim(fit_rand_teamscore_pilot)
# powerCurve(model2)
model2 <- extend(fit_rand_teamscore_pilot, along = "Team", n = 33)
powerSim(model2, nsim=50)

# Individual ----
fit_rand_indScore <- lmer(IndividualScore~Target+SessionOrder+(1|Team)+(1|Player_ID), data = team_aggregate_data_stats)

# Currnt power
powerSim(fit_rand_indScore, nsim = 1000, seed = 300)

# test <- extend(fit_rand_indScore, along = "Team")
test_100 <- extend(fit_rand_indScore, along = "Team", n = 100)
powerSim(test_100, nsim = 100, seed = 300)
powerCurve(test_100, seed = 300, along = "Team")


# powerSim(fit_rand_teamscore_pilot)
# powerCurve(model2)
model2 <- extend(fit_rand_indScore, along = "Team", n = 33)
powerSim(model2, nsim=50)


#-----
u_my <- 2 # Number of coeffecicents
variablility_explained <- .10
effect_size_my <- variablility_explained / (1-variablility_explained)

power_test_v <- pwr.f2.test(u = u_my, v = NULL,sig.level = .05, power = .8, f2=.1)
# n <- v + u + 1

power_test_v$v + u_my + 1

# What is the power with n?
n<- 33
u_my <- 2 # Number of coeffecicents
v_my <- n - u_my - 1


pwr.f2.test(u = u_my, v = v_my, sig.level = .05, power = NULL, f2 = .1)


#Power analysis
powerSim(fit_rand_teamscore)
