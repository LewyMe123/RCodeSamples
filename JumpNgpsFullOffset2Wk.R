library(tidyverse)
library(tidymodels)
library(dplyr)
library(janitor)
library(patchwork)
library(rpart)
library(parsnip)
library(rpart.plot)
library(tidytext)

setwd("C:/Users/Nick.Lewis/Documents/My Tableau Prep Repository/Datasources")

theme_set(theme_light())

jumpNgps <- read.csv("JumpWgpsOffset2Wk.csv")
jumpNgps <- clean_names(jumpNgps)
jumpNgps <- select(jumpNgps, c(-jump_height_imp_mom_cm_cuurent, -jump_height_flight_time_cm_current,
                               -body_weight_kg, -decelerations, -accelerations, -x4wk_avg_max)) %>%
        group_by(full_name, year_1) %>%
        mutate(ID = as.factor(cur_group_id()))%>%
        ungroup()
jumpNgps <- jumpNgps %>%
         select(full_name, week_2_base, everything())
jumpNgps[,c(2:133)] <- lapply(jumpNgps[,c(2:133)], as.numeric)

###### Optional Individual normalization------------------------------------------------------------------
jumpNgps <- jumpNgps %>%
        group_by(ID) %>%
        mutate_at(vars(6:133), scale) %>%
        ungroup()

for (i in colnames(jumpNgps[6:133])) {
        jumpNgps[[i]] <- c(jumpNgps[[i]]) 
}

#jumpNgps <- select(jumpNgps, -full_name)
jumpNgps <- as_tibble(jumpNgps)
jumpNgps <- as.data.frame.data.frame(jumpNgps)

#write.table(jumpNgps, file = "C:/Users/Nick.Lewis/OneDrive - Inter Miami CF/R Data/gpsnormalizedTrial2.csv",
#            row.names = FALSE, col.names = TRUE, sep = ",")

#######-------------------------------------------------------------------------------------------------
jumpNgps <- jumpNgps%>%
        select(-full_name, -week_2_base, -year_1, -ID)

set.seed(487)
jump_split <- initial_split(jumpNgps, prop = 3/4, strata = total_distance)

jump_train <- training(jump_split)
jump_test <- testing(jump_split)

jump_recipe <- recipe(
        ft_c_tdiff_fatigue_current ~ .,
        data = jump_train
)#%>%
#        step_normalize(braking_phase_duration_s:total_metabolic_power)


jump_recipe%>%
        prep()%>%
        bake(new_data = NULL)

tree_model <- decision_tree(
        cost_complexity = tune(),
        tree_depth = tune(),
        min_n = tune())%>%
        set_engine("rpart")%>%
        set_mode("regression")

tree_params <- grid_regular(
        cost_complexity(),
        tree_depth(),
        min_n(),
        levels = 5)

set.seed(136)
cv_folds <- vfold_cv(jump_train, v = 10)

doParallel::registerDoParallel(cores = 3)
set.seed(5867)
model_fit <- tune_grid(
        tree_model,
        jump_recipe,
        resamples = cv_folds,
        grid = tree_params
)

collect_metrics(model_fit)
autoplot(model_fit)
select_best(model_fit, "rmse")

tree_final <- finalize_model(tree_model, select_best(model_fit, "rmse"))

fit_train <- fit(tree_final, ft_c_tdiff_fatigue_current ~ ., jump_train)

library(rpart.plot)

fit_train %>%
        extract_fit_engine() %>%
        rpart.plot()

library(vip)

fit_train %>%
        vip(geom = "col",
            num_features = 40L,
            aesthetics = list(
                    color = "black",
                    fill = "palegreen",
                    alpha = 0.5))

fit_test <- last_fit(tree_final, ft_c_tdiff_fatigue_current ~ .,jump_split)

collect_metrics(fit_test)

fit_test %>%
        collect_predictions() %>%
        ggplot(aes(x = .pred, y = ft_c_tdiff_fatigue_current)) +
        geom_abline(intercept = 0,
                    slope = 1,
                    lty = 2,
                    size = 1.2,
                    color = "red") +
        geom_point(size = 3)

viptable <- fit_train%>%
        vi()

vi_model(fit_train)
