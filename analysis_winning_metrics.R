



model_data <- team_games %>% 
  left_join(team_play_metrics,
            by = c("game_id", "team" = "posteam"))

##test stat. sig and effect size
effect_sizes <- map_dfr(
  analysis_metrics,
  ~{
    result <- cohens_d(
      as.formula(paste(.x, "~win")),
      data = model_data)
    result %>% 
      mutate(metric = .x)
  }) %>% 
  mutate(Cohens_d = -Cohens_d) %>% 
  mutate("Metric" = names_metrics[metric])

#logistical regression
model_formula <- as.formula(
  paste("win ~", paste(
    paste0("scale(",model_metrics, ")"),
    collapse = " + "
  )))
win_model <-
  glm(model_formula,
      data = model_data,
      family = binomial()
  )


##check variables and extract model outputs
odds <- exp(coef(win_model))
ci <- exp(confint(win_model))
vifs <- vif(win_model)


win_prob <- predict(win_model, type = "response")
roc_curve <- roc(response = model_data$win, predictor = win_prob)
auc(roc_curve)

model_results <- tibble(
  metric = names_metrics[model_metrics],
  odds_ratio = as.numeric(odds[-1]),
  lower_ci = ci[-1,1],
  upper_ci = ci[-1,2],
  vif = as.numeric(vifs),
  auc = auc(roc_curve)
)


##download csv for SQL
write.csv(model_data, "data/model_data.csv", row.names = FALSE)
write.csv(team_games, "data/team_games.csv", row.names = FALSE)
write.csv(games, "data/games.csv", row.names = FALSE)
write.csv(effect_sizes, "data/effect_sizes.csv", row.names = FALSE)
write.csv(model_results, "data/model_results.csv", row.names = FALSE)



