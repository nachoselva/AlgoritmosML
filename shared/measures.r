export("accuracy")

accuracy <- function(model, pred, obs, na.rm = FALSE,
                     tol = sqrt(.Machine$double.eps)) {
  err <- obs - pred

  if (na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }

  rmse <- sqrt(mean(err^2))
  mae <- mean(abs(err))
  rmse_porc <- rmse / mean(obs)
  mae_porc <- mae / mean(obs)
  r.squared <- 1 - sum(err^2) / sum((obs - mean(obs))^2)

  resultados <- list(
    "model" = model,
    "rmse" = rmse,
    "mae" = mae,
    "mpe" = rmse_porc,
    "mape" = mae_porc,
    "r.squared" = r.squared
  )

  return(as.data.frame(t(resultados)))
}