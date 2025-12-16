# ==============================================================================
# PROYECTO: RLG (Regresión Logística)
# AUTOR: Luis Bravo Collado (Braco96)
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, install=FALSE)

cargar_datos <- function(f) {
  rutas <- c(f, file.path("RLG", f), file.path("..", f))
  for(r in rutas) if(file.exists(r)) { load(r, .GlobalEnv); return(TRUE) }
  return(FALSE)
}

print("=== PARTE 1: CASO PRÁCTICO (Beetles) ===")
if(cargar_datos("beetles.Rdata") && exists("beetles")) {
  beetles$piperonyl <- as.factor(beetles$piperonyl)
  m1 <- glm(cbind(muertos, expuestos-muertos) ~ pyrethrin + piperonyl, family=binomial(logit), data=beetles)
  print("Odds Ratios:"); print(exp(coef(m1)))
  
  m2 <- glm(cbind(muertos, expuestos-muertos) ~ pyrethrin * piperonyl, family=binomial, data=beetles)
  print("ANOVA Comparativo:"); print(anova(m1, m2, test="Chisq"))
} else { warning("No se encontró beetles.Rdata") }

print("=== PARTE 2: ANEXO TEÓRICO (Dobson) ===")
df_t <- data.frame(dose=c(1.69, 1.72, 1.75, 1.78, 1.81), killed=c(6,13,18,28,52), n=c(59,60,62,56,63))
mod_t <- glm(cbind(killed, n-killed) ~ dose, family=binomial, data=df_t)
print(summary(mod_t))
