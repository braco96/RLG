# Proyecto: RLG

## 👤 Autoría
* **Refactorización:** Luis Bravo Collado
* **Base Teórica:** Inma Garcés Andrés (Estadística II)

## 📚 Fundamentación Teórica: Regresión Logística
Modelo para variables respuesta binarias (Éxito/Fracaso).

### 1. Función Logit
Modelamos el logaritmo de los odds:
$$ \ln\left(\frac{\pi}{1-\pi}\right) = \beta_0 + \beta_1 X $$

### 2. Estimación
Se utiliza **Máxima Verosimilitud** (MLE) mediante algoritmos iterativos (Newton-Raphson), ya que no existe solución cerrada como en MCO.

### 3. Interpretación
Los coeficientes se interpretan mediante Odds Ratios ($OR = e^\beta$).

## 💻 Sobre el Código
Uso de `glm(family = binomial)` para ajustar el modelo y evaluar la deviance.
