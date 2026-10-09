https://github.com/Melissa-mon/st-2026-2-trabajo1-STS9J7.git

# st-2026-2-trabajo1-STS9J7

Trabajo 1 - Tres series: descripción, operador de rezago, dependencia, pronóstico y argumentación


## Qué contiene el repositorio

| Archivo | Contenido | Dependencias |
|---|---|---|
| `R/00-lectura.R` | `leer_serie()`: importa una serie desde un objeto `ts` o un archivo CSV con columnas `fecha` y `valor`, detecta la frecuencia (anual, trimestral o mensual) y devuelve una serie temporal. | Por definir |
| `R/01-graficos.R` | `graficar_serie()`, `grafico_estacional()`, `grafico_rezagos()`, `correlograma()`: funciones de visualización de la serie y su dependencia temporal. | Por definir |
| `R/02-operadores.R` | `diferenciar()`, `polinomio_rezago()`: funciones para calcular diferencias entre observaciones consecutivas y realizar operaciones con polinomios de rezago. | Por definir |
| `R/03-dependencia.R` | `acf_muestral()`, `pacf_dl()`, `ljung_box()`, `varianza_largo_plazo()`: funciones para analizar la autocorrelación, la dependencia temporal y la varianza de largo plazo. | Por definir |
| `R/04-metodos.R` | Un método de pronóstico por función, según la sección 6 del enunciado. | Por definir |
| `R/05-evaluacion.R` | `optimizar()`, `origen_movil()`, `medidas()`: funciones para optimizar los métodos de pronóstico, realizar evaluación con origen móvil y calcular medidas de error. | Por definir |
| `analisis/correr-todo.R` | Carga los scripts de `R/` y ejecuta las tres series en orden. | Scripts de `R/` |
| `informe/informe.qmd` | Documento principal del informe; aplica el protocolo de análisis indicado en el enunciado. | Por definir |
| `informe/informe.html` | Informe renderizado a partir del archivo `.qmd` (también puede entregarse en PDF si corresponde). | `informe.qmd` |
| `informe/nota-interesados.pdf` | Nota correspondiente a la sección 8 del enunciado. | Por definir |
| `exposicion/CODIGO.pdf` | Diapositivas correspondientes a la sección 10 del enunciado. | Por definir |
| `figs/` | Carpeta donde se guardan las figuras generadas al ejecutar `correr-todo.R`. | `analisis/correr-todo.R` |
| `sesion-info.txt` | Archivo con la salida de `sessionInfo()` para registrar la información de la sesión de R. | R |

## Estructura del repositorio

```text
st-2026-2-trabajo1-CODIGO/
|-- README.md
|-- .gitignore
|-- datos/
|   |-- archivo1.csv
|   |-- archivo2.csv
|   |-- archivo3.csv
|-- R/
|   |-- 00-lectura.R
|   |-- 01-graficos.R
|   |-- 02-operadores.R
|   |-- 03-dependencia.R
|   |-- 04-metodos.R
|   |-- 05-evaluacion.R
|-- analisis/
|   |-- correr-todo.R
|-- informe/
|   |-- informe.qmd
|   |-- informe.html
|   |-- nota-interesados.pdf
|-- exposicion/
|   |-- CODIGO.pdf
|-- figs/
|-- sesion-info.txt
```


## Datos

La carpeta `datos/` contiene los tres archivos CSV originales entregados por el profesor:

- `STS9J7-serie-1.csv`
- `STS9J7-serie-2.csv`
- `STS9J7-serie-3.csv`

Los archivos se conservan con sus nombres originales..

## Ejecución

El análisis general se ejecuta mediante `analisis/correr-todo.R`, que carga las funciones de `R/` y procesa las tres series en el orden establecido en el enunciado.

## Informe y exposición

- `informe/informe.qmd`: documento fuente del informe.
- `informe/informe.html`: versión renderizada del informe.
- `informe/nota-interesados.pdf`: nota de la sección 8.
- `exposicion/CODIGO.pdf`: diapositivas de la sección 10.
- `figs/`: figuras generadas durante el análisis.

## Verificación

Los nombres de las carpetas, archivos y funciones corresponden exactamente a los especificados en el enunciado. Esta estructura permite que el script de verificación identifique los componentes del proyecto.