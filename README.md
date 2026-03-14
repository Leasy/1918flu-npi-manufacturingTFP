# NPIs and Manufacturing during the 1918 Influenza Pandemic

This repository contains the replication code and data for the flash paper:

Yi Liu (2026)
*Protecting Jobs without Harming Productivity: Evidence from Non-Pharmaceutical Interventions during the 1918 Flu.*

The project examines how non-pharmaceutical interventions (NPIs) implemented during the 1918 influenza pandemic affected manufacturing employment and productivity in U.S. cities.

---

## Repository Structure

```
.
├── data/
│   ├── city.dta
│   └── city-manufacturing.dta
│
├── main.R
│
└── README.md
```

- data/ contains the datasets used in the analysis.
- main.R is the main replication script that reproduces all empirical results.
- README.md describes the repository and replication instructions.

---

## Data

The analysis uses two datasets.

### 1. city.dta

City-level characteristics and non-pharmaceutical intervention (NPI) measures constructed from historical sources.

Key variables include:

- high_npi
- markel_days_npi
- markel_speed_npi
- log_pop1910
- density1910
- ip_mortality_rate1917

### 2. city-manufacturing.dta

City-level manufacturing outcomes from the U.S. Census of Manufactures.

Key variables include:

- manuf_emp
- manuf_capital
- manuf_valadd
- manuf_wages

---

## Replication Instructions

To reproduce all results:

1. Clone or download this repository.

2. Open R or RStudio.

3. Set the working directory to the repository folder.

4. Run the main script:

source("main.R")

The script will:

- load the datasets from data/
- construct outcome variables
- estimate all regressions
- generate tables and figures used in the paper

All results will be saved automatically in the output directory created by the script.

---

## Construction of Key Variables

### Manufacturing Employment

log_emp = log(manuf_emp)

### Manufacturing TFP Proxy

The paper constructs a productivity proxy assuming a Cobb–Douglas production function.

Production function:

Y = A K^α L^(1−α)

Taking logs:

log(A) = log(Y) − α log(K) − (1−α) log(L)

In the data:

log_tfp_proxy =
    log(manuf_valadd)
    − 0.33 * log(manuf_capital)
    − 0.67 * log(manuf_emp)

This measure is interpreted as a proxy for manufacturing productivity.

---

## Policy Variables (NPI Measures)

Three measures of non-pharmaceutical interventions are used.

High NPI

high_npi = indicator for cities with relatively strong NPI policies

NPI days

markel_days_npi = total duration of major NPI policies

NPI speed

markel_speed_npi = timing of policy adoption relative to pandemic onset

These variables capture different dimensions of pandemic policy:

- policy intensity
- policy duration
- policy timing

## Standardization of NPI Variables

All NPI variables are standardized before entering the regressions.

Specifically, the following variables are transformed into z-scores:

high_npi_z
markel_days_npi_z
markel_speed_npi_z

The transformation is:

NPI_z = (NPI − mean(NPI)) / sd(NPI)

Therefore, regression coefficients should be interpreted as the effect
of a one standard deviation increase in the corresponding NPI measure.

---

## Empirical Specification

The baseline empirical model is:

Y_it = β (NPI_i × Post_t)
     + X_i × Post_t
     + α_i
     + γ_t
     + ε_it

Where:

Y_it     = outcome (log employment or log TFP)
NPI_i    = NPI intensity measure
Post_t   = indicator for years after the pandemic (year ≥ 1919)
X_i      = pre-pandemic city characteristics
α_i      = city fixed effects
γ_t      = year fixed effects

Standard errors are clustered at the city level.

---

## Author

Yi Liu
Department of Economics
Yale University

---

## License

This repository is intended for academic replication and research purposes.