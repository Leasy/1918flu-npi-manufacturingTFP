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
│   ├── city-manufacturing.dta
│   └── CM_tfp_1904_1919_with_cityid.dta
│
├── main.R
├── industry_TFP.R
│
└── README.md
```

- data/ contains the datasets used in the analysis.
- main.R is the main replication script for the baseline city-level analysis.
- industry.R implements the grouped-industry event-study analysis of NPI effects on manufacturing TFP.
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

### 3. CM_tfp_1904_1919_with_cityid.dta
City-year-industry manufacturing data used for the grouped-industry TFP event-study analysis.

Key variables used in the script include:
- city_id
- year
- industry_name
- emp_ops_tot
- cap_tot
- val_add

---

## Replication Instructions

To reproduce all results:

1. Clone or download this repository.

2. Open R or RStudio.

3. Set the working directory to the repository folder.

4. Run the main script:

source("main.R")

To reproduce the grouped-industry TFP event-study analysis:

source("industry_TFP.R")

The script will:

- load the datasets from data/
- construct outcome variables
- estimate all regressions
- generate tables and figures used in the paper as well as extra results not used in the paper. 

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

## Baseline Empirical Specification

The baseline empirical model is:

Y_it = β (NPI_i × Post_t)
     + X_i × Post_t
     + α_i
     + γ_t
     + ε_it

Where:

- Y_it     = outcome (log employment or log TFP)
- NPI_i    = NPI intensity measure
- Post_t   = indicator for years after the pandemic (year ≥ 1919)
- X_i      = pre-pandemic city characteristics
- α_i      = city fixed effects
- γ_t      = year fixed effects

Standard errors are clustered at the city level.

---

## Grouped-Industry TFP Event-Study Analysis

In addition to the baseline city-level analysis, this repository includes a grouped-industry event-study script that examines how NPI exposure is associated with manufacturing TFP across broad industry groups.

### Main steps

The grouped-industry script:

- cleans detailed industry names  
- classifies industries into broad groups using a stable mapping approach  
- aggregates the data to the `city × year × industry_group` level  
- reconstructs grouped manufacturing TFP  
- runs event-study regressions separately for each industry group  
- saves regression results and figures automatically  

---

### Industry groups

Detailed industries are collapsed into the following broad groups:

- Food  
- Consumer goods  
- Printing  
- Chemicals  
- Construction  
- Machinery  
- Other  

---

### Event-study specification

For each industry group and each NPI measure, the script estimates an event-study specification of the form:

log_tfp_ctj = Σ_{τ ≠ 1914} β_τj (1{year = τ} × NPI_c) + α_cj + γ_tj + ε_ctj

- `c` indexes cities  
- `t` indexes years  
- `j` indexes industry groups  
- `1914` is the omitted reference year  
- city and year fixed effects are included  
- standard errors are clustered at the city level  

The grouped-industry analysis keeps the years:

- 1904  
- 1909  
- 1914  
- 1919  

---

### Output files

The grouped-industry script saves output to:

results_grouped_industry_tfp_eventstudy/

This includes:

- industry-group mapping files  
- sample count summaries  
- event-study coefficient tables by NPI measure and year  
- combined result files  
- horizontal coefficient plots in `.png` format  


## Author

Yi Liu, 
Department of Economics, 
Yale University

---

## License

This repository is intended for academic replication and research purposes.
