# =========================================================
# 1918 Flu project
# Grouped-manufacturing event study of NPI effect on TFP
# Baseline year = 1914
#
# Final version:
#   1. Collapse detailed industries into 6 broad groups
#   2. Use stable classification:
#        - exact override for common / error-prone industries
#        - fallback regex for the rest
#   3. Aggregate to city × year × industry_group
#   4. Reconstruct grouped TFP
#   5. Run event study by broad group
#   6. No pretrend filter
#   7. No min sample filter
# =========================================================

# -----------------------------
# 0. Packages
# -----------------------------
required_packages <- c(
  "haven", "dplyr", "fixest", "ggplot2",
  "stringr", "broom", "tibble", "readr", "forcats", "purrr"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
library(stringr)
library(broom)
library(tibble)
library(readr)
library(forcats)
library(purrr)

# -----------------------------
# 1. File paths
# -----------------------------
path_city <- "data/city.dta"
path_manu <- "data/CM_tfp_1904_1919_with_cityid.dta"

out_dir <- "results_grouped_industry_tfp_eventstudy"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -----------------------------
# 2. Read data
# -----------------------------
city <- read_dta(path_city)
manu <- read_dta(path_manu)

cat("city dim:", dim(city), "\n")
cat("manu dim:", dim(manu), "\n")

# -----------------------------
# 3. Helper functions
# -----------------------------
require_vars <- function(df, vars, df_name = deparse(substitute(df))) {
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "Missing variables in ", df_name, ": ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
}

resolve_joined_var <- function(df, varname) {
  if (varname %in% names(df)) return(df)

  x_name <- paste0(varname, ".x")
  y_name <- paste0(varname, ".y")

  has_x <- x_name %in% names(df)
  has_y <- y_name %in% names(df)

  if (has_x && has_y) {
    df[[varname]] <- dplyr::coalesce(df[[x_name]], df[[y_name]])
  } else if (has_x) {
    df[[varname]] <- df[[x_name]]
  } else if (has_y) {
    df[[varname]] <- df[[y_name]]
  }

  df
}

extract_event_year <- function(term_vec) {
  as.numeric(str_extract(term_vec, "\\d{4}"))
}

clean_industry_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[–—]", "-") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# -----------------------------
# 4. Stable classification:
#    exact override first
# -----------------------------
industry_group_override <- function(industry_name_clean) {
  dplyr::case_when(

    # -------------------------
    # Food
    # -------------------------
    industry_name_clean %in% c(
      "bread and other bakery products",
      "butter",
      "cheese",
      "condensed milk",
      "confectionery",
      "confectionery and ice cream",
      "chocolate and cocoa products",
      "flour and gristmill products",
      "flour-mill and gristmill products",
      "food preparations",
      "food preparations, n.e.c.",
      "coffee and spice, roasting and grinding",
      "pickles, preserves, and sauces",
      "slaughtering and meat packing",
      "slaughtering and meatpacking",
      "slaughtering and meat packing, wholesale",
      "slaughtering, wholesale, not including meat packing",
      "sausage",
      "sausage, not made in slaughtering and meat-packing establishments",
      "butter, cheese, and condensed milk",
      "condensed milk and milk products, other than butter and cheese",
      "oleomargarine",
      "oleomargarine and other butter substitutes",
      "lard, refined",
      "lard, not made in slaughtering or meatpacking establishments",
      "lard, refined, not made in slaughtering or meat-packing establishments",
      "rice, cleaning and polishing",
      "peanuts, grading, roasting, cleaning, and shelling",
      "canning and preserving",
      "canning and preserving, fish",
      "canning and preserving, fruits and vegetables",
      "canning and preserving, oysters",
      "poultry, killing and dressing",
      "poultry, killing and dressing, not done in slaughtering and meat-packing establishments",
      "vinegar and cider",
      "mineral and soda waters",
      "mineral and soda waters, not including natural spring waters",
      "liquors, distilled",
      "liquors, malt",
      "liquors, vinous",
      "sugar and molasses",
      "sugar and molasses, refining",
      "sugar refining, not including beet sugar",
      "sugar, cane",
      "sugar, refining",
      "sugar, refining, not including beet sugar",
      "beet sugar",
      "baking powders and yeast",
      "malt"
    ) ~ "Food",

    # -------------------------
    # Consumer goods
    # -------------------------
    industry_name_clean %in% c(
      "clothing, men's",
      "clothing, men's, buttonholes",
      "clothing, men's, including shirts",
      "clothing, women's",
      "shirts",
      "corsets",
      "hosiery and knit goods",
      "knit goods",
      "hand knit goods",
      "furnishing goods, men's",
      "millinery and lace goods",
      "millinery and lace goods, n.e.c.",
      "hats and caps, other than felt, straw, and wool",
      "hats, felt",
      "hats, fur-felt",
      "hats, straw",
      "hats, wool",
      "hats, wool-felt",
      "fur goods",
      "furs, dressed",
      "gloves and mittens, cloth, not including gloves made in textile mills",
      "gloves and mittens, leather",
      "boots and shoes",
      "boots and shoes, including cut stock and findings",
      "boots and shoes, not including rubber boots and shoes",
      "boots and shoes, rubber",
      "boots and shoe cut stock",
      "boots and shoe findings",
      "boots and shoe uppers",
      "boot and shoe cut stock, exclusive of that produced in boot and shoe factories",
      "boot and shoe findings, exclusive of that produced in boot and shoe factories",
      "cotton goods",
      "cotton goods, including small cotton wares",
      "cotton lace",
      "cotton small wares",
      "cotton waste",
      "silk and silk goods",
      "silk and silk goods, including throwsters",
      "silk goods",
      "linen goods",
      "woolen goods",
      "woolen and worsted goods",
      "woolen, worsted, and felt goods, and wool hats",
      "wool pulling",
      "wool scouring",
      "wool shoddy",
      "dyeing and finishing textiles",
      "dyeing and finishing textiles, exclusive of that done in textile mills",
      "textile machinery and parts",
      "leather goods",
      "leather goods, n.e.c.",
      "leather, tanned, curried, and finished",
      "pocketbooks",
      "saddlery and harness",
      "clothing, horse",
      "belting and hose, leather",
      "belting, leather",
      "belting and hose, linen",
      "cordage and twine and jute and linen goods",
      "jute and jute goods",
      "jute goods",
      "jewelry",
      "jewelry and instrument cases",
      "sporting goods",
      "sporting and athletic goods",
      "toys and games",
      "trunks and valises",
      "umbrellas and canes",
      "watch and clock materials",
      "watch and clock materials, except watchcases",
      "watch cases",
      "watchcases",
      "watches",
      "clocks",
      "clocks and watches, including cases and materials",
      "musical instruments and materials",
      "musical instruments and materials, n.e.c.",
      "musical instruments and materials, not specified",
      "musical instruments, organs",
      "musical instruments, piano and organ materials",
      "musical instruments, pianos",
      "musical instruments, pianos and organs and materials",
      "optical goods",
      "collars and cuffs, men's",
      "buttons",
      "house furnishing goods, not elsewhere specified",
      "house-furnishing goods, n.e.c.",
      "house-furnishing goods, not elsewhere specified"
    ) ~ "Consumer goods",

    # -------------------------
    # Machinery
    # -------------------------
    industry_name_clean %in% c(
      "foundry and machine-shop products",
      "foundry supplies",
      "machine tools",
      "electrical machinery, apparatus, and supplies",
      "engines, steam, gas, and water",
      "tools, n.e.c.",
      "tools, not elsewhere specified",
      "cutlery and edge tools",
      "cutlery and tools",
      "cutlery and tools, not elsewhere specified",
      "hardware",
      "brass",
      "brassware",
      "brass castings and brass finishings",
      "brass and bronze products",
      "brass, bronze, and copper products",
      "brass and copper, rolled",
      "bronze castings",
      "copper, tin, and sheet-iron products",
      "copper, tin, and sheet-iron work",
      "coppersmithing and sheet iron working",
      "wirework, including wire rope and cable",
      "wirework, n.e.c.",
      "iron and steel, blast furnaces",
      "iron and steel, steel works and rolling mills",
      "iron and steel, forgings",
      "iron and steel, forgings, not made in steel works or rolling mills",
      "iron and steel, bolts, nuts, washers, and rivets, not made in rolling mills",
      "iron and steel, bolts, nuts, washers, and rivets, not made in rolling mills or steel works",
      "iron and steel, bolts, nuts, washers, and rivets, not made in steel works or rolling mills",
      "iron and steel, nails and spikes, cut and wrought, including wire nails, not made in rolling mills or steel works",
      "iron and steel, nails and spikes, cut and wrought, including wire nails, not made in steel works or rolling mills",
      "iron and steel, cast iron pipe",
      "iron and steel, cast-iron pipe",
      "iron and steel, wrought pipe",
      "iron and steel pipe, wrought",
      "iron and steel, tempering and welding",
      "iron and steel, doors and shutters",
      "structural iron work",
      "structural ironwork",
      "structural ironwork, not made in steel works or rolling mills",
      "tinware",
      "tinware, n.e.c.",
      "tin plate and terneplate",
      "tin foil",
      "tinfoil",
      "tin and other foils, n.e.c.",
      "steel barrels, drums, and tanks",
      "gas and electric fixtures",
      "gas and electric fixtures and lamps and reflectors",
      "gas and lamp fixtures",
      "gas machines and gas and water meters",
      "gas machines and meters",
      "gas, illuminating and heating",
      "plumbers' supplies",
      "plumbers' supplies, n.e.c.",
      "steam fittings and heating apparatus",
      "steam fittings and steam and hot-water heating apparatus",
      "stoves, gas and oil",
      "stoves and furnaces, including gas and oil stoves",
      "stoves and furnaces, not including gas and oil stoves",
      "stoves and hot-air furnaces",
      "pumps, not including power pumps",
      "pumps, not including steam pumps",
      "pumps, steam and other power",
      "scales and balances",
      "cash registers and calculating machines",
      "typewriters and supplies",
      "sewing machines and attachments",
      "sewing machines, cases, and attachments",
      "automobiles",
      "automobiles, including bodies and parts",
      "automobile bodies and parts",
      "automobile repairing",
      "bicycles and tricycles",
      "bicycles, motorcycles, and parts",
      "motorcycles, bicycles, and parts",
      "locomotives",
      "locomotives, not made by railroad companies",
      "shipbuilding, including boat building",
      "shipbuilding, iron and steel",
      "shipbuilding, steel",
      "shipbuilding, wooden, including boat building",
      "carriages and wagons",
      "carriages and wagons and materials",
      "carriages and wagons, including repairs",
      "carriages, wagons, and repairs",
      "carriages and sleds, children's",
      "carriage and wagon materials",
      "springs, steel, car and carriage",
      "springs, steel, car and carriage, not made in steel works or rolling mills",
      "horseshoes, not made in steel works or rolling mills",
      "saws",
      "screws, machine",
      "cars and general shop construction and repairs by steam railroad companies",
      "cars and general shop construction and repairs by street railroad companies",
      "cars, electric-railroad, not including operations of railroad companies",
      "cars, steam-railroad, not including operations of railroad companies",
      "electric railroad repair shops",
      "electric-railroad repair shops",
      "steam railroad repair shops",
      "steam-railroad repair shops",
      "wire",
      "wheelbarrows",
      "windmills",
      "lamps and reflectors",
      "lead, bar, pipe, and sheet",
      "files",
      "electroplating",
      "galvanizing",
      "babbit metal and solder",
      "smelting and refining, lead",
      "smelting and refining, metals n.e.c.",
      "smelting and refining, not from the ore",
      "smelting and refining, zinc",
      "soda water apparatus",
      "soda-water apparatus",
      "steam packing",
      "stamped ware",
      "stamped and enameled ware, n.e.c.",
      "sewing machine cases",
      "sewing-machine cases",
      "pens, steel",
      "card cutting and designing",
      "dairymen's, poultrymen's, and apiarists' supplies"
    ) ~ "Machinery",

    # -------------------------
    # Construction
    # -------------------------
    industry_name_clean %in% c(
      "lumber and timber products",
      "lumber, planing-mill products, not including planing mills connected with sawmills",
      "lumber, planing mill products, including sash, doors, and blinds",
      "furniture",
      "furniture and refrigerators",
      "refrigerators",
      "wood, turned and carved",
      "wooden goods, n.e.c.",
      "woodenware, not elsewhere specified",
      "boxes, wooden packing",
      "boxes, wooden packing, except cigar boxes",
      "cooperage",
      "cooperage and wooden goods, not elsewhere specified",
      "brick and tile",
      "brick, tile, and terra-cotta and fire-clay products",
      "pottery",
      "pottery, terra cotta, and fire clay products",
      "pottery, terra-cotta, and fire-clay products",
      "marble and stone work",
      "artificial stone",
      "artificial stone products",
      "monuments and tombstones",
      "glass",
      "glass cutting, staining, and ornamenting",
      "mirrors",
      "mirrors, framed and unframed",
      "looking-glass and picture frames",
      "looking glass and picture frames",
      "window shades and fixtures",
      "show cases",
      "paving materials",
      "roofing materials",
      "wall plaster",
      "gypsum wall plaster",
      "cement",
      "lime",
      "kaolin and ground earths",
      "minerals and earths, ground or otherwise treated",
      "mantels, slate, marble, and marbleized",
      "mattresses and spring beds",
      "mattresses and spring beds, n.e.c.",
      "upholstering materials",
      "upholstering materials, n.e.c.",
      "window and door screens and weather strips",
      "vault lights and ventilators",
      "show cases",
      "coffins, burial cases, and undertakers' goods",
      "baskets and rattan and willow ware",
      "baskets, and rattan and willow ware",
      "cork cutting",
      "cork, cutting",
      "mirrors",
      "mirrors, framed and unframed"
    ) ~ "Construction",

    # -------------------------
    # Chemicals
    # -------------------------
    industry_name_clean %in% c(
      "chemicals",
      "chemicals and acids",
      "sulphuric, nitric, and mixed acids",
      "fertilizers",
      "patent medicines and compounds",
      "patent medicines and compounds and druggists' preparations",
      "druggists preparations",
      "druggists' preparations",
      "drug grinding",
      "soap",
      "perfumery and cosmetics",
      "cleaning and polishing preparations",
      "cleansing and polishing preparations",
      "blacking",
      "blacking and cleansing and polishing preparations",
      "blacking, stains, and dressings",
      "paint and varnish",
      "paints",
      "varnishes",
      "dyestuffs and extracts",
      "dyestuffs and extracts-- natural",
      "flavoring extracts",
      "glue",
      "glue, n.e.c.",
      "mucilage and paste",
      "mucilage, paste, and other adhesives, n.e.c.",
      "grease and tallow",
      "grease and tallow, not incuding lubricating greases",
      "lubricating greases",
      "axle grease",
      "oil, castor",
      "oil, cottonseed and cake",
      "oil, cottonseed, and cake",
      "oil, essential",
      "oil, lard",
      "oil, linseed",
      "oil, n.e.c.",
      "oil, not elsewhere specified",
      "petroleum, refining",
      "photographic materials",
      "photographic apparatus",
      "photographic apparatus and materials",
      "rubber and elastic goods",
      "rubber goods, n.e.c.",
      "rubber goods, not elsewhere specified",
      "rubber tires, tubes, and rubber goods, n.e.c.",
      "belting and hose, rubber",
      "belting and hose, woven and rubber",
      "fire extinguishers, chemical",
      "oilcloth and linoleum",
      "oilcloth and linoleum, floor",
      "oilcloth, enameled",
      "ink, writing",
      "turpentine and rosin",
      "starch",
      "glucose",
      "glucose and starch",
      "gold and silver, leaf and foil",
      "gold and silver, reducing and refining, not from the ore",
      "graphite and graphite refining",
      "graphite, ground and refined"
    ) ~ "Chemicals",

    # -------------------------
    # Printing
    # -------------------------
    industry_name_clean %in% c(
      "printing and publishing",
      "printing and publishing, book and job",
      "printing and publishing, music",
      "printing and publishing, newspaper and periodicals",
      "printing and publishing, newspapers and periodicals",
      "bookbinding and blank book making",
      "bookbinding and blank-book making",
      "engraving, steel",
      "engraving, steel and copperplate, and plate printing",
      "engraving and diesinking",
      "engraving and dyesinking",
      "engravers' materials",
      "lithographing",
      "lithographing and engraving",
      "photolithographing and photoengraving",
      "photo-engraving",
      "photo-engraving, not done in printing establishments",
      "stereotying and electrotyping",
      "type founding",
      "type founding and printing materials",
      "printing materials",
      "paper and wood pulp",
      "paper: paper and wood pulp",
      "pulp goods",
      "pulp, from fiber other than wood",
      "paper goods, n.e.c.",
      "paper goods, not elsewhere specified",
      "paper patterns",
      "envelopes",
      "labels and tags",
      "boxes, fancy and paper",
      "boxes, paper and other, n.e.c.",
      "bags, paper",
      "bags, paper, exclusive of those made in paper mills",
      "cardboard, not made in paper mills",
      "stationery goods, n.e.c.",
      "stationery goods, not elsewhere specified",
      "hand stamps",
      "hand stamps and stencils and brands",
      "stencils and brands",
      "signs and advertising novelties",
      "hand stamps",
      "hand stamps and stencils and brands",
      "stencils and brands",
      "signs and advertising novelties"
    ) ~ "Printing",

    TRUE ~ NA_character_
  )
}

# -----------------------------
# 5. Fallback regex:
#    only classify those not covered above
# -----------------------------
assign_industry_group_fallback <- function(industry_name_clean) {
  dplyr::case_when(

    # Printing
    str_detect(
      industry_name_clean,
      "printing|publishing|bookbinding|engraving|lithograph|photoengraving|photo-engraving|electrotyping|type founding|paper|pulp|envelope|label|stationery|cardboard|bags, paper|boxes, fancy and paper"
    ) ~ "Printing",

    # Food
    str_detect(
      industry_name_clean,
      "bakery|bread|butter|cheese|condensed milk|confectionery|chocolate|cocoa|flour|gristmill|food preparations|slaughtering|meat packing|meatpacking|sausage|coffee|spice|vinegar|cider|canning|liquors|malt|mineral and soda waters|sugar|molasses|oleomargarine|rice, cleaning|peanuts|pickles|preserves|sauces|poultry, killing"
    ) ~ "Food",

    # Chemicals
    str_detect(
      industry_name_clean,
      "chemical|chemicals|acids|druggists|medicine|soap|perfumery|cosmetics|paint|varnish|dyestuffs|extracts|glue|mucilage|adhesives|grease|oil,|petroleum|photographic|rubber|linoleum|fire extinguishers|ink, writing|turpentine and rosin|starch|glucose|graphite"
    ) ~ "Chemicals",

    # Construction
    str_detect(
      industry_name_clean,
      "lumber|timber|planing|furniture|refrigerator|wood|wooden|brick|tile|terra-cotta|terra cotta|fire clay|fire-clay|marble|stone|cement|lime|glass|mirror|looking-glass|picture frames|window shades|window and door screens|weather strips|show cases|paving|roofing|wall plaster|gypsum|pottery|monuments|tombstones|cooperage|boxes, wooden|mattresses|spring beds|upholstering materials|vault lights|ventilators|baskets|rattan|willow ware|cork"
    ) ~ "Construction",

    # Machinery
    str_detect(
      industry_name_clean,
      "foundry|machine-shop|machinery|machine tools|electrical machinery|engines|tools|cutlery|hardware|brass|bronze|copper|sheet-iron|sheet iron|iron and steel|structural iron|wirework|wire\\b|tinware|tin plate|gas fixtures|gas machines|plumbers' supplies|steam fittings|stoves|furnaces|pumps|typewriters|sewing machines|automobile|locomotive|shipbuilding|carriages|wagons|bicycles|motorcycles|horseshoes|screws, machine|saws|scales and balances|repair shops|railroad repair|electroplating|galvanizing|windmills|wheelbarrows|lamps and reflectors|lead, bar, pipe, and sheet|smelting and refining"
    ) ~ "Machinery",

    # "Consumer goods"
    str_detect(
      industry_name_clean,
      "clothing|shirts|corsets|hosiery|knit goods|millinery|lace goods|hats|caps|fur goods|gloves|boots and shoes|boot and shoe|cotton goods|silk|linen|wool|textile|leather goods|pocketbooks|saddlery|belting, leather|belting and hose, leather|jute goods|jewelry|watch|clock|musical instruments|optical goods|sporting goods|toys and games|trunks and valises|umbrellas and canes|collars and cuffs|buttons"
    ) ~ "Consumer goods",

    TRUE ~ "Other"
  )
}

make_horizontal_event_plot <- function(df_plot, npi_label) {

  df_plot <- df_plot %>%
    mutate(
      industry_group = factor(
        industry_group,
        levels = c(
          "Food",
          "Consumer goods",
          "Printing",
          "Chemicals",
          "Construction",
          "Machinery",
          "Other"
        )
      )
    )

  ggplot(
    df_plot,
    aes(
      y = industry_group,
      x = estimate,
      color = factor(year_event)
    )
  ) +
    geom_vline(xintercept = 0, linewidth = 0.8) +
    
    geom_point(
      position = position_dodge(width = 0.6),
      size = 2
    ) +
    
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",  
      position = position_dodge(width = 0.6),
      width = 0.2          
    ) +
    
    labs(
      x = paste0("Coefficient (relative to 1914)"),
      y = "Industry group",
      color = "Year",
      title = NULL
    ) +
    
    theme_minimal(base_size = 13)
}

# -----------------------------
# 6. Check required vars
# -----------------------------
required_city_vars <- c(
  "city_id",
  "high_npi",
  "markel_days_npi",
  "markel_speed_npi"
)
require_vars(city, required_city_vars, "city")

required_manu_vars <- c(
  "city_id",
  "year",
  "industry_name",
  "emp_ops_tot",
  "cap_tot",
  "val_add"
)
require_vars(manu, required_manu_vars, "manu")

# -----------------------------
# 7. Construct city_core
# -----------------------------
city_core <- city %>%
  transmute(
    city_id,
    high_npi,
    markel_days_npi,
    markel_speed_npi
  ) %>%
  distinct() %>%
  mutate(
    high_npi_z = as.numeric(scale(high_npi)),
    markel_days_npi_z = as.numeric(scale(markel_days_npi)),
    markel_speed_npi_z = as.numeric(scale(markel_speed_npi))
  )

# -----------------------------
# 8. Merge NPI into manufacturing data
# -----------------------------
df <- manu %>%
  left_join(city_core, by = "city_id")

for (v in c(
  "high_npi", "markel_days_npi", "markel_speed_npi",
  "high_npi_z", "markel_days_npi_z", "markel_speed_npi_z"
)) {
  df <- resolve_joined_var(df, v)
}

cat("Share missing high_npi after merge:", mean(is.na(df$high_npi)), "\n")

# -----------------------------
# 9. Clean industry names and classify
# -----------------------------
df <- df %>%
  mutate(
    industry_name = as.character(industry_name),
    industry_name_clean = clean_industry_name(industry_name)
  ) %>%
  filter(
    !is.na(industry_name_clean),
    industry_name_clean != "",
    !industry_name_clean %in% c("all industries, total", "all other industries")
  ) %>%
  mutate(
    industry_group = industry_group_override(industry_name_clean),
    industry_group = ifelse(
      is.na(industry_group),
      assign_industry_group_fallback(industry_name_clean),
      industry_group
    )
  )

industry_mapping <- df %>%
  distinct(industry_name, industry_name_clean, industry_group) %>%
  arrange(industry_group, industry_name_clean)

write_csv(
  industry_mapping,
  file.path(out_dir, "industry_name_to_group_mapping_final.csv")
)

cat("\nIndustry group counts (distinct detailed industries):\n")
print(
  industry_mapping %>%
    count(industry_group, sort = TRUE)
)

# -----------------------------
# 10. Aggregate to city × year × industry_group
# -----------------------------
df_grouped <- df %>%
  group_by(city_id, year, industry_group) %>%
  summarise(
    val_add = sum(val_add, na.rm = TRUE),
    cap_tot = sum(cap_tot, na.rm = TRUE),
    emp_ops_tot = sum(emp_ops_tot, na.rm = TRUE),

    high_npi = dplyr::first(high_npi),
    markel_days_npi = dplyr::first(markel_days_npi),
    markel_speed_npi = dplyr::first(markel_speed_npi),
    high_npi_z = dplyr::first(high_npi_z),
    markel_days_npi_z = dplyr::first(markel_days_npi_z),
    markel_speed_npi_z = dplyr::first(markel_speed_npi_z),
    .groups = "drop"
  )

# -----------------------------
# 11. Construct grouped TFP
# -----------------------------
alpha <- 0.33

df_grouped <- df_grouped %>%
  mutate(
    emp_pos = ifelse(emp_ops_tot > 0, emp_ops_tot, NA_real_),
    cap_pos = ifelse(cap_tot > 0, cap_tot, NA_real_),
    val_add_pos = ifelse(val_add > 0, val_add, NA_real_),

    log_emp = log(emp_pos),
    log_cap = log(cap_pos),
    log_val_add = log(val_add_pos),

    log_tfp = log_val_add - alpha * log_cap - (1 - alpha) * log_emp
  )

cat("\nSummary of grouped log_tfp:\n")
print(summary(df_grouped$log_tfp))

# -----------------------------
# 12. Keep relevant years
# -----------------------------
df_grouped <- df_grouped %>%
  filter(year %in% c(1904, 1909, 1914, 1919))

group_counts <- df_grouped %>%
  group_by(industry_group) %>%
  summarise(
    n_obs = n(),
    n_city = n_distinct(city_id),
    n_year = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

write_csv(
  group_counts,
  file.path(out_dir, "group_sample_counts_final.csv")
)

cat("\nSample counts by broad group:\n")
print(group_counts)

# -----------------------------
# 13. Function: run one-group event study
# -----------------------------
run_one_group_eventstudy <- function(df_sub, npi_var) {

  grp_name <- unique(df_sub$industry_group)
  grp_name <- grp_name[!is.na(grp_name)][1]

  model <- tryCatch(
    feols(
      as.formula(
        paste0("log_tfp ~ i(year, ", npi_var, ", ref = 1914) | city_id + year")
      ),
      cluster = ~city_id,
      data = df_sub,
      notes = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(model)) return(NULL)
  if (length(model$collin.var) > 0) return(NULL)

  td <- broom::tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "year::")) %>%
    mutate(
      industry_group = grp_name,
      year_event = extract_event_year(term),
      n_obs = nrow(df_sub),
      n_city = n_distinct(df_sub$city_id),
      n_year = n_distinct(df_sub$year),
      npi_measure = npi_var
    ) %>%
    arrange(year_event)

  td
}

# -----------------------------
# 14. Function: run all groups for one NPI measure
# -----------------------------
run_all_groups_one_npi <- function(df_input, npi_var, out_dir) {

  cat("\n============================\n")
  cat("Running grouped event-study for:", npi_var, "\n")
  cat("============================\n")

  df_reg <- df_input %>%
    filter(
      !is.na(log_tfp),
      !is.na(.data[[npi_var]]),
      !is.na(city_id),
      !is.na(year),
      !is.na(industry_group)
    )

  group_list <- df_reg %>%
    group_by(industry_group) %>%
    group_split()

  results_all <- map_dfr(
    group_list,
    ~ run_one_group_eventstudy(
      df_sub = .x,
      npi_var = npi_var
    )
  )

  write_csv(
    results_all,
    file.path(out_dir, paste0("group_eventstudy_all_years_", npi_var, ".csv"))
  )

  results_1904 <- results_all %>%
    filter(year_event == 1904) %>%
    arrange(estimate)

  results_1909 <- results_all %>%
    filter(year_event == 1909) %>%
    arrange(estimate)

  results_1919 <- results_all %>%
    filter(year_event == 1919) %>%
    arrange(estimate)

  write_csv(
    results_1904,
    file.path(out_dir, paste0("group_eventstudy_1904_", npi_var, ".csv"))
  )
  write_csv(
    results_1909,
    file.path(out_dir, paste0("group_eventstudy_1909_", npi_var, ".csv"))
  )
  write_csv(
    results_1919,
    file.path(out_dir, paste0("group_eventstudy_1919_", npi_var, ".csv"))
  )

  if (nrow(results_all) > 0) {
    p <- make_horizontal_event_plot(
          df_plot = results_all %>% filter(year_event %in% c(1904, 1909, 1919)),
          npi_label = npi_var
        )

    ggsave(
          filename = file.path(out_dir, paste0("group_eventstudy_horizontal_", npi_var, ".png")),
          plot = p,
          width = 10,
          height = 5.5
        )
  }

  return(list(
    all_years = results_all,
    res_1904 = results_1904,
    res_1909 = results_1909,
    res_1919 = results_1919
  ))
}

# -----------------------------
# 15. Run all NPI measures
# -----------------------------
npi_vars <- c("high_npi_z", "markel_days_npi_z", "markel_speed_npi_z")

setFixest_notes(FALSE)

all_results <- map(
  npi_vars,
  ~ run_all_groups_one_npi(
    df_input = df_grouped,
    npi_var = .x,
    out_dir = out_dir
  )
)
names(all_results) <- npi_vars

setFixest_notes(TRUE)

# -----------------------------
# 16. Combine 1919 results across measures
# -----------------------------
all_1919 <- bind_rows(
  lapply(seq_along(all_results), function(i) {
    all_results[[i]]$res_1919 %>%
      mutate(npi_measure = names(all_results)[i])
  })
)

write_csv(
  all_1919,
  file.path(out_dir, "group_eventstudy_1919_all_measures_final.csv")
)

# -----------------------------
# 17. Combine all event-study results across measures
# -----------------------------
all_event_results <- bind_rows(
  lapply(seq_along(all_results), function(i) {
    all_results[[i]]$all_years %>%
      mutate(npi_measure = names(all_results)[i])
  })
)

write_csv(
  all_event_results,
  file.path(out_dir, "group_eventstudy_all_measures_all_years_final.csv")
)

cat("\nDone. All grouped results saved in folder:", out_dir, "\n")