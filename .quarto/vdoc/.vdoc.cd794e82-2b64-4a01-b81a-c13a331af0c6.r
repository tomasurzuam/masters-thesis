#
#
#
#
#
#| label: packages
#| echo: false

if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc,
               here,
               lavaan,
               psych,
               corrplot,
               ggdist,
               sjlabelled,
               patchwork,
               RColorBrewer,
               poLCA,
               tidyLPA,
               reshape,
               summarytools,
               careless,
               tidySEM,
               semPlot,
               lavaanPlot)

options(scipen=999)
rm(list = ls())
#
#
#
#
#| label: data
#| echo: false

here::i_am("paper/04-resultados.qmd")
load(here::here("paper", "input/data/proc/db_proc.RData"))

#
#
#
#| echo: false

vars_m <- c("perc_effort",
            "perc_talent",
            "perc_rich_parents",
            "perc_contact",
            "pref_effort",
            "pref_talent",
            "pref_rich_parents",
            "pref_contact")
#
#
#
#| label: proc
#| echo: false

db <- db %>% 
  mutate(
    income_4 = case_when(
      income %in% c(
        "Menos de $280.000 mensuales liquidos",
        "De $280.001 a $380.000 mensuales liquidos",
        "De $380.001 a $470.000 mensuales liquidos"
      ) ~ "Low",
      
      income %in% c(
        "De $470.001 a $610.000 mensuales liquidos",
        "De $610.001 a $730.000 mensuales liquidos"
      ) ~ "Middle-low",
      
      income %in% c(
        "De $730.001 a $890.000 mensuales liquidos",
        "De $890.001 a $1.100.000 mensuales liquidos"
      ) ~ "Middle-high",
      
      income %in% c(
        "De $1.100.001 a $2.700.000 mensuales liquidos",
        "De $2.700.001 a $4.100.000 mensuales liquidos",
        "Mas de $4.100.001 mensuales liquidos"
      ) ~ "High",
      
      TRUE ~ NA_character_
    ),
    income_4 = factor(income_4, levels = c("Low", "Middle-low", "Middle-high", "High"))
  )


db$income_4 <- sjlabelled::set_label(db$income_4, "Income")

# income quintiles (approximate, since income is collected in 10 ordered brackets)
income_levels <- c(
  "Menos de $280.000 mensuales liquidos",
  "De $280.001 a $380.000 mensuales liquidos",
  "De $380.001 a $470.000 mensuales liquidos",
  "De $470.001 a $610.000 mensuales liquidos",
  "De $610.001 a $730.000 mensuales liquidos",
  "De $730.001 a $890.000 mensuales liquidos",
  "De $890.001 a $1.100.000 mensuales liquidos",
  "De $1.100.001 a $2.700.000 mensuales liquidos",
  "De $2.700.001 a $4.100.000 mensuales liquidos",
  "Mas de $4.100.001 mensuales liquidos"
)

income_code <- as.integer(factor(db$income, levels = income_levels))
income_breaks <- unique(quantile(income_code, seq(0, 1, .2), type = 1, na.rm = TRUE))

db$income_5 <- cut(income_code, breaks = income_breaks, include.lowest = TRUE,
                   labels = paste0("Q", seq_len(length(income_breaks) - 1))) |>
  sjlabelled::set_label("Income quintile")

db$age <- sjlabelled::set_label(db$age, "Age")

db$sex <- if_else(db$sex == 1, "Male", "Female")

db$sex <- factor(db$sex, levels = c("Male", "Female"))

db$sex <- sjlabelled::set_label(db$sex, "Sex")

# educ is coded 1-9 (1-4 = up to complete secondary or less, 5-6 = higher technical,
# 7-9 = university or more; grouping checked against screen_education in the survey
# repository). Dichotomized: 0 = no university education, 1 = university education.
db$educ <- if_else(db$educ >= 7, 1, 0)

db$educ <- sjlabelled::set_label(db$educ, "University education")


db$pol <- sjlabelled::set_label(db$pol, "Political identification")



#
#
#
# income deciles (approximate, since income is collected in 10 ordered brackets)
decile_breaks <- unique(quantile(income_code, seq(0, 1, .1), type = 1, na.rm = TRUE))

db$income_10 <- cut(income_code, breaks = decile_breaks, include.lowest = TRUE,
                    labels = paste0("D", seq_len(length(decile_breaks) - 1))) |>
  sjlabelled::set_label("Income decile")

# dichotomized: 0 = deciles 1-9, 1 = decile 10
db$income_top10 <- if_else(as.integer(db$income_10) == length(decile_breaks) - 1, 1, 0) |>
  sjlabelled::set_label("Top income decile")

# dichotomized: 0 = brackets 1-8, 1 = brackets 9-10 (more than $2.700.000)
db$income_high <- if_else(income_code >= 9, 1, 0) |>
  sjlabelled::set_label("High income (brackets 9-10)")

table(db$income_10, useNA = "ifany")
table(db$income_top10, useNA = "ifany")
table(db$income_high, useNA = "ifany")
#
#
#
#
#
#
#
#| label: careless-prep
#| include: false

vars_careless <- vars_m

db_careless <- db %>%
  dplyr::select(id, all_of(vars_careless)) %>%
  mutate(across(-id, as.numeric))

x_careless <- db_careless %>% dplyr::select(-id)
#
#
#
#
#| label: fig-longstring
#| include: false

ls_res <- careless::longstring(x_careless, avg = TRUE)

db_careless <- db_careless %>%
  mutate(longstring = ls_res$longstr,
         avgstring  = ls_res$avgstr)

# ggplot(db_careless, aes(x = longstring)) +
#   geom_bar(fill = "#6a1718") +
#   scale_x_continuous(breaks = 1:8) +
#   labs(x = "Longstring index", y = "N respondents",
#        caption = paste0("Source: own elaboration based on Survey EDUMERCO (n = ", nrow(db_careless), ")"))
#
#
#
#
#| label: fig-irv
#| include: false

irv_res <- careless::irv(x_careless, na.rm = TRUE, split = TRUE, num.split = 2)

db_careless <- db_careless %>%
  mutate(irv_total = irv_res$irvTotal,
         irv_perc  = irv_res$irv1,
         irv_pref  = irv_res$irv2)

# db_careless %>%
#   dplyr::select(id, `Overall (8 items)` = irv_total,
#                 `Perceptions block` = irv_perc,
#                 `Preferences block` = irv_pref) %>%
#   pivot_longer(-id, names_to = "block", values_to = "irv") %>%
#   ggplot(aes(x = irv)) +
#   geom_histogram(bins = 20, fill = "#6a1718") +
#   facet_wrap(~block, scales = "free_y") +
#   labs(x = "IRV", y = "N respondents")

#
#
#
#
#| label: fig-mahad
#| include: false

mahad_res <- careless::mahad(x_careless, plot = FALSE, flag = TRUE, confidence = 0.99, na.rm = TRUE)

db_careless <- db_careless %>%
  mutate(mahad_d = mahad_res$d_sq,
         mahad_flag = mahad_res$flagged)

# ggplot(db_careless, aes(x = mahad_d, fill = mahad_flag)) +
#   geom_histogram(bins = 40) +
#   scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#6a1718"), name = "Flagged (99% CI)") +
#   labs(x = expression(paste("Mahalanobis ", D^2)), y = "N respondents")

#
#
#
#
#| label: tbl-careless-flags
#| include: false

careless_flags <- db_careless %>%
  mutate(
    flag_longstring = longstring >= 6,
    flag_irv        = irv_total <= quantile(irv_total, .05, na.rm = TRUE),
    flag_mahad      = mahad_flag,
    n_flags         = rowSums(across(c(flag_longstring, flag_irv, flag_mahad)), na.rm = TRUE),
    careless_2of3   = n_flags >= 2
  )

#
#
#
#| label: careless-merge
#| include: false

db <- db %>%
  left_join(
    careless_flags %>%
      dplyr::select(id, longstring, irv_total, mahad_d,
                    flag_longstring, flag_irv, flag_mahad, n_flags, careless_2of3),
    by = "id"
  )
#
#
#
#
#
#| label: fig-likert-just-educ
#| fig-cap: "Market justice in education (diverging Likert plot)"
#| fig-height: 2.2

item_lab <- "Market justice\nin education"

# proportions by response category
db_likert1 <- db %>%
  filter(!is.na(just_educ)) %>%
  count(cat = just_educ) %>%
  mutate(p = n / sum(n),
         item = item_lab,
         # disagreement goes left (negative), agreement goes right (positive)
         s = if_else(as.integer(cat) <= 2, -p, p))

# stacking order: categories ordered outward from the zero line
db_likert1 <- db_likert1 %>%
  mutate(cat = factor(cat, levels = c("Strongly disagree", "Disagree",
                                      "Agree", "Strongly agree")))

# percentage inside each segment
lab_seg <- db_likert1 %>%
  mutate(side = sign(s)) %>%
  group_by(side) %>%
  arrange(abs(as.integer(cat) - 2.5), .by_group = TRUE) %>%  # category next to zero first
  mutate(x1 = side * (cumsum(p) - p),
         x2 = side * cumsum(p),
         x  = (x1 + x2) / 2,
         lab = scales::percent(p, accuracy = 1)) %>%
  ungroup()

cols <- c("Strongly disagree" = "#6a1718",
          "Disagree"          = "#d08c8c",
          "Agree"             = "#8fb3cf",
          "Strongly agree"    = "#1f4e79")

ggplot(lab_seg, aes(y = item, fill = cat)) +
  geom_tile(aes(x = x, width = p), height = .5) +
  geom_vline(xintercept = 0, linewidth = .35) +
  geom_text(data = lab_seg, aes(x = x, y = item, label = lab),
            inherit.aes = FALSE, size = 3, colour = "white") +
  scale_fill_manual(values = cols, breaks = names(cols)) +
  scale_x_continuous(labels = \(z) scales::percent(abs(z)), limits = c(-1.1, 1.1)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 4, 5, 10))
#
#
#
#
#
#| label: fig-corr
#| fig-cap: "Polychoric correlations between meritocracy indicators and market justice in education (* p < .05, ** p < .01, *** p < .001)"
#| fig-height: 6

db_corr <- db %>%
  dplyr::select(all_of(vars_m), just_educ) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# ordinal items -> polychoric correlations (with p-values, via lavaan)
fit_corr <- lavaan::lavCor(db_corr, ordered = names(db_corr),
                           estimator = "DWLS", se = "standard", output = "fit")
corr_mat <- lavaan::lavInspect(fit_corr, "cor.ov")

pe <- lavaan::parameterEstimates(fit_corr) %>%
  dplyr::filter(op == "~~", lhs != rhs)
p_mat <- matrix(0, nrow(corr_mat), ncol(corr_mat), dimnames = dimnames(corr_mat))
for (k in seq_len(nrow(pe))) {
  p_mat[pe$lhs[k], pe$rhs[k]] <- p_mat[pe$rhs[k], pe$lhs[k]] <- pe$pvalue[k]
}

corr_labels <- c(perc_effort       = "Perc. effort",
                 perc_talent       = "Perc. talent",
                 perc_rich_parents = "Perc. rich parents",
                 perc_contact      = "Perc. contacts",
                 pref_effort       = "Pref. effort",
                 pref_talent       = "Pref. talent",
                 pref_rich_parents = "Pref. rich parents",
                 pref_contact      = "Pref. contacts",
                 just_educ         = "Market justice\neducation")
dimnames(corr_mat) <- list(corr_labels[rownames(corr_mat)],
                           corr_labels[colnames(corr_mat)])

stars <- function(p) {
  dplyr::case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
}

corrplot::corrplot(corr_mat, method = "color", type = "lower",
                   tl.col = "black", tl.cex = .8, tl.srt = 45,
                   col = colorRampPalette(c("#6a1718", "white", "#1f4e79"))(200),
                   cl.cex = .7, diag = FALSE)

# coefficient + significance stars in each lower-triangle cell
n_v <- nrow(corr_mat)
for (i in 2:n_v) {
  for (j in 1:(i - 1)) {
    text(j, n_v - i + 1,
         paste0(formatC(corr_mat[i, j], format = "f", digits = 2), stars(p_mat[i, j])),
         cex = .7, font = 2, xpd = NA)
  }
}
#
#
#
#
#
#
#
db_sem2 <- db %>% 
  dplyr::select(just_educ, all_of(vars_m), age, sex, educ, income_5, pol) |> 
  na.omit()

db_sem2$just_educ <- as.numeric(db_sem2$just_educ)

db_sem2 <- db_sem2 %>% 
  mutate(
    across(
      .cols = all_of(vars_m),
      .fns = ~as.numeric(.)
    ))

# asegurar referencias
db_sem2$income_5 <- relevel(db_sem2$income_5, ref = "Q1")
db_sem2$pol <- relevel(db_sem2$pol, ref = "Left")

# crear dummies
X_income <- model.matrix(~ income_5, data = db_sem2)[, -1, drop = FALSE]
X_pol    <- model.matrix(~ pol, data = db_sem2)[, -1, drop = FALSE]

# unir
db_sem2 <- cbind(db_sem2, as.data.frame(X_income), as.data.frame(X_pol))

# limpiar nombres
names(db_sem2) <- make.names(names(db_sem2))

db_sem2$sex_female <- ifelse(db_sem2$sex == "Female", 1, 0)

model <- c('
  perc_merit =~ perc_effort + perc_talent
  perc_nmerit =~ perc_rich_parents + perc_contact
  pref_merit =~ pref_effort + pref_talent
  pref_nmerit =~ pref_rich_parents + pref_contact

  just_educ ~ perc_merit + perc_nmerit + pref_merit + pref_nmerit +
                 age + educ + sex_female +
                 income_5Q2 + income_5Q3 + income_5Q4 + income_5Q5 +
                 polCenter + polRight + polDoes.not.identify
')

ord_vars <- c(
  "just_educ",
  "perc_effort", "perc_talent",
  "perc_rich_parents", "perc_contact",
  "pref_effort", "pref_talent",
  "pref_rich_parents", "pref_contact"
)

fit_sem2 <- lavaan::sem(
  model,
  data = db_sem2,
  estimator = "WLSMV",
  ordered = ord_vars
)

#summary(fit_sem2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#fitmeasures(fit_sem2, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

#
#
#
#| label: fig-sem2
#| echo: false
#| fig-cap: "SEM: perceptions and preferences of merit and non-merit factors predicting market justice in education (standardized estimates, WLSMV; controls omitted)"
#| fig-cap-location: top
#| fig-width: 11
#| fig-height: 5.5

pe <- lavaan::standardizedSolution(fit_sem2) |>
  dplyr::filter(op %in% c("=~", "~")) |>
  dplyr::mutate(sig = dplyr::case_when(pvalue < .001 ~ "***", pvalue < .01 ~ "**",
                                       pvalue < .05 ~ "*", TRUE ~ ""),
                lab = sprintf("%.2f%s", est.std, sig),
                sig_flag = pvalue < .05)

ind <- c("perc_effort", "perc_talent", "perc_rich_parents", "perc_contact",
         "pref_effort", "pref_talent", "pref_rich_parents", "pref_contact")
ind_lab <- rep(c("Effort", "Talent", "Rich parents", "Contacts"), 2)
lat <- c("perc_merit", "perc_nmerit", "pref_merit", "pref_nmerit")
lat_lab <- c("Perceived\nmerit", "Perceived\nprivilege", "Preferred\nmerit", "Preferred\nprivilege")
ctl <- c("age", "educ", "sex_female", "income_5Q2", "income_5Q3", "income_5Q4",
         "income_5Q5", "polCenter", "polRight", "polDoes.not.identify")
ctl_lab <- c("Age", "Education", "Female", "Income Q2", "Income Q3", "Income Q4",
             "Income Q5", "Pol.: Center", "Pol.: Right", "Pol.: No identif.")

ind_y <- setNames(8:1, ind)
lat_y <- setNames(c(7.5, 5.5, 3.5, 1.5), lat)
X_IND <- 0; X_LAT <- 4.2; X_OUT <- 9; Y_OUT <- 4.5

# loadings (latent -> indicator)
load_df <- pe |>
  dplyr::filter(op == "=~") |>
  dplyr::mutate(xs = X_LAT - .7, ys = lat_y[lhs], xe = X_IND + .75, ye = ind_y[rhs],
                lx = (xs + xe) / 2, ly = (ys + ye) / 2)
# structural paths (latent -> outcome)
str_df <- pe |>
  dplyr::filter(op == "~", rhs %in% lat) |>
  dplyr::mutate(xs = X_LAT + .7, ys = lat_y[rhs], xe = X_OUT - 1.2, ye = Y_OUT,
                lx = xs + (xe - xs) * .5, ly = ys + (ye - ys) * .5)
ggplot2::ggplot() +
  ggplot2::geom_segment(data = load_df,
    ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye),
    colour = "grey45", linewidth = .4,
    arrow = ggplot2::arrow(length = grid::unit(2, "mm"), type = "closed")) +
  ggplot2::geom_label(data = load_df, ggplot2::aes(lx, ly, label = lab),
    size = 3, label.size = 0, fill = "white", label.padding = grid::unit(.5, "mm")) +
  ggplot2::geom_segment(data = str_df,
    ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye,
                 colour = est.std > 0, linetype = sig_flag),
    linewidth = 1,
    arrow = ggplot2::arrow(length = grid::unit(3, "mm"), type = "closed")) +
  ggplot2::geom_label(data = str_df,
    ggplot2::aes(lx, ly, label = lab, colour = est.std > 0),
    size = 3.4, fontface = "bold", label.size = 0, fill = "white",
    label.padding = grid::unit(.6, "mm"), show.legend = FALSE) +
  # nodes
  ggplot2::annotate("tile", x = X_IND, y = unname(ind_y), width = 1.5, height = .7,
                    fill = "white", colour = "grey30") +
  ggplot2::annotate("text", x = X_IND, y = unname(ind_y), label = ind_lab, size = 3) +
  ggplot2::annotate("point", x = X_LAT, y = unname(lat_y), shape = 21, size = 24,
                    fill = "#e8c9c9", colour = "#6a1718") +
  ggplot2::annotate("text", x = X_LAT, y = unname(lat_y), label = lat_lab,
                    size = 3.2, lineheight = .85) +
  ggplot2::annotate("tile", x = X_OUT, y = Y_OUT, width = 2.4, height = 1.1,
                    fill = "white", colour = "black", linewidth = .8) +
  ggplot2::annotate("text", x = X_OUT, y = Y_OUT, label = "Market justice\nin education",
                    size = 3.4, lineheight = .85) +
  ggplot2::annotate("text", x = c(X_IND, X_LAT), y = 9, size = 3.3, fontface = "bold",
                    label = c("Indicators", "Latent factors")) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "#6a1718", `FALSE` = "#2c5f8a"),
                               labels = c(`TRUE` = "Positive", `FALSE` = "Negative"),
                               name = NULL) +
  ggplot2::scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"),
                                 labels = c(`TRUE` = "p < .05", `FALSE` = "n.s."),
                                 name = NULL) +
  ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(colour = "grey30", arrow = NULL)),
                  colour = ggplot2::guide_legend(override.aes = list(arrow = NULL, linetype = "solid"))) +
  ggplot2::coord_fixed(xlim = c(-.9, 10.4), ylim = c(.4, 9.6)) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom")

#
#
#
#
#
#
#
#| include: false
# --- helpers -------------------------------------------------------------

get_K <- function(fit) {
  # robust: number of classes = columns of posterior
  if (!is.null(fit$posterior)) return(as.integer(ncol(fit$posterior)))
  # fallback: rows of probs for first item
  if (!is.null(fit$probs) && length(fit$probs) > 0) return(as.integer(nrow(fit$probs[[1]])))
  stop("Cannot determine K from poLCA object (no posterior/probs).")
}

get_npar <- function(fit) {
  if (!is.null(fit$npar)) return(as.numeric(fit$npar))
  # fallback if npar missing (rare): compute from probs + class shares
  K <- get_K(fit)
  Rj <- vapply(fit$probs, function(m) if (is.matrix(m)) ncol(m) else NA_integer_, integer(1))
  if (anyNA(Rj)) stop("Cannot compute npar: missing category counts in fit$probs.")
  as.numeric(sum(K * (Rj - 1)) + (K - 1))
}

get_n_patterns_obs <- function(fit) {
  # poLCA usually stores predicted cell counts for observed patterns as a vector
  if (!is.null(fit$predcell)) return(as.integer(length(fit$predcell)))
  # fallback: sometimes observed pattern table exists
  if (!is.null(fit$observed) && is.matrix(fit$observed)) return(as.integer(nrow(fit$observed)))
  NA_integer_
}

n_patterns_from_data <- function(data_indicators) {
  # data_indicators = db_lca with only indicators (your a1..a8), already NA-omitted
  as.integer(nrow(dplyr::distinct(data_indicators)))
}


entropy_stats <- function(fit) {
  P <- fit$posterior
  P <- pmax(P, 1e-12)
  N <- nrow(P); K <- ncol(P)
  
  # relative entropy (0..1): 1 = best separation
  rel_entropy <- 1 - (-sum(P * log(P)) / (N * log(K)))
  
  class_hat <- max.col(P)
  max_p <- apply(P, 1, max)
  
  avg_diag <- vapply(1:K, function(k) {
    idx <- which(class_hat == k)
    if (length(idx) == 0) NA_real_ else mean(P[idx, k])
  }, numeric(1))
  
  tibble(
    entropy = rel_entropy,
    mean_max_p = mean(max_p),
    p10_max_p = unname(quantile(max_p, 0.10)),
    avg_diag_min = suppressWarnings(min(avg_diag, na.rm = TRUE)),
    avg_diag_mean = mean(avg_diag, na.rm = TRUE)
  )
}

min_class_share <- function(fit) {
  Ppost <- fit$posterior
  N <- nrow(Ppost); K <- ncol(Ppost)
  class_hat <- max.col(Ppost)
  
  tibble(
    min_class_prior = min(fit$P),
    min_class_map   = min(tabulate(class_hat, nbins = K)) / N
  )
}

#
#
#
#
#| include: false
# 3.3 LCA: dichotomized items ----

# Countermeasure for careless responding: drop respondents flagged by >= 2 of
# the 3 usable careless indices (longstring, IRV, Mahalanobis; see "Patterns
# responses") before building the LCA indicators.
df <- db %>%
  dplyr::filter(!careless_2of3) %>%
  dplyr::select(id, ends_with("_d")) %>%
  na.omit()

a1 <- df$perc_effort_d
a2 <- df$perc_talent_d
a3 <- df$perc_rich_parents_d
a4 <- df$perc_contact_d
a5 <- df$pref_effort_d
a6 <- df$pref_talent_d 
a7 <- df$pref_rich_parents_d
a8 <- df$pref_contact_d
id <- df$id

db_lca <- data.frame(id, a1, a2, a3, a4, a5, a6, a7, a8) %>% as_tibble()

db_lca <- db_lca %>% 
  mutate_at(.vars = 2:9, ~as.factor(.))

f <-cbind(a1, a2, a3, a4, a5, a6, a7, a8)~1

fit_lca <- function(K, data, formula, nrep = 50, maxiter = 3000, seed = 123) {
  set.seed(seed)
  poLCA(formula, data = data, nclass = K,
        nrep = nrep, maxiter = maxiter,
        verbose = FALSE, graphs = FALSE)
}

Ks <- 1:5
#fits <- lapply(Ks, fit_lca, data = db_lca, formula = f, nrep = 30)
#save(fits, file = here("paper", "output/fits_models_lca.RData"))

load(here::here("paper", "output/fits_models_lca.RData"))

fit_tbl <- data.frame(
  K    = Ks,
  logLik = sapply(fits, `[[`, "llik"),
  AIC  = sapply(fits, `[[`, "aic"),
  BIC  = sapply(fits, `[[`, "bic"),
  Gsq  = sapply(fits, `[[`, "Gsq"),
  Chisq = sapply(fits, `[[`, "Chisq")
)

fit_tbl
#
#
#
#
#
#| label: tbl-fit
#| tbl-cap: "Fit comparision of latent class models"
#| tbl-cap-location: top

# ---- build table (no list-cols) -----------------------------------------

N_used <- nrow(db_lca)

m_patterns <- n_patterns_from_data(db_lca)  # scalar

# Robust extractors (avoid list-columns)
get_K <- function(fit) as.integer(ncol(fit$posterior))  # classes = cols of posterior
get_ll  <- function(fit) as.numeric(fit$llik)
get_aic <- function(fit) as.numeric(fit$aic)
get_bic <- function(fit) as.numeric(fit$bic)
get_npar <- function(fit) {
  if (!is.null(fit$npar)) return(as.numeric(fit$npar))
  # fallback: compute from probs
  K <- get_K(fit)
  Rj <- vapply(fit$probs, function(m) ncol(m), integer(1))
  as.numeric(sum(K * (Rj - 1)) + (K - 1))
}

# Table of fit indices
fit_tbl <- tibble(
  K     = map_int(fits, get_K),
  N     = nrow(db_lca),
  logLik= map_dbl(fits, get_ll),
  npar  = map_dbl(fits, get_npar),
  AIC   = map_dbl(fits, get_aic),
  BIC   = map_dbl(fits, get_bic)
) %>%
  arrange(K) %>%
  mutate(
    # Differences relative to preceding (K-1) model
    dAIC = AIC - lag(AIC),
    dBIC = BIC - lag(BIC),
    
    # Likelihood-ratio test (K vs K-1)
    # Test statistic: 2*(LL_K - LL_{K-1}) ~ Chi-square with df = npar_K - npar_{K-1}
    # Note: in LCA, models are not strictly nested in the regular sense;
    # this LRT is widely used as a heuristic, but interpret with caution.
    LRT = 2 * (logLik - lag(logLik)),
    df_LRT = npar - lag(npar),
    p_LRT = ifelse(!is.na(LRT) & df_LRT > 0, pchisq(LRT, df = df_LRT, lower.tail = FALSE), NA_real_)
  )


fit_tbl_report <- fit_tbl %>%
  transmute(
    K, N,
    logLik = round(logLik, 1),
    npar = npar,
    AIC = round(AIC, 1),
    dAIC = round(dAIC, 1),
    BIC = round(BIC, 1),
    dBIC = round(dBIC, 1),
    LRT = round(LRT, 1),
    df_LRT = df_LRT,
    p_LRT = signif(p_LRT, 3)
  )

fit_tbl_report %>% 
 kableExtra::kable(format = "html",
                    align = "c",
                    booktabs = T,
                    escape = F,
                    caption = NULL) %>%
  kableExtra::kable_styling(full_width = T,
                            latex_options = "hold_position",
                            bootstrap_options=c("striped", "bordered", "condensed"),
                            font_size = 23) %>%
  kableExtra::column_spec(c(1,8), width = "3.5cm") %>% 
  kableExtra::column_spec(2:7, width = "4cm") %>% 
  kableExtra::column_spec(4, width = "5cm")

#
#
#
#| label: fig-visaic
#| out-width: '80%'
#| fig-asp: 0.8
#| fig-cap: "Visual: AIC/BIC by K (lower is better)"
#| fig-cap-location: top


# Visual: AIC/BIC by K (lower is better)
fit_long <- fit_tbl %>%
  dplyr::select(K, AIC, BIC) %>%
  pivot_longer(-K, names_to = "criterion", values_to = "value")

ggplot(fit_long, aes(x = K, y = value, group = criterion)) +
  geom_line() +
  geom_point() +
  facet_wrap(~criterion, scales = "free_y") +
  labs(x = "Number of classes (K)", y = "Value") 


#
#
#
#| label: fig-dfvisaic
#| out-width: '80%'
#| fig-asp: 0.8
#| fig-cap: "ΔAIC and ΔBIC (negative = improvement over K-1)"
#| fig-cap-location: top

# Visual: ΔAIC and ΔBIC (negative = improvement over K-1)
delta_long <- fit_tbl %>%
  dplyr::select(K, dAIC, dBIC) %>%
  pivot_longer(-K, names_to = "delta", values_to = "value")

ggplot(delta_long, aes(x = K, y = value, group = delta)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  facet_wrap(~delta, scales = "free_y") +
  labs(x = "K", y = "Change vs K-1 (negative = better)") 
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-prob
#| out-width: '80%'
#| fig-asp: 0.8
#| fig-cap: "P(endorsement | class) for each item"
#| fig-cap-location: top

fit4 <- fits[[4]]

# 2) Extract conditional probabilities per item and class
# fit4$probs is a list: each element is K x Rj matrix.
# For dichotomous items: K x 2 (categories = colnames or "1","2")
probs_long <- bind_rows(lapply(names(fit4$probs), function(item) {
  mat <- fit4$probs[[item]]
  df <- as.data.frame(mat)
  df$class <- 1:nrow(mat)
  df$item <- item
  df
})) %>%
  pivot_longer(cols = -c(class, item), names_to = "category", values_to = "prob") %>%
  mutate(
    class = as.integer(class),
    prob = as.numeric(prob)
  )

# If levels are c("0","1") or c("1","2") you'll want the "higher"/"yes" one.
# Here I will assume endorsement is the LAST level (most common after dichotomization).
endorsement_level <- tail(levels(db_lca$a1), 1)

# 4) Build a clean table: P(endorsement | class) for each item
p_endorse <- probs_long %>%
  filter(category == "Pr(2)") %>%
  dplyr::select(item, class, p = prob) %>%
  mutate(item = factor(item, levels = names(fit4$probs)))

# 5) Plot: profile lines (one line per class)
p_endorse <- p_endorse %>% 
  mutate(
    variable = case_when(item == "a1" ~ "perc_effort",
                         item == "a2" ~ "perc_talent",
                         item == "a3" ~ "perc_rich_parents",
                         item == "a4" ~ "perc_contacts",
                         item == "a5" ~ "pref_effort",
                         item == "a6" ~ "pref_talent",
                         item == "a7" ~ "pref_rich_parents",
                         item == "a8" ~ "pref_contacts"
                         ),
    variable = factor(variable,
                      levels = c("perc_effort",
                                 "perc_talent",
                                 "perc_rich_parents",
                                 "perc_contacts",
                                 "pref_effort",
                                 "pref_talent",
                                 "pref_rich_parents",
                                 "pref_contacts")))

p_endorse <- p_endorse %>% 
  mutate(class = factor(class),
         p = round(p,2))

p_endorse_reordered <- p_endorse %>%
  mutate(class = as.character(class)) %>%
  mutate(class = recode(class,
                        "4" = "1",
                        "1" = "4",
                        "2" = "3",
                        "3" = "2")) %>%
  mutate(class = factor(class, levels = c("1","2","3","4")))

g_class <- ggplot(p_endorse_reordered, aes(x = variable, y = p, group = class, colour = class)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +  
  MetBrewer::scale_color_met_d("VanGogh2") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Item", y = paste0("P(Y = ", "High", " | class)"), group = "Class", colour = "Class") +
  theme(legend.position = "top",
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))


library(plotly)

ggplotly(g_class)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: reg-data
#| include: false

# modal class assignment (same relabelling as in fig-prob: 4->1, 1->4, 2->3, 3->2)
class_lab <- c("Critical meritocrats", "Privilege advocates",
               "Universal rewards", "Against privilege")

class_df <- data.frame(
  id = db_lca$id,
  class4 = factor(recode(as.character(fit4$predclass),
                         "4" = "1", "1" = "4", "2" = "3", "3" = "2"),
                  levels = c("1", "2", "3", "4"), labels = class_lab)
)

db_reg <- db %>%
  dplyr::inner_join(class_df, by = "id") %>%
  dplyr::select(id, just_educ, class4, sex, educ, income_5, pol) %>%
  na.omit() %>%
  dplyr::mutate(just_educ = factor(just_educ, ordered = TRUE))

db_reg$income_5 <- relevel(factor(db_reg$income_5, ordered = FALSE), ref = "Q1")
db_reg$pol      <- relevel(factor(db_reg$pol, ordered = FALSE), ref = "Left")
#
#
#
#| label: tbl-polr
#| tbl-cap: "Ordinal logistic regression of market justice in education (odds ratios)"

fit_polr1 <- MASS::polr(just_educ ~ class4,
                        data = db_reg, Hess = TRUE, method = "logistic")
fit_polr2 <- MASS::polr(just_educ ~ sex + educ + income_5 + pol,
                        data = db_reg, Hess = TRUE, method = "logistic")
fit_polr  <- MASS::polr(just_educ ~ class4 + sex + educ + income_5 + pol,
                        data = db_reg, Hess = TRUE, method = "logistic")
# Model 4: class x university education interaction, plus the remaining controls
fit_polr4 <- MASS::polr(just_educ ~ class4 * educ + sex + income_5 + pol,
                        data = db_reg, Hess = TRUE, method = "logistic")
# Model 5: class x sex interaction, plus the remaining controls
fit_polr5 <- MASS::polr(just_educ ~ class4 * sex + educ + income_5 + pol,
                        data = db_reg, Hess = TRUE, method = "logistic")
# Model 6: class x political identification interaction, plus the remaining controls
fit_polr6 <- MASS::polr(just_educ ~ class4 * pol + sex + educ + income_5,
                        data = db_reg, Hess = TRUE, method = "logistic")

ref_rows <- data.frame(
  term = c("Latent class (Ref = Critical meritocrats)", "Sex (Ref = Male)",
           "University education (Ref = No university)",
           "Income quintile (Ref = Q1)", "Political identification (Ref = Left)",
           "AIC", "BIC"),
  m1 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr1), BIC(fit_polr1)))),
  m2 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr2), BIC(fit_polr2)))),
  m3 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr), BIC(fit_polr)))),
  m4 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr4), BIC(fit_polr4)))),
  m5 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr5), BIC(fit_polr5)))),
  m6 = c(rep("", 5), sprintf("%.1f", c(AIC(fit_polr6), BIC(fit_polr6))))
)
names(ref_rows) <- c("term", paste("Model", 1:6))
hdr_pos <- c(1, 8, 17, 26, 35)   # positions of the reference-category rows
attr(ref_rows, "position") <- c(hdr_pos, 60, 61)

modelsummary::modelsummary(
  list("Model 1" = fit_polr1, "Model 2" = fit_polr2, "Model 3" = fit_polr,
       "Model 4" = fit_polr4,
       "Model 5" = fit_polr5, "Model 6" = fit_polr6),
  exponentiate = TRUE, statistic = "conf.int", stars = TRUE,
  coef_omit = "\\|",
  coef_map = c(
    "class4Privilege advocates" = "Privilege advocates",
    "class4Universal rewards"   = "Universal rewards",
    "class4Against privilege"   = "Against privilege",
    "sexFemale" = "Female",
    "class4Privilege advocates:sexFemale" = "Privilege advocates x Female",
    "class4Universal rewards:sexFemale"   = "Universal rewards x Female",
    "class4Against privilege:sexFemale"   = "Against privilege x Female",
    "educ" = "University education",
    "class4Privilege advocates:educ" = "Privilege advocates x University education",
    "class4Universal rewards:educ"   = "Universal rewards x University education",
    "class4Against privilege:educ"   = "Against privilege x University education",
    "income_5Q2" = "Q2", "income_5Q3" = "Q3", "income_5Q4" = "Q4", "income_5Q5" = "Q5",
    "polCenter" = "Center", "polRight" = "Right",
    "polDoes not identify" = "Does not identify",
    "class4Privilege advocates:polCenter" = "Privilege advocates x Center",
    "class4Universal rewards:polCenter"   = "Universal rewards x Center",
    "class4Against privilege:polCenter"   = "Against privilege x Center",
    "class4Privilege advocates:polRight"  = "Privilege advocates x Right",
    "class4Universal rewards:polRight"    = "Universal rewards x Right",
    "class4Against privilege:polRight"    = "Against privilege x Right",
    "class4Privilege advocates:polDoes not identify" = "Privilege advocates x Does not identify",
    "class4Universal rewards:polDoes not identify"   = "Universal rewards x Does not identify",
    "class4Against privilege:polDoes not identify"   = "Against privilege x Does not identify"
  ),
  gof_map = c("nobs"),
  add_rows = ref_rows,
  output = "tinytable"
) |>
  tinytable::style_tt(i = hdr_pos, bold = TRUE, background = "#f2f2f2")
#
#
#
#
#
#| label: tbl-bch
#| tbl-cap: "Robustness: modal assignment versus three-step BCH correction (odds ratios, Ref = Critical meritocrats)"

# Posterior class probabilities, ordered as the relabelled classes (old 4, 3, 2, 1 -> new 1, 2, 3, 4)
# post <- fit4$posterior[, c(4, 3, 2, 1)]
# rownames(post) <- db_lca$id
# modal_lca <- factor(class_lab[max.col(post)], levels = class_lab)

# # BCH weights: inverse of the classification-error matrix A[s, c] = P(modal = s | class c)
# bch_weights <- function(ids) {
#   p <- post[as.character(ids), , drop = FALSE]
#   m <- factor(class_lab[max.col(p)], levels = class_lab)
#   A <- sapply(1:4, function(cc) tapply(p[, cc], m, sum) / sum(p[, cc]))
#   A[is.na(A)] <- 0
#   list(W = solve(t(A)), modal = as.integer(m))
# }

# # Step 3: stack one copy of the data per class and weight it by the BCH weight
# bch_fit <- function(dat, formula) {
#   bw <- bch_weights(dat$id)
#   stk <- do.call(rbind, lapply(1:4, function(cc) {
#     d <- dat
#     d$cls <- factor(class_lab[cc], levels = class_lab)
#     d$w   <- bw$W[cbind(bw$modal, cc)]
#     d
#   }))
#   suppressWarnings(MASS::polr(formula, data = stk, weights = w,
#                               Hess = FALSE, method = "logistic"))
# }

# f1 <- just_educ ~ cls
# f3 <- just_educ ~ cls + sex + educ + income_5 + pol
# cls_terms <- paste0("cls", class_lab[-1])

# point <- function(dat) {
#   c(exp(coef(bch_fit(dat, f1))[cls_terms]), exp(coef(bch_fit(dat, f3))[cls_terms]))
# }
# est <- point(db_reg)

# # Respondent-level bootstrap of the weights and the outcome model (LCA fixed)
# set.seed(2024)
# B <- 500
# boot <- t(replicate(B, point(db_reg[sample(nrow(db_reg), replace = TRUE), ])))
# ci <- apply(boot, 2, quantile, probs = c(.025, .975))

# fmt <- function(or, lo, hi) sprintf("%.2f [%.2f, %.2f]", or, lo, hi)
# ci_polr <- function(m, nm) {
#   cf <- summary(m)$coefficients[nm, , drop = FALSE]
#   fmt(exp(cf[, 1]), exp(cf[, 1] - 1.96 * cf[, 2]), exp(cf[, 1] + 1.96 * cf[, 2]))
# }
# nm_modal <- paste0("class4", class_lab[-1])

# tab_bch <- data.frame(
#   Class = class_lab[-1],
#   `Model 1 (modal)` = ci_polr(fit_polr1, nm_modal),
#   `Model 1 (BCH)`   = fmt(est[1:3], ci[1, 1:3], ci[2, 1:3]),
#   `Model 3 (modal)` = ci_polr(fit_polr,  nm_modal),
#   `Model 3 (BCH)`   = fmt(est[4:6], ci[1, 4:6], ci[2, 4:6]),
#   check.names = FALSE
# )
# tinytable::tt(tab_bch, notes = "BCH: bootstrap percentile CIs (500 replications); Model 3 includes sex, university education, income quintile and political identification.")
#
#
#
#
#
#
#
#
#| label: lpa-fit
#| include: false

# same sample as the LCA: drop respondents flagged by >= 2 careless indices;
# the eight 4-point items are treated as continuous indicators
db_lpa <- db %>%
  dplyr::filter(!careless_2of3) %>%
  dplyr::select(id, all_of(vars_m)) %>%
  mutate(across(all_of(vars_m), as.numeric)) %>%
  na.omit()

# model 1: equal variances, covariances fixed to zero
set.seed(123)  # fixes the random starts, so profile numbering is reproducible
fits_lpa <- db_lpa %>%
  dplyr::select(-id) %>%
  estimate_profiles(1:5, variances = "equal", covariances = "zero")

fit_lpa <- tidyLPA::get_fit(fits_lpa)
#
#
#
#| label: tbl-lpa-fit
#| tbl-cap: "Latent profile analysis: fit indices by number of profiles"

fit_lpa %>%
  dplyr::select(Classes, LogLik, AIC, BIC, SABIC, Entropy, n_min, BLRT_p) %>%
  mutate(across(c(LogLik, AIC, BIC, SABIC), ~ round(., 0)),
         across(c(Entropy, n_min, BLRT_p), ~ round(., 3))) %>%
  knitr::kable(col.names = c("Profiles", "LogLik", "AIC", "BIC", "SABIC",
                             "Entropy", "Min. class prop.", "BLRT p"))
#
#
#
#| label: fig-lpa-profiles
#| fig-cap: "Estimated means of the meritocracy items by latent profile (three-profile solution)"
#| fig-height: 4.5

# three-profile solution (selected from the fit indices in tbl-lpa-fit)
tidyLPA::get_estimates(fits_lpa[["model_1_class_3"]]) %>%
  dplyr::filter(Category == "Means") %>%
  mutate(Parameter = factor(Parameter, levels = vars_m),
         Class = factor(Class)) %>%
  ggplot(aes(x = Parameter, y = Estimate, group = Class, colour = Class)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  MetBrewer::scale_color_met_d("VanGogh2") +
  coord_cartesian(ylim = c(1, 4)) +
  labs(x = "Item", y = "Estimated mean (1-4)", colour = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
#
#
#
#
#
#| label: lpa-profiles
#| include: false

# name the profiles from their estimated means (profile numbers are arbitrary):
# the profile that most rejects privilege (lowest pref_rich_parents) = Critical
# meritocrats; the one that most endorses it (highest) = Privilege advocates;
# the remaining one = Effort prioritizers
means_lpa <- tidyLPA::get_estimates(fits_lpa[["model_1_class_3"]]) %>%
  dplyr::filter(Category == "Means", Parameter == "pref_rich_parents") %>%
  dplyr::arrange(Estimate)

lpa_names <- c("Critical meritocrats", "Effort prioritizers", "Privilege advocates")
names(lpa_names) <- means_lpa$Class
lpa_levels <- c("Effort prioritizers", "Critical meritocrats", "Privilege advocates")

lpa_df <- data.frame(
  id = db_lpa$id,
  profile = factor(lpa_names[as.character(tidyLPA::get_data(fits_lpa[["model_1_class_3"]])$Class)],
                   levels = lpa_levels)
)

# posterior probabilities of the three-profile solution (same row order as db_lpa)
post_lpa <- tidyLPA::get_data(fits_lpa[["model_1_class_3"]])
post_cols <- paste0("CPROB", names(lpa_names))

size_lpa <- data.frame(
  profile = lpa_df$profile,
  # posterior probability of the profile each respondent was assigned to
  post = as.matrix(post_lpa[, post_cols])[cbind(seq_len(nrow(post_lpa)),
                                                 match(as.character(post_lpa$Class), names(lpa_names)))]
) %>%
  dplyr::group_by(profile) %>%
  dplyr::summarise(n = dplyr::n(), mean_post = mean(post), .groups = "drop") %>%
  dplyr::mutate(pct = n / sum(n) * 100)
#
#
#
#| label: tbl-lpa-size
#| tbl-cap: "Size of the latent profiles (modal assignment)"

size_lpa %>%
  dplyr::transmute(Profile = profile, N = n,
                   `%` = sprintf("%.1f", pct),
                   `Mean posterior probability` = sprintf("%.2f", mean_post)) %>%
  knitr::kable(align = "lrrr")
#
#
#
#| label: fig-lpa-size
#| fig-cap: "Size of the latent profiles (share of respondents)"
#| fig-height: 3

ggplot(size_lpa, aes(x = profile, y = pct, fill = profile)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3.5) +
  MetBrewer::scale_fill_met_d("VanGogh2") +
  scale_y_continuous(limits = c(0, max(size_lpa$pct) * 1.15)) +
  labs(x = NULL, y = "Share of respondents (%)") +
  theme_minimal()
#
#
#
#
#
#| label: lpa-biv-data
#| include: false

# same sample as the regression models: complete cases on all of their variables
db_biv <- db %>%
  dplyr::inner_join(lpa_df, by = "id") %>%
  tidyr::drop_na(just_educ, profile, sex, educ, income_5, pol) %>%
  dplyr::mutate(
    educ = factor(educ, levels = c(0, 1), labels = c("No university", "University")),
    pol  = factor(pol, levels = c("Left", "Center", "Right", "Does not identify"))
  )

# stacked bars: profile composition within each category of a variable
plot_biv <- function(data, var, xlab) {
  data %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    dplyr::count(cat = .data[[var]], profile) %>%
    dplyr::group_by(cat) %>%
    dplyr::mutate(p = n / sum(n)) %>%
    dplyr::ungroup() %>%
    ggplot(aes(x = cat, y = p, fill = profile)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = scales::percent(p, accuracy = 1)),
              position = position_stack(vjust = 0.5), size = 3, colour = "white") +
    scale_y_continuous(labels = scales::percent) +
    MetBrewer::scale_fill_met_d("VanGogh2") +
    labs(x = xlab, y = "Share of respondents", fill = "Profile") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
#
#
#
#| label: fig-lpa-biv
#| fig-cap: "Latent profile composition by educational level, income quintile and political identification (regression sample)"
#| fig-height: 9

g_educ   <- plot_biv(db_biv, "educ", "Educational level")
g_income <- plot_biv(db_biv, "income_5", "Income quintile")
g_pol    <- plot_biv(db_biv, "pol", "Political identification")

(g_educ / g_income / g_pol) + patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
#
#
#

#
#
#
#
#
#
#| label: lpa-reg-data
#| include: false

db_reg_lpa <- db %>%
  dplyr::inner_join(lpa_df, by = "id") %>%
  dplyr::select(id, just_educ, profile, sex, educ, income_5, pol) %>%
  na.omit() %>%
  dplyr::mutate(just_educ = factor(just_educ, ordered = TRUE))

db_reg_lpa$income_5 <- relevel(factor(db_reg_lpa$income_5, ordered = FALSE), ref = "Q1")
db_reg_lpa$pol      <- relevel(factor(db_reg_lpa$pol, ordered = FALSE), ref = "Does not identify")

# one dummy per political category, so that each interaction is estimated in its
# own model (the effects of the interactions do not overlap)
db_reg_lpa <- db_reg_lpa %>%
  dplyr::mutate(polLeft   = as.numeric(pol == "Left"),
                polCenter = as.numeric(pol == "Center"),
                polRight  = as.numeric(pol == "Right"))
#
#
#
#| label: tbl-polr-lpa
#| tbl-cap: "Ordinal logistic regression of market justice in education on latent profiles (odds ratios)"

fit_lpa1 <- MASS::polr(just_educ ~ profile,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")
fit_lpa2 <- MASS::polr(just_educ ~ profile + sex + educ + income_5 + pol,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")

# Models 3-5: profile x one political category each; the other two categories
# enter as main effects only (reference: Does not identify)
fit_lpa3 <- MASS::polr(just_educ ~ profile * polLeft + polCenter + polRight + sex + educ + income_5,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")
fit_lpa4 <- MASS::polr(just_educ ~ profile * polCenter + polLeft + polRight + sex + educ + income_5,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")
fit_lpa5 <- MASS::polr(just_educ ~ profile * polRight + polLeft + polCenter + sex + educ + income_5,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")

# Model 6: profile x university education
fit_lpa6 <- MASS::polr(just_educ ~ profile * educ + sex + income_5 + pol,
                       data = db_reg_lpa, Hess = TRUE, method = "logistic")

ref_rows_lpa <- data.frame(
  term = "Latent profile (Ref = Effort prioritizers)",
  m1 = "", m2 = "", m3 = "", m4 = "", m5 = "", m6 = ""
)
names(ref_rows_lpa) <- c("term", paste("Model", 1:6))
attr(ref_rows_lpa, "position") <- 1

modelsummary::modelsummary(
  list("Model 1" = fit_lpa1, "Model 2" = fit_lpa2, "Model 3" = fit_lpa3,
       "Model 4" = fit_lpa4, "Model 5" = fit_lpa5, "Model 6" = fit_lpa6),
  exponentiate = TRUE, statistic = "conf.int", stars = TRUE,
  coef_omit = "\\|",
  coef_map = c(
    "profileCritical meritocrats" = "Critical meritocrats",
    "profilePrivilege advocates" = "Privilege advocates",
    "sexFemale" = "Female",
    "educ" = "University education",
    "income_5Q2" = "Q2", "income_5Q3" = "Q3", "income_5Q4" = "Q4", "income_5Q5" = "Q5",
    "polLeft" = "Left", "polCenter" = "Center", "polRight" = "Right",
    "profileCritical meritocrats:polLeft"   = "Critical meritocrats x Left",
    "profilePrivilege advocates:polLeft"    = "Privilege advocates x Left",
    "profileCritical meritocrats:polCenter" = "Critical meritocrats x Center",
    "profilePrivilege advocates:polCenter"  = "Privilege advocates x Center",
    "profileCritical meritocrats:polRight"  = "Critical meritocrats x Right",
    "profilePrivilege advocates:polRight"   = "Privilege advocates x Right",
    "profileCritical meritocrats:educ" = "Critical meritocrats x University education",
    "profilePrivilege advocates:educ"  = "Privilege advocates x University education"
  ),
  gof_map = c("nobs", "aic", "bic"),
  add_rows = ref_rows_lpa,
  output = "tinytable"
) |>
  tinytable::style_tt(i = 1, bold = TRUE, background = "#f2f2f2")
#
#
#
#
