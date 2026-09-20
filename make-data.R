# ---------------------------------------------------------------------------
# make-data.R
# Generates the SYNTHETIC teaching datasets used in the EPI R/SAS seminar.
#
# Nothing here is real. The structure deliberately mirrors a linked perinatal /
# newborn-screening analysis (BORN-style birth records + dried blood spot
# analytes + maternal characteristics) so the exercises feel like real work,
# but every value is simulated. Safe to post publicly and to paste into an
# AI assistant.
#
# Deliberate teaching landmines, all intentional:
#   * screening.csv has REPEAT SPECIMENS -> many-to-one join blows up row count
#   * some infants are TWINS -> two infant_id per momid (many-to-one on momid)
#   * x17ohp is right-skewed -> motivates log transformation
#   * x17ohp and mat_bmi have missing values -> motivates na.rm / drop_na
#   * sex has an inconsistent coding ("M"/"F"/"m"/"f") -> motivates cleaning
#   * 8 infants in births.csv have NO screening record -> motivates anti_join
#
# Run from the project root with:  Rscript make-data.R
# ---------------------------------------------------------------------------

set.seed(20260917)
n_mothers <- 1200

# ---- mothers --------------------------------------------------------------
momid <- sprintf("M-%04d", seq_len(n_mothers))

mat_age <- round(rnorm(n_mothers, 30.5, 5.4))
mat_age <- pmin(pmax(mat_age, 16), 46)

parity  <- rbinom(n_mothers, 4, 0.28)

mat_bmi <- round(rlnorm(n_mothers, log(25.5), 0.20), 1)
mat_bmi[sample(n_mothers, round(0.05 * n_mothers))] <- NA   # missing by design

smoking <- rbinom(n_mothers, 1, 0.09)
diabetes_pre <- rbinom(n_mothers, 1, 0.085)
# hypertension risk rises with age and BMI
hyp_lp <- -3.4 + 0.045 * (mat_age - 30) + 0.055 * (ifelse(is.na(mat_bmi), 25.5, mat_bmi) - 25.5)
hypertension <- rbinom(n_mothers, 1, plogis(hyp_lp))

mothers <- data.frame(
  momid        = momid,
  mat_age      = mat_age,
  parity       = parity,
  mat_bmi      = mat_bmi,
  smoking      = ifelse(smoking == 1, "yes", "no"),
  diabetes_pre = diabetes_pre,
  hypertension = hypertension,
  stringsAsFactors = FALSE
)

# ---- births ---------------------------------------------------------------
# ~3% of mothers deliver twins
twin_mom <- rbinom(n_mothers, 1, 0.03)
birth_momid <- rep(momid, times = 1 + twin_mom)
n_births <- length(birth_momid)

infant_id <- sprintf("B-%04d", seq_len(n_births))
multiple  <- as.integer(duplicated(birth_momid) | duplicated(birth_momid, fromLast = TRUE))

mm <- match(birth_momid, mothers$momid)
sex_num <- rbinom(n_births, 1, 0.512)

# Gestational age: mostly term, with a left tail. Shorter for multiples, and
# genuinely (modestly) shorter with smoking, hypertension and pre-existing
# diabetes -- so the logistic model on the slides has something real to find.
ga_weeks <- 39.7 -
  2.6 * multiple -
  0.62 * (mothers$smoking[mm] == "yes") -
  1.35 * mothers$hypertension[mm] -
  1.00 * mothers$diabetes_pre[mm] -
  0.030 * (mothers$mat_age[mm] - 30) +
  rnorm(n_births, 0, 0.95) - rgamma(n_births, shape = 0.30, rate = 0.62)
ga_weeks <- pmin(pmax(ga_weeks, 24.0), 42.0)
# force a handful of very preterm births so the tail is visible in plots
n_vp <- 22
ga_weeks[sample(n_births, n_vp)] <- runif(n_vp, 25.5, 31.8)
ga_weeks <- round(ga_weeks, 2)

# birth weight driven by GA, sex, multiple, smoking, hypertension
bw <- 3350 +
  190 * (ga_weeks - 39) +
  110 * sex_num -
  420 * multiple -
  180 * (mothers$smoking[mm] == "yes") -
  150 * mothers$hypertension[mm] +
  260 * mothers$diabetes_pre[mm] +
  rnorm(n_births, 0, 330)
birth_weight_g <- round(pmin(pmax(bw, 480), 5400))

# messy sex coding, on purpose
sex_clean <- ifelse(sex_num == 1, "M", "F")
messy <- sample(n_births, 26)
sex <- sex_clean
sex[messy] <- tolower(sex[messy])

delivery_mode <- ifelse(
  rbinom(n_births, 1, plogis(-1.4 + 0.9 * multiple + 0.5 * mothers$hypertension[mm])) == 1,
  "caesarean", "vaginal"
)

site <- sample(c("Ottawa", "Toronto", "Kingston", "Sudbury", "Thunder Bay"),
               n_births, replace = TRUE, prob = c(.36, .28, .16, .12, .08))

birth_dt <- as.POSIXct("2025-01-01 00:00", tz = "UTC") +
  runif(n_births, 0, 365 * 24 * 3600)
# twins born minutes apart
for (i in which(duplicated(birth_momid))) birth_dt[i] <- birth_dt[i - 1] + 60 * sample(4:35, 1)

births <- data.frame(
  infant_id      = infant_id,
  momid          = birth_momid,
  birth_datetime = format(birth_dt, "%Y-%m-%d %H:%M"),
  ga_weeks       = ga_weeks,
  birth_weight_g = birth_weight_g,
  sex            = sex,
  multiple       = multiple,
  delivery_mode  = delivery_mode,
  site           = site,
  stringsAsFactors = FALSE
)

# ---- newborn screening ----------------------------------------------------
# 8 infants never get screened; ~11% get a second (repeat) specimen
screened <- setdiff(seq_len(n_births), sample(n_births, 34))
repeats  <- sample(screened, round(0.11 * length(screened)))

spec_infant <- c(infant_id[screened], infant_id[repeats])
spec_no     <- c(rep(1L, length(screened)), rep(2L, length(repeats)))
idx         <- match(spec_infant, infant_id)

age_hours <- round(ifelse(spec_no == 1, runif(length(spec_no), 24, 72),
                                        runif(length(spec_no), 168, 336)), 1)
coll_dt <- birth_dt[idx] + age_hours * 3600

ga_i  <- ga_weeks[idx]
sex_i <- sex_num[idx]

# TSH: log-normal, higher in preterm
tsh <- round(exp(rnorm(length(idx), log(2.4) + 0.06 * (39 - ga_i), 0.55)), 2)

# 17-OHP: strongly right-skewed and strongly GA-dependent (the classic example)
x17ohp <- round(exp(rnorm(length(idx), log(22) + 0.135 * (39 - ga_i), 0.62)), 1)
x17ohp[sample(length(idx), round(0.06 * length(idx)))] <- NA   # missing by design

phe <- round(rnorm(length(idx), 52, 11), 1)
leu <- round(rnorm(length(idx), 118, 27) + 9 * sex_i, 1)
ala <- round(rnorm(length(idx), 246, 52), 1)

screening <- data.frame(
  infant_id           = spec_infant,
  specimen_no         = spec_no,
  collection_datetime = format(coll_dt, "%Y-%m-%d %H:%M"),
  age_at_collection_h = age_hours,
  tsh                 = tsh,
  x17ohp              = x17ohp,
  phe                 = phe,
  leu                 = leu,
  ala                 = ala,
  stringsAsFactors = FALSE
)
screening <- screening[order(screening$infant_id, screening$specimen_no), ]

# ---- small Session 1 dataset (12 adults, easy to eyeball) ------------------
patients <- data.frame(
  id        = 1:12,
  sex       = c("F","M","F","M","F","F","M","M","F","M","F","M"),
  age       = c(34, 58, 45, 67, 23, 39, 72, 51, 29, 61, 48, 55),
  height_cm = c(165, 178, 170, 173, 160, 168, 175, 182, 158, 169, 171, 177),
  weight_kg = c(64, 92, 70, 80, 55, 68, 85, 95, 52, 78, 74, 88),
  smoke     = c("no","yes","no","yes","no","no","yes","no","no","yes","no","yes"),
  a1c       = c(5.4, 7.2, 5.8, 6.5, 5.1, 5.6, 6.9, 7.8, 5.3, 6.2, 5.9, 7.1),
  stringsAsFactors = FALSE
)

# ---- write ----------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
out <- function(x, f) write.csv(x, f, row.names = FALSE, na = "NA", quote = FALSE)
out(mothers,   "data/mothers.csv")
out(births,    "data/births.csv")
out(screening, "data/screening.csv")
out(patients,  "data/patients.csv")

cat("mothers  :", nrow(mothers),   "rows\n")
cat("births   :", nrow(births),    "rows (", sum(births$multiple), "from multiple births )\n")
cat("screening:", nrow(screening), "rows for", length(unique(screening$infant_id)), "infants\n")
cat("patients :", nrow(patients),  "rows\n")
