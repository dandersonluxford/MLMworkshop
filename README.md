Multilevel Modelling (MLM) Workshop — R

Hands-on, data-first intro to multilevel models for repeated-measures / EMA data.
Zero faff: download, open, run.

What you’ll do

Load tidy daily data and a few baseline measures

Make a couple of quick plots

Fit random-intercept and random-slope models

(Optional) Try a NegBin GLMM for counts + simple diagnostics

Repo layout
/data/                      # CSVs used in the workshop
/scripts/
  01_get-data.R            # load + merge (simple, annotated)
  02_explore.R             # quick EDA (skim, counts, summaries)
  03_plots.R               # a few ggplots (spaghetti + effects)
  04_models.R              # RI, RS, GLMM + DHARMa + sjPlot
README.md

Datasets (in /data/)

person_day_plus.csv — daily EMA data (e.g., uid, date, drinks, stress_cwc, stress_pm, weekend, day_num, ...)

FlourishingScale.csv — 8-item flourishing scale with uid and type (pre/post)

PHQ-9- depression measures.csv — PHQ-9 with uid and type (pre/post)

grades.csv — simple grade columns per uid (toy baseline)

piazza.csv — activity counts per uid (toy baseline)

We collapse baselines to four between-person columns: flourish_mean, phq9_score, grade_mean, piazza_eng.

Quick start

Download: Code ▸ Download ZIP → unzip → open the .Rproj.

Open scripts/01_get-data.R and click Source.

Run scripts/03_plots.R, then scripts/04_models.R.

Packages (auto-install in 01_get-data.R)
tidyverse, janitor, here, lubridate, ggplot2,
DHARMa, sjPlot, glmmTMB, performance, lme4


Data is derived from the studentlife package: https://cran.r-project.org/web/packages/studentlife/studentlife.pdf 
studentlife is optional (not needed for the tutorial).

What each script does

01_get-data.R — reads CSVs from /data, cleans names, makes stress_cwc, stress_pm (already in the plus file), and merges baselines. Ends with a quick sanity check.

02_explore.R — tiny EDA: skim(), days per person, quick summaries.

03_plots.R — three plots: spaghetti per person, within-person stress vs drinks, and flourishing vs mean drinks (between-person).

04_models.R —

lmer() random intercepts (RI)

lmer() random slopes (RI+RS for stress_cwc)

glmmTMB() NegBin model (counts)

DHARMa residual checks, sjPlot tables/coef plots.

Minimal learning path

Descriptives → 2. RI model → 3. Add RS → 4. Compare fit

GLMM (neg-bin) → 6. Residual checks → 7. Pretty tables/plots


