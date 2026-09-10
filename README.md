# AI to the Rescue: A Novel Approach to Measure Plate Leftovers

Replication materials for **Study 2** of the paper *AI to the Rescue: A Novel Approach to
Measure Plate Leftovers*.

Study 2 tests how accurately a vision–language model (GPT-4o) can estimate the weight of
food left on a plate from a photograph, and how that accuracy depends on the way the
question is put to the model. Because the plates in this study were composed by the
research team and the leftovers were weighed on a scale, every prediction can be compared
against a known ground truth.

> **Scope of this repository.** Only the **expert sample** is included here — the plates
> prepared and weighed by the research team. The consumer sample used in Study 1
> comes from HelloFresh and is under embargo, so neither its data nor its figures are
> published in this repository. Everything below refers to Study 2 only.

---

## Study design in brief

| | |
|---|---|
| **Ground truth** | Leftovers weighed in grams for each plate |
| **Model** | GPT-4o (see the paper for how the responses were obtained) |
| **Analysis sample** | 93 plates, one photograph each, in `Input/Photos` |
| **Runs per prompt** | 5 independent runs, identified by the suffixes `2024`, `2026`, `2029`, `2033`, `2038` |

### The six prompts, in two phases

**Phase 1 — no recipe context.** The model sees only the photograph and is asked for the
quantity in a particular response format:

| Prompt | The model is asked for |
|---|---|
| `Spoons` | leftovers expressed in spoons, converted to grams afterwards |
| `Range` | a lower and an upper bound in grams |
| `Point` | a single point estimate in grams |

**Phase 2 — with recipe context.** The model additionally receives information about what
the dish was:

| Prompt | The model additionally receives |
|---|---|
| `Recipe Name` | the name of the recipe |
| `Ingredient List` | the list of ingredients, without quantities |
| `Ingredient Quantities` | the list of ingredients with their quantities |

### Two plate-size assumptions

Each of the six prompts was run twice: once telling the model the **actual** plate size,
and once assuming a **standard** plate size. Files, variables and figure labels carrying
the word `Standard` (or `standard` / `generic` in some intermediate filenames) refer to the
standard-plate-size condition.

### Best run

Each prompt was run five times, so `Spoons` appears in the data as `Spoons`, `Spoons 2`
… `Spoons 5`. The **best run** is the one whose predictions correlate most strongly with
the weighed grams; its label is stored in the `PromptMax` column and the run-level
correlations are in the `Allruns_*.csv` files. Figures and tables in the paper are based on
the best run; the spread across all five runs is reported separately.

---

## Repository layout

```
FoodWasteAI/
├── Code/                      R scripts (see "Reproducing the analysis")
├── Input/
│   ├── Photos/                93 photographs, one per analysed plate (~332 MB)
│   ├── PromptAnswers/         raw GPT-4o responses, one JSON per prompt × run
│   ├── All_dataframe_hellofresh_sample.xlsx   plate metadata + weighed leftovers
│   ├── ingredients_study2.xlsx                recipe ingredients (HelloFresh boxes)
│   └── transformations.csv                    spoon → gram conversion table
└── Output/
    ├── Data/                  cleaned and analysed datasets (see below)
    ├── Figures/               figures used in the paper
    └── Tables/                LaTeX tables used in the paper
```

The photographs dominate the repository size (~332 MB of ~363 MB total). A shallow clone
(`git clone --depth 1`) is enough if you only want to inspect the code and results.

---

## Reproducing the analysis

**Requirements.** R (developed under version *[add your R version]*) and the following
packages:

```r
install.packages(c(
  "tidyverse", "readxl", "rjson", "janitor", "plyr", "magrittr",
  "ggpubr", "see", "gridExtra", "gt", "tableone", "xtable",
  "RColorBrewer", "ragg", "zipR"
))
# words2number is not on CRAN:
# remotes::install_github("bhaskarvk/words2number")
```

Open `FoodWasteAI.Rproj` first, so that the working directory is the repository root — all
scripts use paths relative to it.

**Run order matters.** `prepare_data_phase1.R` must run first: it builds
`Final_df_grams_ingredients.csv`, which the other three preparation scripts read. The
remaining three can then run in any order, and the plotting script requires all four:

| # | Script | Produces |
|---|---|---|
| 1 | `Code/prepare_data_phase1.R` | `Final_grams_phase1.csv`, `Allruns_phase1.csv`, `Final_precision_phase1.csv` |
| 2 | `Code/prepare_data_phase1_standard.R` | `Final_grams_phase1_standard.csv`, `Allruns_phase1_standard.csv`, `Final_precision_phase1_standard.csv` |
| 3 | `Code/prepare_data_phase2.R` | `Final_grams_phase2.csv`, `Allruns_phase2.csv`, `Final_precision_phase2.csv` |
| 4 | `Code/prepare_data_phase2_standard.R` | `Final_grams_phase2_standard.csv`, `Allruns_phase2_generic.csv`, `Final_precision_phase2_standard.csv` |
| 5 | `Code/Plots.R` | the figures and tables in `Output/Figures` and `Output/Tables` |

`Code/helper_functions.R` is sourced by the other scripts and does not need to be run on its
own. Steps 1–4 parse the raw JSON responses and are the slow part; step 5 reads the prepared
CSVs and is fast.

Since the prepared datasets in `Output/Data` are committed, you can run step 5 on its own to
regenerate every figure and table without re-parsing the model responses.

---

## Datasets in `Output/Data`

| File | One row per | Contents |
|---|---|---|
| `Final_df_grams_ingredients.csv` | plate | plate metadata, weighed leftovers, and the recipe ingredients merged in |
| `Final_df_grams_converted_phase*.csv` | plate | model responses parsed and converted to grams, still one column per prompt/run |
| `Final_grams_phase*.csv` | plate × prompt × run | **the main analysis files** — long format, one prediction per row |
| `Final_precision_phase*.csv` | prompt × run | precision of the ingredient-level classification |
| `Allruns_phase*.csv` | prompt × run | mean prediction and correlation per run; used to identify the best run |
| `recipes.tex` | recipe | ingredient list per recipe, for the appendix |

Files with `_standard` in the name are the standard-plate-size condition; those without are
the actual-plate-size condition.

### Key variables in `Final_grams_phase*.csv`

| Variable | Meaning |
|---|---|
| `PhotoName` | image file in `Input/Photos`, the unit of analysis |
| `Prompt` | prompt and run, e.g. `Spoons 3` is the third run of the Spoons prompt |
| `PromptMax` | label of the best-performing run for that prompt type |
| `Grams Reported` | **ground truth** — leftovers weighed in grams |
| `Grams Predicted` | model estimate, converted to grams |
| `Classification` | signed error band (see below) |
| `ingredient*` | ingredient-level detections, used for the precision analysis |

`Classification` bins the difference between weighed and predicted grams into seven ordered
categories, from `< more than 250 grams` (large over-prediction) through `+/- 25 grams`
(accurate) to `> more than 250 grams` (large under-prediction), plus `Missing` where the
model returned no usable number.

---

## Figures and tables

| File | Shows |
|---|---|
| `Figures/EXPERT_phase1_R1_hq.png` | predicted vs weighed grams, phase-1 prompts, both plate-size conditions |
| `Figures/EXPERT_phase2_R1_hq.png` | predicted vs weighed grams, phase-2 prompts, both plate-size conditions |
| `Figures/EXPERT_phase1_phase2_R1_hq.png` | all six prompts side by side, best run |
| `Figures/EXPERT_grams_allruns_R1_hq.png` | correlation per prompt, mean ± SD across the five runs |
| `Figures/EXPERT_precision_bestrun_R1_hq.png` | ingredient-detection precision per prompt, best run, by plate-size condition |
| `Figures/EXPERT_precision_allruns_R1_hq.png` | ingredient-detection precision per prompt across all five runs |
| `Tables/EXPERT_MAE_bothphases.tex` | mean absolute error per prompt, in grams and as a percentage |
| `Tables/EXPERT_phase1_bestrun.tex` | error-band distribution per phase-1 prompt |
| `Tables/EXPERT_phase2_bestrun.tex` | error-band distribution per phase-2 prompt |

In the scatter plots the dashed diagonal is the line of perfect prediction; points are
coloured by their `Classification` band.

---

## Data availability

The Study 1 consumer data are under embargo from HelloFresh and are not included in this
repository. All Study 2 materials — photographs, raw model responses, prepared datasets,
code, figures and tables — are provided in full.

## Citation

*[Add the full reference once the paper has a DOI.]*

## Contact

Meike Morren — <meike.morren@vu.nl>  
Vrije Universiteit Amsterdam

Questions about the data or the code are welcome; please open an issue or get in touch by
email.

## Licence

| What | Licence |
|---|---|
| Code in `Code/` | [MIT](LICENSE) |
| Photographs, model responses, datasets, figures and tables | [CC BY 4.0](LICENSE-DATA) |

Reuse of either is free, including commercially, as long as the work is credited. The
recipes and ingredient lists in `Input/ingredients_study2.xlsx` derive from HelloFresh
recipe cards; rights in the underlying recipe content remain with HelloFresh.
