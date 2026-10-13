# Marketing Customer Segmentation in R

An R translation of the Python marketing-segmentation showcase. It generates
synthetic retail customers, builds rule-based and behavioural segments, explains
the segments, and estimates campaign response for targeted marketing.

**Analysis workbook:** [`01_marketing_segmentation.Rmd`](01_marketing_segmentation.Rmd).
GitHub displays the source; knit it to HTML for an interactive report with charts.

## What the analysis covers

1. Synthetic 8,000-customer data with five known, hidden personas
2. RFM quintile baseline
3. Behavioural feature transformations and standardisation
4. K selection using inertia, silhouette, bootstrap adjusted Rand index, and GMM BIC
5. K-Means, Gaussian mixture, and Ward hierarchical clustering
6. Segment profiles, archetype names, PCA map, and validation against known personas
7. Random-forest segment explainability
8. Logistic-regression and gradient-boosting response models, decile lift, and campaign playbook

The generated observations are synthetic. They illustrate a repeatable workflow,
not evidence about a real customer population.

## Requirements

Install R 4.1 or newer, then install the project and workbook dependencies:

```r
install.packages(c(
  "cluster", "clue", "gbm", "mclust", "pROC", "ranger",
  "ggplot2", "knitr", "rmarkdown"
))
```

From PowerShell in the repository root:

```powershell
Rscript -e "install.packages(c('cluster','clue','gbm','mclust','pROC','ranger','ggplot2','knitr','rmarkdown'))"
Rscript scripts/generate_data.R
```

Open `01_marketing_segmentation.Rmd` in RStudio and select **Knit**, or render it
from the project root:

```r
rmarkdown::render("01_marketing_segmentation.Rmd")
```

Generated data and rendered HTML are intentionally excluded from Git. The
workbook generates the sample data if `data/raw/customers.csv` does not exist.

## Tests

```powershell
Rscript tests/test_pipeline.R
```

## Project layout

```text
R/                         reusable data, feature, clustering, and model functions
scripts/generate_data.R    regenerate the mock customer CSV
tests/test_pipeline.R       focused pipeline tests
01_marketing_segmentation.Rmd
```
