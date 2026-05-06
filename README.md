# Labor Market Demand (SkillLab)

[![GitHub Repo](https://img.shields.io/badge/GitHub-Repo-blue?logo=github)](https://github.com/skillab-project/Labor-market-Demand)

Forked from [teomaik/Labor-market-Demand](https://github.com/teomaik/Labor-market-Demand). Original script authored by Konstantinos Charmanas.

## Description

This project provides a **REST API for descriptive and exploratory analytics** on the SkillLab labour market dataset. It is written in **R** using the [Plumber](https://www.rplumber.io/) library and connects to the SkillLab Tracker API to retrieve jobs, skills, occupations, courses, articles, and other resources.

The service provides capabilities for:

- **Data retrieval:** Auto-paginated fetching of jobs, skills, occupations, courses, law/policies, and profiles from the Tracker API with Bearer token authentication.
- **Skill and occupation propagation:** Mapping skills and occupations to their ESCO hierarchy ancestors at configurable levels (Skill, Knowledge, Transversal, Language pillars).
- **Occurrence matrices:** Building binary or frequency co-occurrence matrices between items (jobs, profiles) and skills or occupations.
- **Co-occurrence weighting:** Multiple weighting schemes including inclusion index (II), Jaccard index (JI), equivalence index (EI), and mutual information (MI).
- **Word vector embeddings:** GloVe + UMAP dimensionality reduction for skill space visualisation.
- **Clustering:** Leiden community detection (via `igraph`), Affinity Propagation, K-Means, and Gaussian Mixture Models.
- **Visualisation:** `ggplot2` / `plotly` charts and network graphs.
- **Asynchronous processing:** Multi-session future-based async execution via the `future` and `promises` packages.

The service is part of the [SkillLab](https://github.com/skillab-project) EU Horizon Europe project.

---

## Getting Started Guide

### Prerequisites

- **R 4.2** or newer ([Download R](https://cran.r-project.org/))
- **RStudio** (recommended) or another R IDE
- **Access to the SkillLab Tracker API** — credentials for `API_BASE_URL`, `USERNAME`, and `PASSWORD`.

---

### Installation Steps

1. **Clone the repository:**

   ```bash
   git clone https://github.com/skillab-project/Labor-market-Demand.git
   cd Labor-market-Demand
   ```

2. **Install required R packages:**

   Open R or RStudio and run:

   ```r
   install.packages(c(
     "plumber", "future", "promises", "dotenv",
     "httr", "jsonlite", "dplyr",
     "igraph", "ggplot2", "plotly", "ggthemes", "ggforce",
     "apcluster", "uwot", "text2vec",
     "cluster", "mclust", "factoextra",
     "binda", "FactoMineR"
   ))
   ```

3. **Configure your `.env` file** in the project root:

   ```env
   API_BASE_URL=https://skillab-tracker.csd.auth.gr/api
   USERNAME=your_username
   PASSWORD=your_password
   ```

---

## Running the Application

### Locally

Start the Plumber API from R:

```r
library(plumber)
pr <- plumb("plumber_analytics_Skillab_fin.R")
pr$run(host = "0.0.0.0", port = 8000)
```

Or from the command line:

```bash
Rscript -e "library(plumber); pr <- plumb('plumber_analytics_Skillab_fin.R'); pr$run(host='0.0.0.0', port=8000)"
```

The API will be accessible at `http://localhost:8000`. The Swagger UI is available at `http://localhost:8000/__docs__/`.

### With Docker

```bash
docker-compose up --build
```

Or manually:

```bash
docker build -t labor-market-demand .
docker run -p 8010:8000 --env-file .env labor-market-demand
```

---

## Key Functions

The script provides several utility functions used by the Plumber endpoints:

| Function | Description |
|---|---|
| `api_ex_now()` | Auto-paginated API fetch with Bearer auth |
| `propagation_skills()` | Map skill URIs to ESCO hierarchy ancestors at a given level |
| `propagation_occupations()` | Map occupation URIs to ESCO ancestors at a given level |
| `double_to_occur()` | Build a binary co-occurrence matrix from item–skill pairs |
| `co_occurence_mat()` | Compute a symmetric skill co-occurrence matrix |
| `co_occurence_weight()` | Apply II / JI / EI / MI weighting to the co-occurrence matrix |
| `word_vectors_method()` | GloVe + UMAP skill embeddings |
| `leiden_clust_plot()` | Leiden community detection on the skill network |
| `affinity_clust_plot()` | Affinity Propagation clustering |
| `kmeans_clust_plot()` | K-Means clustering |
| `gmm_clust_plot()` | Gaussian Mixture Model clustering |

---

## Running the Tests

Unit tests are provided in `Unit_tests_live.R`. Run them from R:

```r
source("Unit_tests_live.R")
```

---

## Project Structure

```
Labor-market-Demand/
├── plumber_analytics_Skillab_fin.R  # Main Plumber API script (all endpoints + analytics)
├── Unit_tests_live.R                # Unit tests
├── deploy.sh                        # Deployment helper script
├── Dockerfile                       # Container image definition
├── docker-compose.yml               # Compose configuration
├── .env                             # Environment variables (fill in before running)
├── .gitignore                       # Git ignore rules
├── jenkins/                         # CI/CD pipeline configuration
└── README.md                        # This file
```

---

## Technologies

- **R 4.2+**
- **plumber** — REST API framework for R
- **future / promises** — Asynchronous multi-session processing
- **igraph** — Network construction and Leiden clustering
- **text2vec** — GloVe word vector training
- **uwot** — UMAP dimensionality reduction
- **httr / jsonlite** — HTTP client and JSON parsing
- **ggplot2 / plotly** — Visualisation
- **mclust / apcluster** — Gaussian Mixture Model and Affinity Propagation clustering
- **Docker / Docker Compose** — Containerised deployment

---

## License

This project is licensed under the **Eclipse Public License 2.0 (EPL-2.0)**. See the [LICENSE](LICENSE) file for details.
