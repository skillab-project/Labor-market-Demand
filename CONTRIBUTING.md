# Contributing to Labor Market Demand

Thank you for your interest in contributing! This document outlines the process for reporting issues, proposing changes, and submitting code.

---

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Submitting Code Changes](#submitting-code-changes)
- [Development Setup](#development-setup)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Commit Message Guidelines](#commit-message-guidelines)
- [Pull Request Process](#pull-request-process)

---

## Code of Conduct

This project is part of the [SkillLab](https://github.com/skillab-project) EU Horizon Europe research initiative. All contributors are expected to engage respectfully and constructively. Harassment or disruptive behaviour of any kind will not be tolerated.

---

## Getting Started

Before contributing, please:

1. Read the [README](README.md) to understand the analytics pipeline and the key utility functions.
2. Check the [open issues](https://github.com/skillab-project/Labor-market-Demand/issues) — note that as a fork, some issues may be tracked in the [upstream repository](https://github.com/teomaik/Labor-market-Demand).
3. For significant changes to the co-occurrence weighting, clustering methods, or ESCO propagation logic, open an issue first to discuss your approach.

---

## How to Contribute

### Reporting Bugs

Open an issue and include:

- A clear, descriptive title.
- Steps to reproduce the problem, including the API endpoint and parameters used.
- Expected vs. actual behaviour.
- Environment details: R version, OS, and any relevant package versions.
- Any error messages or stack traces from the R console.

### Suggesting Enhancements

Describe the use case, your proposed solution, and any alternatives considered. Particularly welcome contributions include: new clustering algorithms, additional co-occurrence weighting schemes, new Plumber endpoints, and performance improvements for large data fetches.

### Submitting Code Changes

All contributions are made via **Pull Requests**. See [Pull Request Process](#pull-request-process) below.

---

## Development Setup

1. **Fork and clone your fork:**

   ```bash
   git clone https://github.com/<your-username>/Labor-market-Demand.git
   cd Labor-market-Demand
   ```

2. **Install required R packages** (see [README](README.md) for the full list):

   ```r
   install.packages(c("plumber", "future", "promises", "dotenv", "httr", "jsonlite",
                       "dplyr", "igraph", "ggplot2", "plotly", "ggthemes", "ggforce",
                       "apcluster", "uwot", "text2vec", "cluster", "mclust",
                       "factoextra", "binda", "FactoMineR"))
   ```

3. **Configure `.env`:**

   ```env
   API_BASE_URL=https://skillab-tracker.csd.auth.gr/api
   USERNAME=your_username
   PASSWORD=your_password
   ```

4. **Start the API:**

   ```r
   library(plumber)
   pr <- plumb("plumber_analytics_Skillab_fin.R")
   pr$run(host = "0.0.0.0", port = 8000)
   ```

---

## Coding Standards

- Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
- Use `<-` for assignment rather than `=` except in function arguments.
- Add comments to non-obvious logic, especially in the propagation and weighting functions.
- Do not commit credentials or secrets. Use `.env` and `dotenv::load_dot_env()` for all configuration.
- Do not commit large data files or session cache outputs.
- Keep functions small and focused on a single responsibility. Avoid modifying `data_all_skills` or `data_all_occupations` global state inside endpoint handlers.

---

## Testing

Unit tests are in `Unit_tests_live.R`. Run them from R:

```r
source("Unit_tests_live.R")
```

When contributing:

- Add or update test cases for any new or changed functions.
- Ensure all existing tests pass before opening a PR.
- For new Plumber endpoints, include tests that verify the response structure and that expected fields are present.

---

## Commit Message Guidelines

```
<type>: <short summary>
```

| Type       | When to use                                           |
|------------|-------------------------------------------------------|
| `feat`     | A new endpoint or analytics function                  |
| `fix`      | A bug fix                                             |
| `refactor` | Code restructuring without behaviour change           |
| `test`     | Adding or updating tests                              |
| `docs`     | Documentation changes only                           |
| `chore`    | Dependency updates, CI config, tooling changes        |

Examples:

```
feat: add DBSCAN clustering endpoint
fix: correct MI weighting formula for zero co-occurrence pairs
docs: document propagation_skills() parameter options
```

---

## Pull Request Process

1. **Branch naming:** e.g. `feat/dbscan-clustering` or `fix/mi-weight-zero-division`.
2. **Keep PRs focused:** One logical change per PR.
3. **Fill in the PR description** with what changed, why, and how it was tested.
4. **Link related issues** using `Closes #<issue-number>`.
5. **Review:** At least one maintainer review is required.
6. **CI:** All automated checks must pass.

---

## Questions

Open a [discussion or issue](https://github.com/skillab-project/Labor-market-Demand/issues) if you have questions not covered here.
