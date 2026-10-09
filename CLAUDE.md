# hopi

R package (devtools/roxygen2 workflow) computing housing repeat-sales indices.
Numbered scripts in `R/` (`00-preprocess.R` … `06-download.R`) run in order; `rsindex.R` holds the core index calculation.

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: `DESCRIPTION` Imports/Suggests is the source of truth; `rv` (`rproject.toml` + lockfile) manages the reproducible dev library on top of it, not renv
  - Setup: `rv sync`
  - Add a dependency: add it to `DESCRIPTION` Imports, then `rv add <pkg>` (or `rv sync` if already added to rproject.toml)
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through DESCRIPTION + `rv add`/`rv remove`.
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Load package for interactive dev: `Rscript -e 'devtools::load_all()'`
- Check package: `Rscript -e 'devtools::check()'`
- Document (roxygen): `Rscript -e 'devtools::document()'`
