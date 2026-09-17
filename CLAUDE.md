# hopi

R package (devtools/roxygen2 workflow) computing housing repeat-sales indices.
Numbered scripts in `R/` (`00-preprocess.R` … `06-download.R`) run in order; `rsindex.R` holds the core index calculation.

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`).

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: add it to `DESCRIPTION` Imports, then `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Load package for interactive dev: `Rscript -e 'devtools::load_all()'`
- Check package: `Rscript -e 'devtools::check()'`
- Document (roxygen): `Rscript -e 'devtools::document()'`
