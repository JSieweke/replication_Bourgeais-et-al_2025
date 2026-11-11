# Verification code: I4R comment on Bourgeais et al. (2025)

Code and output for the comment by Jost Sieweke (Vrije Universiteit Amsterdam) on

> Bourgeais, Q., Charrier, R., Sanlaville, E. and Seifert, L. (2025). Temporal passing network in basketball:
> The effect of time pressure on the dynamics of team organization at micro and meso levels.
> *Psychology of Sport and Exercise*, 80, 102882. https://doi.org/10.1016/j.psychsport.2025.102882

Prepared for the Institute for Replication (I4R) AI games, November 2026. The comment verifies the ten findings
of the AI-generated report provided by I4R (Barbarioli et al., 2026). Its main point is that the article's
inferential tests treat overlapping 6-s time windows as independent observations; the analyses here test what
happens when the possession (or the game or team) is treated as the unit.

## Files

### R (verification of the AI report)

| File | Purpose |
|---|---|
| `claims_script.R` | Claims script provided by I4R with the AI report. Loads the data, builds all objects and runs 118 checks of the AI report's numbers. Must run first. |
| `companion_checks.R` | Tests the assumptions behind the claims script: column structure, outcome coding, row order, and robustness of the claim-1 result to alternative outcome codings (Section 3.1 of the comment). |
| `claim1.R` | Window-level vs possession-level tests of position involvement and possession outcome (Table 2). |
| `claim4.R` | The article's sequential χ² procedure at three significance levels (Table 3). |
| `other_claims.R` | Checks for claims 2, 3, 5, 9 and 10 and the pooled entropy curve. |

### Stata (independent reimplementation of the main analysis)

| File | Purpose |
|---|---|
| `stata_claim1.do` | Reimplements the claim-1 analysis in Stata 18 from two raw files only (window file and roster), without the claims script. Reports, for 8 position-by-metric cells: window-level χ² (as in the article), logit with standard errors clustered by possession, rank-sum test with one observation per possession, possession-level permutation test (5,000 draws), and linear probability models with wild cluster bootstrap by game (12 clusters) and by team (8 clusters). |

### Output

| File | Content |
|---|---|
| `output/claims_output.txt` | Console output of `claims_script.R` (118 checks, all PASS). |
| `output/companion_output.txt` | Output of `companion_checks.R`. |
| `output/claim1_results.csv` | Results of `claim1.R` (Table 2). |
| `output/other_claims_output.txt` | Output of `other_claims.R`. |
| `output/entropy_pooled.png` | Pooled entropy by shot clock value. |
| `output/stata_claim1.log` | Log of the Stata run reported in Section 3.1. |

## Main result

Pooled deposit, both possession types, 25,673 windows in 1,683 possessions. Number of the 8 position-by-metric
associations significant at .05:

| Test | R | Stata |
|---|---|---|
| Window-level χ² (windows treated as independent, as in the article) | 5 | 5 |
| Logit, standard errors clustered by possession | 0 | 0 |
| Rank-sum test, one observation per possession | 0 | 0 |
| Permutation of outcomes across possessions (5,000 draws) | 0 | 0 |
| Wild cluster bootstrap, clusters = games (12) | | 0 |
| Wild cluster bootstrap, clusters = teams (8) | | 0 |

R and Stata agree to three decimals on involvement shares and on the window-level, clustered and rank-sum
p-values. Permutation p-values differ slightly because of different random numbers. With 8 or 12 clusters, the
bootstrap results are indicative only.

## Data (not included)

Download the four CSV files from the authors' OSF project, https://osf.io/xv73a/ (DATA_FULL.csv, DATA_TW.csv,
INFO_games.csv, INFO_players.csv), and place them in `pkg/`. They are not redistributed here because the
deposit carries no licence. The Stata do-file needs only DATA_TW.csv and INFO_players.csv.

The R claims script also reads the article text. Obtain the article from the publisher, save it as
`paper/paper.pdf`, and either install `pdftotext` (Poppler) or create the text file once with
`pdftotext -raw paper/paper.pdf paper/paper.txt`. The article is not redistributed here.

Expected layout:

```
audit/
  claims_script.R  companion_checks.R  claim1.R  claim4.R  other_claims.R  stata_claim1.do
  pkg/    DATA_FULL.csv  DATA_TW.csv  INFO_games.csv  INFO_players.csv   (only these four files)
  paper/  paper.pdf  [paper.txt]
```

## Running

**R** (tested with R 4.4.2 on Windows 11), with `audit/` as working directory:

```r
install.packages("sandwich")
sink("claims_output.txt", split = TRUE); source("claims_script.R"); sink()
source("companion_checks.R")          # writes companion_output.txt
source("claim1.R", echo = TRUE)       # Table 2
write.csv(round(res1, 4), "claim1_results.csv")
source("claim4.R", echo = TRUE)       # Table 3
source("other_claims.R")              # writes other_claims_output.txt and entropy_pooled.png
```

The claims script takes a few minutes (permutation tests with 5,000 draws, seed 20250520).

**Stata** (tested with Stata 18), from the `audit/` folder, run the whole file at once (it uses temporary
files that do not survive running it in pieces):

```stata
do stata_claim1.do
```

It installs `boottest` (Roodman et al., 2019) from SSC if missing and writes `stata_claim1.log`. Run time is a
few minutes.

## Notes

- The deposit has no possession-type variable and four position labels instead of five, so all reanalyses
  pool both possession types, include the 216 possessions that start in the offensive half and do not
  reproduce the article's exact tests (see Section 3 of the comment).
- Outcome rule (article): scoring and/or provoking an opponent foul is positive; missed shots, turnovers and
  offensive fouls are negative.
- `companion_checks.R` Section F (adjacent-profile tests) does not implement the article's procedure and is not
  used in the comment.
- `companion_checks.R`, `claim1.R`, `claim4.R`, `other_claims.R` and `stata_claim1.do` were written with the help
  of Claude (Anthropic). All analyses were run and checked by the author.

## References

Barbarioli, B., Aparicio, J. P., Brodeur, A., Abdul Baki, G., Fazio, L. and Valenta, D. (2026). Multi-agent
Reproducibility Auditing with Verifiable Evidence. Available at SSRN: https://ssrn.com/abstract=7651342

Roodman, D., Nielsen, M. Ø., MacKinnon, J. G. and Webb, M. D. (2019). Fast and wild: Bootstrap inference in
Stata using boottest. *The Stata Journal*, 19(1), 4–60.

Zeileis, A., Köll, S. and Graham, N. (2020). Various versatile variances: An object-oriented implementation of
clustered covariances in R. *Journal of Statistical Software*, 95(1), 1–36.
