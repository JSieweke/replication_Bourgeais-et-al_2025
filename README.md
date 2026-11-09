# replication_Bourgeais-et-al_2025
Replication of the paper Bourgeais, Q., Charrier, R., Sanlaville, E. and Seifert, L. (2025). Temporal passing network in basketball: The effect of time pressure on the dynamics of team organization at micro and meso levels. Psychology of Sport and Exercise, 80, 102882

Verification code: I4R comment on Bourgeais et al. (2025)

Code for the comment by Jost Sieweke on Bourgeais, Q., Charrier, R., Sanlaville, E. and Seifert, L. (2025). Temporal passing network in basketball: The effect of time pressure on the dynamics of team organization at micro and meso levels. Psychology of Sport and Exercise, 80, 102882. https://doi.org/10.1016/j.psychsport.2025.102882

Prepared for the Institute for Replication (I4R) AI games, November 2026. The comment verifies the ten findings of the AI-generated report provided by I4R (Barbarioli et al., 2026).

Files
File	Purpose
claims_script.R	Claims script provided by I4R with the AI report. Loads the data, builds all objects and runs 118 checks of the AI report's numbers. Must run first.
companion_checks.R	Tests the assumptions behind the claims script: column structure, outcome coding, row order, and robustness of the claim-1 result to alternative outcome codings (Section 3.1 of the comment).
claim1.R	Window-level vs possession-level tests of position involvement and possession outcome (Table 2).
claim4.R	The article's sequential χ² procedure at three significance levels (Table 3).
other_claims.R	Checks for claims 2, 3, 5, 9 and 10 and the pooled entropy curve.
output/	Console output and results of the runs reported in the comment.
Data (not included)

Download the four CSV files from the authors' OSF project, https://osf.io/xv73a/ (DATA_FULL.csv, DATA_TW.csv, INFO_games.csv, INFO_players.csv), and place them in pkg/. They are not redistributed here because the deposit carries no licence.

The claims script also reads the article text. Obtain the article from the publisher, save it as paper/paper.pdf, and either install pdftotext (Poppler) or create the text file once with pdftotext -raw paper/paper.pdf paper/paper.txt. The article is not redistributed here.

Expected layout:

audit/
  claims_script.R  companion_checks.R  claim1.R  claim4.R  other_claims.R
  pkg/    DATA_FULL.csv  DATA_TW.csv  INFO_games.csv  INFO_players.csv   (only these four files)
  paper/  paper.pdf  [paper.txt]
Running

In R (tested with R 4.4.2 on Windows 11), with audit/ as working directory:

r
install.packages("sandwich")
sink("claims_output.txt", split = TRUE); source("claims_script.R"); sink()
source("companion_checks.R")          # writes companion_output.txt
source("claim1.R", echo = TRUE)       # Table 2
write.csv(round(res1, 4), "claim1_results.csv")
source("claim4.R", echo = TRUE)       # Table 3
source("other_claims.R")              # writes other_claims_output.txt and entropy_pooled.png

The claims script takes a few minutes (permutation tests with 5,000 draws, seed 20250520).

Notes
The deposit has no possession-type variable and four position labels instead of five, so all reanalyses pool both possession types and do not reproduce the article's exact tests (see Section 3 of the comment).
companion_checks.R Section F (adjacent-profile tests) does not implement the article's procedure and is not used in the comment.
companion_checks.R, claim1.R, claim4.R and other_claims.R were written with the help of Claude (Anthropic). All analyses were run and checked by the author.
