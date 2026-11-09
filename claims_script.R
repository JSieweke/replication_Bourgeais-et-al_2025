## ============================================================================================
## CLAIMS SCRIPT — verify every claim in the AI report, one numbered block at a time.
## Claim numbers match the [claim N] tags in the report, the Appendix-A findings table, and the
## rows of the verdict survey.
##
## THE RULE: if you cannot verify a claim marked RETRACTION or CORRIGENDUM MATERIAL after
## honest study, RETURN THE PAPER and take another. Do not skip it. Do not let AI decide.
##
## NEVER contact the paper's authors or the journal. I4R handles all correspondence.
##
## 10 claims: 0 retraction material, 6 corrigendum material, 4 comment material. File language: .R (the deposit's own).
## ============================================================================================

## ---- standalone check harness (same contract as verification.Rmd) ----------------------------
PKG_DIR <- local({
  e <- Sys.getenv("I4R_PKG_DIR", "")
  if (nzchar(e) && dir.exists(e)) return(normalizePath(e))
  for (cand in c(file.path(getwd(), "pkg"), file.path(dirname(getwd()), "audit", "pkg"), "pkg"))
    if (dir.exists(cand)) return(normalizePath(cand))
  stop("cannot locate the deposit: set I4R_PKG_DIR to the extracted package directory (the pkg/ folder)")
})
AUDIT_DIR <- Sys.getenv("I4R_AUDIT_DIR", dirname(PKG_DIR))
read_deposit <- function(file, ...) {
  path <- if (file.exists(file)) file else file.path(PKG_DIR, file)
  if (!file.exists(path)) stop("not in the deposit: ", file)
  ext <- tolower(tools::file_ext(path))
  switch(ext, csv = utils::read.csv(path, ...), tsv = utils::read.delim(path, ...), txt = utils::read.delim(path, ...),
         rds = readRDS(path), stop("no loader for .", ext, " in this claims script; open the file with the matching R package"))
}
.V <- new.env(); .V$exp <- list(); .V$res <- list()
expect <- function(id, ..., tol = 0, note = NULL) {
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("expect(): every value needs a name")
  for (k in names(vals)) { .V$exp[[paste(id, k, sep = "|")]] <- list(id = id, key = k, expected = vals[[k]], tol = tol, note = note)
    cat(sprintf("[%-6s %-34s] EXPECTED  %s
", id, k, format(vals[[k]]))) }
  invisible(TRUE)
}
check <- function(id, ...) {
  vals <- list(...); if (is.null(names(vals)) || any(names(vals) == "")) stop("check(): every value needs a name")
  for (k in names(vals)) { key <- paste(id, k, sep = "|"); e <- .V$exp[[key]]; ob <- vals[[k]]
    if (is.null(e)) { .V$res[[key]] <- "FAIL (never declared with expect())"; cat(sprintf("[%-6s %-34s] FAIL  undeclared
", id, k)); next }
    ok <- if (is.numeric(e$expected)) isTRUE(length(ob) == 1 && !is.na(ob) && abs(e$expected - ob) <= e$tol + 1e-12) else identical(e$expected, ob)
    .V$res[[key]] <- if (ok) "PASS" else "FAIL"
    cat(sprintf("[%-6s %-34s] OBSERVED  %s  %s
", id, k, format(ob), if (ok) "PASS" else "FAIL")) }
  invisible(TRUE)
}
claims_summary <- function() {
  declared <- names(.V$exp); checked <- names(.V$res)
  for (k in setdiff(declared, checked)) .V$res[[k]] <- "FAIL (declared but never checked)"
  fails <- sum(!vapply(.V$res, function(x) identical(x, "PASS"), logical(1)))
  cat(sprintf("
%d checks, %d PASS, %d FAIL
", length(.V$res), length(.V$res) - fails, fails))
  invisible(fails)
}
## ---- audit-specific helpers (verification_snippets/_prelude.R) ----
## Audit-specific helpers: Bourgeais et al. (2025), Psychology of Sport & Exercise 80, 102882 (four semicolon-separated CSV files on OSF).
## Snippets print counts, aggregates and test statistics only: no team name, player number or timecode.
## Shared objects are built once here and never modified by a snippet (work on copies).
if (!exists("SCRATCH")) SCRATCH <- tempfile()
.rd <- function(f) read.csv(file.path(PKG_DIR, f), sep = ";", colClasses = "character", check.names = FALSE, na.strings = c("", "NA"))   # missing cells are written as NA
FULL <- .rd("DATA_FULL.csv"); TW <- .rd("DATA_TW.csv"); GAMES <- .rd("INFO_games.csv"); PLAYERS <- .rd("INFO_players.csv")
.num <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))          # decimal comma; two timecodes are in scientific notation
FULL$t <- .num(FULL$TIME); FULL$clock <- .num(FULL$CLOCK); FULL$sci <- grepl("E", FULL$TIME, fixed = TRUE)
FULL$kind <- ifelse(!is.na(FULL$PASS), "pass", ifelse(!is.na(FULL$RECEP), "reception", "boundary"))
FULL$key <- paste(FULL$GAME, FULL$POSS, sep = "_")
.first <- !duplicated(FULL$key); .last <- !duplicated(FULL$key, fromLast = TRUE)
POSS <- data.frame(key = FULL$key[.first], t0 = FULL$t[.first], c0 = FULL$clock[.first], stringsAsFactors = FALSE)
POSS$t1 <- FULL$t[.last][match(POSS$key, FULL$key[.last])]
POSS$res <- FULL$RESULT[.last][match(POSS$key, FULL$key[.last])]
POSS$npass <- as.vector(tapply(FULL$kind == "pass", factor(FULL$key, levels = POSS$key), sum))
POSS$sci <- as.vector(tapply(FULL$sci, factor(FULL$key, levels = POSS$key), any))
POSS$dur <- (POSS$t1 - POSS$t0) / 1000
.second <- FULL[ave(seq_len(nrow(FULL)), FULL$key, FUN = seq_along) == 2, ]
POSS$pass_at_start <- (.second$kind[match(POSS$key, .second$key)] == "pass") & ((.second$t[match(POSS$key, .second$key)] - POSS$t0) / 1000 <= 0.05)
RES_POS <- c("2 pts shot V", "3 pts shot V", "2 pts shooting foul V", "2 pts shooting foul X", "3 pts shooting foul V", "3 pts shooting foul X", "defensive foul")
RES_NEG <- c("2 pts shot X", "3 pts shot X", "offensive foul", "turnover")
POSS$out <- ifelse(POSS$res %in% RES_POS, "positive", ifelse(POSS$res %in% RES_NEG, "negative", ifelse(is.na(POSS$res), "no result", "other code")))
LONG <- POSS[POSS$dur > 6, ]                                             # the article's 6-s filter (strict: 1,751 possessions)
LONG$nw_exp <- floor((LONG$dur - 6) / 0.5 + 1e-9) + 1
TW$key <- paste(TW$GAME, TW$POSS, sep = "_"); TW$tf <- as.integer(TW$TW_from)
TW <- TW[order(as.integer(TW$GAME), as.integer(TW$POSS), TW$tf), ]
LONG$nw_tw <- as.vector(table(factor(TW$key, levels = LONG$key)))
LONG$clk <- ifelse(is.na(LONG$c0), "missing", ifelse(LONG$c0 == 24, "24", ifelse(LONG$c0 == 14, "14", "other")))
TW$nums <- lapply(regmatches(TW$NETWORK, gregexpr("[0-9]+", TW$NETWORK)), as.integer)
TW$nums[is.na(TW$NETWORK)] <- list(integer(0))
TW$npass <- ifelse(is.na(TW$NETWORK), -1L, vapply(TW$nums, function(v) if (length(v) > 1) length(v) %/% 2L else 0L, 1L))
.roster <- setNames(PLAYERS$POSITION, paste(PLAYERS$TEAM, as.integer(PLAYERS$NUMBER), sep = "|"))
TW$posset <- mapply(function(tm, v) unique(unname(.roster[paste(tm, v, sep = "|")])), TW$TEAM, TW$nums, SIMPLIFY = FALSE)
TW$out <- LONG$out[match(TW$key, LONG$key)]
CODES <- c("PG", "SF", "PF", "C")
## Window-level analysis frame: windows of possessions with a positive or negative outcome and a recorded player (pooled over possession types).
WD <- TW[TW$out %in% c("positive", "negative") & !is.na(TW$NETWORK), c("key", "GRAPHLET", "out")]
WD$neg <- as.integer(WD$out == "negative"); WD$cl <- match(WD$key, unique(WD$key))
WINV <- sapply(CODES, function(cd) vapply(TW$posset[TW$out %in% c("positive", "negative") & !is.na(TW$NETWORK)], function(s) cd %in% s, TRUE) * 1L)
chi_2x2 <- function(tab, correct = FALSE) unname(suppressWarnings(chisq.test(tab, correct = correct))$statistic)
win_test <- function(cd) {
  y <- WINV[, cd]; tab <- table(factor(y, 1:0), factor(WD$neg, 1:0))
  fit <- glm(y ~ WD$neg, family = binomial)
  V <- sandwich::vcovCL(fit, cluster = WD$cl, type = "HC1")
  c(window_chi2 = chi_2x2(tab), cluster_wald_chi2 = unname(coef(fit)[2]^2 / V[2, 2]),
    poss_chi2 = chi_2x2(table(tapply(y, WD$cl, max), tapply(WD$neg, WD$cl, max))))
}
## Article text: pdftotext -raw keeps sentences together; fall back to paper.txt.
.raw_txt <- tryCatch(paste(system2("pdftotext", c("-q", "-raw", shQuote(file.path(AUDIT_DIR, "paper", "paper.pdf")), "-"), stdout = TRUE), collapse = "\n"), error = function(e) "")
if (!nzchar(.raw_txt)) .raw_txt <- paste(readLines(file.path(AUDIT_DIR, "paper", "paper.txt"), warn = FALSE), collapse = "\n")
.raw_txt <- gsub("­[ \t]*\n[ \t]*", "", .raw_txt); .raw_txt <- gsub("­", "", .raw_txt)
for (lig in list(c("ﬀ", "ff"), c("ﬁ", "fi"), c("ﬂ", "fl"), c("ﬃ", "ffi"), c("ﬄ", "ffl"))) .raw_txt <- gsub(lig[1], lig[2], .raw_txt, fixed = TRUE)
paper_flat <- gsub("\\s+", " ", .raw_txt)
paper_has <- function(s) as.integer(grepl(s, paper_flat, fixed = TRUE))
paper_has_ns <- function(s) as.integer(grepl(gsub("\\s", "", s), gsub("\\s", "", paper_flat), fixed = TRUE))
## All windows of possessions with a positive or negative outcome; windows without a recorded player count as no involvement (reports 01 and 03).
WK <- TW[TW$out %in% c("positive", "negative"), c("key", "TEAM", "GRAPHLET", "out", "NETWORK")]
WK$neg <- as.integer(WK$out == "negative"); WK$cl <- match(WK$key, unique(WK$key))
.wk_rows <- TW$out %in% c("positive", "negative")
.mid <- function(v) if (length(v) >= 4) v[seq(2, length(v) - 2, by = 2)] else integer(0)   # receivers of all but the last pass
WK_FC <- sapply(CODES, function(cd) vapply(TW$posset[.wk_rows], function(s) cd %in% s, TRUE) * 1L)
WK_FB <- sapply(CODES, function(cd) vapply(seq_along(TW$nums)[.wk_rows], function(i)
  cd %in% unname(.roster[paste(TW$TEAM[i], .mid(TW$nums[[i]]), sep = "|")]), TRUE) * 1L)
cell_tests <- function(y) {
  x2 <- chi_2x2(table(factor(y, 1:0), factor(WK$neg, 1:0)))
  fit <- glm(y ~ WK$neg, family = binomial); V <- sandwich::vcovCL(fit, cluster = WK$cl, type = "HC1")
  w <- unname(coef(fit)[2]^2 / V[2, 2])
  c(window_chi2 = x2, window_p = pchisq(x2, 1, lower.tail = FALSE), cluster_chi2 = w, cluster_p = pchisq(w, 1, lower.tail = FALSE))
}
CELLS <- do.call(rbind, c(lapply(CODES, function(cd) cell_tests(WK_FC[, cd])), lapply(CODES, function(cd) cell_tests(WK_FB[, cd]))))
rownames(CELLS) <- c(paste(CODES, "FC"), paste(CODES, "FB"))
perm_p <- function(y, B = 5000, seed = 20250520) {        # outcome labels permuted across possessions (windows keep their possession's label)
  ki <- tapply(y, WK$cl, sum); ni <- tapply(y, WK$cl, length); po <- tapply(WK$neg, WK$cl, max)
  chi <- function(lab) chi_2x2(matrix(c(sum(ki[lab == 1]), sum((ni - ki)[lab == 1]), sum(ki[lab == 0]), sum((ni - ki)[lab == 0])), 2, byrow = TRUE))
  obs <- chi(po); set.seed(seed); ge <- 0
  for (b in seq_len(B)) ge <- ge + (chi(sample(po)) >= obs - 1e-12)
  (ge + 1) / (B + 1)
}
## Shot clock graphlet profiles (pooled) and the article's sequential procedure.
PROF <- table(factor(.num(TW$CLOCK), levels = rev(sort(unique(.num(TW$CLOCK))))), TW$GRAPHLET)
first_diff <- function(alpha) {
  s <- as.numeric(rownames(PROF)); out <- setNames(rep(NA_real_, length(s)), s)
  for (i in seq_along(s)) for (j in seq_len(length(s) - i) + i) {
    tab <- PROF[c(i, j), , drop = FALSE]; tab <- tab[, colSums(tab) > 0, drop = FALSE]
    if (suppressWarnings(chisq.test(tab, correct = FALSE))$p.value < alpha) { out[i] <- s[j]; break }
  }
  out
}
phase_summary <- function(alpha) {
  fd <- first_diff(alpha); s <- as.numeric(names(fd)); gap <- ifelse(is.na(fd), Inf, s - fd)
  term <- max(s[vapply(s, function(x) all(is.na(fd[s <= x])), TRUE)])
  c(stable_start = max(s[gap >= 5]), no_later_block = term, first_diff_19 = unname(fd["19"]), first_diff_18 = unname(fd["18"]))
}
shannon_bits <- function(counts) { p <- counts[counts > 0] / sum(counts); -sum(p * log2(p)) }
## Outcome-by-type tables implied by the printed shares (Section 4.1): reading A uses all four printed shares, reading B the printed totals.
OUT_A <- matrix(c(263, 255, 501, 452), 2, byrow = TRUE); OUT_B <- matrix(c(263, 255, 501, 453), 2, byrow = TRUE)
## Unreceived passes: pass rows whose next row in the possession is not a reception.
.nxt <- c(FULL$kind[-1], NA); .same <- c(FULL$key[-1] == FULL$key[-nrow(FULL)], FALSE)
FULL$unreceived <- FULL$kind == "pass" & !(.same & .nxt %in% "reception")
## Window networks rebuilt from the event file (report 02's rule): a pass enters a window when its release is at or after the window
## start and its reception at or before the window end, in video time from the possession start; edge lists are compared as strings.
.pi <- which(FULL$kind == "pass"); .ni <- pmin(.pi + 1, nrow(FULL))
.ok <- .pi + 1 <= nrow(FULL) & FULL$key[.ni] == FULL$key[.pi] & FULL$kind[.ni] == "reception"
PAIRS <- data.frame(key = FULL$key[.pi][.ok], tp = FULL$t[.pi][.ok], tr = FULL$t[.ni[.ok]], from = as.integer(FULL$PASS[.pi][.ok]),
                    to = as.integer(FULL$RECEP[.ni[.ok]]), stringsAsFactors = FALSE)
PAIRS$t0 <- POSS$t0[match(PAIRS$key, POSS$key)]
.sp <- split(PAIRS, PAIRS$key)
.rebuild <- function(key, tf) {
  q <- .sp[[key]]; if (is.null(q)) return(NA_character_)
  inw <- q$tp >= q$t0[1] + tf & q$tr <= q$t0[1] + tf + 6000
  if (!any(inw)) return(NA_character_)
  paste0("c(", paste(as.vector(t(as.matrix(q[inw, c("from", "to")]))), collapse = ", "), ")")
}
.has_edge <- function(x) !is.na(x) & grepl("^c\\(", x)
REBUILT <- mapply(.rebuild, TW$key, TW$tf, USE.NAMES = FALSE)
REBUILD_SAME <- ifelse(.has_edge(TW$NETWORK) & .has_edge(REBUILT), TW$NETWORK == REBUILT, !.has_edge(TW$NETWORK) & !.has_edge(REBUILT))
## ============================================================================================
## ============================================================================================
## CLAIM 1 [CORRIGENDUM MATERIAL]  (finding F1, Section 3)
##   The outcome tests of Section 4.3 (N = 7,100 and 16,868) appear to count overlapping 6-s
##   windows, 13 to 17 times the possessions of each type, as independent observations; in a
##   pooled demonstration on the deposited windows with the four deposited position labels,
##   possession-level permutation removes 4 or 5 of the 5 window-level results significant at
##   .05 (centre betweenness lies at the boundary: p = .044 with 5,000 draws, .051 with 10,000),
##   so clustering can materially alter the inference; the article's type-specific,
##   five-position links to team performance remain unverified pending a clustered reanalysis.
## Check written by the audit (no author code covers this claim):

## C-1: window-level outcome tests (Section 4.3) count overlapping windows; possession-clustered inference removes most significance.
expect("C-1", printed_n_ball_in = 7100, printed_n_ball_out = 16868, ratio_ball_in = 13.2, ratio_ball_out = 16.9, tol = 0.05,
       note = "printed N of the adapted-metric tests against the 537 and 998 possessions of each type (ratios rounded to one decimal)")
check("C-1", printed_n_ball_in = as.numeric(sum(grepl("N = 7100", paper_flat, fixed = TRUE)) > 0) * 7100,
      printed_n_ball_out = as.numeric(sum(grepl("N = 16868", paper_flat, fixed = TRUE)) > 0) * 16868,
      ratio_ball_in = round(7100 / 537, 1), ratio_ball_out = round(16868 / 998, 1))
expect("C-1", gap_to_24908 = 940, windows_without_outcome = 999, windows_with_outcome = 25673, possessions_with_outcome = 1683,
       window_step_ms = 500, window_length_ms = 6000, tol = 0,
       note = "23,968 = 24,908 - 940 fits inside the deposit's windows without an outcome class; pooled analysis frame of reports 01 and 03")
check("C-1", gap_to_24908 = 24908 - (7100 + 16868), windows_without_outcome = sum(!TW$out %in% c("positive", "negative")),
      windows_with_outcome = nrow(WK), possessions_with_outcome = max(WK$cl),
      window_step_ms = unique(diff(TW$tf)[TW$key[-1] == TW$key[-nrow(TW)]]), window_length_ms = unique(as.integer(TW$TW_to) - TW$tf))
expect("C-1", window_level_significant_cells = 5, clustered_significant_cells = 0, tol = 0,
       note = "8 position-by-metric cells (4 deposited labels x centrality, betweenness), pooled over possession types; cluster-robust logistic Wald test by possession")
check("C-1", window_level_significant_cells = sum(CELLS[, "window_p"] < .05), clustered_significant_cells = sum(CELLS[, "cluster_p"] < .05))
expect("C-1", log10_p_pf_centrality_window = -5.05, log10_p_c_betweenness_window = -6.05, tol = 0.1,
       note = "window-level p of power-forward centrality (about 9e-06) and centre betweenness (about 9e-07), as in report 03")
check("C-1", log10_p_pf_centrality_window = log10(CELLS["PF FC", "window_p"]), log10_p_c_betweenness_window = log10(CELLS["C FB", "window_p"]))
expect("C-1", perm_p_pf_centrality = 0.21, tol = 0.02,
       note = "possession-level permutation p for power-forward centrality (reports 01 and 03: 0.21); 5,000 permutations, seed 20250520")
check("C-1", perm_p_pf_centrality = perm_p(WK_FC[, "PF"]))
expect("C-1", article_linked_to_performance = 1, tol = 0, note = "the Abstract's statement that rests on these tests")
check("C-1", article_linked_to_performance = paper_has("some of which linked to team performance"))
## Multiplicity reading of the printed results (operator check): the eight printed adapted-metric tests among the design's 40 outcome tests.
.pt <- regmatches(paper_flat, gregexpr("N =\\s*(7100|16868)\\)\\s*=\\s*[0-9.]+", paper_flat))[[1]]
.px2 <- c(as.numeric(sub(".*=\\s*([0-9.]+)$", "\\1", .pt)))
.pp <- sort(pchisq(.px2, 1, lower.tail = FALSE))
.holm <- cumprod(.pp <= 0.05 / (40 - seq_along(.pp) + 1))
expect("C-1", printed_tests_parsed = 8, holm_survivors_of_40 = 3, tol = 0,
       note = "printed chi-square values at face value; Holm over 5 positions x 2 metrics x 2 possession types x 2 levels = 40 outcome tests")
check("C-1", printed_tests_parsed = length(.px2), holm_survivors_of_40 = sum(.holm))
.perm_sig <- sum(c(vapply(CODES, function(cd) perm_p(WK_FC[, cd]), 1), vapply(CODES, function(cd) perm_p(WK_FB[, cd]), 1)) < .05)
expect("C-1", permutation_keeps_at_most_1_of_8 = 1, tol = 0,
       note = "possession-level permutation (5,000 draws per cell, seed 20250520): at most 1 of the 8 cells stays significant at .05 (report 03: 1 of 8, centre betweenness 0.044)")
check("C-1", permutation_keeps_at_most_1_of_8 = as.integer(.perm_sig <= 1))
expect("C-1", perm_p_c_betweenness = 0.05, tol = 0.008,
       note = "centre betweenness lies at the .05 boundary under possession-level permutation (report 03: .044 with 5,000 draws; operator check: .051 with 10,000); here 5,000 draws, seed 20250520")
check("C-1", perm_p_c_betweenness = perm_p(WK_FB[, "C"]))

## ============================================================================================
## CLAIM 2 [COMMENT MATERIAL]  (finding F2, Section 4)
##   The deposit lacks the possession-start variables (start location and ball in versus ball
##   out) on which every reported result is stratified: DATA_TW.csv holds all 1,751 possessions
##   longer than 6 s and 26,672 windows against the 1,535 possessions and 24,908 windows
##   analysed, the 216 excluded possessions cannot be identified, and no deposited column
##   records the outcome class, so no type-specific table, figure panel or test can be
##   reproduced.
## Check written by the audit (no author code covers this claim):

## C-2: the possession-start variables are not deposited; the window file holds all 1,751 possessions longer than 6 s.
expect("C-2", tw_possessions = 1751, tw_windows = 26672, surplus_possessions = 216, surplus_windows = 1764, tol = 0,
       note = "DATA_TW.csv against the article's 1,535 possessions and 24,908 windows")
check("C-2", tw_possessions = length(unique(TW$key)), tw_windows = nrow(TW), surplus_possessions = length(unique(TW$key)) - 1535,
      surplus_windows = nrow(TW) - 24908)
expect("C-2", type_or_location_columns = 0, tw_equals_long_set = 1, tol = 0,
       note = "no column in either data file names a start location, possession type, ball status or retained flag; the window file's possessions are the strictly longer-than-6-s set")
check("C-2", type_or_location_columns = sum(grepl("type|location|start|ball|retain|half", c(names(FULL)[1:8], names(TW)[1:9]), ignore.case = TRUE)),
      tw_equals_long_set = as.integer(setequal(unique(TW$key), LONG$key)))
expect("C-2", positive = 797, negative = 886, unclassified = 68, tol = 0,
       note = "outcome rule (made shots, shooting fouls and defensive fouls positive; missed shots, turnovers, offensive fouls negative) among the 1,751 possessions")
check("C-2", positive = sum(LONG$out == "positive"), negative = sum(LONG$out == "negative"), unclassified = sum(!LONG$out %in% c("positive", "negative")))
expect("C-2", article_start_categories = 1, article_offensive_half = 1, tol = 0, note = "article strings for the start categories")
check("C-2", article_start_categories = paper_has("998 possessions started with a ball offside in the defensive half, 537 started with a ball onside in the defensive half"),
      article_offensive_half = paper_has("157 started with a ball offside in the offensive half, and 59 started with a ball onside in the offensive half"))
expect("C-2", outcome_class_columns = 0, tol = 0, note = "no deposited column holds the positive/negative outcome class (RESULT holds raw result codes)")
check("C-2", outcome_class_columns = sum(grepl("class|outcome|success", c(names(FULL)[1:8], names(TW)[1:9]), ignore.case = TRUE)))

## ============================================================================================
## CLAIM 3 [COMMENT MATERIAL]  (finding F3, Section 5)
##   The deposited roster has no SG label (PG 16, SF 49, PF 8, C 23), whereas the article
##   reports 16 PG, 21 SG, 28 SF, 8 PF and 23 C; the deposited SF count equals the article's SG
##   plus SF, so the five-position results (the SG associations in Section 4.3, the stated
##   hierarchy PG, SG, SF, C, PF and the SG panels of Figs 5, 7 and 8) cannot be reproduced from
##   the deposit.
## Check written by the audit (no author code covers this claim):

## C-3: the deposited roster has no SG label; SF equals the article's SG plus SF.
expect("C-3", pg = 16, sf = 49, pf = 8, c = 23, sg = 0, players = 96, tol = 0, note = "POSITION counts in INFO_players.csv")
tabp <- table(factor(PLAYERS$POSITION, levels = c("PG", "SG", "SF", "PF", "C")))
check("C-3", pg = unname(tabp["PG"]), sf = unname(tabp["SF"]), pf = unname(tabp["PF"]), c = unname(tabp["C"]), sg = unname(tabp["SG"]), players = nrow(PLAYERS))
expect("C-3", article_roster_sentence = 1, sg_plus_sf = 49, tol = 0, note = "the article's Section 5.2 roster: 16 PG, 21 SG, 28 SF, 8 PF and 23 C")
check("C-3", article_roster_sentence = paper_has("there are 16 PG, 21 SG, 28 SF, 8 PF, and 23 C"), sg_plus_sf = 21 + 28)

## ============================================================================================
## CLAIM 4 [CORRIGENDUM MATERIAL]  (finding F4, Section 6)
##   The three-phase boundaries (about 19 s and about 11 s in Sections 4.2 and 5.1) rest on a
##   chain of chi-square tests whose significance level, multiplicity handling and test variant
##   the article does not state; on the pooled deposit the boundaries move with the significance
##   level, the tests treat overlapping windows from the same possessions as independent
##   samples, the lowest-clock profiles rest on a few dozen windows, and the final phase follows
##   from the end of the range rather than from a test.
## Check written by the audit (no author code covers this claim):

## C-4: phase boundaries on pooled windows depend on the significance level; thin low-clock profiles; reset starts.
ps05 <- phase_summary(.05); ps01 <- phase_summary(.01); ps001 <- phase_summary(.001)
expect("C-4", first_diff_19s = 17.0, first_diff_18s = 16.0, tol = 0,
       note = "pooled windows, alpha .05, the article's sequential procedure (chi-square on the 2 x k graphlet table): first later profile that differs (report 03)")
check("C-4", first_diff_19s = unname(ps05["first_diff_19"]), first_diff_18s = unname(ps05["first_diff_18"]))
expect("C-4", stable_start_a05 = 16.5, stable_start_a01 = 17.5, stable_start_a001 = 19.0, tol = 0,
       note = "report 04's summary: first profile whose first later difference is at least 5 s away")
check("C-4", stable_start_a05 = unname(ps05["stable_start"]), stable_start_a01 = unname(ps01["stable_start"]), stable_start_a001 = unname(ps001["stable_start"]))
expect("C-4", no_later_block_a05 = 12.0, no_later_block_a01 = 13.5, no_later_block_a001 = 17.5, tol = 0,
       note = "report 03's summary: start of the block of profiles none of which has a later difference")
check("C-4", no_later_block_a05 = unname(ps05["no_later_block"]), no_later_block_a01 = unname(ps01["no_later_block"]), no_later_block_a001 = unname(ps001["no_later_block"]))
expect("C-4", windows_24s = 1503, windows_12s = 438, windows_6s = 24, below_24 = 248, at_14 = 135, tol = 0,
       note = "windows per pooled profile; possessions longer than 6 s starting below 24 s on the shot clock")
check("C-4", windows_24s = sum(PROF["24", ]), windows_12s = sum(PROF["12", ]), windows_6s = sum(PROF["6", ]),
      below_24 = sum(LONG$c0 < 24, na.rm = TRUE), at_14 = sum(LONG$c0 == 14, na.rm = TRUE))
expect("C-4", entropy_bits_24s = 1.70, entropy_bits_14s = 2.58, entropy_bits_6s = 1.56, tol = 0.005,
       note = "pooled State Entropy in bits (base 2 fits Fig. 3B; reports 01 and 03)")
check("C-4", entropy_bits_24s = shannon_bits(PROF["24", ]), entropy_bits_14s = shannon_bits(PROF["14", ]), entropy_bits_6s = shannon_bits(PROF["6", ]))
expect("C-4", article_limited_comparators = 1, article_alpha_stated = 0, tol = 0,
       note = "the article infers the last phase from the limited remaining profiles and states no significance level")
check("C-4", article_limited_comparators = paper_has("the number of remaining shot clock values is limited"),
      article_alpha_stated = as.integer(grepl("alpha|significance level|p < 0\\.05|0\\.05 level", paper_flat, ignore.case = TRUE)))
.cv <- .num(TW$CLOCK); .s <- sort(unique(.cv), decreasing = TRUE)
.sh <- vapply(seq_len(length(.s) - 1), function(i) mean(unique(TW$key[.cv == .s[i]]) %in% unique(TW$key[.cv == .s[i + 1]])), 1)
expect("C-4", median_share_shared_possessions = 0.917, tol = 0.0005,
       note = "median share of the possessions at one clock value that also contribute at the next (report 04: 91.7 percent)")
check("C-4", median_share_shared_possessions = median(.sh))
expect("C-4", article_boundaries_42 = 1, article_11s_51 = 1, tol = 0, note = "where the article states the boundaries (Sections 4.2 and 5.1)")
check("C-4", article_boundaries_42 = paper_has("19.5s for “ball-in” and 18.5s for “ball-out”"), article_11s_51 = paper_has("drops below around 11s"))

## ============================================================================================
## CLAIM 5 [CORRIGENDUM MATERIAL]  (finding F5, Section 7)
##   The printed outcome-by-type test (chi-square 0.34, p = 0.559, N = 1,472) reproduces as a
##   continuity-corrected test only on a table whose ball-out positive share is 45.4 percent,
##   not the printed 45.3 (the four printed shares imply 707 positive possessions and N =
##   1,471); the article names neither the correction nor the 63 possessions without an outcome
##   class; and 4 of the 8 printed p-values in Section 4.3 are smaller than their printed
##   chi-square values imply at one degree of freedom.
## Check written by the audit (no author code covers this claim):

## C-5: printed outcome-by-type test and four p-values.
expect("C-5", chi2_b_corrected = 0.342, p_b_corrected = 0.559, chi2_b_uncorrected = 0.409, chi2_a_corrected = 0.366, chi2_a_uncorrected = 0.435, tol = 0.0006,
       note = "reading B: printed totals 764 and 708 with the ball-in shares (263/255, 501/453); reading A: all four printed shares (263/255, 501/452)")
check("C-5", chi2_b_corrected = unname(chisq.test(OUT_B)$statistic), p_b_corrected = chisq.test(OUT_B)$p.value,
      chi2_b_uncorrected = unname(chisq.test(OUT_B, correct = FALSE)$statistic), chi2_a_corrected = unname(chisq.test(OUT_A)$statistic),
      chi2_a_uncorrected = unname(chisq.test(OUT_A, correct = FALSE)$statistic))
expect("C-5", positives_reading_a = 707, n_reading_a = 1471, ball_out_positive_share_b = 45.4, unclassified_possessions = 63, tol = 0.05,
       note = "reading A totals; reading B's ball-out positive share (453 of 998); 1,535 - 1,472 possessions without an outcome class")
check("C-5", positives_reading_a = sum(OUT_A[, 2]), n_reading_a = sum(OUT_A), ball_out_positive_share_b = round(100 * 453 / 998, 1), unclassified_possessions = 1535 - 1472)
.tests <- regmatches(paper_flat, gregexpr("N =\\s*(7100|16868)\\)\\s*=\\s*[0-9.]+,\\s*p\\s*[=<]\\s*[0-9.]+", paper_flat))[[1]]
.x2 <- as.numeric(sub(".*\\)\\s*=\\s*([0-9.]+),.*", "\\1", .tests)); .p <- as.numeric(sub(".*p\\s*[=<]\\s*([0-9.]+)$", "\\1", .tests))
.eq <- grepl("p\\s*=", .tests)
.lo <- pchisq(.x2 + 0.005, 1, lower.tail = FALSE); .hi <- pchisq(.x2 - 0.005, 1, lower.tail = FALSE)
expect("C-5", reported_tests = 8, p_below_implied_range = 4, tol = 0,
       note = "the eight adapted-metric tests of Section 4.3; printed p (plus rounding) below the range allowed by the printed chi-square (plus rounding) at 1 df")
check("C-5", reported_tests = length(gregexpr("df\\s*=\\s*1\\s*,\\s*N\\s*=\\s*(7100|16868)\\)", paper_flat)[[1]]), p_below_implied_range = sum(.eq & (.p + 0.0005 < .lo)))

## ============================================================================================
## CLAIM 6 [CORRIGENDUM MATERIAL]  (finding F6, Section 7)
##   In the Section 4.3 summary of outcome links, two direction words contradict the descriptor
##   beside them (PG and PF described as less involved, with a 1.3% and a 39.4% greater adapted
##   FC), and one metric label is wrong (PF described as less intermediate, with a 60.8% smaller
##   adapted FC, for a test on adapted FB).
## Check written by the audit (no author code covers this claim):

## C-6: direction and metric words in the Section 4.3 summary.
expect("C-6", pg_less_involved_greater = 1, pf_less_involved_greater = 1, pf_intermediate_fc = 1, pf_ball_in_fb_or = 1, tol = 0,
       note = "article strings: 'less involved' with 'greater adapted FC' (PG, PF ball out); 'less intermediate' with 'smaller adapted FC' for a test on adapted FB (OR 1.62)")
check("C-6", pg_less_involved_greater = paper_has("PG position is less involved but more intermediate (i.e., a 1.3 % greater adapted FC"),
      pf_less_involved_greater = paper_has("PF position is less involved and less intermediate (i.e., a 39.4 % greater adapted FC"),
      pf_intermediate_fc = paper_has("PF is less intermediate (i.e., a 60.8 % smaller adapted FC)"),
      pf_ball_in_fb_or = paper_has_ns("PF position [χ2 (df = 1, N = 7100) = 4.13, p = 0.039, OR = 1.62"))

## ============================================================================================
## CLAIM 7 [COMMENT MATERIAL]  (finding F7, Section 8)
##   The deposit has four CSV files and no analysis code, README, codebook or licence file, and
##   the article names no analysis software, significance level, multiplicity rule, chi-square
##   variant or entropy base; the file conventions (semicolon delimiter with decimal commas,
##   literal NA cells, R-style vector strings for networks, two timecodes in spreadsheet
##   scientific notation, undefined result codes LF and NA) and the re-annotation subset behind
##   the 96.4 percent agreement are neither documented nor deposited.
## Check written by the audit (no author code covers this claim):

## C-7: no code, codebook or licence; undocumented conventions.
pk <- list.files(PKG_DIR, recursive = TRUE)
expect("C-7", deposited_files = 4, csv_files = 4, code_or_doc_files = 0, tol = 0, note = "files in the deposit snapshot")
check("C-7", deposited_files = length(pk), csv_files = sum(grepl("\\.csv$", pk)), code_or_doc_files = sum(grepl("\\.(r|py|do|m|ipynb|md|txt|pdf|docx)$|readme|licen|codebook", pk, ignore.case = TRUE)))
expect("C-7", sci_notation_timecodes = 2, clock_na_cells = 48, clock_na_possessions = 22, lf_possessions = 11, na_result_possessions = 95, lf_in_long = 0, tol = 0,
       note = "format and code facts in DATA_FULL.csv")
check("C-7", sci_notation_timecodes = sum(FULL$sci), clock_na_cells = sum(is.na(FULL$clock)), clock_na_possessions = length(unique(FULL$key[is.na(FULL$clock)])),
      lf_possessions = sum(POSS$res %in% "LF"), na_result_possessions = sum(is.na(POSS$res)), lf_in_long = sum(LONG$res %in% "LF"))
expect("C-7", graphlet_code_matches_passes = 26672, tol = 0, note = "GRAPHLET agrees with the pass count of NETWORK in every window (codes 1, 2, 3-4, 5-9, 10)")
.g <- as.integer(TW$GRAPHLET)
check("C-7", graphlet_code_matches_passes = sum((.g == 1 & TW$npass <= 0) | (.g == 2 & TW$npass == 1) | (.g %in% 3:4 & TW$npass == 2) | (.g %in% 5:9 & TW$npass == 3) | (.g == 10 & TW$npass >= 4)))
expect("C-7", article_names_analysis_software = 0, article_code_statement = 0, tol = 0, note = "article text: no analysis software or code-availability statement")
check("C-7", article_names_analysis_software = as.integer(grepl("\\bR software|R Core|Python|networkx|igraph|statsmodels|SPSS|Stata|MATLAB|mgcv", paper_flat)),
      article_code_statement = as.integer(grepl("code availability|code is available|scripts are available", paper_flat, ignore.case = TRUE)))
.raw <- readLines(file.path(PKG_DIR, "DATA_FULL.csv"), warn = FALSE)
expect("C-7", network_vector_strings = 18589, network_bare_values = 7814, event_lines_with_literal_na = 16422, tol = 0,
       note = "NETWORK as R-style c(...) strings or bare single values; literal NA cells in DATA_FULL.csv")
check("C-7", network_vector_strings = sum(grepl("^c\\(", TW$NETWORK)), network_bare_values = sum(grepl("^[0-9]+$", TW$NETWORK)),
      event_lines_with_literal_na = sum(grepl("(^|;)NA(;|$)", .raw)))

## ============================================================================================
## CLAIM 8 [COMMENT MATERIAL]  (finding F8, Section 9)
##   The deposit keys all 96 rostered players by national team and shirt number, a stable
##   pseudonymous per-player key in three data files; pattern scans found no direct identifier,
##   and the article makes no anonymisation claim and has no ethics, consent or data-protection
##   statement; whether such a key counts as personal data is a determination for the authors
##   and their institution.
## Check written by the audit (no author code covers this claim):

## C-8: pseudonymous per-player key (team plus shirt number); counts only.
.keys <- paste(PLAYERS$TEAM, PLAYERS$NUMBER)
.num_teams <- tapply(PLAYERS$TEAM, PLAYERS$NUMBER, function(x) length(unique(x)))
expect("C-8", roster_rows = 96, unique_team_number_keys = 96, players_sharing_number_across_teams = 84, players_in_passes = 92, tol = 0,
       note = "within-file uniqueness of a roster key, not a demonstrated re-identification")
check("C-8", roster_rows = nrow(PLAYERS), unique_team_number_keys = length(unique(.keys)),
      players_sharing_number_across_teams = sum(PLAYERS$NUMBER %in% names(.num_teams)[.num_teams > 1]),
      players_in_passes = length(unique(c(paste(FULL$TEAM, as.integer(FULL$PASS))[!is.na(FULL$PASS)], paste(FULL$TEAM, as.integer(FULL$RECEP))[!is.na(FULL$RECEP)]))))
expect("C-8", key_cells_event_file = 11996, windows_with_keys = 26403, tol = 0, note = "cells and window rows carrying a shirt number")
check("C-8", key_cells_event_file = sum(!is.na(FULL$PASS)) + sum(!is.na(FULL$RECEP)), windows_with_keys = sum(!is.na(TW$NETWORK)))
expect("C-8", article_ethics_or_consent = 0, article_anonymisation_claim = 0, tol = 0, note = "article text has no ethics, consent or anonymisation statement")
check("C-8", article_ethics_or_consent = as.integer(grepl("ethic|consent|data protection|GDPR", paper_flat, ignore.case = TRUE)),
      article_anonymisation_claim = as.integer(grepl("anonymi[sz]", paper_flat, ignore.case = TRUE)))

## ============================================================================================
## CLAIM 9 [CORRIGENDUM MATERIAL]  (finding F9, Section 10)
##   The article's counts of 6,073 and 5,554 successful passes include 150 and 116 pass events
##   with no recorded receiver (101 of the 150 in possessions ending in a turnover); completed
##   passes number 5,923 and 5,438.
## Check written by the audit (no author code covers this claim):

## C-9: pass events without a recorded receiver inside the 'successful passes'.
expect("C-9", pass_rows = 6073, reception_rows = 5923, unreceived = 150, unreceived_long = 116, completed_long = 5438, unreceived_turnover = 101, unreceived_made_shot = 0, tol = 0,
       note = "rows with a passer, rows with a receiver, pass rows not followed by a reception in the same possession")
.ur <- FULL[FULL$unreceived, ]
check("C-9", pass_rows = sum(FULL$kind == "pass"), reception_rows = sum(FULL$kind == "reception"), unreceived = nrow(.ur),
      unreceived_long = sum(.ur$key %in% LONG$key), completed_long = sum(FULL$kind == "pass" & FULL$key %in% LONG$key) - sum(.ur$key %in% LONG$key),
      unreceived_turnover = sum(POSS$res[match(.ur$key, POSS$key)] %in% "turnover"), unreceived_made_shot = sum(POSS$res[match(.ur$key, POSS$key)] %in% c("2 pts shot V", "3 pts shot V")))

## ============================================================================================
## CLAIM 10 [CORRIGENDUM MATERIAL]  (finding F10, Section 10)
##   The window file follows from the event file for about 99.3 to 99.4 percent of windows under
##   the best boundary rule, leaving 157 to 192 windows in 21 to 25 possessions unexplained; the
##   filter keeps possessions strictly longer than 6 s, so the five possessions of exactly 6.000
##   s are excluded although the text excludes only shorter ones; window counts follow the
##   starting shot clock rather than video time, which accounts for 13 possessions; and 269
##   windows in 42 possessions without a completed pass carry graphlet 1 but no player.
## Check written by the audit (no author code covers this claim):

## C-10: window construction details.
expect("C-10", long_strict = 1751, long_inclusive = 1756, exactly_6s = 5, tol = 0, note = "possessions longer than 6 s and of at least 6 s (video time)")
check("C-10", long_strict = sum(POSS$dur > 6), long_inclusive = sum(POSS$dur >= 6), exactly_6s = sum(abs(POSS$dur - 6) < 1e-9))
.cap <- pmin(LONG$nw_exp, floor((LONG$c0 - 6) / 0.5 + 1e-9) + 1)
expect("C-10", counts_match_duration = 1738, counts_match_shot_clock = 1751, tol = 0, note = "window counts per possession against the duration formula and the shot-clock rule")
check("C-10", counts_match_duration = sum(LONG$nw_exp == LONG$nw_tw), counts_match_shot_clock = sum(.cap == LONG$nw_tw))
expect("C-10", rebuilt_equal = 26515, rebuilt_share = 99.41, differing_possessions = 21, tol = 0.005, note = "report 02's rebuild rule (release and reception inside the window)")
check("C-10", rebuilt_equal = sum(REBUILD_SAME), rebuilt_share = 100 * mean(REBUILD_SAME), differing_possessions = length(unique(TW$key[!REBUILD_SAME])))
expect("C-10", na_network_windows = 269, na_network_possessions = 42, share_of_graphlet1 = 3.3, tol = 0.05, note = "windows without a recorded player, all coded graphlet 1")
check("C-10", na_network_windows = sum(is.na(TW$NETWORK)), na_network_possessions = length(unique(TW$key[is.na(TW$NETWORK)])),
      share_of_graphlet1 = 100 * sum(is.na(TW$NETWORK)) / sum(TW$GRAPHLET == "1"))
expect("C-10", playerless_windows_with_outcome = 252, tol = 0, note = "windows without a recorded player in possessions with a positive or negative outcome; they may enter metric denominators as zeros")
check("C-10", playerless_windows_with_outcome = sum(is.na(TW$NETWORK) & TW$out %in% c("positive", "negative")))

## ---- summary of every audit-written check above ----
claims_summary()
