## =============================================================================
## Companion checks for the I4R claims script on Bourgeais et al. (2025)
## Run AFTER claims_script.R, in the SAME R session (it reuses FULL, TW, POSS,
## LONG, CODES, .roster, .mid, chi_2x2, PROF, RES_POS, RES_NEG, .first, .last).
##
## Purpose: the claims script tests whether the AI audit's own code reproduces
## the AI audit's own numbers. These checks test the assumptions underneath:
##   A/B  does the deposit really lack type / outcome information?   (claim 2)
##   C    is the audit's outcome coding the only defensible one?     (claims 1, 2)
##   D    do the data have the row structure the audit assumes?     (claims 9, 10)
##   E    does "clustering removes significance" survive other
##        reasonable analysis choices?                              (claim 1)
##   F    how fragile is the phase procedure to its own definition? (claim 4)
## Output is also written to companion_output.txt in the audit folder.
## =============================================================================

stopifnot(exists("FULL"), exists("TW"), exists("POSS"), exists("LONG"), exists("PROF"))
out_file <- file.path(AUDIT_DIR, "companion_output.txt")
sink(out_file, split = TRUE)
hdr <- function(x) cat("\n\n#### ", x, " ", strrep("#", max(3, 70 - nchar(x))), "\n", sep = "")

## ---- A. Original column structure -------------------------------------------
## C-2 scans only names(FULL)[1:8] and names(TW)[1:9]. If a file has more
## original columns than that, the C-2 name check silently skips them.
hdr("A. Original columns per file")
orig <- list()
for (f in c("DATA_FULL.csv", "DATA_TW.csv", "INFO_games.csv", "INFO_players.csv")) {
  nm <- names(read.csv(file.path(PKG_DIR, f), sep = ";", nrows = 1, check.names = FALSE))
  orig[[f]] <- nm
  cat(sprintf("%-17s %2d columns: %s\n", f, length(nm), paste(nm, collapse = " | ")))
}
cat("C-2 scans 8 columns of DATA_FULL and 9 of DATA_TW. Covered fully:",
    length(orig[["DATA_FULL.csv"]]) <= 8 && length(orig[["DATA_TW.csv"]]) <= 9, "\n")

## ---- B. Value tables of every low-cardinality original column ---------------
## Look for anything that could code possession type, start location, half or
## outcome under an unhelpful name (a name search cannot find these).
hdr("B. Low-cardinality columns (<= 25 distinct values)")
for (f in c("DATA_FULL.csv", "DATA_TW.csv")) {
  d <- if (f == "DATA_FULL.csv") FULL else TW
  for (v in orig[[f]]) {
    k <- length(unique(d[[v]]))
    if (k <= 25) { cat("\n", f, " $ ", v, " (", k, " values)\n", sep = ""); print(table(d[[v]], useNA = "ifany")) }
    else cat("\n", f, " $ ", v, ": ", k, " distinct values (not tabulated)\n", sep = "")
  }
}

## ---- C. Outcome coding ------------------------------------------------------
## The audit classifies possessions from the RESULT code on the last row.
## Compare every code and its audit class with the article's Fig. 2C.
hdr("C. RESULT codes and the audit's outcome classes")
cat("Rows with a non-missing RESULT, by whether the row is the possession's last row:\n")
print(table(has_result = !is.na(FULL$RESULT), last_row = .last))
codes <- sort(unique(POSS$res), na.last = TRUE)
print(data.frame(
  code        = ifelse(is.na(codes), "<NA>", codes),
  n_all       = vapply(codes, function(k) if (is.na(k)) sum(is.na(POSS$res)) else sum(POSS$res %in% k), 1),
  n_long      = vapply(codes, function(k) if (is.na(k)) sum(is.na(LONG$res)) else sum(LONG$res %in% k), 1),
  audit_class = ifelse(codes %in% RES_POS, "positive", ifelse(codes %in% RES_NEG, "negative", "unclassified")),
  row.names = NULL))

## ---- D. Row-structure assumptions --------------------------------------------
hdr("D. Row structure the audit assumes")
cat("Rows with BOTH a passer and a receiver (audit codes these as 'pass'):",
    sum(!is.na(FULL$PASS) & !is.na(FULL$RECEP)), "\n")
cat("Possessions whose rows are not contiguous in the file:",
    sum(duplicated(rle(FULL$key)$values)), "\n")
dt <- ave(FULL$t, FULL$key, FUN = function(x) c(0, diff(x)))
cat("Backward time steps within a possession (file order):", sum(dt < 0, na.rm = TRUE), "\n")
cat("Kind of the first row of each possession:\n"); print(table(FULL$kind[.first], useNA = "ifany"))
cat("Kind of the last row of each possession:\n");  print(table(FULL$kind[.last],  useNA = "ifany"))
cat("Claim 9: what follows an 'unreceived' pass (next row in the file):\n")
nxt_kind <- c(FULL$kind[-1], NA)[FULL$unreceived]
nxt_same <- c(FULL$key[-1] == FULL$key[-nrow(FULL)], FALSE)[FULL$unreceived]
print(table(next_kind = nxt_kind, same_possession = nxt_same, useNA = "ifany"))
cat("Claim 9: possession result of the 150 unreceived passes:\n")
print(table(POSS$res[match(FULL$key[FULL$unreceived], POSS$key)], useNA = "ifany"))

## ---- E. Claim 1 sensitivity ----------------------------------------------------
## Same involvement indicators as the audit (FC = position appears in the
## window network; FB = position receives a non-final pass), re-tested under:
##   V0 audit outcome rule (should reproduce CELLS)
##   V1 audit rule, windows without a recorded player dropped
##   V2 shot-based rule: made shots + and-ones positive; missed shots, turnovers,
##      offensive fouls negative; fouls on missed shots and defensive fouls excluded
## Three tests per cell: naive window chi-square, possession-clustered logistic
## Wald, and a possession-level Wilcoxon on each possession's share of involved
## windows (one observation per possession).
## NOTE: the audit's 0/1 involvement indicators are its own operationalisation.
## Check against the article whether "adapted FC/FB" is binary per window or a
## continuous metric before treating any of this as a test of the article.
hdr("E. Claim 1: window-level vs possession-level inference under alternative choices")
cells_for <- function(out_tw, drop_playerless = FALSE) {
  rows <- out_tw %in% c("positive", "negative")
  if (drop_playerless) rows <- rows & !is.na(TW$NETWORK)
  idx <- which(rows)
  neg <- as.integer(out_tw[idx] == "negative")
  cl  <- match(TW$key[idx], unique(TW$key[idx]))
  FC <- sapply(CODES, function(cd) vapply(TW$posset[idx], function(s) cd %in% s, TRUE) * 1L)
  FB <- sapply(CODES, function(cd) vapply(idx, function(i)
          cd %in% unname(.roster[paste(TW$TEAM[i], .mid(TW$nums[[i]]), sep = "|")]), TRUE) * 1L)
  one <- function(y) {
    x2  <- chi_2x2(table(factor(y, 1:0), factor(neg, 1:0)))
    fit <- suppressWarnings(glm(y ~ neg, family = binomial))
    V   <- sandwich::vcovCL(fit, cluster = cl, type = "HC1")
    w   <- unname(coef(fit)[2]^2 / V[2, 2])
    sh  <- tapply(y, cl, mean); po <- tapply(neg, cl, max)
    wx  <- suppressWarnings(wilcox.test(sh[po == 1], sh[po == 0])$p.value)
    c(n_win = length(y), n_poss = max(cl),
      involved_pct_neg = 100 * mean(y[neg == 1]), involved_pct_pos = 100 * mean(y[neg == 0]),
      window_p = pchisq(x2, 1, lower.tail = FALSE), cluster_p = pchisq(w, 1, lower.tail = FALSE),
      poss_wilcox_p = wx)
  }
  res <- rbind(t(sapply(CODES, function(cd) one(FC[, cd]))), t(sapply(CODES, function(cd) one(FB[, cd]))))
  rownames(res) <- c(paste(CODES, "FC"), paste(CODES, "FB"))
  res
}
show <- function(m, label) {
  cat("\n--", label, "--\n")
  print(round(m, 4))
  cat("Significant at .05:  window", sum(m[, "window_p"] < .05),
      "| clustered", sum(m[, "cluster_p"] < .05),
      "| possession Wilcoxon", sum(m[, "poss_wilcox_p"] < .05), "of", nrow(m), "\n")
}
E0 <- cells_for(TW$out)
cat("Sanity: V0 window p equals the claims script's CELLS:",
    isTRUE(all.equal(unname(E0[, "window_p"]), unname(CELLS[, "window_p"]))), "\n")
show(E0, "V0 audit outcome rule")
show(cells_for(TW$out, drop_playerless = TRUE), "V1 audit rule, playerless windows dropped")
v2_pos <- c("2 pts shot V", "3 pts shot V", "2 pts shooting foul V", "3 pts shooting foul V")
v2_neg <- c("2 pts shot X", "3 pts shot X", "turnover", "offensive foul")
v2 <- ifelse(LONG$res %in% v2_pos, "positive", ifelse(LONG$res %in% v2_neg, "negative", NA))
show(cells_for(v2[match(TW$key, LONG$key)]), "V2 shot-based outcome rule")

## ---- F. Claim 4: an alternative reading of the sequential procedure -----------
## The audit's first_diff() compares each profile with every later one until one
## differs. Another reading compares adjacent profiles only. If the two readings
## give very different boundaries, the article's unstated procedure matters.
hdr("F. Claim 4: adjacent-profile chi-square tests (pooled windows)")
adj_p <- vapply(seq_len(nrow(PROF) - 1), function(i) {
  tab <- PROF[c(i, i + 1), , drop = FALSE]; tab <- tab[, colSums(tab) > 0, drop = FALSE]
  suppressWarnings(chisq.test(tab, correct = FALSE))$p.value
}, 1)
print(data.frame(from_s = rownames(PROF)[-nrow(PROF)], to_s = rownames(PROF)[-1],
                 windows_from = as.vector(rowSums(PROF))[-nrow(PROF)],
                 p = round(adj_p, 4), sig05 = adj_p < .05, row.names = NULL))

hdr("Session")
print(sessionInfo())
sink()
cat("\nCompanion output written to:", out_file, "\n")
