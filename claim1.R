## ======================= CLAIM 1 =======================
## Are the outcome tests (Section 4.3) run on overlapping windows,
## and what happens if we test at the possession level instead?

# (0) Only needed if RStudio was closed since the last run (takes a few minutes):
if (!exists("CELLS")) invisible(capture.output(source("claims_script.R")))

# (1) What the article says it tests (Methods, Section 3.3.2)
regmatches(paper_flat, regexpr("this time, it is the fraction of graphlets.{0,110}", paper_flat))
regmatches(paper_flat, regexpr("Then, the same .{0,160}", paper_flat))

# (2) The article's N against its own possession and window counts
c(N_ball_in = 7100, N_ball_out = 16868, sum = 7100 + 16868, windows_analysed = 24908)
round(c(windows_per_possession_ball_in = 7100 / 537, windows_per_possession_ball_out = 16868 / 998), 1)

# (3) Our analysis frame (pooled over both possession types, 4 deposited position labels)
c(windows = nrow(WK), possessions = max(WK$cl))

# (4) Same 8 tests, four ways: window-level (as in the article) vs three possession-level versions
Y <- cbind(WK_FC, WK_FB); colnames(Y) <- c(paste(CODES, "FC"), paste(CODES, "FB"))
res1 <- t(sapply(colnames(Y), function(k) {
  y   <- Y[, k]
  sh  <- tapply(y, WK$cl, mean)          # share of a possession's windows the position is involved in
  neg <- tapply(WK$neg, WK$cl, max)      # possession outcome (1 = negative)
  c(pct_involved_neg_windows = 100 * mean(y[WK$neg == 1]),
    pct_involved_pos_windows = 100 * mean(y[WK$neg == 0]),
    p_window_chi2            = CELLS[k, "window_p"],
    p_cluster_logit          = CELLS[k, "cluster_p"],
    p_possession_wilcoxon    = wilcox.test(sh[neg == 1], sh[neg == 0], exact = FALSE)$p.value,
    p_possession_permutation = perm_p(y))   # 5,000 permutations, about 1 minute in total
}))
round(res1, 4)

# (5) How many of the 8 cells are significant at .05 under each approach?
colSums(res1[, grep("^p_", colnames(res1))] < .05)
