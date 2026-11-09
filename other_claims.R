## =============================================================================
## Checks for claims 2, 3, 5, 9 and 10 and the pooled entropy curve
## Run after claims_script.R in the same R session (working directory = audit/).
## Output is also written to other_claims_output.txt.
## =============================================================================
if (!exists("TW")) invisible(capture.output(source("claims_script.R")))
sink("other_claims_output.txt", split = TRUE)

## ---- Claim 2: possession-start variables and outcome ----------------------
cat("\n#### Claim 2\n")
print(names(read.csv("pkg/DATA_FULL.csv", sep = ";", nrows = 1, check.names = FALSE)))
print(names(read.csv("pkg/DATA_TW.csv",   sep = ";", nrows = 1, check.names = FALSE)))
print(c(possessions_in_TW = length(unique(TW$key)), windows_in_TW = nrow(TW),
        article_all_over_6s = 998 + 537 + 157 + 59, article_analysed = 998 + 537,
        article_windows = 24908))
print(setequal(unique(TW$key), LONG$key))      # window file = all possessions > 6 s
print(table(TW$RESULT, useNA = "ifany"))
print(table(n_codes_per_possession = tapply(TW$RESULT, TW$key, function(x) length(unique(x)))))
print(table(LONG$out))   # article (1,535 analysed): 708 positive, 764 negative, 63 unclassified

## ---- Claim 3: positions in the roster --------------------------------------
cat("\n#### Claim 3\n")
print(table(PLAYERS$POSITION))
print(regmatches(paper_flat, regexpr(".{0,80}16 PG, 21 SG, 28 SF, 8 PF, and 23 C.{0,40}", paper_flat)))
print(table(PLAYERS$TEAM, PLAYERS$POSITION))

## ---- Claim 5: printed statistics (Sec. 4.1 and 4.3) ------------------------
cat("\n#### Claim 5\n")
print(round(rbind(ball_in  = 537 * c(neg = .490, pos = .475),
                  ball_out = 998 * c(neg = .502, pos = .453)), 1))   # printed totals: 764 neg, 708 pos
tab_shares <- matrix(c(263, 255, 501, 452), 2, byrow = TRUE)          # from the four printed shares
tab_totals <- matrix(c(263, 255, 501, 453), 2, byrow = TRUE)          # matching the printed totals
print(chisq.test(tab_shares)); print(chisq.test(tab_totals)); print(chisq.test(tab_totals, correct = FALSE))
print(1535 - 1472)                                                    # possessions without outcome class
x2 <- c(14.34, 6.21, 5.32, 4.13, 4.74, 27.49, 12.74, 6.68)
print(data.frame(chi2 = x2,
                 p_printed = c("<.001", ".012", ".020", ".039", ".029", "<.001", "<.001", ".009"),
                 p_implied = round(pchisq(x2, 1, lower.tail = FALSE), 4)))

## ---- Claim 9: passes without a receiver -----------------------------------
cat("\n#### Claim 9\n")
print(c(passes = sum(FULL$kind == "pass"), receptions = sum(FULL$kind == "reception"),
        passes_without_receiver = sum(FULL$unreceived)))
print(c(passes_long = sum(FULL$kind == "pass" & FULL$key %in% LONG$key),
        without_receiver_long = sum(FULL$unreceived & FULL$key %in% LONG$key)))
print(table(next_row = c(FULL$kind[-1], NA)[FULL$unreceived]))
print(table(possession_result = POSS$res[match(FULL$key[FULL$unreceived], POSS$key)], useNA = "ifany"))

## ---- Claim 10: window construction ------------------------------------------
cat("\n#### Claim 10\n")
print(c(windows = length(REBUILD_SAME), rebuilt_identical = sum(REBUILD_SAME),
        share_pct = round(100 * mean(REBUILD_SAME), 2), not_rebuilt = sum(!REBUILD_SAME),
        possessions_affected = length(unique(TW$key[!REBUILD_SAME]))))
print(c(longer_than_6s = sum(POSS$dur > 6), at_least_6s = sum(POSS$dur >= 6),
        exactly_6s = sum(abs(POSS$dur - 6) < 1e-9)))
print(c(possessions = nrow(LONG), match_video_time = sum(LONG$nw_exp == LONG$nw_tw),
        match_shot_clock = sum(.cap == LONG$nw_tw)))
print(c(windows_no_player = sum(is.na(TW$NETWORK)),
        possessions = length(unique(TW$key[is.na(TW$NETWORK)]))))
print(table(graphlet = TW$GRAPHLET[is.na(TW$NETWORK)]))

## ---- Entropy curve (Sec. 3.2 of the comment) ---------------------------------
cat("\n#### Pooled entropy by shot clock value (bits)\n")
H <- apply(PROF, 1, function(x) { p <- x[x > 0] / sum(x); -sum(p * log2(p)) })
print(round(H, 2))
sink()
png("entropy_pooled.png", width = 800, height = 500)
plot(as.numeric(names(H)), H, xlim = c(24, 6), ylim = c(0, 3), pch = 19,
     xlab = "Shot clock at start of window", ylab = "Entropy (bits)", main = "Pooled deposit")
dev.off()
