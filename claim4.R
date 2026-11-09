## ======================= CLAIM 4 =======================
if (!exists("PROF")) invisible(capture.output(source("claims_script.R")))

# (1) The article's procedure (Methods 3.3.1) and how it gets the last phase (Results 4.2)
quote <- function(start, n) { i <- regexpr(start, paper_flat, fixed = TRUE); substr(paper_flat, i, i + n) }
quote("considering the shot clock value s", 420)
quote("Finally, the same thing appears", 260)

# (2) Does the article state a significance level or a multiple-testing correction?
sapply(c("alpha", "significance level", "0.05", "Bonferroni", "\\bHolm\\b", "correction", "Yates"),
       function(s) grepl(s, paper_flat, ignore.case = TRUE))

# (3) The procedure on the deposit (both possession types pooled):
#     for each profile, the first later profile that differs, at three significance levels
fd <- data.frame(clock = as.numeric(rownames(PROF)), windows = as.vector(rowSums(PROF)),
                 first_diff_05 = first_diff(.05), first_diff_01 = first_diff(.01),
                 first_diff_001 = first_diff(.001), row.names = NULL)
fd

# (4) How much do neighbouring profiles overlap? Share of possessions shared with the next profile
clk <- .num(TW$CLOCK); s <- sort(unique(clk), decreasing = TRUE)
shared <- sapply(seq_len(length(s) - 1), function(i)
  mean(unique(TW$key[clk == s[i]]) %in% unique(TW$key[clk == s[i + 1]])))
summary(shared)
