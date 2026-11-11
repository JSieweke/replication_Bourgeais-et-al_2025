*==============================================================================
* Independent Stata check of the main criticism (claim 1, Section 3.1)
* Bourgeais et al. (2025): position involvement vs possession outcome,
* windows treated as independent vs possession (and game/team) as the unit.
*
* Uses only two raw deposit files: pkg/DATA_TW.csv and pkg/INFO_players.csv.
*
* Expected results from the R run (Table 2 of the comment):
*   cell    %neg    %pos   p_window  p_clustered  p_rank   p_perm
*   PG_FC   59.93   60.78   .165      .725        .606
*   SF_FC   68.62   68.27   .544      .858        .779
*   PF_FC   14.30   12.40  <.001      .201        .690     .209
*   C_FC    25.09   26.48   .011      .418        .385
*   PG_FB    6.26    6.54   .354      .687        .843
*   SF_FB   18.49   17.50   .040      .404        .189
*   PF_FB    5.08    4.33   .005      .270        .569
*   C_FB     8.80   10.62  <.001      .051        .178     .053
* Small differences are expected for p_rank (Stata's ranksum has no continuity
* correction) and p_perm (different random numbers; C_FB sits near .05).
*==============================================================================
version 18
clear all
set more off
capture log close
log using "stata_claim1.log", replace text

*---- 1. Roster: team + shirt number -> position -------------------------------
import delimited using "pkg/INFO_players.csv", delimiter(";") varnames(1) ///
    stringcols(_all) case(lower) encoding("utf-8") clear
generate num = real(number)
rename position pos
keep team num pos
isid team num                          // checks whether each team/number pair is unique (so no number twice in a team)
tabulate pos                           // expect C 23, PF 8, PG 16, SF 49
tempfile roster
save `roster'

*---- 2. Windows and possession outcome ----------------------------------------
import delimited using "pkg/DATA_TW.csv", delimiter(";") varnames(1) ///
    stringcols(_all) case(lower) encoding("utf-8") clear
count                                  // expect 26,672 windows
egen pid = group(game poss)            // possession id
egen gid = group(game)                 // game id (12)
egen tid = group(team)                 // team in offense (8)
generate wid = _n                      // window id

* Article's rule: scoring and/or provoking an opponent foul = positive
generate neg = .
replace neg = 0 if inlist(result, "2 pts shot V", "3 pts shot V", "2 pts shooting foul V", ///
    "2 pts shooting foul X", "3 pts shooting foul V", "3 pts shooting foul X", "defensive foul")
replace neg = 1 if inlist(result, "2 pts shot X", "3 pts shot X", "offensive foul", "turnover")
tabulate result neg, missing
keep if !missing(neg)
count                                  // expect 25,673 windows
egen ptag = tag(pid)
count if ptag                          // expect 1,683 possessions

*---- 3. Which positions are involved in each window? --------------------------
* NETWORK holds the passes as "c(passer1, receiver1, passer2, receiver2, ...)",
* a single shirt number (one player, no pass) or "NA" (no player recorded).
generate netclean = ustrregexra(network, "[^0-9,]", "")
split netclean, parse(",") generate(nw) destring

preserve
keep wid team nw*
reshape long nw, i(wid) j(k)
drop if missing(nw)
bysort wid: generate nk = _N           // number of shirt numbers in the window
rename nw num
merge m:1 team num using `roster', keep(master match)
count if _merge == 1                   // numbers not found in the roster
drop _merge
* FC: the position appears anywhere in the window's pass network
* FB: the position receives a pass that is not the window's last pass
*     (receivers sit at the even places 2, 4, ..., nk-2 of the sequence)
generate byte fb_slot = mod(k, 2) == 0 & k <= nk - 2
foreach q in PG SF PF C {
    generate byte fc_`q' = (pos == "`q'")
    generate byte fb_`q' = (pos == "`q'") & fb_slot
}
collapse (max) fc_* fb_*, by(wid)
tempfile inv
save `inv'
restore

merge 1:1 wid using `inv', nogenerate
foreach v of varlist fc_* fb_* {
    replace `v' = 0 if missing(`v')    // windows without any player
}

*---- 4. Window-level chi-square computed from possession totals ----------------
* (used by the permutation test; data = one row per possession with
*  k = involved windows, n = windows, neg = outcome)
capture program drop winchi2
program define winchi2, rclass
    quietly summarize k if neg == 1, meanonly
    local a = r(sum)
    quietly summarize n if neg == 1, meanonly
    local b = r(sum) - `a'
    quietly summarize k if neg == 0, meanonly
    local c = r(sum)
    quietly summarize n if neg == 0, meanonly
    local d = r(sum) - `c'
    return scalar chi2 = (`a' + `b' + `c' + `d') * (`a'*`d' - `b'*`c')^2 / ///
        ((`a' + `b') * (`c' + `d') * (`a' + `c') * (`b' + `d'))
end

* boottest (wild cluster bootstrap) for the game- and team-level checks
capture which boottest
if _rc ssc install boottest

*---- 5. Tests for the 8 position-by-metric cells -------------------------------
matrix R = J(8, 8, .)
local row = 0
local names ""
foreach m in fc fb {
    foreach q in PG SF PF C {
        local ++row
        local y `m'_`q'
        local names `names' `q'_`=strupper("`m'")'

        quietly summarize `y' if neg == 1, meanonly
        matrix R[`row', 1] = 100 * r(mean)
        quietly summarize `y' if neg == 0, meanonly
        matrix R[`row', 2] = 100 * r(mean)

        * (a) windows treated as independent, as in the article
        quietly tabulate `y' neg, chi2
        matrix R[`row', 3] = r(p)

        * (b) logit, standard errors clustered by possession
        quietly logit `y' neg, vce(cluster pid)
        matrix R[`row', 4] = 2 * normal(-abs(_b[neg] / _se[neg]))

        * (c) one observation per possession: share of involved windows, rank-sum test
        preserve
        quietly collapse (mean) share = `y' (max) neg, by(pid)
        quietly ranksum share, by(neg)
        matrix R[`row', 5] = 2 * normal(-abs(r(z)))
        restore

        * (d) permutation of outcomes across whole possessions, 5,000 draws
        preserve
        quietly collapse (sum) k = `y' (count) n = `y' (max) neg, by(pid)
        quietly permute neg chi2 = r(chi2), reps(5000) rseed(20250520) nodots: winchi2
        matrix P = r(p_upper)
        matrix R[`row', 6] = P[1, 1]
        restore

        * (e) nesting beyond possessions: linear probability model with
        *     wild cluster bootstrap (Webb weights), clusters = games (12) and teams (8).
        *     Few clusters: treat as indicative only.
        quietly regress `y' neg, vce(cluster gid)
        quietly boottest neg, reps(9999) weighttype(webb) seed(20250520) nograph
        matrix R[`row', 7] = r(p)
        quietly regress `y' neg, vce(cluster tid)
        quietly boottest neg, reps(9999) weighttype(webb) seed(20250520) nograph
        matrix R[`row', 8] = r(p)
    }
}

matrix rownames R = `names'
matrix colnames R = pct_neg pct_pos p_window p_clust_poss p_rank p_perm p_boot_game p_boot_team
matlist R, format(%9.3f) title("Position involvement and possession outcome")

* Number of cells significant at .05 under each test
forvalues j = 3/8 {
    local s = 0
    forvalues i = 1/8 {
        if R[`i', `j'] < .05 local ++s
    }
    local cn : word `j' of `: colnames R'
    display as text %-14s "`cn'" as result `s' " of 8 significant at .05"
}

log close

