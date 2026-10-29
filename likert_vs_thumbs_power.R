# =============================================================================
# Likert vs thumbs up/down: which one needs more participants?
# =============================================================================
#
# Scenario: users try either the OLD or the NEW product.
#   Q1 (Likert): "How satisfied are you with the product?" 1-5, 5 = extremely satisfied
#   Q2 (thumbs): "Are you satisfied with the product?" up / down
#
# Fair comparison: every simulated person has ONE hidden satisfaction level.
# Both answers come from that same level - Likert cuts it into 5 categories,
# thumbs cuts it into 2. Same people, same true effect, different ruler.
#
# HOW TO RUN
#   Open in RStudio and click "Source", or run:  Rscript likert_vs_thumbs_power.R
#   Missing packages are installed automatically.
#   Full simulation takes ~3 minutes. Output files are written to the
#   current working directory (shown at the end).
#
# Expected result (seed 2026): ~435 participants (1-5 scale) vs ~821 (thumbs)
# =============================================================================


# ---- 1. Packages --------------------------------------------------------------

needed  <- c("ggplot2", "scales", "MASS")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

library(ggplot2)
library(MASS)   # polr(): ordinal logistic regression


# ---- 2. Reproducibility -------------------------------------------------------

# Fix the random number generator type as well as the seed, so results are
# identical across R versions and machines
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
set.seed(2026)


# ---- 3. Settings --------------------------------------------------------------

n_sims  <- 1000                                     # simulations per sample size
alpha   <- 0.05
total_n <- c(50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 1000)  # both groups

# Baseline answers for the OLD product (most users already fairly satisfied)
baseline_probs <- c(0.05, 0.10, 0.20, 0.40, 0.25)   # P(1), P(2), P(3), P(4), P(5)
cutpoints      <- qlogis(cumsum(baseline_probs)[1:4])

# Thumbs up if satisfaction is above the middle of the "neutral" category,
# i.e. people who would answer 3 split roughly half up, half down.
# Alternative: thumbs_cut <- cutpoints[3]  (only 4s and 5s give a thumbs up)
thumbs_cut <- mean(cutpoints[2:3])

# Effect of the NEW product: shift of 0.5 on the logit scale (odds ratio ~1.65)
# Mean score 3.70 -> 3.98; share of 4s and 5s 65% -> 75%; thumbs up 76% -> 84%
effect <- 0.5


# ---- 4. One simulated study ---------------------------------------------------

simulate_once <- function(n_total) {
  n_group <- n_total / 2
  group   <- factor(rep(c("old", "new"), each = n_group), levels = c("old", "new"))
  latent  <- rlogis(n_total, location = ifelse(group == "new", effect, 0))
  
  likert <- cut(latent, breaks = c(-Inf, cutpoints, Inf), labels = 1:5, ordered_result = TRUE)
  thumbs <- as.integer(latent > thumbs_cut)
  
  # Likert: ordinal logistic regression
  # (NaN warnings come from an empty answer category at small n; they affect
  #  the cutpoints, not the group effect we test, so they are silenced)
  fit_likert <- suppressWarnings(polr(likert ~ group, Hess = TRUE))
  z          <- suppressWarnings(coef(summary(fit_likert)))["groupnew", "t value"]
  p_likert   <- 2 * pnorm(-abs(z))
  
  # Thumbs: logistic regression
  fit_thumbs <- glm(thumbs ~ group, family = binomial)
  p_thumbs   <- summary(fit_thumbs)$coefficients["groupnew", "Pr(>|z|)"]
  
  c(likert = isTRUE(p_likert < alpha), thumbs = isTRUE(p_thumbs < alpha))
}


# ---- 5. Run the simulation ----------------------------------------------------

message("Running ", n_sims, " simulations x ", length(total_n),
        " sample sizes (takes a few minutes)...")
start <- Sys.time()

results <- do.call(rbind, lapply(total_n, function(n) {
  message("  n = ", n)
  power <- rowMeans(replicate(n_sims, simulate_once(n)))
  data.frame(n = n,
             format = c("5-point Likert", "Thumbs up/down"),
             power  = power)
}))

message("Done in ", round(difftime(Sys.time(), start, units = "mins"), 1), " minutes")
write.csv(results, "likert_vs_thumbs_results.csv", row.names = FALSE)


# ---- 6. Participants needed for 80% power -------------------------------------

# Linear interpolation between the simulated sample sizes
n_for_80 <- sapply(split(results, results$format),
                   function(d) approx(d$power, d$n, xout = 0.8)$y)

cat("\nParticipants needed for 80% power:\n")
print(round(n_for_80))
cat("Thumbs up/down needs", round(100 * (n_for_80[2] / n_for_80[1] - 1)),
    "% more participants\n\n")


# ---- 7. Plot ------------------------------------------------

likert_col <- "#5B4FCF"   # purple
thumbs_col <- "#E8710A"   # orange

crossings <- data.frame(
  format = names(n_for_80),
  n      = as.numeric(n_for_80),
  label  = paste0("~", round(n_for_80, -1), "\npeople")
)

p <- ggplot(results, aes(x = n, y = power, colour = format)) +
  # 80% target line
  geom_hline(yintercept = 0.8, colour = "grey55", linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = 0, y = 0.83, label = "80% target", hjust = 0,
           colour = "grey45", size = 4.2) +
  # drop lines from the 80% crossings down to the labels
  geom_segment(data = crossings, aes(x = n, xend = n, y = 0.15, yend = 0.8),
               linetype = "dotted", linewidth = 0.7) +
  geom_point(data = crossings, aes(x = n, y = 0.8), size = 4) +
  geom_text(data = crossings, aes(x = n, y = 0.08, label = label),
            size = 4.5, fontface = "bold", lineheight = 0.9) +
  # the curves
  geom_line(linewidth = 1.8, lineend = "round") +
  # direct labels instead of a legend
  annotate("text", x = 290, y = 0.9, label = "1-5 scale", size = 5.5,
           colour = likert_col, fontface = "bold") +
  annotate("text", x = 610, y = 0.42, label = "Thumbs up/down", size = 5.5,
           colour = thumbs_col, fontface = "bold") +
  scale_colour_manual(values = c("5-point Likert" = likert_col,
                                 "Thumbs up/down" = thumbs_col),
                      guide = "none") +
  scale_x_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1000),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  labs(
    title    = "Thumbs up/down needs almost twice as many people",
    subtitle = "Chance of detecting the same small improvement in satisfaction",
    x        = "Number of participants",
    y        = NULL,
    caption  = paste0("Simulation: ", format(n_sims, big.mark = ","),
                      " runs per point | ordinal vs logistic regression")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 19),
    plot.subtitle      = element_text(colour = "grey35", size = 14,
                                      margin = margin(b = 12)),
    plot.caption       = element_text(colour = "grey55", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(colour = "grey35"),
    plot.margin        = margin(20, 24, 14, 16)
  )

print(p)

ggsave("likert_vs_thumbs_power.png", p, width = 8, height = 8, dpi = 200, bg = "white")

cat("Saved likert_vs_thumbs_power.png and likert_vs_thumbs_results.csv to:\n",
    getwd(), "\n\n")


# ---- 8. Record versions (for reproducibility) ---------------------------------

sessionInfo()