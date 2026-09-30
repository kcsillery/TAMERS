## #######################################################################

## TAMERS survey data analysis - Katalin Csillery 8 Sept 2026

## 1. data cleaning and creating analysis ready files

## ########################################################################

## cleaning
dat <- read.csv("data/results-survey116296_all_02072026_anonymized.csv", head=T)

names.orig <- names(dat)
dat0 <- dat

to.na <- c("", " ", "NA", "N/A", "na", "n/a", "NULL", "null")
for (j in seq_len(ncol(dat0))) {
  if (is.character(dat0[[j]])) {
    x <- trimws(dat0[[j]])
    x[x %in% to.na] <- NA
    dat0[[j]] <- x
  }
}

dim(dat0)
## [1] 1484  243

## no consent removed
## ###########################
consent_col <- names(dat0)[7]
dat <- dat0[dat0[[consent_col]] == "AO01", ]
dim(dat)
## 1472 rows => 12 rows selected "No" on consent and then stopped

## keep data only if they reached the species questions
## ################################################
table(is.na(dat$groupTime794..Group_time._Conifer_species_selection), is.na(dat$groupTime797..Group_time._Broadleaf_species_selection))
##                    broad
##                    FALSE TRUE
##                    (OK)
## conif FALSE (OK)   694   70
##       TRUE         0  708
## 707 conifer only, 692 answered both, 
table(is.na(dat$groupTime797..Group_time._Broadleaf_species_selection), dat$lastpage..Last_page)
  ##        -1   0   2   3   4   5   6   7   8   9  10
  ## FALSE   1   0   0   0   0   0   0  16  14  11 652
  ## TRUE   17   2 127  84 165  25 141  15   0   0   0

## keep if they at least answered the conifers and create a flag: type: conif, both, all
dat <- subset(dat, !is.na(groupTime794..Group_time._Conifer_species_selection))
dat$sp.flag <- "conif"
dat$sp.flag[!is.na(dat$groupTime797..Group_time._Broadleaf_species_selection)] <- "both"
dat$sp.flag[dat$lastpage..Last_page == 10] <- "all"
table(dat$sp.flag)
  ## all  both conif 
  ## 652    42    70

## check answers by country
## ##################################
country_col <- grep("country", names(dat), ignore.case = TRUE, value = TRUE)[1]
dat$country <- dat[[country_col]]
table(dat$country)
## AO01 AO02 AO03 AO04 AO05 AO06 AO08 AO09 AO11 AO12 AO14 AO15 AO16 AO17 AO18 AO19 AO20 AO27 AO29 AO30 
##    8    2    5    9   19   10    9   18    4  103   67    2   71   29   49    2    3    2    2    1 
## AO31 AO32 AO33 AO34 AO35 AO36 AO37 AO38 AO39 AO40 AO41 AO42 
##   73   16   21   27   20    6   39    4  121    3    6   13 

## Missingness
## #############
dat$na_prop <- rowMeans(is.na(dat))
summary(dat$na_prop)
dat$completeness <- rowSums(!is.na(dat))

## duplicate response checks
## ##########################

## this is my IP, delete all answers
##10.17.5.84 

## key columns
email_col <- grep("Email_address", names(dat), value = TRUE)[1]
ip_col <- grep("IP.address", names(dat), value = TRUE)[1]

## response time column, if present
time_col <- grep("Total.time|totaltime|time", names(dat), ignore.case = TRUE, value = TRUE)

## exclude metadata and derived columns from similarity
email_col <- grep("^G09Q27\\.", names(dat), value = TRUE)[1]
ip_col    <- "ipaddr..IP_address"

exclude <- unique(c(
  ## metadata
  grep("^(id|submitdate|lastpage|startlanguage|seed|ipaddr)\\.", names(dat), value = TRUE),
  
  ## consent / exit
  grep("^(Consent|G00Q36)\\.", names(dat), value = TRUE),
  
  ## optional contact / identifying fields
  email_col,
  grep("^G07Q33\\.", names(dat), value = TRUE),   # garden ID, if identifying
  
  ## open text fields: not useful for exact-answer similarity
  grep("\\.comment\\.|\\.other\\.", names(dat), value = TRUE, ignore.case = TRUE),
  grep("^G06Q28\\.|^G07Q29\\.|^G10Q36\\.", names(dat), value = TRUE),
  
  ## all timing variables
  grep("time", names(dat), value = TRUE, ignore.case = TRUE),
  
  ## derived variables
  c("country", "completeness")
))

survey_vars <- setdiff(names(dat), exclude)
length(survey_vars) ## 170

## similarity function with minimum overlap
sim_fun <- function(a, b, min_overlap = 30) {
  a <- as.character(a)
  b <- as.character(b)
  
  ok <- !(is.na(a) | is.na(b))
  n_overlap <- sum(ok)
  
  if (n_overlap < min_overlap) return(c(sim = NA, overlap = n_overlap))
  
  sim <- sum(a[ok] == b[ok]) / n_overlap
  c(sim = sim, overlap = n_overlap)
}


## clean fake / non-email values
## ################################
fake_emails <- c("-", "nie", "excluded@example.org")

email_valid <- !is.na(dat[[email_col]]) &
  !(dat[[email_col]] %in% fake_emails) &
  grepl("@", dat[[email_col]])

dup_emails <- names(which(table(dat[[email_col]][email_valid]) > 1))

dup_emails
length(dup_emails)

## inspect repeated private emails
## #################################
for (e in dup_emails) {
  cat("\n====================\n")
  cat("EMAIL:", e, "\n")
  
  rows <- which(dat[[email_col]] == e)
}

## pairwise similarity among repeated-email responses
## ###################################################
email_sim <- data.frame()

for (e in dup_emails) {
  rows <- which(dat[[email_col]] == e)
  
  if (length(rows) < 2) next
  
  for (i in 1:(length(rows) - 1)) {
    for (j in (i + 1):length(rows)) {
      
      out <- sim_fun(dat[rows[i], survey_vars],
                     dat[rows[j], survey_vars],
                     min_overlap = 30)
      
      email_sim <- rbind(email_sim, data.frame(
        email = e,
        row1 = rows[i],
        row2 = rows[j],
        id..Response_ID.1 = dat$id..Response_ID[rows[i]],
        id..Response_ID.2 = dat$id..Response_ID[rows[j]],
        sim = out["sim"],
        overlap = out["overlap"]
      ))
    }
  }
}

email_sim

n_pairs <- 10000
set.seed(123)

sim_vals <- numeric(n_pairs)
overlap_vals <- numeric(n_pairs)

for (i in seq_len(n_pairs)) {
  idx <- sample(seq_len(nrow(dat)), 2)
  
  out <- sim_fun(dat[idx[1], survey_vars],
                 dat[idx[2], survey_vars],
                 min_overlap = 30)
  
  sim_vals[i] <- out["sim"]
  overlap_vals[i] <- out["overlap"]
}

sim_vals <- sim_vals[!is.na(sim_vals)]

summary(sim_vals)
quantile(sim_vals, c(.5, .9, .95, .99), na.rm = TRUE)

hist(sim_vals, breaks = 50,
     xlab = "Proportion of identical answers",
     main = "Empirical null distribution")

## Retain most complete response per repeated private email
## ##########################################################
dat$completeness <- rowSums(!is.na(dat[, survey_vars]))

keep_idx <- rep(TRUE, nrow(dat))

for (e in dup_emails) {
  rows <- which(dat[[email_col]] == e)
  
  best <- rows[which.max(dat$completeness[rows])]
  
  cat("\nEMAIL:", e, "\n")
  print(dat[rows, c("id..Response_ID", "completeness")])
  cat("Keeping Response.ID:", dat$id..Response_ID[best], "\n")
  
  keep_idx[setdiff(rows, best)] <- FALSE
}

dat <- dat[keep_idx, ] ## 760 to 751

dim(dat)
## > > > > [1] 751 247

## Check repeated IPs
## #########################
ip_counts <- table(dat[[ip_col]])
dup_ips <- names(ip_counts[ip_counts > 1])

ip_counts[ip_counts > 1]

sort(ip_counts[ip_counts > 1], decreasing = TRUE)

ip_sim <- data.frame()

for (ip in dup_ips) {
  rows <- which(dat[[ip_col]] == ip)
  
  if (length(rows) < 2) next
  
  for (i in 1:(length(rows) - 1)) {
    for (j in (i + 1):length(rows)) {
      
      out <- sim_fun(dat[rows[i], survey_vars],
                     dat[rows[j], survey_vars],
                     min_overlap = 30)
      
      if (!is.na(out["sim"])) {
        ip_sim <- rbind(ip_sim, data.frame(
          ip = ip,
          row1 = rows[i],
          row2 = rows[j],
          sim = out["sim"],
          overlap = out["overlap"]
        ))
      }
    }
  }
}

ip_sim_vals <- ip_sim$sim

summary(ip_sim_vals)
quantile(ip_sim_vals, c(.5, .9, .95), na.rm = TRUE)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1268  0.4071  0.4759  0.4741  0.5357  0.8000 
## >       50%       90%       95% 
## 0.4759001 0.6067817 0.6390936 

## Retain most complete response per repeated IP above the empirical threshold
## ##########################################################
keep_idx_ip <- rep(TRUE, nrow(dat))
crit.val <- quantile(sim_vals, c(.95), na.rm = TRUE)
ip_sim <- subset(ip_sim, sim >= crit.val)
dup_ips <- unique(ip_sim$ip)

for (e in dup_ips) {
  rows <- which(dat[[ip_col]] == e)

  best <- rows[which(dat$completeness[rows]>quantile(dat$completeness, .1))]
  ## if NO response from this IP clears the threshold, keep the single
  ## most complete one rather than dropping the whole group to zero
  if (length(best) == 0) {
      best <- rows[which.max(dat$completeness[rows])]
  }
  
  cat("\nIP:", e, "\n")
  print(dat[rows, c(ip_col, "completeness")])
  cat("Keeping Response.ID:", dat$id..Response_ID[best], "\n")
  
  keep_idx_ip[setdiff(rows, best)] <- FALSE
}

dat <- dat[keep_idx_ip, ]  ## 751 to 747

## #################################################################
email_sim_vals <- email_sim$sim
email_sim_vals <- email_sim_vals[!is.na(email_sim_vals)]

pdf("figures/FigS1_similarity_histogram.pdf", width = 5.2, height = 4.2)

par(
  mar = c(4.2, 4.4, 1.2, 1),
  mgp = c(2.4, 0.7, 0),
  tcl = -0.25,
  las = 1,
  bty = "l",
  cex = 0.8,
  family = "sans"
)

br <- seq(0, 1, by = 0.025)

h0 <- hist(sim_vals, breaks = br, plot = FALSE)
h1 <- hist(ip_sim_vals, breaks = br, plot = FALSE)

plot(
  h0$mids, h0$counts,
  type = "n",
 ## xlim = c(0.05, 0.75),
  ylim = c(0, max(h0$counts, h1$counts, na.rm = TRUE) * 1.15),
  xlab = "Proportion of identical answers",
  ylab = "Frequency"
)

hist(sim_vals, breaks = br,
     col = "grey80", border = "grey50", add = TRUE)

hist(ip_sim_vals, breaks = br,
     col = rgb(0.1, 0.25, 0.8, 0.40),
     border = rgb(0.1, 0.25, 0.8, 0.85),
     add = TRUE)

q95 <- quantile(sim_vals, 0.95, na.rm = TRUE)
abline(v = q95, lwd = 1.2, lty = 2)

## duplicate email similarities as rug + small points
points(
  email_sim_vals,
  rep(max(h0$counts, h1$counts, na.rm = TRUE) * .4, length(email_sim_vals)),
  pch = 16,
  cex = 0.8,
  col = rgb(0.75, 0.1, 0.1, 0.9)
)

legend(
  "topleft",
  legend = c(
    paste0("Random pairs (n = ", length(sim_vals), ")"),
    paste0("Same IP (n = ", length(ip_sim_vals), ")"),
    paste0("Same email (n = ", length(email_sim_vals), ")"),
    "95th percentile"
  ),
  fill = c("grey85", rgb(0.1, 0.25, 0.8, 0.40), NA, NA),
  border = c("grey40", rgb(0.1, 0.25, 0.8, 0.85), NA, NA),
  pch = c(NA, NA, 16, NA),
  col = c(NA, NA, rgb(0.75, 0.1, 0.1, 0.9), "black"),
  lty = c(NA, NA, NA, 2),
  lwd = c(NA, NA, NA, 1.2),
  bty = "n",
  cex = 0.75
)

dev.off()

dat.all <- dat

## dim(dat.all)
## [1] 743 247

save(dat.all, file="data/dat_all.RData")
