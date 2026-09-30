## #######################################################################

## TAMERS survey data analysis - Anastazija Dimitrova 8 Sept 2026

## 1. data cleaning and creating analysis ready files

## ########################################################################

library(readxl)
library(ggplot2)

df <- as.data.frame(read_excel("data/TAMERS_dissemination_anonymized.xlsx"))

library(ggplot2)

# ------------------------------------------------------------
# 1. Define regions used for dissemination visualization
# ------------------------------------------------------------

regions <- list(
  "Baltic" = c("Finland", "Latvia"),
  "Scandinavia" = c("Norway", "Sweden"),
  "Denmark-Belgium" = c("Belgium", "Denmark"),
  "France-Italy" = c("France", "Italy"),
  "Austria-Slovenia" = c("Austria", "Slovenia"),
  "Oder-Elbe Region" = c("Czechia", "Germany", "Poland"),
  "Carpathian" = c("Slovakia", "Ukraine"),
  "Western Balkans" = c("Albania", "Bosnia & Herzegovina", "Croatia", "Kosovo", "Montenegro", "Serbia"),
  "South-East Balkan" = c("Bulgaria", "Greece", "North Macedonia")
)

country.to.region <- setNames(
  rep(names(regions), lengths(regions)),
  unlist(regions)
)


# ------------------------------------------------------------
# 2. Clean dates and assign regions
# ------------------------------------------------------------

dat <- as.data.frame(df)

dat$date_month <- as.integer(dat$date_month)
dat$date_year <- as.integer(dat$date_year)

dat$month_date <- as.Date(
  sprintf("%04d-%02d-01", dat$date_year, dat$date_month)
)

dat$Country <- as.character(dat$Target_country)

dat$Country[
  is.na(dat$Country) |
  dat$Country=="" |
  dat$Country%in%c("Unknown", "All")
] <- "Multi-country"

dat$Region <- unname(country.to.region[dat$Country])

# Countries not included in one of the pooled regions remain individual
ii <- is.na(dat$Region) & dat$Country!="Multi-country"
dat$Region[ii] <- dat$Country[ii]

dat$Region[dat$Country=="Multi-country"] <- "Multi-country"

dat <- dat[
  !is.na(dat$month_date) &
  dat$month_date>=as.Date("2024-12-01") &
  dat$month_date<=as.Date("2026-07-01"),
]


# ------------------------------------------------------------
# 3. Define four-month periods
# ------------------------------------------------------------

period.levels <- c(
  "Dec 2024 - Mar 2025",
  "Apr 2025 - Jul 2025",
  "Aug 2025 - Nov 2025",
  "Dec 2025 - Mar 2026",
  "Apr 2026 - Jul 2026"
)

dat$period <- NA_character_

dat$period[
  dat$month_date>=as.Date("2024-12-01") &
  dat$month_date<=as.Date("2025-03-01")
] <- period.levels[1]

dat$period[
  dat$month_date>=as.Date("2025-04-01") &
  dat$month_date<=as.Date("2025-07-01")
] <- period.levels[2]

dat$period[
  dat$month_date>=as.Date("2025-08-01") &
  dat$month_date<=as.Date("2025-11-01")
] <- period.levels[3]

dat$period[
  dat$month_date>=as.Date("2025-12-01") &
  dat$month_date<=as.Date("2026-03-01")
] <- period.levels[4]

dat$period[
  dat$month_date>=as.Date("2026-04-01") &
  dat$month_date<=as.Date("2026-07-01")
] <- period.levels[5]

dat <- dat[!is.na(dat$period),]

dat$period <- factor(dat$period, levels=period.levels)


# ------------------------------------------------------------
# 4. Order regions for plotting
# ------------------------------------------------------------

pooled.regions <- c(
  "South-East Balkan",
  "Western Balkans",
  "Carpathian",
  "Oder-Elbe Region",
  "Austria-Slovenia",
  "France-Italy",
  "Denmark-Belgium",
  "Scandinavia",
  "Baltic"
)

individual.regions <- sort(setdiff(
  unique(dat$Region),
  c(pooled.regions, "Multi-country")
))

region.order <- c(
  pooled.regions[pooled.regions%in%unique(dat$Region)],
  individual.regions,
  "Multi-country"
)

region.order <- region.order[region.order%in%unique(dat$Region)]

dat$Region <- factor(dat$Region, levels=region.order)


# ------------------------------------------------------------
# 5. Count documented dissemination actions
# ------------------------------------------------------------

agg <- aggregate(
  rep(1,nrow(dat)),
  by=list(Region=dat$Region, period=dat$period),
  FUN=sum
)

names(agg)[3] <- "n_actions"


# ------------------------------------------------------------
# 6. Plot
# ------------------------------------------------------------

p <- ggplot(agg, aes(x=period, y=Region, size=n_actions)) +
  geom_point(color="#9E315D", alpha=.85) +
  scale_size_continuous(
    name="Number of dissemination actions",
    range=c(3.5,17),
    breaks=c(1,10,50,100)
  ) +
  labs(x=NULL, y=NULL) +
  guides(
    size=guide_legend(
      title="Number of dissemination actions",
      nrow=1,
      label.position="right"
    )
  ) +
  theme_bw(base_family="sans", base_size=13) +
  theme(
    panel.border=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_line(color="grey90", linewidth=.4),
    panel.grid.minor=element_blank(),

    axis.text.x=element_text(
      size=11,
      angle=35,
      hjust=1,
      color="black"
    ),

    axis.text.y=element_text(
      size=11,
      color="black"
    ),

    axis.line.x=element_line(color="black", linewidth=.5),
    axis.line.y=element_line(color="black", linewidth=.5),

    legend.position="right",
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=11),
    legend.key.size=unit(.8,"cm"),
    legend.background=element_blank(),
    legend.key=element_blank(),

    plot.margin=margin(8,8,8,8)
  )

print(p)


# ------------------------------------------------------------
# 7. Save
# ------------------------------------------------------------

ggsave(
  "figures/SFig1_dissemination_efforts.pdf",
  plot=p,
  width=8.5,
  height=5.5,
  device="pdf",
  bg="white"
)
