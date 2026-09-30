## #######################################################################

## TAMERS survey data analysis - Katalin Csillery 8 Sept 2026

## 2. create a codebook for the questions

## ########################################################################

load("data/dat_all.RData")

## split variable names into short and long versions

name.split <- strsplit(names(dat.all), "\\.\\.", fixed = FALSE)

short.name <- sapply(name.split, `[`, 1)

long.name <- sapply(name.split, function(x) {
  if(length(x) > 1) {
    paste(x[-1], collapse = "..")
  } else {
    NA
  }
})

## inspect
head(short.name)
head(long.name)


## create base table
library(openxlsx)
codebook <- data.frame(
  short.name = short.name,
  long.name = long.name,
  category = NA,
  target.group = "all",
  full.name = names(dat.all),
  stringsAsFactors = FALSE
)

## categories
codebook$category[grepl("^(id|submitdate|lastpage|startlanguage|seed|ipaddr)$", codebook$short.name)] <- "metadata"
codebook$category[grepl("^Consent|^G00Q36", codebook$short.name)] <- "consent"

codebook$category[grepl("^G01Q", codebook$short.name)] <- "respondent characteristics"
codebook$category[grepl("^G02Q001|^G02Q30|^G02Q31", codebook$short.name)] <- "reference forest"
codebook$category[grepl("^G02Q01|^G02Q02|^G02Q03|^G02Q04|^G02Q05|^G02Q06", codebook$short.name)] <- "experience and reproductive material"
codebook$category[grepl("^G03Q", codebook$short.name)] <- "knowledge and information"
codebook$category[grepl("^G06Q02d", codebook$short.name)] <- "conifer species selection"
codebook$category[grepl("^G06Q03d", codebook$short.name)] <- "broadleaf species selection"
codebook$category[grepl("^G05Q", codebook$short.name)] <- "planning and legal aspects"
codebook$category[grepl("^G10Q35|^G07Q32|^G07Q33|^G10Q36|^G09Q27", codebook$short.name)] <- "end questions"
codebook$category[grepl("time", codebook$short.name, ignore.case = TRUE)] <- "timing"
codebook$category[is.na(codebook$category)] <- "derived / other"

## target groups
## occupation G01Q05: AO01 = forestry sector, AO02 = research, AO03 = other
codebook$target.group[grepl("^G01Q06", codebook$short.name)] <- "research only"
codebook$target.group[grepl("^G01Q66", codebook$short.name)] <- "forestry only"

## based on survey relevance found earlier: G02 experience/knowledge blocks hidden for pure researchers
codebook$target.group[grepl("^G02Q01|^G02Q02|^G02Q03|^G02Q04|^G02Q05|^G02Q06|^G03Q", codebook$short.name)] <- "forestry and other"

## optional: keep metadata separate
codebook$target.group[codebook$category %in% c("metadata", "timing", "consent")] <- "administrative"

## write printable Excel
wb <- createWorkbook()
addWorksheet(wb, "codebook")

writeData(wb, "codebook", codebook)

freezePane(wb, "codebook", firstRow = TRUE)
addFilter(wb, "codebook", rows = 1, cols = 1:ncol(codebook))

setColWidths(wb, "codebook", cols = 1, widths = 18)
setColWidths(wb, "codebook", cols = 2, widths = 65)
setColWidths(wb, "codebook", cols = 3, widths = 28)
setColWidths(wb, "codebook", cols = 4, widths = 20)
setColWidths(wb, "codebook", cols = 5, widths = 80)

header.style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
addStyle(wb, "codebook", header.style, rows = 1, cols = 1:ncol(codebook), gridExpand = TRUE)

wrap.style <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "codebook", wrap.style, rows = 2:(nrow(codebook) + 1), cols = 1:ncol(codebook), gridExpand = TRUE)

saveWorkbook(wb, "data/TAMERS_variable_codebook.xlsx", overwrite = TRUE)


