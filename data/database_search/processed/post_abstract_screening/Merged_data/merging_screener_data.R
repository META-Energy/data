library(readr)
library(tidyverse)
library(openxlsx)
library(here)

# Importing screener 1 data 
s1_data <- read_csv(here("Screener_1_data/abstract-screening-morpep-energy-screener-1.csv"))

# Importing screener 2 data
s2_data <- read_csv(here("Screener_2_data/abstract-screening-morpep-energy-screener-2.csv"))

## Some consistency tests and corrections before merging 
# Screener 1: only valid screener notes?
unique(s1_data$exported_notes_1)
paste("Screener 1 notes are valid.")
# Screener 2: only valid screener notes?
unique(s2_data$exported_notes_1)
paste('For screener 2, there is one 91, which is supposed to be 01. But this would not change the exclusion of the study.')
View(s2_data %>% filter(exported_notes_1=="91")) # Inspecting the entry.
paste(s2_data %>% filter(exported_notes_1=="91") %>% select(title, abstract)) # Inspecting title and abstract of the wrong entry.
paste("Exclusion is valid.")
# Do screener 1 notes correctly correspond to in-/exlclusion of studies?
s1_included_notes <- s1_data$exported_notes_1 %in% c("11", "12", "21", "22") # Index of entries that carry inclusion notes
sum(s1_data$Included_1[s1_included_notes] != 1) 
paste("Valid! (0 implies that NO entry that carries an inclusion note is excluded)")
s1_excluded_notes <- s1_data$exported_notes_1 %in% c("00", "01", "02", "10", "20", "3", "4") # Index of entries that carry exclusion notes
sum(s1_data$Included_1[s1_excluded_notes] != 0) 
paste("For screener 1, there is one entry that is included though it carries an exclusion note. But no correction necessary, as the entry will be excluded in the full text screening phase.")
View(s1_data %>% filter(Included_1 == 1 & exported_notes_1 %in% c("00", "01", "02", "10", "20", "3", "4"))) # Inspect the entry.
# Do screener 2 notes correctly correspond to in-/exclusion of studies?
s2_included_notes <- s2_data$exported_notes_1 %in% c("11", "12", "21", "22")
sum(s2_data$Included_1[s2_included_notes] != 1) 
paste("For screener 2, there is one entry that was excluded though it carries an inclusion note. Correction not necessary because this mistake will only marginally affect the sample.")
s2_excluded_notes <- s2_data$exported_notes_1 %in% c("00", "01", "02", "10", "20", "3", "4")
sum(s2_data$Included_1[s2_excluded_notes] != 0) 
paste("There are 2 entries which were included though they carry exclusion notes. But no correction necessary, as these entries will be excluded in the full text screening phase.")
View(s2_data %>% filter(Included_1 == 1 & exported_notes_1 %in% c("00", "01", "02", "10", "20", "3", "4"))) # Inspecting the entries.

## Merging the data
# Test that record id and Key are matching for both s1_data and s2_data
all(s1_data[, c("record_id", "Key")] == s2_data[, c("record_id", "Key")]) # Alternative test: sum(paste0(s1_data$record_id,s1_data$Key) != paste0(s2_data$record_id,s2_data$Key))
# Merge s1_data and s2_data based on all variables that must have equal values, i.e. those that have not been created by ASReview
columns <- colnames(s1_data) # These are the column names of the dataframe (they are equal for s1_data and s2_data, see: 'sum(colnames(s1_data) != colnames(s2_data))')
matching_columns <- head(columns, -4) # These are the columns that have not been created by ASReview
merged_data <- merge(s1_data, s2_data, by = matching_columns, all = T, sort = F, suffixes = c(".s1",".s2")) # This merges the dataframes by all columns that must have equal values. Columns with separate values are retained.
# Test that merged data correctly matches original data
all(na.omit(merged_data[, c("record_id", "Key", "Included_1.s1", "exported_notes_1.s1")]) == na.omit(s1_data[, c("record_id", "Key", "Included_1", "exported_notes_1")])) # For screener 1, test that record_id, Key, Included_1 and exported_notes_1 in the merged data matches the original entries
all(na.omit(merged_data[, c("record_id", "Key", "Included_1.s2", "exported_notes_1.s2")]) == na.omit(s2_data[, c("record_id", "Key", "Included_1", "exported_notes_1")])) # For screener 1, test that record_id, Key, Included_1 and exported_notes_1 in the merged data matches the original entries

# Analyses of screening
s1_s2_agreement_data <- merged_data %>%   
  slice_tail(n = 21803) %>% # Excluding the prior sample
  select(Key, Included_1.s1, Included_1.s2, exported_notes_1.s1, exported_notes_1.s2) %>% 
  filter(!is.na(Included_1.s1) | !is.na(Included_1.s2))
paste(nrow(s1_s2_agreement_data), "entries were screened by either screener 1 or 2.", sum(s1_s2_agreement_data$Included_1.s1, na.rm = T), "were included by screener 1.", sum(s1_s2_agreement_data$Included_1.s2, na.rm = T), "were included by screener 2") 
# Screener 1 proportions of in-/exclusions notes
round(prop.table(table(s1_s2_agreement_data$exported_notes_1.s1)) * 100, 1)
barplot(prop.table(table(s1_s2_agreement_data$exported_notes_1.s1)) * 100)
title("Screener 1 notes (%)")
# Screener 2 proportions of in-/exclusions notes
round(prop.table(table(s1_s2_agreement_data$exported_notes_1.s2)) * 100, 1)
barplot(prop.table(table(s1_s2_agreement_data$exported_notes_1.s2)) * 100)
title("Screener 2 notes (%)")
# Agreement and overlap
# Included by both screeners
included_both <- s1_s2_agreement_data %>% 
  filter(Included_1.s1 == 1 & Included_1.s2 == 1)
paste(nrow(included_both), "entries were included by both screeners.")
# Excluded by both screeners 
excluded_both <- s1_s2_agreement_data %>% 
  filter(Included_1.s1 == 0 & Included_1.s2 == 0)
paste(nrow(excluded_both), "entries were excluded by both screeners.")
# Included by screener 1, excluded by screener 2
included_s1_excluded_s2 <- s1_s2_agreement_data %>% 
  filter(Included_1.s1 == 1 & Included_1.s2 == 0)
paste(nrow(included_s1_excluded_s2), "entries were included by screener 1 but excluded by screener 2.")
# Included by Screener 2, excluded by screener 1
included_s2_excluded_s1 <- s1_s2_agreement_data %>% 
  filter(Included_1.s1 == 0 & Included_1.s2 == 1)
paste(nrow(included_s2_excluded_s1), "entries were included by screener 2 but excluded by screener 1.")
# Screened by both screener 1 and 2
both <- s1_s2_agreement_data %>% 
  filter(!is.na(Included_1.s2) & !is.na(Included_1.s1))
paste(nrow(both), "entries were screened by both screeners. Of these,",
      sum(both$Included_1.s1), ", i.e.", round(sum(both$Included_1.s1)/nrow(both)*100,1), "%, were included by Screener 1 and", sum(both$Included_1.s2), ", i.e.", round(sum(both$Included_1.s2)/nrow(both)*100,1), "%, were included by Screener 2")
# Exclusively screened by Screener 1
only_s1 <- s1_s2_agreement_data %>% 
  filter(is.na(Included_1.s2) & !is.na(Included_1.s1))
paste(nrow(only_s1), "entries were screened exclusively by Screener 1. Of these,",
      sum(only_s1$Included_1.s1), "were included, i.e.", round(sum(only_s1$Included_1.s1)/nrow(only_s1)*100,1), "%")
# Exclusively screened by Screener 2
only_s2 <- s1_s2_agreement_data %>% 
  filter(is.na(Included_1.s1) & !is.na(Included_1.s2))
paste(nrow(only_s2), "entries were screened exclusively by Screener 2. Of these,",
sum(only_s2$Included_1.s2), "were included, i.e.", round(sum(only_s2$Included_1.s2)/nrow(only_s2)*100,1), "%")

# Extracting relevant studies
relevant_entries <- merged_data %>% 
  filter(Included_1.s1 == 1 | Included_1.s2 == 1)
# Check that number of included in merged relevant entries matches number of included in original screener data
sum(relevant_entries$Included_1.s1, na.rm = T) == sum(s1_data$Included_1, na.rm = T)
sum(relevant_entries$Included_1.s2, na.rm = T) == sum(s2_data$Included_1, na.rm = T)
# Check that sum of number of prior included studies (17) and the numbers of papers inlcluded by one, or both screeners matches length of relevant_entries
number_of_prior_included <- nrow(merged_data %>% slice_head(n = 220) %>% filter(included.s1 == 1 & included.s2 == 1))
nrow(relevant_entries) == sum(nrow(included_both) + nrow(included_s1_excluded_s2) + nrow(included_s2_excluded_s1) + sum(only_s1$Included_1.s1) + sum(only_s2$Included_1.s2) + number_of_prior_included)
# Total number of relevant entries, including those from prior sample
nrow(relevant_entries)

# Adding a bibtex key

#' Creates a Bibtex key from a dataframe with publication information.
#'
#' This function takes three inputs, year, author and
#' title, and creates a Bibtex key for each row in the dataframe. The Bibtex key has
#' the format: name-of-first-author_year_first-word-of-title.
#'
#' @param year A vector with the publication years.
#' @param author A vector with the authors' names.
#' @param title A vector with the publication titles.
#' @return A vector with the Bibtex keys.
create_bibtex_key <- function(year, author, title) {
  # Split author names and select first name
  first_author <- strsplit(author, ",")[[1]][1]
  # Remove non-alphanumeric characters from first word of title and convert to lowercase
  first_word <- tolower(gsub("[^[:alnum:]]", "", strsplit(title, " ")[[1]][1]))
  # Combine components to create Bibtex key
  paste0(first_author, "_", year, "_", first_word)
}
relevant_entries$BibtexKey <- apply(relevant_entries, 1, function(row) {
  create_bibtex_key(row["PublicationYear"], row["Author"], row["title"])
})
# Test uniqueness of bibtex keys:
if (length(unique(relevant_entries$BibtexKey)) == length(relevant_entries$BibtexKey)) {
  message("The generated Bibtex-keys are unique.")
} else {
  warning("There are ", length(relevant_entries$BibtexKey) - length(unique(relevant_entries$BibtexKey)), " duplicate Bibtex-keys.")
  #View(as.data.frame(relevant_entries$BibtexKey[duplicated(relevant_entries$BibtexKey)]))
}

# Randomizing the order of the dataset
# Set the random seed
set.seed(364)
# Randomizing the dataframe
randomized_relevant_entries <- relevant_entries[sample(nrow(relevant_entries)),]

# Extract data for full text download
data_for_download <- randomized_relevant_entries %>% 
  select(record_id, Key, Author, title, PublicationYear, PublicationTitle, Issue, Volume, ItemType, abstract, DOI, Url, BibtexKey) %>% 
  mutate(most_recent_version_available = NA, not_available = NA, retracted = NA, duplicate = NA, notes_PDF_download = NA)

# Create packages for full text download
#' Save Dataframe to Multiple Excel Files
#' 
#' This function saves the entries of a dataframe into xlsx files with a maximum of 100 entries per file.
#' 
#' @param df The dataframe to be saved.
#' @param file_path The path and filename prefix for the output files.
#' 
#' @return None
#' 
#' @export
save_to_xlsx <- function(df, file_path) {
  
  # Get number of files to be saved
  n_files <- ceiling(nrow(df) / 100)
  
  # Split dataframe into chunks of 100 rows
  df_list <- split(df, rep(1:n_files, each=100, length.out=nrow(df)))
  
  # Save each chunk to a separate file
  for (i in seq_along(df_list)) {
    file_name <- paste0(file_path, "_", i, ".xlsx")
    write.xlsx(df_list[[i]], file_name, rowNames = FALSE)
  }
  
}
# save_to_xlsx(data_for_download, here("Merged_data/Packages_for_full_text_download/study_set"))

# Some entries in 'study_set_16.xlsx' got corrupted in the parsing process for unclear reasons. 
# This script extracts these entries again based on their "record_id", which was not lost.

lost_entries_record_id <- c(
  17110, 
  4567, 
  2421, 
  21943, 
  1297, 
  10384, 
  13684, 
  5042, 
  6329, 
  17717, 
  15303, 
  6647, 
  3460, 
  3427, 
  9411, 
  4052, 
  983, 
  11178, 
  1384, 
  20296, 
  11126, 
  1524, 
  15557, 
  365, 
  3072, 
  1418, 
  5020
)

lost_entries <- randomized_relevant_entries[randomized_relevant_entries$record_id %in% lost_entries_record_id,]
View(lost_entries)

# Extract data for full text download
data_for_download_lost <- lost_entries %>% 
  select(record_id, Key, Author, title, PublicationYear, PublicationTitle, Issue, Volume, ItemType, abstract, DOI, Url, BibtexKey) %>% 
  mutate(most_recent_version_available = NA, not_available = NA, retracted = NA, duplicate = NA, notes_PDF_download = NA)

# write_csv(data_for_download_lost, here("study_set_16_lost_entries.csv"))
# The csv file with lost entries ("study_set_16_lost_entries.csv") was then manually imported into Excel and saved as "study_set_16_lost_entries.xlsx"

