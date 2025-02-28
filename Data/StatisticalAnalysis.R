# Load necessary libraries
library(dplyr)
library(tidyr)

# Step 1: Load datasets (adjust paths accordingly)
elements_df <- read.csv("elements_final.csv")
ref_df <- read.csv("RefNoOoLCategoryDocType.csv", sep=";")

# Merge datasets on 'RefNo'
merged_df <- merge(elements_df, ref_df, by = "RefNo")

# Remove rows with missing OoLCategory
merged_df <- merged_df %>% filter(!is.na(OoLCategory))

# Step 2: Split multi-category labels into separate binary columns
category_split <- merged_df %>%
  separate_rows(OoLCategory, sep = ", ") %>%  # Split categories into multiple rows
  mutate(value = 1) %>%
  pivot_wider(names_from = OoLCategory, values_from = value, values_fill = 0)

# Step 3: Perform Mann-Whitney U tests (Wilcoxon rank-sum test)
elements <- colnames(elements_df)[-1]  # Exclude 'RefNo'
categories <- setdiff(colnames(category_split), c("RefNo", "DocType", colnames(elements_df)[-1]))  # Only get OoLCategory labels

results <- list()

for (category in categories) {
  for (element in elements) {
    group1 <- category_split[[element]][category_split[[category]] == 1]  # Category present
    group0 <- category_split[[element]][category_split[[category]] == 0]  # Category absent
    
    # Ensure both groups have values and are not constant
    if (length(group1) > 1 & length(group0) > 1 & var(group1) > 0 & var(group0) > 0) {
      test_result <- wilcox.test(group1, group0, alternative = "two.sided", exact = FALSE)
      results <- append(results, list(data.frame(Category = category, Element = element, P_Value = test_result$p.value)))
    }
  }
}

# Step 4: Convert results list to a dataframe
results_df <- bind_rows(results)

# Step 5: Adjust p-values for multiple comparisons
results_df <- results_df %>%
  mutate(Adjusted_P_Value = p.adjust(P_Value, method = "BH"))  # Benjamini-Hochberg (FDR correction)

write.csv(results_df, "all_results.csv", row.names = FALSE)

# Step 6: Filter significant results based on adjusted p-value
significant_results <- results_df %>% filter(Adjusted_P_Value < 0.05)

# Display results
print(significant_results)

# Optional: Save to CSV
write.csv(significant_results, "significant_results.csv", row.names = FALSE)



### The same analysis, but excluding all review papers ###

# Merge datasets on 'RefNo'
merged_df_noreview <- merge(elements_df, ref_df, by = "RefNo")

# Remove rows with missing OoLCategory and filter out "Review" DocTypes
merged_df_noreview <- merged_df_noreview %>%
  filter(!is.na(OoLCategory) & DocType != "Review")  # ✅ Exclude "Review"

# Step 2: Split multi-category labels into separate binary columns
category_split_noreview <- merged_df_noreview %>%
  separate_rows(OoLCategory, sep = ", ") %>%  # Split categories into multiple rows
  mutate(value = 1) %>%
  pivot_wider(names_from = OoLCategory, values_from = value, values_fill = 0)

# Step 3: Perform Mann-Whitney U tests (Wilcoxon rank-sum test)
elements <- colnames(elements_df)[-1]  # Exclude 'RefNo'
categories <- setdiff(colnames(category_split_noreview), c("RefNo", "DocType", colnames(elements_df)[-1]))  # Only get OoLCategory labels

results_noreview <- list()

for (category in categories) {
  for (element in elements) {
    group1 <- category_split_noreview[[element]][category_split_noreview[[category]] == 1]  # Category present
    group0 <- category_split_noreview[[element]][category_split_noreview[[category]] == 0]  # Category absent
    
    # Ensure both groups have values and are not constant
    if (length(group1) > 1 & length(group0) > 1 & var(group1) > 0 & var(group0) > 0) {
      test_result <- wilcox.test(group1, group0, alternative = "two.sided", exact = FALSE)
      results_noreview <- append(results_noreview, list(data.frame(Category = category, Element = element, P_Value = test_result$p.value)))
    }
  }
}

# Step 4: Convert results list to a dataframe
results_dfs_noreview <- bind_rows(results_noreview)

# Step 5: Adjust p-values for multiple comparisons
results_dfs_noreview <- results_dfs_noreview %>%
  mutate(Adjusted_P_Value = p.adjust(P_Value, method = "BH"))  # ✅ Benjamini-Hochberg (FDR correction)

write.csv(results_dfs_noreview, "all_results_noreview.csv", row.names = FALSE)

# Step 6: Filter significant results based on adjusted p-value
significant_results_noreview <- results_dfs_noreview %>% filter(Adjusted_P_Value < 0.05)

# Display results
print(significant_results_noreview)

# Optional: Save to CSV
write.csv(significant_results_noreview, "significant_results_noreview.csv", row.names = FALSE)
