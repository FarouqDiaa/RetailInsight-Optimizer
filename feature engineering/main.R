suppressPackageStartupMessages({
  library(tidyverse)
})

data_dir <- "../data" # Relative path to data directory
output_dir <- "output" # Output directory in current working directory

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Create a features directory for the CSV files
features_dir <- file.path(output_dir, "features")
if (!dir.exists(features_dir)) {
  dir.create(features_dir, recursive = TRUE)
}

# Function to check if file exists with detailed messaging
check_file <- function(filepath) {
  cat("Checking for file:", filepath, "\n")
  if (file.exists(filepath)) {
    cat("File found\n")
    return(TRUE)
  } else {
    cat("File not found\n")
    return(FALSE)
  }
}

# Function to save a feature set to CSV
save_feature_set <- function(data, feature_columns, name) {
  # Create subset with ID column and feature columns
  if ("Customer ID" %in% colnames(data)) {
    id_col <- "Customer ID"
  } else {
    # If no ID column exists, create row numbers as IDs
    data$ID <- 1:nrow(data)
    id_col <- "ID"
  }
  
  # Add Segment and Cluster columns if they exist
  extra_cols <- c()
  if ("Segment" %in% colnames(data)) extra_cols <- c(extra_cols, "Segment")
  if ("Cluster" %in% colnames(data)) extra_cols <- c(extra_cols, "Cluster")
  
  # Create the subset
  feature_subset <- data[, c(id_col, extra_cols, feature_columns)]
  
  # Create file name
  file_name <- file.path(features_dir, paste0(name, "_features.csv"))
  
  # Save to CSV
  write.csv(feature_subset, file_name, row.names = FALSE)
  cat("Saved", name, "features to", file_name, "\n")
}

data_file <- file.path(data_dir, "data.csv")
if (check_file(data_file)) {
  data <- read.csv(data_file, stringsAsFactors = FALSE)
  cat("Successfully loaded", nrow(data), "rows and", ncol(data), "columns\n")
  
  cat("Actual column names in the file:\n")
  print(colnames(data))
  
  # Clean column names
  original_names <- colnames(data)
  clean_names <- gsub("\\.", " ", original_names)
  colnames(data) <- clean_names
  
  cat("\nColumn name mapping:\n")
  for (i in 1:length(original_names)) {
    if (original_names[i] != clean_names[i]) {
      cat(original_names[i], "->", clean_names[i], "\n")
    }
  }
  
  cat("\nPerforming basic customer segmentation...\n")
  
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols]
  
  numeric_data <- na.omit(numeric_data)
  
  scaled_data <- scale(numeric_data)
  
  set.seed(42)
  k <- 4
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  
  data$Cluster <- NA
  row_indices <- as.numeric(rownames(numeric_data))
  data$Cluster[row_indices] <- kmeans_result$cluster
  
  segment_names <- paste0("Segment ", 1:k)
  data$Segment <- NA
  data$Segment[row_indices] <- segment_names[kmeans_result$cluster]
  
  # Save the base segmented data
  segmented_file <- file.path(data_dir, "segmented_customers.csv")
  write.csv(data, segmented_file, row.names = FALSE)
  cat("Saved segmented data to", segmented_file, "\n")
  
  # ---------- Feature Engineering after clustering ----------
  
  cat("\nPerforming feature engineering on segmented data...\n")
  
  # Track features by category
  demographic_features <- c()
  value_features <- c()
  purchase_behavior_features <- c()
  category_features <- c()
  seasonal_features <- c()
  preference_features <- c()
  
  # Add Age to demographic features if it exists
  if ("Age" %in% colnames(data)) {
    demographic_features <- c(demographic_features, "Age")
  }
  
  # 1. Create value score based on available metrics
  if (all(c("Purchase Amount (USD)", "Previous Purchases") %in% colnames(data))) {
    cat("Creating Value Score feature...\n")
    
    # Normalize each component before combining
    max_purchase <- max(data$`Purchase Amount (USD)`, na.rm = TRUE)
    max_previous <- max(data$`Previous Purchases`, na.rm = TRUE)
    
    # Create value score as weighted combination
    data$Value_Score <- (
      (data$`Purchase Amount (USD)` / max_purchase) * 0.6 + 
        (data$`Previous Purchases` / max_previous) * 0.4
    )
    cat("Value Score feature created\n")
    
    # Add to value features
    value_features <- c(value_features, "Value_Score")
    
    # Also include the original columns
    value_features <- c(value_features, "Purchase Amount (USD)", "Previous Purchases")
  }
  
  # 2. Create category preferences (one-hot encoding)
  if ("Category" %in% colnames(data)) {
    cat("Creating category preference features...\n")
    categories <- unique(na.omit(data$Category))
    
    for (cat in categories) {
      col_name <- paste0("Prefers_", gsub(" ", "_", cat))
      data[[col_name]] <- ifelse(data$Category == cat, 1, 0)
      
      # Add to category features
      category_features <- c(category_features, col_name)
    }
    cat("Created", length(categories), "category preference features\n")
    
    # Add original category column
    category_features <- c(category_features, "Category")
  }
  
  # 3. Create age groups if Age column exists
  if ("Age" %in% colnames(data)) {
    cat("Creating Age Group feature...\n")
    data$Age_Group <- cut(data$Age, 
                          breaks = c(17, 25, 35, 50, 70, Inf),
                          labels = c("18-25", "26-35", "36-50", "51-70", ">70"),
                          right = TRUE)
    cat("Age Group feature created\n")
    
    # Add to demographic features
    demographic_features <- c(demographic_features, "Age_Group")
  }
  
  # Add Gender to demographic features if it exists
  if ("Gender" %in% colnames(data)) {
    demographic_features <- c(demographic_features, "Gender")
  }
  
  # 4. Create Purchase Frequency feature if frequency column exists
  if ("Frequency of Purchases" %in% colnames(data)) {
    cat("Creating numeric Purchase Frequency feature...\n")
    
    # Map textual frequency to numeric value (purchases per year)
    freq_mapping <- c(
      "Weekly" = 52,
      "Fortnightly" = 26,
      "Monthly" = 12, 
      "Quarterly" = 4,
      "Bi-Annually" = 2,
      "Annually" = 1,
      "Every 2 Years" = 0.5
    )
    
    data$Purchase_Frequency_Num <- NA
    for (freq in names(freq_mapping)) {
      data$Purchase_Frequency_Num[data$`Frequency of Purchases` == freq] <- freq_mapping[freq]
    }
    
    cat("Purchase Frequency feature created\n")
    
    # Add to purchase behavior features
    purchase_behavior_features <- c(purchase_behavior_features, "Purchase_Frequency_Num", "Frequency of Purchases")
  }
  
  # 5. Create Discount Sensitivity feature
  if (all(c("Discount Applied", "Promo Code Used") %in% colnames(data))) {
    cat("Creating Discount Sensitivity feature...\n")
    
    # Convert to numeric if needed
    if (!is.numeric(data$`Discount Applied`)) {
      data$`Discount Applied` <- ifelse(data$`Discount Applied` == "Yes", 1, 0)
    }
    
    if (!is.numeric(data$`Promo Code Used`)) {
      data$`Promo Code Used` <- ifelse(data$`Promo Code Used` == "Yes", 1, 0)
    }
    
    # Calculate discount sensitivity (0-2 scale)
    data$Discount_Sensitivity <- data$`Discount Applied` + data$`Promo Code Used`
    
    cat("Discount Sensitivity feature created\n")
    
    # Add to purchase behavior features
    purchase_behavior_features <- c(purchase_behavior_features, 
                                    "Discount_Sensitivity", "Discount Applied", "Promo Code Used")
  } else {
    # Add these columns to purchase behavior if they exist individually
    if ("Discount Applied" %in% colnames(data)) {
      purchase_behavior_features <- c(purchase_behavior_features, "Discount Applied")
    }
    if ("Promo Code Used" %in% colnames(data)) {
      purchase_behavior_features <- c(purchase_behavior_features, "Promo Code Used")
    }
  }
  
  # Add Subscription Status to purchase behavior if it exists
  if ("Subscription Status" %in% colnames(data)) {
    purchase_behavior_features <- c(purchase_behavior_features, "Subscription Status")
  }
  
  # 6. Extract season preference
  if ("Season" %in% colnames(data)) {
    cat("Creating season preference features...\n")
    seasons <- unique(na.omit(data$Season))
    
    for (season in seasons) {
      col_name <- paste0("Shops_", season)
      data[[col_name]] <- ifelse(data$Season == season, 1, 0)
      
      # Add to seasonal features
      seasonal_features <- c(seasonal_features, col_name)
    }
    cat("Created", length(seasons), "season preference features\n")
    
    # Add original season column
    seasonal_features <- c(seasonal_features, "Season")
  }
  
  # 7. Extract payment preferences
  if ("Payment Method" %in% colnames(data)) {
    cat("Creating payment preference features...\n")
    
    # Create Digital Payment flag
    digital_methods <- c("PayPal", "Venmo", "Apple Pay")
    data$Digital_Payment_Pref <- ifelse(data$`Payment Method` %in% digital_methods, 1, 0)
    
    cat("Payment preference features created\n")
    
    # Add to preference features
    preference_features <- c(preference_features, "Digital_Payment_Pref", "Payment Method")
  }
  
  # 8. Extract shipping preferences
  if ("Shipping Type" %in% colnames(data)) {
    cat("Creating shipping preference features...\n")
    
    # Create Fast Shipping flag
    fast_shipping <- c("Express", "Next Day Air")
    data$Fast_Shipping_Pref <- ifelse(data$`Shipping Type` %in% fast_shipping, 1, 0)
    
    cat("Shipping preference features created\n")
    
    # Add to preference features
    preference_features <- c(preference_features, "Fast_Shipping_Pref", "Shipping Type")
  }
  
  # 9. Create Size preference flags
  if ("Size" %in% colnames(data)) {
    cat("Creating size preference features...\n")
    
    data$Prefers_Small <- ifelse(data$Size %in% c("S", "XS"), 1, 0)
    data$Prefers_Large <- ifelse(data$Size %in% c("L", "XL"), 1, 0)
    
    cat("Size preference features created\n")
    
    # Add to preference features
    preference_features <- c(preference_features, "Prefers_Small", "Prefers_Large", "Size")
  }
  
  # Add Color to preference features if it exists
  if ("Color" %in% colnames(data)) {
    preference_features <- c(preference_features, "Color")
  }
  
  # Add Item Purchased to category features if it exists
  if ("Item Purchased" %in% colnames(data)) {
    category_features <- c(category_features, "Item Purchased")
  }
  
  # Add Review Rating to purchase behavior if it exists
  if ("Review Rating" %in% colnames(data)) {
    purchase_behavior_features <- c(purchase_behavior_features, "Review Rating")
  }
  
  # Add Location to demographic features if it exists
  if ("Location" %in% colnames(data)) {
    demographic_features <- c(demographic_features, "Location")
  }
  
  # Save each feature set to separate CSV files
  cat("\nSaving feature sets to separate CSV files...\n")
  
  # Save demographic features
  if (length(demographic_features) > 0) {
    save_feature_set(data, demographic_features, "demographic")
  }
  
  # Save value features
  if (length(value_features) > 0) {
    save_feature_set(data, value_features, "value")
  }
  
  # Save purchase behavior features
  if (length(purchase_behavior_features) > 0) {
    save_feature_set(data, purchase_behavior_features, "purchase_behavior")
  }
  
  # Save category features
  if (length(category_features) > 0) {
    save_feature_set(data, category_features, "category")
  }
  
  # Save seasonal features
  if (length(seasonal_features) > 0) {
    save_feature_set(data, seasonal_features, "seasonal")
  }
  
  # Save preference features
  if (length(preference_features) > 0) {
    save_feature_set(data, preference_features, "preference")
  }
  
  # Save all features together
  all_features_file <- file.path(features_dir, "all_features.csv")
  write.csv(data, all_features_file, row.names = FALSE)
  cat("Saved all features to", all_features_file, "\n")
  
  # ---------- Generate Segment Summary ----------
  
  cat("\nGenerating segment summary...\n")
  sink(file.path(output_dir, "segment_summary.txt"))
  
  cat("========================================================\n")
  cat("   RETAIL INSIGHT OPTIMIZER - CUSTOMER SEGMENTATION     \n")
  cat("========================================================\n\n")
  
  # Count customers in each segment
  segment_counts <- table(data$Segment, useNA = "ifany")
  cat("IDENTIFIED CUSTOMER SEGMENTS:\n")
  for (i in 1:length(segment_names)) {
    segment <- segment_names[i]
    count <- segment_counts[segment]
    if (is.na(count)) count <- 0
    percentage <- round(count / nrow(data) * 100, 1)
    cat(paste0("- ", segment, ": ", count, " customers (", percentage, "%)\n"))
  }
  
  # Generate enhanced segment profiles with new features
  cat("\nSEGMENT CHARACTERISTICS:\n")
  
  # Determine which new features exist for reporting
  new_features <- c()
  if ("Value_Score" %in% colnames(data)) new_features <- c(new_features, "Value_Score")
  if ("Purchase_Frequency_Num" %in% colnames(data)) new_features <- c(new_features, "Purchase_Frequency_Num")
  if ("Discount_Sensitivity" %in% colnames(data)) new_features <- c(new_features, "Discount_Sensitivity")
  if ("Digital_Payment_Pref" %in% colnames(data)) new_features <- c(new_features, "Digital_Payment_Pref")
  if ("Fast_Shipping_Pref" %in% colnames(data)) new_features <- c(new_features, "Fast_Shipping_Pref")
  
  # Create a dynamic summarise expression based on available columns
  segment_data <- data %>%
    filter(!is.na(Segment)) %>%
    group_by(Segment) %>%
    summarise(
      Count = n(),
      Pct = round(n() / sum(!is.na(data$Segment)) * 100, 1),
      
      # Basic demographics
      Avg_Age = if ("Age" %in% colnames(data)) round(mean(Age, na.rm = TRUE), 1) else NA,
      
      # Original metrics
      Avg_Previous = if ("Previous Purchases" %in% colnames(data)) 
        round(mean(`Previous Purchases`, na.rm = TRUE), 1) else NA,
      
      Discount_Rate = if ("Discount Applied" %in% colnames(data)) {
        if (is.numeric(data$`Discount Applied`)) {
          round(mean(`Discount Applied`, na.rm = TRUE) * 100, 1)
        } else {
          round(mean(`Discount Applied` == "Yes", na.rm = TRUE) * 100, 1)
        }
      } else NA,
      
      Subscription_Rate = if ("Subscription Status" %in% colnames(data)) {
        if (is.numeric(data$`Subscription Status`)) {
          round(mean(`Subscription Status`, na.rm = TRUE) * 100, 1)
        } else {
          round(mean(`Subscription Status` == "Yes", na.rm = TRUE) * 100, 1)
        }
      } else NA,
      
      # New metrics from feature engineering
      Value_Score = if ("Value_Score" %in% colnames(data)) 
        round(mean(Value_Score, na.rm = TRUE), 3) else NA,
      
      Purchase_Frequency = if ("Purchase_Frequency_Num" %in% colnames(data)) 
        round(mean(Purchase_Frequency_Num, na.rm = TRUE), 1) else NA,
      
      Discount_Sensitivity = if ("Discount_Sensitivity" %in% colnames(data))
        round(mean(Discount_Sensitivity, na.rm = TRUE), 2) else NA,
      
      Digital_Payment = if ("Digital_Payment_Pref" %in% colnames(data))
        round(mean(Digital_Payment_Pref, na.rm = TRUE) * 100, 1) else NA,
      
      Fast_Shipping = if ("Fast_Shipping_Pref" %in% colnames(data))
        round(mean(Fast_Shipping_Pref, na.rm = TRUE) * 100, 1) else NA
    )
  
  print(segment_data)
  
  # Add category preferences analysis if available
  if (any(grepl("^Prefers_", colnames(data)))) {
    cat("\nCATEGORY PREFERENCES BY SEGMENT:\n")
    
    # Get all category preference columns
    category_cols <- colnames(data)[grepl("^Prefers_", colnames(data))]
    
    # Create category preference summary
    category_prefs <- data %>%
      filter(!is.na(Segment)) %>%
      group_by(Segment) %>%
      summarise(across(all_of(category_cols), ~round(mean(.x, na.rm = TRUE) * 100, 1)))
    
    # Clean column names for display
    colnames(category_prefs) <- gsub("Prefers_", "", colnames(category_prefs))
    colnames(category_prefs) <- gsub("_", " ", colnames(category_prefs))
    
    print(category_prefs)
  }
  
  # Add season preferences analysis if available
  if (any(grepl("^Shops_", colnames(data)))) {
    cat("\nSEASONAL PREFERENCES BY SEGMENT:\n")
    
    # Get all season preference columns
    season_cols <- colnames(data)[grepl("^Shops_", colnames(data))]
    
    # Create season preference summary
    season_prefs <- data %>%
      filter(!is.na(Segment)) %>%
      group_by(Segment) %>%
      summarise(across(all_of(season_cols), ~round(mean(.x, na.rm = TRUE) * 100, 1)))
    
    # Clean column names for display
    colnames(season_prefs) <- gsub("Shops_", "", colnames(season_prefs))
    
    print(season_prefs)
  }
  
  # Add size preferences analysis if available
  if (all(c("Prefers_Small", "Prefers_Large") %in% colnames(data))) {
    cat("\nSIZE PREFERENCES BY SEGMENT:\n")
    
    size_prefs <- data %>%
      filter(!is.na(Segment)) %>%
      group_by(Segment) %>%
      summarise(
        Small = round(mean(Prefers_Small, na.rm = TRUE) * 100, 1),
        Medium = round((1 - mean(Prefers_Small, na.rm = TRUE) - mean(Prefers_Large, na.rm = TRUE)) * 100, 1),
        Large = round(mean(Prefers_Large, na.rm = TRUE) * 100, 1)
      )
    
    print(size_prefs)
  }
  
  sink()
  
  # ---------- Create Enhanced Visualizations ----------
  
  if (require(ggplot2)) {
    cat("\nCreating segment visualizations...\n")
    
    # Create a bar chart of segment sizes
    segment_bar <- ggplot(data %>% filter(!is.na(Segment)), aes(x = Segment, fill = Segment)) +
      geom_bar() +
      labs(title = "Customer Segments Size Distribution", 
           x = "Segment", 
           y = "Number of Customers") +
      theme_minimal()
    
    # Save the visualization
    ggsave(file.path(output_dir, "segment_distribution.png"), segment_bar, width = 8, height = 6)
    cat("Saved segment distribution chart\n")
    
    # Create age distribution by segment if Age column exists
    if ("Age" %in% colnames(data)) {
      age_box <- ggplot(data %>% filter(!is.na(Segment)), aes(x = Segment, y = Age, fill = Segment)) +
        geom_boxplot() +
        labs(title = "Age Distribution by Segment", 
             x = "Segment", 
             y = "Age") +
        theme_minimal()
      
      ggsave(file.path(output_dir, "age_distribution.png"), age_box, width = 8, height = 6)
      cat("Saved age distribution chart\n")
    }
    
    # Create Value Score visualization if available
    if ("Value_Score" %in% colnames(data)) {
      value_box <- ggplot(data %>% filter(!is.na(Segment)), 
                          aes(x = Segment, y = Value_Score, fill = Segment)) +
        geom_boxplot() +
        labs(title = "Customer Value Score by Segment", 
             x = "Segment", 
             y = "Value Score") +
        theme_minimal()
      
      ggsave(file.path(output_dir, "value_score_distribution.png"), value_box, width = 8, height = 6)
      cat("Saved value score distribution chart\n")
    }
    
    # Create Purchase Frequency visualization if available
    if ("Purchase_Frequency_Num" %in% colnames(data)) {
      freq_box <- ggplot(data %>% filter(!is.na(Segment)), 
                         aes(x = Segment, y = Purchase_Frequency_Num, fill = Segment)) +
        geom_boxplot() +
        labs(title = "Purchase Frequency by Segment", 
             x = "Segment", 
             y = "Purchases per Year") +
        theme_minimal()
      
      ggsave(file.path(output_dir, "purchase_frequency.png"), freq_box, width = 8, height = 6)
      cat("Saved purchase frequency chart\n")
    }
    
    # Create Category Preferences visualization if available
    if (any(grepl("^Prefers_", colnames(data)))) {
      # Get all category preference columns
      category_cols <- colnames(data)[grepl("^Prefers_", colnames(data))]
      
      # Prepare data for visualization
      category_data <- data %>%
        filter(!is.na(Segment)) %>%
        group_by(Segment) %>%
        summarise(across(all_of(category_cols), ~mean(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = -Segment, names_to = "Category", values_to = "Preference")
      
      # Clean category names for display
      category_data$Category <- gsub("Prefers_", "", category_data$Category)
      category_data$Category <- gsub("_", " ", category_data$Category)
      
      # Create the visualization
      cat_plot <- ggplot(category_data, aes(x = Category, y = Preference, fill = Segment)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Category Preferences by Segment", 
             x = "Category", 
             y = "Preference Rate") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(output_dir, "category_preferences.png"), cat_plot, width = 10, height = 6)
      cat("Saved category preferences chart\n")
    }
    
  } else {
    cat("ggplot2 package not available. Skipping visualizations.\n")
  }
  
  cat("\nCustomer Segmentation Analysis Complete!\n")
  cat("Results and visualizations saved to '", output_dir, "' directory.\n")
  cat("Segment summary available in '", file.path(output_dir, "segment_summary.txt"), "'.\n")
  cat("Feature CSV files saved to '", features_dir, "' directory.\n")
  cat("========================================================\n")
  
} else {
  cat("Data file not found. Please check the file path and try again.\n")
}