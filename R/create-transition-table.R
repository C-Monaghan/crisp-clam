create_transition_table <- function(year_to, year_from) {
  # Creates a transition frequency table between two specified years of cognitive status data.
  # Calculates row sums and total observations for transition analysis.
  # Arguments:
  #   - year_to: Target year for transitions (character or numeric)
  #   - year_from: Origin year for transitions (character or numeric)
  # Returns:
  #   - List containing: 
  #     - transition frequency table
  #     - row sums
  #     - total observations
  # 
  tbl <- table(
    table_data[[paste0("HRS_", year_to)]],
    table_data[[paste0("HRS_", year_from)]],
    dnn = c(year_to, year_from)
  )
  
  # Calculate row sums and total
  row_sums <- rowSums(tbl)
  total <- sum(row_sums)
  
  # Return as a list with both the table and summary stats
  list(
    table = tbl,
    row_sums = row_sums,
    total = total
  )
}
