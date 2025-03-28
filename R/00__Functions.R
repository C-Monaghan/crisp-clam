# A list of functions used throughout analysis
# Used in 02__Aggregation.R ----------------------------------------------------

process_health_data <- function(health_data, prefix) {
  health_data |>
    select(HHID, PN, 
           Weight = paste0(prefix, "139"),
           H_feet = paste0(prefix, "141"),
           H_inches = paste0(prefix, "142"),
           Hypertension = paste0(prefix, "005"),
           Stroke = paste0(prefix, "053"),
           H_problem = paste0(prefix, "036")) |>
    mutate(across(Hypertension:H_problem, 
                  ~ ifelse(. %in% c(4, 5, 6), 0, 1))) 
}

height_weight <- function(data) {
  data |>
    mutate(H_inches = round(H_inches, digits = 0)) |>
    transform(Height = interaction(H_feet, H_inches)) |>
    mutate(
      Height = as.numeric(as.character(Height)),
      Height = round((Height * 0.3048), digits = 2),
      Weight = round((Weight * 0.453592), digits = 2),
    ) |>
    select(!c(H_feet, H_inches)) |>
    relocate(Height, .before = Weight)
}

impute_height <- function(data) {
  data |>
    select(HHID, PN, starts_with("Height")) |>
    mutate(ID = row_number()) |>
    pivot_longer(cols = starts_with("Height"), names_to = "Year", values_to = "Height") |>
    group_by(ID) |>
    fill(Height, .direction = "downup") |>
    ungroup() |>
    pivot_wider(names_from = "Year", values_from = "Height") |>
    select(-ID)
}

process_cognition_data <- function(cog_data, prefix, focus_total = FALSE){
  cog_data <- cog_data |>
    select(HHID, PN, any_of(paste0(prefix, 110:117))) |>
    mutate(
      across(starts_with(prefix), ~ case_when(
        .x == 1 ~ 1,
        .x == 5 ~ 0,
        TRUE ~ NA_real_
      )),
      
      # Invert values for specific columns
      across(all_of(paste0(prefix, c(113, 115))), ~ ifelse(.x == 1, 0, 1)),
      
      # Calculate the total
      Total_dep = rowSums(across(starts_with(prefix)), na.rm = TRUE)
    )
  
  if(focus_total == TRUE) {
    cog_data <- cog_data |>
      select(HHID, PN, Total_dep)
  }
  
  return(cog_data)
}

# Used in both 04a__Markov_Transitions.R & 04b__Markov_Model.R -----------------
count_transitions <- function(data, years, absorbing = FALSE) {
  # Counts the occurrences of cognitive status transitions across specified 
  # years.
  # Converts numeric cognitive status codes (1, 2, 3) into descriptive labels
  # ("Normal Cognition", "MCI", "Dementia") and handles missing or unexpected values.
  # Aggregates the data to count transitions and reshapes it into a long format for analysis.
  # Arguments:
  #    - data: The input dataset containing cognitive function data.
  #    - years: A vector of years for which transitions should be counted.
  # Returns:
  #   - A dataset with counts of cognitive status transitions, including
  #   "Missing" and "Other" categories.
  
  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  
  data <- data |>
    select(ID, all_of(cogfunction_cols)) |>
    na.omit() |>
    tidyr::pivot_longer(
      cols = !ID,
      names_to = "Wave",
      values_to = "Classification"
    ) |>
    mutate(
      Wave = stringr::str_replace(Wave, "cogfunction", "HRS "),
      Wave = factor(Wave, levels = c(paste0("HRS ", years))),
      
      Classification = case_when(
        Classification == 1 ~ "Normal Cognition",
        Classification == 2 ~ "MCI",
        Classification == 3 ~ "Dementia")
    )
  
  if(absorbing == TRUE){
    data <- data |>
      group_by(ID) |>
      mutate(
        Classification = ifelse(
          cumany(Classification == "Dementia"), "Dementia", Classification)) |>
      ungroup()
  }
  
  data <- data |>
    mutate(Classification = factor(
      Classification,
      levels = c("Normal Cognition", "MCI", "Dementia")))
  
  return(data)
}

plot_cognitive_scores <- function(data, year) {
  # Plots the distribution of cognitive scores over a range of years.
  # Converts the data from wide to long format and generates histograms for each wave.
  # Arguments:
  #   - data: The dataset containing cognitive scores.
  #   - year: The starting year for the range of cognitive scores to plot.
  # Returns:
  #   - A ggplot object showing the distribution of cognitive scores across waves.
  # Plotting dementia transitions
  data |>
    select(ID, any_of(paste0("cogtot27_imp", year:2022))) |>
    na.omit() |>
    tidyr::pivot_longer(cols = !ID,
                        names_to = "Wave",
                        values_to = "Score") |>
    mutate(Wave = as.double(stringr::str_replace(Wave, "cogtot27_imp", ""))) |>
    ggplot(aes(x = Score)) +
    geom_histogram(fill = "skyblue",
                   colour = "black",
                   alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 27, by = 3)) +
    labs(
      title = paste0("Distribution of cognitive scores ", year, "- 2022"),
      x = "Cognitive Score", y = "") +
    facet_wrap( ~ Wave) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 12,
        face = "bold",
        colour = "#2E2E2E"
      ),
      strip.text = element_text(
        size = 10,
        face = "bold",
        colour = "#2E2E2E"
      ),
      panel.grid = element_blank()
    )
}

plot_transitions <- function(data, size) {
  # Visualizes cognitive status transitions over time using an alluvial plot.
  # Represents the flow of individuals between cognitive states across waves.
  # Arguments:
  #   - data: The dataset containing transition data.
  #   - size: The font size for labels in the plot.
  # Returns:
  #   - A ggplot object showing transitions between cognitive states over time.
  
  require(ggalluvial)
  
  data |>
    ggplot(aes(x = Wave, y = n, stratum = Classification, 
               fill = Classification, alluvium = ID
    )) +
    geom_stratum(alpha = 0.5, width = 0.5) +
    geom_flow(width = 0.5) +
    geom_text(
      stat = "stratum",
      aes(label = stringr::str_wrap(Classification, width = 10)),
      hjust = 0.5,
      vjust = 0.5,
      size = size
    ) +
    labs(title = "Dementia transitions across time", 
         x = "", y = "Frequency") +
    scale_fill_viridis_d(direction = -1) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      text = element_text(size = 10)
    ) +
    ggeasy::easy_remove_legend()
}

extract_years <- function(data, years, impute = TRUE, cog_total = FALSE, absorbing = TRUE) {
  # Extracts cognitive function data for specified years from a dataset.
  # Converts numeric cognitive status codes (1, 2, 3) into descriptive labels
  # ("Normal Cognition", "MCI", "Dementia") for easier interpretation.
  # Arguments:
  #   - data: The input dataset containing cognitive function data.
  #   - years: A vector of years for which data should be extracted.
  # Returns:
  #   - A dataset with ID and cognitive status columns for the specified years.
  
  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  cogtotal_cols    <- paste0("cogtot27_imp", years)
  
  if(cog_total == FALSE) {
  data <- data |>
    # Select only the ID column and cognitive function columns for the specified years
    select(ID, any_of(cogfunction_cols)) |>
    mutate(across(!c(ID), ~ case_when(
      .x == 1 ~ "Normal Cognition",
      .x == 2 ~ "MCI",
      .x == 3 ~ "Dementia",
      TRUE ~ NA_character_  # To handle missing/other cases
    )))
  } else {
    data <- data |>
      # Select only the ID column and cognitive function columns for the specified years
      select(ID, any_of(cogtotal_cols)) |>
      rename_with( .cols = !ID, .fn = ~ stringr::str_replace(
        string = .x,
        pattern = "cogtot27_imp", 
        replacement = "cog_score_"))
  }
  
  if(impute == TRUE){
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status") |>
      group_by(ID) |>
      tidyr::fill(Status, .direction = "down") |>
      ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }
  
  if(absorbing == TRUE) {
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status") |>
      group_by(ID) |>
      mutate(Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status)) |>
      ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }
  
  return(data)
}

create_transitions <- function(data, absorbing = FALSE){
  # Reshapes data from wide to long format to track cognitive status transitions over time.
  # Calculates the next wave's cognitive status for each individual and creates a transition column.
  # Optionally treats "Dementia" as an absorbing state, meaning once an individual is classified
  # with dementia, their status cannot change in subsequent waves.
  # Arguments:
  #   - data: The dataset containing cognitive status data.
  #   - absorbing: A logical flag indicating whether "Dementia" should be treated 
  #   as an absorbing state.
  # Returns:
  #   - A dataset with transition information, including current and next wave statuses.
  
  # Reshape the data from wide to long format to track cognitive status over waves
  data <- data |>
    select(ID, starts_with("cogfunction")) |>
    tidyr::pivot_longer(cols = !ID,
                        names_to = "Wave",
                        values_to = "Status") |>
    mutate(Wave = as.factor(stringr::str_replace(Wave, "cogfunction", ""))) |>
    # Arrange by ID and Wave to prepare for transition calculation
    arrange(ID, Wave) |>
    group_by(ID) |>
    # Get the next wave's cognitive status for each person
    mutate(next_wave_status = lead(Status)) |>
    ungroup()
  
  # We can optionally specify dementia as an absorbing state
  # Once an individual is classified with dementia they cannot be classified
  # with anything else 
  if(absorbing == TRUE) {
    data <- data |>
      group_by(ID) |>
      mutate(
        Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status),
        next_wave_status = ifelse(cumany(Status == "Dementia"), "Dementia", next_wave_status),
        transition = paste(Status, next_wave_status, sep = " to ")
      ) |>
      filter(Wave %in% c(2016, 2018, 2020))
  }
  
  # Filter out rows where either the current or next status is missing
  data <- data |>
    group_by(ID) |>
    filter(!is.na(Status), !is.na(next_wave_status)) |>
    ungroup() |>
    # Create a new column representing the transition from one status to the next
    mutate(
      transition = paste(Status, next_wave_status, sep = " to "),
      transition = factor(
        transition, 
        levels = c("Normal Cognition to Normal Cognition", "Normal Cognition to MCI",
                   "Normal Cognition to Dementia", "MCI to Normal Cognition", "MCI to MCI",
                   "MCI to Dementia", "Dementia to Dementia") 
        ))
  
  return(data)
}

calculate_probabilties <- function(data) {
  # Calculates the proportion of each cognitive status transition in the dataset.
  # Counts the occurrences of each transition and computes their relative probabilities.
  # Arguments:
  #   - data: The dataset containing transition information.
  # Returns:
  #   - A dataset with transition probabilities, split into "from" and "to" states.
  
  # Calculate the proportion of each transition by dividing by the total count
  data |>
    count(transition) |>
    mutate(prop = n / sum(n)) |>
    tidyr::separate(transition, into = c("from", "to"), sep = " to ")
}

transition_matrix <- function(data, longitudinal = FALSE) {
  # Constructs a transition matrix from the calculated transition probabilities.
  # The matrix represents the probability of moving from one cognitive state to another.
  # Arguments:
  #   - data: The dataset containing transition probabilities.
  #   - longitudinal: A TRUE/FALSE statement indicating if the probabilities are from multiple waves
  # Returns:
  #   - A transition matrix with rows representing "from" states and columns 
  #   representing "to" states.
  
  if(longitudinal == FALSE) {
    # Defining empty matrix matrix
    states <- c("Normal Cognition", "MCI", "Dementia")
    
    transition_matrix <- matrix(
      0,
      nrow = length(states),
      ncol = length(states),
      dimnames = list(from_state = states, to_state = states))
    
    # Fill the transition matrix with probabilities
    for (i in 1:nrow(data)) {
      from <- data$from[i]
      to <- data$to[i]
      prob <- data$prop[i]
      
      transition_matrix[from, to] <- prob
    }
  } else if(longitudinal == TRUE){
    
    # Aggregate by 'from' and 'to' states to sum transition probabilities
    data_agg <- data |>
      group_by(from, to) |>
      summarise(transition_prob = sum(prop), .groups = "drop")
    
    transition_matrix <- data_agg |>
      tidyr::pivot_wider(names_from = to, values_from = transition_prob, values_fill = 0) |>
      tibble::column_to_rownames("from") |>
      as.matrix()
  }
  
  return(transition_matrix)
}

plot_matrix <- function(data, year, ts = FALSE) {
  # Visualizes the transition matrix as a heatmap.
  # Uses color gradients to represent transition probabilities and includes labels for clarity.
  # Arguments:
  #   - data: The transition matrix or dataset to plot.
  #   - year: The year or time period associated with the data.
  #   - ts: A logical flag indicating whether the title should include a time series label.
  # Returns:
  #   - A ggplot object representing the transition probabilities as a heatmap.
  
  if(ts == TRUE) {
    title <- paste0("Transition Probabilities Across Cognitive States - ", year)
  } else {
    title <- paste0("Transition Probabilities Across Cognitive States (", year, "-2022)")
  }
  
  # Reshaping data for plotting
  data <- data |>
    as.data.frame(row.names = FALSE) |>
    mutate(from_state = c("Normal Cognition", "MCI", "Dementia")) |>
    reshape2::melt(id.vars = "from_state", variable.name = "to_state", value.name = "probability")
  
  # Plotting probabilities
  data |>
    ggplot(aes(x = from_state, y = to_state, fill = probability)) +
    geom_tile() +
    scale_fill_viridis_c(alpha = 0.8) +
    labs(title = title,
         x = "To", y = "From", fill = "Prob") +
    geom_text(aes(label = round(probability, digits = 3)), size = 5) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          panel.grid = element_blank()
    )
}

normalise <- function(x) {
  # Normalizes a matrix so that each row sums to 1.
  # Arguments:
  #   - x: The input matrix to normalize.
  # Returns:
  #   - A normalized matrix where each row sums to 1.
  x / rowSums(x)
}

reshape_matrix <- function(matrix, wave) {
  # Reshapes a transition matrix into a long format for easier analysis and visualization.
  # Adds a "Wave" column to identify the time period associated with the matrix.
  # Arguments:
  #   - matrix: The transition matrix to reshape.
  #   - wave: The wave or time period associated with the matrix.
  # Returns:
  #   - A long-format dataset with "from", "to", and "transition_prob" columns.
  
  matrix |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "from") |>
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "transition_prob") |>
    mutate(Wave = wave)
}

plot_transition_wave <- function(data, wave) {
  # Plots transition probabilities for a specific wave as a heatmap.
  # Filters the data to include only the main transitions and adds labels for clarity.
  # Arguments:
  #   - data: The dataset containing transition probabilities.
  #   - wave: The wave or time period to plot.
  # Returns:
  #   - A ggplot object representing the transition probabilities for the 
  #   specified wave.
  
  # Main transitions per wave
  filter_data <- data.frame(
    from = c("Normal Cognition", "Normal Cognition", "Normal Cognition",  "MCI", "MCI", "MCI", "Dementia", "Dementia", "Dementia"),
    to = c("Normal Cognition", "MCI", "Dementia", "Normal Cognition", "MCI", "Dementia", "Normal Cognition", "MCI", "Dementia")
  )
  
  data |>
    semi_join(filter_data, by = c("from", "to")) |>
    filter(Wave == wave) |>
    ggplot(aes(x = to, y = from, fill = transition_prob)) +
    geom_tile() +
    geom_text(aes(label = transition_prob), size = 5) +
    scale_fill_viridis_c(alpha = 0.8)+
    labs(title = paste0("Transition Probabilities Across Cognitive States (", wave, "-", wave + 2, ")"),
         x = "To", y = "From", fill = "Prob") +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      panel.grid = element_blank(),
      panel.border = element_blank()
    )
}

backtrack_age <- function(data, base_year, current_year) {
  # Back calculates an individual's age by 2-year intervals
  # Arguments:
  #   - data: The dataset containing the age variable and a 'Wave' column indicating the year of the wave
  #   - current_year: The year for which the age is measured (e.g., 2022)
  # Returns:
  #   - A dataset where the age is subtracted by intervals of 2 for each respective year
  
  # Map Wave values (1-10) to their corresponding years
  wave_to_year <- function(wave) {
    base_year + (as.numeric(wave) - 1) * 2
  }
  
  data |>
    group_by(ID) |>
    mutate(Age = Age - (current_year - wave_to_year(Wave))) |>
    ungroup()
}

pivot_data <- function(data, cog_total = FALSE) {
  # Reshapes the input dataset by pivoting cardiovascular, depression, and cognitive data
  # into long format for analysis. Handles two scenarios: one for cognitive status and
  # another for total cognitive scores. Combines the pivoted data into a final dataset
  # for further analysis.
  # Arguments:
  #   - data: The input dataset containing cardiovascular, depression, and cognitive data.
  #   - cog_total: A logical flag indicating whether to pivot total cognitive scores (TRUE)
  #               or cognitive status (FALSE).
  # Returns:
  #   - A reshaped dataset in long format with combined cardiovascular, depression, and
  #     cognitive data, ready for analysis.

  # Pivot cardio dataset
  data_cardio <- data |>
    tidyr::pivot_longer(
      cols = starts_with("Cardio_risk"),
      names_to = "Cardio_Year",
      values_to = "Cardio_risk"
    ) |>
    select(!any_of(c(
      "Cardio_Year", 
      paste0("cogfunction", 2016:2022), 
      paste0("cog_score_", 2016:2022)
      )))
  
  # Pivot dementia dataset
  data_dep <- data |>
    tidyr::pivot_longer(
      cols = starts_with("Total_dep"),
      names_to = "Dep_Year",
      values_to = "Depression_score"
    ) |>
    select(ID, Depression_score)
           
  if(cog_total == FALSE) {
    
    # Pivot cognition dataset
    data_cog <- data |>
      tidyr::pivot_longer(
        cols = starts_with("cogfunction"),
        names_to = "Wave",
        values_to = "Status"
      ) |>
      select(!any_of(c(
        paste0("Cardio_risk_", 16:22), 
        paste0("Total_dep_20", 16:22))
        ))
  
  # Adding cardio data to cognition data
  combine_data <- data_cog |>
    mutate(Cardio_risk = data_cardio$Cardio_risk, .after = Education_tri) |>
    mutate(Depression = data_dep$Depression_score, .after = Cardio_risk)
  
  # Producing final dataset
  final_data <- combine_data |>
    mutate(
      Wave = stringr::str_replace(Wave, "cogfunction", "HRS_"),
      Wave = factor(
        Wave,
        levels = c("HRS_2016", "HRS_2018", "HRS_2020", "HRS_2022"),
        labels = c(1, 2, 3, 4)
      )) |>
    group_by(ID) |>
    mutate(
      Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status),
      Status = factor(
        Status,
        levels = c("Normal Cognition", "MCI", "Dementia"),
        labels = c(1, 2, 3)
      )) |>
    ungroup() |>
    mutate(
      Gender = factor(Gender, levels = c(0, 1)),
      Education_tri = factor(Education_tri, levels = c(0, 1, 2)),
      Wave = as.numeric(Wave),
      Status = as.numeric(Status)
    )
  } else {
    
    # Pivot cog total data
    data_score <- data |>
      tidyr::pivot_longer(
        cols = starts_with("cog_score"),
        names_to = "Wave",
        values_to = "Score"
      ) |>
      mutate(
        Wave = stringr::str_replace(Wave, "cog_score_", ""),
        Wave = factor(
          Wave, 
          levels = c("2016", "2018", "2020", "2022"),
          labels = c(1, 2, 3, 4)),
        
        Gender = as.factor(Gender),
        Education_tri = as.factor(Education_tri)
      ) |>
      select(!any_of(c(
        paste0("Cardio_risk_", 16:22), 
        paste0("Total_dep_20", 16:22))
      ))
    
    # Combine cardio and cog total data
    final_data <- data_score |>
      mutate(Cardio_risk = data_cardio$Cardio_risk, .after = Education_tri) |>
      mutate(Depression = data_dep$Depression_score, .after = Cardio_risk)
    }
  
  return(final_data)
}

gentleman_test <- function(data, model) {
  # Compares observed and expected prevalence of cognitive states (Normal Cognition, MCI, Dementia)
  # over time. Reshapes the observed and expected data into a long format and generates a
  # faceted line plot to visualize the comparison.
  # Arguments:
  #   - data: A list containing observed and expected prevalence data.
  #   - model: A string representing the model name, used in the plot title.
  # Returns:
  #   - A ggplot object displaying the observed vs. expected prevalence of cognitive states
  #     over time.

  # Reshaping observed prevalence
  do1 <- as_tibble(row.names(data$Observed)) |>
    rename(time = value) |>
    mutate(time = as.numeric(time))
  
  do2 <- as_tibble(data$Observed) |> 
    mutate(type = "observed")
  
  do <- cbind(do1, do2) |>
    select(-Total) |>
    tidyr::gather(state, number, -c(time, type)) |>
    mutate(state = case_when(state == "State 1" ~ "Normal Cognition",
                             state == "State 2" ~ "MCI",
                             state == "State 3" ~ "Dementia"))
  
  # Reshaping expected prevalence
  de1 <- as_tibble(row.names(data$Expected)) |>
    rename(time = value) |>
    mutate(time = as.numeric(time))
  
  de2 <- as_tibble(data$Expected) |>
    mutate(type = "expected")
  
  de <- cbind(de1,de2) |>
    select(-Total) |>
    tidyr::gather(state, number, -c(time, type))
  
  # Turn into single data frame and plotting
  rbind(do, de) |>
    mutate(type = factor(type),
           state = factor(state, levels = c("Normal Cognition", "MCI", "Dementia")),
           time = round(time,3)) |>
    group_by(state) |>
    ggplot(aes(x = time, y = number, colour = type)) +
    geom_line(linewidth = 1) +
    labs(title = paste0("Observed vs Fitted Values: ", model), x = "Time", y = "") +
    facet_wrap(~ state) +
    theme_bw() +
    ggeasy::easy_remove_legend_title()
}

process_hr <- function(data, fix_upper = TRUE) {
  # Processes hazard ratio (HR) data by cleaning and reshaping it for visualization.
  # Converts covariate names into descriptive labels and ensures proper formatting
  # of transitions and confidence intervals. Optionally fixes upper confidence intervals
  # that exceed a specified threshold.
  # Arguments:
  #   - data: The input dataset containing hazard ratio data.
  #   - fix_upper: A logical flag indicating whether to fix upper confidence intervals
  #                that exceed a threshold (default: TRUE).
  # Returns:
  #   - A cleaned and reshaped dataset ready for hazard ratio visualization.
  
  data <- do.call(rbind.data.frame, data) |>
    tibble::rownames_to_column(var = "Covariate") |>
    tidyr::separate_wider_delim(cols = Covariate, 
                                delim = ".", 
                                names = c("Covariate", "Transition")) |>
    mutate(Covariate = case_when(
      Covariate == "Gender1" ~ "Female",
      Covariate == "Education_tri1" ~ "High School",
      Covariate == "Education_tri2" ~ "College",
      Covariate == "Cardio_risk" ~ "Cardiovascular Risk",
      Covariate == "Depression" ~ "Depression",
      Covariate == "Total_p" ~ "Total Procrastination",
      Covariate == "Depression:Total_p" ~ "Depression x Procrastination",
      TRUE ~ Covariate),
      
      Covariate = factor(
        Covariate, 
        levels = c("Female", "Age", "High School", "College", 
                   "Cardiovascular Risk", "Depression", 
                   "Total Procrastination", "Depression x Procrastination"))
      ) |>
    mutate(Transition = factor(
      Transition, 
      levels = c("Normal Cognition - MCI", 
                 "Normal Cognition - Dementia",
                 "MCI - Normal Cognition",
                 "MCI - Dementia")))
  
  if(fix_upper == TRUE){
    data <- data |>
      mutate(U = case_when(
        U >= 5 & Transition == "Normal Cognition - Dementia" ~ HR,
        # U >= 5 & HR < 1 ~ 1,
        TRUE ~ U
      ))
  }
  
  return(data)
}

calculate_p_value <- function(data) {
  # Calculates p-values for hazard ratios (HR) based on log-transformed HR values and
  # their confidence intervals. Adds the calculated p-values to the input dataset.
  # Arguments:
  #   - data: The input dataset containing hazard ratios and confidence intervals.
  # Returns:
  #   - The input dataset with an additional column for p-values
  
  log_HR <- log(data$HR)
  log_lower_CI <- log(data$L)
  log_upper_CI <- log(data$U)
  
  SE <- (log_upper_CI - log_lower_CI) / (2 * 1.96)
  Z <- log_HR / SE
  p_value <- 2 * (1 - pnorm(abs(Z)))
  
  data$pval <- p_value |> round(digits = 3)
  
  return(data)
}

plot_hr <- function(data) {
  # Generates a forest plot of hazard ratios (HR) for cognitive transitions, including
  # confidence intervals and significance stars. Customizes the plot based on covariate
  # labels and ensures proper formatting for visualization.
  # Arguments:
  #   - data: The input dataset containing hazard ratios, confidence intervals, and p-values.
  # Returns:
  #   - A ggplot object displaying hazard ratios for cognitive transitions, faceted by covariate.
  
  # Making a plot customization
  labeller <- c(
    Female = "Being female vs. Male",
    `High School` = "Having a highshool education vs. No education",
    College = "Having a further education vs. No education",
    `Cardiovascular Risk` = "Total number of cardiovascular risk factors",
    Depression = "Depression",
    Total_p = "Procrastination",
    `Depression x Procrastination` = "Depression x Procrastination"
  )
  
  # Add a column for significance stars
  data <- data |>
    mutate(
      stars = case_when(
        pval < 0.001 ~ "***",
        pval < 0.01 ~ "**",
        pval < 0.05 ~ "*",
        pval < 0.10 ~ "+",
        TRUE ~ "" 
      ))
  
  # Plotting  
  haz_plot <- data |>
    ggplot(aes(x = HR, y = forcats::fct_rev(Transition), colour = HR > 1)) +
    geom_point(size = 4) +
    geom_linerange(aes(xmin = L, xmax = U), linewidth = 1.5, alpha = 0.5) +
    geom_vline(xintercept = 1, colour = "gray75", linewidth = 0.75, linetype = "dashed") +
    geom_text(aes(label = stars), vjust = -0.5, hjust = 0.5, size = 5, color = "black") +
    labs(title = "Hazard Ratio for dementia transitions",
         x = "Hazard Ratio (95% Confidence Interval)", 
         y = "") +
    scale_color_manual(values = c("#E69F00", "#56B4E9"), guide = "none") +
    guides(colour = "none") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x = element_text(size = 12),
      strip.text = element_text(size = 10)
      )
  
  if(nrow(data) == 32){
  # Centering the bottom facet because I am pedantic
    haz_plot <- haz_plot + ggh4x::facet_manual(
    ~ Covariate,
    scales = "free_x",
    labeller = labeller(Covariate = labeller),
    design = c(
    "
    AABBCC
    DDEEFF
    #GGHH#
    "
    ))
  } else {
    # Centering the bottom facet because I am pedantic
    haz_plot <- haz_plot + ggh4x::facet_manual(
      ~ Covariate,
      scales = "free_x",
      labeller = labeller(Covariate = labeller),
      design = c(
      "
      AABBCC
      DDEEFF
      ##GG##
      "
      ))
  }
  
  return(haz_plot)
}

