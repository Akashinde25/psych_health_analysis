

# Define the file path for storing mental health logs
log_file_path <- "mental_health_log.csv"

# --- Function to Load Data ---
# This function checks if the log file exists.
# If it exists, it reads the data from the CSV.
# If not, it creates a new empty data frame with predefined columns.
load_mental_health_data <- function() {
  if (file.exists(log_file_path)) {
    # Read the CSV file. stringsAsFactors=FALSE is important for text columns.
    # header=TRUE ensures the first row is treated as column names.
    # Convert Date column to Date type immediately upon loading
    data <- read.csv(log_file_path, stringsAsFactors = FALSE, header = TRUE)
    data$Date <- as.Date(data$Date) # Ensure Date column is of Date type
    message("Existing mental health log loaded.")
  } else {
    # Create an empty data frame with the specified column types
    data <- data.frame(
      Date = as.Date(character()), # Use as.Date(character()) for empty Date column
      DayOfWeek = character(),
      Month = character(),
      OverallMood = character(),
      SpecificEmotions = character(),
      StressLevel = integer(), # Using integer for stress level (1-5)
      Notes = character(),
      stringsAsFactors = FALSE
    )
    message("New mental health log created.")
  }
  return(data)
}

# --- Function to Save Data ---
# This function takes the current data frame and writes it to the CSV file.
# overwrite=TRUE ensures the file is completely rewritten with the latest data.
save_mental_health_data <- function(data) {
  write.csv(data, log_file_path, row.names = FALSE)
  message("Mental health log saved.")
}

# --- Function to Add a New Entry ---
# This function guides the user to input their daily mental health status.
# It captures the current date and prompts for mood, stress, and notes.
add_new_entry <- function(current_data) {
  # Get current date information
  current_date <- Sys.Date()
  day_of_week <- format(current_date, "%A") # e.g., "Monday"
  month_name <- format(current_date, "%B")  # e.g., "July"
  
  cat("\n--- Add New Mental Health Entry ---\n")
  cat("Today's Date:", as.character(current_date), "\n")
  
  # Prompt for Overall Mood
  overall_mood <- ""
  valid_moods <- c("Happy", "Neutral", "Sad", "Anxious", "Angry", "Calm", "Energetic", "Tired", "Excited", "Relaxed", "Stressed", "Overwhelmed")
  while (!(overall_mood %in% valid_moods)) {
    cat("Enter your overall mood (e.g., Happy, Neutral, Sad, Anxious, Angry, Calm, Energetic, Tired, Excited, Relaxed, Stressed, Overwhelmed):\n")
    overall_mood <- readline()
    if (!(overall_mood %in% valid_moods)) {
      cat("Invalid mood. Please choose from:", paste(valid_moods, collapse = ", "), "\n")
    }
  }
  
  # Prompt for Specific Emotions (comma-separated)
  cat("Enter specific emotions you felt today (e.g., tension, anxiety, joy, frustration - comma-separated):\n")
  specific_emotions <- readline()
  
  # Prompt for Stress Level (1-5)
  stress_level <- NA
  # Helper function for numeric range check (defined outside to avoid re-definition)
  between <- function(x, lower, upper) {
    x >= lower && x <= upper
  }
  while (is.na(stress_level) || !between(stress_level, 1, 5)) {
    cat("Enter your stress level (1 = Very Low Stress/Very Positive, 5 = Very High Stress/Very Negative):\n")
    stress_level_input <- readline()
    stress_level <- as.integer(stress_level_input)
    if (is.na(stress_level) || !between(stress_level, 1, 5)) {
      cat("Invalid input. Please enter a number between 1 and 5.\n")
    }
  }
  
  # Prompt for Notes/Reason
  cat("Enter any notes or reasons for your feelings today:\n")
  notes <- readline()
  
  # Create a new row for the data frame
  new_entry <- data.frame(
    Date = current_date, # Keep as Date type
    DayOfWeek = day_of_week,
    Month = month_name,
    OverallMood = overall_mood,
    SpecificEmotions = specific_emotions,
    StressLevel = stress_level,
    Notes = notes,
    stringsAsFactors = FALSE
  )
  
  # Append the new entry to the existing data
  updated_data <- rbind(current_data, new_entry)
  message("Entry added successfully.")
  return(updated_data)
}

# --- Function to View the Entire Log ---
# This function simply prints the entire mental health log.
view_mental_health_log <- function(data) {
  cat("\n--- Your Mental Health Log ---\n")
  if (nrow(data) == 0) {
    cat("Your log is currently empty. Add an entry!\n")
  } else {
    print(data)
  }
}

# --- Function to Plot Weekly Mental State Trend ---
plot_weekly_mental_state <- function(data) {
  if (nrow(data) < 2) {
    cat("\nNot enough data to plot a weekly trend. Please add more entries.\n")
    return()
  }
  
  # Ensure Date column is in Date format
  data$Date <- as.Date(data$Date)
  
  # Calculate week number (e.g., "YYYY-WW" for unique week identification)
  # "%Y" for year, "%U" for week number (starting Sunday, 00-53)
  data$Week <- format(data$Date, "%Y-%U")
  
  # Aggregate data by week to get the average StressLevel
  # na.rm=TRUE handles any potential missing stress levels
  weekly_avg_stress <- aggregate(StressLevel ~ Week, data = data, FUN = mean, na.rm = TRUE)
  
  # Sort by week to ensure correct plotting order
  weekly_avg_stress <- weekly_avg_stress[order(weekly_avg_stress$Week), ]
  
  # Prepare for plotting
  # Set up margins to prevent plot labels from being cut off
  par(mar = c(5, 5, 4, 2) + 0.1) # Default is c(5, 4, 4, 2) + 0.1
  
  # Create the plot
  # type = "b" for both points and lines
  # xaxt = "n" to suppress default x-axis ticks and labels (we'll add custom ones)
  plot(
    x = 1:nrow(weekly_avg_stress),
    y = weekly_avg_stress$StressLevel,
    type = "b",
    col = "darkblue",
    pch = 19, # Solid circle points
    lwd = 2,  # Line width
    ylim = c(1, 5), # Ensure y-axis covers the full stress level range
    main = "Weekly Mental State Trend (Average Stress Level)",
    xlab = "Week",
    ylab = "Average Stress Level (1=Low, 5=High)",
    xaxt = "n", # Suppress default x-axis
    cex.main = 1.2, # Size of main title
    cex.lab = 1.1,  # Size of axis labels
    cex.axis = 0.9  # Size of axis tick labels
  )
  
  # Add custom x-axis labels (week numbers)
  axis(
    side = 1, # Bottom axis
    at = 1:nrow(weekly_avg_stress), # Positions of ticks
    labels = weekly_avg_stress$Week, # Labels for ticks
    las = 2, # Rotate labels vertically for better readability
    cex.axis = 0.8 # Size of week labels
  )
  
  # Add a horizontal line at the average of all stress levels for reference
  abline(h = mean(data$StressLevel, na.rm = TRUE), col = "red", lty = 2)
  text(x = 1, y = mean(data$StressLevel, na.rm = TRUE),
       labels = paste("Overall Avg:", round(mean(data$StressLevel, na.rm = TRUE), 2)),
       pos = 3, col = "red", cex = 0.8)
  
  # Add grid for better readability
  grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
  
  cat("\nWeekly mental state trend plot generated. Check your R plotting device.\n")
}


# --- Main Program Loop ---
# This is the main interactive part of the program.
# It presents a menu to the user and performs actions based on their choice.
main <- function() {
  # Load existing data or create a new log
  mental_health_log <- load_mental_health_data()
  
  repeat {
    cat("\n--- Mental Health Tracker Menu ---\n")
    cat("1. Add a new daily entry\n")
    cat("2. View my mental health log\n")
    cat("3. Plot weekly mental state trend\n")
    cat("4. Exit\n")
    cat("Enter your choice (1-4): ")
    
    choice <- as.integer(readline())
    
    if (is.na(choice)) {
      message("Invalid input. Please enter a number.")
      next
    }
    
    if (choice == 1) {
      mental_health_log <- add_new_entry(mental_health_log)
      save_mental_health_data(mental_health_log) # Save after adding
    } else if (choice == 2) {
      view_mental_health_log(mental_health_log)
    } else if (choice == 3) {
      plot_weekly_mental_state(mental_health_log)
    } else if (choice == 4) {
      message("Exiting Mental Health Tracker. Goodbye!")
      break # Exit the loop
    } else {
      message("Invalid choice. Please enter 1, 2, 3, or 4.")
    }
  }
}

# Run the main program
main()

