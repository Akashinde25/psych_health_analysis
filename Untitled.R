# =========================
# Part 1 - Logical and Computational Thinking in R
# Question 1: Day of the Week Checker
# Author: Jean Claude ISHIMWE
# Description: Checks if a given day number is a working day or weekend.
# =========================

# Set default input (change this value for testing)
day_num <- 5  # Change this manually to test other days (0-6)

# OR uncomment the next 2 lines if running interactively
# cat("Enter a number from 0 (Sunday) to 6 (Saturday): ")
# day_num <- as.integer(readline())

# Define vector of day names
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Validate input and print result
if (!is.na(day_num) && day_num >= 0 && day_num <= 6) {
  day_name <- day_names[day_num + 1]
  
  if (day_num == 0 || day_num == 6) {
    message <- paste(day_name, "- Weekend")
  } else {
    message <- paste(day_name, "- Working day")
  }
  
  print(message)
} else {
  print("Invalid input. Please enter a number from 0 to 6.")
}



# =========================
# Part 1 - Logical and Computational Thinking in R
# Question 2: Days in a Month
# Author: Jean Claude ISHIMWE
# Description: Asks the user for a number (0–11) and prints the number of days in that month, ignoring leap years.
# =========================

# Define vector of month names
month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

# Define corresponding days in each month (ignoring leap years)
month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Uncomment to run interactively
# cat("Enter a number from 0 (January) to 11 (December): ")
# month_num <- as.integer(readline())

# For testing, set a manual month number (change this value)
month_num <- 4  # Change manually to test (0 = January, 1 = February, ..., 11 = December)

# Validate input and print result
if (!is.na(month_num) && month_num >= 0 && month_num <= 11) {
  month_name <- month_names[month_num + 1]
  days_in_month <- month_days[month_num + 1]
  message <- paste(month_name, "has", days_in_month, "days.")
  print(message)
} else {
  print("Invalid input. Please enter a number from 0 to 11.")
}


# =========================
# Part 1 - Logical and Computational Thinking in R
# Question 3: Multiplication Table
# Author: Jean Claude ISHIMWE
# Description: Reads an integer (0–9) and prints its multiplication table up to N.
# =========================

# Uncomment to run interactively
# cat("Enter a base number (0 to 9): ")
# base_num <- as.integer(readline())
# cat("Enter the limit N (natural number): ")
# limit_n <- as.integer(readline())

# For testing, set manual values
base_num <- 3  # Base number (0–9)
limit_n <- 5   # Limit N (natural number)

# Validate inputs
if (!is.na(base_num) && base_num >= 0 && base_num <= 9 &&
    !is.na(limit_n) && limit_n > 0) {
  
  cat("Multiplication table for", base_num, "up to", limit_n, ":\n")
  
  for (i in 1:limit_n) {
    result <- base_num * i
    cat(base_num, "x", i, "=", result, "\n")
  }
  
} else {
  print("Invalid input. Please enter a base number (0–9) and a positive N value.")
}


# =========================
# Part 1 - Logical and Computational Thinking in R
# Question 4: Multiples of 3, 7, or 11
# Author: Jean Claude ISHIMWE
# Description: Prints all numbers between 0 and 40 that are multiples of 3, 7, or 11.
# =========================

# Define range
start_num <- 0
end_num <- 40

cat("Numbers between", start_num, "and", end_num, "that are multiples of 3, 7, or 11:\n")

# Loop through range and check condition
for (num in start_num:end_num) {
  if (num %% 3 == 0 || num %% 7 == 0 || num %% 11 == 0) {
    cat(num, "\n")
  }
}


# =========================
# Part 1 - Logical and Computational Thinking in R
# Question 5: Point Position Checker
# Author: Jean Claude ISHIMWE
# Description: Determines the position of a point (x, y) in the plane.
# =========================

repeat {
  # Uncomment for interactive input
  # cat("Enter x coordinate (or type 'q' to quit): ")
  # x_input <- readline()
  # if (x_input == "q") break
  # x <- as.numeric(x_input)
  
  # cat("Enter y coordinate (or type 'q' to quit): ")
  # y_input <- readline()
  # if (y_input == "q") break
  # y <- as.numeric(y_input)
  
  # For testing, set manual values (change as needed)
  x <- 3
  y <- -4
  
  # Check for NA or invalid input
  if (is.na(x) || is.na(y)) {
    print("Invalid input. Please enter numeric values.")
    break
  }
  
  # Check position
  if (x == 0 && y == 0) {
    cat("The point is at the origin (0,0).\n")
  } else if (x == 0) {
    cat("The point lies on the Y-axis.\n")
  } else if (y == 0) {
    cat("The point lies on the X-axis.\n")
  } else if (x > 0 && y > 0) {
    cat("The point is in the 1st quadrant.\n")
  } else if (x < 0 && y > 0) {
    cat("The point is in the 2nd quadrant.\n")
  } else if (x < 0 && y < 0) {
    cat("The point is in the 3rd quadrant.\n")
  } else if (x > 0 && y < 0) {
    cat("The point is in the 4th quadrant.\n")
  }
  
  # Uncomment to continue asking or quit
  # cat("Do you want to check another point? (y/n): ")
  # continue <- readline()
  # if (tolower(continue) != "y") break
  
  break  # Remove this break if using interactive loop
}


# =========================
# Part 2 - Data Analysis with student_depression_dataset.csv
# Question 1: Import the Dataset
# Author: Jean Claude ISHIMWE
# Description: Loads the student depression dataset and displays the first 6 rows.
# =========================

# Load necessary package
# If not installed, uncomment the next line:
# install.packages("readr")

library(readr)

# Import the dataset
data <- read_csv("student_depression_dataset.csv")

# Display the first 6 rows
head(data)



# =========================
# Part 2 - Question 2: Understand the Structure
# Author: Jean Claude ISHIMWE
# Description: Displays the structure and summary of the student depression dataset.
# =========================

# Check the structure of the dataset (shows variable types and examples)
str(data)

# Display summary statistics (gives min, max, mean, etc. for each column)
summary(data)


# =========================
# Part 2 - Question 3: Basic Descriptive Statistics
# Author: Jean Claude ISHIMWE
# Description: Calculates mean, median, and standard deviation for three numerical variables.
# =========================

# List of numeric variables
num_vars <- c("Age", "Academic Pressure", "CGPA")

# Loop through each and calculate stats
for (var in num_vars) {
  cat("\nStatistics for:", var, "\n")
  cat("Mean:", mean(data[[var]], na.rm = TRUE), "\n")
  cat("Median:", median(data[[var]], na.rm = TRUE), "\n")
  cat("Standard Deviation:", sd(data[[var]], na.rm = TRUE), "\n")
}



# =========================
# Part 2 - Question 4: Gender Distribution
# Author: Jean Claude ISHIMWE
# Description: Creates a table for gender distribution and displays it using a bar plot.
# =========================

# Create a table showing the number of students by gender
gender_table <- table(data$Gender)

# Display the gender distribution table
cat("\nGender Distribution Table:\n")
print(gender_table)

# Adjust margins before plotting (default is c(5, 4, 4, 2) for bottom, left, top, right)
par(mar = c(5, 5, 4, 2)) 

# Plot the gender distribution as a bar plot
barplot(gender_table, 
        main = "Gender Distribution of Students", 
        xlab = "Gender", 
        ylab = "Number of Students", 
        col = c("lightblue", "lightpink"), 
        border = "black")




# =========================
# Part 2 - Question 5: Depression by Gender
# Author: Jean Claude ISHIMWE
# Description: Count how many students reported being depressed, grouped by gender, 
# and present the result using a stacked bar chart.
# =========================

# Create a table that counts depression status by gender
depression_by_gender <- table(data$Gender, data$Depression)

# Display the table of depression counts by gender
cat("\nDepression Count by Gender:\n")
print(depression_by_gender)

# Plot the depression by gender using a stacked bar chart
barplot(depression_by_gender, 
        beside = FALSE, 
        col = c("lightblue", "lightpink"), 
        main = "Depression by Gender", 
        xlab = "Gender", 
        ylab = "Number of Students", 
        legend = rownames(depression_by_gender), 
        args.legend = list(title = "Depression Status", 
                           fill = c("lightblue", "lightpink"), 
                           border = "black"))


# =========================
# Part 2 - Question 6: Average Sleep Duration by City
# Author: Jean Claude ISHIMWE
# Description: Calculate and display the average sleep duration for each city.
# =========================

# Function to convert sleep duration to numeric (midpoint of the range)
convert_sleep_duration <- function(sleep_str) {
  # Remove unwanted characters and split based on the hyphen
  sleep_range <- gsub("[^0-9-]", "", sleep_str)
  sleep_values <- as.numeric(strsplit(sleep_range, "-")[[1]])
  
  # Return the midpoint of the range
  return(mean(sleep_values))
}

# Apply the function to the 'Sleep Duration' column to convert it to numeric
data$SleepDurationNumeric <- sapply(data$`Sleep Duration`, convert_sleep_duration)

# Calculate the average sleep duration by city
avg_sleep_by_city <- aggregate(SleepDurationNumeric ~ City, data = data, FUN = mean, na.rm = TRUE)

# Display the results
cat("\nAverage Sleep Duration by City:\n")
print(avg_sleep_by_city)



# =========================
# Part 2 - Question 8: Missing Data Check
# Author: Jean Claude ISHIMWE
# Description: Checks for missing values in the dataset and counts how many per variable.
# =========================

# Check for missing values in the entire dataset
missing_values <- colSums(is.na(data))

# Display the missing values count for each variable
cat("\nMissing Values per Variable:\n")
print(missing_values)

# Impute missing values for 'SleepDurationNumeric' with the mean (or median)
data$SleepDurationNumeric[is.na(data$SleepDurationNumeric)] <- mean(data$SleepDurationNumeric, na.rm = TRUE)



# =========================
# Part 2 - Question 9: Boxplot Visualization
# Author: Jean Claude ISHIMWE
# Description: Creates a boxplot for CGPA grouped by Depression status.
# =========================

# Create the boxplot
boxplot(CGPA ~ Depression, data = data, 
        main = "CGPA by Depression Status", 
        xlab = "Depression Status", 
        ylab = "CGPA", 
        col = c("lightblue", "lightcoral"), 
        border = "darkblue", 
        notch = TRUE, 
        outline = FALSE)

# Add grid for better visualization
grid()



# =========================
# Part 2 - Question 10: Linear Modeling
# Author: Jean Claude ISHIMWE
# Description: Fits a linear regression model with CGPA as the response variable and selected predictors.
# =========================

# Fit the linear regression model
model <- lm(CGPA ~ Age + `Academic Pressure` + `Financial Stress`, data = data)

# Display the summary of the model
summary(model)

# Interpreting the Output:
# 1.Coefficients:
# Intercept: 7.670154 — This is the estimated average CGPA when all predictors (Age, Academic Pressure, and Financial Stress) are zero. In this context, it doesn’t have a practical meaning but serves as a baseline for the model.

# Age: 0.001264 — This means that for each additional year of age, the CGPA is expected to increase by 0.001264 points. However, the p-value (0.484056) is greater than 0.05, indicating that Age is not a statistically significant predictor of CGPA in this model.

# Academic Pressure: -0.024924 — This means that for each additional unit of Academic Pressure, CGPA is expected to decrease by 0.024924 points. The p-value (0.000114) is less than 0.05, indicating that Academic Pressure is a statistically significant predictor of CGPA.

# Financial Stress: 0.010066 — For each unit increase in Financial Stress, CGPA is expected to increase by 0.010066 points. However, the p-value (0.105541) is greater than 0.05, indicating that Financial Stress is not statistically significant in predicting CGPA at the 5% significance level.


# 2.Residuals:
# The residuals (the differences between the observed and predicted CGPA values) range from -7.7508 to 2.4052, with a median value close to zero (0.1167). This suggests that the model's predictions are fairly close to the actual data points, although there is some variability.


# 3.R-squared:
# Multiple R-squared: 0.0005998 — This means that only 0.06% of the variation in CGPA is explained by the predictors (Age, Academic Pressure, and Financial Stress). This is very low, indicating that these predictors don't explain much of the variability in CGPA.

# Adjusted R-squared: 0.0004924 — The adjusted R-squared accounts for the number of predictors in the model and is slightly lower than the R-squared, indicating that adding more variables doesn't significantly improve the model.



# 4.F-statistic:
# The F-statistic of 5.581 and its associated p-value (0.0007996) indicate that at least one of the predictors is statistically significant in explaining the variance in CGPA. This suggests that the model as a whole is useful, even though the individual predictors are not all significant.



# Summary:
# The significant predictors in the model are Academic Pressure, which negatively impacts CGPA, and the Intercept, which represents the baseline CGPA when the predictors are zero.
# Age and Financial Stress do not significantly contribute to explaining the variation in CGPA based on their high p-values.
# The model's R-squared value is very low, suggesting that the selected predictors do not explain much of the variance in CGPA. Consider adding more relevant predictors to improve the model's explanatory power.
