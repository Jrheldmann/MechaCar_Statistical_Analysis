# Use the library() function to load the dplyr package
library(dplyr)

# Import and read in the MechaCar_mpg.csv file as a dataframe
MechaCar <- read.csv(file="MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)

# Perform linear regression using the lm() function. In the lm() function, pass in all six variables (i.e., columns), 
# and add the dataframe you created in Step 4 as the data parameter.
linModel <- lm(MechaCar$mpg ~ MechaCar$vehicle_length + 
              MechaCar$vehicle_weight + 
              MechaCar$spoiler_angle +
              MechaCar$ground_clearance +
              MechaCar$AWD, MechaCar)
# Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(linModel)

# Save your MechaCarChallenge.RScript file to your GitHub repository.

