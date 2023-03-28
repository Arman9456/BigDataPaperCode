# create lags function


# The zoo package allows for easy handling of time series
library(zoo)
# The nowcasting package implements the information criteria of Bai & Ng
library(nowcasting) # I installed this manually through zip file and installed all required packages
# bootUR has a simple way to create appropriate differences
library(bootUR)

# Set the URL from which to download the data
FRED_url <- url("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2022-01.csv")
# Download and read in the data
FRED_MD <- read.csv(FRED_url)
# Extract the transformation codes
trans_codes <- FRED_MD[1, -1]
# Extract the data (we delete the first and last row)
data_fred <- FRED_MD[-c(1, nrow(FRED_MD)), -1]
# Read in the dates and transform them to dates in zoo format
fred_date <- as.yearmon(FRED_MD[-c(1, nrow(FRED_MD)), 1], "%m/%d/%Y")
# Transform data into a time series
fred_tseries <- zoo(data_fred, fred_date)
# Record the number of variables
N_fred <- ncol(fred_tseries)

# Although McCracken and Ng Suggest seconds differences of price serious,
# most researchers actually use first differences. We are going to do the same.
# The first differences of log prices correspond to inflation,
# which makes things easier to interpret.
# However, that does mean that we need to make some adjustments to the transformation codes.
trans_codes[100:119] <- 5

# Take logs of the relevant series
fred_tseries[, trans_codes %in% 4:6] <- log(fred_tseries[, trans_codes %in% 4:6])
# Create a vector that says how often we need differ each series
n_diff <- 1 * (trans_codes %in% c(2, 5)) + 2 * (trans_codes %in% c(3, 6))

# Use the function in bootUR to create the appropriate differences
fred_diff <- diff_mult(fred_tseries, n_diff)

# For the sake of the exercise, we are only going to consider data from 1995 on
fred_final <- fred_diff[index(fred_diff) >= "jan 1995"]
# Record the number of observations
T_fred <- nrow(fred_final)

# Let's check if there are still missing data
bootUR::plot_missing_values(fred_final)
# So there are still some missing data. Right now we don't particularly care
# about the missing values at the end of the sample.
# But the missing ones in the middle are a problem.
# We could just delete the series, but here I am going to impute the missing values.
# First, we check which series are affected.
where_missing <- bootUR::check_missing_insample_values(fred_final)
plot(fred_final[, where_missing])
# Let's simply impute the missing values linearly.
index_missing1 <- (1:T_fred)[is.na(fred_final$CP3Mx)]
weight1 <- 2:1 / 3
fred_final$CP3Mx[index_missing1] <- weight1 * as.double(fred_final$CP3Mx[index_missing1[1] - 1]) +
  (1 - weight1) * as.double(fred_final$CP3Mx[index_missing1[2] + 1])
index_missing2 <- (1:T_fred)[is.na(fred_final$COMPAPFFx)]
weight2 <- 0.5
fred_final$COMPAPFFx[index_missing2] <- weight2 *
  as.double(fred_final$COMPAPFFx[index_missing2 - 1]) +
  (1 - weight2) * as.double(fred_final$COMPAPFFx[index_missing2 + 1])
# We have now taken care of the missing values

# Now you can start your forecasting analysis!

# A function to create a matrix of lags. Inputs:
# X					The data matrix to take the lags of.
# lags				The lags you want to take as a vector, e.g. 1:3 for first three lags.
# include.original	If TRUE, the original data are added too.
# trim				If TRUE, the rows with missing observations are deleted.
#					If FALSE, the missing observations are set to 0.
create_lags <- function(X, lags, include_original = FALSE, trim = TRUE) {
  x <- as.matrix(X)
  n <- nrow(x)
  k <- ncol(x)
  p <- length(lags)
  lx <- matrix(0, nrow = n, ncol = (p + include_original) * k)
  if (is.null(colnames(x))) {
    c_names <- rep("", k)
  } else {
    c_names <- colnames(x)
  }
  colnames(lx) <- rep(c_names, p + include_original)
  for (i in 1:p) {
    cols <- k * (i - 1 + include_original) + 1:k
    lx[(1 + lags[i]):n, cols] <- x[1:(n - lags[i]), ]
    colnames(lx)[cols] <- paste(c_names, "_lag_", lags[i], sep = "")
  }
  if (include_original) {
    lx[, 1:k] <- x
  }
  if (requireNamespace("zoo", quietly = TRUE)) {
    if (zoo::is.zoo(x)) {
      lx <- zoo::zoo(lx, order.by = zoo::index(y))
    }
  }
  return(lx[(1 + trim * p):n, , drop = FALSE])
}

