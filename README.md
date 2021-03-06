
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jbutilities

This package contains multiple functions I found or created through the
years and may be useful to keep

## Installation

You can install the released version of jbutilities from
[GITHUB](https://github.com/bronnimannj/jbutilities).

## Functions inside the package:

### check\_format\_df

This function verifies:

-   The format of the table: data.table, data.frame or tibble?
-   The names of the columns needed: only letters, integers or "\_"
-   The format of the columns: contains only factors and numeric columns
-   Missing data per column
-   Other strange values: “NaN”, “NA”, or +/- Inf

Inputs:

-   data : a data frame containing the dataset to check
-   digits: an integer, number of digits we will round the numerical
    variables. Default is 6

It returns :

-   For each boolean column the number of True or False values,
-   For each categorical (factor) column the number of levels and the
    first 3 levels
-   For each numerical column the min, mean, median, max, number of NAS,
    and the 1,10,25,75,90 and 99 quantiles.

### create\_df\_rd

This function creates a random dataset with some user inputs, without
any NA nor Inf.

Inputs:

-   “size”: an integer
-   “seed”: an integer or NA
-   “Numerics”: NULL or a character vector of names of normally
    distributed numeric vectors
-   “Booleans”: NULL or a named (or not) vector of probabilities between
    0 and 1 to have True
-   “Categories”: NULL or a list of factor variables, each element
    containing the levels as vector

It returns a data.frame that can be tested by check\_format\_df

### cut\_fat

This function is a fast version of cut that will put every element of x
in one of the intervals created by breaks. Each interval is left closed
and right opened, example: ‘\[0,1)’.

If -Inf or/and +Inf are not in the vector ‘breaks’, it will be added to
it.

If none of the elements are out of the intervales between the breaks,
then the factor returned is only constituted of the breaks intervals.
Else, +/-Inf will be added.

Inputs:

-   x: Numeric vector of elements to put into segments
-   breaks: Numeric vector of breaks

It returns a factor of intervals

### find\_series

Function to find the latest file in a series. Types regognized are:

Inputs:

-   Name String: representing the name of the file
-   Type String: representing the extension of the file

It returns a String, with the last name of the series

### my\_ggtheme

Function to put my favorite ggplot theme to any ggplot plot

Inputs:

-   x\_percent boolean, if TRUE we label the x axis into percents
-   y\_percent boolean, if TRUE we label the y axis with percents

It returns a ggplot with a new theme

### name\_series

Function to create files without over writing them.

It will add an underscore and the next number of the series.

Inputs:

-   Name: String, representing the name of the file
-   Type: String, representing the extension of the file

It returns a String, with the next name of the series

### step\_time

Function to print a step and time

Input: “step”, a string character representing the step. Can be a number
too

It returns a string containing the step and the date + time in a format
“(X.Y) - \[ 06/03/2021 18:03:32 \]”
