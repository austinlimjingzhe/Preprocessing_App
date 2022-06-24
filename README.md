# Preprocessing App
Application for no-code data preprocessing using R Shiny.

## Summary
An attempt at creating an easy-to-use, no-code application for data preprocessing for machine learning.

Try it at: https://austinlim.shinyapps.io/Preprocessing_App/

## Background
As mentioned in the project, <a href="https://github.com/austinlimjingzhe/datascienceandanalysis-app">Data Science and Analysis Application</a>, a compliemntary project on a no-code solution for data preprocessing, similar to how Paxata is to DataRobot, was created around the same time. However, my initial approach was not working out and I put it on hold for about 10 months. During the break before starting work, I restarted the project with a slightly different approach and this worked out much better and allowed me to make significant progress on this project.

## Features

### 1. Converting Data Types
Datasets may contain numbers that represent factors and strings that represent Datetimes, hence this feature allows users to easily convert between data types using the console.

![convert factor](https://user-images.githubusercontent.com/88301287/175457383-29e5d65c-7df0-4684-82ef-6dc1e77142bf.gif)

By specifying a datetime format, we can also convert strings to datetime.

![convert datetime](https://user-images.githubusercontent.com/88301287/175457695-5dec1956-4b54-4c80-9f26-720aa8fa8b7f.gif)

### 2. Handling Erroneous Data
Datasets may contain duplicated data or missing values. Hence, this feature allows users to identify these rows and either drop or impute them in the case of missing values.

![drop dupes](https://user-images.githubusercontent.com/88301287/175458174-0e693979-70d3-476c-b831-6cd993296ced.gif)

For missing values, we can also remove them, in this example, I used the famous iris data set where I introduced <code>NAs</code> using the <code>prodNA</code> of <code>missingForest</code> library.

![drop missing](https://user-images.githubusercontent.com/88301287/175458500-3c397ada-3fb5-49d1-add4-530daaa53547.gif)

Alternatively, we can replace the <code>NAs</code> using the mean or other values with the simple imputation function.

![simple impute](https://user-images.githubusercontent.com/88301287/175458786-fc70e4ba-38dd-427a-82f3-99922f77fd0e.gif)

### 3. Data Wrangling
Sometimes, we may need a subset of the data or simply reorder/rename data. Other times, we may need a snapshot summary of the data, we have. Hence, this feature allows users to make use of all the common <code>dplyr</code> functions: <code>select</code>,<code>filter</code>,<code>rename</code>,<code>arrange</code> and <code>summarize</code>. A list of summary functions includes: <code>mean</code>,<code>median</code>,<code>max</code>,<code>min</code>,<code>sum</code>,<code>sd</code>,<code>IQR</code> and <code>count</code>.

### 4. Transform Variables

### 5. Advanced Options - Advanced Imputation

### 6. Advanced Options - Dimensionality Reduction

### 7. Advanced Options - Imbalanced Classes

### 8. Advanced Options - Merge Datasets

### 9. Advanced Options - Rolling Windows & Datetime Functions

## Discussion




