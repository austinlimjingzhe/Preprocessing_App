# Preprocessing App
Application for no-code data preprocessing using R Shiny.

## Summary
An attempt at creating an easy-to-use, no-code application for data preprocessing for machine learning.

Try it at: https://austinlim.shinyapps.io/Preprocessing_App/

## Background
As mentioned in the project, <a href="https://github.com/austinlimjingzhe/datascienceandanalysis-app">Data Science and Analysis Application</a>, a compliemntary project on a no-code solution for data preprocessing, similar to how Paxata is to DataRobot, was created around the same time. However, my initial approach was not working out and I put it on hold for about 10 months. During the break before starting work, I restarted the project with a slightly different approach and this worked out much better and allowed me to make significant progress on this project.

## Features

### 1. Uploading Datasets
Users can upload their own <code>.csv</code> files with the options to choose a separator, use the first row as headings or convert strings into factors. The uploaded data can be viewed as an interactive data table using <code>DT</code> library and a quick summary of the data is also found at the bottom. See <a href="https://github.com/austinlimjingzhe/datascienceandanalysis-app">Feature 1</a>.

### 2. Converting Data Types
Datasets may contain numbers that represent factors and strings that represent Datetimes, hence this feature allows users to easily convert between data types using the console. Currently supports conversions to <code>numeric</code>,<code>factor</code> and <code>date</code> data types. The example file below is the famous <code>mtcars</code> dataset in R.

![convert factor](https://user-images.githubusercontent.com/88301287/175457383-29e5d65c-7df0-4684-82ef-6dc1e77142bf.gif)

By specifying a datetime format, we can also convert strings to datetime. The example below uses a snippet of data for the <a href="https://github.com/austinlimjingzhe/top_songs_app">top_songs_app</a> project which contains a <code>Date</code> and a <code>Published</code> column that are datetime in nature.

![convert datetime](https://user-images.githubusercontent.com/88301287/175457695-5dec1956-4b54-4c80-9f26-720aa8fa8b7f.gif)

### 3. Handling Erroneous Data
Datasets may contain duplicated data or missing values. Hence, this feature allows users to identify these rows and either drop or impute them in the case of missing values.

![drop dupes](https://user-images.githubusercontent.com/88301287/175458174-0e693979-70d3-476c-b831-6cd993296ced.gif)

For missing values, we can also remove them, in this example, I used the famous <code>iris</code> dataset where I introduced <code>NAs</code> using the <code>prodNA</code> of <code>missingForest</code> library.

![drop missing](https://user-images.githubusercontent.com/88301287/175458500-3c397ada-3fb5-49d1-add4-530daaa53547.gif)

Alternatively, we can replace the <code>NAs</code> using the mean or other values with the simple imputation function. Currently, this feature supports <code>mean</code>, <code>median</code>, backfill, frontfill and random methods for <code>numeric</code> data, and <code>Mode</code>, backfill, frontfill and new cateogry methods (treat <code>NAs</code> as a new category) for <code>factor</code> data. 

![simple impute](https://user-images.githubusercontent.com/88301287/175458786-fc70e4ba-38dd-427a-82f3-99922f77fd0e.gif)

### 4. Data Wrangling
Sometimes, we may need a subset of the data or simply reorder/rename data. Other times, we may need a snapshot summary of the data, we have. Hence, this feature allows users to make use of all the common <code>dplyr</code> functions: <code>select</code>, <code>filter</code>, <code>rename</code>, <code>arrange</code> and <code>summarize</code>.  Under <code>filter</code>, data can be subsetted by: <code>==</code>, <code>></code>, <code>>=</code>, <code><</code>, <code><=</code>, <code>!=</code>, <code>%in%</code> and <code>contains</code>

![wrangle1](https://user-images.githubusercontent.com/88301287/175491516-ef4945a1-4e94-4bd0-a400-c199c80a366a.gif)

A list of summary functions includes: <code>mean</code>, <code>median</code>, <code>max</code>, <code>min</code>, <code>sum</code>,<code>sd</code>,<code>IQR</code> and <code>count</code>.

![wrangle2](https://user-images.githubusercontent.com/88301287/175491538-160e863e-4d3f-47d6-8c3d-ba9e25f49b8a.gif)

### 5. Transform Variables
"T" in "ETL" stands for Transform and variables in datasets may need to be transformed to create new variables. Hence, common transformations for <code>numeric</code> data and <code>factor</code> data have been included. Methods to transform <code>numeric</code> data inlcude: <code>log</code>, <code>abs</code>,<code>Winsorize</code>, <code>rank</code>, <code>sqrt</code>, <code>square</code>, <code>normalize</code>, <code>standardize</code>, <code>binning</code>, and <code>boxcox</code>  transformations.

![transform1](https://user-images.githubusercontent.com/88301287/175491655-da23f9a0-ec63-416e-8d6d-8ca742d24d47.gif)

While methods to transform <code>factor</code> data currently includes: <code>onehotencoding</code> and <code>relevelling</code>.

![transform2](https://user-images.githubusercontent.com/88301287/175491664-1040b1b8-2dac-4b42-adb5-548a6e6197ea.gif)

### 6. Advanced Options - Advanced Imputation
To leverage on the abundence of resources and packages developed for smart imputation of missing values, this feature allows the user to use packages such as <code>mice</code>, <code>Amelia</code> and <code>missForest</code>. However, currently the application only uses the default settings of the respective functions. More customizability to be added as an upgrade in future.

![advanced impute](https://user-images.githubusercontent.com/88301287/175476280-4a270301-326b-4100-b08c-692e7083141e.gif)

### 7. Advanced Options - Dimensionality Reduction
Dimensionality reduction may be necessary for datasets with too many variables that may be highly correlated, leading to multicollinearity issues amongst many other problems, hence this feature allows users to use 4 different methods: <code>lda</code>, <code>ica</code>, <code>t-SNE</code> and <code>pca</code>. Note that the <code>t-SNE</code> method only works for datasets with no duplicated data. The exmaple uses the <code>iris</code> dataset that has already removed the duplicated row.

![reduce dimensions](https://user-images.githubusercontent.com/88301287/175477478-b6290609-d1a4-4c12-a3a8-34a469d7ba14.gif)

### 8. Advanced Options - Imbalanced Classes
Most machine learning problems will have a class imbalance issue which may affect results hence this feature allows users to use packages such as <code>ROSE</code> and <code>smotefamily</code> to deal with class imbalance by resampling the data. This example uses the famous <code>nhanes</code> dataset combined with a randomly generated <code>ill</code> column that is treated as the target variable. A plot showing the distribution of target variable helps to depict the effect of the resampling.

![imbalanced classes](https://user-images.githubusercontent.com/88301287/175478318-8f91c0b6-3564-4525-aa63-b8bbbabf02e4.gif)

### 9. Advanced Options - Merge Datasets
Another feature that may be necessary in data cleaning is joining datasets together from different <code>.csv</code> files. Hence, this feature allows the user to upload an additional dataset and select the keys from each dataset to join on. Joining methods include are: <code>left</code>, <code>right</code>, <code>inner</code> and <code>cross</code>. This example merges 2 sets of <code>iris</code> data on <code>Sepal.Length</code>.

![merge data](https://user-images.githubusercontent.com/88301287/175479605-6501e481-63c1-49be-b6a8-7c2a78321202.gif)

### 10. Advanced Options - Rolling Windows & Datetime Functions
Lastly, time-related functions such as rolling window transformations and datetime functions have been included as additional options for data engineering. For rolling windows, the same list of aggregation functions as <code>summarize</code> in Feature #3 can be specified, along with the size of the window to roll on. 

![rolling window](https://user-images.githubusercontent.com/88301287/175491687-aafea9e2-c1f7-4d86-b524-ecdd1b75e2c8.gif)

Meanwhile the datetime functions include: extracting other <code>datetime</code> elements from <code>datetime</code> variable(s) and calculating intervals of time.
The <code>datetime</code> elements that can be extracted include: <code>year</code>, <code>semester</code>, <code>quarter</code>, <code>month</code>, <code>monthname</code> (Spelt out as opposed to numbers), <code>week</code>, <code>day</code>, <code>dayofweek</code> (Spelt out as opposed to numbers), <code>hour</code>, <code>minute</code> and <code>second</code>. Units for interval calculations can also be specified, and the list is the same sans <code>semester</code>, <code>quarter</code>, <code>monthname</code> and <code>dayofweek</code>.

![datetime](https://user-images.githubusercontent.com/88301287/175491705-d4c8a5e9-2d7c-42df-a00c-50db3d36c73e.gif)

### 0. Downloads
All pages except the first have a <code>Download</code> button to allow users to download the processed dataset that they are working on. 

## Discussion
While working on this project, I realized a few things:
1. Sometimes it is easier to take a break and resume work another day. There have been days where I would stare at the codes for hours on end without being able to fix a bug. Only for me to return to it the next day and realize that it was a silly little mistake like a missing/additional comma or a naming issue with my UI elements.
2. There are multiple ways to get to the same outcome. For example, subsetting can either use <code>dplyr</code>'s <code>filter</code> or <code>base</code> R's <code>subset</code>. When encountering an issue with writing code 1 way, as long as time complexity isn't a huge issue, try another method. In fact, what allowed me to restart the project is trying out another approach as my original one was not behaving how I would like the data to behave.
3. On the second point, it can be detrimental to depend only on 1 way of doing things. There were many times as I was developing this project where I went: "I wished I could use Python" such as when implementing the <code>Imbalanced Classes</code> resampling functionality as I had been more used to using Python's <code>imblearn</code> package for example. Personally, I feel that while the no-code is an ideal to strive for, it's still very much useful to learn widely and implement the best/easiest method for yourself. 

Finally, as always, this project has several areas of improvement:
1. The <code>Data Wrangling</code> tab sees each <code>dplyr</code> function working independently which means the user will have to repeatedly download and upload the temporary data. Apart from fixing this bug to make it so that all functions affect the temporary data cumulatively, it would be good for UX to allow multiple functions to be used at once due to the existence of <code>%>%</code> operators. However, I've not implemented this as the order in which the functions are used matters and so I held off.
2. It was quite late into development of the application where I realized how I could code to aviod multiple long <code>if-else</code> statements which means that a cleanup of the code would make the code much more readable.
3. As mentioned, there are some functions that I could not fully flesh out such as the advanced imputation methods. Furthermore, I tested each feature independently which means that there could be cases where the application breaks when the method is not suitable for the data and it would be good to display a message to the user to notify them.

Again, there may be other bugs or areas for improvement or even features that I may not have considered. I would love to get into a discussion on what they are and how I can implement them.
