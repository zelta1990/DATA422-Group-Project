# Environmental and Surrounding Conditions at Locations of Serious and Fatal crashes in Christchurch between 2012 and 2022

## How to
1.In a terminal run "git clone https://github.com/zelta1990/DATA422-Group-Project.git"
to clone the project code to your own machine. 

2.Run group-project.ipynb. Make sure you install all required packages. 

3.nzta_api_Julia.ipynb is used to generate "cameras.csv" using web scraping, which is used by group-project.ipynb. 
However it is not required for the successful run of group-project.ipynb as a copy of "cameras.csv" generated by nzta_api_Julia.ipynb 
is stored in a public repository (https://github.com/zelta1990/crash_data_nz) and can be downloaded by group-project.ipynb itself.

## Expected result
On finish running the code in group-project.ipynb, you should be able to see an interactive
map hosted on your localhost with the port specified by the execution output. You can also check 
a same map deployed to R Shiny server, which can be accessed in any mainstream browser by visiting
https://mmo191.shinyapps.io/deploy422/.

Check the Wiki page(https://github.com/zelta1990/DATA422-Group-Project/wiki) on how to use the interactive map.


![alt text](https://github.com/zelta1990/DATA422-Group-Project/blob/main/map_screenshot.png?raw=true)

