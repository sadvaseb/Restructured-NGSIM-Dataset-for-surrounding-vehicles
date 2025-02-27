# Restructured NGSIM Dataset to Include Surrounding Vehicles' Attributes
 
## Background
The Federal Highway Administration (FHWA) gathered the Next Generation Simulation dataset (NGSIM) for Interstate 80 freeway (I-80) in Emeryville, California. NGSIM trajectory data has been collected by FHWA in 2005 by a set of synchronized cameras on the freeway. The original dataset is available from [here](https://ops.fhwa.dot.gov/trafficanalysistools/ngsim.htm).

the original dataset contains several noises. So, the database was cleaned and soften by the Multitude team. They created the Reconstructed NGSIM dataset, which is available from [here](http://www.multitude-project.eu/reconstructed-ngsim.html) However, these datasets only contains the subject vehicle's attributes (e.g., ID, location, and speed) and the following's and preceding's IDs. 

## This Dataset
We extended the Reconstructed NGSIM dataset to include surrounding vehicles' (i.e., following, subject, preceding, second preceding, and putative leaders on the left and right lanes) attributes. The surrounding vehicles are shown in the below picture. The attributes are the vehicles' IDs, locations, speeds, accelerations, velocity difference (i.e. velocity difference between the subject vehicle and its surrounding vehicles), headway (i.e. the bumper to bumper distance between the subject vehicle and its surrounding vehicle). Since no dataset with this information exists, NGSIM is restructured by an R code to generate the information. 

![alt text](https://github.com/sadvaseb/Restructured-NGSIM-Dataset-for-surrounding-vehicles/blob/master/pic%201.png "Pic 1")

## Data Cleaning
The following data cleaning processes were performed. Vehicles data was filtered to only passenger vehicles (i.e. only pair of passenger-passenger vehicles excluding heavy vehicles). Data for those subject vehicles changing lanes were removed to control for the impact of lane changing. Observations with missing data have also been removed. NGSIM dataset was divided into groups of vehicles on HOV, left-most lane, middle lanes, and rightmost lane. 
The correlations between the attributes are also provided as the below figure. 

![alt text](https://github.com/sadvaseb/Restructured-NGSIM-Dataset-for-surrounding-vehicles/blob/master/Correlation%20all.png "Pic 2")

The R code includes a detailed explanation of each line of code. It should be easy to modify the code/dataset for your application. 

Best of luck!

## Cite
Please cite this dataset/code as:

Vasebi S., Hayeri Y.M., Jin J. (2020) Human Car-Following Behavior: Parametric, Machine-Learning, and Deep-Learning Perspectives. In: Stanton N. (eds) Advances in Human Aspects of Transportation. AHFE 2020. Advances in Intelligent Systems and Computing, vol 1212. Springer, Cham. https://doi.org/10.1007/978-3-030-50943-9_6
 
