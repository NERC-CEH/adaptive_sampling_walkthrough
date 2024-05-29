# Empirical and model-based adaptive sampling

This repository contains code to support Box 1 in the paper "Adaptive sampling in ecology: key challenges and future opportunities" (Henrys _et al.,_). The paper discusses the key principles of adaptive sampling, the various decisions and considerations required at each step and how it may be useful in ecological monitoring. Please refer to it for more information. The code here, provides a walkthrough of adaptive sampling using a simulated species, the hipporhinostricow. We outline the process of:

1. Using initial data to generate a criterion layer (a surface detailing sampling priorities)
2. Selecting sampling locations from this layer
3. Deciding on the sampling activity - rounds vs batch size

## Code overview

All of the code is written in R v3.4.1 and found in the `scripts/` folder. Only three scripts are needed to carry out the adaptive sampling workflow:

- `as_functions.R` - all functions for adaptive sampling
- `simulate_species.R` - simulate the distribution of hipporhinostricows
- `adaptive_sampling_walkthrough.Rmd` - R markdown document outlining the entire adaptive sampling process 

Prior to starting, data need to be downloaded and placed in a folder called `data` in your working directory. There are three datasets that need to be downloaded (as zipped files):

1. Landcover data from the UKCEH Landcover Map 2015 (https://catalogue.ceh.ac.uk/documents/0255c014-1630-4c2f-bc05-48a6400dd045)
2. Elevation data provided by WorldClim (https://www.worldclim.org/data/worldclim21.html)
3. Climate data provided by WorldClim (https://www.worldclim.org/data/worldclim21.html)

After these have been downloaded, the unzipped folders need to be placed in the `data` subfolder. The `simulate_species.R` script must be run first. This simulates the distribution of hipporhinostricows across Great Britain and prepares the datasets for the rest of the walkthrough. Once this has been run it creates some sub-folders containing all the data needed to run the `adaptive_sampling_walkthrough.Rmd` script. The `adaptive_sampling_walkthrough.Rmd` script also produces all of the figures in Box 1 of the paper.


The name of the simulated species is from a poem by Spike Milligan which was read to me by my nan as a child, and is very special to me:


## The Hipporhinostricow  
by Spike Milligan


Such a beast is the Hipporhinostricow  
How it got so mixed up we'll never know how  
It sleeps all day and whistles all night  
And it wears yellow socks which are far too tight.  


If you laugh at the Hipporhinostricow  
You're bound to get into an awful row  
The creature is protected you see  
From Silly people like you and me.  

  

