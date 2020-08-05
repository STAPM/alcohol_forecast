# Forecast drinking trends

The code in this repo gives a worked example of how to use the STAPM modelling to forecast trends in alcohol consumption. It provides a complete workflow, i.e. the code files perform all steps linking the raw data to the model outputs. This code repo is also being used for model development, so the code in it will be changing as development progresses.  

Only members of our project team are able to run the code in this repo - because we use a number of private R packages.   

The code in this repo projects the proportion of drinkers and distribution of alcohol consumed by drinkers from 2011 to 2025 (but the projection could easily extent to later years). The forecast of the proportion of drinkers is based on HSE 2001-2017 and the forecast of the distribution of consumption is based on the HSE 2011-2017, since 2011 was the start of usable data recording.     

The next steps in development are to:  

- develop the method used to construct the synthetic population, to add calibration of the proportions of drinkers in the sample.  
- add correlation terms that control the dynamics of individual lifecourse drinking trajectories.  


