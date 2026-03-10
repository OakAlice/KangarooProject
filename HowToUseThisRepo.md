This repo is very large and contains many different subsections of code and processing.

*Detailed instructions to be added later*

## Preparing the Data
The data was collected on Artemis boards as part of the [Mobile Biomechanics Lab collar](https://github.com/OakAlice/IntegratedCollarAnalysis) setup. This data has a very specific formatting and can be read in using a specific workflow written by Christofer Clemente and I. During this stage, misreads must be managed, and the GPS clocks used to update the internal accelerometer clock.

## Behavioural Annotation
Because field work is so hard, we accidentally forgot to perform calibration checks on these collars while out in the field (yeeek!). Therefore, we have to align them based on the GPS timestamp matching to the videos. This is pretty good, but not quite perfect. Therefore, we have developed a workflow that allows us to manually clip a section of data to within a few seconds of the likely video start time. These clipped sections are then fed into the custom matlab GUI "SyncStation" and behavioural annotations for each second of the video applied.

## Machine Learning Behavioural Classification
Using the annotated data prepared in the previous step, a supervised machine learning program to detect specific fine scale behaviours is developed. Working on this classification pipeline has been the core of my thesis. This is a streamlined version.

## Dead Reckoning 
Using the data prepared earlier, with the magnetometer, accelerometer, and GPS, we can reconstruct exact movement paths. As we did not calibrate these collars in the field, there may be a few spicy challenges with this section of analysis. 
