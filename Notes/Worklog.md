# Oak Worklog as I move through the analysis
I like to take notes as I work, these are things I observed and decisions I made.
## Reading the files together
* 10/03: Found that the devices had turned on and off as much as 26 times during the deployment. It seems that most of these are maybe the death-throws of the battery. In the big Cahuna data, there was 11 straight days and then 23 times of being turned on for a few seconds. Decided to delete any events that were less than 1 hour (because we couldn't get any meaningful data out of them anyway, really).
