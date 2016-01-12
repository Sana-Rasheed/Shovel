# Shovel
## Motivation
There have been two big ideas undermining the company, combining open data and prediction. My work on the census enables bringing these ideas together to construct a one big demographic dataset.

I created a pipeline for the application of statistical models to public use microdata samples (pums) census data to predict new attributes. 

So far this has allowed predicting the row's (people's) county with much greater precision. However, we can predict more then just counties. With the right data we can predict whether the person is a republican or democrat or whether they went to the hospital in the past year.

The key being *the person*. When we can say if a person is republican, we also know that a 50 year-old electrician making 80k a year who commutes 30min to work on the bus is a republican. Of course, we can't really be sure that is the case as the data has been made with keeping that impossible. But we have a better shot about being right saying there is about 100 people similar to him living in this specific county. 

Here I have developed a plan for the combining of open data.
