# Data and Programming for Public Policy II
# PPHA 30536


## Final Project: Reproducible Research
## Winter 2021


## Due: Wednesday, March 17th by midnight on GitHub

### Project Description:
The goal of this project is to showcase your knowledge of R by applying it to a research project of your own.  This is not a research methods course, so the quality of the
research is ancillary to the quality of your programming.  As a baseline your project should have the following major components:

  1. Data wrangling
  
You must use a minimum of three datasets.  All processing of the data should be handled by your code, including all merging and reshaping.  This is where you can showcase your abilities practiced in homework 1.

  2. Plotting
  
From that data, you will create a minimum of four plots, at least one of which must be interactive.  The skills used here will roughly correspond to your work on homework 2.

  3. Text processing

You will now introduce some form of text analysis, similar to that of homework 3.  This may be separate data from what you created in part 1, and the results of it may be used in your plotting or analysis.

  4. Analysis
  
Then you will fit a model to your data and report basic results.  As this is not a statistics or econometrics class, the model you choose and the validity of your results are not terribly important; fitting an OLS model that has insignificant p-values will do.  The goal is to show you can prepare your data through the previous steps to have it ready for model fitting.

  5. Writeup
  
You will then spend *no more than 2-5 pages* writing up your project.  You should describe your research question, then discuss the approach you took and the coding involved, including discussing any weaknesses or difficulties encountered.  Finish with a brief discussion of results, and how this could be fleshed out in future research.  The primary purposes of this writeup is to inform me of what I am reading before I look at your code.

### Instructions
You may work on this project alone, or in [groups of two](https://github.blog/2018-03-06-how-to-use-group-assignments-in-github-classroom/).  All groups must be formed *on GitHub classrooms* before any work is done - it is not possible to join a group after.  If you work alone, then at least one of the first three parts above must be of a more advanced nature that exhibits your coding skills.  If you work in a pair, then two of the parts must be more advanced.  The exact nature of what makes something more advanced will be discussed in class; imagine, for example, the difference between standard ggplot output with default settings, and output that has many elements adjusted to improve the display.

It is required that you use GitHub, and I will use your past commits to understand your thought process for partial credit, and to monitor group participation.  You will not get full credit if your repository does not show multiple commits as you build your project, especially for groups.  Expectations for the scope of the project will be higher for groups than for individuals, and the division of labor will be up to each group.  Note again that I will be using your GitHub commit history in grading, so while I will lean toward giving the same grade to everyone in a group, it is possible that group members will recieve different grades.

Your final repository must contain the following: 
1. Your R code and commit history
2. The initial, *unmodified* dataframes you download, unless they are large, in which case they should be hosted on Drive or Dropbox with a link provided
3. Saved .png versions of your static plots
4. The final versions of the dataframe(s) you built (with the same caveat as in 2)
5. Your writeup (Word or markdown are both fine)

### Suggestions and Tips
 * I encourage you to create a project on a subject that is relevant to your interests, and other Harris classes.  If your research idea is a much larger project, think of how you can develop a basic framework for it using this project, which can then later be expanded into a proper research project.
 * The exact components of your project are negotiable; use the five steps above as a starting point, and contact me for discussion and approval if you wish to do something differently.
 * If you feel stuck coming up with research ideas, feel free to contact me or one of the TAs so we can discuss your interests and make suggestions.
 * When you have a research idea, please send me a short, informal proposal describing the idea so we can make sure you're on the right track before you begin work.
 * You may use libraries and methods we did not go over in class, but ones that we did go over should be preferred if they duplicate the functionality.  Remember all citation rules from the academic dishonesty policy in the syllabus.
 * Effort put into organizing your code and making it readable, by, for example, following the Tidyverse Style Guide, and good usage of diplyr, functions, variable names, and comments will be rewarded.
 * The entire point of reproducible research is to make it possible for others (and for a future you who has had time to forget what you did and why) to understand, replicate, and modify your work.  Keeping this in mind as you work will be good for your grade, and helpful to you in the future if you expand on the project.
