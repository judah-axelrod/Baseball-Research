The full blog post can be viewed here: https://someballparkfigures.wordpress.com/2020/04/28/how-we-evaluate-situational-pitching/

For the sake of brevity, the part of the code related to obtaining and importing the raw data is omitted.

More Information about the raw data: 

1. *Run Values 2019.ipynb* utilizes Retrosheet Play-by-Play data from 2019. 
     - Information about how to download the data can be found in Appendix A of *Analyzing Baseball Data with R* by Max Marchi, Jim Albert, and Ben Baumer or on retrosheet.org

2. *Situational Pitching to Contact.Rmd* utilizes the following data:
     - Pitch-level data from Baseball Savant pulled into a MySQL database and then queried for the analysis. For more information, please review Bill Petti's baseballr package.
     - Fangraphs Leaderboards data, accessible from Fangraphs.com
