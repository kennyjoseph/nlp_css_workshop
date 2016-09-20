This is the repository for the code and data for the following article. Please reference it if you use the data and shoot me an email so I know its being used!

```
@inproceedings{joseph_relating_2016,
    title = {Relating semantic similarity and semantic association to how humans label other people},
    booktitle = {NLP+CSS Workshop at EMNLP 2016},
    author = {Joseph, Kenneth and Carley, Kathleen M.},
    year = {2016}
}
```

# Code/Replication

Open up ```nlp_css_workshop.Rproj``` in RStudio to replicate the results, all of which can be run from the ```analysis.R``` script. 

# Data

The survey data is in different fields for questions, respondent demographics and answers, but can be combined by running the function ```get_simplex_basic()``` in the file ```util.R```, as is done in the ```analysis.R``` code. After running the line of code, the fields in the dataset will be as follows:

- responseid - Unique (anonymized) ID of the respondent
- questionid - Unique ID for the question being asked 
- questiontype - Type of question - "IsA" or "SeenWith"
- query - The identity presented in the question text 
- simlexpair - The element in the Simlex identity pair presented as an answer. Note that by chance, there may be multiple pairs in a single question. All are given a unique row in this file
- answer1 - The first answer presented for this question
- answer2 - The second answer presented for this question
- answer3 - The third answer presented for this question
- answer4 - The fourth answer presented for this question
- answer5 - The fifth answer presented for this question
- answer - The answer selected by the survey respondent
- embeddeddata - Irrelevant
- startdate - Time the respondent started the survey
- enddate - Time the respondent ended the survey
- gender - Gender of the respondent
- age - Age of the respondnet
- hispanic - Is this respondent of Hispanic descent?
- race1 - Race/Ethnicity of respondent 
- race2 - Optional answer for additional Race/Ethnicity of respondent
- borninus - Was the respondent born in the U.S.?
- percentinus - What percentage of the respondent's life has been lived in the U.S.?
- wherelivedlongest - Where has the respondent lived the longest?
- wherelivedrecently - Where has the respondent lived the most recently?
- education - Level of education of the respondent
- political - Political leaning of the respondent
- survey - Which iteration of this first survey; was given in three chunks (irrelevant)


