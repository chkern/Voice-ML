## Overview

Publication: The Sound of Respondents: Predicting Respondents’ Level of Interest with Voice Data in Smartphone Surveys

Author(s): {author name(s)}{contact information}

Last update: 2022-03-14 

Description: Web surveys completed on smartphones open novel ways for measuring respondents’ attitudes, behaviors, and beliefs that are crucial for social science research and many adjacent research fields. In this study, we make use of the built-in microphones of smartphones to record voice answers in smartphone surveys and extract non-verbal cues, such as amplitudes and pitches, from the collected voice data. This allows us to infer respondents’ level of interest, which may expand the opportunities for researching respondents’ survey engagement and answer behavior. We conducted a smartphone survey in a German online access panel and asked respondents four open-ended questions on political parties with requests for voice answers. In addition, we measured respondents’ self-reported survey interest using a closed-ended question. The results show a non-linear association between respondents’ predicted level of interest and answer length. Respondents with a predicted medium level of interest provide longer answers in terms of number of words and response times. However, respondents’ predicted level of interest and their self-reported interest are only weakly associated. Finally, we argue that voice answers contain rich meta-information about respondents’ affective states which are yet to be utilized in survey research.

Keywords: Answer behavior, Interest prediction, Microphone, Natural Language Processing, Open-ended questions, Smartphone, Voice recordings

## Files and directories

- 00_predict.sh
- 01_run_predict
- 02_data_prep.R
- MainAnalysis/01_1_surveyDataCleaning.R
- MainAnalysis/01_2_sentimentScores.R
- MainAnalysis/01_3_durationWAV.R
- MainAnalysis/01_4_dataImportCleaning.R
- MainAnalysis/02_analysisDesc.R
- MainAnalysis/03_mainAnalysis.R

## Software

- R (version 4.0.3), with add-on packages quanteda (version 3.0.0), lme4 (version 1.1-27) and ordinal (version 2019.12-10).

## Licence 

Creative Commons Attribution 4.0 International https://creativecommons.org/licenses/by/4.0/
