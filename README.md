# RevisionEvent extraction
Scripts from the manuscript titled "Automated extraction of revision events from keystroke logs" (under review)
by Rianne Conijn, Emily Dux Speltz, and Evgeny Chukharev-Hudilainen

Extraction of revision events for Inputlog and CyWrite data. Code can be adapted to other keystroke logging/eye tracking applications. Feel free to email me if you have any questions about the code. In most cases, the case_study.R file will be enough to run the model on a new dataset. All other files are provided as a refernce to show how the models are created.

## Inputlog data:

`load_data_inputlog.R` 

Combines inputlog XML files into one csv file.

in:  general.zip, summary.zip 
     
out: all_logs.csv, all_summary.csv
     
	 
`rule_based_inputlog.R`
Transforms Inputlogs keystroke data into the revision annotation table, which can be used for manual annotation or 
automated analysis of revision start/end using machine learning (via case_study.R).

in:  all_logs.csv

out: revision_annotation.csv


## Cywrite data:
`load_data_cywrite.R`
Combines CyWrite JSON files into one csv file.

in:  automatic.zip, fixations.zip

out: all_keystrokes.csv, all_fixations.csv

`get_revision_end.R`
Adds the manually annotated revision end to the data.

in:  all_keystrokes.csv

out: all_rev_end.csv

`add_fixation_data.R`
Pre-processes fixation data into saccades and adds it to the dataset with annotations of revision_end. 
This step may be ignored if you do not have eye fixation data.

in:  all_rev_end.csv, all_fixations.csv

out: all_eye_keys.csv

## Modeling of Revision events
`predict_revision_start.R`
Runs several machine learning model to predict revision start, based on the annotated dataset.

in:  all_keystrokes.csv

out: several machine learning models

`predict_revision_end.R`
Runs several machine learning model to predict revision end, based on the annotated dataset.

in:  all_eye_keys.csv

out: several machine learning models (see models folder)

`interpret_revision_end.R`
Several functions to explore the performance of the machine learning models, including plots and feature importance.

# Apply model on new dataset
`case_study.R`
in: CyWrite:  automatic.zip & fixations.zip

In: Inputlog: revision_annotation.csv
	

