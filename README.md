# RevisionEvent extraction
Scripts from the manuscript titled "Automated extraction of revision events from keystroke logs" (under review)
by Rianne Conijn, Emily Dux Speltz, and Evgeny Chukharev-Hudilainen

Extraction of revision events for Inputlog and CyWrite data. Code can be adapted to other keystroke logging/eye tracking applications. Feel free to email me if you have any questions about the code. In most cases, the case_study.R file will be enough to run the model on a new dataset. All other files are provided as a reference to show how the models are created.

_DISCLAIMER:_ 

_For the manual & automated annotation: The rule-based approximation for Inputlog data uses the location of the character in the text (positionFull), as identified by Inputlog. For longer texts or texts with high recursiveness, the positionFull can be one character off (Inputlog versions 8 and lower). This can result in some small inconsistencies in the content of the revision events._

_For the automated annotation: The machine learning models are all trained and tested on the dataset described in the manuscript using the scripts below. The accuracy for the revision start is 99.6%. The accuracy for the revision end is 71% (83% for one character off). The performance is expected to be worse on datasets that are highly different from the dataset used for training. Hence, we suggest the researcher to take caution when interpreting the results and, where necessary, perform some manual checks._

## Inputlog data:
**Load data:**

`load_data_inputlog.R` Combines inputlog XML files into one csv file.

in:  general.zip, summary.zip; out: all_logs.csv, all_summary.csv
     
**Manual annotation & pre-processing data:**

`rule_based_inputlog.R` Transforms Inputlogs keystroke data into the revision annotation table, which can be used for manual annotation or automated analysis of revision start/end using machine learning (via `run_ml_models.R`).

in:  all_logs.csv; out: revision_annotation.csv _for manual annotation_, all_logs_extended.csv _for automated annotation_

**Automated annotation:**

`run_ml_models.R` Runs the best performing machine learning model on Inputlog data to automatically identify revision events. For an example including additional visualizations and interpretations, see `case_study.R` (on CyWrite data).

in: all_logs_extended.csv; out: revisions_inputlog_ml.csv

## CyWrite data:
**Load data:**

`load_data_cywrite.R` Combines CyWrite JSON files into one csv file.

in:  automatic.zip, fixations.zip; out: all_keystrokes.csv, all_fixations.csv

**Manual annotation & pre-processing data:**

`get_revision_end.R` Adds the manually annotated revision end to the data.

in:  all_keystrokes.csv; out: all_rev_end.csv

`add_fixation_data.R`
Pre-processes fixation data into saccades and adds it to the dataset with annotations of revision_end. 
This step may be ignored if you do not have eye fixation data.

in:  all_rev_end.csv, all_fixations.csv ; out: all_eye_keys.csv

**Training machine learning models of Revision events:**

`predict_revision_start.R` Trains several machine learning model to predict revision start, based on the annotated dataset.

in:  all_keystrokes.csv; out: several machine learning models

`predict_revision_end.R`
Trains several machine learning model to predict revision end, based on the annotated dataset.

in:  all_eye_keys.csv; out: several machine learning models 

`interpret_revision_end.R` Several functions to explore the performance of the machine learning models, including plots and feature importance.

**Automated annotation:**

`case_study.R` Example to run the best performing machine learning model on unseen CyWrite data, including visualizations as used in the case study described in the manuscript.

in:  automatic.zip & fixations.zip; out: revisions_ml.csv, several visualizations
