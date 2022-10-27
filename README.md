# earlylifeadversity_ambiguity_study

This repository contains the code (task and analysis) for a study examining how early life adversity (ELA) relates to neurobehavioral responses to ambiguity and threat.

*Corresponding paper.*

**Title: Early life adversity is associated with greater similarity in neural representations of ambiguous and threatening stimuli.**

**Authors: Natalie Saragosa-Harris, João F. Guassi Moreira, Yael H. Waizman, Anna Sedykin, Tara S. Peris+, and Jennifer A. Silvers+.
+Equal author contribution.**


The representational similarity analysis script (nilearnscript_conditionlevel) uses nilearn, a Python package (https://nilearn.github.io/stable/index.html).
1. If you are getting errors about libraries not loading, you probably have not installed the necessary requirements. To do so, type this: pip install -r requirements.txt
2. Note: If the virtual environment doesn’t seem to be working, this is how you delete it and create a new one.
rm -rf venv
module load python/3.7.0
python3.7 -m venv venv/
. venv/bin/activate
pip install -r requirements.txt