# earlylifeadversity_ambiguity_study

This repository contains the code (task and analysis) for a study examining how early life adversity (ELA) relates to neurobehavioral responses to ambiguity and threat.

*Corresponding paper.*

**Title: Early life adversity is associated with greater similarity in neural representations of ambiguous and threatening stimuli.**

**Authors: Natalie Saragosa-Harris, João F. Guassi Moreira, Yael H. Waizman, Anna Sedykin, Tara S. Peris+, and Jennifer A. Silvers+.
+Equal author contribution.**

The stimuli for the fMRI task and post-scan behavioral task (code is located in scripts/task) are from the Racially Diverse Affective Expression (RADIATE) face stimulus set (https://www.sciencedirect.com/science/article/pii/S0165178117321893), an open access dataset available in the supplemental materials for the study. 

The representational similarity analysis script (in scripts/rsa_scripts) is written in Python 3 and uses nilearn (https://nilearn.github.io/stable/index.html), a Python library.

1. If you are getting errors about libraries not loading, you probably have not installed the necessary requirements. To do so, type this: pip install -r requirements.txt
2. Note: If the virtual environment doesn’t seem to be working, this is how you delete it and create a new one.
rm -rf venv
module load python/3.7.0
python3.7 -m venv venv/
. venv/bin/activate
pip install -r requirements.txt
