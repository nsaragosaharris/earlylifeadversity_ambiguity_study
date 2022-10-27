import numpy as np
import os
from nilearn.input_data import NiftiMasker
from nilearn import image
from nilearn.masking import apply_mask
import pandas
import glob
from scipy import stats
import re

'''
This script uses Python 3, so make sure that you are using Python 3 and have the correct libraries installed.

This script gets multivoxel estimates of BOLD signal within the a given region of interest for angry, surprised, and happy trials for each participant.

It uses the unsmoothed first level models (Ghost_run1_mod1_unsmoothed.feat, Ghost_run2_mod1_unsmoothed.feat, Ghost_run3_mod1_unsmoothed.feat), which has the three conditions modeled and combined, for each run, for each participant. So at this point, each participant should have whole-brain beta estimates (unsmoothed) that corresponds to their average responses to angry, surprised, and happy faces for each run (three total). Within each ROI, we are going to estimate the overlap in those representations, and then average the correlations across the runs to get a single correlation for each pair for each participant.

Note: Tried using second level models (Ghost_lev2_mod1_unsmoothed.gfeat) but that step transforms out of native space so doing this instead.
Note: Also not creating the raw values csv anymore since the correlations look correct.

Structure of the first level models: /u/project/silvers/data/CTS/CTS_ID/model/Ghost_run*_mod1_unsmoothed.feat/stats/zstat[1-3].nii.gz.

Note: The 'detail' variable used to be the 'threshold' variable, but since the cortical masks (e.g., vmPFC and anterior insula) are not thresholded (because they were not created from Harvard Oxford atlas, but rather downloaded from a meta analysis), I changed it to be more flexible to these different masks. The script now simply requires that all masks are saved in the same directory, with the format region_hemisphere_detail.nii.gz. 

If you are creating masks from existing masks created using the Harvard Oxford atlas, set the 'detail' variable to indicate how the atlas was thresholded when making the mask in FSL and make sure this matches the format of your mask file names (e.g., 'Thr25', 'Thr50', etc.).

Example: accumbens_L_Thr25.nii.gz (region = 'accumbens', detail = 'Thr25', hemisphere = 'L').

If you are creating masks that were not thresholded (e.g., the ones from Xu et al., 2021), instead of a threshold, put the author name here, as that is the format in which it is saved in the masks folder.

Example: anteriorinsula_L_xu2021.nii.gz(region = 'anteriorinsula', detail = 'xu2021', hemisphere = 'L').

This script creates a dictionary to match zstat1, zstat2, and zstat3 to the correct conditions (this matching information is from a design image from an example level 1 feat directory; make sure this is correct!).
zstat1: angry
zstat2: happy
zstat3: surprised

This script creates one correlation csv (all participants in one file).

In the correlation csv, there should be a row for every participant. Each column should be the pair for the correlation.
Columns:
Participant ID,
number_voxels_{region}_{hemisphere}_{detail},
AO_SUR_corr_run1_{region}_{hemisphere}_{detail}, 
AO_SUR_corr_run2_{region}_{hemisphere}_{detail}, 
AO_SUR_corr_run3_{region}_{hemisphere}_{detail},
AO_SUR_corr_average_{region}_{hemisphere}_{detail},
HO_SUR_corr_run1_{region}_{hemisphere}_{detail}, 
HO_SUR_corr_run2_{region}_{hemisphere}_{detail}, 
HO_SUR_corr_run3_{region}_{hemisphere}_{detail},
HO_SUR_corr_average_{region}_{hemisphere}_{detail},
AO_HO_corr_run1_{region}_{hemisphere}_{detail}, 
AO_HO_corr_run2_{region}_{hemisphere}_{detail}, 
AO_HO_corr_run3_{region}_{hemisphere}_{detail},
AO_HO_corr_average_{region}_{hemisphere}_{detail}

'''

# Function to mask data.
def setMasker(msk, smth):
    out = NiftiMasker(mask_img=msk, smoothing_fwhm=smth,
                      standardize=False, memory="nilearn_cache", memory_level=1)
    return (out)

# Dictionary matching zstat directories to conditions.
zstat_dict = {'zstat1':'angry', 'zstat2':'happy', 'zstat3':'surprised'}
zstats = ['zstat1','zstat2','zstat3']

studydir = '/u/project/silvers/data/CTS'
outputdir = '/u/home/n/nsaragos/scripts/cts/rsa_conditionlevel_byparticipant/correlations'

participants = ['CTS001','CTS003','CTS004','CTS006','CTS009','CTS011','CTS012','CTS014','CTS019','CTS023','CTS026','CTS028', 'CTS030','CTS031','CTS034','CTS038','CTS039','CTS042','CTS044','CTS047','CTS053','CTS054','CTS057','CTS058', 'CTS060','CTS062','CTS070','CTS073','CTS074','CTS075','CTS077','CTS079','CTS081','CTS083','CTS084','CTS088','CTS096','CTS098','CTS099','CTS100','CTS101']
#participants = ['CTS001']
region = 'V1' # Input mask of interest here. Make sure it matches how your masks are saved (e.g., 'amy' for 'amygdala', etc.).
detail = 'Thr75' # Make sure this matches the format of your mask files (e.g., 'Thr25', 'xu2021', etc.; see earlier notes for more information).
hemisphere = 'L' # Usually, 'bilateral', 'R', or 'L'. If you want to make all three, change and run script for each separately.

# Define column names for the correlation csv.
correlation_colnames = ['Participant_ID',f'number_voxels_run1_{region}_{hemisphere}_{detail}',f'number_voxels_run2_{region}_{hemisphere}_{detail}', f'number_voxels_run3_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_run1_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_run2_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_run3_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_average_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_run1_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_run2_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_run3_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_average_{region}_{hemisphere}_{detail}', f'AO_HO_corr_run1_{region}_{hemisphere}_{detail}', f'AO_HO_corr_run2_{region}_{hemisphere}_{detail}', f'AO_HO_corr_run3_{region}_{hemisphere}_{detail}', f'AO_HO_corr_average_{region}_{hemisphere}_{detail}']

correlation_df = pandas.DataFrame(columns = correlation_colnames)

runs = ['run1','run2','run3']
'''
Loop 1: Go through every participant.
From their model folder, save the mask that only includes gray matter.

Loop 2: Go through each run for the participant.
From their model folder, save the mask that only includes gray matter.

Loop 3: Go through every condition. Use the dictionary to figure out, from the zstat information (zstat1, zstat2, or zstat3), which emotion condition it is (AO, HO, or SUR).
'''
for p in participants:
    print(f'Processing participant {p}.')
    # Add participant ID to next row of correlation_df.
    correlation_df.at[len(correlation_df),"Participant_ID"] = p
    for run in runs:
        print(f'{run}.')
        # The mask we are using here is the run-specific mask that takes into account the gray matter segmentation.
        mask = glob.glob(f'{studydir}/{p}/model/masks/masks_from_conditionlevel_models/unsmoothed/{run}_{region}_{hemisphere}_{detail}_functional_GMONLY.nii.gz')
        num_voxels = os.popen(f'fslstats {mask[0]} -V').read() # Get size of the mask (how many voxels) using fslstats.
        num_voxels = num_voxels.split(' ')[0] # Get value before the space (FSL prints as voxel, then space, then weird dimension thing that is not useful).
        num_voxels = int(num_voxels)
        # Save the number of voxels for this run-specific mask to the correct column (current run) in the correlation file.
        correlation_df.loc[correlation_df['Participant_ID'] == p,f'number_voxels_{run}_{region}_{hemisphere}_{detail}'] = num_voxels
        # Using the file names, load the mask as an image.
        mask_image = image.load_img(mask)
        # It will give a warning because smoothing is set to 0, which is fine because we do not want to smooth in order to keep the small scale spatial information of each voxel.
        masker = setMasker(mask_image, 0) # Note: Could make this smoothing parameter a variable that is defined at the top of the script (and also included as a column in the csv file and in the title of the csv file) as well.
        for zstat in zstats:
            condition = zstat_dict.get(f'{zstat}') # for the given zstat, use that key to get the value (emotion condition).
            print(f'Loading {zstat}, which corresponds to the {condition} condition.')
            # There should be zstat (in the current run) that corresponds to this emotion condition.
            zstat_image = glob.glob(f'{studydir}/{p}/model/Ghost_{run}_mod1_unsmoothed.feat/stats/{zstat}.nii.gz') # Should be one for zstat1, zstat2, and zstat3.
            if len(zstat_image) != 1: # Check that there are is a zstat file.
                print("Error: This condition does not have a zstat.")
                continue
            # Load in zstat image.
            curr_condition_img = image.load_img(zstat_image)

            # Using the masks created before with the masker function, transform the image to only have the values within that mask.
            curr_condition_z = masker.fit_transform(curr_condition_img)
            
            # Save current condition to correct variable to use later in correlation.
            if condition == 'angry':
                AO_condition_z = curr_condition_z
            elif condition == 'surprised':
                SUR_condition_z = curr_condition_z
            elif condition == 'happy':
                HO_condition_z = curr_condition_z
            else:
                print("Error: Could not match to a given condition.")

        # Now, for the current participant, in the current run, there should be a surprised, angry, and happy face array (masks's z statistic values).
        # Based on Lila Davachi's slides, let's z score within the ROI to account for mean activation.
        AO_condition_z_score = stats.zscore(AO_condition_z[0,], ddof = 1)
        SUR_condition_z_score= stats.zscore(SUR_condition_z[0,], ddof = 1)  
        HO_condition_z_score= stats.zscore(HO_condition_z[0,], ddof = 1)

        # Now correlate across conditions.
        AO_SUR_corr_zscore = np.corrcoef(AO_condition_z_score, SUR_condition_z_score)[0, 1]
        HO_SUR_corr_zscore = np.corrcoef(HO_condition_z_score, SUR_condition_z_score)[0, 1]
        AO_HO_corr_zscore =  np.corrcoef(AO_condition_z_score, HO_condition_z_score)[0, 1]

        # Now, find that participant's row and add the rest of their data to the corresponding columns for the current run.
        correlation_df.loc[correlation_df['Participant_ID'] == p,f'AO_SUR_corr_{run}_{region}_{hemisphere}_{detail}'] = AO_SUR_corr_zscore
        correlation_df.loc[correlation_df['Participant_ID'] == p,f'HO_SUR_corr_{run}_{region}_{hemisphere}_{detail}'] = HO_SUR_corr_zscore
        correlation_df.loc[correlation_df['Participant_ID'] == p,f'AO_HO_corr_{run}_{region}_{hemisphere}_{detail}'] = AO_HO_corr_zscore

# Once all participants have been added, average across runs for each correlation value (pair) for every participant
# and save to the correction columns (HO_SUR_corr_average_{region}_{hemisphere}_{detail}, etc.).
correlation_df[f'AO_SUR_corr_average_{region}_{hemisphere}_{detail}'] = correlation_df[[f'AO_SUR_corr_run1_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_run2_{region}_{hemisphere}_{detail}', f'AO_SUR_corr_run3_{region}_{hemisphere}_{detail}']].mean(axis=1)

correlation_df[f'HO_SUR_corr_average_{region}_{hemisphere}_{detail}'] = correlation_df[[f'HO_SUR_corr_run1_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_run2_{region}_{hemisphere}_{detail}', f'HO_SUR_corr_run3_{region}_{hemisphere}_{detail}']].mean(axis=1)

correlation_df[f'AO_HO_corr_average_{region}_{hemisphere}_{detail}'] = correlation_df[[f'AO_HO_corr_run1_{region}_{hemisphere}_{detail}', f'AO_HO_corr_run2_{region}_{hemisphere}_{detail}', f'AO_HO_corr_run3_{region}_{hemisphere}_{detail}']].mean(axis=1)

# Save the correlation_df to a csv.
correlation_df.to_csv(f'{outputdir}/ROI_zscored_correlations/allparticipants_{region}_{hemisphere}_correlations_ROIzscored.csv')


# From https://nilearn.github.io/auto_examples/01_plotting/plot_visualization.html#sphx-glr-auto-examples-01-plotting-plot-visualization-py.
# Are apply_mask and masker.fit_transform the same?
#masked_data = apply_mask(curr_condition_img, mask_image)
# Look at the length of the array; does it match the number of voxels for that participant's functional mask?
# len(masked_data[0]) # So far, yes, this does seem correct.
# len(curr_condition_z[0])
# It is a 2D array because this function applies the mask for each time point, but there is only one timepoint, so just index the 'first' (0th) array.
