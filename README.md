# CompNeuro_axonGuidance
A few theoretical simulations I made as a part of my PhD coursework.

Folder contents:

Theoretical Guidance Cue Synergy: 
    - Two PDFs containing figures and text for the simulations I performed.

CueX_2E_1I_2RR_4xCue_1000reps_csv.csv: 
    - Two csv spreadsheets (Cue1 and Cue2) containing poorly formatted data exported from Netlogo behaviour space

** R_load_spreadsheet_data_netlogo_summarize.ipynb: 
    - A Jupyter notebook running the R kernel, where I ported the code from 'readNetlogoSpreadsheets_general.R' to be presented more      clearly, and better organized. 

readNetlogoSpreadsheets_general.R: 
    - An R script to load data from the above spreadsheets and format it appropriately. 

axonSimulations.R: 
    - Simulation of axon guidance and measurement of turned angles. This script is used to generate the data from Figure 3(A-D) and Figure 4(C-D). This script takes a long time to run.






Some notes on the original work this is based on.

My thesis was at the intersection of theoretical neurodevelopment and computational neuroengineering (or any combination of those sexy descriptors).
I spent most of my time writing custom analysis macros in ImageJ and Matlab that aren't much use to anyone. 
However, I did make some simulations and perform some theoretical analysis as a part of my coursework that were related to but not included in my thesis or publication.

My first author Plos Biology paper 'Integration of Shallow Gradients of Shh and Netrin-1 Guides Commissural Axons'can be found here:
http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1002119

While my thesis in its entirety (how daring of you) can be found here: 
http://digitool.library.mcgill.ca/webclient/DeliveryManager?pid=141289&custom_att_2=direct

The topic of my research was the guided growth of neurons towards targets. The growth cone at the tip of growing axons are guided by concentration gradients of attactive and repulsive guidance cues. 
I developped a microfluidic assay to measure axon turning in response to combinations of guidance cues. 
These experiments created a wealth of data, that took a computational skillset to analyze. 
These experiments also beared on theoretical questions (largely work from the Goodhill lab), and it is these theoretical questions I'm examining in this folder.

