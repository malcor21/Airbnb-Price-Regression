## Overview

This subdirectory contains modeling scripts and objects for the submission pipeline.

## Subdirectories

- [`data`](data): resampling objects
- [`pred`]([pred): prediction submissions
- [`recipes`](recipes): recipe objects
- [`results`](results): tuning objects

## Files

- `X1_initial_setup.R`: initial setup script
- `X2_var_Y.R`: variable selection using modeling method Y
- `X3_recipes.R`: recipe creation
- `X4.A_Y_tune.R`: tuning script for method Y, where A denotes a separate variable selection method if applicable
- `X5_model_evaluation.R`: model evaluation
- `X6_final_fitting.R`: model fitting to the testing dataset and submission generation