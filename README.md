**Description** 

These scripts are designed to evaluate and verify the performance of the two-stage Rosenbrock solver (ROS2) against the original implicit solver in E3SMv1.

**Prerequisites**

Anyone who wants to use these will need to obtain the E3SMv1 code from the official repository: https://github.com/E3SM-Project/E3SM
This implementation is branch-specific due to the fixed chemical mechanism 
For using chemUCI: used branch tangq/atm/UCI-chem_Jan2021
For using trop_strat_mozart_mam4: used branch tangq/atm/trop_strat_mam4_resus_mom

Due to the fixed number of components and reactions, the code can be used only in these two branches.

**Implementation**

After configuring the E3SMv1, replace the original source code in the E3SM directory with the files provided in this repository based on the numerical scheme setup.
 For using the original implicit solver (chemUCI_IMP for chemUCI, trop_strat_IMP for trop_strat_mozart_mam4)
 For using the implicit solver for 180s (chemUCI_180 for chemUCI, trop_strat_180 for trop_strat_mozart_mam4)
 For using the two-stage Rosenbrock (chemUCI_ROS2 for chemUCI, trop_strat_ROS2 for trop_strat_mozart_mam4)
