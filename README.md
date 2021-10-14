# MELVI
Marine Biological Laboratory (MBL) Multiple Element Limitation Model (MEL)

The Multiple Element Limitation (MEL) model simulates changes in carbon (C), nitrogen (N), and phosphorus(P) stocks in and fluxes among plant biomass, Phase I and Phase II (Melillo et al. 1989) soil organic matter (SOM), detritus, and inorganic nutrients as well as changes in soil water content. Daily driving variables are atmospheric CO_2, maximum and minimum air temperature, precipitation, total shortwave radiation, and nutrient inputs.

The MEL model couples ecosystem C, N, P, and water cycles and generates output for all stocks and fluxes on a daily time step. The differential equations that describe the mass balance for each of the simulated components of the ecosystem are solved numerically using a 4th-5th order Runge-Kutta integrator with a time-step size that adapts with each pass through the integrator to optimize precision and computation time (Press et al. 1986).  The model is coded in Lazarus 2.0.12 (2020) Free Pascal and runs on a PC or Mac computer.

Version VI of the MEL model, includes improvements to the plant resource acquisition algorithm described in Rastetter et al. (2013) for version IV.  The major modification is a hierarchical allocation scheme for resource-acquisition effort (Rastetter and Kwiatkowski 2020).  We define effort allocated toward a particular resource as the fraction of all vegetation assets (e.g., allocation to leaf or root tissue, enzyme production, carbohydrate expenditure) that can be allocated toward the acquisition of resources from the environment.  We assume that these assets increase as biomass increases and can be incrementally reallocated among resources.  Effort is first allocated toward acquisition of C, N, and P based on the relative amounts needed to replace losses (litterfall and respiration) and to correct any stoichiometric imbalance.  The primary allocation of N effort is then partitioned into sub-effort allocated toward NH4, NO3, DON, and symbiotic N fixation based on the highest relative return of N per unit effort expended.  Similarly, the primary C effort is subdivided into sub efforts allocated toward CO2, light, and water acquisition based on the highest relative increase in photosynthesis per unit effort expended.  All else being equal, resource acquisition increases monotonically both with biomass and with the effort allocated toward that resource.


The full MEL model is desribed in the file, MELVI.txt. 

----
### Publications 



--------------------------------------------------------------------------
### Code Instructions 

The MEL model is written for modelshell, a model develepment package, which is written in Lazarus/Free Pascal. Modelshell allows the user to create a model by making creating a plain text file description of the model. Modelshell provides a GUI interface, an integrator, file IO, and a simple graph. All output files created by modelshell are comma delimited text files.

Detailed instructions for compiling and running the model are in "Install and Run MEL.docx"

--------------------------------
### Funding 

