# MELVI
Marine Biological Laboratory (MBL) Multiple Element Limitation Model (MEL)

The Multiple Element Limitation (MEL) model simulates changes in carbon (C), nitrogen (N), and phosphorus(P) stocks in and fluxes among plant biomass, Phase I and Phase II (Melillo et al. 1989) soil organic matter (SOM), detritus, and inorganic nutrients as well as changes in soil water content. Daily driving variables are atmospheric CO2, maximum and minimum air temperature, precipitation, total shortwave radiation, and nutrient inputs.

The MEL model couples ecosystem C, N, P, and water cycles and generates output for all stocks and fluxes on a daily time step. The differential equations that describe the mass balance for each of the simulated components of the ecosystem are solved numerically using a 4th-5th order Runge-Kutta integrator with a time-step size that adapts with each pass through the integrator to optimize precision and computation time (Press et al. 1986).  The model is coded in Lazarus 2.0.12 (2020) Free Pascal and runs on a PC or Mac computer.

Version VI of the MEL model, includes improvements to the plant resource acquisition algorithm described in Rastetter et al. (2013) for version IV.  The major modification is a hierarchical allocation scheme for resource-acquisition effort (Rastetter and Kwiatkowski 2020).  We define effort allocated toward a particular resource as the fraction of all vegetation assets (e.g., allocation to leaf or root tissue, enzyme production, carbohydrate expenditure) that can be allocated toward the acquisition of resources from the environment.  We assume that these assets increase as biomass increases and can be incrementally reallocated among resources.  Effort is first allocated toward acquisition of C, N, and P based on the relative amounts needed to replace losses (litterfall and respiration) and to correct any stoichiometric imbalance.  The primary allocation of N effort is then partitioned into sub-effort allocated toward NH4, NO3, DON, and symbiotic N fixation based on the highest relative return of N per unit effort expended.  Similarly, the primary C effort is subdivided into sub efforts allocated toward CO2, light, and water acquisition based on the highest relative increase in photosynthesis per unit effort expended.  All else being equal, resource acquisition increases monotonically both with biomass and with the effort allocated toward that resource.


The full MEL model is described in the file, MELVI.txt.

----
### Publications
Rastetter EB, and BL Kwiatkowski. 2020. An approach to modeling resource optimization for substitutable and interdependent resources. Ecological Modelling 425: 109033. DOI: /10.1016/j.ecolmodel.2020.109033

Rastetter EB, BL Kwiatkowski, DW Kicklighter, A Barker Plotkin, H Genet, JB Nippert, K O'Keefe, SS Perakis, S Porder, SS Roley, RW Ruess, JR Thompson, WR Wieder, K Wilcox, RD Yanai. in press. N and P constrain C in eocsystems under climate change: role of nutrient redistribution, accumulation, and stoichiometry. Ecological Applications.

Rastetter, E., B. Kwiatkowski, D. Kicklighter, A. Barker Plotkin, H. Genet, J. Nippert, K. O'Keefe, S. Perakis, S. Porder, S. Roley, R. Ruess, J. Thompson, W. Wieder, K. Wilcox, and R. Yanai. 2022. Steady state carbon, nitrogen, phosphorus, and water budgets for twelve mature ecosystems ranging from prairie to forest and from the arctic to the tropics ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/b737b5f0855aa7afeda68764e77aec2a.

Rastetter, E., B. Kwiatkowski, D. Kicklighter, A. Barker Plotkin, H. Genet, J. Nippert, K. O'Keefe, S. Perakis, S. Porder, S. Roley, R. Ruess, J. Thompson, W. Wieder, K. Wilcox, and R. Yanai. 2022. Ecosystem responses to changes in climate and carbon dioxide in twelve mature ecosystems ranging from prairie to forest and from the arctic to the tropics ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/7ca56dfbe6c9bedf5126e9ff7e66f28d.


--------------------------------------------------------------------------
### Code Instructions

The MEL model is written for Modelshell, a model development package, which is written in Lazarus/Free Pascal. Modelshell allows the user to create a model by making creating a plain text file description of the model. Modelshell provides a GUI interface, an integrator, file IO, and a simple graph. All files used and created by Modelshell are ASCII text files.

Detailed instructions for compiling and running the model are in "Install and Run MEL.docx"

--------------------------------
### Funding
This work was supported in part by the National Science Foundation under NSF grants 1651722 and 1637459. Any opinions, findings and conclusions or recommendations expressed in this material are those of the authors and do not necessarily reflect those of the National Science Foundation.
