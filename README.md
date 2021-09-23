# lockdown_impacts_PM2.5

Repository for code and data used in the paper entitled "Differential Impacts of COVID-19 Lockdowns on PM<sub>2.5</sub> across the United States".
The preprint of this paper can be found on [medRxiv](https://www.medrxiv.org/content/10.1101/2021.03.10.21253284v1).

[`data_cleaning.R`](https://github.com/kevinleec/lockdown_impacts_PM2.5/blob/main/data_cleaning.R) contains code for cleaning and preprocessing PM<sub>2.5</sub> data as well as meteorological, socioeconomic, demographic, emissions, and mobility data.

[`att.R`](https://github.com/kevinleec/lockdown_impacts_PM2.5/blob/main/att.R) contains code for estimating counterfactual PM<sub>2.5</sub> concentrations during the lockdown period, as well as creating point-level and IDW-smoothed maps. Also contains sensitivity analyses for ATT estimation.

[`regression.R`](https://github.com/kevinleec/lockdown_impacts_PM2.5/blob/main/regression.R) contains code for regression model to identify factors associated with PM<sub>2.5</sub> changes during the lockdown.

[`health_impacts.R`](https://github.com/kevinleec/lockdown_impacts_PM2.5/blob/main/health_impacts.R) contains code for estimating the effects of PM<sub>2.5</sub> changes on respiratory and cardiovascular hospitalizations.

[`prediction_accuracy.R`]() contains code for evaluating the predictive accuracy of SCM as compared to a 3-year simple average.
