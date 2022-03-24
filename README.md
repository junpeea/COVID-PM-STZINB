# A Spatio-Temporal analytical outlook of the exposure to air pollution and COVID-19 mortality in the United States

According to the World Health Organization (WHO), this is an emergency and the whole world is going through the Coronavirus disease (COVID-19) Pandemic. Countably infinite number of people are working on this all over the world and we, as a team, would like to contribute our viewpoint as related to this Pandemic, besides our primary commitment to work.

Conditioned on the strength of our team and the dynamics of the disease, we realize that it is impossible (more specifically, the size of our team and the dimension of the problem will never ever meet the convergence of reality) to analyze the underline distribution of this Pandemic.
Therefore, we have decided to restrict ourselves to information available only in the USA. Yes, it is still an insurmountable task for us to focus on every aspect of the Pandemic, conditioned on the diverse nature of this country. We acknowledge each and every one who is on this problem and value YOUR contribution.

Our work is solely based on publicly available data in the USA. We, as a team revere every single person who are involved in this humongous project: every minuscule COUNTS. Our primary focus is only on the US data as related to this Pandemic. We provide the data repository for public available code and data to reproduce our analyses.

<b>Abstract: </b><br>
The whole world experienced a pandemic due to the outbreak of the COVID-19 dis-ease, caused by a virus called the Severe Acute Respiratory Syndrome Coronavirus2 (SARS-CoV-2). The United States is also severely experienced extensive mortalityfrom this disease. Several studies suggested that many of the pre-existing conditionsincreased the risk of death in those with COVID-19. The long-term exposure to theair pollutants is also proven to be one of the major causes behind this mortality. Inthis study, we have explored the relationship between long-term exposure to one ofthe well-known air pollutants, PM2.5and the mortality from the COVID-19 diseasewhile adjusting for several social and environmental factors. Since the COVID-19pandemic is highly spatial in nature of its association and spread pattern, we believetaking into account the spatial dependence of the disease spread process will be veryuseful. Also, as the disease is spread across time, there is a strong temporal natureof the data. Hence our belief is a spatio-temporal model which can take into accountthe complex spatial and temporal dependencies is essential to understand and inves-tigate the COVID-19. In that respect we are using a Bayesian Zero Inflated NegativeBinomial regression model where the spatial and temporal associations are modeledthrough random effects. Our model can capture the overall uncertainty pattern acrosstime and space. This uncertainty quantification taking into account the spatial corre-lation is an attractive novelty of our approach. We have applied our model on a fewstates where the mortality rates are high compared to the other states of the US. Asa part of this study, we have developed a user friendly R tool which can be used toget inference from any state of the US. Along with that, application can also be car-ried out on the entire US data with aggregated state level information (i.e. work withstate level COVID-19 counts).

<b>Summary Results: </b><br>
![](./Paper_work/Table1.PNG) <br>
Table 1. Summary statistics for the four regions

![](./Paper_work/Table3.PNG) <br>
TABLE 3. Point estimates and 95\% credible intervals of the fixed effects for the expected COVID-19 death counts

![](./Paper_work/Figure2.PNG) <br>
Figure2. Visualization of time-averged probability at COVID-death risk for the four regions

![](./Paper_work/Figure3.PNG) <br>
Figure3. he median value of estimated nonlinear mixed time effect in the negative binomial component (the COVID-19 death counts) using the selected model structure



<b>Code: </b><br>
[`Result_general.R`](https://github.com/junpeea/COVID-PM-STZINB/blob/main/Paper_work/Code/Result_general.R) includes the code to provide main tables and figues in our Result session.

[`Result 210401 Div2(NJ,NY,PA).R`](https://github.com/junpeea/COVID-PM-STZINB/blob/main/Papaer_work/Code/Result 210401 Div2(NJ,NY,PA).R) includes the code to provide model outputs in the Mid-Atlantic (New Jersey, New York, and Pennsylvania) study.

[`Result 210401 Div4(IA,KS,MO,NE,ND,SD).R`](https://github.com/junpeea/COVID-PM-STZINB/blob/main/Papaer_work/Code/Result 210401 Div4(IA,KS,MO,NE,ND,SD).R) includes the code to provide main results in the Midwest (Iowa, Kansas, Missouri, Nebraska, North Dakota, and South Dakota) study.

[`Result 210401 Div5(GA,FL,NC,SC).R`](https://github.com/junpeea/COVID-PM-STZINB/blob/main/Papaer_work/Code/Result 210401 Div5(GA,FL,NC,SC).R) includes the code to provide main results in the South Atlantic (Florida, Georgia, North Carolina, and South Carolina) study.

[`Result 210401 Div9(CA,OR,WA).R`](https://github.com/junpeea/COVID-PM-STZINB/blob/main/Papaer_work/Code/Result 210401 Div9(CA,OR,WA).R) includes the code to provide main results in the Pacific (California, Oregon, and Washington) study.

<b>Data: </b><br>

Adjacency.csv: Adjacency information across states and counties in US.

County_details.csv

<b>Reference pages: </b><br>
Reference Dashboard: [Johns Hopkins University COVID-19 dashboard](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)<br>
COVID-19 data source 1: [The COVID Tracking Project](https://covidtracking.com/)<br>
COVID-19 data source 2: [2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE ](https://github.com/CSSEGISandData/COVID-19)<br>
PM2.5 data source: [Public available code and data to Reproduce Analyses in <Exposure to air pollution and COVID-19 mortality in the United States>](https://github.com/wxwx1993/PM_COVID) <br>
US Hospitalization: [ArcGIS Hub](https://hub.arcgis.com/search) <br>
