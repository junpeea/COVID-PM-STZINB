# A Spatio-Temporal analytical outlook of the exposure to air pollution and COVID-19 mortality in the United States

According to the World Health Organization (WHO), this is an emergency and the whole world is going through the Coronavirus disease (COVID-19) Pandemic. Countably infinite number of people are working on this all over the world and we, as a team, would like to contribute our viewpoint as related to this Pandemic, besides our primary commitment to work.

Conditioned on the strength of our team and the dynamics of the disease, we realize that it is impossible (more specifically, the size of our team and the dimension of the problem will never ever meet the convergence of reality) to analyze the underline distribution of this Pandemic.
Therefore, we have decided to restrict ourselves to information available only in the USA. Yes, it is still an insurmountable task for us to focus on every aspect of the Pandemic, conditioned on the diverse nature of this country. We acknowledge each and every one who is on this problem and value YOUR contribution.

Our work is solely based on publicly available data in the USA. We, as a team revere every single person who are involved in this humongous project: every minuscule COUNTS. Our primary focus is only on the US data as related to this Pandemic. We provide the data repository for public available code and data to reproduce our analyses.

<b>Abstract: </b><br>
The whole world experienced a pandemic due to the outbreak of the COVID-19 dis-ease, caused by a virus called the Severe Acute Respiratory Syndrome Coronavirus2 (SARS-CoV-2). The United States is also severely experienced extensive mortalityfrom this disease. Several studies suggested that many of the pre-existing conditionsincreased the risk of death in those with COVID-19. The long-term exposure to theair pollutants is also proven to be one of the major causes behind this mortality. Inthis study, we have explored the relationship between long-term exposure to one ofthe well-known air pollutants, PM2.5and the mortality from the COVID-19 diseasewhile adjusting for several social and environmental factors. Since the COVID-19pandemic is highly spatial in nature of its association and spread pattern, we believetaking into account the spatial dependence of the disease spread process will be veryuseful. Also, as the disease is spread across time, there is a strong temporal natureof the data. Hence our belief is a spatio-temporal model which can take into accountthe complex spatial and temporal dependencies is essential to understand and inves-tigate the COVID-19. In that respect we are using a Bayesian Zero Inflated NegativeBinomial regression model where the spatial and temporal associations are modeledthrough random effects. Our model can capture the overall uncertainty pattern acrosstime and space. This uncertainty quantification taking into account the spatial corre-lation is an attractive novelty of our approach. We have applied our model on a fewstates where the mortality rates are high compared to the other states of the US. Asa part of this study, we have developed a user friendly R tool which can be used toget inference from any state of the US. Along with that, application can also be car-ried out on the entire US data with aggregated state level information (i.e. work withstate level COVID-19 counts).

<b>Summary Results: </b><br>
![](./Paper work/Table4.PNG)
Table4. The results of effect estimation on the expected COVID-19 weekly death counts among the four states (Illinois,Washington, Florida, and California)

![](./Paper work/Table6.PNG)
TABLE 6. The estimated nonlinear fixed time effect in the Negative Binomial component (the COVID-19 death counts) fromthe selected models. The x-axis represents timeline (weeks). A blue line is the estimated time effect. A shaded area representsits 95% credible interval. A red dash line is a zero reference.

![](./Paper work/Table7.PNG)
TABLE 7. Visualization of time-averaged COVID-19 death probability at risk using the selected STZINB models for the fourstates. (IL, WA, FL, CA)

<b>Code: </b><br>
[`Result_general.R`](https://github.com/junpeea/COVID-PM-STZINB/Papaer_work/Code/Reult_general.R) includes the code to provide main tables and figues in our Result session.

[`Result_CA.R`](https://github.com/junpeea/COVID-PM-STZINB/Papaer_work/Code/Reult_CA.R) includes the code to provide model outputs in our California study.

[`Result_FL.R`](https://github.com/junpeea/COVID-PM-STZINB/Papaer_work/Code/Reult_FL.R) includes the code to provide main results in our Florida study.

[`Result_IL.R`](https://github.com/junpeea/COVID-PM-STZINB/Papaer_work/Code/Reult_IL.R) includes the code to provide main results in our Illinois study.

[`Result_WA.R`](https://github.com/junpeea/COVID-PM-STZINB/Papaer_work/Code/Reult_WA.R) includes the code to provide main results in our Washington study.

<b>Data: </b><br>

Adjacency.csv: Adjacency information across states and counties in US.

County_details.csv

<b>Reference pages: </b><br>
Reference Dashboard: [Johns Hopkins University COVID-19 dashboard](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)<br>
COVID-19 data source 1: [The COVID Tracking Project](https://covidtracking.com/)<br>
COVID-19 data source 2: [2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE ](https://github.com/CSSEGISandData/COVID-19)<br>
PM2.5 data source: [Public available code and data to Reproduce Analyses in <Exposure to air pollution and COVID-19 mortality in the United States>](https://github.com/wxwx1993/PM_COVID) <br>
US Hospitalization: [ArcGIS Hub](https://hub.arcgis.com/search) <br>

<b>Contact Us: </b><br>
1. Sounak Chakraborty <chakrabortys@missouri.edu>
2. Tanujit Dey <tanujit.dey@gmail.com>
3. Francesca Dominici <fdominic@hsph.harvard.edu>
4. Yoon-Bae Jun <junpeea@gmail.com>
5. Chae Young Lim <limc.stat@gmail.com>
6. Anish Mukherjee <anishmk9@gmail.com>

<b>Terms of Use:</b><br>

