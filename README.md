# How the EU funds its economically disadvantaged regions

**Story**: You can read the full story here: [English](https://www.dw.com/a-48354538) | [German](https://www.dw.com/a-48354537)

**Research, data analysis and visualization, writing:** [Kira Schacht](https://twitter.com/daten_drang)

**The EU spends a fifth of its budget on "regional development": That's €200 billion to support universities, roads, businesses, banks and more. This analysis shows how the European Union's regions benefit from EU funding. Here, you’ll find the code, data and sources behind the story.**

For this analysis, we used the [publicly available data](○%09https:/cohesiondata.ec.europa.eu/funds/erdf) from the EU detailing how much money out of the ERDF is planned, allocated and paid out to the European regions by topic.

## Files in this repository

`erdf_analysis.R` Analysis script.

`erdf_analysis_data.RData` Final R workspace objects for analysis.

`data/eu/...` Datasets (see below).

## Datasets

### Implementation details for ERDF 2014-2020

**Link:** [ESIF 2014-2020 Finance Implementation Details](https://cohesiondata.ec.europa.eu/EU-Level/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52)

The amounts planned, allocated to projects and actually paid out for each project within the ESIF funds, including the ERDF. Broken down by fund, topic and category of region (more developed, less developed, etc. where available). Figures are cumulative, meaning the 2019 amounts include the ones from 2018, for example. 2019 figures were used for this analysis.

**Downloaded on:** 11.04.2019


### Planned amounts for ERDF 2014-2020

**Link:** [ESIF 2014-2020 FINANCES PLANNED DETAILS](https:/cohesiondata.ec.europa.eu/dataset/ESIF-2014-2020-FINANCES-PLANNED-DETAILS/e4v6-qrrq)

The total amounts planned for each project within the ESIF funds, including the ERDF. Broken down by fund, topic and category of region

**Downloaded on:** 11.04.2019


### NUTS classification list

**Link:** [NUTS (Nomenclature of Territorial Units for Statistics), by regional level, version 2013](http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_CLS_DLD&StrNom=NUTS_2013L&StrLanguageCode=EN&StrLayoutCode=HIERARCHIC)

Regional classification of the European Union with region names and NUTS codes, for geographical matching with programme titles. Used to produce ERDF_2014-2020_regions_match
