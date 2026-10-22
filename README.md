# Cognitive Decline and Financial Behavior

Nelson-Mendenhall Summer Program, UW School of Pharmacy (CHOICE Institute), Summer 2026.
Zayna Badr and Jing Li. Presented at the Undergraduate Research Symposium.

## Overview
Impaired financial decision-making can begin years before a clinical dementia diagnosis and is linked to greater wealth loss. Dementia is also often underdiagnosed, especially in socially disadvantaged populations. This project asks which specific financial behaviors are associated with early cognitive decline.

## Data
- RAND American Life Panel (ALP), a nationally representative online panel
- Questions from the FINRA National Financial Capability Study fielded in 2012 (2,094 respondents) and 2018 (1,233 respondents)
- Raw data are not included in this repository.

## Methods
- Cognitive ability was measured with three financial literacy questions that correlate highly with cognitive ability (Lusardi and Mitchell, 2011). A decline in score between 2012 and 2018 was treated as likely pre-dementia.
- Compared demographics and 2012 financial behavior (credit card usage and payment, high-cost borrowing) between respondents whose scores declined vs. were unchanged/improved, and between those who dropped out between waves vs. remained
- Analysis in R and Excel

## Key Findings
- Financial literacy scores declined more among respondents aged 65+ than younger respondents.
- Respondents with declining scores were more likely to be racial minorities, have less education, and have lower income.
- Respondents with declining scores, or who dropped out between waves, were more likely to take a payday loan, get a tax refund advance, fail to pay credit cards in full, or overuse credit cards.

## Limitations
- ALP participants are cognitively healthier than average
- Sample attrition between 2012 and 2018
- Only two waves available
- No detailed investment behavior and limited longitudinal cognition data

## Further Research
Study investment behavior among individuals experiencing cognitive decline.

## Files
- `poster.pdf`: research poster
- `Dementia_and_Wealth_Analysis.R`: R code for analysis

## References
- Li, J., et al. (2026). Dementia and long-run trajectories in household finances. NBER Working Paper No. w34659.
- Lusardi, A. and Mitchell, O.S. (2011). Financial Literacy and Planning: Implications for Retirement Wellbeing. NBER Working Paper No. w17078.

## Author
Zayna Badr, University of Washington
