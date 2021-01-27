# Pandemic Lockdown Severity Index

![](Results/plots/pop_weighted_standardized_aggregate.png)
![](Results/plots/pop_weighted_facet.png)
![](Results/plots/categorical_facet.png)

## Overview
We all want to know if lockdowns work. They would be a huge price to pay if not! In order to understand a relationship between cause and effect, both must be measured. We need to understand the severity of lockdowns – the cause – before we can do any regression or correlation analysis with death toll or economic impact – the effects. Metrics for these effects exist, but understanding the cause poses a challenge. The index presented here offers a useful measure to understand the severity of lockdown restrictions. It is available for each of nine individual lockdown categories, for each of the fifty states, and aggregated into one measure for the whole United States. 

Many great indices exist in this vein, but the Pandemic Lockdown Severity Index provides two novel features:
1)	Weighted by population of each state.
2)	Each category is standardized to impose equal weighting.

The first ensures California holds more importance than Alaska, for example. The second ensures that regardless of the number of levels within each category, each hold an equal importance. Each of these also provide the opportunity for the user to modify the index with demographic, epidemiologic, or economic significance by adding external weights. 

## Terminology
Terminology:
-	**Pandemic Lockdown Severity Index**: name of the population-weighted, standardized, nation-wide series measuring severity of lockdowns. Can also be written as Lockdown Severity Index, or, in context, just the Index.
-	**Category**: a categorical variable, also called a factor. The Index comprises nine categories and are listed in the appendix. Opposed to a continuous variable, which may contain *any number* (temperature, age), or a discrete variable, which may contain *any whole number* (number of people, number on a dice), a categorical variable contains a *character string*.
-	**Level**: the character string of a categorical variable. Categories are comprised of up to seven levels (strings) and are listed in the appendix. 
- 	Standardize: a process of shifting and scaling data. It comprises two steps:
	1) subtract the mean of the series from each data point (shift)
	2) divide each of these values by the standard deviation of the series (scale)

Formulaically, this looks like the following. For each date *t* of *value* of a series *s*, 

    ```math
    standardized\:score = \frac{value_{t,s} - mean_s}{std dev_s} = \frac{v_{t,s} - \overline{v_s}}{\sigma_s}
    ```
    
The purpose of this is discussed in the methodology section.


- Population-weighted: a process of scaling data by a weight. Here, the weight is the fraction of population the state owns out of the total United States population (50 states + DC). 

Formulaically, this looks like the following. For each date *t* of *value* in a series for each state *s*, 
    
```math
    population\:weighted\:score = \frac{population_{s}}{\sum_{s=1}^{51}population_s} * value_{s,t}
```
    
The purpose of this is discussed in the methodology section. 


## Data
Data about the lockdown restrictions is collected for each state from the [Kaiser Family Foundation's Covid Policy Report](/https://www.kff.org/report-section/state-covid-19-data-and-policy-actions-policy-actions/). Vintage data is collected using the Way Back Machine(https://archive.org/web/) and stored as one csv per date, containing all 50 states + DC. 

There are approximately 18 categories that are filtered down to 9 (some are merged, some are ignored altogether). 

*Original Factors:*
- Location     
- Date   
- Stay at Home Order  
- Stay At Home Order 
- Status of Reopening                       
- Non-Essential Business Closures
- Large Gatherings Ban   
- Emergency Declaration                                                      
- Mandatory Quarantine for Travelers             
- Restaurant Limits 
- Bar/Restaurant Limits                
- Bar Closures                                  
- Bar Closures*
- State-Mandated School Closures            
- School Closures
- State Is Easing Social Distancing Measures                        
- Face Covering Requirement 
- Mandatory Quarantine
- Primary Election Postponement            

*Cleaned Factors:*
- Location           
- Date       
- Stay_Home_Order     
- Status_of_Reopening   
- NonEss_Business_Closed
- Gathering_Limit 
- Emergency_Declaration 
- Travel_Quarantine     
- Mask_Requirement     
- Restaurants         
- School_Closed
   

Each of these 9 categories have between 1 and 7 levels, stored as text strings such as “take out only” or “masks for general public required”. 

These factor levels are re-ordered to reflect their level of severity using human judgment. Then, a number from 0 to the number of levels is matched to each level. 0 is reserved for NAs, “-“, or otherwise blank data. These new numbers are all integers. 


## Code Structure
This is written entirely in R. The repo contains all code and data necessary to reproduce all series and can be sourced sequentially. 
The code is ordered numerically and roughly follows this outline:
- `010_compile_kff_data`: read in csv from each date containing 18 categories. Merges similar categories together and cleans up similar levels within each category. Formats and exports as csv in long form.
- `012_gather_populations`: read in csv with US Census 2020 state population estimates. Format and export as csv.
- `020_categorical_plots`: reorders category levels and assigns severity score to each level *now deprecated in favor of individual scripts*

## Acknowledgments 
I'd like to thank Alessandro Barbarino for input and mentorship, and Jaclyn Lee for help problem-solving code.
The lockdown data originate from the [Kaiser Family Foundation](https://www.kff.org/report-section/state-covid-19-data-and-policy-actions-policy-actions/). Thank you for making your [data publicly available!](https://github.com/KFFData/COVID-19-Data/tree/kff_master/State%20Policy%20Actions/State%20Social%20Distancing%20Actions)


