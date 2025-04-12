# 🌍 Final Project: Geospatial Data Science and Economic Spatial Models (BSE)

Welcome to the repository for the final project in Geospatial Data Science and Economic Spatial Models (subject from the BSE)! 

With natural disasters (such as wildfires) becoming more frequent and violent with climate change, **this project investigates how natural disasters influence voting behavior in Spain, focusing on Catalunya and Comunitat Valenciana**. Specifically, we examine whether exposure to wildfires, an increasingly frequent natural shock, affects political preferences, turnout, and support for incumbent governments.

This project blends spatial analysis with empirical techniques to uncover regional trends and effects, by using two-way fixed effects as the identification strategy. The final output is a polished HTML report that showcases our data visualizations and regression analysis.

## 👥 Authors

The project was developed by:
- Anastasiia Chernavskaia
- Blanca Jimenez
- Nour Mohamed
- Pablo Fernández

## 📁 Repository Structure

## Repository Structure

```
├── Take_Home_Exam_Final.Rmd # The original R Markdown file containing the project code and analysis 
├── Take_Home_Exam_Final.html # The knitted HTML output from the R Markdown file (final report) 
├── source_code/ # Directory for pre-processing and cleaning the data (data files are not included in the repo due to their large size)
```

**Note:**  
The `source_code` folder holds the scripts used to clean the data and initially develop the code in the R Markdown file. The data files are not included in the repository because they are too large to distribute.

## 🌟 Project Motivation

Recent studies suggest that voters affected by disasters may punish the incumbent government if the response is inadequate. Others find that such events can increase support for conservative parties, particularly when linked to climate change skepticism. Our goal is to test these theories empirically using geospatial data on fire perimeters and granular election outcomes disaggregated at the sección censal level, the highest level of territorial granularity available in Spain. By leveraging spatial analysis techniques, we integrate fire perimeters with administrative boundaries to precisely measure the extent of fire exposure at the local level.

The final output, [Take_Home_Exam_Final.html](./Take_Home_Exam_Final.html), showcases the comprehensive analysis performed, including visualizations, model diagnostics, and robust discussions around the methodology and results.

## 📊 Data Sources

The analysis leverages multiple data sources, which include:
- **Geospatial Data:** Data of the most granular administrative boundaries in Spain (_secciones censales_), together with data of wildfire perimeters in Catalunya (from 1986 to 2023) and Comunitat Valenciana (from 1993 to 2022).
- **Electoral data**: We use detailed eletoral outcomes for the general elections from Catalunya and Comunitat Valenciana, which we use to compute (for each election election) 1) the rate of votes towards the incumbent government, 2) the voter turnout rate, 3) a political index that aggregates the partisan preferences per administrative unit and a 4) nationalist index that aggregates the proportion of votes towards nationalist parties per administrative unit.
- **Panel Data:** With the previous data sources we build a panel with two dimensions: the administrative units (each _sección censal_ in Catalunya and Comunitat Valenciana) and the election years.

## 🛠️ Methodology

The project uses a combination of geospatial analysis techniques and fixed effects regression modeling:
- **Geospatial Analysis:** For each election year, we assess the area affected by wildfires for each administrative unit. This area represents the treatment variable in our empirical analysis.  
- **Two-Way Fixed Effects Models:** We adopt a two-way fixed effects strategy, with election (time) and sección censal fixed effects. Our identification strategy relies on the assumption that fire occurrence is exogenous with respect to political preferences, and that treated and control areas would have followed similar electoral trends in the absence of fire exposure.

## 📤 Results and Discussion

The final knitted HTML report summarizes key findings:
- Mapping visualizations that highlight the main areas affected by wildfires in Catalunya and Comunitat Valenciana.
- Regression tables showcasing the impact estimates after controlling for fixed effects.
- Interpretative commentary on how the chosen methodologies contribute to a deeper understanding of the observed phenomena.

*(For more information on the results of the study, check out the final output file, [Take_Home_Exam_Final.html](./Take_Home_Exam_Final.html).)*

## ⚠️ Project Limitations and Extensions

Although the analysis offers robust insights, the project was developed under severe time constraints, making it challenging to address certain known limitations:
1. **Geographical Measurement Error**: The boundaries of “secciones censales” (census sections) slightly vary over time, particularly in urban areas. This could introduce some measurement error in our regressions. However, the impact is likely minimal since most boundary changes occur in cities where large fires are rare.
2. **Limited Disaster Scope**: Our study focuses solely on wildfires, potentially limiting the generalizability of our findings to other types of natural disasters.
3. **Lack of Alternative Control Groups**: We did not conduct robustness checks using different control groups, which could have strengthened our causal inference approach.
4. **Restricted Model Specifications**: Our analysis employed a limited set of controls and regression specifications. We did not explore potentially relevant interaction effects or more complex model structures.
5. **Simplistic Political Index Construction**: The political ideology indices were constructed using a somewhat manual approach. A more sophisticated method, such as applying textual analysis to political manifestos or accounting for ideological shifts of parties over time, could have provided more nuanced measures.

Future work could expand on these limitations by exploring alternative modeling strategies and incorporating additional data to improve the robustness of the findings.

## 💻 How to Use This Repository

1. **View the Final Report:** Open the `Take_Home_Exam_Final.html` file in any modern web browser.
2. **Review the Source Analysis:** The R Markdown file, `Take_Home_Exam_Final.Rmd`, includes all the code and commentary used to generate the report.
3. **Examine Pre-processing Code:** Check the code in the `source_code` folder.

---

*This README file is intended to serve as an overview and guide to the repository. For full details on methodology and results, please refer to the knitted HTML report and the R Markdown document.*
