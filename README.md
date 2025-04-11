# 🚀 Final Project: Geospatial Data Science and Economic Spatial Models (BSE)

Welcome to the repository for our Take Home Exam project in Geospatial Data Science! This project blends spatial analysis with robust empirical techniques to uncover regional trends and effects using two-way fixed effects. The final output is a polished HTML report that showcases our data visualizations and regression analysis.

## 👥 Authors

The project was developed by:
- Anastasiia Chernavskaia
- Blanca Jimenez
- Pablo Fernández
- Nour Mohamed

## 📁 Repository Structure

## Repository Structure

```
├── Take_Home_Exam_Final.Rmd # The original R Markdown file containing the project code and analysis 
├── Take_Home_Exam_Final.html # The knitted HTML output from the R Markdown file (final report) 
  └── source_code/ # Directory for pre-processing and cleaning code (data files are not included in the repo due to their large size)
```

**Note:**  
The `source_code` folder holds the scripts used to clean and initially develop the code in the R Markdown file. These files are not included in the repository because the associated raw data files are too large to distribute.

## 🌟 Project Motivation

This project was designed to address key analytical questions through the integration of geospatial data methods and empirical econometric techniques. The motivation behind the project was to explore:
- **Geospatial Data:** How spatial patterns and geolocated data contribute to understanding regional or local phenomena.
- **Empirical Methodology:** The application of two-way fixed effects models to control for both time-invariant and entity-specific differences, enabling robust inference from panel data.

The final output, [Take_Home_Exam_Final.html](./Take_Home_Exam_Final.html), showcases the comprehensive analysis performed, including visualizations, model diagnostics, and robust discussions around the methodology and results.

## 📊 Data Sources

The analysis leverages multiple data sources, which include:
- **Geospatial Data:** Regional datasets that provide spatial information for mapping and spatial econometric analysis.
- **Panel Data:** Empirical datasets that enable the implementation of two-way fixed effects models.

*(For detailed information about each data source, please refer to the documentation within the R Markdown file.)*

## 🛠️ Methodology

The project uses a combination of geospatial analysis techniques and fixed effects regression modeling:
- **Geospatial Analysis:** Techniques to map and visualize spatial data distributions and detect regional patterns.
- **Two-Way Fixed Effects Models:** Empirical strategies designed to account for both time-specific and entity-specific heterogeneity in panel data, which improves the reliability of causal interpretations.

## 📤 Results and Discussion

The final knitted HTML report summarizes key findings:
- Mapping visualizations that highlight significant spatial trends.
- Regression tables showcasing the impact estimates after controlling for fixed effects.
- Interpretative commentary on how the chosen methodologies contribute to a deeper understanding of the observed phenomena.

## ⚠️ Project Limitations

While the analysis provides robust insights, several limitations were acknowledged:
- **Data Limitations:** Due to the size and complexity of the raw datasets, only cleaned and processed data were used in the final analysis.
- **Time Constraints:** Some potentially impactful variables and extended robustness checks could not be fully explored due to limited available time.
- **Model Constraints:** The use of two-way fixed effects, while powerful, does impose restrictions on the flexibility of modeling dynamic relationships.

Future work could expand on these limitations by exploring alternative modeling strategies and incorporating additional data to improve the robustness of the findings.

## 💻 How to Use This Repository

1. **View the Final Report:** Open the `Take_Home_Exam_Final.html` file in any modern web browser.
2. **Review the Source Analysis:** The R Markdown file, `Take_Home_Exam_Final.Rmd`, includes all the code and commentary used to generate the report. Running this file (with the necessary dependencies installed) will reproduce the analysis locally.
3. **Examine Pre-processing Code:** Although the `source_code` folder is not included, its purpose is documented here. For more details or to collaborate on data cleaning scripts, please contact the authors.

---

*This README file is intended to serve as an overview and guide to the repository. For full details on methodology and results, please refer to the knitted HTML report and the R Markdown document.*
