# ML Challenge by Orbility 🚗🛠️

## Overview
This project, created by **CodeHorosho** for the GigaHack event, explores machine learning and data analytics to extract valuable insights for the **parking equipment business**. We focused on analyzing repair data, costs, repair times, and repair dates to uncover patterns, optimize decision-making, and provide actionable intelligence for business operations.

Our approach leverages cutting-edge techniques in **Machine Learning (ML), Natural Language Processing (NLP), and AI models** to develop a comprehensive solution that enhances equipment repair management.

## Project Components

### 1. **Data Analysis & Preprocessing**
We analyzed historical data related to parking equipment repairs, encompassing:
- **Repair Costs**: Breakdown of expenses related to equipment maintenance.
- **Repair Dates**: Temporal distribution and trends of repair events.
- **Time of Repair**: Duration analysis of each repair event.
  
The EDA (Exploratory Data Analysis) was done using R, which helped in identifying data patterns and preparing the dataset for machine learning modeling.

### 2. **ML Models & Predictions**
The heart of our solution is a set of **ML models** developed to:
- **Predict equipment breakdowns** based on historical trends.
- **Optimize repair scheduling** to minimize downtime and costs.
- **Forecast repair costs** using time-series analysis.

We implemented multiple algorithms, including random forest, Prophet, and others, to provide robust predictions.

### 3. **Natural Language Processing (NLP)**
We applied **NLP techniques** to process repair logs and extract key topics and insights from textual descriptions. This enables better understanding of frequently occurring issues and allows for proactive maintenance planning.

### 4. **Power BI Reporting**
We built an interactive **Power BI dashboard** to visualize the insights from our analysis. The report allows users to:
- Explore breakdown incidents, repair costs, and repair time dynamically.
- Identify outliers and compare equipment performance with baselines.
- Use AI/ML-generated insights to inform business strategies.

#### Key Features of the Power BI Dashboard:
- **Repair Time Analysis**: Visual representation of repair durations.
- **Cost Analysis**: Detailed breakdown of expenses related to specific repairs.
- **Breakdown Trends**: Time-series analysis of equipment breakdown incidents.

## Project Files

- **1_deepl_translate_v3_key_rep_defect.py**: Python script used for translating defect reports and processing textual data via the DeepL and Google APIs.
- **1_main_eda_ml_ai_models_4.R**: R script used for exploratory data analysis, data preprocessing, and development of machine learning models.
- **bi_report_v5.pbix**: Power BI report file containing all visualizations and insights generated from the analysis.

## Technologies Used
- **Languages**: Python, R
- **Machine Learning**: Random Forest, Prophet, and others
- **NLP**: Topic modeling on repair logs
- **Power BI**: Data visualization and reporting
- **APIs**: DeepL and Google API for text translation

## Installation and Usage

### Pre-requisites
- **Python**: Ensure you have Python installed (v3.7 or higher).
- **R**: RStudio with all necessary libraries for data manipulation and ML.
- **Power BI**: Power BI Desktop to view the interactive report.

### Running the Scripts
1. **Text Translation & Preprocessing**: Use `1_deepl_translate_v3_key_rep_defect.py` to translate and preprocess defect reports.
2. **ML Modeling**: Run `1_main_eda_ml_ai_models_4.R` to execute data analysis and model training.
3. **Dashboard**: Open `bi_report_v5.pbix` in Power BI to explore the final results and visualizations.

## Future Work
- **Real-time monitoring**: Implementing real-time data updates for repair schedules and cost predictions.
- **Further Optimization**: Enhance ML models for more accurate predictions and cost-saving opportunities.
- **Integration**: Integrating with IoT devices for better equipment monitoring and predictive maintenance.

## Team
- **CodeHorosho** @ GigaHack 2024  
   Project Lead: Alexandr Cecetov
   Data Scientists: Alexandr Cecetov, Iana Doliuk, Dumitru Samburi, Artur Bondarenco
