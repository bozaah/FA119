---
output:
  word_document: default
  html_document: default
---
# Near real-time monitoring of animal welfare for sustainable pork production using IoT-enabled temperature sensors

#### Abstract  
This study explores near real-time physiological monitoring through IoT-enabled ear tag temperature sensors combined with health observations. We classified animal health status based on temperature observations from temperature sensors placed at pigs ears. We designed an approach applying time-series modeling, feature engineering, and machine learning (ML) approaches. The XGBoost model emerged as the most effective classifier, achieving an accuracy of 80.9% with a false positive rate of 0.25. Rolling averages, temperature variability, and anomaly detection features significantly improved early detection of health issues, enabling timely interventions. Integrating IoT technologies supports automated animal welfare monitoring while promoting sustainability through precision livestock management by reducing inspection costs and allow for targeted animal welfare support.

## Introduction 

Pork production, a key segment of the livestock industry, faces challenges in balancing animal welfare, productivity, and environmental sustainability. Due to the scale of production, traditional monitoring methods rely heavily on manual inspections prone to delays and observer bias, often resulting in missed health issues until they become severe.  

Advances in the internet of things (IoT) and artificial intelligence (AI) present opportunities for real-time animal welfare monitoring and decision support on farm. Wireless sensors continuously collect physiological and environmental data, enabling automated health assessments. By reducing reliance on manual inspections, these technologies promise improved animal welfare, reduced production costs, and lower environmental impacts. This study investigates how ear tag temperature sensors, combined with ML-driven analytics, can improve health monitoring in pigs while reducing pork farming’s environmental footprint.

The research focuses on developing a near real-time monitoring system that integrates IoT-enabled ear tag temperature sensors with machine learning models. We evaluate the system’s effectiveness in detecting early signs of health issues, reducing manual interventions, and supporting more efficient and sustainable pork production.

## Materials and Methods  

### Data collection  

#### IoT temperature sensors  
Ear tag temperature sensors were deployed on pigs in a commercial farm setting. These sensors recorded ear temperatures every 10 minutes and transmitted data wirelessly to a central repository. Each record included a timestamp, animal ID, temperature reading and metadata, including room and pen assignment. Sensors (model, brand) featured a temperature detection range of -X°C to X°C with ±0.X°C accuracy.  

#### Health observations  
Farm staff conducted weekly health checks, noting clinical signs such as coughing, lameness, and skin lesions. These observations were systematically recorded and linked to corresponding sensor data using unique animal IDs. Health statuses were categorized as “Healthy” or “Sick” based on clinical assessments.  

### Data preprocessing  

We performed extensive preprocessing to ensure data quality. Records with missing IDs, timestamps, or health statuses were removed, while identical records were eliminated to prevent skewed results. Animals were categorised into "Healthy" or "Sick" conditions, with binary alert flags indicating abnormal conditions for cumulative alert calculations.

### Feature engineering  

To capture relevant physiological patterns, we generated over 50 time-series features. Rolling averages, standard deviations, and temperature differences across multiple time intervals (from 3 to 48 hours) were calculated to reflect short- and long-term changes in animal temperatures. STL decomposition isolated temperature seasonality, trends, and residuals, highlighting unusual variations potentially linked to health issues. Cumulative alerts were computed by summing abnormal events per animal over time.

### Machine learning framework  

#### Model development  
The animal health classification task was framed as a binary classification problem, distinguishing "Healthy" from "Sick" statuses. Decision trees, XGBoost, and neural networks were selected due to their complementary strengths. Decision trees provided interpretability, while XGBoost was expected to handle complex feature interactions effectively. Neural networks were included to explore potential gains in detecting nonlinear patterns.  

#### Model training and evaluation  
The dataset was split into training (80%) and testing (20%) sets. A 5-fold repeated cross-validation process ensured robust performance evaluation. Model accuracy, precision, recall, F1-score, and area under the ROC curve (AUC-ROC) were used as evaluation metrics. Hyperparameter tuning for XGBoost was performed using a randomized search to optimize predictive performance.

## Results  

### Model performance  
The XGBoost model achieved the highest average accuracy of 80.9%, surpassing decision trees (74.5%) and neural networks (78.2%). Evaluation metrics, including precision, recall, and F1-score, indicated that the XGBoost model effectively balanced sensitivity and specificity.  

| Model              | Accuracy (%) | Precision | Recall | F1-Score | AUC-ROC |
|--------------------|---------------|------------|---------|------------|----------|
| Decision Tree      | 74.5          | 0.72       | 0.70    | 0.71       | 0.76     |
| XGBoost            | 80.9          | 0.81       | 0.82    | 0.81       | 0.88     |
| Neural Network     | 78.2          | 0.78       | 0.79    | 0.78       | 0.85     |

### Feature importance  
Key features identified by the XGBoost model included 24-hour and 48-hour rolling averages, 12-hour temperature variability, and anomaly residuals extracted from STL decomposition. These features provided a comprehensive view of physiological patterns, enabling early health issue detection.

### Anomaly detection  
The anomaly detection system effectively highlighted significant temperature deviations, often preceding observable clinical signs. This early warning capability demonstrated the system's potential for proactive health management.

### Model tuning 
Hyperparameter tuning for XGBoost reduced the false positive rate to 0.25 while maintaining high accuracy and recall, making it the most suitable model for deployment.

## Discussion 

### Performance analysis  
The superior performance of XGBoost highlights its ability to manage complex relationships within time-series features. Rolling averages and anomaly detection features played a critical role in predicting health status changes.

### Near real-time monitoring implications  
The near real-time nature of the monitoring system enabled continuous animal welfare tracking with minimal lag. This allows timely interventions, reducing mortality and minimizing antibiotic use. The system’s responsiveness supports its seamless integration into commercial farm operations.

### Sustainability considerations  
Automated monitoring reduces the need for manual health inspections, lowering operational costs and can potentially improving welfare outcomes. Early detection reduces disease-related losses and prevents non-targeted medical treatments.

## Conclusion 

This study confirmed the feasibility of using IoT-enabled ear tag temperature sensors with ML-driven analytics for near real-time animal welfare monitoring. The integration of time-series features, including rolling averages and anomaly detection, significantly enhanced early detection capabilities. XGBoost emerged as the best-performing model, achieving high accuracy and recall despite imbalanced data. The system supports sustainable pork production by automating health monitoring and reducing environmental impact through precision livestock management.

## References  
 
