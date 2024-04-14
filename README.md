Project Proposal
Title: Enhancing Urban Living: A Geospatial Analysis of Accessibility in Singapore
Introduction and Motivation
Singapore, a hallmark of development and urban planning, is renowned for its efficient public transportation system and well-planned residential areas. Despite these achievements, there are recurring concerns among its citizens about the varying levels of convenience and accessibility in different regions. This disparity has led to unequal demand for housing, with some areas being more sought after than others. This project aims to investigate the truth behind these claims, focusing on whether certain areas in Singapore indeed suffer from a lack of accessibility and convenience. By identifying these areas, we intend to highlight potential regions that could benefit from urban planning improvements, thereby enhancing the quality of life for its residents.
Problem Statement
While Singapore is celebrated for its overall livability, anecdotal evidence suggests that not all areas offer the same level of access to amenities and services. This inequality in accessibility can impact the desirability of housing, affect daily commutes, and overall convenience for the residents. The main problems and issues this project aims to address are:

Identifying regions within Singapore that have lower accessibility scores.
Understanding how the distribution of various amenities affects the perceived and actual convenience of different areas.
Providing a data-driven analysis to suggest areas for improvement to the Singapore government.

Approach
The project will adopt a multi-faceted approach to analyze accessibility in Singapore, including:

Modeling Geographic Accessibility: We will map out the geographic accessibility of different regions in Singapore, taking into account all amenities, road networks, and pedestrian pathways.

Hotspot Analysis: With the accessibility scores calculated for each region, we will perform hotspot analysis to identify areas with significantly higher or lower accessibility levels.

Geographic segmentation: Geographic segmentation will be used to identify clusters of regions which are separated by their different needs. This will help in understanding which amenities each cluster is lacking and where resources should be allocated to in development.  
Data
Context 
For the HDB transactions, we have obtained a dataset from Kaggle which has transactions of HDBs in different regions from dates 2012 to 2023. 

Source: https://www.kaggle.com/datasets/syrahmadi/resale-hdb-flat-prices-2000-2022
Analysis
The data we have chosen has been compiled from multiple sources. For each amenity type, we have a list of names that have been webscrapped or manually collected from the internet (e.g. school names from MOE). The list of names will be put through a python API which will take a location name as input and output its coordinates. 

The final dataset will consist of various location names and coordinates under these amenity types:

Schools (Primary, Secondary, JC, Poly, ITE, Kindergarten)
MRT
Malls
Supermarket
Hawkers
Early Prototypes and Storyboards
Data Dashboards: 
A time series heatmap that users can use to visualize the frequency of transactions from year 2012 to a user specified date. The frequency of transactions will be represented using shades of colors, providing users with context on the popularity of the area in different time periods. Darker shades on the map indicate higher transaction frequency, vise versa.

A series of heatmaps showing the density of different types of amenities in Singapore, alongside areas identified as hotspots of low accessibility. This visual contrast will help in pinpointing where additional amenities and services are needed.



Interactive Maps: 
An interactive map highlighting the accessibility scores of different neighborhoods in Singapore. Users can click on an area to see a detailed breakdown of its score and contributing factors.

Responsibilities
Each member will be cleaning and processing the data to fit the analytical method that they will be using. 
Russell
Modeling Geographic Accessibility 
Qi Yang
Geographic segmentation
Glenn: 
Hotspot analysis




