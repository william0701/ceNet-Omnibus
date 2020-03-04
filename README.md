# CeNet Omnibus
## Introduction
The ceRNA regulation is a newly discovered post-transcriptional regulation mechanism and plays significant roles in physiological and pathological progress. The analysis of ceRNAs and ceRNA network has been widely used to detect survival biomarkers, select candidate regulators of disease genes, and pre-dict long noncoding RNA functions. However, there is no software platform to provide overall functions from construction to analysis of ceRNA networks. To solve this problem, we introduce CeNet Omnibus, a R/Shiny application, which provides a unified framework of ceRNA network construction and analysis. CeNet Omnibus is characterized by comprehensiveness, high efficiency, high expandability and user customizability, and it also offers the web-based user-friendly interface for users to obtain the output intuitionally in time.

CeNet Omnibus consists of five components, including **Data Input**, **Data Processing**, **Network Construction**, **Network Visualization** and **Netwoek Analysis**. The framework of CeNet Omnibus is shown below.
![Framework of CeNet Ominbus](https://github.com/william0701/Figures/blob/master/CeNet%20Omnibus/framework.svg "Framework of CeNet Ominbus")
## Installation
To install the package from the github repository please use the following code.
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
deps <- c("devtools")
BiocManager::install("devtools", dependencies = TRUE)
devtools::install_github("BioinformaticsFMRP/TCGAbiolinksGUI.data",ref = "R_3.4")
````
## Data Preparation
CeNet Omnibus demands users to upload four files for the constrction of ceRNA network, including 
- The expression profiles of candidate ceRNAs and microRNAs
- The interaction between microRNA and candidate ceRNAs
- Essential information of candidate ceRNAs, eg. symbols, biotypes, and etc.

## Get Start
The following commands should be used to start CeNet Omnibus.
```r
library(CeNetOmnibus)
CeNetOmnibus()
```
### 1. Data Input
**Data input** provides the interface for users to upload data for the construction of ceRNA network.
#### 1.1 Expression Profiles
The expression profiles of ceRNAs and microRNAs should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. In addition, to make sure the rows of final data represent ceRNAs/microRNAs, user should select if row/column is ceRNAs/microRNAs. To name the datasets, please confirm if the program should name dataset with/without the first row/column.

#### 1.2 The Interaction between ceRNAs and microRNAs
#### 1.3 Essential information
### 2. Data Processing
### 3. Network Construction
### 4. Network Visualization
### 5. Network Analysis