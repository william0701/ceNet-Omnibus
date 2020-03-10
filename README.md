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
```

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

The expression profiles of ceRNAs and microRNAs should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. To name the datasets, please confirm if the program should name dataset with/without the first row/column. The rows of uploaded files should represent ceRNAs/microRNAs, while the columns should represent samples.

<center><b>CeRNA Expression Profile Samples</b></center>

|                 | TCGA\.3C\.AAAU\.01 | TCGA\.3C\.AALI\.01 | TCGA\.3C\.AALJ\.01 | TCGA\.3C\.AALK\.01 | TCGA\.4H\.AAAK\.01 |
| --------------- | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ |
| ENSG00000275454 | 0\.35              | 0\.13              | 0\.25              | 0\.23              | 0\.2               |
| ENSG00000261519 | 0\.06              | 0\.04              | 0\.07              | 0\.01              | 0\.09              |
| ENSG00000267405 | 0\.03              | 0\.18              | 0\.15              | 0\.13              | 0                  |
| ENSG00000115365 | 25\.05             | 9\.96              | 8\.47              | 11\.39             | 15\.6              |
| ENSG00000274395 | 0\.05              | 0                  | 0\.09              | 0\.13              | 0\.42              |
| ENSG00000177272 | 0\.13              | 0\.49              | 0\.22              | 0\.33              | 0\.13              |
| ENSG00000235142 | 0\.05              | 0\.01              | 0\.03              | 0\.03              | 0                  |

<center><b>CeRNA Expression Profile Samples</b></center>

| mirbase21\_ID | TCGA\.BH\.AB28\.01 | TCGA\.AO\.A128\.01 | TCGA\.A1\.A0SD\.01 | TCGA\.B6\.A0I1\.01 | TCGA\.BH\.A0BF\.01 |
| ------------- | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ |
| MIMAT0002841  | 0\.745546          | 0                  | 0\.253132          | 0                  | 0                  |
| MIMAT0002840  | 0\.186387          | 0                  | 0                  | 0                  | 0                  |
| MIMAT0021122  | 0                  | 0                  | 0                  | 0                  | 0                  |
| MIMAT0021123  | 0                  | 0                  | 0                  | 0                  | 0                  |
| MIMAT0021120  | 0                  | 0                  | 0                  | 0                  | 0                  |
| MIMAT0021121  | 0                  | 0                  | 0                  | 0                  | 0                  |

<div background-color=#123456>
Please Remeber to Click <mark><b><font color=red>Preview</font></b></mark> Button on the right-bottom cornor of the panel once set parameters properly.
</div>

#### 1.2 The Interaction between ceRNAs and microRNAs

The interactions file between ceRNAs and microRNAs should be 0-1 matrix to represent if there are interactions between ceRNAs and microRNAs. The file should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. To name the datasets, please confirm if the program should name dataset with/without the first row/column. The rows of uploaded files should represent ceRNAs, while the columns should represent microRNAs.

<center><b>CeRNA Expression Profile Samples</b></center>

|                 | MIMAT0000646 | MIMAT0002809 | MIMAT0000617 | MIMAT0000266 | MIMAT0000264 | MIMAT0000263 | MIMAT0000261 | MIMAT0005951 | MIMAT0000278 |
| --------------- | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ |
| ENSG00000275454 | 1            | 1            | 0            | 1            | 0            | 1            | 1            | 1            | 1            |
| ENSG00000261519 | 1            | 0            | 0            | 1            | 1            | 1            | 0            | 1            | 0            |
| ENSG00000267405 | 0            | 1            | 0            | 1            | 1            | 1            | 1            | 0            | 0            |
| ENSG00000115365 | 1            | 1            | 1            | 1            | 1            | 1            | 1            | 1            | 1            |
| ENSG00000274395 | 1            | 0            | 1            | 0            | 1            | 1            | 1            | 0            | 1            |
| ENSG00000177272 | 1            | 1            | 1            | 1            | 1            | 1            | 1            | 0            | 1            |
| ENSG00000235142 | 1            | 1            | 1            | 1            | 1            | 1            | 1            | 1            | 1            |

<div background-color=#123456>
Please Remeber to Click <mark><b><font color=red>Preview</font></b></mark> Button on the right-bottom cornor of the panel once set parameters properly.
</div>

#### 1.3 Essential information

### 2. Data Processing

### 3. Network Construction

### 4. Network Visualization

### 5. Network Analysis
