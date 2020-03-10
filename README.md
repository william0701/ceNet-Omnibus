# CeNet Omnibus

## Introduction

The ceRNA regulation is a newly discovered post-transcriptional regulation mechanism and plays significant roles in physiological and pathological progress. The analysis of ceRNAs and ceRNA network has been widely used to detect survival biomarkers, select candidate regulators of disease genes, and pre-dict long noncoding RNA functions. However, there is no software platform to provide overall functions from construction to analysis of ceRNA networks. To solve this problem, we introduce CeNet Omnibus, a R/Shiny application, which provides a unified framework of ceRNA network construction and analysis. CeNet Omnibus is characterized by comprehensiveness, high efficiency, high expandability and user customizability, and it also offers the web-based user-friendly interface for users to obtain the output intuitionally in time.

CeNet Omnibus consists of five components, including **Data Input**, **Data Processing**, **Network Construction**, **Network Visualization** and **Netwoek Analysis**. The framework of CeNet Omnibus is shown below.

![](https://raw.githubusercontent.com/william0701/Figures/master/CeNetOmnibus/framework.svg)

## Installation

To install the package from the github repository please use the following code.

```r
if (!requireNamespace("BiocManager", quietly=TRUE))    install.packages("BiocManager")
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

The expression profiles of ceRNAs and microRNAs should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. To name the datasets, please confirm if the program should name dataset with/without the first column. The rows of uploaded files should represent ceRNAs/microRNAs, while the columns should represent samples.

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

> NOTE: Please Remeber to Click **Preview** Button on the right-bottom corner of the panel once set parameters properly.

#### 1.2 The Interaction between ceRNAs and microRNAs

The interactions file between ceRNAs and microRNAs should be 0-1 matrix to represent if there are interactions between ceRNAs and microRNAs. The file should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. To name the datasets, please confirm if the program should name dataset with/without the first column. The rows of uploaded files should represent ceRNAs, while the columns should represent microRNAs.

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

> NOTE: Please Remeber to Click **Preview** Button on the right-bottom corner of the panel once set parameters properly.

#### 1.3 Essential ceRNA Information

The Enssential ceRNA Information will be used to group ceRNAs and set network/module visualization

The Essential ceRNA Information can be supplied by two ways:

- **Biomart Download**

- **Custom Upload**

For **Biomart Download** method, users can select Ensembl Archieve, Species, Gene ID Type, and search items in the Ensembl Database. Users are able to select a part of ceRNAs in the input datasets.

For **Custom Upload** method, the uploaded file should be plain text delimited by tab, comma, space, semicolon or any other prac-ticable marks. Users can set seperators, quotes, with/without headers. To name the datasets, please confirm if the program should name dataset with/without the first column. The rows of uploaded files should represent ceRNAs, while the columns should represent the information of ceRNAs. The first column of the uploaded file should be the ceRNA identifiers.

<center><b>CeRNA Information Samples</b></center>

|                 | ensembl\_gene\_id | description                                                                | strand | external\_gene\_name | gene\_biotype    |
| --------------- | ----------------- | -------------------------------------------------------------------------- | ------ | -------------------- | ---------------- |
| ENSG00000006377 | ENSG00000006377   | distal\-less homeobox 6 \[Source:HGNC Symbol;Acc:HGNC:2919\]               | 1      | DLX6                 | protein\_coding  |
| ENSG00000010361 | ENSG00000010361   | fuzzy planar cell polarity protein \[Source:HGNC Symbol;Acc:HGNC:26219\]   | \-1    | FUZ                  | protein\_coding  |
| ENSG00000010438 | ENSG00000010438   | serine protease 3 \[Source:HGNC Symbol;Acc:HGNC:9486\]                     | 1      | PRSS3                | protein\_coding  |
| ENSG00000036054 | ENSG00000036054   | TBC1 domain family member 23 \[Source:HGNC Symbol;Acc:HGNC:25622\]         | 1      | TBC1D23              | protein\_coding  |
| ENSG00000047249 | ENSG00000047249   | ATPase H\+ transporting V1 subunit H \[Source:HGNC Symbol;Acc:HGNC:18303\] | \-1    | ATP6V1H              | protein\_coding  |
| ENSG00000056998 | ENSG00000056998   | glycogenin 2 \[Source:HGNC Symbol;Acc:HGNC:4700\]                          | 1      | GYG2                 | protein\_coding. |

> NOTE: Please Remeber to Click **Preview** Button on the right-bottom corner of the panel once set parameters properly.

> WARNING: The Biomart Download may take relatively longer time, especially for the users on the Chinese Mainland.

### 2. Data Processing

**Data Processing**  provide the interface to *Group CeRNAs*, *Filter Bad Samples*, *Filter Non-Expressed MicroRNAs/CeRNAs* and *Value Transformation*. 

When enter the **Data Processing** tab, the program will obtains the common ceRNAs, microRNAs and samples in the uploaded  files.

![](https://github.com/william0701/Figures/blob/master/CeNetOmnibus/Valid_Gene.png)

#### 2.1 Gene Grouping

Users are allowed to group ceRNAs into different groups to set different paramenters for the ceRNA filter and network construction. For example, the non-coding RNAs, especially long non-coding RNAs (lncRNAs)  usually have relatively lower expression levels. The thresh of non-expressed lncRNAs may be lower than that of mRNAs.

Users can group ceRNAs according to the essential ceRNA information uploaded in step 1.3.  Validated columns are listed. 

![](https://github.com/william0701/Figures/blob/master/CeNetOmnibus/Gene_Mapping_1.jpg)

> NOTE: Click **Preview** Button on the right-bottom corner of the panel once set parameters properly.
> The ceRNAs of non-selected items will be removed in the next processing.

![](https://github.com/william0701/Figures/blob/master/CeNetOmnibus/Group_statistic.svg)

#### 2.2 Sample Filter

There may be some bad samples in the ceRNA/microRNA expression profiles. This section allows users to remove these bad samples.

Firstly, users should set the thresh of a good microRNA/ceRNA in samples. For example, we think the CPM of a microRNA larger than 50 is good, while the RPKM of a ceRNA larger than 0.1 is good. Then, the program will calculate the good microRNAs/ceRNAs ratio each sample, and create the distribution plot. Finally, users need to decide how many samples should be remained by sliding the percentile bar.

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/Sample_Filter1.jpg)

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/microSampleFilter.svg)

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/RNASampleFilter.svg)

> NOTE: Click **Preview** to create the plot, and the change of Percentile bar will update the plot.

> NOTE: Please Remeber to Click **Filter** Button on the right-bottom corner of the panel once set parameters properly to execute the Sample Filter.

#### 2.3 RNA Filter

Because the expressions of ceRNAs and microRNAs have tissue-specificity, non-expressed microRNAs and ceRNAs should not appear in the ceRNA networks. This section allow users to remove non-expressed microRNAs and ceRNAs.

Firstly, users should set the minimal expression thresh of a expressed microRNA or ceRNA . For example, we think the CPM of a microRNA larger than 100 is expressed, while the RPKM of a noncoding ceRNA larger than 0.5 is expressed and 1 for a coding ceRNA. Then, the program will calculate the expressed sample ratio of each RNA, and create the distribution plot. Finally, users need to decide how many samples a RNA  should express in by sliding the Minimal Sample Ratio bar.
![](https://github.com/william0701/Figures/blob/master/CeNetOmnibus/RNA_Filter1.jpg)

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/microStatistic.svg)

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/NoncodingStatistic.svg)

![](http://github.com/william0701/Figures/blob/master/CeNetOmnibus/CodingStatistic.svg)

> NOTE: Click **Preview** to create the plot, and the change of Percentile bar will update the plot.

> NOTE: Please Remeber to Click **Filter** Button on the right-bottom corner of the panel once set parameters properly to execute the RNA Filter.


#### 2.4 Value Transformation

### 3. Network Construction

#### 3.1 Choose Measurement

#### 3.2 Measurement Setting

#### 3.3 Network Construction

### 4. Network Visualization

### 5. Network Analysis

#### 5.1 Network Topological Property

#### 5.2 Network Module

#### 5.3 Enrichment Analysis

#### 5.4 Survival Analysis
