---
title: "Many_Analysts"
author: "Analyst: Sabrina McNew"
date: "7/10/2020"
output: html_document
---


#### Project info: https://osf.io/34fzc/
#### Contact: sm983@cornell.edu
  
### Project goals
The goal of this project is to analyze a dataset of breeding blue tits 
in order to answer the question "does competition affect growth of tit nestlings?"

### Project approach outline (updated July 10, 2020)
Use linear models to test for sources of variation in tarsus length, nestling 
mass, and fledging success. Covariates include, treatment (increased, decreased,
or control brood size), brood size, lay date, nestling sex, whether the nestling 
was transported into the nest or not. Random effects include: parent ids and geographical
identifiers/structures of the nest, year. 

Secondarily I tested whether the *variance* in tarsus length and width within 
a brood varies with clutch size or treatment. 
