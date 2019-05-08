---
title: "File Descriptions"
output: 
        html_document:
          keep_md: yes
          toc: yes
---




# Received Files  

When I received a SET file from a reserve, I saved it to the `SETr_data_transformations/data/submitted` directory. I saved it using the naming convention `yyyy-mm-dd_ReserveCode`, where `yyyy-mm-dd` is the date the file was received.  

Some files were `.xlsx` format and others were `.xls`. I left the file extension as-is.  


# Differences in QA/QC coding  

Each reserve has handled notes / observations / flags and codes slightly differently. In this project, we developed a list of common "things" that need to be documented, and created standardized codes to identify them. The list of codes can be found [here](https://docs.google.com/spreadsheets/d/1cqxi_Bz1CjpticfrPLf1_GgsoN2g_V1jlV9gAbzN6xw/edit?usp=sharing).  

Some Reserves added these codes to their own files before sending them to me; with other reserves, I generated the Excel output file and sent it back to them requesting that QA/QC codes be inserted. I have attempted to do as much as possible within R, and document any manual QA/QC edits in a reserve's data transformataion script so that if the file needs to be regenerated, we know what was done outside of R.  

### Fully automated  

The following reserve files did not require any manual modification after running the data transformation scripts:  

+  APA  
+  CBV-GI  
+  DEL
+  GND  
+  PDB  (on site SETs)  
+  WQB  



### QA/QC codes added after file generated  

+  CBM  (in progress at reserve)  
+  ELK  (in progress at reserve)  
+  GRB  (complete)
+  NAR  
+  PDB  (in progress at reserve)
+  SOS  (modified by reserve; complete)
+  WKB  
