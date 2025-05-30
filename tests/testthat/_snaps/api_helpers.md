# Can print microdata extracts

    Code
      print(test_usa_extract())
    Output
      Unsubmitted IPUMS USA extract 
      Description: Test USA extract
      
      Samples: (1 total) us2017b
      Variables: (2 total) RACE, YEAR

---

    Code
      print(test_cps_extract())
    Output
      Unsubmitted IPUMS CPS extract 
      Description: Compare age-sex-race breakdowns 1976
      
      Samples: (2 total) cps2018_03s, cps2019_03s
      Variables: (3 total) AGE, SEX, RACE

---

    Code
      print(test_ipumsi_extract())
    Output
      Unsubmitted IPUMS International extract 
      Description: Test IPUMSI extract
      
      Samples: (2 total) mx2015a, cl2017a
      Variables: (3 total) AGE, SEX, EDATTAIN

---

    Code
      print(test_atus_extract())
    Output
      Unsubmitted IPUMS ATUS extract 
      Description: Test ATUS extract
      
      Samples: (2 total) at2020, at2021
      Variables: (5 total) STATEFIP, AGE, SEX, DIFFANY, RELATER
      Time Use Variables: (2 total) ACT_PCARE, my_time_use_var

# Can print aggregate data extracts

    Code
      print(test_nhgis_extract())
    Output
      Unsubmitted IPUMS NHGIS extract 
      Description: Extract for R client testing
      
      Dataset: 2014_2018_ACS5a
        Tables: B01001, B01002
        Geog Levels: nation
      
      Dataset: 2015_2019_ACS5a
        Tables: B01001, B01002
        Geog Levels: blck_grp
      
      Time Series Table: CW3
        Geog Levels: state
        Years: 1990
      
      Geographic extents: 110, 100
      
      Shapefiles: 110_blck_grp_2019_tl2019

---

    Code
      print(test_nhgis_extract_shp())
    Output
      Unsubmitted IPUMS NHGIS extract 
      Description: 
      
      Shapefiles: 110_blck_grp_2019_tl2019

---

    Code
      print(test_ihgis_extract())
    Output
      Unsubmitted IPUMS IHGIS extract 
      Description: Extract for R client testing
      
      Dataset: AL2001pop
        Tables: AL2001pop.ADF, AL2001pop.ADG
        Tabulation Geogs: AL2001pop.g0, AL2001pop.g1

