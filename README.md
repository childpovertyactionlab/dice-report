# dice-report

This tool tracks the progress of the DICE initiative across the City of Dallas. It is designed to integrate new data overtime, as the initiative will run over the course of a year. It was created using Quarto, and the structure generated through R-Markdown. R has been used for figures and analysis. CSS and html were included for visual improvements and interactivity. 

### Data ###
Data comes into the tool through a Google Sheet, consisting of clean data from multiple surveys. Some data is not collected through the surveys, and must be input manually. Once in the tool, data input from partners is filtered and analyzed to generate several figures. Data is displayed at the city, division, and organizational levels. The tool will automatically update with the current data in the Google Sheet when re-rendered.  

### Narrative and Photos ###
In addition to numerical figures, the tool contains a narrative aspect reflecting qualitative data and media collected from the communities being served. This content will not change, but more may be added over time. Photos are in the media folder, sorted by initiative. As photos are submitted, they can be placed into the existing structure of the photo galleries.

### Maps ###
The tool includes several maps. Since locations have been predetermined, all addresses have been geo-coded ahead of time. Every row is already in the set, just filtered out to a selected date. Currently, the filter date is the last day of the previous month (last update 7/10/24).  

### Future Additions ### 
The tool is designed in a modular way, with uniform sections containing different data. Future initiatives can be added by replicating sections, and adding new columns for outputs in the Google Sheet.  
