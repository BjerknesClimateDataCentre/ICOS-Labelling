About the Merge Script
==========================================================================

### Description ###
This script joins two datasets together by close enough matched time stamps.
The allowed time difference is set json settings file. The joining is a
left-join, meaning the merged dataset contains all rows from the primary
dataset (typically the CO2 dataset), and matches from the 'secondary' dataset
(typically the hydrography data) are only included if rows are withing the
allowed time difference.

### Requirements ###
1. The two datasets to merge must either be in the input folder, or they must
be found in the data folder (See point 2 below how this is specified in the
settings file)
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- "date_column_pri" : Name of the date column. If date is split in three columns
(day, month, year), write in this way: "year_col, month_col, day_col". If the
column name contains special characters (e.g. 'Date/Time'), use a dot
('Date.Time') instead.
- ....


### Output ###
The merged data will be located in the output folder