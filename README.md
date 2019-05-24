# MAPS-eyetracking
Working with eyetracking data for MAPS

### Code is organized into four scripts:

1_Read Data.R: This reads tab delimited data files for each participant, combines them into a single .rds file, and saves it
2_Add AOIs.R: This reads AOIs from the file `AOI List.xlsx` (where they are manually inputted), adds them to the dataset, and saves it
3_Make Eyetracking Data.R: This uses `eyetrackingR`'s `make_eyetracking_data` function to transform the dataset into one that is ready for analysis
4_Analysis.R: TBD analysis

### Additional files: 

Eyetracking Lookup.yaml: This is a yaml file, which is a markup language used to store information in a human-readable format. This includes long filepaths that we don't want to call individually from within the R scripts, and names them. Then, when we're calling these files from our R scripts, we only need to use the yaml names for them.

Data/Eyetracking/AOI List.xlsx (only on Northwestern server): This is an Excel file where we have manually inputed borders (in pixels) for each Area of Interest for each question. The AOI Documentation folder includes screenshots of each question and its AOIs, for reference.

### Contact:

Code author Isaac Ahuvia, isaac.ahuvia@northwestern.edu or isaacahuvia@gmail.com
