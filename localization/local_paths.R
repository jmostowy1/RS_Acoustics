##### This script defines the local paths for data sources and EV files, as well as the destination paths for outputs

#Local EV repository: This is where all my EV files live
local.EV.repository = "G:\\My Drive\\School\\Thesis\\Data_Analysis\\Acoustic_Analysis\\EV_File_Repository"

#Local EV metadata csv location. This file contains information for every single EV file: Station name, dates, ev file name, type of survey, etc. I use it to quickly subset stations I would like to perform multiple operations on
local.EVMetadata.loc = "G:\\My Drive\\School\\Thesis\\Data_Analysis\\Acoustic_Analysis\\EV_file_metadata\\ev_file_metadata.csv"

#Local export root path. This is a path to the highest level in my export folder. Basically, data that I export from echoview lives here, or in a subfolder in here. I have subfolders within this folder, so to actually export something to the proper folder I combine this with a subfolder name
local.export.root = "G:\\My Drive\\School\\Thesis\\Data_Analysis\\Acoustic_Analysis\\EV_exports"

