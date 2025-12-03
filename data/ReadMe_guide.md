
---
### Expanding and updating the FINS dataset

The data collection to create the FINS dataset commenced in 2020. We have downloaded all neoselachian occurrences and their associated collections from the Cretaceous and Cenozoic from the PBDB in June 2020. As many publications describing neoselachian fossils were published in the last 5 years, we  provide an option to apply the same data treatment used when creating FINS on new data downloaded from PBDB and to merge this data with FINS to create a more up to date dataset. Please read the following guide before uploading new data. 

### Downloading data from PBDB

Data from many publications published after 2020 were entered into PBDB. If you would like to include data from a publication not in PBDB yet, we recommend first entering it into PBDB and then downloading the data from PBDB. 

Below are the parameters we recommend using when downloading data from the [PBDB](https://paleobiodb.org/classic/displayDownloadGenerator), listed by type of data and by sections:
- **Occurrences**
  - _Select by taxonomy_
    - **Taxon or taxa to include**: neoselachii
  - _Select by time_
    - **Interval or Ma range**: Cretaceous _through_ Cenozoic
  - _Select by metadata_
    - **Occurrences entered after**: 2021
  - _Choose output options_
    - tick **ref attribution**
  
- **Collections**
  - _Select by taxonomy_
    - **Taxon or taxa to include**: neoselachii
  - _Select by time_
    - **Interval or Ma range**: Cretaceous _through_ Cenozoic
  - _Select by metadata_
    - **Collections entered after**: 2021
  - _Choose output options_
    - tick **location**, **geological context**, and **ref attribution**

### _Add data_ tab

Once you have downloaded the data you will be able to upload them into the application via the Add data tab (top right). In this tab the new data can be previewed, certain data treatments can be applied (details provided below) and finally this data can be appended to the FINS dataset.

Each datapoint (collection or occurrence) will be marked as _PBDB_U_ in the "source" column, making it easily distinguishable in the combined dataset.

There are 3 types of files that can be uploaded:

  1. **Collections**
     - Collections downloaded from PBDB should be uploaded first, as collection information will be appended to the associated occurrences in the next step.
     - The application will automatically extract the following information from the uploaded file - collection number, country, state, latitude, longitude, formation, member, stratigraphic scale, collection name, collection aka, early and late interval, max ma and min ma, reference, and collection comments.
     - The application will automatically populate the following columns based on the raw data:
       - continent - based on the country column
       - latitude band - based on the latitude column
       - late interval - if left blank in PBDB, it means it is the same as early interval, thus the value from the early interval column is copied
       - age range - based on min and max ma
       - interval type - based on early interval
       - early and late epoch, early and late period, early and late era - based on the early and late interval columns
     - The option to calculate paleo-coordinates and to assign paleooceans for the collections can be selected separately, using tick-boxes underneath the upload box. These two processes are not done automatically as they can take some time to compute, depending on the number of collections uploaded. You will therefore be able to first preview the data and only initiate the calculation once you are happy with the data.
     - Once the calculation is finished, you can append the new collections to FINS

  2. **Occurrences**
     - Once new collections were appended to FINS, the new occurrences can be uploaded
     - The application automatically extracts the following information - occurrence number, collection number, identified name, reference
     - The following columns are populated by extracting information from the associated collecions - max ma, min ma, age range, early and late interval/epoch/period/era, interval type, (paleo)latitude, (paleo)longitude, continent, latitude band, paleoocean
     - The application automatically populates the following columns based on the raw data:
       - modified identified name - identified name but stripped of any potential taxonomic classifiers (e.g., nov. sp., n. gen., etc.)
       - accepted name - valid name based on the modified identified name column; if the name already represents a valid name, the two columns will be identical, if the name represents an outdated synonym, the corresponding valid name will be provided in the accepted name column
       - rank - based on accepted name - **Please note that the application only recognizes the following ranks - species, genus, family, order, superorder, clade, and subcohort. If the occurrence rank represents any other rank (e.g. infraorder, subfamily) this will have to be added manually.**
       - higher taxonomy (genus, family, order, superorder) - based on accepted name column
       - status - based on accepted name column
       - genus status - based on genus column, if genus is not known, the status is NA
     - **IMPORTANT: The valid names and higher taxonomy are automatically assigned based on the taxonomic list used during the creation of FINS. If you wish to implement an alternative taxonomic list, see the instructions below (3. Alternative taxonomic list).** 
     - Please carefully evaluate the data in the preview. If name was misspelled in the identified name column, it will be flagged in the accepted name column as _unknown_. You can search for specific terms in the search bar in the upper right corner. We recommend fixing such issues directly in the .csv file downloaded from PBDB and reuploading it into the application.
     - Once happy with the data, you can append them to FINS.

### IMPORTANT: Curation of the data (i.e., evaluation of reliability of data associated with collections and occurrences) cannot be automated and should be carried out by the user. Curation criteria implemented during the creation of FINS can be accessed in the manuscript.


  3. **Alternative taxonomic list**
     - Taxonomy and classification change constantly, it is therefore possible that you may wish to use an alternative, more up-to-date taxonomic list, as opposed to the list used when creating the FINS dataset.
     - The alternative list needs to have the same structure as the list used in FINS. This list can be accessed via [this](link to Zenodo repo) repository.
     - Once such list is prepared, it can be uploaded in the last section of the _Add data_ tab.
     - This will make a new option available in the _Occurrences_ tab, in the _Taxonomic list_ subsection. In this subsection, you will be able to select which taxonomic list will be used to assign valid names and higher taxonomy in all Occurrences.
     - If you wish to use an alternative taxonomic list, and you are also adding new data, make sure you **first upload** and **append** the new data to FINS, and only **apply the alternative taxonomic treatment after**.



