# dccmonitor

This package is intended to assist data curators to check the status of metadata and documentation files uploaded via the [dccvalidator](https://sage-bionetworks.github.io/dccvalidator/) shiny application. dccmonitor is primarily a shiny application, with the ability to use functions for gathering validation information using the package functions only, if desired.

## Installation

dccmonitor uses the reticulate package with the [Synapse Python Client](https://github.com/Sage-Bionetworks/synapsePythonClient). See the [reticulate documentation](https://rstudio.github.io/reticulate/#python-version) for more information on setting up reticulate to work with your local Python environment. Additionally, see the [Synapse Python Client](https://github.com/Sage-Bionetworks/synapsePythonClient) for installation instructions.

dccmonitor can be installed via devtools:
``` R
devtools::install_github("Sage-Bionetworks/dccmonitor")
```

## Shiny Application

Using the dccmonitor shiny application requires that the user have a Synapse account, and the user needs access to the specific Synapse folder of interest for monitoring (see Customization for details).

After installing the package, the following will run the application locally:
``` R
library(dccmonitor)
shiny::runApp()
```

## Customization

Customizing the application is done via the config.yml file and the specific file validation checks.

### config.yml

Of the configuration options, only two are specific to dccmonitor while the rest are used to customize the dccvalidator checks (see [Customizing the dccvalidator](https://sage-bionetworks.github.io/dccvalidator/articles/customizing-dccvalidator.html)). The dccmonitor specific configurations are:

- `teams`: Synapse team ID. This is used to verify that the user is a member of the team and, therefore, has access to the folder containing the metadata and documentation files of interest.
- `consortium_fileview`: Synapse file view ID. A Synapse file view that shows all the files in the `parent` folder (this is the folder that dccvalidator uploads metadata and documentation files to), along with their annotations. The file view should include the columns: id, name, createdOn, createdBy, modifiedOn, currentVersion, study, metadataType, species, assay. Note that the following file-specific annotations are required for dccmonitor to function properly:
    - documentation: study
    - manifest: study, metadataType = manifest
    - individual metadata: study, species, metadataType = individual
    - biospecimen metadata: study, species, metadataType = biospecimen
    - assay metadata: study, species, assay, metadataType = assay

Simply set a configuration for the application in the config.yml and change "default" in run-app.R to your configuration name before running.

### Validation Checks

dccmonitor uses dccvalidator to validate the metadata and manifest files. Currently, this is done in a set of functions that are updated to mirror the most recent version of the dccvalidator application. To customize the validation checks, the following functions would need to be changed:

#### validate-by-study.R

	- `validate_study()`: handles the validation of an individual study, sending files on to be validated by themselves and doing validation checks across files.

#### validate-by-file.R

	- `validate_manifest()`: validation checks specific to the manifest.
	- `validate_individual_meta()`: validation checks specific to the individual metadata.
	- `validate_biospecimen_meta()`: validation checks specific to the biospecimen metadata.
	- `validate_assay_meta()`: validation checks specific to the assay metadata.
