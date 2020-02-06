# dccmonitor

This package is intended to assist Sage Bionetworks data curators to check the status of metadata and documentation files uploaded via the [dccvalidator](https://sage-bionetworks.github.io/dccvalidator/) shiny application. The dccmonitor package contains functions to gather validation information. Optionally, the included Shiny application can be used to view the results.

**Note:** The application takes time to fully populate the validation information and currently does not have a progress indicator. The application will initialize the study boxes after checking for team membership and gathering the studies represented in the `consortium_fileview` (see Customization for details on this file view). After this, the application will populate the study boxes after the validation checks have run for all represented studies.

## Requirements

dccmonitor uses the reticulate package with the [Synapse Python Client](https://github.com/Sage-Bionetworks/synapsePythonClient). See the [reticulate documentation](https://rstudio.github.io/reticulate/#python-version) for more information on setting up reticulate to work with your local Python environment. Additionally, see the [Synapse Python Client](https://github.com/Sage-Bionetworks/synapsePythonClient) for installation instructions. The Synapse Python Client should be installed in the same Python environment used by reticulate.

Using the dccmonitor Shiny application requires that the user have a Synapse account, and have permission to access necessary Synapse files. These files include the specific Synapse folder of interest for monitoring (see [Customization](https://github.com/Sage-Bionetworks/dccmonitor/tree/update-readme#customization) for details), along with access to all dccvalidator Synapse dependencies (ex: template and annotation files).

## Installation and Use

dccmonitor can be installed and used in two ways: package installation, and cloning the repository.

### Package Installation

dccmonitor can be installed via devtools:

``` R
devtools::install_github("Sage-Bionetworks/dccmonitor")
```

To run the Shiny application, there needs to be a config.yml file in the working directory with the required options (see [Customization](https://github.com/Sage-Bionetworks/dccmonitor/tree/update-readme#customization)). After creating the config.yml file, the application can be run with:

``` R
library(dccmonitor)
Sys.setenv(R_CONFIG_ACTIVE = "default") # Replace "default" with the configuration name if not using default.
run_app()
```

### Cloning Repository

[Clone the repository](https://help.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository) to a local directory. Customize the config.yml file, as needed (see [Customization](https://github.com/Sage-Bionetworks/dccmonitor/tree/update-readme#customization)).

If your working directory is the application directory, you can change the app.R file to have your configuration name, if not using the default configuration. Then the following will start the app:

``` R
renv::restore() # Update project package library
shiny::runApp()
```

## Customization

Customizing the application is done via the config.yml file and the specific file validation checks.

### config.yml

A configuration file is required for the application to behave correctly. Create a configuration for the application called config.yml (see [this repo's config.yml](https://github.com/Sage-Bionetworks/dccmonitor/blob/master/config.yml) for an example) in your working directory if you have installed the package, or alter the config.yml file if you have cloned the repository. If you are not using default as your configuration, set the active configuration as described in the instructions for your choice of installation, [package](https://github.com/Sage-Bionetworks/dccmonitor/tree/update-readme#package-installation) or [cloned repository](https://github.com/Sage-Bionetworks/dccmonitor/tree/update-readme#cloning-repository). Alternatively, if you have forked the repository and added a new set of configuration options to the config.yml file, you can open a Pull Request to see about adding the new options.

**Note:** a default configuration is required in the config.yml file. Additionally, if using a non-default (named) configuration, any settings in default that are not overwritten by the named configuration will be inherited.

Of the configuration options, only two are specific to dccmonitor while the rest are used to customize the dccvalidator checks. The dccmonitor specific configurations are:

- `teams`: Synapse administration team ID. The app data should only be accessible to curators with admin privileges.
- `consortium_fileview`: Synapse file view ID. A Synapse file view that shows all the files in the `parent` folder (this is the folder that dccvalidator uploads metadata and documentation files to), along with their annotations. The file view should include the columns: id, name, createdOn, createdBy, modifiedOn, currentVersion, study, metadataType, species, assay. Note that the following file-specific annotations are required for dccmonitor to function properly:
    - documentation: study
    - manifest: study, metadataType = manifest
    - individual metadata: study, species, metadataType = individual
    - biospecimen metadata: study, species, metadataType = biospecimen
    - assay metadata: study, species, assay, metadataType = assay

A brief overview of the dccvalidator specific configurations are below, but more information can be found in the [dccvalidator documentation](https://sage-bionetworks.github.io/dccvalidator/articles/customizing-dccvalidator.html#configuration-options):

- `annotations_table`: Synapse ID for the annotations master table.
- `templates`: List of master templates for each type of metadata or manifest file.
- `species_list`: List of species.
- `complete_columns`: List of columns that are required to be complete for each type of metadata or manifest file.
  
### Validation Checks

dccmonitor uses dccvalidator to validate the metadata and manifest files. Currently, this is done in a set of functions that are updated to mirror the most recent version of the dccvalidator application. To customize the validation checks performed, the following functions would need to be changed:

#### validate-by-study.R

- `validate_study()`: handles the validation of an individual study, sending files on to be validated by themselves and doing validation checks across files.

#### validate-by-file.R

- `validate_manifest()`: validation checks specific to the manifest.
- `validate_individual_meta()`: validation checks specific to the individual metadata.
- `validate_biospecimen_meta()`: validation checks specific to the biospecimen metadata.
- `validate_assay_meta()`: validation checks specific to the assay metadata.
