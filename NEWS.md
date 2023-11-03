All notable changes to this project will be documented in this file.

The format is loosely based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), i.e.:

- There should be an entry for every single version.
- The latest version comes first.
- The release date of each version is displayed.
- The same types of changes should be grouped.
- The following keywords are used to denote different types of changes:
  - `Added` for new features
  - `Changed` for changes in existing functionality
  - `Deprecated` for soon-to-be removed features
  - `Removed` for now removed features
  - `Fixed` for bug fixes
  - `Security` in case of vulnerabilities
  - `Infrastructure` for updates of files not related to the package itself,
    e.g. .github/workflows/*, README.md, etc. Infrastructure updates increase
    the patch version.

# 3.3.2.0010
    - `Changed`: rROC save_path is now the actual savepath, no ".qs" is added anymore
  
# 3.3.2.0009
    - `Added`: plot_density_rROC_empirical with points now!

# 3.3.2.008
    - `Fixed`: Bug when "current_file_oldversion" has been used

# 3.3.2.004
    - `Added`: plot_rROC

# 3.3.2
    - `Fixed`: devtools::check() now passes

# 3.3.1
    - `Added`: Documentation for rROC_models included 

# 3.3.0
    - `Added`: rROC_model based on h2o included: ``train_rROC_h2o`` and ``predict_rROC_h2o``

# 3.2.2
    - `Added`: rROC.data.frame() can now handle ``dependent_vars`` _or_ ``y`` as input

# 3.2.1
    - `Added`: Add rROC() as _the_ goto function for restricted ROC analysis


# 3.1.7

    - Add the possibility to have more than 2 groups in the data (and define the positive group, ONE vs ALL)

# restrictedROC 3.1.6

  - Added quiet=TRUE into pROC::roc() within predict.restrictedROC()

# restrictedROC 3.0.0

  - Added code and articles from the former gitlab repository
    [restrictedROC-gitlab](https://git.uni-regensburg.de/03_projects/theory/restrictedROC).

# restrictedROC 0.0.0.9000

* Initialization.
