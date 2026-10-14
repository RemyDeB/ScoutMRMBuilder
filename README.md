This repository is a copy of the [gitlab](https://gitlab.com/chemod/scout_mrm)

# Scout-MRM Builder

Scout-MRM Builder provides a convenient workflow to generate highly multiplexed MRM methods from untargeted LC-MS/MS data (DDA or DIA), based on a scout-triggered MRM approach.

## Description

This repository contains functions to generate scout-triggered MRM methods using MS/MS spectra acquired from untargeted experiments (DDA or DIA).

## Usage

Untargeted experiments must first be processed to extract MS² spectra. This step can be performed using software such as XCMS, MZmine, MS-DIAL, MassCube, or any equivalent tool.

Within these tools, you should:
- Perform peak detection in MS¹
- Associate MS² spectra with detected features (for DDA data)

For DIA data, additional processing is required:
- Perform peak detection in the MS² dimension
- Reconstruct MS² spectra (refer to the documentation of your processing software for details)

Additional preprocessing steps are recommended, such as:
- Removing signals present in blank samples
- Removing isotopic peaks
- Filtering low-quality spectra

The objective is to export MS² spectra in a format compatible with the [Spectra package](https://bioconductor.org/packages/3.22/bioc/html/Spectra.html), including the following information:
- Precursor m/z
- Precursor intensity
- Retention time
- Fragment m/z
- Fragment intensity

An easy-to-follow tutorial is available here: [vignette/Guide_StMRM_Builder.Rmd](https://gitlab.com/chemod/scout_mrm/vignette)

## Installation
```
devtools::install_gitlab("chemod/scout_mrm")
```

## Questions

If you have any questions or encounter issues, please open an issue in this repository.

## Citation
If you use this package, please cite:

```
Rémy De Boni, Guillaume Rossignol, Delphine Arquier, Benoît Joseph, Guy Fournet, Jérôme Lemoine, Jérôme Randon, Sophie Ayciriex, Yohann Clement, Arnaud Salvador (2026). "Scout-triggered MRM from untargeted data: An R-based strategy for multiplexed targeted metabolomics" Talenta, 307, doi : 10.1016/j.talanta.2026.129865.
(https://www.sciencedirect.com/science/article/pii/S0039914026005217)
```

