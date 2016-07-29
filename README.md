# README for ggspectra #

Package '**ggspectra**' is a set of stats, geoms and functions to be used together with packages 'ggplot2' and 'photobiology' for plotting radiation-related spectra and annotating the plots with quantities derived from spectral data. Unweighted derived quantities represent summaries of a given range of wavelengths, which can be expressed either in energy or photon based units. Derived _biologically effective_ quantities are used to quantify the effect of radiation on different organisms or processes within organisms. These effects can range from damage to perception of informational light signals. Peaks and valleys can also be annotated.

The following statistics, geoms and methods are exported:

* `ggplot()` methods specialized for the spectral classes defined in package 'photobiology' adding a default _x_ and _y_ mappings.
* `plot()` methods specialized for the spectral and `waveband` classes defined in package 'photobiology' returning `ggplot` objects for fully labelled and annotated plots.
* `stat_color()` calculate RGB colours from wavelengths mapped to _x_ aesthetic.
* `stat_peaks()` find and label local or global maxima in _y_.
* `stat_valleys()` find and label local or global minima in _y_.
* `stat_wb_contribution()` summarize _y_ for ranges, defined by `waveband` objects.
* `stat_wb_e_irrad` integrate energy irradiance, mapped to _y_, for wavebands.
* `stat_wb_e_sirrad` integrate spectral energy irradiance, mapped to _y_, for wavebands.
* `stat_wb_irrad` integrate irradiance, mapped to _y_, for wavebands.
* `stat_wb_label` label ranges of _x_ under spectral curve according to wavebands.
* `stat_wb_mean` integrate _y_ over ranges defined by `waveband` objects.
* `stat_wb_q_irrad` integrate photon (= quantum) irradiance, mapped to _y_, for wavebands.
* `stat_wb_q_sirrad` integrate spectral photon (= quantum) irradiance, mapped to _y_, for wavebands.
* `stat_wb_relative` integrate _y_, over ranges of _x_ defined by `waveband` objects.
* `stat_wb_sirrad` integrate spectral irradiance, mapped to _y_, for wavebands.
* `stat_wb_total` integrate _y_ over ranges of _x_ for spectral curves.
* `stat_wl_strip` calculate RGB colours from wavelength mapped to _x_ aesthetic.
* `stat_wl_summary` summarize _y_ for regions defined as ranges of wavelengths.
* `wl_guide` calculate colours from wavelengths mapped to _x_ and possibly add a visual depiction of the wavelengths as a color strip, or a color background to a plot.

This package uses the new functionality added to `ggplot2` at version 2.1.0 and is not compatible with earlier versions.

Please, see the web site [r4photobiology](http://www.r4photobiology.info) for details on other packages available as part of the suite, and on how to install them.