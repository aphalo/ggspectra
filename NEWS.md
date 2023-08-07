---
editor_options: 
  markdown: 
    wrap: 72
---

# ggspectra 0.3.12

- Improve how plots created with autoplot() look, e.g., limit number of digits
in labels and improve some annotations in the case of multiple spectra.
- Fix bug in `autoplot.source_spct()` introduced in version 0.3.10: bad handling
of single waveband objects as argument to `w.band`.
- Track bug fix in 'photobiologyWavebands', that updated the definition of
`PAR()` (photosynthetically active radiation) as a BSWF (biological spectral
weighting function). The labelling as PAR is now restored when plotting
spectral photon response using the `autoplot()` method. 

# ggspectra 0.3.11

-   'photobiology' (>= 0.10.16) is needed as a bug in 'photobiology' 
(<= 0.10.15) can trigger an occasional error in `autoplot()`.
-   Bug fix, attempt in `autoplot()` to use `linetype` aesthetic with more than
the 13 supported levels. This changes plots with more than 13 spectra
compared to earlier versions, but in these plots some spectra were missing. New
behaviour is to use semi-transparent solid lines for more than 13 spectra.

# ggspectra 0.3.10

-   Track bug fix in 'photobiologyWavebands', that updated the definition of
`PAR()` (photosynthetically active radiation) as a BSWF (biological spectral
weighting function). The previous definition of `PAR()` as a wavelength range
returned a value that should be called "PAR" only when used to compute photon
irradiance. This change does not affect the numerical values of energy
irradiances or other spectral quantities, but now the waveband is labelled _PhR_
instead of _PAR_ in such cases.
-   Track changes in 'photobiology' 0.10.15.
-   Add `sec_axis_energy_eV()` and `sec_axis_energy_J()` to express wavelength
as energy per photon, and `sec_axis_wl()` to allow a secondary wavelength axis
with wavelengths expressed using a different scale factor than in the main axis.
-   Add scales `scale_x_wavenumber_continuous()`, `scale_x_frequency_continuous()`.
`scale_x_energy_eV_continuous()` and `scale_x_energy_J_continuous()`.
-   This version requires R (>= 4.1.0) and 'ggplot2' (>= 3.4.0).

# ggspectra 0.3.9

-   Maintenance release for compatibility with 'ggplot2' >= 3.4.0.
-   Several bug fixes.
-   Revise all `autoplot()` methods adding formal parameter `geom`.

# ggspectra 0.3.8

This update brings many improvements to `autoplot()` methods. These
include enhanced capabilities for handling of normalized and scaled
spectral data, as well as cosmetic tweaks to labels. It also tracks
changes in packages 'ggrepel' (\>= 0.9.1), 'photobiology' (\>= 0.10.10)
and 'ggplot2' (\>= 3.3.3) and deprecation of functions in 'tidyr' (\>=
1.0.0). The updated code depends on the revised `normalize()` function
in 'photobiology' (\>= 0.10.10) and on 'ggrepel' (\>= 0.9.1).

-   Revise all `autoplot()` methods to support on-the-fly normalization
    internally using `photobiology::normalize()`.
-   Revise `autoplot()` methods for `source_spct`, `response_spct`,
    `filter_spct`, `reflector_spct`, `source_mspct`, `response_mspct`,
    `filter_mspct`, and `reflector_mspct` so that if the data are
    already normalized, by default the normalization is updated
    according to the arguments passed to `unit.out` or `plot.qty` and
    `range`. (Update is possible only for objects normalized with
    'photobiology' (\>= 0.10.7). For objects created with earlier
    versions, the old behaviour of plotting spectra *as is* remains
    unchanged.)
-   Revise `autoplot()` method for `object_mspct` to force use of facets
    when `plot.qty = "all"` instead of forcing plotting of transmittance
    only.
-   Revise `autoplot()` method for `object_mspct` to correctly handle
    normalization of collections of spectra, one spectrum at a time,
    correcting a bug.
-   Revise all `autoplot()` methods to generate shorter *y*-axis labels
    also improving reporting of scaling and normalization when needed.
-   Revise all `autoplot()` methods to fix bug leading to bad object
    name in plot title due to delayed evaluation.
-   Add `autoplot.cps_mspct()` and `autoplot.raw_mspct()` methods.
-   Add support for faceting to all `autoplot()` methods for collections
    of spectra.
-   Revise all `autoplot()` methods for collections of spectra adding
    support for `sum`, `prod`, `var`, `sd`, and `se` as summaries. *The
    support for these summaries depends on* `autoplot.generic_spct()`
    *which is still experimental and subject to change.*
-   Update `decoration()` to track changes in 'ggrepel' (\>= 0.9.1).
-   Update `decoration()` to use `position_nudge()` and
    `position_nudge_repel()` to displace labels instead of "off-range"
    justification.
-   Improve handling of multiple spectra in long form by `ggplot()`
    methods and by `autotitle()`.
-   Remove direct dependency on 'dplyr'.
-   Add to the documentation of all `autoplot()` methods an explanation
    of how to modify the default plot annotations and titles using
    arguments passed to parameter `annotations`. Also add *see also*
    links to related help pages.

------------------------------------------------------------------------

-   Although this update should not break any code, **the *y*-axis
    labels in plots returned by `autoplot()`** methods have changed, in
    most cases only cosmetically to shorten them. In a few cases more
    significant edits correct problems.
-   Although this update should not break any code, **the labels for
    peaks and valleys can be at a slightly different position than with
    earlier versions**.
-   Although this update does not break any code, plots of collections
    of object spectra with normalization, will differ from earlier ones
    as member spectra are now normalized individually (this corrects an
    earlier bug!).

------------------------------------------------------------------------

# ggspectra 0.3.7

-   Track and profit from updated 'photobiology' (\>= 0.10.4), with (\>=
    0.10.5) now required.
-   Use performance-optimized computation for colors from wavelengths.
-   Improve `stat_wl_strip()` performance by simplifying data before
    plotting.
-   Add support for plotting action spectra (preliminary).
-   Update *y*-axis scales to support scaled and normalized data.
-   Update *y*-scales to generate more compact axis labels and use
    IUPAC-recommended symbols.
-   Update *x*-axis- and *y*-axis scales to allow users to override the
    default label text.
-   Implement `scale_y_Afr_continuous()` for absorptance spectra.
-   Implement `scale_y_mult_continuous()` for calibration spectra.
-   Update `autoplot()` methods to generate more compact axis labels
    (preliminary).
-   Update `autoplot.source_spct()` method to to natively support facets
    (partial).
-   Update `stat_wb_box()` adding parameter box.height (default behavior
    unchanged).
-   Fix a documentation example that stopped working because of changes
    to 'ggplot2'.

------------------------------------------------------------------------

-   Although this update should not break any code, the **plots created
    will in some cases differ slighttly from those created with earlier
    versions**. These changes affect the formatting of axis labels, and
    are readily visible.
-   Colors may differ very slightly while `stat_wl_strip()` draws fewer
    rectangles (in most cases these changes are barely visible).

------------------------------------------------------------------------

# ggspectra 0.3.6

-   Add support for computation of colors using other chromaticity
    coordinates than the color matching function (CMF) for human vision.
-   Revise for compatibility with 'dplyr' (\>= 1.0.0).
-   Track changes in 'photobiology' 0.10.1.
-   Fix two bugs in `ggplot()` method for class object_spct.
-   Add `stat_spikes()`, useful for highlighting spikes.
-   Fitted peaks and valleys are now supported (experimental).
-   Rewrite `stat_find_wls()` using `photobiology::wls_at_target()`.
-   Support new annotations `"wls"` and `"wls.labels"` in all
    `autoplot()` methods.
-   Update decoration() for this, and also other small tweaks.

------------------------------------------------------------------------

Although this update should not break any code, **the plots created will
in many cases differ from those created with earlier versions. Changes
only affect the text of labels.**

------------------------------------------------------------------------

# ggspectra 0.3.5

-   Revise ggplot() methods so that the class and attributes of spectral
    objects are retained in the `"data"` member of the `"gg"` object.
    This allows use of methods specific to spectra in data
    transformations.
-   Track changes in 'photobiology' 0.9.30.

# ggspectra 0.3.4

-   Update for code-breaking changes in 'tidyr' (\>= 1.0.0).

# ggspectra 0.3.3-1

-   Add convenience functions to make easier to modify plotting options:
    `set_annotations_default()` and `set_w.band_default()`.
-   Add support for simultaneously adding and removing annotations from
    the current default by accepting lists of character vectors as
    argument for parameter annotations in `autoplot()` methods.
-   Add support in `multiplot()` for adding a title to the composite
    figure.
-   Add support for setting of the plot caption with autotitle() and as
    annotations in `autoplot()` methods. Make autotitle() a synonim of
    `ggtitle_spct()`, which is now deprecated.
-   Track changes in 'photobiology' (\>= 0.9.28) adding support fot
    `"how.measured"` attribute in plot titles, subtitles and captions.
-   Use a compute panel function instead of a compute group function in
    `stat_wb_label()`, `stat_wb_box()` and `stat_wl_strip()`, solving a
    bug affecting `autoplot()` methods when called with multiple
    spectra.

# ggspectra 0.3.2

-   Move 'ggplot2' from Imports: to Depends: as the objects returned
    belong to classes defined in this package.
-   Move 'photobiology' from Imports: to Depends: as the objects
    accepted as input belong to classes defined in this package and is
    natural for users to want to use specialized methods to operate on
    them.
-   Define `autoplot()` methods as replacements for `plot()` methods,
    which are now deprecated. All documentation now uses `autoplot()`.
-   As 'ggplot2' exports a generic for `autoplot()` and the expected
    returned value is a ggplot which matches the returned value of the
    methods defined here, we have renamed them to `autoplot()`. For
    backwards compatibility `plot()` methods remain available, but their
    use in new code is deprecated.

# ggspectra 0.3.1

-   Fix major bug in `plot()` methods for `source_spct` and
    `response_spct` classes, that triggered errors in all cases when
    `time.unit` was not `"second"`.
-   Add `scale_y_s.e.irrad_log10()` and `scale_y_s.q.irrad_log10()`.
-   Test with 'ggplot2' version 3.1.0.

# ggspectra 0.3.0

------------------------------------------------------------------------

**Change the names of some the values calculated by the stats defined in
'ggspectra' to avoid confusion with the names of 'ggplot2' aesthetics.
This is a code breaking change!**

------------------------------------------------------------------------

Revise to track changes in 'photobiology' version 0.9.24 and 'ggplot2'
3.0.0, which are now required.

-   `stat_label_peaks()` and `stat_label_valleys()` now have a new
    parameter, `label.fill` which can be used to set the content of
    `..x.label..` and `..y.label..` for labels not labeled as peaks or
    valleys. The earlier use of `""` is retained as default.
-   Add `stat_find_wls()` and `stat_find_qtys()`, two new statistics
    useful for highlighting and labeling features in spectra.
-   Add parameter `ylim` to all `plot()` methods.
-   Revise `plot()` methods to support objects with multiple spectra in
    long form.
-   Revise `plot()` methods to NOT display by default calculated
    summaries in annotations when a plot contains multiple spectra.
-   Revise `plot()` methods to retrieve and validate the name of the
    factor used to identify multiple spectra, using as default the value
    stored in the attribute `"idfactor"` implemented in 'photobiology'
    0.9.21 and later.
-   Add `autoplot()` as an alias of `plot()` for spectra and collections
    of spectra.

# ggspectra 0.2.4

Revise to track changes in 'photobiology' 0.9.20 to avoid spurious
messages.

-   Add `ggplot()` method specialization for collections of spectra.

-   Add `plot()` method specializations for collections of spectra.

# ggspectra 0.2.3

Track changes in package 'photobiology' 0.9.18.

-   Add `plot.calibration_spct().`

-   Add `ggplot.calibration_spct()`.

Improve handling of plot titles.

-   Add utility function `ggtitle_spct()`.

-   Update all `plot()` methods to allow flexible contents in automatic
    titles including setting of the time zone and format used for
    displaying time.

# ggspectra 0.2.2

Track changes in package 'photobiology' 0.9.16 and 0.9.17. This among
other improvements makes it easy to plot spectral absorptance. Expand
*y* scale to expected range in all cases, but do not force these limits
in any case when data exceeds them, except for absorbance for which the
y scale is not expanded past 6 a.u. Add new annotation `"boundaries"` to
highlight with horizontal line(s) the expected range of the plotted
quantity, highlighting the limit(s) exceeded if data fall outside the
expected/range. Add previously missing `ggplot()` method for class
`object_spct`. Revise vignettes.

# ggspectra 0.2.1

Track changes in package 'photobiology' 0.9.14. Add continuous scales
with defaults suitable for spectral data, with support for automatic
axis labels both as R-expressions and as LaTeX mark-up. Add utility
functions for SI scale prefixes. Add utility functions for
*x*-axis-labels and secondary *x*-axis. Add utility functions for
y-axis-labels. Update User Guide (add examples of graphical comparison
of multiple lists of wavebands).

# ggspectra 0.2.0

Improve handling of annotations parameter in `plot()` methods. Add
functions `ggcolorchart()` and `black_or_white()`. Add statistics
`stat_wb_box()` and `stat_wb_column()` with `"rect"` as default geom and
`stat_wb_hbar()` with `"errorbarh"` as default geom. Change default geom
from `"rect"` to `"text"` in all the summary stats for spectra. Update
stats with "text" as default geom to use an additional variable
`BW.color` computed by `black_or_white()` as default mapping for color.
As default geoms and color mapping have changed, this update can break
existing code. Changes to `plot()` methods are backwards compatible but
default color of some text labels is changed from `"white"` to `"black"`
to improve readability, and fewer peaks and valleys are highlighted by
default. In addition more ticks marks are used on the wavelength axis.
Fix minor bugs in handling of color mapping. Update documentation.

# ggspectra 0.1.12

Import 'ggplot2' in whole as only the most basic use of `plot()` methods
would not require user access to 'ggplot2'. Add `stat_label_peaks()` and
`stat_label_valleys()` which are designed to fully take advantage of
`geom_text_repel()` and `geom_label_repel()` as revised in package
'ggrepel' (\>= 0.6.3). Change behaviour of `plot()` methods so that
"illegal" values such as negative spectral irradiances or transmittances
are plotted by default. Fix several minor bugs affecting `plot()`
methods with certain non-default sets of annotations. Improve
documentation and vignettes.

# ggspectra 0.1.11

# ggspectra 0.1.10

Add code to maintain same stacking order under 'ggplot2' 1.1.0, 2.0.0,
2.1.0, and 2.1.9000 in `plot.object_spct()`.

# ggspectra 0.1.9

Add support for `na.rm` to all `plot()` methods setting default to
`TRUE`, and avoid triggering spurious warnings from internal code. Make
default for parameter `label.qty` in plot methods dependent on whether
data are expressed in absolute units or have been *rescaled* or
*normalized*. Move from the 'User Guide' the sections on `plot()`
methods creating a new vignette titled 'Plot Methods'.

# ggspectra 0.1.8

Add support for `plot.qty = "absorptance"` to `plot.filter_spct().` Add
support for `plot.qty` to `plot.object_spct()` with possible values:
`"all"` or `NULL` (previous behaviour, and current default),
`"transmittance"`, `"absorbance"`, `"absorptance"`, and `"reflectance"`.
The new options plot only one quantity at a time as a line instead of
the default area plot.

# ggspectra 0.1.7

Add `stat_wb_label()`, `stat_wb_e_irrad()`, `stat_wb_q_irrad()`,
`stat_wb_e_sirrad()`, and `stat_wb_q_irrad()`. Revise `plot.raw_spct()`
to handle multiple `"counts"` columns. Revise `plot.cps_spct()` to
handle multiple `"cps"` columns. Add `text.size` parameter to all
`plot()` methods. Can be used to control the size of the font used for
text decorations related to wavebands, peaks and valleys. Increase
slightly the default font size for plot decorations, and decrease the
number of *peaks* and *valleys* labelled by the `plot()` methods. Fix
existing bug revealed by dplyr update. Fix bug in `decoration()`
affecting `plot.object_spct()`. Fix other minor bugs.

# ggspectra 0.1.6

**(First CRAN release)**

Clean documentation and code for CRAN submission. Add examples of
plotting of waveband objects as these were removed from the vignette of
package 'photobiologyWavebands'.

# ggspectra 0.1.5

Fix bug in documentation.

# ggspectra 0.1.4

Improve documentation.

# ggspectra 0.1.3

Remove `wb_guide()`. Remove `wb_wb_summary()`. Add `stat_wb_mean()`,
`stat_wb_total()`, `stat_wb_contribution()`, `stat_wb_relative()`. Add
`stat_wb_irrad()`, `stat_wb_sirrad()`

Implement 100% of the old functionality of the `plot()` methods, but
without using any annotations. In other words, now all calculations work
as expected with grouping and panels. Some aesthetics may require
tweaking in the case of grouping.

Tested with packages 'ggrepel' and 'cowplot'.

# ggspectra 0.1.2

Add text explaining examples to User Guide.

**Rename several stats and functions with more descriptive names.**

`stat_color_guide()` -\> `stat_wl_strip()`

`stat_average()` -\> `stat_wl_summary()`

`stat_waveband()` -\> `stat_wb_summary()`

`color_guide()` -\> `wl_guide()`

`waveband_guide()` -\> `wb_guide()`

# ggspectra 0.1.1

Add `multiplot()` function (unchanged from package photobiologygg).
Improve `stat_waveband()`. Add `color_guide()`, `waveband_guide()`.

Add `ggplot()` methods for spectra, which use default mappings if
mapping not explicitly supplied as argument to call.

Make `plot()` methods almost fully functional. `"contribution"` and
`"participation"` summary values not yet implemented. Neither summaries
for BSWFs. If requested these summaries will show as `NaN` (not a
number) in the plots.

# ggspectra 0.1.0

First preview version. Still with incomplete functionality.
