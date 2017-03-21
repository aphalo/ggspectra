sd_section("Package overview", "",
           c("ggspectra-package")
)

sd_section("Geoms",
  "Geoms, short for geometric objects, describe the type of plot you will produce.",
  c("geom_spct")
)

sd_section("Statistics",
  "It's often useful to summarize spectral data before plotting, and that's what these transformations do.",
  c("stat_color",
    "stat_peaks",
    "stat_label_peaks",
    "stat_wb_box",
    "stat_wb_hbar",
    "stat_wb_column",
    "stat_wb_contribution",
    "stat_wb_irrad",
    "stat_wb_label",
    "stat_wb_mean",
    "stat_wb_relative",
    "stat_wb_sirrad",
    "stat_wb_total",
    "stat_wl_strip",
    "stat_wl_summary")
)

sd_section("Scales",
           "Scales control the mapping between data and aesthetics.
           These scales are continuous scales with defaults suitable for spectral data.",
           c("scale_x_wl_continuous",
             "scale_y_s.e.response_continuous",
             "scale_y_s.e.irrad_continuous",
             "scale_y_A_internal_continuous",
             "scale_y_Tfr_internal_continuous",
             "scale_y_Rfr_specular_continuous",
             "scale_y_cps_continuous",
             "scale_y_counts_continuous",
             "sec_axis_w_number"
             )
)

sd_section("Plot creation", "Plot methods for spectra and a function for color-patch charts.",
  c("ggplot",
    "plot.raw_spct",
    "plot.cps_spct",
    "plot.source_spct",
    "plot.response_spct",
    "plot.filter_spct",
    "plot.reflector_spct",
    "plot.object_spct",
    "plot.waveband",
    "color_chart"
    )
)

sd_section("Helper functions", "Formatters, International System of units, unit conversions.",
           c("multiplot",
             "black_or_white",
             "exponent2prefix",
             "SI_pl_format",
             "SI_tg_format",
             "w_number",
             "A_internal_label",
             "counts_label",
             "cps_label",
             "Rfr_specular_label",
             "Tfr_internal_label",
             "s.e.irrad_label",
             "s.e.response_label",
             "w_length_label"
           )
)
