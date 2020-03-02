#This is a list of all the Export variables which should be activated for every EV file prior to export. So far as I can tell this process is case-insensitive once you actually pass it to the COM object, but I'm using the Echoview case here as a precaution
export.variable.list = c(
  #Analysis common
  "Date_M",
  'Dist_M',
  "Lat_E",
  "Lat_M",
  "Lat_S",
  'Lon_E',
  'Lon_M',
  'Lon_S',
  'Num_intervals',
  'Num_layers',
  'Ping_E',
  'Ping_S',
  #Analysis domain
  'Beam_volume_sum',
  'Depth_mean',
  'Exclude_above_line_range_mean',
  'First_layer_depth_start',
  'Height_mean',
  'Last_layer_depth_stop',
  'Layer_depth_min',
  'Layer_depth_max',
  'Range_mean',
  'Thickness_mean',
  'Wedge_volume_sampled',
  #Analysis export
  'EV_filename',
  #Biomass
  'Density_number',
  'Density_weight',
  #Integration results
  'NASC',
  'Sv_mean',
  'Standard_deviation',
  #Integration schools
  '3D_school_volume',
  'Attack_angle',
  'Corrected_length',
  'Corrected_thickness',
  'Uncorrected_area',
  'Uncorrected_length',
  'Uncorrected_thickness',
  #Integration settings
  'Minimum_Sv_threshold_applied',
  'Maximum_integration_threshold',
  'Maximum_Sv_threshold_applied',
  'Noise_Sv_1m',
  #Single targets
  'Num_targets',
  'Target_depth_max',
  'Target_depth_min',
  'Target_depth_mean',
  'Target_range_mean',
  'TS_mean'
)
