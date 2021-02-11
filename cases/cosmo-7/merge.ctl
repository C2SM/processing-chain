&RunSpecification
strict_nl_parsing  = .true.
verbosity          = "moderate"
diagnostic_length  = 110
soft_memory_limit  = 20.0
strict_usage       = .true. 
/

&GlobalResource
dictionary           = "/users/mjaehn/github/fieldextra/resources/dictionary_cosmo.txt"
grib_definition_path = "/users/mjaehn/github/fieldextra/resources/eccodes_definitions_cosmo",
                       "/users/mjaehn/github/fieldextra/resources/eccodes_definitions_vendor" 
/

&GlobalSettings
default_model_name = "cosmo"
/

&ModelSpecification
earth_axis_large   = 6371229.
earth_axis_small   = 6371229.
model_name         = "cosmo" 
/

&Process
  in_file  = "{in_file}"
  out_type = "INCORE" /
&Process in_field = "HSURF", tag="HSURF" /
&Process in_field = "FR_LAND", tag="fr_land" /
&Process in_field = "T_SO", levlist=0, tag="sst" /


# Here we define the fields that are merged from the interpolated IFS file
# this are external parameters and the SST
&Process
  in_file      = "{ifs_in_file}"
  out_file     = "{out_file}"
  out_type     = "GRIB1", in_regrid_target = "HSURF", in_size_field=1200 /
&Process in_field = "VIO3", levlist = -1, set_reference_date  = {laf_output_refdate} /
&Process in_field = "HMO3",   set_reference_date  = {laf_output_refdate} / 
&Process in_field = "PLCOV",  set_reference_date  = {laf_output_refdate} / 
&Process in_field = "LAI",    set_reference_date  = {laf_output_refdate} / 
&Process in_field = "ROOTDP", set_reference_date  = {laf_output_refdate} / 
&Process in_field = "FOR_D",  set_reference_date  = {laf_output_refdate} / 
&Process in_field = "FOR_E",  set_reference_date  = {laf_output_refdate} / 
# select IFS but use COSMO (sst) over land where fr_land > 0.5 or fr_lake > 0.5
&Process in_field = "T_SO",   levlist=0, merge_with="sst", merge_mask="fr_land>0.5",
                 set_auxiliary_metainfo="generatingProcessIdentifier=5", set_reference_date  = {laf_output_refdate} /

# Here we define the fields that are not taken from the COSMO file of the last cycle
&Process
  in_file  = "{in_file}"
  out_file = "{out_file}"
  out_type = "GRIB1", in_regrid_target = "HSURF", in_size_field=1200
  selection_mode = "EXCLUDE"/
&Process in_field = "VIO3"  /
&Process in_field = "HMO3"  /
&Process in_field = "PLCOV" /
&Process in_field = "LAI"   /
&Process in_field = "ROOTDP"/
&Process in_field = "T_SO"  , levlist = 0/
&Process in_field = "FOR_D" /
&Process in_field = "FOR_E" /

