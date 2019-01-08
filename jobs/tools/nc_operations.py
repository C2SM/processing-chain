import numpy as np

from netCDF4 import Dataset

class DimensionCopier:
    """Copy netCDF-Dimensions"""
    def __init__(self, src_name, dst_name=None):
        self.src_name = src_name
        if dst_name is None:
            self.dst_name = src_name
        else:
            self.dst_name = dst_name

    def apply_to(self, src_dataset, dst_dataset):
        """Copy the specified dimension from src_dataset to dst_dataset"""
        src_dim = src_dataset.dimensions[self.src_name]
        dst_dataset.createDimension(dimname=self.dst_name, size=src_dim.size)

    def copy_variable(self, src_dataset, dst_dataset):
        """Copy the variable associated with the dimension"""
        copier = VariableCopier(src_names=[self.src_name],
                                dst_name=self.dst_name)
        copier.apply_to(src_dataset, dst_dataset)


class VariableCopier:
    """Copy (and possibly alter) netCDF-Variables"""
    def __init__(self, src_names, dst_name=None,
                 var_args={}, var_val_indices=np.s_[:], var_attrs={}):
        """
        Parameters
        ----------
        src_names : list(str)
            Names of the variables to be copied.
            If this is a string or a list of length 1, the corresponding
            netCDF-Variable will be copied into the new netCDF-file and
            named dst_name.
            If this is a list of strings, the variables will be added together
            and stored in the new netCDF-file named dst_name.
            Datatype and ncattrs will be copied from the first variable.
        dst_name : str
            Name of the variable in the destination file.
            If this is None, the name of the first variable in src_names will be
            used.
            Default: None.
        var_args: dict(str: str)
            Arguments for the netCDF4.Dataset.createVariable-function. Arguments
            given here overwrite the values obtained from the first variable in
            src_names. Useful for setting zlib, complevel, shuffle, fletcher32,
            contiguous, chunksizes and endian, as these can't be inferred from
            the source-variable.
            Default: dict()
        var_val_indices: numpy.lib.index_tricks.IndexExpression object
            Which values to copy from the source variables. By default, all
            values are copied.
            If only a subset of the values are copied, make sure the shape of
            the copied values matches the dimensions of the destination
            variable.
            Default: numpy.s_[:]
        var_attrs : dict(str: str)
            key-value pairs get turned into additional ncattrs for the var, on
            top of the ncattrs copied from the first variable in scr_names.
            Values given here overwrite values from the copied variable with
            the same attribute-name.
            Useful for adding or changing attributes, such as units.
            Default: Dict()
        """
        if isinstance(src_names, str):
            self.src_names = [src_names]
        else:
            self.src_names = src_names

        if dst_name is None:
            self.dst_name = self.src_names[0]
        else:
            self.dst_name = dst_name
        self.var_args = var_args.copy()
        self.var_val_indices = var_val_indices
        self.var_attrs = var_attrs.copy()

    def apply_to(self, src_dataset, dst_dataset):
        """Copy the specified variable(s) from src_dataset to dst_dataset.

        If there are multiple source variables, their values will be added.
        Raise an exception if their dimensions are different.

        The datatype, least_significant_digit and the attributes will be copied
        from the first element of the src_names list.

        Constructor arguments  zlib, complevel, shuffle, fletcher32, contiguous,
        chunksizes and endian can't (easily) be obtained from the variable
        in src_dataset. If they should be set, specify them with the var_args.
        """
        ref_var = src_dataset[self.src_names[0]]

        name = self.dst_name
        # Take datatype from ref_var and not from self.var_args, since we
        # need to check if we can accumulate values based on src-dtype, not
        # dest-dtype.
        dtype = ref_var.dtype
        try:
            dims_names = self.var_args['dimensions']
        except KeyError:
            dims_names = ref_var.dimensions
        digit = getattr(ref_var, 'least_significant_digit', None)

        # Test if variable datatype is Character, if not accumulate data
        if dtype == np.dtype('S1'):
            if len(self.src_names) > 1:
                # Characters can't be added together
                raise TypeError("Can't combine variables with {} as "
                                "datatype.".format(dtype))
            vals = src_dataset[self.src_names[0]][self.var_val_indices]
        else:
            vals = np.zeros_like(ref_var[self.var_val_indices])

            for var_name in self.src_names:
                if src_dataset[var_name].dimensions != ref_var.dimensions:
                    msg = "Different dimensions:\n{}: {},\n{}: {}.".format(
                        self.src_names[0], ref_var.dimensions,
                        var_name, src_dataset[var_name].dimensions)
                    raise ValueError(msg)

                vals += src_dataset[var_name][self.var_val_indices]

        # Get fill value
        ncattrs = ref_var.ncattrs()
        try:
            # Remove fill value from ncattrs because setting it after
            # construction is not possible
            ncattrs.remove('_FillValue')
            fill_value = ref_var.getncattr('_FillValue')
        except ValueError:
            fill_value = None

        var_args = dict(varname=name,
                        datatype=dtype,
                        dimensions=dims_names,
                        least_significant_digit=digit,
                        fill_value=fill_value)
        var_args.update(self.var_args)

        var_attrs = dict([(name, ref_var.getncattr(name)) for name in ncattrs])
        var_attrs.update(self.var_attrs)

        (VariableCreator(var_args=var_args, var_vals=vals, var_attrs=var_attrs)
         .apply_to(dst_dataset))


class VariableCreator:
    """Creates a netCDF4 variable with the specified parameters"""
    def __init__(self, var_args, var_vals, var_attrs={}):
        """

        Parameters
        ----------
        var_args : dict
            Gets unpacked as kwargs to netCDF4.Dataset.createVariable.
            Has to contain at least 'varname' & 'datatype'.
        var_vals : np.array
            Values to assign to the variable.
        var_attrs : dict
            key-value pairs get turned into ncattrs for the var with
            netCDF4.Variable.setncattr.
        """
        assert 'varname' in var_args and 'datatype' in var_args, (
            "varname and datatype are required arguments for "
            "variable creation.")

        self.varname = var_args['varname']
        self.var_args = var_args
        self.var_vals = var_vals
        self.var_attrs = var_attrs

    def apply_to(self, dst_dataset, _=None):
        """Create the variable with the parameters specified in the constructor
        in the netCDF4.Dataset dst_dataset

        For consistency with the other apply_to-functions (DimensionCopier,
        VariableCopier), this function takes two arguments.

        Parameters
        ----------
        dst_dataset : netCDF4.Dataset
            The variable is created in this dataset
        _
            Ignored, defaults to None
        """
        dst_dataset.createVariable(**self.var_args)
        dst_dataset[self.varname][:] = self.var_vals
        for attrname, attrval in self.var_attrs.items():
            dst_dataset[self.varname].setncattr(name=attrname, value=attrval)


def copy_nc_dataset(src_filename, dst_filename):
    with Dataset(src_filename) as src:
        copiers = []
        for dim in src.dimensions.keys():
            copiers.append(DimensionCopier(dim))

        for var in src.variables.keys():
            copiers.append(VariableCopier(src_names=var))

        with Dataset(dst_filename, 'w') as dst:
            for attr in src.ncattrs():
                dst.setncattr(attr, src.getncattr(attr))
            for copier in copiers:
                copier.apply_to(src, dst)


if __name__=='__main__':
    srcname = 'emis_2015062600.nc'
    dstname = 'dst2.nc'

    copy_nc_dataset(srcname, dstname)
