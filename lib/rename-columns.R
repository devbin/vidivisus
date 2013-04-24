renameColumn = function(data, from=f, to=t)
{
    names(data)[names(data) == from] = c(to)
    data
}

renameWormdata = function(data)
{   
    data = renameColumn(data, "p_elim",         "elimination_probability")
    data = renameColumn(data, "inputfile",      "treatment_interval")
    data = renameColumn(data, "alpha",          "exposure")
    data = renameColumn(data, "rbr",            "precontrol")
    # data = renameColumn(data, "IVMset",         "IVMset")
    # data = renameColumn(data, "pastrounds",     "pastrounds")
    # data = renameColumn(data, "futurerounds",   "futurerounds")
    # data = renameColumn(data, "coverage",       "coverage")   
}