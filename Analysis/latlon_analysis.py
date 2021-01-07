import numpy as np
from netCDF4 import Dataset

latlonfilename = '/mnt/HA/groups/cappsGrp/cl3293/CMAQ_53_Beta/CMAQ-Drexel/CCTM/src/grid/latlon.1day'
latlonfile = Dataset(latlonfilename, mode='r', open=True)

gridLat =  np.squeeze(latlonfile.variables['LAT'][:][:][:][:])
gridLon =  np.squeeze(latlonfile.variables['LON'][:][:][:][:])

last1ind,last2ind = gridLon.shape

loncrn = [gridLon[0,0],gridLon[last1ind-1,last2ind-1]]
latcrn = [gridLat[0,0],gridLat[last1ind-1,last2ind-1]]
lonmid = gridLon[last1ind//2,last2ind//2]
latmid = gridLat[last1ind//2-1,last2ind//2-1]

print(gridLat.shape)
print(gridLon.shape)
print(last1ind)
print(last2ind)

print('\n')
print(loncrn)
print(latcrn)
print(lonmid)
print(latmid)