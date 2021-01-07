###!/home/cl3293/anaconda3/bin/python

### Script for plotting and evaluating the ACONC in CMAQ v5.3 beta copied from Dan's directory

### CHANGE above: specify local python installation

import matplotlib as mpl
mpl.use('Agg')
from netCDF4 import Dataset
import pandas as pd
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
from math import fsum, exp
from random import uniform
from matplotlib import cm
from mpl_toolkits.basemap import Basemap
import matplotlib.ticker
from scipy import stats

#236 variables, below are species related to NH3
list_variables = ['NH3', 'HNO3', 'HCL', 'ASO4J', 'ASO4I', 'ANH4J', 'ANH4I', 'ANO3J', 'ANO3I', 'ANAJ', 'ANAI', 'ACLJ', 'ACLI']

#loop over all the selected species
for variable in list_variables:

    ## Ingest the adjoint sensitivities w.r.t emissions
    # Specify number of days to be read
    days = 1

    # iterate over this period of time
    for dd in range(0, days):
        day = 1 + dd     
        # Ensure that the string for day will match the CMAQ file
        if (day < 10):
            # Add zero before 
            small = str('0')
            dStamp = small+str(day)
        else:
            dStamp = str(day)
        dStamp
    
        #CHANGE DIRECTORY:
        # For each day, open the adjoint sensitivities w.r.t emissions file
        ACONCbasename_dan = '/mnt/HA/groups/cappsGrp/users/dca54/CMAQ_53_Beta/CMAQ-Drexel/data/output_CCTM_v53sc_intel_SE52BENCH/CCTM_ACONC_v53sc_intel_SE52BENCH_201107'+dStamp+'.nc'
        ACONCbasename_el = '/mnt/HA/groups/cappsGrp/users/el662/CMAQ_53_Beta/CMAQ-Drexel/data/output_CCTM_v53el_intel_SE52BENCH/CCTM_ACONC_v53el_intel_SE52BENCH_201107'+dStamp+'.nc'
        ACONCbasefile_dan = Dataset(ACONCbasename_dan, mode='r', open=True)
        ACONCbasefile_el = Dataset(ACONCbasename_el, mode='r', open=True)
        
        #Create ACONChrbase for every species'
        ACONChrbase_dan = ACONCbasefile_dan.variables[variable][:][:][:][:]
        ACONChrbase_el = ACONCbasefile_el.variables[variable][:][:][:][:]

        # Combine the hourly sensitivities
        if dd == 0:
            ACONChrbase_dan = ACONChrbase_dan
            ACONChrbase_el = ACONChrbase_el
        else:
            ACONChrbase_dan  = np.concatenate((ACONChrbase_dan ,ACONChrbase_dan ),axis=0)
            ACONChrbase_el  = np.concatenate((ACONChrbase_el ,ACONChrbase_el ),axis=0)
    
        grdACONChrbase_dan = np.squeeze(ACONChrbase_dan)
        grdACONChrbase_el  = np.squeeze(ACONChrbase_el)

     
        ltime, lrow, lcol = (grdACONChrbase_dan.shape)
        print("grdACONChrbase_dan.shape", grdACONChrbase_dan.shape)
        print(ltime)

    # Get coordinates for map projection
    ### CHANGE: specify path
    latlonfilename = '/mnt/HA/groups/cappsGrp/cl3293/CMAQ_53_Beta/CMAQ-Drexel/CCTM/src/grid/latlon.1day'
    latlonfile = Dataset(latlonfilename, mode='r', open=True)

    gridLat =  np.squeeze(latlonfile.variables['LAT'][:][:][:][:])
    gridLon =  np.squeeze(latlonfile.variables['LON'][:][:][:][:])

    last1ind,last2ind = gridLon.shape

    loncrn = [gridLon[0,0],gridLon[last1ind-1,last2ind-1]]
    latcrn = [gridLat[0,0],gridLat[last1ind-1,last2ind-1]]
    lonmid = gridLon[last1ind//2,last2ind//2]
    latmid = gridLat[last1ind//2-1,last2ind//2-1]

    #Calc the diff
    ACONChrbase_diff = grdACONChrbase_dan - grdACONChrbase_el
    ACONChrbase_diff  = np.squeeze(ACONChrbase_diff)
    
    print("ACONChrbase_diff.shape", ACONChrbase_diff.shape)

    ### Plot
    #Set font size
    font_size = 14

    #set ticks format: 2 digits after decimal point, scientific notations
    class FormatScalarFormatter(matplotlib.ticker.ScalarFormatter):
        def __init__(self, fformat="%1.1f", offset=True, mathText=True):
            self.fformat = fformat
            matplotlib.ticker.ScalarFormatter.__init__(self,useOffset=offset,
                                                            useMathText=mathText)
        def _set_format(self, vmin, vmax):
            self.format = self.fformat
            if self._useMathText:
                self.format = '$%s$' % matplotlib.ticker._mathdefault(self.format)
    fmt = FormatScalarFormatter("%.2f")


    #ACONC scatter plot
    unit = 'ppmV'
    pdf_file = '/mnt/HA/groups/cappsGrp/users/el662/CMAQ_53_Beta/CMAQ-Drexel/plots/scatterPlot_ACONC_dan.vs.el_for_' + variable + '.pdf'
    with PdfPages(pdf_file) as pdf:
     
      nArray = np.reshape(grdACONChrbase_el,[-1,]) #x
      fArray = np.reshape(grdACONChrbase_dan,[-1,]) #y

        # Calculate Statistical Parameters
      slope, intercept, r_value, p_value, std_err = stats.linregress(nArray, fArray)
      plt.rcParams["font.size"] = 14
                
      cs = plt.scatter(nArray,fArray, cmap=cm.Spectral_r)

      rangemin = min(np.min(nArray),np.min(fArray))
      rangemax = max(np.max(nArray),np.max(fArray))
      v = [rangemin, rangemax, rangemin, rangemax]
      plt.axis(v)

      plt.plot([rangemin, rangemax], [rangemin, rangemax])
      ax = plt.gca()
      ax.ticklabel_format(style='sci', axis='x', scilimits=(0,0))
      ax.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
      ax.set_ylabel(r'ACONC dca54 (' + unit + ')')
      ax.set_xlabel(r'ACONC el662 (' + unit + ')')
      ax.minorticks_on()
      ax.yaxis.grid(True, which='major', color='black', linestyle='--', linewidth='0.4')
      ax.yaxis.grid(True, which='minor', color='gray', linestyle=':', linewidth='0.3',)
      ax.xaxis.grid(True, which='major', color='black', linestyle='--', linewidth='0.4')
      ax.xaxis.grid(True, which='minor', color='gray', linestyle=':', linewidth='0.3')

      ax.annotate('R$^2$ = {:0.2F}\nSlope = {:0.2F}\nIntercept = {:0.2E}\nBlue Line = 1:1'.format(r_value**2., slope, intercept), xy=(0.05, 0.7), fontsize=14, fontweight='bold', xycoords='axes fraction',   \
                          horizontalalignment='left', verticalalignment='bottom')
        
      plt.tight_layout()
        
      # add title
      plt.title( r'ACONC evaluation for'+variable)
  
          
      pdf.savefig()
      plt.close()
          
      plt.hist2d (nArray, fArray, bins =(10,10), cmap=plt.cm.BuPu)
      plt.colorbar()
      pdf.savefig()
      plt.close()
        
   #ACONC
    with PdfPages('/mnt/HA/groups/cappsGrp/users/el662/CMAQ_53_Beta/CMAQ-Drexel/plots/ACONC_dan.'+variable+'.pdf') as pdf:

        # Hour range is the timeframe in the ingested CMAQ values to be plotted and compared
        ## For example, only day 6 of the days ingested above (May 30, 2007)
        ## j is a separate index for the monitor dates and times
        j = 0 
        ###CHANGE range for that date
        for i in range(0,24):
            ###CHANGE date
            rng = pd.date_range('7/1/2011',periods=24,freq='H')
        
            # Put the monitoring data on top of the concentration from CMAQ
            # Establish basemap
            m = Basemap(projection='lcc', llcrnrlon=loncrn[0], llcrnrlat=latcrn[0],\
                        urcrnrlon=loncrn[1], urcrnrlat=latcrn[1], lon_0=lonmid,\
                        lat_0=latmid, resolution='l')
        
            # draw coastlines, state and country boundaries, edge of map.
            m.drawcoastlines()
            m.drawstates()
            m.drawcountries()
        
            # Try plotting hourly ozone from CMAQ
            ny = grdACONChrbase_dan.shape[1]; nx = grdACONChrbase_dan.shape[2]
            lons, lats = m.makegrid(nx, ny) # get lat/lons of ny by nx evenly space grid.
            x, y = m(lons, lats) # compute map proj coordinates.
        
            # draw filled contours.
            clevs = np.arange(np.min(grdACONChrbase_dan[0:24,:,:]),np.max(grdACONChrbase_dan[0:24,:,:])+(np.max(grdACONChrbase_dan[0:24,:,:])-np.min(grdACONChrbase_dan[0:24,:,:]))/100,(np.max(grdACONChrbase_dan[0:24,:,:])-np.min(grdACONChrbase_dan[0:24,:,:]))/100)
            print("max",np.max(grdACONChrbase_dan[0:24,:,:]))
            print("min",np.min(grdACONChrbase_dan[0:24,:,:]))
            # shift the hours by a fixed amount to the start of the CMAQ day of interest
            cs = m.contourf(x,y,grdACONChrbase_dan[i,:,:],clevs,cmap=cm.Spectral_r,vmin=np.min(grdACONChrbase_dan),vmax=np.max(grdACONChrbase_dan))
        
            # add colorbar.
            cbar = m.colorbar(cs,location='right',pad="0.01%")
            cbar.set_label('(ppm)')
            plt.tight_layout()
       
            # add title 
            #CHANGE TITLE
            plt.title('Hourly '+variable+' for '+str(rng[j]))
        
            # iterate over dates
            j = j+1
        
            #plt.show()
        
            pdf.savefig()
            plt.close()

    #ACONC_diff
    with PdfPages('/mnt/HA/groups/cappsGrp/users/el662/CMAQ_53_Beta/CMAQ-Drexel/plots/ACONC_'+variable+'.diff.dan.vs.el.pdf') as pdf:
        j = 0 
        ###CHANGE range for that date
        for i in range(0,24):
            ###CHANGE date
            rng = pd.date_range('7/1/2011',periods=24,freq='H')
    
            # Establish basemap
            m = Basemap(projection='lcc', llcrnrlon=loncrn[0], llcrnrlat=latcrn[0],\
                        urcrnrlon=loncrn[1], urcrnrlat=latcrn[1], lon_0=lonmid,\
                        lat_0=latmid, resolution='l')
    
            # draw coastlines, state and country boundaries, edge of map.
            m.drawcoastlines()
            m.drawstates()
            m.drawcountries()
    
            ny = ACONChrbase_diff.shape[1]; nx = ACONChrbase_diff.shape[2]
            lons, lats = m.makegrid(nx, ny) # get lat/lons of ny by nx evenly space grid.
            x, y = m(lons, lats) # compute map proj coordinates.
    
            # draw filled contours.
            upperbound = max(np.abs(np.nanmax(ACONChrbase_diff[i,:,:])), np.abs(np.nanmin(ACONChrbase_diff[i,:,:])))
            print("upperbound", upperbound)
            lowerbound = -upperbound
            clevs = np.arange(lowerbound,upperbound+(upperbound - lowerbound)/20,(upperbound - lowerbound)/20)                   
            cmap=cm.seismic

            # shift the hours by a fixed amount to the start of the CMAQ day of interest
            cs = m.contourf(x,y,ACONChrbase_diff[i,:,:],clevs,cmap=cmap,extend='both')        # add colorbar.
            cbar = m.colorbar(cs,location='right',pad="0.00%")
            #scientific notation
            cbar.formatter.set_powerlimits((0, 0))
            cbar.update_ticks()
            # change tick font size
            cbar.ax.tick_params(labelsize=font_size)
            # add label
            cbar.set_label('(ppm)', fontsize=font_size)

    
            # add title
            plt.title('ACONC '+variable+' dan - el')
            
            # iterate over dates
            j = j+1
   
            plt.tight_layout()
            pdf.savefig()
            plt.close()