import dateutil
from Py6S import *
import datetime
import os
import sys
from atmospheric import Atmospheric
import numpy as np
import ee
import math

ee.Authenticate()
ee.Initialize()


#Define parameters

TARGET_DATE = ee.Date('2020-07-31') #YYYY-MM-DD
START_DATE = TARGET_DATE.advance(-8, 'day')
END_DATE = TARGET_DATE.advance(8, 'day')
COMBINED = True #combine Terra and Aqua
MIN_SAMPLES = 7


#Combine Terra and Aqua and filter data within the 16-day window

collection = getData(START_DATE, END_DATE, COMBINED)


#Mask clouds

cloudsRemoved = collection.map(maskMODIS)


#Mask insufficient observations

numPixels = cloudsRemoved.count()

newMask = numPixels.select('sur_refl_b01').gt(MIN_SAMPLES)

masked = cloudsRemoved.map(maskFew)


#Calculate kernels for Nadir, Black-Sky, and White-Sky

import math

withKernels_bs = masked.map(kernels_bs)


#Calculate kernels with observed geometry

kernels_obs = masked.map(addKernels_obs)


#Retrieve BRDF parameters using least squares estimates

blue = kernels_obs.select(['sur_refl_b03','ones','Kvol','Ksparse']).toArray()
x_blue = blue.arraySlice(1, 1, 4)
y_blue = blue.arraySlice(1, 0, 1).divide(10000)
fit_blue = x_blue.matrixSolve(y_blue)

green = kernels_obs.select(['sur_refl_b04','ones','Kvol','Ksparse']).toArray()
x_green = green.arraySlice(1, 1, 4)
y_green = green.arraySlice(1, 0, 1).divide(10000)
fit_green = x_green.matrixSolve(y_green)

red = kernels_obs.select(['sur_refl_b01','ones','Kvol','Ksparse']).toArray()
x_red = red.arraySlice(1, 1, 4)
y_red = red.arraySlice(1, 0, 1).divide(10000)
fit_red = x_red.matrixSolve(y_red)

nir = kernels_obs.select(['sur_refl_b02','ones','Kvol','Ksparse']).toArray()
x_nir = green.arraySlice(1, 1, 4)
y_nir = green.arraySlice(1, 0, 1).divide(10000)
fit_nir = x_nir.matrixSolve(y_nir)

swir1 = kernels_obs.select(['sur_refl_b06','ones','Kvol','Ksparse']).toArray()
x_swir1 = blue.arraySlice(1, 1, 4)
y_swir1 = blue.arraySlice(1, 0, 1).divide(10000)
fit_swir1 = x_swir1.matrixSolve(y_swir1)

swir2 = kernels_obs.select(['sur_refl_b07','ones','Kvol','Ksparse']).toArray()
x_swir2 = blue.arraySlice(1, 1, 4)
y_swir2 = blue.arraySlice(1, 0, 1).divide(10000)
fit_swir2 = x_swir2.matrixSolve(y_swir2)


#Apply BRDF parameters for Nadir, Black-Sky, and White-Sky

corrected_images = withKernels_bs.map(correctNadir)


#Isolate target image for plotting

corrected_images_plot = corrected_images.filterDate(TARGET_DATE).first()


#Calculate AN_Ratio for Black- and White-Sky

corrected_nadir = corrected_images_plot.select(['blue_nadir', 'green_nadir', 'red_nadir', 'nir_nadir', 'swir1_nadir', 'swir2_nadir'])

corrected_bs = corrected_images_plot.select(['blue_bs', 'green_bs', 'red_bs', 'nir_bs', 'swir1_bs', 'swir2_bs'])

corrected_ws = corrected_images_plot.select(['blue_ws', 'green_ws', 'red_ws', 'nir_ws', 'swir1_ws', 'swir2_ws'])

nadir_constrained = corrected_nadir.updateMask(corrected_nadir.gte(0))

bs_constrained = corrected_bs.updateMask(corrected_bs.gte(0))

ws_constrained = corrected_ws.updateMask(corrected_ws.gte(0))

nadir_constrained = nadir_constrained.updateMask(nadir_constrained.lte(1.5))

bs_constrained = bs_constrained.updateMask(bs_constrained.lte(1.5))

ws_constrained = ws_constrained.updateMask(ws_constrained.lte(1.5))

AN_Ratio_bs = bs_constrained.divide(nadir_constrained)

AN_Ratio_ws = ws_constrained.divide(nadir_constrained)

