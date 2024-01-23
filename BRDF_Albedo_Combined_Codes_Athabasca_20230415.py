#Code to generate Sentinel-2 albedo estimates with GEE Python API for Athabasca Basin

#Code to atmospherically correct Sentinel-2 images using 6S through Google Earth Engine
#From https://github.com/samsammurphy/gee-atmcorr-S2

#cd /media/project/abertoncini/02_GPM_MRR/23_Python_Envs/00_GEE

#module load gcc/9.3.0

#module load python/3.8

#virtualenv --no-download ENV

#source ENV/bin/activate

#pip install earthengine-api

#pip install gcloud

#pip install git+https://github.com/robintw/Py6S.git

#pip install python-dateutil

#pip install IPython

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


#Function to calculate kernels

def calcKernel(theta_i1, theta_v1, phi_i1, phi_v1):
  theta_i = ee.Image(theta_i1).multiply(math.pi/180)
  theta_v = ee.Image(theta_v1).multiply(math.pi/180)
  phi_i = ee.Image(phi_i1).multiply(math.pi/180)
  phi_v = ee.Image(phi_v1).multiply(math.pi/180)
  phi_r = phi_v.subtract(math.pi).subtract(phi_i)

  cos_xi = theta_i.cos().multiply(theta_v.cos()).add(theta_i.sin().multiply(theta_v.sin()).multiply(phi_r.cos()))

  xi = cos_xi.acos()

  Kvol = (((xi.multiply(-1).add(math.pi/2)).multiply(cos_xi).add(xi.sin())).divide(theta_i.cos().add(theta_v.cos()))).subtract(math.pi/4)

  h_b = 2
  b_r = 1

  theta_ip = (theta_i.tan().multiply(b_r)).atan()
  theta_vp = (theta_v.tan().multiply(b_r)).atan()

  D = (theta_ip.tan().pow(2).add(theta_vp.tan().pow(2)).subtract(theta_ip.tan().multiply(theta_vp.tan()).multiply(phi_r.cos()).multiply(2))).sqrt()

  cos_t1 = (D.pow(2).add((theta_ip.tan().multiply(theta_vp.tan()).multiply(phi_r.sin())).pow(2))).sqrt().multiply(h_b).divide(theta_ip.cos().pow(-1).add(theta_vp.cos().pow(-1)))

  cos_t2 = cos_t1.where(cos_t1.lt(-1),-1)
  cos_t = cos_t2.where(cos_t2.gt(1),1)

  t = cos_t.acos()

  O = (t.subtract(t.sin().multiply(cos_t))).multiply(theta_ip.cos().pow(-1).add(theta_vp.cos().pow(-1))).divide(math.pi)

  cos_xip = theta_ip.cos().multiply(theta_vp.cos()).add(theta_ip.sin().multiply(theta_vp.sin()).multiply(phi_r.cos()))

  Ksparse = O.subtract(theta_ip.cos().pow(-1)).subtract(theta_vp.cos().pow(-1)).add((cos_xip.add(1)).multiply(theta_v.cos().pow(-1)).multiply(0.5))

  out = Kvol.addBands(Ksparse).addBands(theta_ip).addBands(theta_vp).addBands(D).addBands(cos_t).addBands(t).addBands(O).addBands(cos_xip)

  return out.rename(['Kvol','Ksparse','theta_ip','theta_vp','D','cost_t','t','O','cos_xip'])


#Function to combine MODIS Terra and Aqua

def getData(start, end, combined):
  c = ee.ImageCollection('MODIS/006/MOD09GA')
  if combined == True:
    c = c.merge('MODIS/006/MYD09GA')

  return c.filterDate(start, end)


#Function to mask clouds

def maskMODIS(image):
  cloudInternalBitMask = 1 << 10
  shadowBitMask = 4
  adjacentBitMask = 1 << 13

  qa = image.select('state_1km')

  shadowMask = qa.bitwiseAnd(shadowBitMask).eq(0);
  adjacentMask = qa.bitwiseAnd(adjacentBitMask).eq(0);
  cloudInternalMask = qa.bitwiseAnd(cloudInternalBitMask).eq(0)

  return image.updateMask(cloudInternalMask)#.updateMask(shadowMask).updateMask(adjacentMask)


#Function to mask insufficient observations

def maskFew(image):

  return image.updateMask(newMask)


#Function to generate Nadir, Black-sky, and White-Sky kernels

def kernels_bs(image):

  #calculate zenith at solar noon
  date = ee.Date(image.get('system:time_start'))
  year = date.get('year')
  yearStart = ee.Date.fromYMD(year,1,1)
  days = date.difference(yearStart,'day').toDouble()

  d = ((((ee.Image(days).subtract(173)).multiply(0.98563*math.pi/180)).cos()).multiply(0.39795)).asin().rename('d')

  lat = image.pixelLonLat().select('latitude').multiply(math.pi/180)

  sn = (lat.subtract(d)).multiply(180/(math.pi)).rename('sn')


  #Integrate for Black-Sky kernels

  solar_list = ee.List([0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6])

  view_list = ee.List([0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6])

  azimuth_list = ee.List([0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,
                          2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4,4.1,4.2,4.3,4.4,4.5,4.6,4.7,
                          4.8,4.9,5,5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9,6,6.1,6.2,6.3])


  for j in range(64):

    for i in range(17):

      view_kernels = calcKernel(sn, ee.Image(i), ee.Image(j), ee.Image(0)).select(['Kvol','Ksparse']).rename(['Kvol_bs','Ksparse_bs'])

      view_Kvol = ee.Image(0).add(view_kernels.select('Kvol_bs'))

      view_Ksparse = ee.Image(0).add(view_kernels.select('Ksparse_bs'))

    azim_Kvol = ee.Image(0).add(view_Kvol)

    azim_Ksparse = ee.Image(0).add(view_Ksparse)

  azim_Kvol = azim_Kvol.multiply(0.1)

  azim_Ksparse = azim_Ksparse.multiply(0.1)


  #Integrate for White-Sky kernels

  for k in range(17):

    for j in range(64):

      for i in range(17):

        view_kernels = calcKernel(ee.Image(k), ee.Image(i), ee.Image(j), ee.Image(0)).select(['Kvol','Ksparse']).rename(['Kvol_ws','Ksparse_ws'])

        view_Kvol = ee.Image(0).add(view_kernels.select('Kvol_ws'))

        view_Ksparse = ee.Image(0).add(view_kernels.select('Ksparse_ws'))

      azim_Kvol_ws = ee.Image(0).add(view_Kvol)

      azim_Ksparse_ws = ee.Image(0).add(view_Ksparse)

    solar_Kvol_ws = ee.Image(0).add(azim_Kvol_ws)

    solar_Ksparse_ws = ee.Image(0).add(azim_Ksparse_ws)

  solar_Kvol_ws = solar_Kvol_ws.multiply(0.1)

  solar_Ksparse_ws = solar_Ksparse_ws.multiply(0.1)


  kernels_nadir = calcKernel(sn, ee.Image(0), ee.Image(0), ee.Image(0)).select(['Kvol','Ksparse']).rename(['Kvol_nadir','Ksparse_nadir'])

  return image.addBands(ee.Image(1).rename('ones')).addBands(azim_Kvol.rename('Kvol_bs')).addBands(azim_Ksparse.rename('Ksparse_bs')).addBands(solar_Kvol_ws.rename('Kvol_ws')).addBands(solar_Ksparse_ws.rename('Ksparse_ws')).addBands(kernels_nadir)


#Function to calculate kernels from observed geometry

def addKernels_obs(image):
  theta_i = image.select('SolarZenith').multiply(0.01)
  theta_v = image.select('SensorZenith').multiply(0.01)
  phi_i = image.select('SolarAzimuth').multiply(0.01)
  phi_v = image.select('SensorAzimuth').multiply(0.01)
  phi_r = phi_i.subtract(phi_v)

  #calculate kernels at acquired angles
  kernels = calcKernel(theta_i,theta_v, phi_i, phi_v).select(['Kvol','Ksparse'])

  ones = ee.Image(1).rename('ones')

  return image.addBands(ones).addBands(kernels)


#Function to correct surface reflectance to BRDF effects

def correctNadir(image):
  kernels_nadir = ee.ImageCollection(image.select(['ones','Kvol_nadir','Ksparse_nadir'])).toArray()

  kernels_bs = ee.ImageCollection(image.select(['ones','Kvol_bs','Ksparse_bs'])).toArray()

  kernels_ws = ee.ImageCollection(image.select(['ones','Kvol_ws','Ksparse_ws'])).toArray()

  reflectance_nadir = kernels_nadir.matrixMultiply(fit_blue_array).arrayGet([0,0]).rename(['blue_nadir']).addBands(kernels_nadir.matrixMultiply(fit_green_array).arrayGet([0,0]).rename(['green_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_red_array).arrayGet([0,0]).rename(['red_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_nir_array).arrayGet([0,0]).rename(['nir_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_swir1_array).arrayGet([0,0]).rename(['swir1_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_swir2_array).arrayGet([0,0]).rename(['swir2_nadir']))

  reflectance_bs = kernels_bs.matrixMultiply(fit_blue_array).arrayGet([0,0]).rename(['blue_bs']).addBands(kernels_bs.matrixMultiply(fit_green_array).arrayGet([0,0]).rename(['green_bs'])).addBands(kernels_bs.matrixMultiply(fit_red_array).arrayGet([0,0]).rename(['red_bs'])).addBands(kernels_bs.matrixMultiply(fit_nir_array).arrayGet([0,0]).rename(['nir_bs'])).addBands(kernels_bs.matrixMultiply(fit_swir1_array).arrayGet([0,0]).rename(['swir1_bs'])).addBands(kernels_bs.matrixMultiply(fit_swir2_array).arrayGet([0,0]).rename(['swir2_bs']))

  reflectance_ws = kernels_ws.matrixMultiply(fit_blue_array).arrayGet([0,0]).rename(['blue_ws']).addBands(kernels_ws.matrixMultiply(fit_green_array).arrayGet([0,0]).rename(['green_ws'])).addBands(kernels_ws.matrixMultiply(fit_red_array).arrayGet([0,0]).rename(['red_ws'])).addBands(kernels_ws.matrixMultiply(fit_nir_array).arrayGet([0,0]).rename(['nir_ws'])).addBands(kernels_ws.matrixMultiply(fit_swir1_array).arrayGet([0,0]).rename(['swir1_ws'])).addBands(kernels_ws.matrixMultiply(fit_swir2_array).arrayGet([0,0]).rename(['swir2_ws']))

  return image.addBands(reflectance_nadir).addBands(reflectance_bs).addBands(reflectance_ws)


#Function to extract dates of available Sentinel-2 images

from datetime import datetime

CLOUD_FILTER = 30

available_s2 = ee.ImageCollection('COPERNICUS/S2').filterBounds(ee.Geometry.Point(-117.258500, 52.186170)).filterDate('2017-07-01', '2021-10-01').filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', CLOUD_FILTER))

dates_ee = available_s2.aggregate_array('system:time_start')

dates_python = dates_ee.getInfo()

dates_np_array = np.array(dates_python)/1000

dates_int = dates_np_array.astype(int)


dates_python_ymd = []

dates_python_julian = []

years = []

for i in range(len(dates_int)):

  dates_python_ymd.append(datetime.fromtimestamp(dates_int[i]).strftime('%Y-%m-%d'))

  years.append(datetime.fromtimestamp(dates_int[i]).strftime('%Y'))

  date_str = datetime.fromtimestamp(dates_int[i])

  date_tuple = date_str.timetuple()

  dates_python_julian.append(date_tuple.tm_yday)


#Loop through all the available images

albedo_timeseries = []

albedo_total_timeseries = []

albedo_timeseries_st = []

sca_timeseries = []

hru_perc_timeseries = []

multipliers_export = []

athabasca_hrus = ee.FeatureCollection('projects/ee-andresbertoncini/assets/athabasca_hrus')

for l in range(len(dates_int)):

      #Define parameters

      TARGET_DATE = ee.Date(dates_python_ymd[l]) #YYYY-MM-DD
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


      #Correct MODIS surface reflectance to Sentinel-2 spectral response

      MCD12Q1 = ee.ImageCollection('MODIS/006/MCD12Q1')

      MCD12Q1_2017 = MCD12Q1.filter(ee.Filter.eq('system:index', MCD12Q1.aggregate_array('system:index').get(16))).first().select('LC_Type1')
      MCD12Q1_2018 = MCD12Q1.filter(ee.Filter.eq('system:index', MCD12Q1.aggregate_array('system:index').get(17))).first().select('LC_Type1')
      MCD12Q1_2019 = MCD12Q1.filter(ee.Filter.eq('system:index', MCD12Q1.aggregate_array('system:index').get(18))).first().select('LC_Type1')
      MCD12Q1_2020 = MCD12Q1.filter(ee.Filter.eq('system:index', MCD12Q1.aggregate_array('system:index').get(19))).first().select('LC_Type1')

      SBAF = ee.List([0.923, 0.983, 0.955, 0.971, 1.031, 1.060, 0.987, 1.023, 0.975, 0.975, 1.061, 0.849, 0.956, 1.012, 0.970, 0.976, 1.049, 0.939, 0.987, 1.021, 0.980, 0.968, 1.063, 0.866, 0.948, 1.018, 0.973, 0.992, 1.070, 0.693, 1.014, 1.024, 0.981, 0.981, 1.050, 0.865, 0.983, 1.023, 0.970, 0.975, 1.064, 0.824, 0.981, 1.018, 0.970, 0.967, 1.051, 0.941, 0.960, 1.010, 0.969, 0.977, 1.040, 0.988, 1.000, 1.020, 0.989, 0.959, 1.061, 0.945, 0.982, 1.012, 0.984, 0.960, 1.054, 0.972, 0.959, 1.018, 0.973, 0.981, 1.060, 0.823])

      if years[l] == '2017':

          MCD12Q1_selected = MCD12Q1_2017

      elif years[l] == '2018':

          MCD12Q1_selected = MCD12Q1_2018

      elif years[l] == '2019':

          MCD12Q1_selected = MCD12Q1_2019

      elif years[l] == '2020':

          MCD12Q1_selected = MCD12Q1_2020

      else:

          MCD12Q1_selected = MCD12Q1_2020



      def SpectralResponse(image):

        SBAF_blue = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(0), SBAF.get(6), SBAF.get(12), SBAF.get(18), SBAF.get(24), SBAF.get(30), SBAF.get(36), SBAF.get(42), SBAF.get(48), SBAF.get(54), SBAF.get(60), SBAF.get(66)]))
        SBAF_green = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(1), SBAF.get(7), SBAF.get(13), SBAF.get(19), SBAF.get(25), SBAF.get(31), SBAF.get(37), SBAF.get(43), SBAF.get(49), SBAF.get(55), SBAF.get(61), SBAF.get(67)]))
        SBAF_red = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(2), SBAF.get(8), SBAF.get(14), SBAF.get(20), SBAF.get(26), SBAF.get(32), SBAF.get(38), SBAF.get(44), SBAF.get(50), SBAF.get(56), SBAF.get(62), SBAF.get(68)]))
        SBAF_nir = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(3), SBAF.get(9), SBAF.get(15), SBAF.get(21), SBAF.get(27), SBAF.get(33), SBAF.get(39), SBAF.get(45), SBAF.get(51), SBAF.get(57), SBAF.get(63), SBAF.get(69)]))
        SBAF_swir1 = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(4), SBAF.get(10), SBAF.get(16), SBAF.get(22), SBAF.get(28), SBAF.get(34), SBAF.get(40), SBAF.get(46), SBAF.get(52), SBAF.get(58), SBAF.get(64), SBAF.get(70)]))
        SBAF_swir2 = MCD12Q1_selected.remap(ee.List([16, 1, 10, 4, 15, 17, 5, 6, 7, 8, 9, 11]), ee.List([SBAF.get(5), SBAF.get(11), SBAF.get(17), SBAF.get(23), SBAF.get(29), SBAF.get(35), SBAF.get(41), SBAF.get(47), SBAF.get(53), SBAF.get(59), SBAF.get(65), SBAF.get(71)]))

        return image.addBands(image.select('sur_refl_b03').multiply(SBAF_blue).rename('sur_refl_b03_1')).addBands(image.select('sur_refl_b04').multiply(SBAF_green).rename('sur_refl_b04_1')).addBands(image.select('sur_refl_b01').multiply(SBAF_red).rename('sur_refl_b01_1')).addBands(image.select('sur_refl_b02').multiply(SBAF_nir).rename('sur_refl_b02_1')).addBands(image.select('sur_refl_b06').multiply(SBAF_swir1).rename('sur_refl_b06_1')).addBands(image.select('sur_refl_b07').multiply(SBAF_swir2).rename('sur_refl_b07_1'))

      masked = masked.map(SpectralResponse)


      #Apply topographic correction to MODIS surface reflectance

      from IPython.core.display import Image
      import math

      SRTM = ee.Image('CGIAR/SRTM90_V4')

      modisProjection = masked.first().select('sur_refl_b01_1').projection()

      SRTM_MODIS = SRTM.reduceResolution(reducer = ee.Reducer.mean()).reproject(crs = modisProjection)

      slope = ee.Terrain.slope(SRTM_MODIS).multiply(math.pi/180)

      aspect = ee.Terrain.aspect(SRTM_MODIS).multiply(math.pi/180)

      def TopoCorrection(image):

        modis_theta_s_i = image.select('SolarZenith').multiply(0.01).multiply(math.pi/180)

        modis_phi_s = image.select('SolarAzimuth').multiply(0.01).multiply(math.pi/180)

        modis_phi_s_minus_aspect = modis_phi_s.subtract(aspect)

        local_cos = ((slope.cos()).multiply(modis_theta_s_i.cos())).add((slope.sin()).multiply(modis_theta_s_i.sin()).multiply(modis_phi_s_minus_aspect.cos()))

        cos_theta = modis_theta_s_i.cos()

        topo_multiplier = (cos_theta.divide(local_cos)).pow(0.5)

        obs_refl_01 = image.select('sur_refl_b01_1').multiply(0.0001)
        obs_refl_02 = image.select('sur_refl_b02_1').multiply(0.0001)
        obs_refl_03 = image.select('sur_refl_b03_1').multiply(0.0001)
        obs_refl_04 = image.select('sur_refl_b04_1').multiply(0.0001)
        obs_refl_06 = image.select('sur_refl_b06_1').multiply(0.0001)
        obs_refl_07 = image.select('sur_refl_b07_1').multiply(0.0001)

        ref_cor_01 = obs_refl_01.multiply(topo_multiplier)
        ref_cor_02 = obs_refl_02.multiply(topo_multiplier)
        ref_cor_03 = obs_refl_03.multiply(topo_multiplier)
        ref_cor_04 = obs_refl_04.multiply(topo_multiplier)
        ref_cor_06 = obs_refl_06.multiply(topo_multiplier)
        ref_cor_07 = obs_refl_07.multiply(topo_multiplier)

        return image.addBands(local_cos.rename('local_cos')).addBands(topo_multiplier.rename('topo_multiplier')).addBands(ref_cor_01.rename('sur_refl_b01_cor')).addBands(ref_cor_02.rename('sur_refl_b02_cor')).addBands(ref_cor_03.rename('sur_refl_b03_cor')).addBands(ref_cor_04.rename('sur_refl_b04_cor')).addBands(ref_cor_06.rename('sur_refl_b06_cor')).addBands(ref_cor_07.rename('sur_refl_b07_cor'))


      corrected_reflectance = masked.map(TopoCorrection)


      #Calculate kernels for Nadir, Black-Sky, and White-Sky

      withKernels_bs = corrected_reflectance.map(kernels_bs)


      #Calculate kernels with observed geometry

      kernels_obs = corrected_reflectance.map(addKernels_obs)


      #Generate weights for model inversion

      obs_cov = withKernels_bs.select('obscov_500m')

      weights_blue = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b03_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b03_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_blue_matrix = weights_blue.toArray().matrixToDiag()


      weights_green = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b04_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b04_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_green_matrix = weights_green.toArray().matrixToDiag()


      weights_red = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b01_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b01_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_red_matrix = weights_red.toArray().matrixToDiag()


      weights_nir = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b02_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b02_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_nir_matrix = weights_nir.toArray().matrixToDiag()


      weights_swir1 = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b06_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b06_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_swir1_matrix = weights_swir1.toArray().matrixToDiag()


      weights_swir2 = ee.ImageCollection.fromImages([obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(0))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(0))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(1))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(1))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(2))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(2))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(3))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(3))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(4))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(4))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(5))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(5))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(6))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(6))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(7))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(7))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(8))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(8))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(9))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(9))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(10))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(10))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(11))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(11))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(12))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(12))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(13))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(13))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(14))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(14))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(15))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(15))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(16))).first().divide(100).multiply(0.50).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(16))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(17))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(17))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(18))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(18))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(19))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(19))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(20))).first().divide(100).multiply(0.63).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(20))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(21))).first().divide(100).multiply(0.68).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(21))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(22))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(22))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(23))).first().divide(100).multiply(0.84).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(23))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(24))).first().divide(100).multiply(1.00).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(24))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(25))).first().divide(100).multiply(0.85).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(25))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(26))).first().divide(100).multiply(0.75).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(26))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(27))).first().divide(100).multiply(0.69).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(27))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(28))).first().divide(100).multiply(0.64).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(28))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(29))).first().divide(100).multiply(0.59).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(29))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(30))).first().divide(100).multiply(0.56).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(30))).first().mask()),
                                                   obs_cov.filter(ee.Filter.eq('system:index', obs_cov.aggregate_array('system:index').get(31))).first().divide(100).multiply(0.53).pow(-1).updateMask(kernels_obs.select('sur_refl_b07_cor').filter(ee.Filter.eq('system:index', kernels_obs.select('sur_refl_b07_cor').aggregate_array('system:index').get(31))).first().mask())])

      weights_swir2_matrix = weights_swir2.toArray().matrixToDiag()


      #Retrieve BRDF parameters using least squares estimates

      blue = kernels_obs.select(['sur_refl_b03_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_blue = blue.arraySlice(1, 1, 4)
      y_blue = blue.arraySlice(1, 0, 1)

      green = kernels_obs.select(['sur_refl_b04_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_green = green.arraySlice(1, 1, 4)
      y_green = green.arraySlice(1, 0, 1)

      red = kernels_obs.select(['sur_refl_b01_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_red = red.arraySlice(1, 1, 4)
      y_red = red.arraySlice(1, 0, 1)

      nir = kernels_obs.select(['sur_refl_b02_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_nir = nir.arraySlice(1, 1, 4)
      y_nir = nir.arraySlice(1, 0, 1)

      swir1 = kernels_obs.select(['sur_refl_b06_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_swir1 = swir1.arraySlice(1, 1, 4)
      y_swir1 = swir1.arraySlice(1, 0, 1)

      swir2 = kernels_obs.select(['sur_refl_b07_cor', 'ones', 'Kvol', 'Ksparse']).toArray()
      x_swir2 = swir2.arraySlice(1, 1, 4)
      y_swir2 = swir2.arraySlice(1, 0, 1)


      fit_blue = x_blue.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue).matrixInverse().matrixMultiply(x_blue.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green = x_green.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green).matrixInverse().matrixMultiply(x_green.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red = x_red.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red).matrixInverse().matrixMultiply(x_red.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir = x_nir.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir).matrixInverse().matrixMultiply(x_nir.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1 = x_swir1.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1).matrixInverse().matrixMultiply(x_swir1.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2 = x_swir2.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2).matrixInverse().matrixMultiply(x_swir2.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)


      #Generate alternative model inversions to substitute in case of negative least squares coefficients from inversion above

      x_blue_iso = x_blue.arraySlice(1, 0, 1)
      x_green_iso = x_green.arraySlice(1, 0, 1)
      x_red_iso = x_red.arraySlice(1, 0, 1)
      x_nir_iso = x_nir.arraySlice(1, 0, 1)
      x_swir1_iso = x_swir1.arraySlice(1, 0, 1)
      x_swir2_iso = x_swir2.arraySlice(1, 0, 1)

      fit_blue_iso = x_blue_iso.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_iso).matrixInverse().matrixMultiply(x_blue_iso.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_iso = x_green_iso.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_iso).matrixInverse().matrixMultiply(x_green_iso.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_iso = x_red_iso.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_iso).matrixInverse().matrixMultiply(x_red_iso.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_iso = x_nir_iso.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_iso).matrixInverse().matrixMultiply(x_nir_iso.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_iso = x_swir1_iso.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_iso).matrixInverse().matrixMultiply(x_swir1_iso.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_iso = x_swir2_iso.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_iso).matrixInverse().matrixMultiply(x_swir2_iso.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)

      x_blue_vol = x_blue.arraySlice(1, 1, 2)
      x_green_vol = x_green.arraySlice(1, 1, 2)
      x_red_vol = x_red.arraySlice(1, 1, 2)
      x_nir_vol = x_nir.arraySlice(1, 1, 2)
      x_swir1_vol = x_swir1.arraySlice(1, 1, 2)
      x_swir2_vol = x_swir2.arraySlice(1, 1, 2)

      fit_blue_vol = x_blue_vol.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_vol).matrixInverse().matrixMultiply(x_blue_vol.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_vol = x_green_vol.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_vol).matrixInverse().matrixMultiply(x_green_vol.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_vol = x_red_vol.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_vol).matrixInverse().matrixMultiply(x_red_vol.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_vol = x_nir_vol.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_vol).matrixInverse().matrixMultiply(x_nir_vol.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_vol = x_swir1_vol.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_vol).matrixInverse().matrixMultiply(x_swir1_vol.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_vol = x_swir2_vol.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_vol).matrixInverse().matrixMultiply(x_swir2_vol.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)

      x_blue_geo = x_blue.arraySlice(1, 2, 3)
      x_green_geo = x_green.arraySlice(1, 2, 3)
      x_red_geo = x_red.arraySlice(1, 2, 3)
      x_nir_geo = x_nir.arraySlice(1, 2, 3)
      x_swir1_geo = x_swir1.arraySlice(1, 2, 3)
      x_swir2_geo = x_swir2.arraySlice(1, 2, 3)

      fit_blue_geo = x_blue_geo.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_geo).matrixInverse().matrixMultiply(x_blue_geo.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_geo = x_green_geo.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_geo).matrixInverse().matrixMultiply(x_green_geo.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_geo = x_red_geo.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_geo).matrixInverse().matrixMultiply(x_red_geo.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_geo = x_nir_geo.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_geo).matrixInverse().matrixMultiply(x_nir_geo.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_geo = x_swir1_geo.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_geo).matrixInverse().matrixMultiply(x_swir1_geo.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_geo = x_swir2_geo.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_geo).matrixInverse().matrixMultiply(x_swir2_geo.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)

      x_blue_isovol = x_blue.arraySlice(1, 0, 2)
      x_green_isovol = x_green.arraySlice(1, 0, 2)
      x_red_isovol = x_red.arraySlice(1, 0, 2)
      x_nir_isovol = x_nir.arraySlice(1, 0, 2)
      x_swir1_isovol = x_swir1.arraySlice(1, 0, 2)
      x_swir2_isovol = x_swir2.arraySlice(1, 0, 2)

      fit_blue_isovol = x_blue_isovol.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_isovol).matrixInverse().matrixMultiply(x_blue_isovol.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_isovol = x_green_isovol.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_isovol).matrixInverse().matrixMultiply(x_green_isovol.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_isovol = x_red_isovol.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_isovol).matrixInverse().matrixMultiply(x_red_isovol.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_isovol = x_nir_isovol.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_isovol).matrixInverse().matrixMultiply(x_nir_isovol.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_isovol = x_swir1_isovol.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_isovol).matrixInverse().matrixMultiply(x_swir1_isovol.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_isovol = x_swir2_isovol.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_isovol).matrixInverse().matrixMultiply(x_swir2_isovol.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)

      x_blue_isogeo = x_blue.arraySlice(1, 0, 3, 2)
      x_green_isogeo = x_green.arraySlice(1, 0, 3, 2)
      x_red_isogeo = x_red.arraySlice(1, 0, 3, 2)
      x_nir_isogeo = x_nir.arraySlice(1, 0, 3, 2)
      x_swir1_isogeo = x_swir1.arraySlice(1, 0, 3, 2)
      x_swir2_isogeo = x_swir2.arraySlice(1, 0, 3, 2)

      fit_blue_isogeo = x_blue_isogeo.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_isogeo).matrixInverse().matrixMultiply(x_blue_isogeo.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_isogeo = x_green_isogeo.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_isogeo).matrixInverse().matrixMultiply(x_green_isogeo.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_isogeo = x_red_isogeo.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_isogeo).matrixInverse().matrixMultiply(x_red_isogeo.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_isogeo = x_nir_isogeo.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_isogeo).matrixInverse().matrixMultiply(x_nir_isogeo.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_isogeo = x_swir1_isogeo.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_isogeo).matrixInverse().matrixMultiply(x_swir1_isogeo.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_isogeo = x_swir2_isogeo.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_isogeo).matrixInverse().matrixMultiply(x_swir2_isogeo.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)

      x_blue_volgeo = x_blue.arraySlice(1, 1, 3)
      x_green_volgeo = x_green.arraySlice(1, 1, 3)
      x_red_volgeo = x_red.arraySlice(1, 1, 3)
      x_nir_volgeo = x_nir.arraySlice(1, 1, 3)
      x_swir1_volgeo = x_swir1.arraySlice(1, 1, 3)
      x_swir2_volgeo = x_swir2.arraySlice(1, 1, 3)

      fit_blue_volgeo = x_blue_volgeo.arrayTranspose().matrixMultiply(weights_blue_matrix).matrixMultiply(x_blue_volgeo).matrixInverse().matrixMultiply(x_blue_volgeo.arrayTranspose()).matrixMultiply(weights_blue_matrix).matrixMultiply(y_blue)
      fit_green_volgeo = x_green_volgeo.arrayTranspose().matrixMultiply(weights_green_matrix).matrixMultiply(x_green_volgeo).matrixInverse().matrixMultiply(x_green_volgeo.arrayTranspose()).matrixMultiply(weights_green_matrix).matrixMultiply(y_green)
      fit_red_volgeo = x_red_volgeo.arrayTranspose().matrixMultiply(weights_red_matrix).matrixMultiply(x_red_volgeo).matrixInverse().matrixMultiply(x_red_volgeo.arrayTranspose()).matrixMultiply(weights_red_matrix).matrixMultiply(y_red)
      fit_nir_volgeo = x_nir_volgeo.arrayTranspose().matrixMultiply(weights_nir_matrix).matrixMultiply(x_nir_volgeo).matrixInverse().matrixMultiply(x_nir_volgeo.arrayTranspose()).matrixMultiply(weights_nir_matrix).matrixMultiply(y_nir)
      fit_swir1_volgeo = x_swir1_volgeo.arrayTranspose().matrixMultiply(weights_swir1_matrix).matrixMultiply(x_swir1_volgeo).matrixInverse().matrixMultiply(x_swir1_volgeo.arrayTranspose()).matrixMultiply(weights_swir1_matrix).matrixMultiply(y_swir1)
      fit_swir2_volgeo = x_swir2_volgeo.arrayTranspose().matrixMultiply(weights_swir2_matrix).matrixMultiply(x_swir2_volgeo).matrixInverse().matrixMultiply(x_swir2_volgeo.arrayTranspose()).matrixMultiply(weights_swir2_matrix).matrixMultiply(y_swir2)


      fit_blue_flatten = fit_blue.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_blue_remapped = fit_blue_flatten.where(fit_blue_flatten.lt(0), 0)

      fit_green_flatten = fit_green.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_green_remapped = fit_green_flatten.where(fit_green_flatten.lt(0), 0)

      fit_red_flatten = fit_red.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_red_remapped = fit_red_flatten.where(fit_red_flatten.lt(0), 0)

      fit_nir_flatten = fit_nir.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_nir_remapped = fit_nir_flatten.where(fit_nir_flatten.lt(0), 0)

      fit_swir1_flatten = fit_swir1.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_swir1_remapped = fit_swir1_flatten.where(fit_swir1_flatten.lt(0), 0)

      fit_swir2_flatten = fit_swir2.arrayFlatten([['1_fiso', '2_fvol', '3_fgeo'], ['x']])
      fit_swir2_remapped = fit_swir2_flatten.where(fit_swir2_flatten.lt(0), 0)


      fit_blue_iso_complete = fit_blue_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_blue_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_blue_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_blue_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_blue_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_blue_isovol_complete = fit_blue_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_blue_isogeo_complete = fit_blue_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_blue_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_blue_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_blue_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))

      fit_green_iso_complete = fit_green_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_green_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_green_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_green_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_green_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_green_isovol_complete = fit_green_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_green_isogeo_complete = fit_green_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_green_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_green_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_green_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))

      fit_red_iso_complete = fit_red_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_red_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_red_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_red_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_red_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_red_isovol_complete = fit_red_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_red_isogeo_complete = fit_red_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_red_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_red_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_red_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))

      fit_nir_iso_complete = fit_nir_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_nir_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_nir_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_nir_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_nir_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_nir_isovol_complete = fit_nir_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_nir_isogeo_complete = fit_nir_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_nir_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_nir_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_nir_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))

      fit_swir1_iso_complete = fit_swir1_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir1_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_swir1_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir1_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_swir1_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_swir1_isovol_complete = fit_swir1_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir1_isogeo_complete = fit_swir1_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_swir1_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_swir1_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_swir1_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))

      fit_swir2_iso_complete = fit_swir2_iso.arrayFlatten([['1_fiso'], ['x']]).addBands(ee.Image(0).rename('2_fvol_x')).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir2_vol_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_swir2_vol.arrayFlatten([['2_fvol'], ['x']])).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir2_geo_complete = ee.Image(0).rename('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_swir2_geo.arrayFlatten([['3_fgeo'], ['x']]))
      fit_swir2_isovol_complete = fit_swir2_isovol.arrayFlatten([['1_fiso', '2_fvol'], ['x']]).addBands(ee.Image(0).rename('3_fgeo_x'))
      fit_swir2_isogeo_complete = fit_swir2_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('1_fiso_x').addBands(ee.Image(0).rename('2_fvol_x')).addBands(fit_swir2_isogeo.arrayFlatten([['1_fiso', '3_fgeo'], ['x']]).select('3_fgeo_x'))
      fit_swir2_volgeo_complete = ee.Image(0).rename('1_fiso_x').addBands(fit_swir2_volgeo.arrayFlatten([['2_fvol', '3_fgeo'], ['x']]))


      fit_blue_kernels_iso = fit_blue_remapped.select('1_fiso_x').where(fit_blue_remapped.select('1_fiso_x').neq(0), 1)
      fit_blue_kernels_vol = fit_blue_remapped.select('2_fvol_x').where(fit_blue_remapped.select('2_fvol_x').neq(0), 3)
      fit_blue_kernels_geo = fit_blue_remapped.select('3_fgeo_x').where(fit_blue_remapped.select('3_fgeo_x').neq(0), 5)
      fit_blue_kernels_sum = fit_blue_kernels_iso.addBands(fit_blue_kernels_vol).addBands(fit_blue_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_blue_final = fit_blue_remapped.where(fit_blue_kernels_sum.eq(1), fit_blue_iso_complete)
      fit_blue_final = fit_blue_final.where(fit_blue_kernels_sum.eq(3), fit_blue_vol_complete)
      fit_blue_final = fit_blue_final.where(fit_blue_kernels_sum.eq(5), fit_blue_geo_complete)
      fit_blue_final = fit_blue_final.where(fit_blue_kernels_sum.eq(4), fit_blue_isovol_complete)
      fit_blue_final = fit_blue_final.where(fit_blue_kernels_sum.eq(6), fit_blue_isogeo_complete)
      fit_blue_final = fit_blue_final.where(fit_blue_kernels_sum.eq(8), fit_blue_volgeo_complete)

      fit_green_kernels_iso = fit_green_remapped.select('1_fiso_x').where(fit_green_remapped.select('1_fiso_x').neq(0), 1)
      fit_green_kernels_vol = fit_green_remapped.select('2_fvol_x').where(fit_green_remapped.select('2_fvol_x').neq(0), 3)
      fit_green_kernels_geo = fit_green_remapped.select('3_fgeo_x').where(fit_green_remapped.select('3_fgeo_x').neq(0), 5)
      fit_green_kernels_sum = fit_green_kernels_iso.addBands(fit_green_kernels_vol).addBands(fit_green_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_green_final = fit_green_remapped.where(fit_green_kernels_sum.eq(1), fit_green_iso_complete)
      fit_green_final = fit_green_final.where(fit_green_kernels_sum.eq(3), fit_green_vol_complete)
      fit_green_final = fit_green_final.where(fit_green_kernels_sum.eq(5), fit_green_geo_complete)
      fit_green_final = fit_green_final.where(fit_green_kernels_sum.eq(4), fit_green_isovol_complete)
      fit_green_final = fit_green_final.where(fit_green_kernels_sum.eq(6), fit_green_isogeo_complete)
      fit_green_final = fit_green_final.where(fit_green_kernels_sum.eq(8), fit_green_volgeo_complete)

      fit_red_kernels_iso = fit_red_remapped.select('1_fiso_x').where(fit_red_remapped.select('1_fiso_x').neq(0), 1)
      fit_red_kernels_vol = fit_red_remapped.select('2_fvol_x').where(fit_red_remapped.select('2_fvol_x').neq(0), 3)
      fit_red_kernels_geo = fit_red_remapped.select('3_fgeo_x').where(fit_red_remapped.select('3_fgeo_x').neq(0), 5)
      fit_red_kernels_sum = fit_red_kernels_iso.addBands(fit_red_kernels_vol).addBands(fit_red_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_red_final = fit_red_remapped.where(fit_red_kernels_sum.eq(1), fit_red_iso_complete)
      fit_red_final = fit_red_final.where(fit_red_kernels_sum.eq(3), fit_red_vol_complete)
      fit_red_final = fit_red_final.where(fit_red_kernels_sum.eq(5), fit_red_geo_complete)
      fit_red_final = fit_red_final.where(fit_red_kernels_sum.eq(4), fit_red_isovol_complete)
      fit_red_final = fit_red_final.where(fit_red_kernels_sum.eq(6), fit_red_isogeo_complete)
      fit_red_final = fit_red_final.where(fit_red_kernels_sum.eq(8), fit_red_volgeo_complete)

      fit_nir_kernels_iso = fit_nir_remapped.select('1_fiso_x').where(fit_nir_remapped.select('1_fiso_x').neq(0), 1)
      fit_nir_kernels_vol = fit_nir_remapped.select('2_fvol_x').where(fit_nir_remapped.select('2_fvol_x').neq(0), 3)
      fit_nir_kernels_geo = fit_nir_remapped.select('3_fgeo_x').where(fit_nir_remapped.select('3_fgeo_x').neq(0), 5)
      fit_nir_kernels_sum = fit_nir_kernels_iso.addBands(fit_nir_kernels_vol).addBands(fit_nir_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_nir_final = fit_nir_remapped.where(fit_nir_kernels_sum.eq(1), fit_nir_iso_complete)
      fit_nir_final = fit_nir_final.where(fit_nir_kernels_sum.eq(3), fit_nir_vol_complete)
      fit_nir_final = fit_nir_final.where(fit_nir_kernels_sum.eq(5), fit_nir_geo_complete)
      fit_nir_final = fit_nir_final.where(fit_nir_kernels_sum.eq(4), fit_nir_isovol_complete)
      fit_nir_final = fit_nir_final.where(fit_nir_kernels_sum.eq(6), fit_nir_isogeo_complete)
      fit_nir_final = fit_nir_final.where(fit_nir_kernels_sum.eq(8), fit_nir_volgeo_complete)

      fit_swir1_kernels_iso = fit_swir1_remapped.select('1_fiso_x').where(fit_swir1_remapped.select('1_fiso_x').neq(0), 1)
      fit_swir1_kernels_vol = fit_swir1_remapped.select('2_fvol_x').where(fit_swir1_remapped.select('2_fvol_x').neq(0), 3)
      fit_swir1_kernels_geo = fit_swir1_remapped.select('3_fgeo_x').where(fit_swir1_remapped.select('3_fgeo_x').neq(0), 5)
      fit_swir1_kernels_sum = fit_swir1_kernels_iso.addBands(fit_swir1_kernels_vol).addBands(fit_swir1_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_swir1_final = fit_swir1_remapped.where(fit_swir1_kernels_sum.eq(1), fit_swir1_iso_complete)
      fit_swir1_final = fit_swir1_final.where(fit_swir1_kernels_sum.eq(3), fit_swir1_vol_complete)
      fit_swir1_final = fit_swir1_final.where(fit_swir1_kernels_sum.eq(5), fit_swir1_geo_complete)
      fit_swir1_final = fit_swir1_final.where(fit_swir1_kernels_sum.eq(4), fit_swir1_isovol_complete)
      fit_swir1_final = fit_swir1_final.where(fit_swir1_kernels_sum.eq(6), fit_swir1_isogeo_complete)
      fit_swir1_final = fit_swir1_final.where(fit_swir1_kernels_sum.eq(8), fit_swir1_volgeo_complete)

      fit_swir2_kernels_iso = fit_swir2_remapped.select('1_fiso_x').where(fit_swir2_remapped.select('1_fiso_x').neq(0), 1)
      fit_swir2_kernels_vol = fit_swir2_remapped.select('2_fvol_x').where(fit_swir2_remapped.select('2_fvol_x').neq(0), 3)
      fit_swir2_kernels_geo = fit_swir2_remapped.select('3_fgeo_x').where(fit_swir2_remapped.select('3_fgeo_x').neq(0), 5)
      fit_swir2_kernels_sum = fit_swir2_kernels_iso.addBands(fit_swir2_kernels_vol).addBands(fit_swir2_kernels_geo).reduce(reducer = ee.Reducer.sum())
      fit_swir2_final = fit_swir2_remapped.where(fit_swir2_kernels_sum.eq(1), fit_swir2_iso_complete)
      fit_swir2_final = fit_swir2_final.where(fit_swir2_kernels_sum.eq(3), fit_swir2_vol_complete)
      fit_swir2_final = fit_swir2_final.where(fit_swir2_kernels_sum.eq(5), fit_swir2_geo_complete)
      fit_swir2_final = fit_swir2_final.where(fit_swir2_kernels_sum.eq(4), fit_swir2_isovol_complete)
      fit_swir2_final = fit_swir2_final.where(fit_swir2_kernels_sum.eq(6), fit_swir2_isogeo_complete)
      fit_swir2_final = fit_swir2_final.where(fit_swir2_kernels_sum.eq(8), fit_swir2_volgeo_complete)


      fit_blue_array = ee.ImageCollection(fit_blue_final).toArray().matrixTranspose()
      fit_green_array = ee.ImageCollection(fit_green_final).toArray().matrixTranspose()
      fit_red_array = ee.ImageCollection(fit_red_final).toArray().matrixTranspose()
      fit_nir_array = ee.ImageCollection(fit_nir_final).toArray().matrixTranspose()
      fit_swir1_array = ee.ImageCollection(fit_swir1_final).toArray().matrixTranspose()
      fit_swir2_array = ee.ImageCollection(fit_swir2_final).toArray().matrixTranspose()



      #Apply BRDF parameters for Nadir, Black-Sky, and White-Sky

      corrected_images = withKernels_bs.map(correctNadir)


      #Isolate target image for plotting

      corrected_images_plot = corrected_images.filterDate(TARGET_DATE).mean()


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


      #Code to correct S2 images with 6S


      # Set date and AOI
      date = TARGET_DATE
      geom = ee.Geometry.Point(-117.258500, 52.186170)

      # The first Sentinel 2 image
      S2 = ee.Image(
        ee.ImageCollection('COPERNICUS/S2')
          .filterBounds(geom)
          .filterDate(date,date.advance(1,'day'))
          .sort('system:time_start')
          .first()
        )

      # top of atmosphere reflectance
      toa = S2.divide(10000)

      # get metadata info
      info = S2.getInfo()['properties']
      scene_date = datetime.utcfromtimestamp(info['system:time_start']/1000)# i.e. Python uses seconds, EE uses milliseconds
      solar_z = info['MEAN_SOLAR_ZENITH_ANGLE']

      # retrieve atmospheric parameters
      h2o = Atmospheric.water(geom,date).getInfo()
      o3 = Atmospheric.ozone(geom,date).getInfo()
      aot = Atmospheric.aerosol(geom,date).getInfo()

      # get target altitude
      SRTM = ee.Image('CGIAR/SRTM90_V4')# Shuttle Radar Topography mission covers *most* of the Earth
      alt = SRTM.reduceRegion(reducer = ee.Reducer.mean(),geometry = geom.centroid()).get('elevation').getInfo()
      km = alt/1000 # i.e. Py6S uses units of kilometers

      # Instantiate
      s = SixS("/home/abertoncini/source/6SV1.1/sixsV1.1")

      # Atmospheric constituents
      s.atmos_profile = AtmosProfile.UserWaterAndOzone(h2o,o3)
      s.aero_profile = AeroProfile.Continental
      s.aot550 = aot

      # Earth-Sun-satellite geometry
      s.geometry = Geometry.User()
      s.geometry.view_z = 0               # always NADIR (I think..)
      s.geometry.solar_z = solar_z        # solar zenith angle
      s.geometry.month = scene_date.month # month and day used for Earth-Sun distance
      s.geometry.day = scene_date.day     # month and day used for Earth-Sun distance
      s.altitudes.set_sensor_satellite_level()
      s.altitudes.set_target_custom_altitude(km)

      #Function for Spectral response functions

      def spectralResponseFunction(bandname):
          """
          Extract spectral response function for given band name
          """
          bandSelect = {
              'B1':PredefinedWavelengths.S2A_MSI_01,
              'B2':PredefinedWavelengths.S2A_MSI_02,
              'B3':PredefinedWavelengths.S2A_MSI_03,
              'B4':PredefinedWavelengths.S2A_MSI_04,
              'B5':PredefinedWavelengths.S2A_MSI_05,
              'B6':PredefinedWavelengths.S2A_MSI_06,
              'B7':PredefinedWavelengths.S2A_MSI_07,
              'B8':PredefinedWavelengths.S2A_MSI_08,
              'B8A':PredefinedWavelengths.S2A_MSI_8A,
              'B9':PredefinedWavelengths.S2A_MSI_09,
              'B10':PredefinedWavelengths.S2A_MSI_10,
              'B11':PredefinedWavelengths.S2A_MSI_11,
              'B12':PredefinedWavelengths.S2A_MSI_12,
              }
          return Wavelength(bandSelect[bandname])

      #Function to Convert TOA to Radiance

      def toa_to_rad(bandname):
          """
          Converts top of atmosphere reflectance to at-sensor radiance
          """

          # solar exoatmospheric spectral irradiance
          ESUN = info['SOLAR_IRRADIANCE_'+bandname]
          solar_angle_correction = math.cos(math.radians(solar_z))

          # Earth-Sun distance (from day of year)
          doy = scene_date.timetuple().tm_yday
          d = 1 - 0.01672 * math.cos(0.9856 * (doy-4))# http://physics.stackexchange.com/questions/177949/earth-sun-distance-on-a-given-day-of-the-year

          # conversion factor
          multiplier = ESUN*solar_angle_correction/(math.pi*d**2)

          # at-sensor radiance
          rad = toa.select(bandname).multiply(multiplier)

          return rad

      #Function for atmospheric correction

      def surface_reflectance(bandname):
          """
          Calculate surface reflectance from at-sensor radiance given waveband name
          """

          # run 6S for this waveband
          s.wavelength = spectralResponseFunction(bandname)
          s.run()

          # extract 6S outputs
          Edir = s.outputs.direct_solar_irradiance             #direct solar irradiance
          Edif = s.outputs.diffuse_solar_irradiance            #diffuse solar irradiance
          Lp   = s.outputs.atmospheric_intrinsic_radiance      #path radiance
          absorb  = s.outputs.trans['global_gas'].upward       #absorption transmissivity
          scatter = s.outputs.trans['total_scattering'].upward #scattering transmissivity
          tau2 = absorb*scatter                                #total transmissivity

          # radiance to surface reflectance
          rad = toa_to_rad(bandname)
          ref = rad.subtract(Lp).multiply(math.pi).divide(tau2*(Edir+Edif))

          return ref

      #Perform atmospheric correction

      # surface reflectance rgb
      b = surface_reflectance('B2')
      g = surface_reflectance('B3')
      r = surface_reflectance('B4')
      n = surface_reflectance('B8A')
      s1 = surface_reflectance('B11')
      s2 = surface_reflectance('B12')
      ref = r.addBands(g).addBands(b).addBands(r).addBands(n).addBands(s1).addBands(s2)
      
      
      #Export surface reflectance
      
      #filename = 'Athabasca_SR_'+dates_python_ymd[l]

      #region = geom.buffer(5000).bounds().getInfo()['coordinates']

      #ee.batch.Export.image.toDrive(image = ref, 
      #               description = 'image_export',
      #               folder ='Athabasca_Export_Images',
      #               fileNamePrefix = filename,
      #               region = region,
      #               scale = 20).start()


      #Apply cloud/shadow mask to corrected reflectances

      def get_s2_sr_cld_col(aoi, start_date, end_date):
          # Import and filter S2 SR.
          s2_sr_col = (ee.ImageCollection('COPERNICUS/S2')
              .filterBounds(aoi)
              .filterDate(start_date, end_date)
              .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', CLOUD_FILTER)))

          # Import and filter s2cloudless.
          s2_cloudless_col = (ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
              .filterBounds(aoi)
              .filterDate(start_date, end_date))

          # Join the filtered s2cloudless collection to the SR collection by the 'system:index' property.
          return ee.ImageCollection(ee.Join.saveFirst('s2cloudless').apply(**{
              'primary': s2_sr_col,
              'secondary': s2_cloudless_col,
              'condition': ee.Filter.equals(**{
                  'leftField': 'system:index',
                  'rightField': 'system:index'
              })
          }))

      def add_cloud_bands(img):
          # Get s2cloudless image, subset the probability band.
          cld_prb = ee.Image(img.get('s2cloudless')).select('probability')

          # Condition s2cloudless by the probability threshold value.
          is_cloud = cld_prb.gt(CLD_PRB_THRESH).rename('clouds')

          # Add the cloud probability layer and cloud mask as image bands.
          return img.addBands(ee.Image([cld_prb, is_cloud]))

      def add_shadow_bands(img):
          # Identify water pixels from the SCL band.
          not_water = img.select('B8A').gte(1000) #this line was changed from the original

          # Identify dark NIR pixels that are not water (potential cloud shadow pixels).
          SR_BAND_SCALE = 1e4
          dark_pixels = img.select('B8').lt(NIR_DRK_THRESH*SR_BAND_SCALE).multiply(not_water).rename('dark_pixels')

          # Determine the direction to project cloud shadow from clouds (assumes UTM projection).
          shadow_azimuth = ee.Number(90).subtract(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE')));

          # Project shadows from clouds for the distance specified by the CLD_PRJ_DIST input.
          cld_proj = (img.select('clouds').directionalDistanceTransform(shadow_azimuth, CLD_PRJ_DIST*10)
              .reproject(**{'crs': img.select(0).projection(), 'scale': 100})
              .select('distance')
              .mask()
              .rename('cloud_transform'))

          # Identify the intersection of dark pixels with cloud shadow projection.
          shadows = cld_proj.multiply(dark_pixels).rename('shadows')

          # Add dark pixels, cloud projection, and identified shadows as image bands.
          return img.addBands(ee.Image([dark_pixels, cld_proj, shadows]))

      def add_cld_shdw_mask(img):
          # Add cloud component bands.
          img_cloud = add_cloud_bands(img)

          # Add cloud shadow component bands.
          img_cloud_shadow = add_shadow_bands(img_cloud)

          # Combine cloud and shadow mask, set cloud and shadow as value 1, else 0.
          is_cld_shdw = img_cloud_shadow.select('clouds').add(img_cloud_shadow.select('shadows')).gt(0)

          # Remove small cloud-shadow patches and dilate remaining pixels by BUFFER input.
          # 20 m scale is for speed, and assumes clouds don't require 10 m precision.
          is_cld_shdw = (is_cld_shdw.focalMin(2).focalMax(BUFFER*2/20)
              .reproject(**{'crs': img.select([0]).projection(), 'scale': 20})
              .rename('cloudmask'))

          # Add the final cloud-shadow mask to the image.
          return img_cloud_shadow.addBands(is_cld_shdw)

      def apply_cld_shdw_mask(img):
          # Subset the cloudmask band and invert it so clouds/shadow are 0, else 1.
          not_cld_shdw = img.select('cloudmask').Not()

          # Subset reflectance bands and update their masks, return the result.
          return img.select('B.*').updateMask(not_cld_shdw)


      AOI = ee.Geometry.Point(-117.258500, 52.186170)
      date = TARGET_DATE
      CLOUD_FILTER = 30
      CLD_PRB_THRESH = 40
      NIR_DRK_THRESH = 0.15
      CLD_PRJ_DIST = 2
      BUFFER = 100

      s2_sr_cld_col = get_s2_sr_cld_col(AOI, date, date.advance(1,'day'))

      toa = (s2_sr_cld_col.map(add_cld_shdw_mask).map(apply_cld_shdw_mask).first()).divide(10000)

      cloudless_mask = toa.select('B8A').mask()

      ref_masked = ref.updateMask(cloudless_mask)


      #Apply topographic correction to S2 corrected reflectance

      sentinelProjection = toa.select('B12').projection()

      SRTM_Sentinel = SRTM.resample('bilinear').reproject(crs = sentinelProjection, scale = 20)

      slope = ee.Terrain.slope(SRTM_Sentinel).multiply(math.pi/180)

      aspect = ee.Terrain.aspect(SRTM_Sentinel).multiply(math.pi/180)

      info = S2.getInfo()['properties']

      modis_theta_s_i = info['MEAN_SOLAR_ZENITH_ANGLE']*(math.pi/180)

      modis_phi_s = info['MEAN_SOLAR_AZIMUTH_ANGLE']*(math.pi/180)

      modis_phi_s_minus_aspect = aspect.multiply(-1).add(modis_phi_s)

      local_cos = ((slope.cos()).multiply(math.cos(modis_theta_s_i))).add((slope.sin()).multiply(math.sin(modis_theta_s_i)).multiply(modis_phi_s_minus_aspect.cos()))

      cos_theta = math.cos(modis_theta_s_i)

      topo_multiplier = (local_cos.pow(-1).multiply(cos_theta)).pow(0.5)

      ref_cor_01 = ref.select('B2').multiply(topo_multiplier)
      ref_cor_02 = ref.select('B3').multiply(topo_multiplier)
      ref_cor_03 = ref.select('B4').multiply(topo_multiplier)
      ref_cor_04 = ref.select('B8A').multiply(topo_multiplier)
      ref_cor_06 = ref.select('B11').multiply(topo_multiplier)
      ref_cor_07 = ref.select('B12').multiply(topo_multiplier)

      S2_corrected = ref_cor_01.addBands(ref_cor_02).addBands(ref_cor_03).addBands(ref_cor_04).addBands(ref_cor_06).addBands(ref_cor_07)


      shadow_mask = ee.Terrain.hillShadow(SRTM_Sentinel, info['MEAN_SOLAR_AZIMUTH_ANGLE'], info['MEAN_SOLAR_ZENITH_ANGLE'])

      shadow_mask_filtered = shadow_mask.focalMode(radius = 5, kernelType = 'circle', iterations = 1).reproject(crs = sentinelProjection, scale = 20)

      S2_corrected = S2_corrected.updateMask(shadow_mask_filtered.eq(1))


      #Perform K-means classification

      #Make the training dataset
      training = S2_corrected.sample(region = geom.buffer(5000), scale = 20, numPixels = 5000)

      #Instantiate the clusterer and train it
      clusterer = ee.Clusterer.wekaKMeans(6).train(training)

      #Cluster the input using the trained clusterer
      result = S2_corrected.cluster(clusterer)
      
      
      #Export surface reflectance
      
      #filename = 'Athabasca_Cluster_'+dates_python_ymd[l]

      #region = geom.buffer(5000).bounds().getInfo()['coordinates']

      #ee.batch.Export.image.toDrive(image = result, 
      #               description = 'image_export',
      #               folder ='Athabasca_Export_Images',
      #               fileNamePrefix = filename,
      #               region = region,
      #               scale = 20).start()


      #Select pixels that only have more than 60% of an unique class

      import numpy as np

      modis_grid = ee.FeatureCollection('projects/ee-andresbertoncini/assets/Athabasca_MODIS_GRID_Sinu')

      modes = result.reduceRegions(collection = modis_grid, reducer = ee.Reducer.mode(), scale = 20)

      value_count = result.reduceRegions(collection = modes, reducer = ee.Reducer.frequencyHistogram(), scale = 20)


      def addListValues(feature):
          value_list = ee.Dictionary(feature.get('histogram')).values().filter(ee.Filter.gt('item', 0.6)).length()

          return feature.set('valid_pixels', value_list)

      modes_with_numbers = value_count.map(addListValues)


      #Create one feature collection with dissolved polygons for each class

      cluster_0 = modes_with_numbers.filter(ee.Filter.eq("mode", 0)).filter(ee.Filter.eq("valid_pixels", 1))
      cluster_1 = modes_with_numbers.filter(ee.Filter.eq("mode", 1)).filter(ee.Filter.eq("valid_pixels", 1))
      cluster_2 = modes_with_numbers.filter(ee.Filter.eq("mode", 2)).filter(ee.Filter.eq("valid_pixels", 1))
      cluster_3 = modes_with_numbers.filter(ee.Filter.eq("mode", 3)).filter(ee.Filter.eq("valid_pixels", 1))
      cluster_4 = modes_with_numbers.filter(ee.Filter.eq("mode", 4)).filter(ee.Filter.eq("valid_pixels", 1))
      cluster_5 = modes_with_numbers.filter(ee.Filter.eq("mode", 5)).filter(ee.Filter.eq("valid_pixels", 1))


      cluster_0_union = cluster_0.union()
      cluster_1_union = cluster_1.union()
      cluster_2_union = cluster_2.union()
      cluster_3_union = cluster_3.union()
      cluster_4_union = cluster_4.union()
      cluster_5_union = cluster_5.union()


      cluster_merged = cluster_0_union.merge(cluster_1_union).merge(cluster_2_union).merge(cluster_3_union).merge(cluster_4_union).merge(cluster_5_union)


      #Calculate the mean for each cluster class

      means_AN_ratio_bs = AN_Ratio_bs.reduceRegions(collection = cluster_merged, reducer = ee.Reducer.mean(), scale = 500)

      means_AN_ratio_ws = AN_Ratio_ws.reduceRegions(collection = cluster_merged, reducer = ee.Reducer.mean(), scale = 500)


      #Generate the AN ratio multipliers for each class and type of BRDF as lists

      multiplier_bs_blue = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('blue_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('blue_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('blue_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('blue_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('blue_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('blue_bs')]).replaceAll([ ],-9999).flatten()

      multiplier_bs_green = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('green_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('green_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('green_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('green_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('green_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('green_bs')]).replaceAll([ ],-9999).flatten()

      multiplier_bs_red = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('red_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('red_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('red_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('red_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('red_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('red_bs')]).replaceAll([ ],-9999).flatten()

      multiplier_bs_nir = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('nir_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('nir_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('nir_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('nir_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('nir_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('nir_bs')]).replaceAll([ ],-9999).flatten()

      multiplier_bs_swir1 = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('swir1_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('swir1_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('swir1_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('swir1_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('swir1_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('swir1_bs')]).replaceAll([ ],-9999).flatten()

      multiplier_bs_swir2 = ee.List([means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(0))).aggregate_array('swir2_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(1))).aggregate_array('swir2_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(2))).aggregate_array('swir2_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(3))).aggregate_array('swir2_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(4))).aggregate_array('swir2_bs'),
                         means_AN_ratio_bs.filter(ee.Filter.eq('system:index', means_AN_ratio_bs.aggregate_array('system:index').get(5))).aggregate_array('swir2_bs')]).replaceAll([ ],-9999).flatten()


      multiplier_ws_blue = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('blue_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('blue_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('blue_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('blue_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('blue_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('blue_ws')]).replaceAll([ ],-9999).flatten()

      multiplier_ws_green = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('green_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('green_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('green_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('green_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('green_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('green_ws')]).replaceAll([ ],-9999).flatten()

      multiplier_ws_red = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('red_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('red_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('red_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('red_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('red_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('red_ws')]).replaceAll([ ],-9999).flatten()

      multiplier_ws_nir = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('nir_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('nir_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('nir_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('nir_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('nir_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('nir_ws')]).replaceAll([ ],-9999).flatten()

      multiplier_ws_swir1 = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('swir1_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('swir1_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('swir1_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('swir1_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('swir1_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('swir1_ws')]).replaceAll([ ],-9999).flatten()

      multiplier_ws_swir2 = ee.List([means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(0))).aggregate_array('swir2_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(1))).aggregate_array('swir2_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(2))).aggregate_array('swir2_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(3))).aggregate_array('swir2_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(4))).aggregate_array('swir2_ws'),
                                  means_AN_ratio_ws.filter(ee.Filter.eq('system:index', means_AN_ratio_ws.aggregate_array('system:index').get(5))).aggregate_array('swir2_ws')]).replaceAll([ ],-9999).flatten()


      #Remap multipliers to S2 resolution according to cluster class Number

      cluster_list = ee.List([0,1,2,3,4,5])

      albedo_multiplier_bs_blue = result.remap(cluster_list, multiplier_bs_blue)
      albedo_multiplier_bs_green = result.remap(cluster_list, multiplier_bs_green)
      albedo_multiplier_bs_red = result.remap(cluster_list, multiplier_bs_red)
      albedo_multiplier_bs_nir = result.remap(cluster_list, multiplier_bs_nir)
      albedo_multiplier_bs_swir1 = result.remap(cluster_list, multiplier_bs_swir1)
      albedo_multiplier_bs_swir2 = result.remap(cluster_list, multiplier_bs_swir2)

      albedo_multiplier_ws_blue = result.remap(cluster_list, multiplier_ws_blue)
      albedo_multiplier_ws_green = result.remap(cluster_list, multiplier_ws_green)
      albedo_multiplier_ws_red = result.remap(cluster_list, multiplier_ws_red)
      albedo_multiplier_ws_nir = result.remap(cluster_list, multiplier_ws_nir)
      albedo_multiplier_ws_swir1 = result.remap(cluster_list, multiplier_ws_swir1)
      albedo_multiplier_ws_swir2 = result.remap(cluster_list, multiplier_ws_swir2)


      #Export multipliers
      
      multipliers_vec = [ ]

      multipliers_vec.append(multiplier_bs_blue.getInfo())
      multipliers_vec.append(multiplier_bs_green.getInfo())
      multipliers_vec.append(multiplier_bs_red.getInfo())
      multipliers_vec.append(multiplier_bs_nir.getInfo())
      multipliers_vec.append(multiplier_bs_swir1.getInfo())
      multipliers_vec.append(multiplier_bs_swir2.getInfo())

      multipliers_vec.append(multiplier_ws_blue.getInfo())
      multipliers_vec.append(multiplier_ws_green.getInfo())
      multipliers_vec.append(multiplier_ws_red.getInfo())
      multipliers_vec.append(multiplier_ws_nir.getInfo())
      multipliers_vec.append(multiplier_ws_swir1.getInfo())
      multipliers_vec.append(multiplier_ws_swir2.getInfo())
      
      multipliers_vec = np.concatenate(np.array(multipliers_vec))
      
      
      #Apply multipliers to S2 corrected reflectance

      spectral_albedo_bs_blue = albedo_multiplier_bs_blue.multiply(ref_masked.select('B2'))
      spectral_albedo_bs_green = albedo_multiplier_bs_green.multiply(ref_masked.select('B3'))
      spectral_albedo_bs_red = albedo_multiplier_bs_red.multiply(ref_masked.select('B4'))
      spectral_albedo_bs_nir = albedo_multiplier_bs_nir.multiply(ref_masked.select('B8A'))
      spectral_albedo_bs_swir1 = albedo_multiplier_bs_swir1.multiply(ref_masked.select('B11'))
      spectral_albedo_bs_swir2 = albedo_multiplier_bs_swir2.multiply(ref_masked.select('B12'))

      spectral_albedo_ws_blue = albedo_multiplier_ws_blue.multiply(ref_masked.select('B2'))
      spectral_albedo_ws_green = albedo_multiplier_ws_green.multiply(ref_masked.select('B3'))
      spectral_albedo_ws_red = albedo_multiplier_ws_red.multiply(ref_masked.select('B4'))
      spectral_albedo_ws_nir = albedo_multiplier_ws_nir.multiply(ref_masked.select('B8A'))
      spectral_albedo_ws_swir1 = albedo_multiplier_ws_swir1.multiply(ref_masked.select('B11'))
      spectral_albedo_ws_swir2 = albedo_multiplier_ws_swir2.multiply(ref_masked.select('B12'))


      #Retrieve incoming solar radiation from ERA5-Land

      ERA5 = ee.ImageCollection("ECMWF/ERA5_LAND/HOURLY").filter(ee.Filter.date(TARGET_DATE.update(hour = 20, minute = 00, second = 00)));

      ERA5_solar_rad = ERA5.select('surface_solar_radiation_downwards_hourly');

      solar_rad_wm2 = ERA5_solar_rad.first().divide(3600);

      solar_rad_wm2_number = solar_rad_wm2.reduceRegion(reducer = ee.Reducer.mean(), geometry = geom.centroid()).get('surface_solar_radiation_downwards_hourly').getInfo()


      #Calculate fraction of diffuse incoming radiation

      import numpy as np

      def SolarIrradiance(latitude, julian_date):
          #Input variables and parameters
          lat = latitude*0.0174533 #rad
          long = -115.1389 #deg
          SM = -105 #deg longitude for local time zone
          Gsc = 1367 #W.m^-2 Solar Constant

          #Declination angle

          delta = (0.409*math.sin(((2*math.pi/365)*julian_date) - 1.39))

          delta_deg = delta*57.2958

          #Equation of time

          t = ((2*math.pi*julian_date)/366) + 4.8718

          E =  (5.0323 - 430.847*math.cos(t) + 12.5024*math.cos(2*t) + 18.25*math.cos(3*t) - 100.976*math.sin(t) + 595.275*math.sin(2*t) + 3.6858*math.sin(3*t) - 12.47*math.sin(4*t))/60

          #Solar noon and solar time

          SN = 12 - (E/60) - ((SM - long)/15)

          LST = np.arange(0, 24, 1) #local standard time

          #Hour angle

          h = (LST-SN)*(math.pi/12)

          #Zenith angle

          cos_Z = (math.sin(lat)*math.sin(delta)) + (math.cos(lat)*math.cos(delta)*np.cos(h))

          #Hourly extraterrestrial radiation

          dr = 1 + 0.033*math.cos(((2*math.pi)/365)*julian_date)

          Ra_hor = Gsc*dr*cos_Z

          if Ra_hor.any() < 0:
            Ra_hor = 0

          return(Ra_hor[13])


      lat_rad = np.array(S2_corrected.pixelLonLat().select('latitude').reduceRegion(reducer = ee.Reducer.mean(), geometry = ee.Geometry.Point(-117.258500, 52.186170), scale = 20).values().getInfo())

      incoming_rad = SolarIrradiance(lat_rad, dates_python_julian[l])

      kt = solar_rad_wm2_number/incoming_rad

      #f_dif from Ellis and Pomeroy (2007) "Estimating sub-canopy shortwave irradiance to melting snow on forested slopes" - Hydrological Processes.

      f_dif = 1.1 - (1.09*kt)


      #Convert narrow to broadband albedo

      albedo_bsa_int = ((spectral_albedo_bs_blue.multiply(-0.1992)).add(spectral_albedo_bs_green.multiply(2.3002)).add(spectral_albedo_bs_red.multiply(-1.9121)).add(spectral_albedo_bs_nir.multiply(0.6715)).add(spectral_albedo_bs_swir1.multiply(-2.2728)).add(spectral_albedo_bs_swir2.multiply(1.9341))).subtract(0.0001)
      albedo_wsa_int = ((spectral_albedo_ws_blue.multiply(-0.1992)).add(spectral_albedo_ws_green.multiply(2.3002)).add(spectral_albedo_ws_red.multiply(-1.9121)).add(spectral_albedo_ws_nir.multiply(0.6715)).add(spectral_albedo_ws_swir1.multiply(-2.2728)).add(spectral_albedo_ws_swir2.multiply(1.9341))).subtract(0.0001)

      blue_sky_albedo = (albedo_bsa_int.multiply(1 - f_dif)).add(albedo_wsa_int.multiply(f_dif))

      blue_sky_albedo = blue_sky_albedo.updateMask(blue_sky_albedo.lt(1))

      blue_sky_albedo = blue_sky_albedo.updateMask(blue_sky_albedo.gt(0))

      blue_sky_albedo_masked = blue_sky_albedo.updateMask(S2_corrected.select('B8A').gt(0.15))

      blue_sky_albedo_masked = blue_sky_albedo_masked.updateMask(S2_corrected.normalizedDifference(['B3', 'B11']).gt(0.4))

      blue_sky_albedo_masked = blue_sky_albedo_masked.updateMask(blue_sky_albedo_masked.gt(0.15))
      
      
      #Export albedo without mask
      
      #filename = 'Athabasca_blue_sky_albedo_'+dates_python_ymd[l]

      #region = geom.buffer(5000).bounds().getInfo()['coordinates']

      #ee.batch.Export.image.toDrive(image = blue_sky_albedo, 
      #               description = 'image_export',
      #               folder ='Athabasca_Export_Images',
      #               fileNamePrefix = filename,
      #               region = region,
      #               scale = 20).start()
                     
      
      #Export masked albedo
                     
      #filename = 'Athabasca_blue_sky_albedo_masked_'+dates_python_ymd[l]

      #region = geom.buffer(5000).bounds().getInfo()['coordinates']

      #ee.batch.Export.image.toDrive(image = blue_sky_albedo_masked, 
      #               description = 'image_export',
      #               folder ='Athabasca_Export_Images',
      #               fileNamePrefix = filename,
      #               region = region,
      #               scale = 20).start()


      #Extract albedo for given coordinates

      hrus_mean_albedo = blue_sky_albedo_masked.reduceRegions(reducer = ee.Reducer.mean(), collection = athabasca_hrus, scale = 20)

      hrus_mean_albedo_total = blue_sky_albedo.reduceRegions(reducer = ee.Reducer.mean(), collection = athabasca_hrus, scale = 20)

      station_albedo = blue_sky_albedo_masked.reduceRegion(reducer = ee.Reducer.mean(), geometry = ee.Geometry.Point(-117.250500, 52.192320), scale = 20).values().getInfo()

      hru_albedos = ee.List([hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 1)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 2)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 3)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 4)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 5)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 6)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 7)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 8)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 9)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 10)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 12)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 11)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 13)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 14)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 15)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 16)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 17)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 18)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 19)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 20)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 21)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 22)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 23)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 24)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 25)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 26)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 27)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 28)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 29)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 30)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 31)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 32)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 33)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 34)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 35)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 36)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 37)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 38)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 39)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 40)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 41)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 42)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 43)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 44)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 45)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 46)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 47)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 48)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 49)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 50)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 51)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 52)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 53)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 54)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 55)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 56)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 57)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 58)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 59)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 60)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 61)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 62)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 63)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 64)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 65)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 66)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 67)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 68)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 69)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 70)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 71)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 72)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 73)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 74)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 75)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 76)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 77)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 78)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 79)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 80)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 81)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 82)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 83)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 84)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 85)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 86)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 87)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 88)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 89)).aggregate_array('mean'),
                            hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 90)).aggregate_array('mean')]).replaceAll([ ], -9999).flatten()


      hru_albedos_total = ee.List([hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 1)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 2)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 3)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 4)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 5)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 6)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 7)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 8)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 9)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 10)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 12)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 11)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 13)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 14)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 15)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 16)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 17)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 18)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 19)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 20)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 21)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 22)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 23)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 24)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 25)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 26)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 27)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 28)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 29)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 30)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 31)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 32)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 33)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 34)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 35)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 36)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 37)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 38)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 39)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 40)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 41)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 42)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 43)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 44)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 45)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 46)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 47)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 48)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 49)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 50)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 51)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 52)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 53)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 54)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 55)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 56)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 57)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 58)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 59)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 60)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 61)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 62)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 63)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 64)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 65)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 66)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 67)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 68)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 69)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 70)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 71)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 72)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 73)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 74)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 75)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 76)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 77)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 78)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 79)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 80)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 81)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 82)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 83)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 84)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 85)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 86)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 87)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 88)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 89)).aggregate_array('mean'),
                                  hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 90)).aggregate_array('mean')]).replaceAll([ ], -9999).flatten()


      albedo_timeseries.append(hru_albedos.getInfo())

      albedo_total_timeseries.append(hru_albedos_total.getInfo())

      pixel_count_snow = blue_sky_albedo_masked.reduceRegions(reducer = ee.Reducer.count(), collection = athabasca_hrus, scale = 20)

      pixel_count_cov = blue_sky_albedo.reduceRegions(reducer = ee.Reducer.count(), collection = athabasca_hrus, scale = 20)

      hru_no = hrus_mean_albedo.aggregate_array('HRU_NO')

      hru_perc = (np.array(pixel_count_cov.aggregate_array('count').sort(hru_no).getInfo())/np.array(blue_sky_albedo_masked.unmask(-9999).reduceRegions(reducer = ee.Reducer.count(), collection = athabasca_hrus, scale = 20).aggregate_array('count').sort(hru_no).getInfo()))*100

      hru_perc_timeseries.append(hru_perc)

      sca = (np.array(pixel_count_snow.aggregate_array('count').sort(hru_no).getInfo())/np.array(blue_sky_albedo_masked.unmask(-9999).reduceRegions(reducer = ee.Reducer.count(), collection = athabasca_hrus, scale = 20).aggregate_array('count').sort(hru_no).getInfo()))*100

      sca_timeseries.append(sca)

      print(dates_python_ymd[l])

      print(hrus_mean_albedo.filter(ee.Filter.eq('HRU_NO', 15)).aggregate_array('mean').getInfo())

      print(hrus_mean_albedo_total.filter(ee.Filter.eq('HRU_NO', 15)).aggregate_array('mean').getInfo())

      print(sca[14])

      print(hru_perc[14])

      print(station_albedo)

      albedo_timeseries_st.append(station_albedo)
      
      multipliers_export.append(multipliers_vec)


#Write timeseries to file

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/Dates_Athabasca_20230415.csv", dates_python_ymd, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/HRU_Snow_Albedo_Athabasca_20230415.csv", albedo_timeseries, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/HRU_Total_Albedo_Athabasca_20230415.csv", albedo_total_timeseries, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/HRU_SCA_Athabasca_20230415.csv", sca_timeseries, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/HRU_Perc_Athabasca_20230415.csv", hru_perc_timeseries, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/Station_Albedo_Athabasca_20230415.csv", albedo_timeseries_st, delimiter = ",", fmt='%s')

np.savetxt("/media/project/abertoncini/03_Data_Assimilation/03_Albedo_Outputs/Athabasca_AN_Ratios_20230415.csv", multipliers_export, delimiter = ",", fmt='%s')

