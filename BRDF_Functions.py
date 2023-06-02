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
  qa = image.select('state_1km')
  cloudInternalMask = qa.bitwiseAnd(cloudInternalBitMask).eq(0)
  mask = cloudInternalMask

  return image.updateMask(mask)


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

  lat = ee.Image(51).multiply(math.pi/180)

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

  reflectance_nadir = kernels_nadir.matrixMultiply(fit_blue).arrayGet([0,0]).rename(['blue_nadir']).addBands(kernels_nadir.matrixMultiply(fit_green).arrayGet([0,0]).rename(['green_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_red).arrayGet([0,0]).rename(['red_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_nir).arrayGet([0,0]).rename(['nir_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_swir1).arrayGet([0,0]).rename(['swir1_nadir'])).addBands(kernels_nadir.matrixMultiply(fit_swir2).arrayGet([0,0]).rename(['swir2_nadir']))

  reflectance_bs = kernels_bs.matrixMultiply(fit_blue).arrayGet([0,0]).rename(['blue_bs']).addBands(kernels_bs.matrixMultiply(fit_green).arrayGet([0,0]).rename(['green_bs'])).addBands(kernels_bs.matrixMultiply(fit_red).arrayGet([0,0]).rename(['red_bs'])).addBands(kernels_bs.matrixMultiply(fit_nir).arrayGet([0,0]).rename(['nir_bs'])).addBands(kernels_bs.matrixMultiply(fit_swir1).arrayGet([0,0]).rename(['swir1_bs'])).addBands(kernels_bs.matrixMultiply(fit_swir2).arrayGet([0,0]).rename(['swir2_bs']))

  reflectance_ws = kernels_ws.matrixMultiply(fit_blue).arrayGet([0,0]).rename(['blue_ws']).addBands(kernels_ws.matrixMultiply(fit_green).arrayGet([0,0]).rename(['green_ws'])).addBands(kernels_ws.matrixMultiply(fit_red).arrayGet([0,0]).rename(['red_ws'])).addBands(kernels_ws.matrixMultiply(fit_nir).arrayGet([0,0]).rename(['nir_ws'])).addBands(kernels_ws.matrixMultiply(fit_swir1).arrayGet([0,0]).rename(['swir1_ws'])).addBands(kernels_ws.matrixMultiply(fit_swir2).arrayGet([0,0]).rename(['swir2_ws']))

  return image.addBands(reflectance_nadir).addBands(reflectance_bs).addBands(reflectance_ws)


