#Code to correct S2 images with 6S

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

#python

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

