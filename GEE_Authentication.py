#Function to authenticate GEE environment

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

