import urllib.request
import time
import math
from time import sleep
import datetime
import csv
import os

try:
        from smbus2 import SMBus
except ImportError:
        from smbus import SMBus

import colorsys
import sys
import ST7735
try:
    # Transitional fix for breaking change in LTR559
    from ltr559 import LTR559
    ltr559 = LTR559()
except ImportError:
    import ltr559

import pandas as pd
from bme280 import BME280
from enviroplus import gas
from subprocess import PIPE, Popen
from sps30 import SPS30
from PIL import Image
from PIL import ImageDraw
from PIL import ImageFont
from fonts.ttf import RobotoMedium as UserFont
import logging

import ST7735
from urllib.request import urlopen

myAPI = 'PU5E06PLO4LHJOR9'
baseURL = 'https://api.thingspeak.com/update?api_key=%s' % myAPI

bus = SMBus(1)
bme280 = BME280(i2c_dev=bus)

sps = SPS30(1)

seconds = 0

# Create ST7735 LCD display class
st7735 = ST7735.ST7735(
    port=0,
    cs=1,
    dc=9,
    backlight=12,
    rotation=90,
    spi_speed_hz=10000000
)

# Initialize display
st7735.begin()

WIDTH = st7735.width
HEIGHT = st7735.height

# Set up canvas and font
img = Image.new('RGB', (WIDTH, HEIGHT), color=(0, 0, 0))
draw = ImageDraw.Draw(img)
font_size_small = 10
font_size_large = 20
font = ImageFont.truetype(UserFont, font_size_large)
smallfont = ImageFont.truetype(UserFont, font_size_small)
x_offset = 2
y_offset = 2

message = ""

# The position of the top bar
top_pos = 25
# Create a values dict to store the data
variables = ["temperature",
             "humidity",
             "light",
             "pm1",
             "pm25",
             "pm10"]

units = ["C",
         "%",
         "Lux",
         "ug/m3",
         "ug/m3",
         "ug/m3"]

limits = [[4, 18, 28, 35],
         [20, 30, 60, 70],
         [-1, -1, 30000, 100000],
         [0, 15, 35, 75],
         [0, 15, 40, 65],
         [0, 55, 150, 250]]
# RGB palette for values on the combined screen
palette = [(0, 0, 255),           # Dangerously Low
          (0, 255, 255),         # Low
          (0, 255, 0),           # Normal
          (255, 255, 0),         # High
          (255, 0, 0)]           # Dangerously High
values = {}

# Check internet connection
try:
    response = urlopen('https://www.google.com')
    internet_connected = True
except:
    internet_connected = False

# Display message
if internet_connected:
    message = "Internet ON"
else:
    message = "Internet OFF"
draw.text((x_offset, top_pos), message, font=font, fill=(255, 255, 255))
st7735.display(img)

def get_cpu_temperature() -> float:
    """
    Gets CPU temperature
    :return: Float CPU temperature value
    """
    process = Popen(['vcgencmd', 'measure_temp'], stdout=PIPE, universal_newlines=True)
    output, _error = process.communicate()
    return float(output[output.index('=') + 1:output.rindex("'")])

def get_compensated_temperature() -> float:
    """
    Temporary method compensating heat from CPU

    :return: Float compensated temperature value
    """
    comp_factor = 2.0
    cpu_temp = get_cpu_temperature()
    raw_temp = bme280.get_temperature()
    comp_temp = raw_temp - ((cpu_temp - raw_temp) / comp_factor)
    comp_temp_adjusted = comp_temp - 3.5
    return comp_temp_adjusted


def save_data(idx, data):
    variable = variables[idx]
    # Maintain length of list
    values[variable] = values[variable][1:] + [data]
    unit = units[idx]
    message = "{}: {:.1f} {}".format(variable[:4], data, unit)

# Displays all the text on the LCD
def display_everything():
    draw.rectangle((0, 0, WIDTH, HEIGHT), (0, 0, 0))
    column_count = 2
    row_count = (len(variables) / column_count)
    for i in range(len(variables)):
        variable = variables[i]
        data_value = values[variable][-1]
        unit = units[i]
        x = x_offset + ((WIDTH // column_count) * (i // row_count))
        y = y_offset + ((HEIGHT / row_count) * (i % row_count))
        message = "{}: {:.1f} {}".format(variable[:4], data_value, unit)
        lim = limits[i]
        rgb = palette[0]
        for j in range(len(lim)):
            if data_value > lim[j]:
                rgb = palette[j + 1]
        draw.text((x, y), message, font=smallfont, fill=rgb)
    st7735.display(img)

press_reading = bme280.get_pressure()

# Humidity
humid = bme280.get_humidity()

# Light meter
light_reading = ltr559.get_lux()

for v in variables:
        values[v] = [1] * WIDTH

# PM sensor
if sps.read_article_code() == sps.ARTICLE_CODE_ERROR:
    raise Exception("ARTICLE CODE CRC ERROR!")
else:
    print("ARTICLE CODE: " + str(sps.read_article_code()))

if sps.read_device_serial() == sps.SERIAL_NUMBER_ERROR:
    raise Exception("SERIAL NUMBER CRC ERROR!")
else:
    print("DEVICE SERIAL: " + str(sps.read_device_serial()))

sps.set_auto_cleaning_interval(seconds) # default 604800, set 0 to disable auto-cleaning

sps.device_reset() # device has to be powered-down or reset to check new auto-cleaning interval

if sps.read_auto_cleaning_interval() == sps.AUTO_CLN_INTERVAL_ERROR: # or returns the interval in seconds
    raise Exception("AUTO-CLEANING INTERVAL CRC ERROR!")
else:
    print("AUTO-CLEANING INTERVAL: " + str(sps.read_auto_cleaning_interval()))

sleep(3)
sps.start_measurement()
sleep(3)

while not sps.read_data_ready_flag():
    sleep(0.25)
    if sps.read_data_ready_flag() == sps.DATA_READY_FLAG_ERROR:
        raise Exception("DATA-READY FLAG CRC ERROR!")

if sps.read_measured_values() == sps.MEASURED_VALUES_ERROR:
    raise Exception("MEASURED VALUES CRC ERROR!")
else:
    #print ("PM1.0 Value in   g/m3: " + str(sps.dict_values['pm1p0']))
    print ("PM2.5 Value in   g/m3: " + str(sps.dict_values['pm2p5']))

sps.stop_measurement()
sps.start_fan_cleaning()

def get_hum():
    """Get humidity from the weather sensor"""
    humidity = bme280.get_humidity()
    hum = humidity + 14.5
    return(hum)

def get_pm1():
    variable = "pm1"
    unit = "ug/m3"
    data = sps.dict_values['pm1p0']
    data = round((data),2)
    return(data)

def get_pm25():
    variable = "pm25"
    unit = "ug/m3"
    data = sps.dict_values['pm2p5']
    data = round((data),2)
    return(data)

def get_pm10():
    variable = "pm10"
    unit = "ug/m3"
    data = sps.dict_values['pm10p0']
    data = round((data),2)
    return(data)


def time_now():
    now = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    now = str(now)
    return(now)

import csv
import os.path

# create a list of headers for the CSV file
headers = ["Date/Time", "Temperature (C)", "Humidity (%)", "PM1 (ug/m3)", "PM2.5 (ug/m3)", "PM10 (ug/m3)"]

# check if the CSV file already exists
if not os.path.isfile('data.csv'):
    # if it doesn't, create a new CSV file with headers
    with open('data.csv', mode='w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(headers)

# create a list of data to be saved to the CSV file
data = [[time_now(), get_compensated_temperature(), get_hum(), get_pm1(), get_pm25(), get_pm10()]]

# open the CSV file with append mode
with open('data.csv', mode='a', newline='') as file:
    # create a writer object
    writer = csv.writer(file)

    # write data to the following rows
    writer.writerows(data)

print("Data saved to data.csv!")


# Process data for Display
save_data(0, get_compensated_temperature())
save_data(1, get_hum())
save_data(2, light_reading)
save_data(3, get_pm1())
save_data(4, get_pm25())
save_data(5, get_pm10())

display_everything()

f = urlopen(baseURL + '&field1=%s&field2=%s&field3=%s&field4=%s&field5=%s' % (get_compensated_temperature(), get_hum(), get_pm1(), get_pm25(), get_pm10()))
f.read()
f.close()
