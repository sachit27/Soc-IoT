### Hardware components for building Co-Sense Unit
1. [Raspberry Pi Zero WH](https://thingspeak.com/channels/1328211)
2. [Sensirion SPS 30](https://www.digikey.ch/de/products/detail/sensirion-ag/SPS30/9598990?utm_adgroup=General&utm_source=google&utm_medium=cpc&utm_campaign=PMax:%20Smart%20Shopping_Product_Zombie%20SKU&utm_term=&productid=9598990&gclid=CjwKCAjwhNWZBhB_EiwAPzlhNobWZCC9LTumIblpqT72Mplms0zE3mOhY61Uit1KO_4wqUNXLDBwfBoCV2IQAvD_BwE)
3. [Enviro Plus](https://www.pi-shop.ch/enviro-for-raspberry-pi)
4. 16 GB SD card (can be purchased from any vendor)
5. [JST ZHR Cable for SPS30](https://www.sparkfun.com/products/15108)
6. [Female header for connecting JST ZHR cable coming from SPS30 to Enviro Plus board](https://www.sparkfun.com/products/115)
7. 3D printed case for the device. The [STL files](https://github.com/sachit27/Soc-IoT/tree/main/CoSense%20Unit/stl%20files) are available in the repository.
8. [Power supply for Raspberry Pi](https://www.digikey.ch/de/products/detail/raspberry-pi/RPI%2520USB-C%2520POWER%2520SUPPLY%2520BLACK%2520EU/10258762?utm_adgroup=AC%20DC%20Desktop%2C%20Wall%20Adapters&utm_source=google&utm_medium=cpc&utm_campaign=Shopping_Product_Power%20Supplies%20-%20External%2FInternal%20%28Off-Board%29%29&utm_term=&productid=10258762&gclid=CjwKCAjwhNWZBhB_EiwAPzlhNs-Flrl3aNjrVcC_WfVP8yzEdqXtZj_x1LE9Q2Hj-jMKwd1s871pAhoCffAQAvD_BwE) 

You will need to solder the Female header pins to the Enviro Plus GPIO. Not all the pins would be used. Only 5V, GND, GND, SCL and SDA would be used for connecting the SPS30 sensor. You can use the [wiring diagram](https://github.com/sachit27/Soc-IoT/blob/main/SPS30_Wiring.png) to add the connection.

### Steps to setup your Raspberry Pi

The first step is to install the OS in the SD card of your Pi using the Raspberry Pi Imager. You can either use the officia version of the OS or use custom images. For this prototype, we have used [Comitup lite OS](http://davesteele.github.io/comitup/).
Also, you can use the settings option in the Raspberry Pi Imager to predefine the username, password, set timezones and other functionalities. Once you have installed the OS, put the SD card back in your Pi. Once it reboots, you will see a WiFi network labelled "comitup-xxx" in your network options.
You can click on that and setup the WiFi connection.

### Some basics commands to finish setting up the Pi
Once you have connected to the WiFi, you can use the terminal to create a connection with your Pi using SSH. Use the username and password that selected earlier while installing the OS to create a connection.
Once the connection is made, use the following commands.

1. sudo apt-get update
2. sudo apt-get upgrade
3. sudo apt-get install python3-dev python3-pip
4. curl -sSL https://get.pimoroni.com/enviroplus | bash
5. pip3 install sps30
6. pip3 install smbus2
7. sudo apt-get install python3-pandas

Note: Remember to enable the i2c by using "sudo raspi-config" and then changing the permissions.

### Running the program in Raspberry Pi
Once the Raspberry Pi is setup, create a file for saving the Python code. You can use it by using the following command:
# nano code.py #
This will create

