### Steps to setup your Raspberry Pi

The first step is to install the OS in the SD card of your Pi using the Raspberry Pi Imager. You can either use the officia version of the OS or use custom images. For this prototype, we have used Comitup lite OS.
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




