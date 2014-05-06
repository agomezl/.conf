#!/bin/bash

convert -resize 1920x1080 -quality 100 -density 150 -sharpen 0x1.0 CS.pdf CS_1920x1080.png
convert -resize 1366x768  -quality 100 -density 150 -sharpen 0x1.0 CS.pdf CS_1366x768.png
convert -resize 1280x1024 -quality 100 -density 150 -sharpen 0x1.0 CS.pdf CS_1280x1024.png
