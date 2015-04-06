#!/bin/bash

./Main.exe
hpc report Main.exe --exclude=Main
hpc markup Main.exe --exclude=Main --destdir=coverage/
