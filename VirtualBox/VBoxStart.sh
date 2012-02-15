#!/bin/bash

# Script to start a virtual machine in headless mode.

# PARAMS
# -----------------------------------------------------------------------------

# First parameter is the machine name.
MACHINE_NAME=$1

# START
# -----------------------------------------------------------------------------

VBoxHeadless --startvm $MACHINE_NAME --vrde=off

