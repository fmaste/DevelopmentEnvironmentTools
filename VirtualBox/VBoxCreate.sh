#!/bin/bash

# Script to create an Ubuntu Server virtual machine for development.

# CHECK CPU
# ----------------------------------------------------------------------------

# On AMD look for svm (secure virtual machine)
# Intel's equivalent is vmx.
# TODO: VIRTUALIZATION_FLAGS=`grep -E '^flags.*(vmx|svm)' /proc/cpuinfo`

# PARAMS
# -----------------------------------------------------------------------------

# First parameter is the machine name.
MACHINE_NAME=$1

# Second parameter must be the HDD to clone.
# TODO

# CREATE AND REGISTER
# -----------------------------------------------------------------------------

# This command creates a new XML virtual machine definition file.
# Since this name is used by default as the file name of the settings file (with
# the extension .xml) and the machine folder (a subfolder of the 
# .VirtualBox/Machines folder), it must conform to your host operating system's 
# requirements for file name specifications.
# When creating a new virtual machine with VBoxManage createvm, you can directly
# specify the --register option to avoid having to register it separately.
# The avaiable OS types can be listed with: 
#	VBoxManage list ostype
VBoxManage createvm --name $MACHINE_NAME --ostype Ubuntu_64 --register

# MEMORY
# -----------------------------------------------------------------------------

# This sets the amount of RAM, in MB, that the virtual machine should allocate 
# for itself from the host.
VBoxManage modifyvm $MACHINE_NAME --memory 512

# This sets the amount of RAM that the virtual graphics card should have.
# Must be in range [1, 256] MB.
# With less than 8 MB the "non-optimal settings" messagge is shown.
VBoxManage modifyvm $MACHINE_NAME --vram 16

# DISPLAY
# -----------------------------------------------------------------------------

# This enables, if the Guest Additions are installed, whether hardware 3D 
# acceleration should be available
VBoxManage modifyvm $MACHINE_NAME --accelerate3d off

# The VirtualBox Guest Additions contain experimental hardware 2D video 
# acceleration support for Windows guests.
VBoxManage modifyvm $MACHINE_NAME --accelerate2dvideo off

# This enables multi-monitor support.
VBoxManage modifyvm $MACHINE_NAME --monitorcount 1

# With the VirtualBox graphical user interface, this enables or disables the 
# VirtualBox remote desktop extension (VRDE) server.
# Note that when you use VBoxHeadless to start a VM, since the headless server
# has no other means of output, the VRDP server will always be enabled, 
# regardless of whether you had enabled the VRDP server in the VM's settings.
# If this is undesirable (for example because you want to access the VM via ssh 
# only), start the VM like this:
#     VBoxHeadless --startvm <uuid|name> --vrde=off
VBoxManage modifyvm $MACHINE_NAME --vrde off

# MOTHERBOARD
# -----------------------------------------------------------------------------

# Allows to choose the type of chipset being emulated for the given storage 
# controller.
# Chipset piix3 is VirtualBox default, better for portability.
# Some guest, like Mac OS X Server need Intel's ich9.
VBoxManage modifyvm $MACHINE_NAME --chipset piix3

# ACPI is the current industry standard to allow operating systems to recognize
# hardware, configure motherboards and other devices and manage power. As all 
# modern PCs contain this feature and Windows and Linux have been supporting it 
# for years, it is also enabled by default in VirtualBox.
# It cannot even be turned off using VirtualBox GUI.
# Must not be turned off after installation of a Windows guest OS.
VBoxManage modifyvm $MACHINE_NAME --acpi on

# Advanced Programmable Interrupt Controllers (APICs) are a newer x86 hardware
# feature that have replaced old-style Programmable Interrupt Controllers (PICs)
# in recent years. With an I/O APIC, operating systems can use more than 16 
# interrupt requests (IRQs) and therefore avoid IRQ sharing for improved 
# reliability.
# Enabling the I/O APIC is required for 64-bit guest operating systems, 
# especially Windows Vista; it is also required if you want to use more than one
# virtual CPU in a virtual machine.
# However, software support for I/O APICs has been unreliable with some 
# operating systems other than Windows. Also, the use of an I/O APIC slightly
# increases the overhead of virtualization and therefore slows down the guest OS
# a little.
# For Linux 64 bits and Mac OS X Server it is enabled by default.
# Must not be turned off after installation of a Windows guest OS.
VBoxManage modifyvm $MACHINE_NAME --ioapic on

# With this option, you can set whether the VM should have audio support.
VBoxManage modifyvm $MACHINE_NAME --audio none

# With this setting, you can select whether the guest operating system's 
# clipboard should be shared with the host.
# This requires that the Guest Additions be installed in the virtual machine.
VBoxManage modifyvm $MACHINE_NAME --clipboard disabled

# To be able to disable USB, make the mouse and keyboard use PS2 instead.
VBoxManage modifyvm $MACHINE_NAME --mouse ps2
VBoxManage modifyvm $MACHINE_NAME --keyboard ps2
# This option enables or disables the VM's virtual USB controller.
VBoxManage modifyvm $MACHINE_NAME --usb off
# This option enables or disables the VM's virtual USB 2.0 controller.
VBoxManage modifyvm $MACHINE_NAME --usbehci off

# If checked, VirtualBox will report the system time in UTC format to the guest 
# instead of local (host) time. This affects how the virtual real-time clock 
# (RTC) operates and may be useful for Unix-like guest operating systems, which 
# typically expect the hardware clock to be set to UTC.
# This is also VirtualBox default setting.
VBoxManage modifyvm $MACHINE_NAME --rtcuseutc on

# BIOS
# -----------------------------------------------------------------------------

# Specifies which firmware is used to boot particular virtual machine: EFI or 
# BIOS. Use EFI only if your fully understand what you're doing.
# For Mac OS X Server EFI is needed.
VBoxManage modifyvm $MACHINE_NAME --firmware bios

# You can determine whether the logo should fade in and out, respectively.
VBoxManage modifyvm $MACHINE_NAME --bioslogofadein off
VBoxManage modifyvm $MACHINE_NAME --bioslogofadeout off

# This specifies whether the BIOS allows the user to select a temporary boot 
# device. menuonly suppresses the message, but the user can still press F12 to 
# select a temporary boot device.
VBoxManage modifyvm $MACHINE_NAME --biosbootmenu messageandmenu

# This specifies the boot order for the virtual machine. There are four "slots",
# which the VM will try to access from 1 to 4, and for each of which you can set
# a device that the VM should attempt to boot from. 
# This are VirtualBox default settings.
VBoxManage modifyvm $MACHINE_NAME --boot1 floppy
VBoxManage modifyvm $MACHINE_NAME --boot2 dvd
VBoxManage modifyvm $MACHINE_NAME --boot3 disk
VBoxManage modifyvm $MACHINE_NAME --boot4 none

# PROCESSOR
# -----------------------------------------------------------------------------

# This enables CPU hot-plugging. When enabled, virtual CPUs can be added to and 
# removed from a virtual machine while it is running.
# If enabled the --cpus option specifies the maximum number of CPUs that the 
# virtual machine can have
VBoxManage modifyvm $MACHINE_NAME --cpuhotplug off

# This sets the number of virtual CPUs for the virtual machine
VBoxManage modifyvm $MACHINE_NAME --cpus 1

# This setting controls how much cpu time a virtual CPU can use. A value of 50
# implies a single virtual CPU can use up to 50% of a single host CPU.
VBoxManage modifyvm $MACHINE_NAME --cpuexecutioncap 90

# PAE determines whether the PAE and NX capabilities of the host CPU will be
# exposed to the virtual machine. PAE stands for "Physical Address Extension".
# Normally, if enabled and supported by the operating system, then even a 
# 32-bit x86 CPU can access more than 4 GB of RAM. This is made possible by 
# adding another 4 bits to memory addresses, so that with 36 bits, up to 64 GB 
# can be addressed. Some operating systems (such as Ubuntu Server) require PAE 
# support from the CPU and cannot be run in a virtual machine without it.
# I don't know why when creating an Ubuntu 64 bits PAE/NX is not enabled by default.
VBoxManage modifyvm $MACHINE_NAME --pae on

# Since 2006, Intel and AMD processors have had support for so-called "hardware
# virtualization". This means that these processors can help VirtualBox to 
# intercept potentially dangerous operations that a guest operating system may 
# be attempting and also makes it easier to present virtual hardware to a 
# virtual machine.
# VirtualBox's 64-bit guest support and multiprocessing (SMP) both require 
# hardware virtualization to be enabled.
# This enables or disables the use of hardware virtualization extensions (Intel
# VT-x or AMD-V) in the processor of your host system.
VBoxManage modifyvm $MACHINE_NAME --hwvirtex on

# This specifies whether VirtualBox will make exclusive use of the hardware 
# virtualization extensions (Intel VT-x or AMD-V) in the processor of your host
# system. If you wish to simultaneously share these extensions with other 
# hypervisors, then you must disable this setting. Doing so has negative 
# performance implications.
VBoxManage modifyvm $MACHINE_NAME --hwvirtexexcl off

# A newer feature called "nested paging" implements some memory management in 
# hardware, which can greatly accelerate hardware virtualization since these 
# tasks no longer need to be performed by the virtualization software.
# Nested paging thus significantly improves virtualization performance.
# If hardware virtualization is enabled, this additional setting enables or 
# disables the use of the nested paging feature in the processor of your host
# system.
# This extension is always enbaled by default when available.
VBoxManage modifyvm $MACHINE_NAME --nestedpaging on

# TODO: Check Intel first or VirtualBox just tries to use it?
# If hardware virtualization and nested paging are enabled, for Intel VT-x 
# only, an additional performance improvement of up to 5% can be obtained by 
# enabling this setting. This causes the hypervisor to use large pages to reduce
# TLB use and overhead.
VBoxManage modifyvm $MACHINE_NAME --largepages on

# TODO: Check Intel first or VirtualBox just tries to use it?
# If hardware virtualization is enabled, for Intel VT-x only, this additional 
# setting enables or disables the use of the tagged TLB (VPID) feature in the 
# processor of your host system.
# "Virtual Processor Identifiers" (VPIDs) can greatly accelerate context 
# switching by reducing the need for expensive flushing of the processor's 
# Translation Lookaside Buffers (TLBs).
VBoxManage modifyvm $MACHINE_NAME --vtxvpid on

# NETWORK 1
# -----------------------------------------------------------------------------

# Set the first network adapter in NAT mode for internet access.

# Be careful with VirtualBox NAT limitations:
# - ICMP protocol limitations:
#	Some frequently used network debugging tools (e.g. ping or tracerouting) rely 
# 	on the ICMP protocol for sending/receiving messages. While ICMP support has 
# 	been improved with VirtualBox 2.1 (ping should now work), some other tools 
# 	may not work reliably.
# - Receiving of UDP broadcasts is not reliable:
#	The guest does not reliably receive broadcasts, since, in order to save 
# 	resources, it only listens for a certain amount of time after the guest has 
# 	sent UDP data on a particular port. As a consequence, NetBios name resolution 
# 	based on broadcasts does not always work (but WINS always works). As a 
# 	workaround, you can use the numeric IP of the desired server in the 
# 	\\server\share notation.
# - Protocols such as GRE are unsupported:
#	Protocols other than TCP and UDP are not supported. This means some VPN 
# 	products (e.g. PPTP from Microsoft) cannot be used. There are other VPN 
# 	products which use simply TCP and UDP.
# - Forwarding host ports < 1024 impossible:
# 	On Unix-based hosts (e.g. Linux, Solaris, Mac OS X) it is not possible to 
# 	bind to ports below 1024 from applications that are not run by root. As a 
# 	result, if you try to configure such a port forwarding, the VM will refuse 
# 	to start. 
VBoxManage modifyvm $MACHINE_NAME --nic1 nat

# In NAT mode, the guest network interface is assigned to the IPv4 range 
# 10.0.x.0/24 by default where x corresponds to the instance of the NAT 
# interface +2. So x is 2 when there is only one NAT instance active. In that 
# case the guest is assigned to the address 10.0.2.15, the gateway is set to 
# 10.0.2.2 and the name server can be found at 10.0.2.3.
# If, for any reason, the NAT network needs to be changed, this can be achieved
# with the following command:
# 	VBoxManage modifyvm "VM name" --natnet1 "192.168/16"
# This command would reserve the network addresses from 192.168.0.0 to 
# 192.168.254.254 for the first NAT network instance of "VM name". The guest IP
# would be assigned to 192.168.0.15 and the default gateway could be found at 
# 192.168.0.2.
# Will be using the common commercial routers internal network addresses.
VBoxManage modifyvm $MACHINE_NAME --natnet1 "192.168/16"

# The PCNet FAST III is the default because it is supported by nearly all
# operating systems out of the box, as well as the GNU GRUB boot manager.
# When installing with the GUI Intel PRO/1000 MT Desktop (82540EM) is used.
VBoxManage modifyvm $MACHINE_NAME --nictype1 82540EM

# With this option you can set the MAC address of the virtual network card. 
# Normally, each virtual network card is assigned a random address by 
# VirtualBox at VM creation.
VBoxManage modifyvm $MACHINE_NAME --macaddress1 auto

# This allows you to temporarily disconnect a virtual network interface, as if 
# network cable had been pulled from a real network card.
VBoxManage modifyvm $MACHINE_NAME --cableconnected1 on

# By default, the NAT core uses aliasing and uses random ports when generating 
# an alias for a connection. This works well for the most protocols like SSH, 
# FTP and so on. Though some protocols might need a more transparent behavior or
# may depend on the real port number the packet was sent from.
# - log: enables logging.
# - proxyonly: switches off aliasing mode makes NAT transparent.
# - sameports: enforces NAT engine to send packets via the same port as they 
# originated on.
# - default: disable all mentioned modes above.
# These modes can be combined if necessary.
VBoxManage modifyvm $MACHINE_NAME --nataliasmode1 default

# This option specifies whether the built-in DHCP server passes the domain name
# for network name resolution.
VBoxManage modifyvm $MACHINE_NAME --natdnspassdomain1 on

# This option makes the NAT engine proxy all guest DNS requests to the host's 
# DNS servers.
# The NAT engine by default offers the same DNS servers to the guest that are 
# configured on the host. In some scenarios, it can be desirable to hide the DNS
# server IPs from the guest, for example when this information can change on the
# host due to expiring DHCP leases. In this case, you can tell the NAT engine to
# act as DNS proxy using the following command:
#	VBoxManage modifyvm $MACHINE_NAME --natdnsproxy1 on|off

# This option makes the NAT engine use the host's resolver mechanisms to handle
# DNS requests.
# For resolving network names, the DHCP server of the NAT engine offers a list 
# of registered DNS servers of the host. If for some reason you need to hide 
# this DNS server list and use the host's resolver settings, thereby forcing the
# VirtualBox NAT engine to intercept DNS requests and forward them to host's
# resolver, use the following command:
#	VBoxManage modifyvm $MACHINE_NAME --natdnshostresolver1 on|off
# Note that this setting is similar to the DNS proxy mode, however whereas the
# proxy mode just forwards DNS requests to the appropriate servers, the resolver
# mode will interpret the DNS requests and use the host's DNS API to query the
# information and return it to the guest.

# By default, VirtualBox's NAT engine will route TCP/IP packets through the 
# default interface assigned by the host's TCP/IP stack. (The technical reason 
# for this is that the NAT engine uses sockets for communication.) If, for some
# reason, you want to change this behavior, you can tell the NAT engine to bind
# to a particular IP address instead. Use the following command:
# 	VBoxManage modifyvm $MACHINE_NAME --natbindip1 "10.45.0.2"

# NETWORK 2
# -----------------------------------------------------------------------------

# Host-only mode for communication with the host.
# To access the guest from outside the host use ssh and port forwardind.

# Host-only networking mode.
# The virtual machine is connected to a virtual interface created on the host.
# Virtual machines can communicate between them and with the host but they
# cannot communicate with the world outside of the host.
VBoxManage modifyvm $MACHINE_NAME --nic2 hostonly

VBoxManage modifyvm $MACHINE_NAME --nictype2 82540EM

VBoxManage modifyvm $MACHINE_NAME --macaddress2 auto

VBoxManage modifyvm $MACHINE_NAME --cableconnected2 on

# Create a new one host-only network interface to use.
NEWHOSTONLYIF=`VBoxManage hostonlyif create | cut -d "'" -f 2`

# NEW=`VBoxManage list hostonlyifs|grep -w Name|tail -n -1|cut -c 18-`
# This doesn't work because the newly created isn't always the last one.
# For example if you remove the first interface and then create a new one.

# Set the newly created host-only network interface as the adapter.
VBoxManage modifyvm $MACHINE_NAME --hostonlyadapter2 $NEWHOSTONLYIF

# Add a DHCP server to the new interface.
# Will be using the 172.16.0.0 – 172.31.255.255 range of private IPs.
# Finally, you must specify --enable or the DHCP server will be created in the
# disabled state, doing nothing.
VBoxManage dhcpserver add --ifname $NEWHOSTONLYIF --ip 172.16.0.1 --netmask 255.240.0.0 --lowerip 172.16.0.2 --upperip 172.16.0.10 --enable

# STORAGE CONTROLLER
# -----------------------------------------------------------------------------

# Like a real SATA controller, VirtualBox's virtual SATA controller operates 
# faster and also consumes less CPU resources than the virtual IDE controller.
# This is also the default when creating a new virtual machine.
# By default VirtualBox creates an IDE for the DVD and a SATA one for the HDD.
# But does not work for the installation of an Ubuntu Virtualization Minimal.
# So only one IDE controller with two devices attached will be created.

# VirtualBox creates the IDE controller with this name by default.
IDE_CONTROLLER="IDE Controller"

# Create the controller.
VBoxManage storagectl $MACHINE_NAME --name "$IDE_CONTROLLER" --add ide

# Use the default controller to make more portable.
VBoxManage storagectl $MACHINE_NAME --name "$IDE_CONTROLLER" --controller piix4

# DONT KNOW!!!!
VBoxManage storagectl $MACHINE_NAME --name "$IDE_CONTROLLER" --hostiocache on
VBoxManage storagectl $MACHINE_NAME --name "$IDE_CONTROLLER" --bootable on

# STORAGE MEDIUM
# -----------------------------------------------------------------------------

# Will create an IDE controller with a HDD as primary master and a DVD as 
# secondary master.

# TODO: Take into account when cloning
#	--options link|keepallmacs|keepnatmacs
#	--options keepallmacs|keepnatmacs

# TODO:
# VBoxManage storageattach $MACHINE_NAME --storagectl "$IDE_CONTROLLER" --port 0 --device 0 --type hdd --medium ~/VirtualBox\ VMs/crowbar_admin/crowbar_admin.vdi

# Attach an empty DVD drive.
# Attach to port 1 (not 0) to make it the secondary master like the default.
VBoxManage storageattach $MACHINE_NAME --storagectl "$IDE_CONTROLLER" --device 0 --port 1 --type dvddrive --medium emptydrive

# OTHER 
# -----------------------------------------------------------------------------

# With this setting you turn on or off whether a machine waits for a teleporting 
# request to come in on the network when it is started.
VBoxManage modifyvm $MACHINE_NAME --teleporter off

# This allows you to specify the folder in which snapshots will be kept for a 
# virtual machine.
VBoxManage modifyvm $MACHINE_NAME --snapshotfolder default

# TO DELETE
# -----------------------------------------------------------------------------

# The unregistervm command unregisters a virtual machine. If --delete is also 
# specified, the following files will automatically be deleted as well:
# 1 - all hard disk image files, including differencing files, which are used by 
# the machine and not shared with other machines;
# 2 - saved state files that the machine created, if any (one if the machine was 
# in "saved" state and one for each online snapshot);
# 3 - the machine XML file and its backups;
# 4 - the machine log files, if any;
# 5 - the machine directory, if it is empty after having deleted all the above.

# Also remove the virtual interface used for the host-only network.
# VBoxManage hostonlyif remove $MACHINE_NAME
# And the associated DHCP server.
# VBoxManage dhcpserver remove --ifname $NEWHOSTONLYIF

