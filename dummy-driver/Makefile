#
# Makefile for the industrial I/O core.
#

ccflags-y := -DCONFIG_IIO_SIMPLE_DUMMY_EVENTS -DCONFIG_IIO_SIMPLE_DUMMY_BUFFER

obj-m += iio_dummy.o
iio_dummy-y := iio_simple_dummy.o
iio_dummy-y += iio_simple_dummy_events.o
iio_dummy-y += iio_simple_dummy_buffer.o

obj-m += iio_dummy_evgen.o


all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

install:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules_install
	depmod

connoct:
	modprobe iio_dummy
	modprobe iio_trig_sysfs
	echo "0" > /sys/bus/iio/devices/iio_sysfs_trigger/add_trigger
	echo "sysfstrig0" > iio\:device0/trigger/current_trigger

unconnoct:
	echo "NULL" > /sys/bus/iio/devices/iio\:device0/trigger/current_trigger
	echo "0" > /sys/bus/iio/devices/iio_sysfs_trigger/remove_trigger
	modprobe -r iio_trig_sysfs
	modprobe -r iio_dummy

poke:
	echo "derp" > /sys/bus/iio/devices/trigger0/trigger_now
