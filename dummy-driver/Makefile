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
