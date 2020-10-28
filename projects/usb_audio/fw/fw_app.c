/*
 * fw_app.c
 *
 * Copyright (C) 2019 Sylvain Munaut
 * All rights reserved.
 *
 * LGPL v3+, see LICENSE.lgpl3
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "console.h"
#include "led.h"
#include "mini-printf.h"
#include "spi.h"
#include <no2usb/usb.h>
#include <no2usb/usb_ac_proto.h>
#include <no2usb/usb_dfu_rt.h>
#include <no2usb/usb_hw.h>
#include <no2usb/usb_priv.h>
#include "utils.h"
#include "config.h"


// Volume helpers
// ---------------------------------------------------------------------------

/* [round(256*(math.pow(2,i/256.0)-1)) for i in range(256)] */
static const uint8_t vol_log2lin_lut[] = {
	0x00, 0x01, 0x01, 0x02, 0x03, 0x03, 0x04, 0x05,
	0x06, 0x06, 0x07, 0x08, 0x08, 0x09, 0x0a, 0x0b,
	0x0b, 0x0c, 0x0d, 0x0e, 0x0e, 0x0f, 0x10, 0x10,
	0x11, 0x12, 0x13, 0x13, 0x14, 0x15, 0x16, 0x16,
	0x17, 0x18, 0x19, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
	0x1d, 0x1e, 0x1f, 0x20, 0x20, 0x21, 0x22, 0x23,
	0x24, 0x24, 0x25, 0x26, 0x27, 0x28, 0x28, 0x29,
	0x2a, 0x2b, 0x2c, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x35, 0x36,
	0x37, 0x38, 0x39, 0x3a, 0x3a, 0x3b, 0x3c, 0x3d,
	0x3e, 0x3f, 0x40, 0x41, 0x41, 0x42, 0x43, 0x44,
	0x45, 0x46, 0x47, 0x48, 0x48, 0x49, 0x4a, 0x4b,
	0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x51, 0x52,
	0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
	0x5b, 0x5c, 0x5d, 0x5e, 0x5e, 0x5f, 0x60, 0x61,
	0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71,
	0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
	0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81,
	0x82, 0x83, 0x84, 0x85, 0x87, 0x88, 0x89, 0x8a,
	0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92,
	0x93, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b,
	0x9c, 0x9d, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
	0xa5, 0xa6, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad,
	0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb6, 0xb7,
	0xb8, 0xb9, 0xba, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0,
	0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc8, 0xc9, 0xca,
	0xcb, 0xcd, 0xce, 0xcf, 0xd0, 0xd2, 0xd3, 0xd4,
	0xd6, 0xd7, 0xd8, 0xd9, 0xdb, 0xdc, 0xdd, 0xde,
	0xe0, 0xe1, 0xe2, 0xe4, 0xe5, 0xe6, 0xe8, 0xe9,
	0xea, 0xec, 0xed, 0xee, 0xf0, 0xf1, 0xf2, 0xf4,
	0xf5, 0xf6, 0xf8, 0xf9, 0xfa, 0xfc, 0xfd, 0xff,
};

/*  [round(math.log2(1.0 + x / 256.0) * 256) for x in range(256)] */
static const uint8_t vol_lin2log_lut[] = {
	0x00, 0x01, 0x03, 0x04, 0x06, 0x07, 0x09, 0x0a,
	0x0b, 0x0d, 0x0e, 0x10, 0x11, 0x12, 0x14, 0x15,
	0x16, 0x18, 0x19, 0x1a, 0x1c, 0x1d, 0x1e, 0x20,
	0x21, 0x22, 0x24, 0x25, 0x26, 0x28, 0x29, 0x2a,
	0x2c, 0x2d, 0x2e, 0x2f, 0x31, 0x32, 0x33, 0x34,
	0x36, 0x37, 0x38, 0x39, 0x3b, 0x3c, 0x3d, 0x3e,
	0x3f, 0x41, 0x42, 0x43, 0x44, 0x45, 0x47, 0x48,
	0x49, 0x4a, 0x4b, 0x4d, 0x4e, 0x4f, 0x50, 0x51,
	0x52, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
	0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63,
	0x64, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c,
	0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x74, 0x75,
	0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d,
	0x7e, 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
	0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d,
	0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
	0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9b, 0x9c,
	0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
	0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xa9, 0xaa, 0xab,
	0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb2,
	0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xb9,
	0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc0,
	0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc6, 0xc7,
	0xc8, 0xc9, 0xca, 0xcb, 0xcb, 0xcc, 0xcd, 0xce,
	0xcf, 0xd0, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd4,
	0xd5, 0xd6, 0xd7, 0xd8, 0xd8, 0xd9, 0xda, 0xdb,
	0xdc, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe0, 0xe1,
	0xe2, 0xe3, 0xe4, 0xe4, 0xe5, 0xe6, 0xe7, 0xe7,
	0xe8, 0xe9, 0xea, 0xea, 0xeb, 0xec, 0xed, 0xee,
	0xee, 0xef, 0xf0, 0xf1, 0xf1, 0xf2, 0xf3, 0xf4,
	0xf4, 0xf5, 0xf6, 0xf7, 0xf7, 0xf8, 0xf9, 0xf9,
	0xfa, 0xfb, 0xfc, 0xfc, 0xfd, 0xfe, 0xff, 0xff,
};



#define VOL_INVALID (-32768)

/* 16384 * math.pow(10, x/(20*256)) */
static int16_t
vol_log2lin(int16_t log)
{
	uint16_t lin;
	int s = 0;

	/* Special cases */
	if (log == VOL_INVALID)	/* Special value */
		return 0x0000;

	if (log >= 1541)	/* Max is ~6 dB */
		return 0x7fff;

	/* Integer part */
	while (log < 0) {
		log += 1541;
		s += 1;
	}

	/* LUT */
	lin = vol_log2lin_lut[(log * 680) >> 12];

	/* Scaling */
	lin = (lin << 6) | (lin >> 2) | 0x4000;
	lin >>= s;

	return lin;
}

/* 20 * 256 * math.log10(lin / 16384) */
static int16_t
vol_lin2log(int16_t lin)
{
	int32_t l = 0;

	/* Special cases */
	if (lin <= 0)
		return VOL_INVALID;

	/* Integer part */
	while (lin < 0x4000) {
		lin <<= 1;
		l = l - 256;
	}

	/* LUT correct */
	l += vol_lin2log_lut[(lin >> 6) & 0xff];

	/* Final scaling */
	l = (l * 1541) >> 8;

	return (int16_t) l;
}


// Audio
// ---------------------------------------------------------------------------

struct wb_audio {
	uint32_t csr;
	uint32_t volume;
	uint32_t fifo;
} __attribute__((packed,aligned(4)));

static volatile struct wb_audio * const audio_regs = (void*)(AUDIO_BASE);

static struct {

	bool active;
	bool mute_all;

	struct {
		bool     mute;
		int16_t  vol_log;
		uint16_t vol_lin;
	} chan[2];

	uint8_t bdi;
} g_audio;

static struct usb_fn_drv _audio_drv;


static void
audio_hw_update_volume(void)
{
	audio_regs->volume =
		(((!g_audio.mute_all && !g_audio.chan[1].mute) ?
			g_audio.chan[1].vol_lin : 0) << 16) |
		(((!g_audio.mute_all && !g_audio.chan[0].mute) ?
			g_audio.chan[0].vol_lin : 0));
}

static void
audio_set_volume(uint8_t chan, int16_t vol_log)
{
	printf("Volume set %d to %d\n", chan, vol_log);

	if (g_audio.chan[chan].vol_log == vol_log)
		return;

	g_audio.chan[chan].vol_lin = vol_log2lin(vol_log);
	g_audio.chan[chan].vol_log = vol_lin2log(g_audio.chan[chan].vol_lin);

	audio_hw_update_volume();
}

static void
audio_init(void)
{
	/* Local state */
	memset(&g_audio, 0x00, sizeof(g_audio));

	/* Audio enabled at -6 dB by default */
	audio_set_volume(0, -6*256);
	audio_set_volume(1, -6*256);

	/* HW */
	audio_hw_update_volume();

	/* USB */
	usb_register_function_driver(&_audio_drv);
}

static void
audio_debug_print(void)
{
	uint32_t csr = audio_regs->csr;

	printf("Audio tick       : %04x\n", csr >> 16);
	printf("Audio FIFO level : %d\n", (csr >> 4) & 0xfff);
	printf("Audio State      : %d\n", csr & 3);
}

static int
audio_level(void)
{
	return (audio_regs->csr >> 4) & 0xfff;
}


// Audio USB data
// ---------------------------------------------------------------------------

static void
audio_usb_fill_feedback_ep(void)
{
//	uint32_t val = 8192;
//
//	/* Prepare buffer */
//	usb_data_write(64, &val, 4);
//	usb_ep_regs[1].in.bd[0].ptr = 64;
//	usb_ep_regs[1].in.bd[0].csr = USB_BD_STATE_RDY_DATA | USB_BD_LEN(3);
}



static void
audio_usb_flow_start(void)
{
	/* Reset Buffer index */
	g_audio.bdi = 0;

	/* EP 1 OUT: Type=Isochronous, dual buffered */
	usb_ep_regs[1].out.status = USB_EP_TYPE_ISOC | USB_EP_BD_DUAL;

	/* EP1 OUT: Queue two buffers */
	usb_ep_regs[1].out.bd[0].ptr = 1024;
	usb_ep_regs[1].out.bd[0].csr = USB_BD_STATE_RDY_DATA | USB_BD_LEN(288);

	usb_ep_regs[1].out.bd[1].ptr = 1024 + 288;
	usb_ep_regs[1].out.bd[1].csr = USB_BD_STATE_RDY_DATA | USB_BD_LEN(288);

	/* EP1 IN: Type=Isochronous, single buffered */
	usb_ep_regs[1].in.status = USB_EP_TYPE_ISOC;

	audio_usb_fill_feedback_ep();
}

static void
audio_usb_flow_stop(void)
{
	/* EP 1 OUT: Disable */
	usb_ep_regs[1].out.status = 0;

	/* EP 1 IN: Disable */
	usb_ep_regs[1].in.status = 0;

	/* Stop playing audio */
	audio_regs->csr = 0;
}

static void
audio_usb_set_active(bool active)
{
	if (g_audio.active == active)
		return;

	g_audio.active = active;

	if (active)
		audio_usb_flow_start();
	else
		audio_usb_flow_stop();
}

static void
audio_usb_poll(void)
{
	/* Check if enough space in FIFO */
	if (audio_level() >= 440)
		return;

	/* EP BD Status */
	uint32_t ptr = usb_ep_regs[1].out.bd[g_audio.bdi].ptr;
	uint32_t csr = usb_ep_regs[1].out.bd[g_audio.bdi].csr;

	/* Check if we have a USB packet */
	if ((csr & USB_BD_STATE_MSK) == USB_BD_STATE_RDY_DATA)
		return;

	/* Valid data ? */
	if ((csr & USB_BD_STATE_MSK) == USB_BD_STATE_DONE_OK)
	{
		static uint32_t lt;
		uint32_t ct;

		volatile uint32_t __attribute__((aligned(4))) *src_u32 = (volatile uint32_t *)((USB_DATA_BASE) + ptr);
		int len = (csr & USB_BD_LEN_MSK) - 2; /* Reported length includes CRC */

		for (int i=0; i<len; i+=4)
			audio_regs->fifo = *src_u32++;

		ct = usb_get_tick();
		if ((ct-lt) > 1)
			printf("%d %d %d %d\n", len, audio_level(), ct-lt, ct);
		lt = ct;

		/* If we have enough in the FIFO, enable core */
		if ((audio_level() > 200) && !(audio_regs->csr & 1))
			audio_regs->csr = 1;
	}

	/* Next transfer */
	usb_ep_regs[1].out.bd[g_audio.bdi].csr = USB_BD_STATE_RDY_DATA | USB_BD_LEN(288);
	g_audio.bdi ^= 1;
}



// Audio USB control
// ---------------------------------------------------------------------------

enum usb_ac_feat_unit_control {
	USB_AC_FUC_UNDEFINED = 0x00,
	USB_AC_FUC_MUTE = 0x01,
	USB_AC_FUC_VOLUME = 0x02,
	USB_AC_FUC_BASS = 0x03,
	USB_AC_FUC_MID = 0x04,
	USB_AC_FUC_TREBLE = 0x05,
	USB_AC_FUC_GRAPHIC_EQUALIZER = 0x06,
	USB_AC_FUC_AUTOMATIC_GAIN = 0x07,
	USB_AC_FUC_DELAY = 0x08,
	USB_AC_FUC_BASS_BOOST = 0x09,
	USB_AC_FUC_LOUDNESS = 0x0A,
};



static bool
_audio_usb_set_cur_cb(struct usb_xfer *xfer)
{
	struct usb_ctrl_req *req = xfer->cb_ctx;

	uint8_t chan = (req->wValue & 0xff) - 1;
	switch (req->wValue)
	{
	case (USB_AC_FUC_MUTE << 8) | 0:
		g_audio.mute_all = xfer->data[0];
		audio_hw_update_volume();
		break;

	case (USB_AC_FUC_MUTE << 8) | 1:
	case (USB_AC_FUC_MUTE << 8) | 2:
		g_audio.chan[chan].mute = xfer->data[0];
		audio_hw_update_volume();
		break;

	case (USB_AC_FUC_VOLUME << 8) | 1:
	case (USB_AC_FUC_VOLUME << 8) | 2:
		audio_set_volume(chan, *((int16_t*)xfer->data));
		break;
	}

	return true;
}

static enum usb_fnd_resp
_audio_usb_set_cur(struct usb_ctrl_req *req, struct usb_xfer *xfer)
{
	switch (req->wValue)
	{
	case (USB_AC_FUC_MUTE << 8) | 0:
	case (USB_AC_FUC_MUTE << 8) | 1:
	case (USB_AC_FUC_MUTE << 8) | 2:
		if (req->wLength != 1)
			return USB_FND_ERROR;
		break;

	case (USB_AC_FUC_VOLUME << 8) | 1:
	case (USB_AC_FUC_VOLUME << 8) | 2:
		if (req->wLength != 2)
			return USB_FND_ERROR;
		break;

	default:
		return USB_FND_ERROR;
	}

	xfer->cb_done = _audio_usb_set_cur_cb;
	xfer->cb_ctx  = req;

	return USB_FND_SUCCESS;
}

static enum usb_fnd_resp
_audio_usb_get_cur(struct usb_ctrl_req *req, struct usb_xfer *xfer)
{
	uint8_t chan = (req->wValue & 0xff) - 1;

	switch (req->wValue)
	{
	case (USB_AC_FUC_MUTE << 8) | 0:
		xfer->len = 1;
		xfer->data[0] = g_audio.mute_all;
		break;

	case (USB_AC_FUC_MUTE << 8) | 1:
	case (USB_AC_FUC_MUTE << 8) | 2:
		xfer->len = 1;
		xfer->data[0] = g_audio.chan[chan].mute;
		break;

	case (USB_AC_FUC_VOLUME << 8) | 1:
	case (USB_AC_FUC_VOLUME << 8) | 2:
		xfer->len = 2;
		*((int16_t*)xfer->data) = g_audio.chan[chan].vol_log;
		break;

	case (USB_AC_FUC_VOLUME << 8) | 0xff:
		/* FIXME ? */
		return USB_FND_ERROR;

	default:
		return USB_FND_ERROR;
	}

	return USB_FND_SUCCESS;
}

static enum usb_fnd_resp
_audio_usb_get_minmax(struct usb_ctrl_req *req, struct usb_xfer *xfer, bool minmax)
{
	switch (req->wValue)
	{
	case (USB_AC_FUC_VOLUME << 8) | 1:
	case (USB_AC_FUC_VOLUME << 8) | 2:
		xfer->len = 2;
		*((int16_t*)xfer->data) = minmax ? (5 * 256) : (-80 * 256);
		break;

	case (USB_AC_FUC_VOLUME << 8) | 0xff:
		/* FIXME ? */
		return USB_FND_ERROR;

	default:
		return USB_FND_ERROR;
	}

	return USB_FND_SUCCESS;
}


static enum usb_fnd_resp
audio_usb_ctrl_req(struct usb_ctrl_req *req, struct usb_xfer *xfer)
{
	/* Check it's a class request to an interface */
	if ((USB_REQ_TYPE(req) | USB_REQ_RCPT(req)) != (USB_REQ_TYPE_CLASS | USB_REQ_RCPT_INTF))
		return USB_FND_CONTINUE;

	/* Check it's audio class / control interface */
	if ((req->wIndex & 0xff) != 1)	/* XXX: Hardcoded bInterfaceNumber */
		return USB_FND_CONTINUE;

	/* Check unit ID */
	if ((req->wIndex >> 8) != 2)
		return USB_FND_ERROR;

	/* */
	switch (req->wRequestAndType)
	{
	case USB_RT_AC_SET_CUR_INTF:
		return _audio_usb_set_cur(req, xfer);

	case USB_RT_AC_GET_CUR_INTF:
		return _audio_usb_get_cur(req, xfer);

	case USB_RT_AC_GET_MIN_INTF:
		return _audio_usb_get_minmax(req, xfer, false);

	case USB_RT_AC_GET_MAX_INTF:
		return _audio_usb_get_minmax(req, xfer, true);

	default:
		return USB_FND_ERROR;
	}
}

static enum usb_fnd_resp
audio_usb_set_intf(const struct usb_intf_desc *base, const struct usb_intf_desc *sel)
{
	/* Check it's audio class */
	if (base->bInterfaceClass != 0x01)
		return USB_FND_CONTINUE;

	/* Sub class */
	switch (base->bInterfaceSubClass)
	{
	case USB_AC_SCLS_AUDIOCONTROL:
		return USB_FND_SUCCESS;

	case USB_AC_SCLS_AUDIOSTREAMING:
		audio_usb_set_active(sel->bAlternateSetting != 0);
		return USB_FND_SUCCESS;

	default:
		return USB_FND_ERROR;
	}
}

static enum usb_fnd_resp
audio_usb_get_intf(const struct usb_intf_desc *base, uint8_t *alt)
{
	/* Check it's audio class */
	if (base->bInterfaceClass != 0x01)
		return USB_FND_CONTINUE;

	/* Sub class */
	switch (base->bInterfaceSubClass)
	{
	case USB_AC_SCLS_AUDIOCONTROL:
		*alt = 0;
		return USB_FND_SUCCESS;

	case USB_AC_SCLS_AUDIOSTREAMING:
		*alt = g_audio.active ? 1 : 0;
		return USB_FND_SUCCESS;

	default:
		return USB_FND_ERROR;
	}
}

static struct usb_fn_drv _audio_drv = {
	.ctrl_req = audio_usb_ctrl_req,
	.set_intf = audio_usb_set_intf,
	.get_intf = audio_usb_get_intf,
};


// xxxx
// ---------------------------------------------------------------------------

extern const struct usb_stack_descriptors app_stack_desc;

static void
serial_no_init()
{
	uint8_t buf[8];
	char *id, *desc;
	int i;

	flash_manuf_id(buf);
	printf("Flash Manufacturer : %s\n", hexstr(buf, 3, true));

	flash_unique_id(buf);
	printf("Flash Unique ID    : %s\n", hexstr(buf, 8, true));

	/* Overwrite descriptor string */
		/* In theory in rodata ... but nothing is ro here */
	id = hexstr(buf, 8, false);
	desc = (char*)app_stack_desc.str[1];
	for (i=0; i<16; i++)
		desc[2 + (i << 1)] = id[i];
}

static void
boot_dfu(void)
{
	/* Force re-enumeration */
	usb_disconnect();

	/* Boot firmware */
	volatile uint32_t *boot = (void*)0x80000000;
	*boot = (1 << 2) | (1 << 0);
}

void
usb_dfu_rt_cb_reboot(void)
{
        boot_dfu();
}


void
main()
{
	int cmd = 0;

	/* Init console IO */
	console_init();
	puts("Booting Audio image..\n");

	/* LED */
	led_init();
	led_color(48, 96, 5);
	led_blink(true, 200, 1000);
	led_breathe(true, 100, 200);
	led_state(true);

	/* SPI */
	spi_init();

	/* Enable USB directly */
	serial_no_init();
	usb_init(&app_stack_desc);
	usb_dfu_rt_init();

	/* Audio */
	audio_init();

	/* Connect */
	usb_connect();

	/* Main loop */
	while (1)
	{
		/* Prompt ? */
		if (cmd >= 0)
			printf("Command> ");

		/* Poll for command */
		cmd = getchar_nowait();

		if (cmd >= 0) {
			if (cmd > 32 && cmd < 127) {
				putchar(cmd);
				putchar('\r');
				putchar('\n');
			}

			switch (cmd)
			{
			case 'p':
				audio_debug_print();
				break;
			case 'b':
				boot_dfu();
				break;
			case 'c':
				usb_connect();
				break;
			case 'd':
				usb_disconnect();
				break;
			default:
				break;
			}
		}

		/* USB poll */
		usb_poll();
		audio_usb_poll();
	}
}
