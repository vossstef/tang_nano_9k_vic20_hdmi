# tang_nano_9k_vic20_hdmi
[VIC20](https://en.wikipedia.org/wiki/VIC-20) living in a [Gowin GW1NR-9](https://www.gowinsemi.com/en/product/detail/49/) FPGA on a [Sipeed Tang Nano 9k](https://api.dl.sipeed.com/shareURL/TANG/Nano%209K/1_Specification) with HDMI Video and Audio Output.<br>
<br>

Original VIC-20 core by MikeJ (Mike Johnson) and T65 WoS (Wolfgang Scherr)

Features:
* HDMI 720x576p @50Hz Video and Audio Output
* PS/2 Keyboard
* Joystick
* optional Speaker Sound

By default cartridge ROM will be booted (see push button how to suppress).

![vic20_dvi](\.assets/vic20_dvi.png)<br> <br>
**Warning** HDMI Signal is not fully compatible with many Monitor/TV as mode 720x576p@50Hz isn't an official [VESA](https://glenwing.github.io/docs/VESA-DMT-1.13.pdf) mode!<br>
Working on e.g. BENQ GL2450HM, Check [EDID](https://en.wikipedia.org/wiki/Extended_Display_Identification_Data) Display Monitor Timing (DMT) of your target display for support.
An [EDID Database](https://github.com/bsdhw/EDID) or other support material might help.<br>

![vic20_hdmi](\.assets/hdmi_1.png)<br>
<br>

## Push Button utilization
* S1 push button Reset
* S2 Cartridge ROM disable (keep S2 pressed while power-on or excert a S1 push-button Reset, release after)
## Powering
Prototype circuit with Keyboard and optional Audio Amp can be powered by Tang USB-C connector from PC or a Power Supply Adapter. 
## Synthesis
Source code can be synthesized, fitted and programmed with GOWIN IDE Windows or Linux.

## Simulation
Basic testbench as a starting point in the TB folder (*vic20_tb.vhd*)<br/>
Script for compiling the Gowin library, sources and testbench in the simulation folder (*sim_vic20.do*).<br/>
For Simulation run execute_simulation.bat (*Windows*) or execute_simulation.sh (*Linux*)

## GOWIN IP Blocks
For sake of simplification i use block SRAM resources for all memories (SP, SDP, pROM). In addition rPLL, CLK divdiers and GSR resource.
## Pin mapping 
see pin configuration in .cst configuration file

## cartride ROM
The bin2mi tool can be used to generate from a 8192 byte Game ROM new pROM VHDL code. For the ROM image i took had to remove the first byte in the generated HEX file indicating the VIC-20 ROM region (0xA0) before providing needed Memory initialization file to the IP Block generator. 
## HW circuit considerations
- PS/2 keyboard has to be connected to 3.3V tolerant FPGA via level shifter to avoid damage of inputs ! Use e.g. 2 pcs SN74LVC1G17DBVR 5V to 3V3 level shifter. My Keyboard has internal pull-up resistors to 5V for Clock and Data Signals so didn't needed external ones. 
- Joystick interface is 3.3V tolerant. Joystick 5V supply pin has to be left floating !
- The FPGA pin delivering SigmaDelta Audio signal to the Amplifier need a low pass filter. 3K3 series Resistor and 4n7 Capacitor to GND.
- Tang Nano 5V output connected to Audio Amplifier and Keyboard supply. Tang 3V3 output to level shifter supply.

**Pinmap D-SUB 9 Joystick Interface** <br>
![pinmap](\.assets/vic20-Joystick.png)

| Joystick pin | Tang Nano pin | FPGA pin | Joystick Function |
| ----------- | ---   | --------  | ----- |
| 1 | J5 8  | 28   | Joy3 RIGHT |
| 2 | J5 7  | 27 | Joy2 LEFT |
| 3 | J5 6  | 26 | Joy1 DOWN |
| 4 | J5 5 | 25 | Joy0 UP | 
| 5 | n.c. | n.c. | POT Y |
| 6 | J5 9 | 29 | FIRE B.|
| 7 | n.c. | n.c. | 5V |
| 8 | J6 23 | - | GND |
| 9 | n.c. | n.c. | POT X |

**Pinmap PS2 Interface** <br>
![pinmap](\.assets/ps2conn.png)

| PS2 pin | Tang Nano pin | FPGA pin | PS2 Function |
| ----------- | ---   | --------  | ----- |
| 1 | J6 10  | 77   | DATA  |
| 2 | n.c.  | - | n.c. |
| 3 | J6 23 | - | GND |
| 4 | J6 18 | - | +5V |
| 5 | J6 11| 76 | CLK |
| 6 | n.c. | - | n.c |

**low pass filter for Audio Amplifier input** <br>
![pinmap](\.assets/audiofilter.png)<br>
### BOM

[Sipeed Tang Nano 9k](https://api.dl.sipeed.com/shareURL/TANG/Nano%209K/1_Specification)<br> 
D-SUB 9 M connector<br> 
Commodore/[Atari](https://en.wikipedia.org/wiki/Atari_CX40_joystick) compatible Joystick<br> 
or alternatively 5D Rocker Joystick navigation button module<br>
PS/2 Keyboard<br>
PS/2 Socket Adapter Module<br>
2 pcs [SN74LVC1G17DBVR](http://www.ti.com/document-viewer/SN74LVC1G17/datasheet) level shifter<br>
Prototype Board<br>
TFT Monitor with HDMI Input<br>
optional 3K3 Resistor<br>
optional 4n7 Ceramics<br>
optional Mini PAM8403 Audio Amplifier Module<br>
optional 8R Speaker<br>