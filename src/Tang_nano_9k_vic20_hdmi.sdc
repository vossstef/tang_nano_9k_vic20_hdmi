//Copyright (C)2014-2023 GOWIN Semiconductor Corporation.
//All rights reserved.
//File Title: Timing Constraints file
//GOWIN Version: 1.9.8.11 Education
//Created Time: 2023-08-16 21:49:35
create_clock -name I_CLK_REF -period 37.037 -waveform {0 18} [get_ports {I_CLK_REF}]
create_generated_clock -name clk_5x_pixel -source [get_ports {I_CLK_REF}] -master_clock I_CLK_REF -divide_by 1 -multiply_by 5 [get_nets {hdmi/clk_5x_pixel}]
