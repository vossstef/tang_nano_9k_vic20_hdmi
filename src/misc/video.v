// video.v

module video (
          input	   clk,
          input    clk_pixel_x5,
          input    pll_lock,
          input [8:0] audio_div,

          input    ntscmode,
          input    vb_in,
          input    hb_in,
	      input	   vs_in_n,
	      input	   hs_in_n,

	      input [3:0]  r_in,
	      input [3:0]  g_in,
	      input [3:0]  b_in,

          input [17:0] audio_l,
          input [17:0] audio_r,

          // values that can be configure by the user via osd          
          input [1:0]  system_scanlines,
          input [1:0]  system_volume,
          input	       system_wide_screen,

	      // hdmi/tdms
	      output	   tmds_clk_n,
	      output	   tmds_clk_p,
	      output [2:0] tmds_d_n,
	      output [2:0] tmds_d_p  
	      );
   



/* -------------------- HDMI video and audio -------------------- */

// generate 48khz audio clock
reg clk_audio;

reg [8:0] aclk_cnt;
reg vresetD;

always @(posedge clk) begin
    // divisor = pixel clock / 48000 / 2 - 1
    if(aclk_cnt < audio_div)
        aclk_cnt <= aclk_cnt + 9'd1;
    else begin
        aclk_cnt <= 9'd0;
        clk_audio <= ~clk_audio;
    end
end

wire vreset;
wire [1:0] vmode;

video_analyzer video_analyzer (
   .clk(clk),
   .vs(vs_in_n),
   .hs(hs_in_n),
   .de(1'b1),
   .ntscmode(ntscmode),
   .mode(vmode),
   .vreset(vreset)
);  

wire sd_hs_n, sd_vs_n; 
wire [5:0] sd_r;
wire [5:0] sd_g;
wire [5:0] sd_b;
  
scandoubler #(10) scandoubler (
        // system interface
        .clk_sys(clk),
        .bypass(1'b0),      // bypass in ST high/mono
        .ce_divider(3'd1),
        .pixel_ena(),

        // scanlines (00-none 01-25% 10-50% 11-75%)
        .scanlines(system_scanlines),

        // shifter video interface
        .hb_in(hb_in),
	    .vb_in(vb_in),
        .hs_in(hs_in_n),
        .vs_in(vs_in_n),
        .r_in( r_in ),
        .g_in( g_in ),
        .b_in( b_in ),

        // output interface
        .hb_out(),
        .vb_out(),
        .hs_out(sd_hs_n),
        .vs_out(sd_vs_n),
        .r_out(sd_r),
        .g_out(sd_g),
        .b_out(sd_b)
);

wire [5:0] osd_r;
wire [5:0] osd_g;
wire [5:0] osd_b;  

wire [2:0] tmds;
wire tmds_clock;

// Audio c64 core specific
reg [15:0] alo,aro;
always @(posedge clk) begin
	reg [16:0] alm,arm;

	arm <= {audio_r[17],audio_r[17:2]};
	alm <= {audio_l[17],audio_l[17:2]};
	alo <= ^alm[16:15] ? {alm[16], {15{alm[15]}}} : alm[15:0];
	aro <= ^arm[16:15] ? {arm[16], {15{arm[15]}}} : arm[15:0];
end

// scale audio for valume by signed division
wire [15:0] audio_vol_l = 
    (system_volume == 2'd0)?16'd0:
    (system_volume == 2'd1)?{ {2{alo[15]}}, alo[15:2] }:
    (system_volume == 2'd2)?{ alo[15], alo[15:1] }:
    alo;

wire [15:0] audio_vol_r = 
    (system_volume == 2'd0)?16'd0:
    (system_volume == 2'd1)?{ {2{aro[15]}}, aro[15:2] }:
    (system_volume == 2'd2)?{ aro[15], aro[15:1] }:
    aro;

hdmi #(
   .AUDIO_RATE(48000), 
   .AUDIO_BIT_WIDTH(16),
   .VENDOR_NAME( { "MiSTle", 16'd0} ),
   .PRODUCT_DESCRIPTION( {"VIC20", 64'd0} )
) hdmi(
  .clk_pixel_x5(clk_pixel_x5),
  .clk_pixel(clk),
  .clk_audio(clk_audio),
  .audio_sample_word( { audio_vol_l, audio_vol_r } ),
  .tmds(tmds),
  .tmds_clock(tmds_clock),

  // video input
  .stmode(vmode),    // current video mode PAL/NTSC/MONO
  .wide(system_wide_screen),       // adopt to wide screen video
  .reset(vreset),    // signal to synchronize HDMI
  // Atari STE outputs 4 bits per color. Scandoubler outputs 6 bits (to be
  // able to implement dark scanlines) and HDMI expects 8 bits per color
  .rgb( { sd_r, 2'b00, sd_g, 2'b00, sd_b, 2'b00 } )
);

// differential output
ELVDS_OBUF tmds_bufds [3:0] (
        .I({tmds_clock, tmds}),
        .O({tmds_clk_p, tmds_d_p}),
        .OB({tmds_clk_n, tmds_d_n})
);

endmodule
